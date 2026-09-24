# =============================================================================
# render.R - Tile assembly, provenance, the cache
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# The orchestrator. tile_setup() reads the coarse window with its margin;
# render_elevation_tile() and render_biome_tile() call every subsystem above in
# compositing order; tile_path() decides between the static pyramid, the cache and
# a fresh synthesis.
#
# The provenance stamps live here too. They are what make a change in any other
# file re-render the tiles it affects, so this is the file that has to know the
# others exist -- which is why it is last.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Tile rendering
# -----------------------------------------------------------------------------

#' Render one procedural tile to a 256x256 PNG (north-up RGBA) at `png_path`.
#'
#' Phase 1: faithful upsample only (no synthesized detail). The coarse VRT data
#' is projected to the exact mercator tile grid (with a pixel pad so the
#' hillshade has edge context) and composited with the same styling as the
#' static pyramid. Phases 2+ insert synthesize_* between the upsample and the
#' composite.
#'
#' @param z,x,y_xyz  XYZ tile coordinates (caller converts TMS y first).
#' @param tile_px    Output tile size (256).
#' @param pad_px     Edge pad (each side) for hillshade context; cropped off.
#' Build the padded EPSG:3857 target grid + the coarse-data context for a tile.
#' Shared by the elevation and biome renderers. Returns a list with the padded
#' target raster, the exact (unpadded) tile extent, and the upsampled fine
#' layers (elevation already micro-relieved when detail_amp > 0; biome dithered).
tile_setup <- function(z, x, y_xyz, tile_px = 256L, pad_px = 10L, detail_amp = 1) {
  ext3857 <- tile_extent_3857(z, x, y_xyz)
  cell  <- (ext3857$xmax - ext3857$xmin) / tile_px
  ext_p <- ext(ext3857$xmin - pad_px * cell, ext3857$xmax + pad_px * cell,
               ext3857$ymin - pad_px * cell, ext3857$ymax + pad_px * cell)
  tgt_pad <- rast(ext_p, ncol = tile_px + 2 * pad_px, nrow = tile_px + 2 * pad_px,
                  crs = "EPSG:3857")

  # Margin of 8 coarse cells (~8.8 km) >> the coastline/biome warp (<=900 m) and
  # the valleyness smoothing, so all neighbourhood/warp ops sample real data and
  # stay seamless across tiles.
  bbox <- tile_bbox_4326(z, x, y_xyz, margin_cells = 8)
  ctx  <- read_coarse_context(bbox, layers = c("elevation", "water_class",
                                               "biome", "capacity",
                                               "water_influence"))

  if (is.null(ctx$elevation)) {
    elev_fine <- tgt_pad; values(elev_fine) <- NA_real_
  } else {
    # cubicspline (NOT bilinear): bilinear's slope is piecewise-flat per coarse
    # cell and jumps at cell edges, so the hillshade comes out as coarse-cell
    # BOXES. Cubicspline is C1-smooth -> smooth slope -> smooth hillshade. The
    # coarse shading must be smoothly interpolated before it reaches high zoom.
    elev_fine <- project(ctx$elevation, tgt_pad, method = "cubicspline")
  }
  anchor_fine <- elev_fine        # smooth coarse anchor, pre-carve/pre-relief
  wc_fine    <- if (!is.null(ctx$water_class)) project(ctx$water_class, tgt_pad, method = "near") else NULL
  # Smooth shelf shading (0 shallow shore .. 1 deep): distance from water to the
  # nearest land, measured on the COARSE window (its margin -> seamless), then
  # projected. Replaces the coarse/NA water_depth.vrt that caused blocky coastal
  # water patches.
  shelf_fine <- NULL
  if (!is.null(ctx$water_class)) {
    cw <- ctx$water_class
    land_c <- ifel(!is.na(cw) & cw != WATER_CLASS$OCEAN & cw != WATER_CLASS$LAKE, 1, NA)
    lv <- values(land_c)[, 1]
    if (sum(!is.na(lv)) > 0 && sum(is.na(lv)) > 0) {            # need BOTH land & water
      # Smoothstep (not linear clamp): the linear ramp's C0 kink where it
      # saturates at SHELF_DIST draws a visible hard line parallel to straight
      # coasts (reads as rectangular shallow patches).
      st <- clamp(terra::distance(land_c) / SHELF_DIST, 0, 1)
      shelf_c <- st * st * (3 - 2 * st)
      shelf_fine <- project(shelf_c, tgt_pad, method = "bilinear")
    } else {                                                    # all-water -> deep; all-land -> unused
      shelf_fine <- tgt_pad; values(shelf_fine) <- 1
    }
  }
  # Biome + coastline are warp-sampled from the COARSE windows (margin) so they
  # don't seam at tile edges.
  biome_fine <- if (!is.null(ctx$biome)) synthesize_biome(ctx$biome, tgt_pad) else NULL

  # Road-aware warp suppression: fade the coastline warp to 0 within
  # COAST_WARP_SUPPRESS_M of a (non-ferry) road, so no crenellation inlet cuts
  # across the road and it stays on the shore. Roads selected with that margin
  # (so roads just outside the tile still suppress near the edge); softened with
  # a small focal so the warp doesn't switch on abruptly at the ring.
  warp_scale <- NULL
  if (detail_amp > 0) {
    m <- COAST_WARP_SUPPRESS_M
    rext <- ext(ext_p$xmin - m, ext_p$xmax + m, ext_p$ymin - m, ext_p$ymax + m)
    rds  <- crop_vectors(rext)$roads
    if (!is.null(rds) && nrow(rds) > 0) {
      isf <- if ("is_ferry" %in% names(rds)) !is.na(rds$is_ferry) & rds$is_ferry else rep(FALSE, nrow(rds))
      rds <- rds[!isf, ]
      if (nrow(rds) > 0) {
        sm <- terra::rasterize(terra::vect(sf::st_buffer(rds, m)), tgt_pad, field = 1)
        warp_scale <- focal(ifel(is.na(sm), 1, 0), w = 9, fun = "mean", na.rm = TRUE)
      }
    }
  }

  # Single shared coastline drives the water paint in BOTH the elevation and
  # biome renderers, so they always agree. detail_amp == 0 = un-warped coarse mask.
  waterness <- NULL
  water_mask <-
    if (is.null(ctx$water_class)) NULL
    else if (detail_amp > 0) {
      cl <- synthesize_coastline(ctx$water_class, tgt_pad, warp_scale = warp_scale)
      waterness <- cl$waterness
      cl$mask
    } else (!is.na(wc_fine) & (wc_fine == WATER_CLASS$OCEAN | wc_fine == WATER_CLASS$LAKE))

  # Seamless valleyness from the coarse elevation window; shared by relief + botany.
  valleyness <- if (!is.null(ctx$elevation) && detail_amp > 0)
    valleyness_from_coarse(ctx$elevation, tgt_pad, elev_fine) else NULL

  # River valleys: carve the floodplain profile into the anchor BEFORE the
  # micro-relief (so noise sits on the carved surface), damp the noise across
  # the floodplain (flat alluvium), and fold the floodplain into valleyness
  # (riparian meadows; dendritic tributaries drain toward the real rivers).
  rvf <- NULL
  if (!is.null(ctx$elevation) && detail_amp > 0) {
    mrv   <- 1500
    rext2 <- ext(ext_p$xmin - mrv, ext_p$xmax + mrv, ext_p$ymin - mrv, ext_p$ymax + mrv)
    rvf   <- river_valley_fields(tgt_pad, crop_vectors(rext2)$rivers)
    if (!is.null(rvf)) {
      ef0 <- elev_fine
      elev_fine <- elev_fine - rvf$carve
      elev_fine <- ifel(!is.na(ef0) & ef0 > 0.5 & elev_fine < 0.5, 0.5, elev_fine)
      if (!is.null(valleyness)) valleyness <- max(valleyness, 0.7 * rvf$fpw)
    }
  }
  if (!is.null(ctx$elevation) && detail_amp > 0) {
    elev_fine <- add_microrelief(elev_fine, biome_fine, water_mask,
                                 amp_scale = detail_amp, valleyness = valleyness,
                                 damp = if (!is.null(rvf)) 1 - 0.85 * rvf$fpw else NULL)
  }

  capacity_fine <- if (!is.null(ctx$capacity)) project(ctx$capacity, tgt_pad, method = "bilinear") else NULL
  waterinf_fine <- if (!is.null(ctx$water_influence)) project(ctx$water_influence, tgt_pad, method = "bilinear") else NULL

  # Annual lunar climate (Io/Ganymede/Deos mean intensity, EPSG:4326 1deg). The
  # source is tiny, so just project the whole thing to the tile grid (smooth,
  # province-scale). Drives vegetation TYPE mix + the moon-colour wash.
  moon_fine <- NULL
  mp <- .tile_moon_path()
  if (file.exists(mp)) {
    mw <- try(project(rast(mp)[[1:3]], tgt_pad, method = "bilinear"), silent = TRUE)
    if (!inherits(mw, "try-error")) moon_fine <- mw
  }

  list(ext = ext3857, tile_px = tile_px, has_data = !is.null(ctx$elevation),
       elev = elev_fine, anchor = anchor_fine, wc = wc_fine, shelf = shelf_fine,
       biome = biome_fine, capacity = capacity_fine, moon = moon_fine,
       water_mask = water_mask, valleyness = valleyness,
       waterness = waterness, water_inf = waterinf_fine,
       fpw = if (!is.null(rvf)) rvf$fpw else NULL)
}

#' Crop a padded composite to the exact tile grid, add opaque alpha, write PNG.
write_tile_png <- function(comp, ext3857, tile_px, png_path, verbose = FALSE) {
  comp <- crop(comp, ext3857)
  comp <- resample(comp, rast(ext3857, ncol = tile_px, nrow = tile_px, crs = "EPSG:3857"),
                   method = "near")
  alpha <- comp[[1]]; values(alpha) <- 255
  rgba <- c(comp, alpha)
  dir.create(dirname(png_path), recursive = TRUE, showWarnings = FALSE)
  writeRaster(round(rgba), png_path, overwrite = TRUE, datatype = "INT1U",
              filetype = "PNG", NAflag = NA)
  if (verbose) cat("  rendered", png_path, "\n")
  invisible(png_path)
}

#' Render one procedural ELEVATION tile (hillshade composite + crenellated
#' coastline) to a 256x256 PNG. detail_amp = 0 reproduces the Phase-1 smooth
#' upsample.
render_elevation_tile <- function(z, x, y_xyz, png_path,
                                   tile_px = 256L, pad_px = 10L,
                                   detail_amp = 1, verbose = FALSE) {
  s <- tile_setup(z, x, y_xyz, tile_px, pad_px, detail_amp)
  feats <- detail_amp > 0 && s$has_data
  vec   <- if (feats) crop_vectors(ext(s$elev)) else list(rivers = NULL, roads = NULL)

  # Valley incision now happens in tile_setup (before micro-relief); the fine
  # per-tier distances here only drive the variable-width channel drawing.
  tier_d <- if (feats) river_tier_distance(s$elev, vec$rivers) else NULL
  elev <- s$elev

  # Roads are land-only (RoadBuilder); the fine coastline warp can grow water a
  # cell or two over a shore-hugging road. Carve the (non-ferry) road corridor
  # OUT of the water mask -> the road stays on land and the warped lake/coast
  # edge bends AROUND it (road appears to follow the water's edge), instead of
  # crossing or dead-ending into the water.
  wmask <- s$water_mask
  if (feats && !is.null(wmask) && !is.null(vec$roads) && nrow(vec$roads) > 0) {
    isf <- if ("is_ferry" %in% names(vec$roads)) !is.na(vec$roads$is_ferry) & vec$roads$is_ferry else rep(FALSE, nrow(vec$roads))
    land_rd <- vec$roads[!isf, ]
    if (nrow(land_rd) > 0) {
      half <- (ROAD_CASE_PX * (1 + 0.18 * max(0, z - 12)) / 2 + 1.5) * res(s$elev)[1]
      corr_rd <- .line_mask(land_rd, s$elev, half)
      if (!is.null(corr_rd)) wmask <- ifel(corr_rd, FALSE, wmask)
    }
  }

  # Settlement + sacred footprints (clearings/fields/groves), deterministic.
  sett <- if (feats) crop_settlements(ext(s$elev)) else list(setts = NULL, paths = NULL)
  sfld <- if (feats && !is.null(sett$setts)) settlement_fields(s$elev, sett$setts, wmask) else NULL
  sac  <- if (feats) crop_sacred(ext(s$elev)) else NULL
  sfx  <- if (feats && !is.null(sac)) sacred_fields(s$elev, sac) else NULL

  comp <- composite_terrain(elev, s$wc, s$shelf, water_mask = wmask,
                            elev_anchor = if (feats) s$anchor else NULL)
  if (feats && !is.null(s$biome)) {
    corr <- corridor_mask(elev, vec$rivers, vec$roads)
    if (!is.null(sfld)) corr <- max(corr, sfld$f)        # clear the settlement footprint
    if (!is.null(sfx))  corr <- max(corr, sfx$clear)     # ...and sacred clearings
    comp <- apply_vegetation(comp, s$biome, s$capacity, wmask, elev, s$moon,
                             suppress = corr, valleyness = s$valleyness,
                             fpw = s$fpw, water_inf = s$water_inf,
                             waterness = s$waterness,
                             grove = if (!is.null(sfx)) sfx$grove else NULL, zoom = z)
  }
  if (feats && !is.null(sfld)) comp <- apply_settlement_ground(comp, sfld)
  if (feats) {
    comp <- draw_streams(comp, s$valleyness, wmask, zoom = z)  # creeks under rivers
    slope_anchor <- if (z >= 13L) terrain(s$anchor, v = "slope", unit = "degrees") else NULL
    comp <- draw_rivers(comp, vec$rivers, wmask, tier_d,       # variable width + estuary
                        waterness = s$waterness, slope_deg = slope_anchor, zoom = z)
    comp <- draw_paths(comp, sett$paths, wmask, zoom = z)
    if (z >= SACRED_MIN_Z && !is.null(sac)) {           # pilgrim paths to shrines
      spl <- sacred_paths(sac, vec$roads, sett$paths)
      comp <- draw_paths(comp, spl, wmask, zoom = z)
    }
    comp <- draw_roads(comp, vec$roads, zoom = z)        # cased line on the carved land
    comp <- draw_bridges(comp, vec$roads, tier_d, z)     # stone decks at crossings
    comp <- draw_buildings(comp, sett$setts, z, wmask,
                           roads = vec$roads, paths = sett$paths,
                           waterness = s$waterness)
    comp <- draw_walls(comp, sett$setts, z, vec$roads, sett$paths, wmask)
    comp <- draw_sacred(comp, sac, z, wmask)
  }
  write_tile_png(comp, s$ext, tile_px, png_path, verbose)
}

#' Render one procedural BIOME tile (flat dithered biome colours, matching the
#' static biome overlay). Land/water split follows the synthesized coastline so
#' it lines up with the elevation tile.
render_biome_tile <- function(z, x, y_xyz, png_path,
                              tile_px = 256L, pad_px = 10L,
                              detail_amp = 1, verbose = FALSE) {
  s <- tile_setup(z, x, y_xyz, tile_px, pad_px, detail_amp)
  if (is.null(s$biome)) {                          # no biome data -> ocean colour
    comp <- colorize_discrete(setValues(rast(s$elev), 0))
  } else {
    biome <- s$biome
    if (!is.null(s$water_mask)) biome[s$water_mask] <- 0   # water cells -> Ocean(0)
    comp <- colorize_discrete(biome)
  }
  write_tile_png(comp, s$ext, tile_px, png_path, verbose)
}

#' Top-level dispatch used by the server. Returns the path to a PNG on disk,
#' rendering + write-through caching it first if z is above native and it is not
#' already cached.
#'
#' Everything that decides what a synthesized tile looks like.
#'
#' Wide on purpose: a tile is the product of nearly the whole file, so almost
#' any rendering change should invalidate it. The cost of being wrong in the
#' generous direction is a re-render (seconds, on demand, only for tiles someone
#' actually looks at). The cost of being wrong in the stingy direction is a map
#' that silently shows last week's code.
# Everything that decides what a tile LOOKS like, hashed as source files.
#
# This replaced an enumerated list of 31 function names. The list was wrong:
# the engine defines 73 functions, and the 42 it did not name included
# shape_roads(), meander_lines(), .hash01(), .line_mask(), .paint() and
# colorize_discrete() -- all of which change the map. Editing any of them moved
# nothing here, so the pyramid kept serving tiles drawn by the old code and the
# change looked like it had done nothing. That is the failure the version
# integers had, reintroduced one level up, and it is why the dependency is now
# the file rather than a promise to keep a list current.
#
# Constants are no longer listed separately either: BIOME_RELIEF_M, RIVER_TIER,
# ROAD_CASE_PX and the rest all live inside these files, so hashing the files
# covers them. NOISE_FIELDS is the one real dependency outside them, and it
# stays a field-level stamp on purpose -- prov_field() ignores `note` and
# `used_by`, so documenting a field does not re-render 24,000 tiles.
.prov_render_stamp <- function() prov_stamp(list(
  engine = prov_rfile(TILE_DRAW_FILES),
  fields = prov_field(names(NOISE_FIELDS))))

.prov_render_stamp_file <- function()
  file.path(.tile_pyramid(), ".render-stamp")

# The mtime given to a stamp written for the FIRST time, when adopting an
# existing pyramid. Any date earlier than every tile on disk does the job; this
# one is obviously artificial, so a human reading `dir` sees a deliberate marker
# rather than a plausible-looking build time.
.PROV_STAMP_ADOPT_TIME <- as.POSIXct("1970-01-02", tz = "UTC")

#' Bring the on-disk render stamp up to date; return its mtime.
#'
#' Rewritten only when the hash actually changes, because the file's MTIME is
#' the invalidation boundary -- touching it on every call would condemn the
#' whole pyramid on every process start.
#'
#' THE BOOTSTRAP CASE, which is the one that bites.
#'
#' When no stamp exists at all, the pyramid on disk predates provenance
#' tracking and there is no way to know what code drew it. Writing the first
#' stamp with the current time answers that question with "something older than
#' now", which condemns EVERY tile -- on this map, ~24,000 of them, hours of
#' re-rendering, triggered by simply starting the server for the first time
#' after the feature landed. Nothing warns you, because the "render code
#' changed" message only fires when there was a previous stamp to compare.
#'
#' So a first write ADOPTS what is already there: the stamp is backdated, and
#' every existing tile counts as current. That trades one generation of
#' invalidation -- a change made before tracking existed will not be caught --
#' for not re-rendering a pyramid that is almost certainly fine. Every
#' subsequent change is detected normally, because from then on there is a
#' stamp to compare against.
#'
#' Pass `adopt = FALSE` (or call invalidate_tile_cache() afterwards) if you know
#' the pyramid is stale and want it redrawn.
#'
#' Memoised per process: the stamp deparses ~30 functions, which is cheap once
#' and not cheap once per tile.
.prov_stamp_env <- new.env(parent = emptyenv())
sync_render_stamp <- function(force = FALSE, adopt = TRUE) {
  if (!is.null(.prov_stamp_env$mtime) && !force) return(.prov_stamp_env$mtime)
  f <- .prov_render_stamp_file()
  cur <- .prov_render_stamp()
  had_file <- file.exists(f)
  old <- if (had_file) {
    r <- try(readRDS(f), silent = TRUE)
    if (inherits(r, "try-error")) NULL else r
  } else NULL

  if (is.null(old) || !identical(old$combined, cur$combined)) {
    dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
    ok <- try(saveRDS(cur, f), silent = TRUE)
    if (!inherits(ok, "try-error")) {
      if (!had_file) {
        # First stamp ever. Adopt or condemn, but say which -- silently doing
        # either to a pyramid someone spent hours on is not acceptable.
        if (adopt) {
          try(Sys.setFileTime(f, .PROV_STAMP_ADOPT_TIME), silent = TRUE)
          message("  no render stamp found - adopting the existing tile pyramid ",
                  "as current.\n  (Re-render it with invalidate_tile_cache() if ",
                  "you know it is stale.)")
        } else {
          message("  no render stamp found - condemning the existing tile ",
                  "pyramid; it will re-render on demand")
        }
      } else if (is.null(old)) {
        # The file is there but unreadable, so there is nothing to compare and
        # no claim that the tiles are current. Condemn: damage is rare, and
        # guessing in the permissive direction here would hide a real problem.
        message("  render stamp unreadable - rewriting; procedural tiles will ",
                "re-render on demand")
      } else {
        message("  render code changed (", prov_why(old, cur),
                ") - procedural tiles will re-render on demand")
      }
    }
  }
  .prov_stamp_env$mtime <- if (file.exists(f)) file.info(f)$mtime else Sys.time()
  .prov_stamp_env$mtime
}

#' Condemn the whole procedural pyramid: every cached tile re-renders on its
#' next request. Nothing is deleted.
#'
#' The escape hatch for the adoption above, and for "I changed something the
#' stamp does not track". Touching the stamp is all it takes, because the
#' mtime IS the boundary.
invalidate_tile_cache <- function() {
  f <- .prov_render_stamp_file()
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  if (!file.exists(f)) saveRDS(.prov_render_stamp(), f)
  Sys.setFileTime(f, Sys.time())
  .prov_stamp_env$mtime <- file.info(f)$mtime
  message("  tile pyramid invalidated - every procedural tile will re-render ",
          "on its next request")
  invisible(.prov_stamp_env$mtime)
}

#' Declare the pyramid current under the code as it stands now.
#'
#' The counterpart to invalidate_tile_cache(), for the case a refactor creates:
#' the engine's source changed, so the stamp moved and every tile is condemned,
#' but you have DEMONSTRATED that the output is unchanged. Splitting this engine
#' into Functions/tiles/ was exactly that -- a pure move, verified by rendering
#' the same five tiles before and after and comparing md5 -- and re-rendering
#' 24,000 tiles to arrive at identical bytes is pure waste.
#'
#' Use it only with that evidence in hand. "It looks the same to me" is not the
#' same claim, and this is the one lever that can make the cache lie.
adopt_tile_cache <- function() {
  f <- .prov_render_stamp_file()
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(.prov_render_stamp(), f)
  Sys.setFileTime(f, .PROV_STAMP_ADOPT_TIME)
  .prov_stamp_env$mtime <- file.info(f)$mtime
  message("  pyramid adopted under the current render stamp - nothing will ",
          "re-render.\n  Only correct if you have shown the output is unchanged.")
  invisible(.prov_stamp_env$mtime)
}

#' Was this PNG drawn before the current rendering code?
tile_is_stale <- function(png) {
  st <- try(sync_render_stamp(), silent = TRUE)
  if (inherits(st, "try-error") || is.null(st)) return(FALSE)   # never block a serve
  fi <- file.info(png)
  !is.na(fi$mtime) && fi$mtime < st
}

#' How much of the procedural pyramid the current code has superseded.
#' Nothing is deleted; stale tiles re-render when next requested.
tile_cache_status <- function(layer = "elevation") {
  st <- sync_render_stamp()
  root <- file.path(.tile_pyramid(), layer)
  if (!dir.exists(root)) return(invisible(NULL))
  zs <- suppressWarnings(as.integer(basename(list.dirs(root, recursive = FALSE))))
  zs <- sort(zs[!is.na(zs) & zs > (TILE_NATIVE_MAX[[layer]] %||% 8L)])
  cat(sprintf("Procedural tiles for '%s' (stamp %s)\n", layer, format(st)))
  for (z in zs) {
    f <- list.files(file.path(root, z), "\\.png$", recursive = TRUE, full.names = TRUE)
    if (!length(f)) next
    stale <- sum(file.info(f)$mtime < st, na.rm = TRUE)
    cat(sprintf("  z%-3d %6d tiles  %6d stale\n", z, length(f), stale))
  }
  invisible(NULL)
}

#' @param layer   "elevation" (Phase 1) ; "biome" added in Phase 2.
#' @param y       TMS y exactly as it arrives in the {y} URL slot.
#' @param force   Re-render even if cached.
tile_path <- function(layer, z, x, y, force = FALSE, verbose = FALSE) {
  z <- as.integer(z); x <- as.integer(x); y <- as.integer(y)
  cache_png <- file.path(.tile_pyramid(), layer, z, x, paste0(y, ".png"))

  native_max <- TILE_NATIVE_MAX[[layer]] %||% 8L
  if (z <= native_max) {
    return(if (file.exists(cache_png)) cache_png else NULL)   # static pyramid only
  }
  if (z > TILE_PROCEDURAL_MAX) return(NULL)
  # A cached PNG older than the render stamp was drawn by code that has since
  # changed, so it is re-rendered rather than served. Before this, a rendering
  # change was invisible until someone deleted the pyramid by hand -- and the
  # usual symptom was an afternoon spent wondering why an edit had no effect.
  if (file.exists(cache_png) && !force && !tile_is_stale(cache_png))
    return(cache_png)

  y_xyz <- tms_to_xyz_y(y, z)
  if (layer == "elevation") {
    render_elevation_tile(z, x, y_xyz, cache_png, verbose = verbose)
  } else if (layer == "biome") {
    render_biome_tile(z, x, y_xyz, cache_png, verbose = verbose)
  } else {
    stop("Procedural layer not implemented yet: ", layer)
  }
  if (file.exists(cache_png)) cache_png else NULL
}

# Null-coalescing helper (MapBuilder defines one too, but be self-sufficient).
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
