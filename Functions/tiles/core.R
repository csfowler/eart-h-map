# =============================================================================
# core.R - Configuration, coordinates, coarse reads, colour ramp
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# Shared ground. Every other file in this directory depends on something here,
# and almost nothing here depends on them, so this is the file to read first and
# the file to change most carefully.
#
# WORLD_SEED lives here. So do the zoom ceilings (TILE_NATIVE_MAX,
# TILE_PROCEDURAL_MAX), the slippy-map coordinate math with its TMS y-flip, the
# windowed VRT reader that gives every render its 8-coarse-cell margin, and the
# elevation colour ramp.
#
# A change here is felt by all five subsystems at once, which is the opposite of
# the property the rest of this split is for. Prefer adding to a subsystem file.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

# The engine's own files, as provenance dependencies. Stamps hash these rather
# than naming functions one by one: a hand-kept list of function names has the
# failure mode the version integers had -- you forget -- and it had already
# happened. .prov_render_stamp() named 31 functions while the engine defined 73,
# so editing shape_roads() (student project 1) rebuilt the road geometry and
# re-rendered nothing, and the map silently kept the old roads.
#
# server.R is deliberately absent: how tiles are SERVED does not change how they
# LOOK, and an HTTP change should not condemn a pyramid.
TILE_DRAW_FILES <- file.path("Functions/tiles",
  c("core.R", "terrain.R", "vegetation.R", "shading.R",
    "linear.R", "settlements.R", "sacred.R", "render.R"))

# A single fixed seed makes the whole sub-cell world reproducible. Changing it
# re-rolls every synthesized tile (and invalidates the procedural cache).
WORLD_SEED <- 1789L

# Offshore distance (metres) over which water shades shallow -> deep. Water is
# shaded by smooth distance-to-shore (computed on the coarse window so it's
# seamless), NOT by the coarse/NA-prone water_depth.vrt, which produced blocky
# patches along the coast.
SHELF_DIST <- 2500

# Within this distance (metres) of a road the coastline warp is faded out (to
# the smooth coarse boundary) so no crenellation inlet cuts across a road. >=
# the coastline warp amplitude (~900 m) so an inlet can't reach the road.
COAST_WARP_SUPPRESS_M <- 950

# Native (data) zoom per raster layer -- mirrors MAP_CONFIG.tile in
# build_reference_map(). At/below this, the static pyramid is authoritative.
TILE_NATIVE_MAX <- list(elevation = 8L, biome = 8L)

# Highest zoom we will synthesize. Raised after visual sign-off (12 -> 14).
TILE_PROCEDURAL_MAX <- 14L

# Web-Mercator constants.
MERC_R      <- 6378137.0
MERC_ORIGIN <- pi * MERC_R          # 20037508.342789244

# Project root + canonical paths.
#
# TWO roots, and they are no longer parent and child. .tile_root() is the repo
# (Functions/, Input Data/ — the generated world). map_root() is the Map/
# artefact, which now sits outside OneDrive at C:/Map. Anything under Map/ must
# go through map_path(); anything under Input Data/ through .tile_root().
.tile_root      <- function() here::here()
.tile_pyramid   <- function() map_path("tiles")
.tile_vrt_path  <- function(name) file.path(.tile_root(), "Input Data", "HighResolution", paste0(name, ".vrt"))
.tile_moon_path <- function() file.path(.tile_root(), "Input Data", "Moons", "annual_moon_power_stats.tif")

# -----------------------------------------------------------------------------
# Coordinate math (standard slippy-map mercator, with the TMS y-flip)
# -----------------------------------------------------------------------------

#' Leaflet requests tiles with `tms:true`, so the {y} in the URL is the TMS y
#' (origin bottom-left). Convert to the XYZ y (origin top-left) used by all the
#' mercator math below.
tms_to_xyz_y <- function(y_tms, z) (2L^z - 1L) - y_tms

#' EPSG:3857 extent of an XYZ tile.
tile_extent_3857 <- function(z, x, y_xyz) {
  n    <- 2^z
  size <- (2 * MERC_ORIGIN) / n
  xmin <- -MERC_ORIGIN + x * size
  xmax <- xmin + size
  ymax <-  MERC_ORIGIN - y_xyz * size
  ymin <- ymax - size
  ext(xmin, xmax, ymin, ymax)
}

#' Inverse spherical mercator -> lon/lat degrees (vectorised).
merc_to_lonlat <- function(mx, my) {
  lon <- (mx / MERC_ORIGIN) * 180
  lat <- (2 * atan(exp(my / MERC_R)) - pi / 2) * 180 / pi
  list(lon = lon, lat = lat)
}

#' Lon/lat (EPSG:4326) bounding box of an XYZ tile. Mercator is axis-aligned,
#' so the corners map directly. `margin_cells` pads by N coarse (0.01deg) cells
#' on every side so hillshade and noise have neighbour context.
tile_bbox_4326 <- function(z, x, y_xyz, margin_cells = 0) {
  e  <- tile_extent_3857(z, x, y_xyz)
  ll <- merc_to_lonlat(c(e$xmin, e$xmax), c(e$ymin, e$ymax))
  pad <- margin_cells * 0.01
  ext(min(ll$lon) - pad, max(ll$lon) + pad,
      min(ll$lat) - pad, max(ll$lat) + pad)
}

# -----------------------------------------------------------------------------
# Coarse-data context
# -----------------------------------------------------------------------------

#' Window-read the coarse VRTs that cover a tile (+margin). terra reads only the
#' overlapping blocks, so this is cheap even though the VRTs are global.
#'
#' Returns a list of SpatRasters (EPSG:4326) cropped to bbox4326, any of which
#' may be NULL if that VRT is missing. `elevation` and `water_class` are the
#' ones Phase 1 needs; the rest are read for the synthesis phases.
read_coarse_context <- function(bbox4326, layers = c("elevation", "water_class",
                                                     "water_depth", "biome",
                                                     "capacity")) {
  out <- list()
  for (nm in layers) {
    p <- .tile_vrt_path(nm)
    if (!file.exists(p)) { out[[nm]] <- NULL; next }
    # Materialise inside the try. A .vrt whose source tifs have been pruned
    # OPENS cleanly (it is only XML) and crops cleanly (terra is lazy) -- it
    # fails later, at project() time, outside any handler, which 500s the whole
    # tile over one optional layer. The 2026-08-25 prune left water_influence
    # exactly like that. Windows here are a tile plus 8 cells, so forcing the
    # read costs nothing and turns a dangling layer back into a missing one.
    r <- try({
      rr <- crop(rast(p), bbox4326, snap = "out")
      rr * 1
    }, silent = TRUE)
    if (inherits(r, "try-error") || ncell(r) == 0) { out[[nm]] <- NULL; next }
    out[[nm]] <- r
  }
  out
}

# -----------------------------------------------------------------------------
# Colour ramp (gdaldem color-relief equivalent, in-memory)
# -----------------------------------------------------------------------------

#' Linearly interpolate a value raster through a hex colour ramp, returning a
#' 3-band (R,G,B) 0-255 SpatRaster aligned to `value_rast`. Matches gdaldem
#' color-relief's piecewise-linear interpolation between stops.
colorize_ramp <- function(value_rast, breaks, colors) {
  rgb <- grDevices::col2rgb(colors)               # 3 x N
  v   <- values(value_rast)[, 1]
  out <- rast(value_rast, nlyrs = 3)
  for (b in 1:3) {
    ch <- stats::approx(breaks, rgb[b, ], xout = v, rule = 2)$y
    values(out[[b]]) <- ch
  }
  names(out) <- c("r", "g", "b")
  out
}

#' Map a categorical (biome-code) raster to a 3-band 0-255 RGB SpatRaster using
#' BIOME_INFO colours. Unknown codes -> mid-grey.
colorize_discrete <- function(code_rast, info = BIOME_INFO) {
  codes <- as.integer(names(info))
  rgb   <- grDevices::col2rgb(vapply(info, function(x) x$color, character(1)))
  out <- rast(code_rast, nlyrs = 3)
  v   <- values(code_rast)[, 1]
  for (b in 1:3) {
    lut <- stats::setNames(rgb[b, ], as.character(codes))
    ch  <- lut[as.character(v)]
    ch[is.na(ch)] <- 128
    values(out[[b]]) <- as.numeric(ch)
  }
  names(out) <- c("r", "g", "b")
  out
}

#' Crenellated coastline by domain-warping the CANONICAL water class.
#'
#' water_class.vrt is the authoritative land/water truth (it correctly marks
#' even slightly-positive-elevation ocean cells as OCEAN, so we must NOT
#' re-derive water from the elevation sign). To get a fractal coast we instead
#' warp the *sampling coordinates* by world-seeded noise and read the blocky
#' (nearest-upsampled) water class at the perturbed position -- exactly the
#' synthesize_biome trick. Every sample is a real water_class value from within
#' ~1 coarse cell, so the boundary wiggles organically but the mask never
#' invents ocean in a continental interior nor land in the open sea. Because the
#' warp is deterministic and shared, the elevation and biome tiles get an
#' identical coastline.
#'
#' @param coarse_wc COARSE water_class window (EPSG:4326, with read margin).
#' @param template  Tile grid (EPSG:3857) to produce the mask on.
#' @param warp_m    Peak warp amplitude in metres (~one coarse cell).
#' @return a logical SpatRaster (TRUE = water: OCEAN or LAKE), or NULL.
#'
#' The warp samples the CANONICAL COARSE window (which extends a margin beyond
#' the tile), NOT a tile-local projection -- so a warped sample near a tile edge
#' still finds real data instead of falling off into NA (which previously read as
#' spurious "land in water"). This keeps the coastline seamless across tiles.
#' @param warp_scale optional 0..1 SpatRaster scaling the warp per cell. Near
#'   roads it is driven to 0 so the coastline follows the smooth COARSE boundary
#'   (no crenellation inlet can cut across a road), full warp elsewhere.
synthesize_coastline <- function(coarse_wc, template, warp_m = 900,
                                 base_wavelength_m = nf_wl("coast.warp.x"),
                                 octaves = nf_octaves("coast.warp.x"),
                                 warp_scale = NULL) {
  if (is.null(coarse_wc)) return(NULL)
  # Sample a SMOOTH waterness field (bilinear on the binary water indicator,
  # threshold 0.5), not the nearest-neighbour class codes: where the warp fades
  # to 0 (near roads) an NN sample reproduces the raw 0.01deg STAIRCASE blocks,
  # while the 0.5-contour of the bilinear field is a smooth curve through the
  # same cells. Pure bilinear (no focal) so single-cell lakes/islands survive.
  wbin <- ifel(!is.na(coarse_wc) &
                 (coarse_wc == WATER_CLASS$OCEAN | coarse_wc == WATER_CLASS$LAKE), 1, 0)
  xy <- crds(template, na.rm = FALSE)
  wx <- warp_m * fbm_world(xy[, 1], xy[, 2], octaves = octaves,
                           base_wavelength_m = base_wavelength_m,
                           seed = nf_seed("coast.warp.x"))
  wy <- warp_m * fbm_world(xy[, 1], xy[, 2], octaves = octaves,
                           base_wavelength_m = base_wavelength_m,
                           seed = nf_seed("coast.warp.y"))
  if (!is.null(warp_scale)) {
    ws <- terra::values(warp_scale)[, 1]; ws[is.na(ws)] <- 1
    wx <- wx * ws; wy <- wy * ws
  }
  ll <- merc_to_lonlat(xy[, 1] + wx, xy[, 2] + wy)
  wv <- terra::extract(wbin, cbind(ll$lon, ll$lat), method = "bilinear")[, 1]
  # Return the mask AND the continuous waterness field: waterness rises 0 -> 1
  # approaching the (warped) shore, so a band just under 0.5 is a "near-shore
  # on land" measure -- reused for beach sand and port piers.
  list(mask      = setValues(rast(template), !is.na(wv) & wv >= 0.5),
       waterness = setValues(rast(template), ifelse(is.na(wv), 0, wv)))
}
