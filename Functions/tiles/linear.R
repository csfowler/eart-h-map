# =============================================================================
# linear.R - Rivers and roads: shaping, then drawing
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# Two halves, and the order matters.
#
# SHAPING turns the pipeline's ruler-straight routed lines into geometry that
# knows about terrain: meander_lines() offsets perpendicular with world noise
# (tapering to zero at nodes, or the network comes apart), then shape_roads() and
# shape_rivers() pull vertices down the terrain gradient from terrain.R. This runs
# ONCE at cache build, not per tile.
#
# DRAWING paints them: variable river width, estuary funnels, rapids, road casing,
# bridges, ferry dashes, incised valleys.
#
# STUDENT PROJECT 1 (more realistic roads) lives here. RIVER_TIER is the model to
# copy for a road hierarchy -- rivers have three tiers with their own widths and
# valley profiles, roads have one width for everything.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Linear features: rivers (incised + drawn), roads, bridges
# -----------------------------------------------------------------------------

# River tiers: cartographic line width (px) and the valley carved into the DEM
# (metres): a FLAT floodplain floor of half-width fp_w at full depth, rising
# smoothly to grade over wall_w. Trunk rivers sit in broad flat-bottomed
# valleys the meandering channel wanders inside (the old narrow V-notch read
# as a canal cut). Drawn in the lake/river blue so they blend with lakes.
RIVER_TIER <- list(
  big   = list(width_px = 3.0, fp_w = 380, wall_w = 700, depth = 55),
  med   = list(width_px = 2.0, fp_w = 140, wall_w = 300, depth = 26),
  small = list(width_px = 1.3, fp_w = 45,  wall_w = 120, depth = 12)
)
RIVER_COL  <- c(62, 104, 150)                    # #3e6896, matches lake fill + vector layer
ROAD_CASE  <- c(60, 38, 20)                      # dark casing
ROAD_FILL  <- c(124, 86, 50)                     # lighter road surface
ROAD_CASE_PX <- 3.4; ROAD_FILL_PX <- 1.7         # cartographic widths
ROAD_CLEAR_M <- 45                               # vegetation cleared either side

#' Add deterministic meander to straight vector lines so they sit naturally in
#' the terrain instead of reading as ruler-straight segments. Each line is
#' densified, then every vertex is offset PERPENDICULAR to the line by a
#' world-coordinate noise value (so the same line wiggles identically in every
#' tile that draws it). The offset tapers to 0 at the endpoints so shared nodes
#' (junctions, confluences) stay put. Done once at load -> free per tile.
meander_lines <- function(lines, amp, wl, step = 80, seed = WORLD_SEED, simplify_tol = 0) {
  if (is.null(lines) || !nrow(lines)) return(lines)
  g <- sf::st_segmentize(sf::st_geometry(lines), dfMaxLength = step)
  allco <- sf::st_coordinates(g)
  parts <- split(seq_len(nrow(allco)), allco[, "L1"])    # ordered 1..nrow(lines)
  geoms <- vector("list", length(parts))
  for (j in seq_along(parts)) {
    co <- allco[parts[[j]], 1:2, drop = FALSE]; n <- nrow(co)
    if (n < 3) { geoms[[j]] <- sf::st_linestring(co); next }
    dx <- c(co[2,1]-co[1,1], (co[3:n,1]-co[1:(n-2),1])/2, co[n,1]-co[n-1,1])
    dy <- c(co[2,2]-co[1,2], (co[3:n,2]-co[1:(n-2),2])/2, co[n,2]-co[n-1,2])
    Ln <- sqrt(dx*dx + dy*dy) + 1e-9; nx <- -dy/Ln; ny <- dx/Ln
    off <- amp * fbm_world(co[,1], co[,2], octaves = 3, base_wavelength_m = wl,
                           seed = seed)
    tt <- (seq_len(n) - 1) / (n - 1); off <- off * pmin(1, pmin(tt, 1 - tt) / 0.12)
    co[,1] <- co[,1] + nx*off; co[,2] <- co[,2] + ny*off
    geoms[[j]] <- sf::st_linestring(co)
  }
  out <- sf::st_sf(sf::st_drop_geometry(lines),
                   geometry = sf::st_sfc(geoms, crs = sf::st_crs(lines)))
  if (simplify_tol > 0)                              # drop densified redundancy, keep curves
    out <- suppressWarnings(sf::st_simplify(out, dTolerance = simplify_tol, preserveTopology = FALSE))
  out
}

#' Terrain-aware road shaping: meander + hill avoidance in one pass.
#'
#' Roads drawn from the 0.01deg routing grid are ruler-straight below coarse
#' resolution. Pure-noise meander helps but is terrain-blind: the line still
#' cuts through the synthetic hills the tiles render. Here each densified
#' vertex is also PULLED down the cross-road gradient of the SAME world-seeded
#' fbm field add_microrelief() adds to the elevation (base_wavelength 4000 m),
#' so roads swing around the very knolls that appear at z12+ — deterministic,
#' seamless, computed ONCE at cache build (render cost unchanged).
#'
#' Offsets taper to 0 within `taper_m` of endpoints so junction nodes stay put.
shape_roads <- function(rd, amp_noise = 32, wl_noise = nf_wl("terrain", "wiggle"),
                        pull_k = 26000, pull_iters = 2, pull_h = 45,
                        pull_wl = nf_wl("terrain"), max_off = 110, step = 60,
                        taper_m = 250, simplify_tol = 30,
                        seed = nf_seed("terrain")) {
  if (is.null(rd) || !nrow(rd)) return(rd)
  g <- sf::st_segmentize(sf::st_geometry(rd), dfMaxLength = step)
  allco <- sf::st_coordinates(g)
  parts <- split(seq_len(nrow(allco)), allco[, "L1"])
  geoms <- vector("list", length(parts))
  for (j in seq_along(parts)) {
    co <- allco[parts[[j]], 1:2, drop = FALSE]; n <- nrow(co)
    if (n < 3) { geoms[[j]] <- sf::st_linestring(co); next }
    dx <- c(co[2,1]-co[1,1], (co[3:n,1]-co[1:(n-2),1])/2, co[n,1]-co[n-1,1])
    dy <- c(co[2,2]-co[1,2], (co[3:n,2]-co[1:(n-2),2])/2, co[n,2]-co[n-1,2])
    Ln <- sqrt(dx*dx + dy*dy) + 1e-9; nx <- -dy/Ln; ny <- dx/Ln
    seg <- sqrt(diff(co[,1])^2 + diff(co[,2])^2)
    s0  <- c(0, cumsum(seg)); s1 <- rev(s0)[1] - s0
    taper <- pmin(1, pmin(s0, s1) / taper_m)
    off <- numeric(n)
    for (it in seq_len(pull_iters)) {
      px <- co[,1] + nx*off; py <- co[,2] + ny*off
      eL <- fbm_world(px - pull_h*nx, py - pull_h*ny,
                      octaves = nf_octaves("terrain", "pull"),
                      base_wavelength_m = pull_wl, seed = seed)
      eR <- fbm_world(px + pull_h*nx, py + pull_h*ny,
                      octaves = nf_octaves("terrain", "pull"),
                      base_wavelength_m = pull_wl, seed = seed)
      stp <- -pull_k * (eR - eL) / (2 * pull_h)
      stp <- stats::filter(stp, rep(1/7, 7), sides = 2)       # keep curvature road-like
      stp[is.na(stp)] <- 0
      off <- pmin(pmax(off + as.numeric(stp) * taper, -max_off), max_off)
    }
    wig <- amp_noise * fbm_world(co[,1], co[,2],
                                 octaves = nf_octaves("terrain", "wiggle"),
                                 base_wavelength_m = wl_noise, seed = seed)
    off <- off + wig * taper
    co[,1] <- co[,1] + nx*off; co[,2] <- co[,2] + ny*off
    geoms[[j]] <- sf::st_linestring(co)
  }
  out <- sf::st_sf(sf::st_drop_geometry(rd),
                   geometry = sf::st_sfc(geoms, crs = sf::st_crs(rd)))
  if (simplify_tol > 0)
    out <- suppressWarnings(sf::st_simplify(out, dTolerance = simplify_tol, preserveTopology = FALSE))
  out
}

#' Terrain-aware river shaping — the shape_roads idea with much wider range.
#'
#' The routed river lines are straight staircase traces of the coarse grid, and
#' a 16 m noise meander left them reading as canals. Here every densified
#' vertex gets (a) a MULTI-BAND perpendicular meander — sweeping valley-scale
#' bends, mid-scale loops, fine wiggle — and (b) the same cross-line terrain
#' pull as roads (down the gradient of the micro-relief fbm), so rivers drift
#' into the synthetic hollows. incise_rivers() carves the valley along the
#' SHAPED geometry, so the valley follows the bends. Offsets taper to 0 at
#' segment endpoints, which are the confluence nodes -> the network stays
#' connected. All noise is world-anchored: deterministic + seamless.
#'
#' Unlike shape_roads (887 lines), this runs over ~13k lines, so the fbm is
#' evaluated in ONE batch across all vertices; the per-line loop only does
#' cheap bookkeeping.
shape_rivers <- function(rv, step = 70,
                         bands = lapply(seq_len(nf("river.meander")$active),
                                        function(i) c(amp = nf_amp("river.meander", i),
                                                      wl  = nf_wl("river.meander", i))),
                         pull_k = 40000, pull_h = 60,
                         pull_wl = nf_wl("terrain"),
                         anchor_k = 3000, anchor_h = 120,
                         max_off = 220, taper_m = 180, simplify_tol = 20,
                         seed = nf_seed("terrain")) {
  if (is.null(rv) || !nrow(rv)) return(rv)
  g  <- sf::st_segmentize(sf::st_geometry(rv), dfMaxLength = step)
  co <- sf::st_coordinates(g)
  L  <- co[, "L1"]; x <- co[, 1]; y <- co[, 2]; n <- length(x)

  # Per-part central-difference tangents/normals (index arithmetic, no loop).
  first <- c(TRUE, L[-1] != L[-n]); last <- c(first[-1], TRUE)
  ip <- seq_len(n) - 1L; ip[first] <- which(first)          # prev (clamped in-part)
  in_ <- seq_len(n) + 1L; in_[last] <- which(last)          # next (clamped in-part)
  dx <- x[in_] - x[ip]; dy <- y[in_] - y[ip]
  Ln <- sqrt(dx * dx + dy * dy) + 1e-9
  nx <- -dy / Ln; ny <- dx / Ln

  # Batched world-anchored offsets: meander bands + one terrain-pull pass.
  off <- numeric(n)
  for (i in seq_along(bands)) {
    b <- bands[[i]]
    off <- off + b["amp"] * fbm_world(x, y,
                                      octaves = nf_octaves("river.meander"),
                                      base_wavelength_m = b["wl"],
                                      seed = nf_seed("river.meander", i))
  }
  eL <- fbm_world(x - pull_h * nx, y - pull_h * ny,
                  octaves = nf_octaves("terrain", "pull"),
                  base_wavelength_m = pull_wl, seed = seed)
  eR <- fbm_world(x + pull_h * nx, y + pull_h * ny,
                  octaves = nf_octaves("terrain", "pull"),
                  base_wavelength_m = pull_wl, seed = seed)
  off <- off - pull_k * (eR - eL) / (2 * pull_h)

  # ANCHOR pull: the coarse DEM already carries the canonical river valleys
  # (WorldBuilder hydrology), and terrain-blind meander shoves the channel up
  # the valley side -- river beside its own valley. Sampling the elevation VRT
  # across the line and stepping downhill keeps the bends swinging WITHIN the
  # real valley. Batched in chunks: points along a line are spatially local,
  # so the windowed VRT reads stay cheap.
  ev <- try(rast(.tile_vrt_path("elevation")), silent = TRUE)
  if (!inherits(ev, "try-error")) {
    exv <- function(px, py) {
      out <- numeric(length(px))
      for (i0 in seq(1, length(px), by = 500000)) {
        i1 <- min(i0 + 499999, length(px))
        ll <- merc_to_lonlat(px[i0:i1], py[i0:i1])
        out[i0:i1] <- terra::extract(ev, cbind(ll$lon, ll$lat))[, 1]
      }
      out
    }
    aL <- exv(x - anchor_h * nx, y - anchor_h * ny)
    aR <- exv(x + anchor_h * nx, y + anchor_h * ny)
    ga <- (aR - aL) / (2 * anchor_h); ga[is.na(ga)] <- 0
    off <- off - anchor_k * pmin(pmax(ga, -0.12), 0.12)   # slope-capped downhill step
  }
  off <- pmin(pmax(off, -max_off), max_off)

  # Arclength taper to fixed confluence endpoints (per part, vectorised).
  seg <- sqrt(diff(x)^2 + diff(y)^2); seg[first[-1]] <- 0
  cs  <- cumsum(c(0, seg))
  s_start <- cs - rep(cs[first], times = tabulate(L))
  part_len <- ave(s_start, L, FUN = max)
  taper <- pmin(1, pmin(s_start, part_len - s_start) / taper_m)
  x <- x + nx * off * taper; y <- y + ny * off * taper

  idx <- split(seq_len(n), L)
  geoms <- lapply(idx, function(ii)
    if (length(ii) >= 2) sf::st_linestring(cbind(x[ii], y[ii])) else NULL)
  keep <- !vapply(geoms, is.null, logical(1))
  out <- sf::st_sf(sf::st_drop_geometry(rv)[as.integer(names(idx))[keep], , drop = FALSE],
                   geometry = sf::st_sfc(geoms[keep], crs = sf::st_crs(rv)))
  if (simplify_tol > 0)
    out <- suppressWarnings(sf::st_simplify(out, dTolerance = simplify_tol, preserveTopology = FALSE))
  out
}

#' Split ferry-flagged routes into LAND parts and true WATER CROSSINGS.
#'
#' RoadBuilder's is_ferry is a per-EDGE flag, but a rescued edge is typically
#' ~98% land: e.g. pk 385_536 is a 445 km shore-hugging road with ONE 6.7 km
#' lake-neck crossing and dozens of sub-500 m bay grazes. Rendering the whole
#' edge as ferry dashes (and excluding it from the cased-road pipeline, coast
#' warp suppression, and the water-mask carve) is what made these routes read
#' as chains of fictional ferries instead of coastal roads.
#'
#' Here each ferry edge is densified and classified against water_class; only
#' contiguous water runs >= min_cross_m become is_ferry crossing stubs (dash
#' render + offshore nudge). Everything else becomes an ordinary road part:
#' cased line, warp suppression, corridor carve (bends around bays), roadside
#' buildings. Short grazes stay inside the land parts, where the road corridor
#' carve force-lands them — the coast-hugging behaviour.
#'
#' Crossings separated by < merge_gap_m of land are merged (no road slivers);
#' each crossing keeps one shore vertex at each end so parts stay connected
#' (shape_roads/shape_ferries both pin endpoints, preserving continuity).
split_ferry_edges <- function(rd, min_cross_m = 1500, merge_gap_m = 600, step = 300) {
  isf <- if ("is_ferry" %in% names(rd)) !is.na(rd$is_ferry) & rd$is_ferry else rep(FALSE, nrow(rd))
  if (!any(isf)) return(rd)
  wc <- try(rast(.tile_vrt_path("water_class")), silent = TRUE)
  if (inherits(wc, "try-error")) return(rd)
  out <- list(rd[!isf, ])
  for (j in which(isf)) {
    ln <- rd[j, ]
    g  <- sf::st_segmentize(sf::st_geometry(ln), dfMaxLength = step)
    co <- sf::st_coordinates(g)[, 1:2, drop = FALSE]
    n  <- nrow(co)
    ll <- merc_to_lonlat(co[, 1], co[, 2])
    v  <- terra::extract(wc, cbind(ll$lon, ll$lat))[, 1]
    wet <- !is.na(v) & (v == WATER_CLASS$OCEAN | v == WATER_CLASS$LAKE |
                        v == WATER_CLASS$RIVER_NAV)
    cs <- c(0, cumsum(sqrt(diff(co[, 1])^2 + diff(co[, 2])^2)))
    r  <- rle(wet); rends <- cumsum(r$lengths); rstarts <- rends - r$lengths + 1
    cross <- data.frame(i0 = rstarts[r$values], i1 = rends[r$values])
    if (nrow(cross)) cross <- cross[cs[cross$i1] - cs[cross$i0] >= min_cross_m, , drop = FALSE]
    if (nrow(cross) == 0) {                          # grazes only -> plain road
      ln$is_ferry <- FALSE
      out[[length(out) + 1]] <- ln
      next
    }
    if (nrow(cross) > 1) {                           # merge near-adjacent crossings
      acc <- list(); m <- cross[1, ]
      for (k in 2:nrow(cross)) {
        if (cs[cross$i0[k]] - cs[m$i1] < merge_gap_m) m$i1 <- cross$i1[k]
        else { acc[[length(acc) + 1]] <- m; m <- cross[k, ] }
      }
      acc[[length(acc) + 1]] <- m
      cross <- do.call(rbind, acc)
    }
    pieces <- list(); prev <- 1L
    for (k in seq_len(nrow(cross))) {
      a <- max(cross$i0[k] - 1L, 1L); b <- min(cross$i1[k] + 1L, n)   # shore anchors
      if (a > prev) pieces[[length(pieces) + 1]] <- list(idx = prev:a, ferry = FALSE)
      pieces[[length(pieces) + 1]] <- list(idx = a:b, ferry = TRUE)
      prev <- b
    }
    if (prev < n) pieces[[length(pieces) + 1]] <- list(idx = prev:n, ferry = FALSE)
    for (k in seq_along(pieces)) {
      p <- pieces[[k]]
      if (length(p$idx) < 2) next
      seg <- ln
      sf::st_geometry(seg) <- sf::st_sfc(sf::st_linestring(co[p$idx, , drop = FALSE]),
                                         crs = sf::st_crs(rd))
      seg$is_ferry <- p$ferry
      seg$pk <- paste0(ln$pk, if (p$ferry) "_F" else "_L", k)
      out[[length(out) + 1]] <- seg
    }
  }
  do.call(rbind, out)
}

#' Nudge ferry-route vertices OFFSHORE. Phase-1 ocean routing ran at 0.25deg,
#' so ferry lines often sit on the modern fine coastline (drawn dashes ride
#' the beach). Each on-land or near-shore vertex steps toward the most watery
#' of 8 bearings (water fraction sampled from the canonical water_class VRT)
#' until it is comfortably at sea. Endpoints (ports) stay fixed.
shape_ferries <- function(rd, step_m = 260, iters = 5, probe_m = 520) {
  isf <- if ("is_ferry" %in% names(rd)) !is.na(rd$is_ferry) & rd$is_ferry else rep(FALSE, nrow(rd))
  if (!any(isf)) return(rd)
  wc <- try(rast(.tile_vrt_path("water_class")), silent = TRUE)
  if (inherits(wc, "try-error")) return(rd)
  is_wet <- function(px, py) {
    ll <- merc_to_lonlat(px, py)
    v  <- terra::extract(wc, cbind(ll$lon, ll$lat))[, 1]
    !is.na(v) & (v == WATER_CLASS$OCEAN | v == WATER_CLASS$LAKE)
  }
  th <- (0:7) * pi / 4
  g  <- sf::st_geometry(rd)
  for (j in which(isf)) {
    co <- sf::st_coordinates(g[j])[, 1:2, drop = FALSE]; n <- nrow(co)
    if (n < 3) next
    mov <- 2:(n - 1)                                   # keep port endpoints
    for (it in seq_len(iters)) {
      # a vertex is "beached" if it or any close probe touches land
      wet_c <- is_wet(co[mov, 1], co[mov, 2])
      wfrac <- rowMeans(vapply(th, function(a)
        is_wet(co[mov, 1] + probe_m * cos(a), co[mov, 2] + probe_m * sin(a)),
        logical(length(mov))))
      need <- !wet_c | wfrac < 0.7
      if (!any(need)) break
      # step toward the most watery bearing (wetness at 2 probe radii)
      best <- rep(0, length(mov))
      bw   <- rep(-1, length(mov))
      for (a in th) {
        w <- (is_wet(co[mov, 1] + probe_m * cos(a), co[mov, 2] + probe_m * sin(a)) +
              is_wet(co[mov, 1] + 2 * probe_m * cos(a), co[mov, 2] + 2 * probe_m * sin(a))) / 2
        upd <- w > bw; best[upd] <- a; bw[upd] <- w[upd]
      }
      co[mov[need], 1] <- co[mov[need], 1] + step_m * cos(best[need])
      co[mov[need], 2] <- co[mov[need], 2] + step_m * sin(best[need])
    }
    g[j] <- sf::st_linestring(co)
  }
  sf::st_geometry(rd) <- g
  rd
}

# Lazily-loaded, reprojected (EPSG:3857) road + river vectors, cached for the
# life of the process (server) or script. Lines are meandered at load.
#
# Roads and rivers carry SEPARATE stamps so they invalidate independently: a
# change to the river meander bands must not throw away the road cache. What
# they share is `terrain` -- both pull their vertices down its gradient -- so a
# change there correctly invalidates both, including when it is made by someone
# editing hydrology who has never opened this file.
#
# (This replaces ROAD_SHAPE_VERSION / RIVER_SHAPE_VERSION. Those were integers a
#  developer had to remember to bump, and which two forks would both bump to the
#  same value and merge without conflict. See Functions/Provenance.R.)
.tilevec <- new.env(parent = emptyenv())
.tile_vec_cache <- function() map_path("data", "tile_vectors_3857.rds")

.prov_roads_stamp <- function() prov_stamp(list(
  source = prov_file("Input Data/Roads/road_routes.rds"),
  # The whole file, not a list of names: everything that shapes a road lives
   # here, and a helper added next week is covered without anyone remembering.
   # It over-invalidates slightly -- editing how rivers are PAINTED rebuilds the
   # road geometry too, ~3 minutes -- which is the right way round, because the
   # alternative is geometry that is silently stale.
  code   = prov_rfile("Functions/tiles/linear.R", "Functions/tiles/core.R"),
  fields = prov_field("terrain"),
  consts = prov_const(WORLD_SEED = WORLD_SEED)))

.prov_rivers_stamp <- function() prov_stamp(list(
  # The river tier geojsons live in the MAP, not the repo, and are rebuilt by
  # build_reference_map(); stamp them so a retile invalidates the shaping.
  source = prov_file(map_path("data", "rivers_big.geojson",   require_exists = FALSE),
                     map_path("data", "rivers_med.geojson",   require_exists = FALSE),
                     map_path("data", "rivers_small.geojson", require_exists = FALSE)),
  code   = prov_rfile("Functions/tiles/linear.R", "Functions/tiles/core.R"),
  fields = prov_field("terrain", "river.meander"),
  consts = prov_const(WORLD_SEED = WORLD_SEED)))

#' Load (and cache) the meandered EPSG:3857 road + river vectors. Meandering 13k
#' rivers takes ~3 min, so the result is cached to disk; later starts read it
#' instantly. Pass force = TRUE (or delete the cache) to rebuild.
get_tile_vectors <- function(force = FALSE) {
  if (isTRUE(.tilevec$loaded)) return(.tilevec)
  cache <- .tile_vec_cache()

  rs <- .prov_roads_stamp(); vs <- .prov_rivers_stamp()
  obj <- NULL
  if (file.exists(cache) && !force) {
    raw <- try(readRDS(cache), silent = TRUE)
    if (!inherits(raw, "try-error") && is.list(raw)) obj <- raw
  }
  # Two stamps in one file: the halves are rebuilt independently but share a
  # cache, because one .rds keeps the on-disk story simple.
  roads_ok  <- !is.null(obj) && identical(obj$.prov_roads$combined,  rs$combined)
  rivers_ok <- !is.null(obj) && identical(obj$.prov_rivers$combined, vs$combined)
  if (!is.null(obj) && !roads_ok)
    message("  rebuilding road geometry: ", prov_why(obj$.prov_roads, rs))
  if (!is.null(obj) && !rivers_ok)
    message("  rebuilding river geometry: ", prov_why(obj$.prov_rivers, vs))

  if (roads_ok) {
    .tilevec$roads <- obj$roads; .tilevec$roads_bb <- obj$roads_bb
  } else {
    rd <- try(readRDS(file.path(.tile_root(), "Input Data", "Roads", "road_routes.rds")), silent = TRUE)
    .tilevec$roads <- if (!inherits(rd, "try-error"))
      shape_ferries(shape_roads(split_ferry_edges(sf::st_transform(rd, 3857)))) else NULL
    .tilevec$roads_bb <- .feature_bboxes(.tilevec$roads)
  }
  if (rivers_ok) {
    .tilevec$rivers <- obj$rivers; .tilevec$rivers_bb <- obj$rivers_bb
  } else {
    rivs <- list()
    for (t in c("big", "med", "small")) {
      f <- map_path("data", paste0("rivers_", t, ".geojson"))
      if (file.exists(f)) {
        g <- try(sf::st_read(f, quiet = TRUE), silent = TRUE)
        if (!inherits(g, "try-error") && nrow(g)) { g$tier <- t; rivs[[t]] <- g["tier"] }
      }
    }
    .tilevec$rivers <- if (length(rivs))
      shape_rivers(sf::st_transform(do.call(rbind, rivs), 3857)) else NULL
    .tilevec$rivers_bb <- .feature_bboxes(.tilevec$rivers)
  }
  if (!roads_ok || !rivers_ok) {
    dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
    try(saveRDS(list(roads = .tilevec$roads, rivers = .tilevec$rivers,
                     roads_bb = .tilevec$roads_bb, rivers_bb = .tilevec$rivers_bb,
                     .prov_roads = rs, .prov_rivers = vs), cache), silent = TRUE)
  }
  .tilevec$loaded <- TRUE
  .tilevec
}

.feature_bboxes <- function(x) {
  if (is.null(x) || !nrow(x)) return(NULL)
  co <- sf::st_coordinates(x)
  L  <- co[, ncol(co)]                               # top-level feature id
  data.frame(xmin = tapply(co[, 1], L, min), xmax = tapply(co[, 1], L, max),
             ymin = tapply(co[, 2], L, min), ymax = tapply(co[, 2], L, max))
}

#' Select the road/river features overlapping a tile (SpatExtent, EPSG:3857) via
#' a cheap numeric bbox test. Features are NOT clipped -- rasterize/buffer in the
#' draw helpers clip to the tile grid -- so this is fast even with 13k rivers.
crop_vectors <- function(ext3857) {
  v <- get_tile_vectors()
  x0 <- terra::xmin(ext3857); x1 <- terra::xmax(ext3857)
  y0 <- terra::ymin(ext3857); y1 <- terra::ymax(ext3857)
  sel <- function(x, bb) {
    if (is.null(x) || is.null(bb)) return(x[0, ])
    idx <- which(bb$xmin <= x1 & bb$xmax >= x0 & bb$ymin <= y1 & bb$ymax >= y0)
    x[idx, ]
  }
  list(roads = sel(v$roads, v$roads_bb), rivers = sel(v$rivers, v$rivers_bb))
}

#' Per-tier distance-to-centerline rasters (metres), computed ONCE per tile and
#' shared by incise_rivers (valley profile) and draw_rivers (variable width) --
#' the rasterize+distance pass was previously duplicated between them.
river_tier_distance <- function(template, rivers) {
  if (is.null(rivers) || nrow(rivers) == 0) return(NULL)
  out <- list()
  for (t in names(RIVER_TIER)) {
    sub <- rivers[rivers$tier == t, ]
    if (nrow(sub) == 0) next
    m <- terra::rasterize(terra::vect(sub), template, field = 1)
    if (all(is.na(values(m)))) next
    out[[t]] <- terra::distance(m)
  }
  if (length(out)) out else NULL
}

#' River-valley carve + floodplain weight for a tile.
#'
#' Returns `carve` (metres to subtract from the anchor: flat floor at full tier
#' depth within fp_w, smoothstepping to grade over wall_w) and `fpw` (0..1
#' floodplain weight, used to DAMP micro-relief noise -- alluvium is flat --
#' and to BOOST valleyness so botany meadows and the dendritic tributary
#' incision concentrate along the rivers).
#'
#' SEAMLESSNESS: the valley reach (fp_w + wall_w, ~1.1 km for trunks) is far
#' wider than the 10 px render pad, so a river just outside the pad must still
#' carve this tile. The distances are therefore computed on a COARSENED grid
#' expanded margin_m beyond the padded tile and projected back -- the same
#' rule as every other neighbourhood op (sample beyond the tile, never clip at
#' its edge). The ~coarse-cell distance error is invisible in a smooth profile.
river_valley_fields <- function(template, rivers, margin_m = 1500, coarsen = 4) {
  if (is.null(rivers) || nrow(rivers) == 0) return(NULL)
  te   <- ext(template)
  vext <- ext(te$xmin - margin_m, te$xmax + margin_m,
              te$ymin - margin_m, te$ymax + margin_m)
  vg <- rast(vext, resolution = res(template)[1] * coarsen, crs = crs(template))
  carve <- fpw <- NULL
  for (t in names(RIVER_TIER)) {
    sub <- rivers[rivers$tier == t, ]
    if (nrow(sub) == 0) next
    m <- terra::rasterize(terra::vect(sub), vg, field = 1)
    if (all(is.na(values(m)))) next
    d <- terra::distance(m)
    p <- RIVER_TIER[[t]]
    w <- clamp((p$fp_w + p$wall_w - d) / p$wall_w, 0, 1)
    w <- w * w * (3 - 2 * w)
    cv <- w * p$depth
    carve <- if (is.null(carve)) cv else max(carve, cv)
    fpw   <- if (is.null(fpw))   w  else max(fpw, w)
  }
  if (is.null(carve)) return(NULL)
  list(carve = project(carve, template, method = "bilinear"),
       fpw   = project(fpw,   template, method = "bilinear"))
}

#' Logical mask (TRUE) of a buffered set of lines rasterised to `template`.
.line_mask <- function(lines, template, width_m) {
  if (is.null(lines) || nrow(lines) == 0) return(NULL)
  buf <- sf::st_buffer(lines, width_m)
  m <- terra::rasterize(terra::vect(buf), template, field = 1)
  !is.na(m)
}

.paint <- function(comp, msk, col) {
  if (is.null(msk)) return(comp)
  for (b in 1:3) comp[[b]] <- ifel(msk, col[b], comp[[b]])
  comp
}

#' Vegetation-suppression mask [0,1] over the river-valley + road corridors so
#' canopy thins to a cleared strip along water and roads.
corridor_mask <- function(template, rivers, roads) {
  xres <- res(template)[1]; msk <- template * 0
  if (!is.null(rivers) && nrow(rivers)) {
    rm <- .line_mask(rivers, template, 55)
    if (!is.null(rm)) msk <- ifel(rm, 1, msk)
  }
  if (!is.null(roads) && nrow(roads)) {
    dm <- .line_mask(roads, template, ROAD_CLEAR_M)
    if (!is.null(dm)) msk <- ifel(dm, 1, msk)
  }
  msk
}

#' Minor streams (z13+): faint creeks along the zero-set of the SAME channel
#' noise drainage_incision carves (seeds +503/+504), gated by valleyness -- so
#' every creek lies in its own already-carved mini-valley and fades in where
#' the land drains toward the real rivers. Drawn under the canonical rivers.
draw_streams <- function(comp, valleyness, water_mask = NULL, zoom = 12) {
  if (zoom < 13L || is.null(valleyness)) return(comp)
  xy <- crds(comp, na.rm = FALSE); mx <- xy[, 1]; my <- xy[, 2]
  ssf <- function(x, e0, e1) { t <- pmin(pmax((x - e0) / (e1 - e0), 0), 1); t * t * (3 - 2 * t) }
  n3 <- gen_simplex(mx, my, frequency = 1 / nf_wl("drainage", 3),
                    seed = nf_seed("drainage", 3))
  n4 <- gen_simplex(mx, my, frequency = 1 / nf_wl("drainage", 4),
                    seed = nf_seed("drainage", 4))
  vy <- terra::values(valleyness)[, 1]; vy[is.na(vy)] <- 0
  gate <- ssf(vy, 0.25, 0.6)
  a <- pmin((pmax(0, 1 - abs(n3) / 0.030) * 0.8 +
             pmax(0, 1 - abs(n4) / 0.024) * 0.55) * gate, 0.75)
  if (!is.null(water_mask)) {
    wv <- terra::values(water_mask)[, 1]
    a[!is.na(wv) & wv > 0] <- 0
  }
  if (!any(a > 0.02)) return(comp)
  STREAM_COL <- c(66, 102, 128)
  cm <- terra::values(comp)
  for (b in 1:3) cm[, b] <- cm[, b] * (1 - a) + STREAM_COL[b] * a
  setValues(comp, cm)
}

#' Draw rivers (blue, width by tier) onto the composite.
#'
#' Width is VARIABLE: the tier's cartographic width is the baseline (per the
#' user: tier classification is width enough), modulated by two world-anchored
#' noise bands -- a slow "breathing" so reaches widen and narrow (0.6-1.5x)
#' and a fine raggedness so the banks aren't buffer-parallel canal walls. The
#' mask is distance-to-centerline (tier_d, shared with incise_rivers) compared
#' against that spatially-varying half-width; a floor keeps the channel from
#' pinching shut. World-anchored => the same reach is wide at every zoom and
#' across tile seams.
#'
#' `water_mask` clips the drawn line to LAND: the vectorised D8 downstream
#' segments continue across below-sea-level margin cells, so trunk rivers
#' otherwise draw straight across the open ocean; clipping also ends estuaries
#' exactly at the crenellated shore.
#' @param waterness Continuous shore-proximity field: drives the ESTUARY
#'   funnel — the channel widens up to ~4.5x approaching the 0.5 contour, so
#'   rivers visibly open into the sea/lake instead of ending as a clipped
#'   stroke. Sandbars stipple the widened mouth at z13+.
#' @param slope_deg Coarse-anchor slope (degrees): steep reaches get white
#'   RAPIDS flecks at z13+ (the anchor, not the carved surface, so the foam
#'   marks real descents rather than our own valley walls).
draw_rivers <- function(comp, rivers, water_mask = NULL, tier_d = NULL,
                        waterness = NULL, slope_deg = NULL, zoom = 12) {
  if (is.null(rivers) || nrow(rivers) == 0) return(comp)
  xres <- res(comp)[1]
  wland <- if (is.null(water_mask)) NULL else !water_mask
  if (is.null(tier_d)) tier_d <- river_tier_distance(comp, rivers)
  if (is.null(tier_d)) return(comp)
  xy <- crds(comp, na.rm = FALSE)
  ssf <- function(x, e0, e1) { t <- pmin(pmax((x - e0) / (e1 - e0), 0), 1); t * t * (3 - 2 * t) }
  breathe <- 0.5 + 0.5 * fbm_world(xy[, 1], xy[, 2], octaves = nf_octaves("river.breathe"),
                                   base_wavelength_m = nf_wl("river.breathe"), seed = nf_seed("river.breathe"))
  ragged  <- fbm_world(xy[, 1], xy[, 2], octaves = nf_octaves("river.ragged"),
                       base_wavelength_m = nf_wl("river.ragged"), seed = nf_seed("river.ragged"))
  wn <- if (!is.null(waterness)) { w <- terra::values(waterness)[, 1]; w[is.na(w)] <- 0; w }
        else rep(0, nrow(xy))
  funnel <- 1 + 3.5 * ssf(wn, 0.30, 0.50)                      # estuary widening
  fine   <- zoom >= 13L
  bar_n  <- if (fine) fbm_world(xy[, 1], xy[, 2], octaves = nf_octaves("river.sandbar"),
                                base_wavelength_m = nf_wl("river.sandbar"), seed = nf_seed("river.sandbar")) else NULL
  foam_n <- if (fine) fbm_world(xy[, 1], xy[, 2], octaves = nf_octaves("river.foam"),
                                base_wavelength_m = nf_wl("river.foam"), seed = nf_seed("river.foam")) else NULL
  sv <- if (!is.null(slope_deg)) { s <- terra::values(slope_deg)[, 1]; s[is.na(s)] <- 0; s }
        else NULL
  SANDBAR <- c(198, 183, 148); FOAM <- c(224, 234, 240)
  cm <- terra::values(comp)
  for (t in c("small", "med", "big")) {                        # big last = on top
    if (is.null(tier_d[[t]])) next
    bh   <- RIVER_TIER[[t]]$width_px * xres / 2                # baseline half-width (m)
    half <- (bh * (0.6 + 0.9 * breathe) + 0.35 * bh * ragged) * funnel
    dv   <- terra::values(tier_d[[t]])[, 1]
    inch <- !is.na(dv) & dv <= pmax(half, 0.4 * bh)
    if (!is.null(wland)) inch <- inch & terra::values(wland)[, 1]
    if (!any(inch)) next
    for (b in 1:3) cm[inch, b] <- RIVER_COL[b]
    if (fine) {
      # sandbars: mid-channel bars inside the widened mouth
      bar <- inch & wn > 0.36 & wn < 0.49 & bar_n > 0.52 & dv > 0.25 * half
      for (b in 1:3) cm[bar, b] <- SANDBAR[b]
      # rapids: foam flecks where the anchor descends steeply
      if (!is.null(sv)) {
        rap <- inch & sv > 2.2 & foam_n > 0.28
        for (b in 1:3) cm[rap, b] <- FOAM[b]
      }
    }
  }
  setValues(comp, cm)
}

#' Draw roads (cased) onto the composite, ABOVE rivers so crossings read as
#' bridges. Where a road would cross OCEAN/LAKE, its over-water pixels are SNAPPED
#' onto the nearest shoreline cell, so the road hugs the warped lake edge instead
#' of ploughing across it (the old behaviour) or dead-ending at the shore (the
#' clipped behaviour). Narrow navigable rivers aren't in water_mask, so genuine
#' bridges still draw straight across.
draw_roads <- function(comp, roads, water_mask = NULL, zoom = 12) {
  if (is.null(roads) || nrow(roads) == 0) return(comp)
  xres <- res(comp)[1]; zf <- 1 + 0.18 * max(0, zoom - 12)
  is_ferry <- if ("is_ferry" %in% names(roads)) !is.na(roads$is_ferry) & roads$is_ferry else rep(FALSE, nrow(roads))

  # Ferry routes: muted dashes over the water (arclength-parity dashing, so the
  # pattern is a property of the world-fixed line -> identical across tiles).
  fer <- roads[is_ferry, ]
  if (nrow(fer) > 0) {
    FERRY_COL <- c(104, 92, 74)
    dash <- 170                                       # dash + gap length (m)
    gg <- sf::st_segmentize(sf::st_geometry(fer), dfMaxLength = 55)
    co <- sf::st_coordinates(gg)
    segs <- list(); k <- 0L
    for (lid in unique(co[, "L1"])) {
      cc <- co[co[, "L1"] == lid, 1:2, drop = FALSE]
      if (nrow(cc) < 2) next
      cl <- c(0, cumsum(sqrt(diff(cc[, 1])^2 + diff(cc[, 2])^2)))
      on <- (floor(cl / dash) %% 2L) == 0L
      for (i in seq_len(nrow(cc) - 1)) if (on[i] && on[i + 1]) {
        k <- k + 1L; segs[[k]] <- sf::st_linestring(cc[i:(i + 1), ])
      }
    }
    if (k > 0) {
      dsf <- sf::st_sf(geometry = sf::st_sfc(segs, crs = sf::st_crs(fer)))
      comp <- .paint(comp, .line_mask(dsf, comp, 1.4 * zf * xres / 2), FERRY_COL)
    }
  }

  land <- roads[!is_ferry, ]
  if (nrow(land) == 0) return(comp)
  caseR <- .line_mask(land, comp, ROAD_CASE_PX * zf * xres / 2)
  fillR <- .line_mask(land, comp, ROAD_FILL_PX * zf * xres / 2)
  if (is.null(fillR)) return(comp)

  # Roads draw as a plain cased line on land. They stay off water because the
  # caller carves the road corridor OUT of the water mask (force-land) before
  # compositing, so the warped lake/coast edge bends around the road rather than
  # the road crossing/dead-ending into water. (water_mask kept for signature
  # compatibility; no longer used here.)
  comp <- .paint(comp, caseR, ROAD_CASE)
  comp <- .paint(comp, fillR, ROAD_FILL)
  comp
}
