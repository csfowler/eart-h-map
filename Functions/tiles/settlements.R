# =============================================================================
# settlements.R - Clearings, field mosaics, lanes, buildings, walls
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# Settlement footprints and everything drawn inside them: the urban/farmland
# intensity fields with noise-wobbled radii, the Worley field patchwork, radial
# lanes, buildings placed three ways (roadside ribbons, hash-thinned lattice,
# scattered farmsteads), wall rings with gates, and piers for ports.
#
# STUDENT PROJECT 4 (settlements that look real) lives here.
#
# .hash01() is the hinge: every property of every building must be a pure function
# of its world position, or buildings change shape as you pan. Derive size and
# orientation from the hash, never from anything tile-relative.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Settlements: clearings, field mosaics, radial paths, buildings
# -----------------------------------------------------------------------------
# Below coarse resolution a settlement is invisible: the canon gives a point +
# population, and the tiles must invent the rest. Everything here is a pure
# function of world position + settlement attributes (id, pop), so every tile
# that overlaps a settlement renders the identical clearing/paths/buildings.
#
#   clearing+fields  all procedural zooms (reads as a cleared patch from afar)
#   radial paths     z >= SETTLE_PATH_MIN_Z
#   buildings        z >= SETTLE_BLDG_MIN_Z (1px; 2x2px at z14) + farmsteads

SETTLE_PATH_MIN_Z <- 12L
SETTLE_BLDG_MIN_Z <- 13L
BLDG_LATTICE_M    <- 34            # world-anchored building lattice (merc m)

PATH_COL <- c(150, 118, 82); PATH_PX <- 1.4
BLDG_COL <- c(74, 55, 40)          # roof/timber base, hash-varied per building
URBAN_COL <- c(178, 160, 128)      # packed-earth / plaster town ground
FIELD_PALETTE <- rbind(            # field patch tints: grain, pasture, fallow, plough
  c(201, 180, 118),
  c(164, 178, 103),
  c(181, 154,  96),
  c(148, 122,  82))

# Deterministic [0,1) hash of integer lattice coords / ids (world-seeded).
.hash01 <- function(a, b = 0L, k = 1L) {
  x <- sin(a * 127.1 + b * 311.7 + k * 74.7 + WORLD_SEED * 0.618) * 43758.5453
  x - floor(x)
}

.tile_settle_cache <- function() map_path("data", "tile_settlements_3857.rds")

# Settlement footprints depend on the settlement canon, the lane meander field,
# and .hash01 (which seeds every per-building decision off WORLD_SEED). They do
# NOT depend on `terrain`, so a hydrology change does not force this rebuild.
.prov_setts_stamp <- function() prov_stamp(list(
  source = prov_file("Input Data/Combined/settlements_final.rds"),
  # Settlement geometry reads meander_lines() and .hash01() from linear.R and
  # core.R, so both are dependencies -- but NOT terrain.R. Settlements do not
  # read the terrain field, and a change to it must leave this cache alone;
  # tests/test-provenance.R pins that.
  code   = prov_rfile("Functions/tiles/settlements.R",
                      "Functions/tiles/linear.R", "Functions/tiles/core.R"),
  fields = prov_field("path.settlement", "settle.wobble1", "settle.wobble2")))

#' Load (and cache) settlements in EPSG:3857 with derived radii + the radial
#' path lines. Separate cache file from tile_vectors so adding this phase does
#' not force the ~3 min river re-meander.
get_tile_settlements <- function(force = FALSE) {
  if (isTRUE(.tilevec$setts_loaded) && !force) return(.tilevec)
  cache <- .tile_settle_cache()
  st <- .prov_setts_stamp()
  hit <- cache_load(cache, st, force = force)
  if (hit$hit) {
    .tilevec$setts <- hit$obj$setts; .tilevec$spaths <- hit$obj$spaths
    .tilevec$spaths_bb <- hit$obj$spaths_bb; .tilevec$setts_loaded <- TRUE
    return(.tilevec)
  }
  f <- file.path(.tile_root(), "Input Data", "Combined", "settlements_final.rds")
  s <- try(readRDS(f), silent = TRUE)
  if (inherits(s, "try-error")) {
    .tilevec$setts <- NULL; .tilevec$spaths <- NULL; .tilevec$spaths_bb <- NULL
    .tilevec$setts_loaded <- TRUE
    return(.tilevec)
  }
  if (inherits(s, "sf")) s <- cbind(sf::st_drop_geometry(s), sf::st_coordinates(s))
  s <- s[!is.na(s$lon) & !is.na(s$lat) & abs(s$lat) < 84.5, ]   # mercator range
  pop  <- pmax(s$population, 30)
  pts  <- sf::st_transform(sf::st_as_sf(s, coords = c("lon", "lat"), crs = 4326), 3857)
  xy   <- sf::st_coordinates(pts)
  mpm  <- 1 / cos(s$lat * pi / 180)          # ground-m -> mercator-m inflation
  r_core  <- 30 + 5.0 * sqrt(pop)            # built-up radius (ground m)
  r_clear <- 400 + 2.5 * r_core              # cleared/field radius (ground m)
  # Numeric id for the deterministic hashes (settlement_id is character;
  # non-numeric ids fall back to a codepoint checksum).
  idn <- suppressWarnings(as.numeric(s$settlement_id))
  bad <- is.na(idn)
  if (any(bad)) idn[bad] <- vapply(s$settlement_id[bad], function(z)
    sum(utf8ToInt(z) * seq_along(utf8ToInt(z))), numeric(1))
  setts <- data.frame(id = idn, x = xy[, 1], y = xy[, 2],
                      pop = pop, cosl = cos(s$lat * pi / 180),
                      rc_m = r_core * mpm, rf_m = r_clear * mpm,
                      port = if ("is_port" %in% names(s)) !is.na(s$is_port) & s$is_port
                             else rep(FALSE, nrow(s)),
                      ang  = pi * .hash01(idn, 11L))   # strip-field orientation

  # Radial paths: 2-6 lanes out of the settlement node (the node road lines
  # already terminate at), deterministic per settlement, meandered like roads.
  geoms <- list(); gi <- 0L
  for (i in seq_len(nrow(setts))) {
    n <- max(2L, min(6L, 1L + as.integer(floor(log10(setts$pop[i] + 1)))))
    for (k in seq_len(n)) {
      a <- 2 * pi * (k - 1 + 0.8 * .hash01(setts$id[i], k)) / n
      L <- setts$rf_m[i] * (0.7 + 0.25 * .hash01(setts$id[i], k, 2L))
      gi <- gi + 1L
      geoms[[gi]] <- sf::st_linestring(rbind(
        c(setts$x[i], setts$y[i]),
        c(setts$x[i] + L * cos(a), setts$y[i] + L * sin(a))))
    }
  }
  spaths <- sf::st_sf(geometry = sf::st_sfc(geoms, crs = 3857))
  spaths <- meander_lines(spaths, amp = 24, wl = nf_wl("path.settlement"), step = 50,
                          seed = nf_seed("path.settlement"), simplify_tol = 12)
  .tilevec$setts <- setts
  .tilevec$spaths <- spaths
  .tilevec$spaths_bb <- .feature_bboxes(spaths)
  cache_save(cache, list(setts = setts, spaths = spaths,
                         spaths_bb = .tilevec$spaths_bb), st)
  .tilevec$setts_loaded <- TRUE
  .tilevec
}

#' Settlements whose cleared disc touches `ext3857`, + the path lines there.
crop_settlements <- function(ext3857) {
  v <- get_tile_settlements()
  if (is.null(v$setts)) return(list(setts = NULL, paths = NULL))
  x0 <- terra::xmin(ext3857); x1 <- terra::xmax(ext3857)
  y0 <- terra::ymin(ext3857); y1 <- terra::ymax(ext3857)
  st <- v$setts
  keep <- st$x + st$rf_m >= x0 & st$x - st$rf_m <= x1 &
          st$y + st$rf_m >= y0 & st$y - st$rf_m <= y1
  paths <- NULL
  if (!is.null(v$spaths) && !is.null(v$spaths_bb)) {
    bb <- v$spaths_bb
    idx <- which(bb$xmin <= x1 & bb$xmax >= x0 & bb$ymin <= y1 & bb$ymax >= y0)
    if (length(idx)) paths <- v$spaths[idx, ]
  }
  list(setts = if (any(keep)) st[keep, ] else NULL, paths = paths)
}

#' Per-pixel urban-core (u) and farmland (f) intensities in [0,1] as
#' SpatRasters. Radii are wobbled by two world-anchored noise bands so the
#' clearing edge is organic, identical in every tile that sees it. Water-clipped.
settlement_fields <- function(template, setts, water_mask = NULL) {
  if (is.null(setts) || nrow(setts) == 0) return(NULL)
  xy <- crds(template, na.rm = FALSE)
  mx <- xy[, 1]; my <- xy[, 2]; N <- length(mx)
  ss <- function(x) { t <- pmin(pmax(x, 0), 1); t * t * (3 - 2 * t) }
  wob1 <- fbm_world(mx, my, octaves = nf_octaves("settle.wobble1"), base_wavelength_m = nf_wl("settle.wobble1"), seed = nf_seed("settle.wobble1"))
  wob2 <- fbm_world(mx, my, octaves = nf_octaves("settle.wobble2"), base_wavelength_m = nf_wl("settle.wobble2"), seed = nf_seed("settle.wobble2"))
  u <- f <- numeric(N); ang <- numeric(N)
  for (i in seq_len(nrow(setts))) {
    d  <- sqrt((mx - setts$x[i])^2 + (my - setts$y[i])^2)
    we <- 1 + 0.20 * wob1 + 0.06 * wob2      # low fine-wobble: no sawtooth edge
    rc <- setts$rc_m[i] * we; rf <- setts$rf_m[i] * we
    u  <- pmax(u, ss((rc - d) / (0.45 * setts$rc_m[i])))
    fi <- ss((rf - d) / (0.35 * setts$rf_m[i]))
    upd <- fi > f                                   # dominant settlement's angle
    ang[upd] <- setts$ang[i]
    f  <- pmax(f, fi)
  }
  if (!is.null(water_mask)) {
    wv <- terra::values(water_mask)[, 1]; iw <- !is.na(wv) & wv > 0
    u[iw] <- 0; f[iw] <- 0
  }
  list(u = setValues(rast(template), u), f = setValues(rast(template), f),
       ang = setValues(rast(template), ang))
}

#' Tint the composite inside the settlement footprint: a Worley-cell patchwork
#' of field colours over the farmland belt, and packed-earth ground in the
#' urban core. Shaded by terrain luminance so hillshade reads through.
apply_settlement_ground <- function(comp, sfld) {
  if (is.null(sfld)) return(comp)
  uv <- terra::values(sfld$u)[, 1]; fv <- terra::values(sfld$f)[, 1]
  uv[is.na(uv)] <- 0; fv[is.na(fv)] <- 0
  if (!any(uv > 0.01 | fv > 0.01)) return(comp)
  xy <- crds(comp, na.rm = FALSE)
  # Strip fields: rotate into the dominant settlement's field axis and stretch
  # 3.5:1, so the Worley patchwork reads as elongated medieval strips running
  # back from the lanes rather than isotropic blobs. World-anchored: angle is a
  # pure hash of the settlement id, so the pattern matches across tiles.
  av <- if (!is.null(sfld$ang)) terra::values(sfld$ang)[, 1] else rep(0, length(uv))
  av[is.na(av)] <- 0
  ca <- cos(av); sa_ <- sin(av)
  ux <- ( xy[, 1] * ca + xy[, 2] * sa_) / 2.8
  vy <- (-xy[, 1] * sa_ + xy[, 2] * ca)
  cid <- ambient::gen_worley(ux, vy, frequency = 1 / nf_wl("settle.fieldcells"),
                             seed = nf_seed("settle.fieldcells"), value = "cell")
  h1 <- .hash01(cid, 1L); h2 <- .hash01(cid, 2L)
  pal_i   <- 1L + (floor(h1 * 4) %% 4L)
  a_field <- 0.30 * fv * (0.62 + 0.38 * h2) * (1 - uv)   # fields fade under core
  a_urb   <- 0.55 * uv
  cm  <- terra::values(comp)
  lum <- pmin(pmax((0.3 * cm[, 1] + 0.59 * cm[, 2] + 0.11 * cm[, 3]) / 160, 0.5), 1.15)
  base <- 1 - a_field - a_urb
  for (b in 1:3) {
    cm[, b] <- pmin(pmax(cm[, b] * base +
                           FIELD_PALETTE[pal_i, b] * lum * a_field +
                           URBAN_COL[b] * lum * a_urb, 0), 255)
  }
  setValues(comp, cm)
}

#' Draw the settlement lanes (thin, uncased, land-clipped, under the roads).
draw_paths <- function(comp, paths, water_mask = NULL, zoom = 12) {
  if (zoom < SETTLE_PATH_MIN_Z || is.null(paths) || nrow(paths) == 0) return(comp)
  xres <- res(comp)[1]
  msk <- .line_mask(paths, comp, PATH_PX * xres / 2)
  if (is.null(msk)) return(comp)
  if (!is.null(water_mask)) msk <- msk & !water_mask
  .paint(comp, msk, PATH_COL)
}

#' Bridges (z13+): where a land road crosses a river channel, repaint the road
#' pixels within the channel's reach as a stone deck with dark casing — the
#' crossing reads as built infrastructure instead of a road drawn over water.
#' Uses the shared per-tier distance rasters; no intersection geometry needed.
draw_bridges <- function(comp, roads, tier_d, zoom) {
  if (zoom < 13L || is.null(roads) || nrow(roads) == 0 || is.null(tier_d)) return(comp)
  xres <- res(comp)[1]; zf <- 1 + 0.18 * max(0, zoom - 12)
  isf <- if ("is_ferry" %in% names(roads)) !is.na(roads$is_ferry) & roads$is_ferry else rep(FALSE, nrow(roads))
  land <- roads[!isf, ]
  if (nrow(land) == 0) return(comp)
  # Bridge is WIDER than the road (case x1.5) so the span reads as a structure.
  caseR <- .line_mask(land, comp, ROAD_CASE_PX * zf * xres / 2 * 1.5)
  fillR <- .line_mask(land, comp, ROAD_FILL_PX * zf * xres / 2 * 1.6)
  if (is.null(caseR) || is.null(fillR)) return(comp)
  cv <- terra::values(caseR)[, 1]; fv <- terra::values(fillR)[, 1]
  near <- rep(FALSE, length(cv))
  for (t in names(tier_d)) {
    dv <- terra::values(tier_d[[t]])[, 1]
    hw <- RIVER_TIER[[t]]$width_px * xres / 2 * 1.7 + 1.5 * xres   # channel + verge
    near <- near | (!is.na(dv) & dv <= hw)
  }
  bc <- cv & near & !(fv & near); bf <- fv & near
  if (!any(bc | bf)) return(comp)
  BR_CASE <- c(46, 38, 30); BR_DECK <- c(168, 152, 124)
  cm <- terra::values(comp)
  for (b in 1:3) { cm[bc, b] <- BR_CASE[b]; cm[bf, b] <- BR_DECK[b] }
  setValues(comp, cm)
}

#' Pilgrim paths (z13+): tier-4+ sacred sites within `max_m` of the road/lane
#' network get a thin meandered path from the shrine to the nearest point on
#' the network. World-fixed geometry in, deterministic meander out — seamless.
sacred_paths <- function(sac, roads, paths, max_m = 2000) {
  if (is.null(sac) || nrow(sac) == 0) return(NULL)
  sac <- sac[sac$tier >= 4, , drop = FALSE]
  if (nrow(sac) == 0) return(NULL)
  gg <- list()
  if (!is.null(roads) && nrow(roads)) {
    isf <- if ("is_ferry" %in% names(roads)) !is.na(roads$is_ferry) & roads$is_ferry else rep(FALSE, nrow(roads))
    if (any(!isf)) gg$r <- sf::st_geometry(roads[!isf, ])
  }
  if (!is.null(paths) && nrow(paths)) gg$p <- sf::st_geometry(paths)
  if (!length(gg)) return(NULL)
  net <- sf::st_union(do.call(c, unname(gg)))
  out <- list()
  for (i in seq_len(nrow(sac))) {
    pt <- sf::st_sfc(sf::st_point(c(sac$x[i], sac$y[i])), crs = 3857)
    np <- try(sf::st_nearest_points(pt, net), silent = TRUE)
    if (inherits(np, "try-error") || !length(np)) next
    L <- as.numeric(sf::st_length(np))
    if (is.finite(L) && L > 30 && L <= max_m) out[[length(out) + 1]] <- np[[1]]
  }
  if (!length(out)) return(NULL)
  sp <- sf::st_sf(geometry = sf::st_sfc(out, crs = 3857))
  meander_lines(sp, amp = 14, wl = nf_wl("path.pilgrim"), step = 50,
                seed = nf_seed("path.pilgrim"), simplify_tol = 8)
}

# Walls around large settlements.
WALL_POP_MIN <- 8000
WALL_COL     <- c(60, 54, 46)
TOWER_COL    <- c(44, 40, 34)

#' City walls (z13+): settlements above WALL_POP_MIN get a wall ring around
#' the urban core. The radius is wobbled by noise sampled on a FIXED circle
#' around the settlement centre (a pure function of bearing), so every tile
#' draws the identical ring. Gates: the wall breaks where roads/lanes cross.
draw_walls <- function(comp, setts, zoom, roads = NULL, paths = NULL, water_mask = NULL) {
  if (zoom < SETTLE_BLDG_MIN_Z || is.null(setts) || nrow(setts) == 0) return(comp)
  ws <- setts[setts$pop >= WALL_POP_MIN, , drop = FALSE]
  if (nrow(ws) == 0) return(comp)
  xy <- crds(comp, na.rm = FALSE); mx <- xy[, 1]; my <- xy[, 2]
  wallm <- rep(FALSE, length(mx))
  hw <- max(6, 0.95 * res(comp)[1])                   # ~2 px: a wall, not a hairline
  twr <- NULL
  for (i in seq_len(nrow(ws))) {
    R <- 0.8 * ws$rc_m[i]
    d <- sqrt((mx - ws$x[i])^2 + (my - ws$y[i])^2)
    sel <- d > 0.55 * R & d < 1.55 * R
    if (!any(sel)) next
    th  <- atan2(my[sel] - ws$y[i], mx[sel] - ws$x[i])
    wob <- fbm_world(ws$x[i] + 900 * cos(th), ws$y[i] + 900 * sin(th),
                     octaves = nf_octaves("settle.wallring"), base_wavelength_m = nf_wl("settle.wallring"), seed = nf_seed("settle.wallring"))
    wallm[sel] <- wallm[sel] | abs(d[sel] - R * (1 + 0.13 * wob)) < hw
    # 12 towers on the same wobbled ring (same noise, sampled at tower bearings)
    tth <- 2 * pi * (0:11) / 12 + 0.5 * .hash01(ws$id[i], 31L)
    tw  <- fbm_world(ws$x[i] + 900 * cos(tth), ws$y[i] + 900 * sin(tth),
                     octaves = nf_octaves("settle.wallring"), base_wavelength_m = nf_wl("settle.wallring"), seed = nf_seed("settle.wallring"))
    tR  <- R * (1 + 0.13 * tw)
    twr <- rbind(twr, cbind(ws$x[i] + tR * cos(tth), ws$y[i] + tR * sin(tth)))
  }
  if (!any(wallm)) return(comp)
  gate <- rep(FALSE, length(mx))
  gg <- list()
  if (!is.null(roads) && nrow(roads)) {
    isf <- if ("is_ferry" %in% names(roads)) !is.na(roads$is_ferry) & roads$is_ferry else rep(FALSE, nrow(roads))
    if (any(!isf)) gg$r <- sf::st_geometry(roads[!isf, ])
  }
  if (!is.null(paths) && nrow(paths)) gg$p <- sf::st_geometry(paths)
  if (length(gg)) {
    gl <- sf::st_sf(geometry = do.call(c, unname(gg)))
    gm <- .line_mask(gl, comp, 16)
    if (!is.null(gm)) gate <- terra::values(gm)[, 1] > 0
  }
  wet <- if (is.null(water_mask)) rep(FALSE, length(mx)) else {
    wv <- terra::values(water_mask)[, 1]; !is.na(wv) & wv > 0
  }
  keep <- wallm & !gate & !wet
  if (!any(keep)) return(comp)
  cm <- terra::values(comp)
  for (b in 1:3) cm[keep, b] <- WALL_COL[b]
  if (!is.null(twr)) {                                # towers: 2x2 darker knots
    tc <- terra::cellFromXY(comp, twr)
    tc <- tc[!is.na(tc)]
    tc <- tc[keep[tc]]                                # wall survived here: not a gate, not water
    if (length(tc)) {
      nc <- ncol(comp)
      tc <- unique(c(tc, tc + 1L, tc + nc, tc + nc + 1L))
      tc <- tc[tc >= 1 & tc <= ncell(comp)]
      tc <- tc[!wet[tc]]                              # 2x2 expansion must not overhang water
      for (b in 1:3) cm[tc, b] <- TOWER_COL[b]
    }
  }
  setValues(comp, cm)
}

#' Stamp buildings. Three placement modes give settlements real structure:
#'   1) ROADSIDE: candidate sites every ~30 m along the road/lane lines,
#'      offset to either verge, kept with probability decaying from the core
#'      -- villages read as linear ribbons along their roads.
#'   2) LATTICE CORE: the old hash-thinned world lattice fills the urban core
#'      between the ribbons; for PORT settlements the density is boosted in
#'      the near-shore waterness band, crowding buildings onto the waterfront.
#'   3) FARMSTEADS: sparse scatter across the field belt.
#' Ports also get 1-2 deterministic PIERS: the shore is found by marching the
#' waterness field toward water from the settlement centre, then a short
#' timber line is painted out past the 0.5 contour.
#' Painted into pixel cells (1px at z13, 2x2 at z14).
draw_buildings <- function(comp, setts, zoom, water_mask = NULL,
                           roads = NULL, paths = NULL, waterness = NULL) {
  if (zoom < SETTLE_BLDG_MIN_Z || is.null(setts) || nrow(setts) == 0) return(comp)
  g <- BLDG_LATTICE_M
  e <- ext(comp)
  wnv <- if (!is.null(waterness)) terra::values(waterness)[, 1] else NULL
  ssv <- function(x, e0, e1) { t <- pmin(pmax((x - e0) / (e1 - e0), 0), 1); t * t * (3 - 2 * t) }
  bx <- by <- bh <- numeric(0)

  # --- 1) roadside ribbons -------------------------------------------------
  geoms <- list()
  if (!is.null(roads) && nrow(roads)) {
    isf <- if ("is_ferry" %in% names(roads)) !is.na(roads$is_ferry) & roads$is_ferry else rep(FALSE, nrow(roads))
    if (any(!isf)) geoms$rd <- sf::st_geometry(roads[!isf, ])
  }
  if (!is.null(paths) && nrow(paths)) geoms$pt <- sf::st_geometry(paths)
  if (length(geoms)) {
    gl <- sf::st_segmentize(do.call(c, unname(geoms)), dfMaxLength = 30)
    co <- sf::st_coordinates(gl)[, 1:2, drop = FALSE]
    n  <- nrow(co)
    if (n >= 2) {
      dx <- c(diff(co[, 1]), 0); dy <- c(diff(co[, 2]), 0)
      Ln <- sqrt(dx * dx + dy * dy); bad <- Ln < 1e-9 | Ln > 120
      dx[bad] <- 1; dy[bad] <- 0; Ln[bad] <- 1        # seam vertices: direction moot
      nxv <- -dy / Ln; nyv <- dx / Ln
      dmin <- rep(Inf, n); rfv <- rep(1, n); clv <- rep(1, n)
      for (i in seq_len(nrow(setts))) {
        d <- sqrt((co[, 1] - setts$x[i])^2 + (co[, 2] - setts$y[i])^2)
        upd <- d < dmin
        dmin[upd] <- d[upd]; rfv[upd] <- setts$rf_m[i]; clv[upd] <- setts$cosl[i]
      }
      prob <- 0.8 * pmin(pmax(1 - dmin / (0.85 * rfv), 0), 1)^2 * clv
      rc1 <- round(co[, 1]); rc2 <- round(co[, 2])
      offm <- 13 + 14 * .hash01(rc1, rc2, 14L)
      for (side in c(-1, 1)) {
        hh <- .hash01(rc1, rc2, if (side < 0) 12L else 13L)
        keep <- hh < prob
        if (any(keep)) {
          bx <- c(bx, co[keep, 1] + side * nxv[keep] * offm[keep])
          by <- c(by, co[keep, 2] + side * nyv[keep] * offm[keep])
          bh <- c(bh, .hash01(rc1[keep], rc2[keep], if (side < 0) 15L else 16L))
        }
      }
    }
  }

  # --- 2) lattice core (+ port waterfront) & 3) farmsteads ------------------
  for (i in seq_len(nrow(setts))) {
    r  <- setts$rf_m[i]
    ix <- seq(floor(max(setts$x[i] - r, e$xmin) / g), ceiling(min(setts$x[i] + r, e$xmax) / g))
    iy <- seq(floor(max(setts$y[i] - r, e$ymin) / g), ceiling(min(setts$y[i] + r, e$ymax) / g))
    if (!length(ix) || !length(iy)) next
    gg <- expand.grid(ix = ix, iy = iy)
    h1 <- .hash01(gg$ix, gg$iy, 3L); h2 <- .hash01(gg$ix, gg$iy, 4L)
    h3 <- .hash01(gg$ix, gg$iy, 5L); h4 <- .hash01(gg$ix, gg$iy, 6L)
    px <- (gg$ix + 0.5 + 0.7 * (h2 - 0.5)) * g
    py <- (gg$iy + 0.5 + 0.7 * (h3 - 0.5)) * g
    d  <- sqrt((px - setts$x[i])^2 + (py - setts$y[i])^2)
    ucore <- pmin(pmax((setts$rc_m[i] - d) / (0.5 * setts$rc_m[i]) + 1, 0), 1)
    fbelt <- d < setts$rf_m[i] * 0.92
    # modest lattice fill: the roadside ribbons carry the visual structure
    prob <- 0.35 * ucore^2.2 * setts$cosl[i] + ifelse(fbelt, 0.006, 0)
    if (setts$port[i] && !is.null(wnv)) {             # waterfront crowding
      cl <- terra::cellFromXY(comp, cbind(px, py))
      wn <- ifelse(is.na(cl), 0, wnv[cl]); wn[is.na(wn)] <- 0
      prob <- prob * (1 + 2.5 * ssv(wn, 0.22, 0.42) * (d < r))
    }
    keep <- h1 < prob
    if (any(keep)) {
      bx <- c(bx, px[keep]); by <- c(by, py[keep]); bh <- c(bh, h4[keep])
    }
  }

  cm <- terra::values(comp)
  if (length(bx)) {
    cells <- terra::cellFromXY(comp, cbind(bx, by))
    ok <- !is.na(cells); cells <- cells[ok]; bh <- bh[ok]
    if (!is.null(water_mask)) {
      wv <- terra::values(water_mask)[, 1]
      dry <- !(!is.na(wv[cells]) & wv[cells] > 0)
      cells <- cells[dry]; bh <- bh[dry]
    }
    if (length(cells)) {
      if (zoom >= 14L) {                              # 2x2 px footprint
        nc <- ncol(comp)
        cells <- c(cells, cells + 1L, cells + nc, cells + nc + 1L)
        bh <- rep(bh, 4)
        inb <- cells >= 1 & cells <= ncell(comp)
        cells <- cells[inb]; bh <- bh[inb]
      }
      shade <- 0.8 + 0.5 * bh                         # per-building tint variety
      for (b in 1:3) cm[cells, b] <- pmin(pmax(BLDG_COL[b] * shade, 0), 255)
    }
  }

  # --- 4) piers for ports ----------------------------------------------------
  if (!is.null(wnv)) {
    PIER_COL <- c(92, 70, 46)
    for (i in which(setts$port)) {
      # bearing of steepest waterness rise = straight toward open water
      th <- (0:7) * pi / 4
      sc <- terra::cellFromXY(comp, cbind(setts$x[i] + 220 * cos(th),
                                          setts$y[i] + 220 * sin(th)))
      wv8 <- ifelse(is.na(sc), -1, wnv[sc]); wv8[is.na(wv8)] <- -1
      if (max(wv8) < 0.3) next
      bt <- th[which.max(wv8)]
      tt <- seq(0, 1200, by = 12)
      sx <- setts$x[i] + cos(bt) * tt; sy <- setts$y[i] + sin(bt) * tt
      cl <- terra::cellFromXY(comp, cbind(sx, sy))
      wl <- ifelse(is.na(cl), NA, wnv[cl])
      k  <- which(!is.na(wl) & wl >= 0.5)[1]
      if (is.na(k)) next
      npier <- 1L + (( .hash01(setts$id[i], 17L)) > 0.55)
      for (j in seq_len(npier)) {
        hj <- .hash01(setts$id[i], 17L + j)
        # shift the base along the shore, run the pier out past the contour
        px0 <- sx[k] - sin(bt) * (hj - 0.5) * 130
        py0 <- sy[k] + cos(bt) * (hj - 0.5) * 130
        plen <- 45 + 55 * .hash01(setts$id[i], 20L + j)
        pt <- seq(-18, plen, by = res(comp)[1] / 2)
        pc <- terra::cellFromXY(comp, cbind(px0 + cos(bt) * pt, py0 + sin(bt) * pt))
        pc <- unique(pc[!is.na(pc)])
        if (zoom >= 14L && length(pc)) pc <- unique(c(pc, pc + ncol(comp)))
        pc <- pc[pc >= 1 & pc <= ncell(comp)]
        for (b in 1:3) cm[pc, b] <- PIER_COL[b]
      }
    }
  }
  setValues(comp, cm)
}
