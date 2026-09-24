# =============================================================================
# sacred.R - Sacred sites: groves, clearings, standing stones
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# The Divinity layer. Tier 4+ sites clear their surroundings; tiers 1-3 do the
# opposite and grow a grove that thickens and darkens the canopy. A destroyed site
# keeps its clearing but loses its grove -- the scar remains -- which is a piece of
# world logic expressed entirely in two rasters.
#
# sacred_fields() is also the reference implementation for a noise-wobbled
# clearing edge. Copy it rather than stamping a fixed-radius disc.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Sacred sites: groves, clearings, standing stones
# -----------------------------------------------------------------------------
# The Divinity layer (19,783 sites, tier 1-9 where 9 is grandest) exists only
# as Leaflet markers; here the sites are rendered INTO the terrain so zooming
# in feels like discovering them. Tier 1-3: a darker, denser sacred GROVE.
# Tier 4-6: a small clearing with a shrine stone. Tier 7-9: a broad clearing
# with a standing-stone ring. Deterministic from site id.

SACRED_MIN_Z  <- 13L
STONE_COL     <- c(99, 96, 90)

.tile_sacred_cache <- function() map_path("data", "tile_sacred_3857.rds")

# Sacred sites had NO version guard at all: any cache file was accepted, so a
# change to the tier radii or the grove/clearing split was invisible until
# someone deleted the file by hand.
.prov_sacred_stamp <- function() prov_stamp(list(
  source = prov_file("Input Data/Divinity/sacred_sites_attributed.RData"),
  code   = prov_rfile("Functions/tiles/sacred.R", "Functions/tiles/core.R"),
  fields = prov_field("sacred.wobble")))

#' Load (and cache) sacred sites in EPSG:3857 with derived radii.
get_tile_sacred <- function(force = FALSE) {
  if (isTRUE(.tilevec$sacred_loaded) && !force) return(.tilevec)
  cache <- .tile_sacred_cache()
  st <- .prov_sacred_stamp()
  hit <- cache_load(cache, st, force = force)
  if (hit$hit) {
    .tilevec$sacred <- hit$obj; .tilevec$sacred_loaded <- TRUE
    return(.tilevec)
  }
  f <- file.path(.tile_root(), "Input Data", "Divinity", "sacred_sites_attributed.RData")
  en <- new.env()
  ok <- try(load(f, envir = en), silent = TRUE)
  if (inherits(ok, "try-error")) {
    .tilevec$sacred <- NULL; .tilevec$sacred_loaded <- TRUE
    return(.tilevec)
  }
  sc <- en$sacred_sites_sf
  sc <- sc[!is.na(sc$latitude) & abs(sc$latitude) < 84.5, ]
  pts <- sf::st_transform(sf::st_as_sf(as.data.frame(sc)[, c("site_id", "tier", "status",
                                                             "longitude", "latitude")],
                                       coords = c("longitude", "latitude"), crs = 4326), 3857)
  xy  <- sf::st_coordinates(pts)
  idn <- vapply(as.character(sc$site_id), function(z)
    sum(utf8ToInt(z) * seq_along(utf8ToInt(z))), numeric(1))
  sac <- data.frame(id = idn, x = xy[, 1], y = xy[, 2], tier = sc$tier,
                    destroyed = !is.na(sc$status) & sc$status == "destroyed",
                    r = (18 + 9 * sc$tier) / cos(sc$latitude * pi / 180))
  .tilevec$sacred <- sac
  cache_save(cache, sac, st)
  .tilevec$sacred_loaded <- TRUE
  .tilevec
}

#' Sacred sites whose footprint (grove reach ~2r) touches `ext3857`.
crop_sacred <- function(ext3857) {
  v <- get_tile_sacred()
  if (is.null(v$sacred)) return(NULL)
  s <- v$sacred; rr <- 2.2 * s$r
  keep <- s$x + rr >= terra::xmin(ext3857) & s$x - rr <= terra::xmax(ext3857) &
          s$y + rr >= terra::ymin(ext3857) & s$y - rr <= terra::ymax(ext3857)
  if (any(keep)) s[keep, ] else NULL
}

#' Per-pixel clearing (veg suppression, tier 4+) and grove (canopy boost,
#' tier 1-3) weights. Edges wobbled by the shared world noise so they read as
#' organic openings, not stamped discs. Destroyed sites keep their clearing
#' (the scar remains) but lose the grove.
sacred_fields <- function(template, sac) {
  if (is.null(sac) || nrow(sac) == 0) return(NULL)
  xy <- crds(template, na.rm = FALSE)
  mx <- xy[, 1]; my <- xy[, 2]; N <- length(mx)
  ssf <- function(x) { t <- pmin(pmax(x, 0), 1); t * t * (3 - 2 * t) }
  wob <- fbm_world(mx, my, octaves = nf_octaves("sacred.wobble"), base_wavelength_m = nf_wl("sacred.wobble"), seed = nf_seed("sacred.wobble"))
  clear <- grove <- numeric(N)
  for (i in seq_len(nrow(sac))) {
    d <- sqrt((mx - sac$x[i])^2 + (my - sac$y[i])^2)
    re <- sac$r[i] * (1 + 0.25 * wob)
    if (sac$tier[i] >= 4) {
      clear <- pmax(clear, ssf((re - d) / (0.45 * sac$r[i])))
    } else if (!sac$destroyed[i]) {
      grove <- pmax(grove, ssf((1.9 * re - d) / (0.9 * sac$r[i])))
    }
  }
  if (!any(clear > 0.01) && !any(grove > 0.01)) return(NULL)
  list(clear = setValues(rast(template), clear),
       grove = setValues(rast(template), grove))
}

#' Stamp standing stones (z13+): tier 7-9 a ring + centre stone, tier 4-6 a
#' single shrine stone. Destroyed sites show a toppled partial ring.
draw_sacred <- function(comp, sac, zoom, water_mask = NULL) {
  if (zoom < SACRED_MIN_Z || is.null(sac) || nrow(sac) == 0) return(comp)
  sac <- sac[sac$tier >= 4, ]
  if (nrow(sac) == 0) return(comp)
  cm <- terra::values(comp)
  wv <- if (!is.null(water_mask)) terra::values(water_mask)[, 1] else NULL
  px <- py <- ph <- numeric(0)
  for (i in seq_len(nrow(sac))) {
    if (sac$tier[i] >= 7) {
      nst <- 6L + 2L * (sac$tier[i] - 7L)
      rs  <- 0.6 * sac$r[i]
      a0  <- 2 * pi * .hash01(sac$id[i], 21L)
      th  <- a0 + 2 * pi * (seq_len(nst) - 1) / nst
      keep <- if (sac$destroyed[i]) .hash01(sac$id[i], 22L + seq_len(nst)) < 0.45 else rep(TRUE, nst)
      px <- c(px, sac$x[i] + rs * cos(th[keep]), sac$x[i])
      py <- c(py, sac$y[i] + rs * sin(th[keep]), sac$y[i])
      ph <- c(ph, .hash01(sac$id[i], 30L + seq_len(sum(keep) + 1L)))
    } else {
      px <- c(px, sac$x[i]); py <- c(py, sac$y[i])
      ph <- c(ph, .hash01(sac$id[i], 30L))
    }
  }
  cells <- terra::cellFromXY(comp, cbind(px, py))
  ok <- !is.na(cells); cells <- cells[ok]; ph <- ph[ok]
  if (!is.null(wv)) { dry <- !(!is.na(wv[cells]) & wv[cells] > 0); cells <- cells[dry]; ph <- ph[dry] }
  if (!length(cells)) return(comp)
  if (zoom >= 14L) {
    nc <- ncol(comp)
    cells <- c(cells, cells + 1L, cells + nc, cells + nc + 1L); ph <- rep(ph, 4)
    inb <- cells >= 1 & cells <= ncell(comp); cells <- cells[inb]; ph <- ph[inb]
  }
  shade <- 0.85 + 0.4 * ph
  for (b in 1:3) cm[cells, b] <- pmin(STONE_COL[b] * shade, 255)
  setValues(comp, cm)
}
