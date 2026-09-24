# =============================================================================
# reference-tiles.R — the fixed set of tiles a change is judged on
#
# Sourced by render-reference.R. Defines reference_tiles(), which returns one
# row per tile: a name, the tags saying what is in frame, and z/x/y.
#
# The places are DERIVED from canon by rule (the biggest walled city, a port, a
# sacred grove, one clean patch of each biome...) rather than hardcoded, for the
# same reason test-render.R finds its tiles: a hardcoded list only works on the
# full world, and a partial export is exactly what someone debugging has. The
# rules are deterministic, so two checkouts of the same data get the same set,
# which is all a before/after comparison needs.
#
# Tags drive the report's "collateral change" check. A PR declares what it means
# to change (--expect roads,settlements); a changed tile carrying none of those
# tags is flagged. `terrain` and `shading` are on every tile, because every tile
# has terrain -- declaring either one means "expect change everywhere".
# =============================================================================

REF_ZOOMS <- c(10L, 12L, 14L)

#' lon/lat -> XYZ tile (render_elevation_tile() takes XYZ y, not TMS).
ref_lonlat_tile <- function(lon, lat, z) {
  n <- 2^z
  lat <- max(min(lat, 85.05), -85.05)
  r <- lat * pi / 180
  c(x = floor((lon + 180) / 360 * n),
    y = floor((1 - log(tan(r) + 1 / cos(r)) / pi) / 2 * n))
}

#' TRUE where the 0.01-degree canon actually has data at lon/lat. A partial
#' export ships every outline but rasters for only some continents.
.ref_has_data <- function(lon, lat) {
  v <- try(terra::extract(terra::rast(here::here("Input Data/HighResolution/elevation.vrt")),
                          cbind(lon, lat))[, 1], silent = TRUE)
  !inherits(v, "try-error") && length(v) && !is.na(v[1])
}

#' Deterministic pick: the candidate at the median position of a sorted order,
#' skipping any without high-resolution data. Median rather than first, so the
#' pick is not always an edge case of whatever the sort key is.
.ref_pick <- function(lon, lat, order_by = seq_along(lon)) {
  ok <- is.finite(lon) & is.finite(lat) & abs(lat) < 60
  idx <- which(ok)[order(order_by[ok])]
  if (!length(idx)) return(NULL)
  mid <- ceiling(length(idx) / 2)
  for (i in idx[unique(c(mid:length(idx), rev(seq_len(mid))))])
    if (.ref_has_data(lon[i], lat[i])) return(c(lon[i], lat[i]))
  NULL
}

#' The reference places, one row each: name, tags, lon, lat.
reference_places <- function() {
  P <- list()
  add <- function(name, tags, pt)
    if (!is.null(pt)) P[[length(P) + 1L]] <<- data.frame(
      name = name, tags = paste(c(tags, "terrain", "shading"), collapse = ","),
      lon = pt[1], lat = pt[2], stringsAsFactors = FALSE)

  sf_ <- here::here("Input Data/Combined/settlements_final.rds")
  if (file.exists(sf_)) {
    s <- readRDS(sf_)
    big <- function(rows) if (length(rows)) {
      rows <- rows[order(-s$population[rows])]
      for (i in rows) if (abs(s$lat[i]) < 60 && .ref_has_data(s$lon[i], s$lat[i]))
        return(c(s$lon[i], s$lat[i]))
    }
    pop <- s$population
    add("city",    c("settlements", "roads", "walls"), big(which(pop >= 8000)))
    add("port",    c("settlements", "roads", "coast", "piers"),
        big(which(s$is_port %in% TRUE)))
    add("village", c("settlements", "roads"),
        .ref_pick(s$lon, s$lat, ifelse(pop >= 200 & pop < 2000, pop, NA)))
    add("river town", c("settlements", "rivers", "roads"),
        big(which(s$on_river %in% TRUE & !(s$coastal %in% TRUE))))
    # A modest coastal place rather than the port: this tile is about the
    # coastline itself, not the waterfront buildings.
    add("coast", c("coast", "settlements"),
        .ref_pick(s$lon, s$lat, ifelse(s$coastal %in% TRUE & !(s$is_port %in% TRUE) &
                                         pop < 2000, s$settlement_id, NA)))
  }

  ss_ <- here::here("Input Data/Divinity/sacred_sites_attributed.RData")
  if (file.exists(ss_)) {
    e <- new.env(); nm <- load(ss_, envir = e); d <- e[[nm[1]]]
    lon <- d$longitude; lat <- d$latitude
    add("sacred grove",    "sacred", .ref_pick(lon, lat, ifelse(d$tier <= 3, d$site_id, NA)))
    add("sacred clearing", "sacred", .ref_pick(lon, lat, ifelse(d$tier >= 4, d$site_id, NA)))
  }

  rd_ <- here::here("Input Data/Roads/road_routes.rds")
  wc_ <- here::here("Input Data/HighResolution/water_class.vrt")
  if (file.exists(rd_) && file.exists(wc_)) {
    rd <- readRDS(rd_)
    if ("is_ferry" %in% names(rd) && any(rd$is_ferry %in% TRUE)) {
      wc <- terra::rast(wc_)
      co <- sf::st_coordinates(sf::st_geometry(rd[rd$is_ferry %in% TRUE, ]))
      co <- co[seq(1, nrow(co), by = max(1L, nrow(co) %/% 400L)), , drop = FALSE]
      w  <- terra::extract(wc, co[, 1:2])[, 1]
      hit <- which(w %in% c(WATER_CLASS$OCEAN, WATER_CLASS$LAKE) & abs(co[, 2]) < 60)
      if (length(hit)) add("ferry crossing", c("roads", "ferries", "coast"),
                           co[hit[ceiling(length(hit) / 2)], 1:2])
    }
  }

  # One clean patch of each land biome, from the 0.05-degree climate raster: a
  # cell whose 3x3 neighbourhood is all that biome, so the tile is about it.
  bi_ <- here::here("Input Data/Climate/biome.tif")
  if (file.exists(bi_)) {
    b <- terra::rast(bi_)
    same <- terra::focal(b, w = 3, fun = "min") == terra::focal(b, w = 3, fun = "max")
    bv <- terra::values(b)[, 1]; sv <- terra::values(same)[, 1]
    for (code in 1:8) {
      nm <- BIOME_INFO[[as.character(code)]]$name
      cells <- which(bv == code & sv %in% TRUE)
      # Some biomes (desert, here) never fill a clean 3x3 inside +-60 degrees
      # with data under it; any cell of the biome beats having no tile of it.
      xy <- if (length(cells)) terra::xyFromCell(b, cells)
      if (!length(cells) || is.null(.ref_pick(xy[, 1], xy[, 2], cells)))
        cells <- which(bv == code)
      if (!length(cells)) next
      xy <- terra::xyFromCell(b, cells)
      add(tolower(nm), c("vegetation", paste0("biome:", tolower(nm))),
          .ref_pick(xy[, 1], xy[, 2], cells))
    }
  }
  do.call(rbind, P)
}

#' What is actually IN a tile, from canon: the tags a place was chosen for say
#' nothing about the road that happens to cross the taiga tile, and the
#' collateral check has to know about that road or it flags every road change.
.ref_content_tags <- function(z, x, y, ctx) {
  n <- 2^z
  lon <- c(x, x + 1) / n * 360 - 180
  lat <- atan(sinh(pi * (1 - 2 * c(y + 1, y) / n))) * 180 / pi
  pad <- 0.02                                   # a little over the render margin
  bb  <- c(lon[1] - pad, lon[2] + pad, lat[1] - pad, lat[2] + pad)
  inb <- function(lo, la) any(lo >= bb[1] & lo <= bb[2] & la >= bb[3] & la <= bb[4], na.rm = TRUE)
  tags <- character(0)
  if (!is.null(ctx$s)) {
    if (inb(ctx$s$lon, ctx$s$lat)) tags <- c(tags, "settlements")
    big <- ctx$s$population >= 8000
    if (inb(ctx$s$lon[big], ctx$s$lat[big])) tags <- c(tags, "walls")
    port <- ctx$s$is_port %in% TRUE
    if (inb(ctx$s$lon[port], ctx$s$lat[port])) tags <- c(tags, "piers")
  }
  if (!is.null(ctx$ss) && inb(ctx$ss[, 1], ctx$ss[, 2])) tags <- c(tags, "sacred")
  box <- sf::st_as_sfc(sf::st_bbox(c(xmin = bb[1], xmax = bb[2], ymin = bb[3], ymax = bb[4]),
                                   crs = 4326))
  if (!is.null(ctx$rd)) {
    hit <- lengths(sf::st_intersects(ctx$rd, box)) > 0
    if (any(hit)) tags <- c(tags, "roads")
    if (any(hit & ctx$rd_ferry)) tags <- c(tags, "ferries")
  }
  if (!is.null(ctx$wc)) {
    w <- try(terra::values(terra::crop(ctx$wc, terra::ext(bb)))[, 1], silent = TRUE)
    if (!inherits(w, "try-error")) {
      w <- w[!is.na(w)]
      wet <- w %in% c(WATER_CLASS$OCEAN, WATER_CLASS$LAKE)
      if (any(wet) && any(w == WATER_CLASS$LAND)) tags <- c(tags, "coast")
      if (any(w %in% c(WATER_CLASS$RIVER_NAV, WATER_CLASS$STREAM_MINOR))) tags <- c(tags, "rivers")
      if (any(w == WATER_CLASS$LAND)) tags <- c(tags, "vegetation")
    }
  }
  tags
}

#' The reference tiles: every place at every REF_ZOOMS level, tagged with the
#' place's own tags plus everything actually in frame.
reference_tiles <- function() {
  p <- reference_places()
  if (is.null(p) || !nrow(p)) stop("no reference places could be derived from this checkout")
  old <- suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old)))
  rd_ <- here::here("Input Data/Roads/road_routes.rds")
  ss_ <- here::here("Input Data/Divinity/sacred_sites_attributed.RData")
  ctx <- list(
    s  = tryCatch(readRDS(here::here("Input Data/Combined/settlements_final.rds")), error = function(e) NULL),
    ss = tryCatch({ e <- new.env(); nm <- load(ss_, envir = e)
                    cbind(e[[nm[1]]]$longitude, e[[nm[1]]]$latitude) }, error = function(e) NULL),
    rd = tryCatch(sf::st_geometry(sf::st_transform(readRDS(rd_), 4326)), error = function(e) NULL),
    wc = tryCatch(terra::rast(here::here("Input Data/HighResolution/water_class.vrt")), error = function(e) NULL))
  ctx$rd_ferry <- tryCatch(readRDS(rd_)$is_ferry %in% TRUE, error = function(e) logical(0))
  out <- do.call(rbind, lapply(seq_len(nrow(p)), function(i) do.call(rbind, lapply(REF_ZOOMS, function(z) {
    t <- ref_lonlat_tile(p$lon[i], p$lat[i], z)
    tags <- unique(c(strsplit(p$tags[i], ",")[[1]],
                     .ref_content_tags(z, unname(t["x"]), unname(t["y"]), ctx)))
    data.frame(id = sprintf("%s_z%d", gsub("[^a-z0-9]+", "-", p$name[i]), z),
               name = p$name[i], tags = paste(tags, collapse = ","), z = z,
               x = unname(t["x"]), y = unname(t["y"]),
               lon = p$lon[i], lat = p$lat[i], stringsAsFactors = FALSE)
  }))))
  out[!duplicated(out$id), ]
}
