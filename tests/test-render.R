# =============================================================================
# test-render.R — the suite renders a tile
#
# Everything else here tests fields, caches and invariants: pure functions of
# coordinates, evaluated without ever drawing anything. That left the single
# most likely thing a person breaks completely uncovered — an ordinary R error
# inside a drawing function. You could leave `draw_roads()` throwing and the
# whole suite stayed green; you found out by opening the map, which is the
# slowest feedback loop available and the worst one for someone new.
#
# So this renders real tiles and looks at the pixels. It is a SMOKE test, not a
# pixel comparison: it asserts that a tile comes out, at the right size, with
# real variation in it, and that water and land both appear. Comparing against a
# reference image would make every deliberate visual change a test failure,
# which is what the golden fingerprint is for and it does that job better.
#
# THE TILES ARE FOUND, NOT HARDCODED. The first version named specific z10/z13
# tiles taken from the author's own pyramid, which worked there and failed
# everywhere else: run against an export built with `continents = "Kiliman"` and
# every one of them was open ocean, so "real variation" and "land and water
# both present" failed on a perfectly good repo. A test that assumes the whole
# world is present cannot run on a partial checkout, which is exactly the
# checkout someone debugging an export has. These derive their tiles from the
# continent polygons that ship alongside the rasters.
# =============================================================================

cat("\n[render]\n")

if (!have_render_data()) {
  skip("Input Data rasters unavailable - render tests need a full checkout")
} else {

  .render_dir <- file.path(tempdir(), "eh-render-tests")
  dir.create(.render_dir, recursive = TRUE, showWarnings = FALSE)

  #' lon/lat -> XYZ tile indices. render_elevation_tile() wants XYZ y, so no
  #' TMS flip here.
  .lonlat_tile <- function(lon, lat, z) {
    n <- 2^z
    lat <- max(min(lat, 85.05), -85.05)
    r <- lat * pi / 180
    list(x = as.integer(floor((lon + 180) / 360 * n)),
         y = as.integer(floor((1 - log(tan(r) + 1 / cos(r)) / pi) / 2 * n)))
  }

  #' A point on a continent outline WITH RASTER DATA AROUND IT.
  #'
  #' Two separate things have to be true and only one of them is geometric.
  #' continent_polygons.gpkg always ships all ten outlines, but an export built
  #' with `continents = "Kiliman"` has rasters for one of them -- so picking the
  #' biggest polygon and trusting it gave a point in the middle of blank ocean,
  #' and the render tests failed on a perfectly good repo. So every candidate is
  #' checked against the canon that is actually present, and the first one with
  #' land and water in frame wins.
  .coast_point <- function() {
    f <- here::here("Input Data/continent_polygons.gpkg")
    wcf <- here::here("Input Data/HighResolution/water_class.vrt")
    if (!file.exists(f) || !file.exists(wcf)) return(NULL)
    p <- try(sf::st_read(f, quiet = TRUE), silent = TRUE)
    if (inherits(p, "try-error") || !nrow(p)) return(NULL)

    # s2 off for this block. These outlines are procedurally generated and some
    # have self-intersecting loops, which s2 rejects outright ("Loop 208 edge 8
    # crosses loop 228 edge 4") rather than tolerating -- the same trap
    # DivinityBuilder hit. Nothing here needs spherical exactness: it wants one
    # point on a coast.
    old_s2 <- sf::sf_use_s2()
    suppressMessages(sf::sf_use_s2(FALSE))
    on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)

    # Biggest first, by bounding box rather than area: st_area goes through the
    # same geometry engine that objects to these shapes.
    span <- vapply(seq_len(nrow(p)), function(i) {
      b <- sf::st_bbox(p[i, ])
      as.numeric((b["xmax"] - b["xmin"]) * (b["ymax"] - b["ymin"]))
    }, numeric(1))

    wc <- terra::rast(wcf)
    for (k in order(span, decreasing = TRUE)) {
      g  <- sf::st_geometry(p[k, ])
      co <- try(sf::st_coordinates(sf::st_cast(g, "MULTILINESTRING")), silent = TRUE)
      if (inherits(co, "try-error") || !nrow(co)) next
      for (j in unique(round(seq(1, nrow(co), length.out = 25)))) {
        # unname(): st_coordinates columns carry their names through, and
        # c(X = <named>) would produce "X.X".
        lon <- unname(co[j, "X"]); lat <- unname(co[j, "Y"])
        # Mercator degenerates near the poles: a z13 tile there covers almost no
        # ground and renders as a few KB of nothing.
        if (!is.finite(lon) || !is.finite(lat) || abs(lat) > 60) next
        w <- try(terra::values(terra::crop(
               wc, terra::ext(lon - 0.1, lon + 0.1, lat - 0.1, lat + 0.1))),
               silent = TRUE)
        if (inherits(w, "try-error") || !length(w)) next
        w <- w[!is.na(w)]
        if (!length(w)) next
        wet <- w == WATER_CLASS$OCEAN | w == WATER_CLASS$LAKE
        if (any(wet) && any(!wet)) return(c(X = lon, Y = lat))
      }
    }
    NULL
  }

  #' Read a PNG back as a raster and describe it, without trusting the writer.
  .png_facts <- function(p) {
    if (!file.exists(p)) stop("no file was written: ", p, call. = FALSE)
    if (!identical(readBin(p, "raw", n = 8),
                   as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))))
      stop("not a PNG (bad magic bytes)", call. = FALSE)
    r <- terra::rast(p); v <- terra::values(r)
    list(nrow = terra::nrow(r), ncol = terra::ncol(r), nlyr = terra::nlyr(r),
         kb = file.info(p)$size / 1024, v = v,
         distinct = length(unique(round(rowSums(v[, 1:3, drop = FALSE])))))
  }

  .pt <- .coast_point()

  if (is.null(.pt)) {
    skip("continent_polygons.gpkg unavailable - cannot locate a land tile")
  } else {

    test_that("a tile renders at all, without features", {
      # detail_amp = 0 turns the feature pass off, so this is the terrain,
      # coastline, biome, vegetation and compositing path on its own.
      t <- .lonlat_tile(.pt[["X"]], .pt[["Y"]], 11L)
      p <- file.path(.render_dir, "plain.png")
      render_elevation_tile(11L, t$x, t$y, p, detail_amp = 0, verbose = FALSE)
      f <- .png_facts(p)
      ok(f$nrow == 256 && f$ncol == 256,
         sprintf("elevation tile is 256x256 (got %dx%d)", f$ncol, f$nrow))
      ok(f$nlyr == 4, sprintf("tile has RGBA, 4 bands (got %d)", f$nlyr))
      ok(f$distinct > 50,
         sprintf("tile has real variation, not a flat fill (%d distinct levels)",
                 f$distinct))
    })

    test_that("the biome layer renders the same tile", {
      # Both layers share the coastline warp, so they must agree on geometry.
      t <- .lonlat_tile(.pt[["X"]], .pt[["Y"]], 11L)
      pe <- file.path(.render_dir, "reg_elev.png")
      pb <- file.path(.render_dir, "reg_biome.png")
      render_elevation_tile(11L, t$x, t$y, pe, detail_amp = 0, verbose = FALSE)
      render_biome_tile(11L, t$x, t$y, pb, verbose = FALSE)
      fe <- .png_facts(pe); fb <- .png_facts(pb)
      ok(fe$ncol == fb$ncol && fe$nrow == fb$nrow,
         sprintf("both layers render at the same size (%dx%d)", fb$ncol, fb$nrow))
    })

    test_that("a coastal tile shows both land and water", {
      # The failure this guards is the one CLAUDE.md warns about twice: deriving
      # water from the sign of elevation, or losing a nodata flag, turns a coast
      # into all-sea or all-land. It still renders; it is just wrong.
      t <- .lonlat_tile(.pt[["X"]], .pt[["Y"]], 11L)
      p <- file.path(.render_dir, "coast.png")
      render_elevation_tile(11L, t$x, t$y, p, detail_amp = 0, verbose = FALSE)
      v <- .png_facts(p)$v
      blueish <- v[, 3] > v[, 1] + 10          # water paint is blue-dominant
      frac <- mean(blueish, na.rm = TRUE)
      ok(frac > 0.02 && frac < 0.98,
         sprintf("tile on a continent outline is a mix (%.0f%% water-toned)",
                 100 * frac))
    })

    test_that("the full feature pass renders", {
      # Roads, rivers, settlements, sacred sites and every draw_* function.
      # Runs only on warm caches: a cold rebuild is ~3 minutes and this suite is
      # meant to be run before every push.
      if (!have_map_root()) {
        skip("no map root - the feature caches live under it"); return(invisible(NULL))
      }
      if (!feature_caches_warm()) {
        skip("feature caches are cold or stale - run the tile server once, then re-run")
        return(invisible(NULL))
      }

      # A tile over the biggest settlement present, so roads and buildings are
      # certain to be in frame.
      sf_ <- here::here("Input Data/Combined/settlements_final.rds")
      if (!file.exists(sf_)) {
        skip("settlements_final.rds unavailable"); return(invisible(NULL))
      }
      s <- readRDS(sf_)
      # Away from the poles. The largest settlement on this world sits at
      # latitude 89 on the polar continent, where Mercator degenerates and a
      # z13 tile covers almost no ground -- it rendered as 2 KB of nothing and
      # looked like a broken feature pass. 60 degrees is well inside the range
      # anyone actually browses.
      cand <- which(abs(s$lat) < 60 & is.finite(s$population))
      if (!length(cand)) {
        skip("no non-polar settlement in this checkout"); return(invisible(NULL))
      }
      i <- cand[which.max(s$population[cand])]
      t <- .lonlat_tile(s$lon[i], s$lat[i], 13L)

      p <- file.path(.render_dir, "features.png")
      render_elevation_tile(13L, t$x, t$y, p, verbose = FALSE)
      f <- .png_facts(p)
      ok(f$nrow == 256 && f$ncol == 256,
         sprintf("full-feature z13 tile over %s is 256x256 (%.0f KB)",
                 if (!is.null(s$name)) s$name[i] else "the largest settlement", f$kb))
      ok(f$distinct > 150,
         sprintf("full-feature tile is richer than the plain one (%d levels)",
                 f$distinct))
    })
  }
}
