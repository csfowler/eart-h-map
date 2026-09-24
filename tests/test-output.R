# =============================================================================
# test-output.R — the invariants, checked on RENDERED tiles
#
# test-seamlessness, test-crosszoom and test-canon check the noise FIELDS. A
# change can keep every field perfect and still break the picture: normalise
# colours per tile and every seam shows; blur without a margin and the tile
# edge darkens; paint a new layer blue and the coast "moves". None of that
# touches a field, so these tests look at the pixels that ship.
#
# Each limit is a multiple of what the current renderer measures, so a failure
# means "clearly worse than it was", not "slightly different". The measured
# value is printed on every run; if a deliberate change moves one, raise the
# limit in the same commit and say why.
# =============================================================================

cat("\n[rendered output]\n")

SEAM_RATIO_MAX   <- 3.0   # step across a tile edge vs the steps beside it
XZOOM_DIFF_MAX   <- 5     # mean |z13 children averaged - z12 parent|, 0-255. Measured
                          # 0.7-1.5 when correct; 10-30 for the WRONG tile (a neighbour's children)
CANON_MISMATCH   <- 0.03  # share of clearly-land/clearly-water pixels painted wrong
BLACK_PIXELS_MAX <- 0.01  # opaque black is how NA has leaked before

if (!have_render_data()) {
  skip("Input Data rasters unavailable - output tests need a full checkout")
} else {
  source(here::here("tests/reference-tiles.R"))
  .places <- reference_places()
  .place  <- function(name) {
    r <- .places[.places$name == name, ]
    if (nrow(r)) c(r$lon[1], r$lat[1])
  }
  .feat <- feature_caches_warm()
  .with <- if (.feat) "with features" else "terrain only (feature caches cold)"

  # Max channel difference between pixel rows/columns, averaged.
  .step <- function(a, b) mean(apply(abs(a - b), 1, max))
  .col  <- function(v, j) v[seq(j, by = 256, length.out = 256), , drop = FALSE]  # column j, top to bottom
  .row  <- function(v, i) v[(i - 1) * 256 + 1:256, , drop = FALSE]

  test_that("adjacent rendered tiles meet without a visible seam", {
    pt <- .place("city") %||% .place("coast")
    if (is.null(pt)) { skip("no settled place in this checkout"); return(invisible(NULL)) }
    t <- lonlat_tile(pt[1], pt[2], 12L)
    L <- render_rgb(12L, t$x, t$y, .feat); R <- render_rgb(12L, t$x + 1L, t$y, .feat)
    D <- render_rgb(12L, t$x, t$y + 1L, .feat)
    # Judge the edge step against the steps just beside it, on both sides: the
    # interior of a tile can be far busier or calmer than the strip at its edge,
    # and a seam is only visible relative to its own surroundings. The floor of
    # 1 level keeps a flat strip (open water) from turning noise into a ratio.
    local <- function(a1, a2, b1, b2, edge) edge / max(1, mean(c(.step(a1, a2), .step(b1, b2))))
    ew <- local(.col(L, 254), .col(L, 255), .col(R, 2), .col(R, 3), .step(.col(L, 256), .col(R, 1)))
    ns <- local(.row(L, 254), .row(L, 255), .row(D, 2), .row(D, 3), .step(.row(L, 256), .row(D, 1)))
    ok(ew < SEAM_RATIO_MAX,
       sprintf("east-west tile edge step is %.2fx the neighbouring steps (max %.1f; %s)", ew, SEAM_RATIO_MAX, .with))
    ok(ns < SEAM_RATIO_MAX,
       sprintf("north-south tile edge step is %.2fx the neighbouring steps (max %.1f)", ns, SEAM_RATIO_MAX))
  })

  test_that("zooming in refines a tile rather than replacing it", {
    pt <- .place("temperate forest") %||% .place("taiga") %||% .place("grassland")
    if (is.null(pt)) { skip("no vegetated place in this checkout"); return(invisible(NULL)) }
    t <- lonlat_tile(pt[1], pt[2], 12L)
    parent <- render_rgb(12L, t$x, t$y, .feat)
    kids <- lapply(0:3, function(k) render_rgb(13L, 2L * t$x + k %% 2L, 2L * t$y + k %/% 2L, .feat))
    # Average each 2x2 block of the 512x512 child mosaic down to the parent grid.
    down <- matrix(0, 256 * 256, 3)
    for (k in 0:3) {
      v <- kids[[k + 1]]
      for (ch in 1:3) {
        m <- matrix(v[, ch], 256, 256, byrow = TRUE)
        s <- (m[c(TRUE, FALSE), c(TRUE, FALSE)] + m[c(FALSE, TRUE), c(TRUE, FALSE)] +
              m[c(TRUE, FALSE), c(FALSE, TRUE)] + m[c(FALSE, TRUE), c(FALSE, TRUE)]) / 4
        rows <- (k %/% 2) * 128 + 1:128; cols <- (k %% 2) * 128 + 1:128
        idx <- as.vector(t(outer((rows - 1) * 256, cols, `+`)))
        down[idx, ch] <- as.vector(t(s))
      }
    }
    d <- mean(abs(down - parent))
    ok(d < XZOOM_DIFF_MAX,
       sprintf("z13 children average to within %.1f of their z12 parent (max %g)", d, XZOOM_DIFF_MAX))
  })

  test_that("rendered water sits where canon says water", {
    pt <- .place("coast") %||% .place("port")
    if (is.null(pt)) { skip("no coastal place in this checkout"); return(invisible(NULL)) }
    t <- lonlat_tile(pt[1], pt[2], 12L)
    v <- render_rgb(12L, t$x, t$y, .feat)
    e <- tile_extent_3857(12L, t$x, t$y)
    tmpl <- terra::rast(terra::ext(e$xmin, e$xmax, e$ymin, e$ymax), ncol = 256, nrow = 256, crs = "EPSG:3857")
    wc <- terra::crop(terra::rast(here::here("Input Data/HighResolution/water_class.vrt")),
                      tile_bbox_4326(12L, t$x, t$y, margin_cells = 3))
    # Only judge pixels whose whole 3x3 coarse neighbourhood agrees: the coast
    # warp may legitimately move the shore by up to one coarse cell. Rivers and
    # streams (classes 3-4) are painted blue on land, so they are left out too.
    homog <- terra::focal(wc, 3, "min") == terra::focal(wc, 3, "max")
    cls <- terra::values(terra::project(terra::ifel(homog, wc, NA), tmpl, method = "near"))[, 1]
    wet  <- cls %in% c(WATER_CLASS$OCEAN, WATER_CLASS$LAKE)
    land <- cls %in% WATER_CLASS$LAND
    blue <- v[, 3] > v[, 1] + 10
    judged <- wet | land
    if (sum(judged) < 1000) { skip("too few unambiguous pixels on this tile"); return(invisible(NULL)) }
    mis <- mean((wet & !blue) | (land & blue)) / mean(judged)
    ok(mis < CANON_MISMATCH,
       sprintf("%.2f%% of unambiguous pixels disagree with water_class (max %.0f%%; %d judged)",
               100 * mis, 100 * CANON_MISMATCH, sum(judged)))
  })

  test_that("a tile renders byte-identically in a fresh R process", {
    pt <- .place("village") %||% .place("city")
    if (is.null(pt)) { skip("no settled place in this checkout"); return(invisible(NULL)) }
    t <- lonlat_tile(pt[1], pt[2], 13L)
    rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
    one <- function(i) {
      png <- tempfile(fileext = ".png"); scr <- tempfile(fileext = ".R")
      writeLines(c(
        sprintf('setwd(%s)', deparse(here::here())),
        'suppressMessages(source(here::here("Functions/TileServer.R")))',
        sprintf('render_elevation_tile(13L, %dL, %dL, %s, detail_amp = %d)',
                t$x, t$y, deparse(png), if (.feat) 1L else 0L)), scr)
      system2(rscript, shQuote(scr), stdout = FALSE, stderr = FALSE)
      if (file.exists(png)) unname(tools::md5sum(png)) else NA_character_
    }
    h <- c(one(1), one(2))
    ok(!anyNA(h) && identical(h[1], h[2]),
       sprintf("two separate R processes wrote the same bytes (%s)", .with))
  })

  test_that("awkward places still render cleanly", {
    black <- function(v) mean(rowSums(v) == 0)   # render_rgb() tiles are always opaque
    cases <- list()
    # Open ocean: a z10 tile whose centre has no canon at all.
    for (lon in seq(-170, 170, by = 20)) {
      if (!.ref_has_data(lon, 0)) { cases$`open ocean` <- lonlat_tile(lon, 0, 10L); break }
    }
    cases$`far north (lat 75)` <- lonlat_tile(10, 75, 10L)
    cases$`antimeridian (west edge)` <- list(x = 0L, y = 2L^9L)
    cases$`antimeridian (east edge)` <- list(x = 2L^10L - 1L, y = 2L^9L)
    for (nm in names(cases)) {
      t <- cases[[nm]]
      v <- render_rgb(10L, t$x, t$y, .feat)
      ok(nrow(v) == 256 * 256 && black(v) < BLACK_PIXELS_MAX,
         sprintf("%s renders, %.1f%% opaque black", nm, 100 * black(v)))
    }
  })
}
