# =============================================================================
# test-seamlessness.R — adjacent tiles must agree where they touch
#
# The largest class of error in this codebase. Every neighbourhood operation --
# smoothing, distance, warping, hillshade -- has to read data from BEYOND the
# tile it is rendering. Forget, and the tile is computed from a window that
# stops at its own edge, so it disagrees with its neighbour along the seam.
# The map then shows a faint grid, most visible on a shallow coast.
#
# These tests work on the FIELDS rather than on rendered PNGs, because a field
# test needs no map artefact and no tile pyramid, so it runs on a fresh clone.
# The field-level property is the one that matters: if the underlying field is
# continuous across the boundary, the render is too.
# =============================================================================

cat("\n[seamlessness]\n")

#' World coordinates of a column of points straddling a tile boundary.
#'
#' Samples `n` points either side of the vertical line x = x_edge, at the same
#' y. A seamless field is continuous there; a tile-relative one is not.
straddle <- function(x_edge, y0 = 1.2e6, n = 64, span = 400) {
  dx <- seq(-span, span, length.out = 2 * n)
  cbind(x = x_edge + dx, y = rep(y0, length(dx)))
}

test_that("noise fields are continuous across a tile boundary", {
  # A real z12 tile edge: mercator origin stepped by whole tiles.
  z <- 12L
  tile_m <- (2 * MERC_ORIGIN) / (2^z)
  x_edge <- -MERC_ORIGIN + 3000 * tile_m

  pts <- straddle(x_edge)
  v <- fbm_world(pts[, "x"], pts[, "y"], octaves = 6,
                 base_wavelength_m = nf_wl("terrain"))

  # Continuity: no step at the crossing larger than the field's own local
  # variation. Compare the jump at the midpoint against the largest jump
  # anywhere else along the transect.
  mid <- length(v) / 2
  jump_at_edge <- abs(v[mid + 1] - v[mid])
  jumps_elsewhere <- abs(diff(v))[-mid]

  ok(jump_at_edge <= max(jumps_elsewhere),
     sprintf("terrain field continuous at a z%d tile edge (step %.2e vs max %.2e)",
             z, jump_at_edge, max(jumps_elsewhere)))
})

test_that("the same world point gives the same value from either tile", {
  # The decisive property. Two tiles overlapping a point must compute the same
  # value for it -- which is automatic IF the field is sampled in world
  # coordinates, and false the moment anything is tile-relative.
  z <- 13L
  tile_m <- (2 * MERC_ORIGIN) / (2^z)
  x_edge <- -MERC_ORIGIN + 5000 * tile_m
  pt <- cbind(x = x_edge, y = 2.4e6)

  # "From the left tile" and "from the right tile" differ only in the window a
  # caller would have read; the field call is identical because it is absolute.
  a <- fbm_world(pt[, "x"], pt[, "y"], octaves = 6, base_wavelength_m = 4000)
  b <- fbm_world(pt[, "x"], pt[, "y"], octaves = 6, base_wavelength_m = 4000)
  ok(identical(a, b), "a shared boundary point evaluates identically")

  # And the drainage ladder, which is what carves valleys across seams.
  d1 <- ambient::gen_simplex(pt[, "x"], pt[, "y"],
                             frequency = 1 / nf_wl("drainage", 1),
                             seed = nf_seed("drainage", 1))
  d2 <- ambient::gen_simplex(pt[, "x"], pt[, "y"],
                             frequency = 1 / nf_wl("drainage", 1),
                             seed = nf_seed("drainage", 1))
  ok(identical(d1, d2), "drainage network evaluates identically at a seam")
})

test_that("the coarse read margin exceeds the warp amplitude", {
  # tile_setup() reads 8 coarse cells (~8.8 km) beyond the tile. Every warp
  # must fit inside that or a warped sample falls off the window into NA, which
  # renders as spurious land in open water.
  margin_m <- 8 * 1100          # 8 coarse cells at ~0.01 degree

  warps <- c(coastline = 900, biome = 600, veg.openness = 750)
  worst <- max(warps)
  ok(worst < margin_m,
     sprintf("largest warp %.0f m fits inside the %.0f m read margin",
             worst, margin_m))

  # River valleys reach further than any warp and are handled separately, by
  # river_valley_fields() expanding its own margin. Check the reach is declared.
  trunk_reach <- RIVER_TIER$big$fp_w + RIVER_TIER$big$wall_w
  ok(trunk_reach <= 1500,
     sprintf("trunk river valley reach %.0f m is within its 1500 m margin",
             trunk_reach))
})

test_that("no field is sampled in tile-relative coordinates", {
  # A static check, because the dynamic one needs a full render. Any call that
  # passes something other than world metres into fbm_world/gen_simplex is the
  # bug this whole file is about. Flag the obvious spellings.
  src <- unlist(lapply(NOISE_SCAN_FILES, function(f) {
    p <- here::here(f)
    if (file.exists(p)) readLines(p, warn = FALSE) else character(0)
  }))
  src <- src[!grepl("^\\s*#", src)]
  bad <- grep("(fbm_world|gen_simplex|gen_worley)\\s*\\([^)]*\\b(col|row|ix|iy|px_|tile_x|tile_y)\\b",
              src, value = TRUE)
  ok(length(bad) == 0,
     if (length(bad)) paste("suspicious tile-relative sampling:",
                            paste(trimws(bad), collapse = " | "))
     else "no noise call takes an obviously tile-relative coordinate")
})
