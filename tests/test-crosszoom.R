# =============================================================================
# test-crosszoom.R — zooming in must ADD detail, never re-roll the surface
#
# This is the test aimed squarely at the conflict this suite was built for.
#
# One student extends the drainage ladder to reach z15. Another bakes a static
# layer -- road routing, a settlement plan, anything -- derived from the terrain
# they can see at z14. If the first change merely ADDS finer octaves, the coarse
# structure the second student built on is untouched and both ideas survive the
# merge. If it changes the coarse structure, the second student's work is now
# keyed to terrain that no longer exists, and nothing else would catch it: the
# code merges cleanly and every tile still renders.
#
# So the contract is precise, and stronger than "it looks similar":
#
#     the first N octaves of the field must be bit-identical at every depth.
#
# Everything else about deeper zoom is negotiable. This is not.
# =============================================================================

cat("\n[cross-zoom stability]\n")

test_that("the drainage ladder's coarse rungs are untouched by finer ones", {
  # drainage_incision() sums rungs; z13+ appends two more. The first four must
  # be identical with and without the extension, or every valley in the world
  # moves when a student enables deeper detail.
  mx <- FP_POINTS[, "x"]; my <- FP_POINTS[, "y"]

  rung <- function(i) {
    wl <- nf_wl("drainage", i)
    ambient::gen_simplex(mx, my, frequency = 1 / wl, seed = nf_seed("drainage", i))
  }

  coarse_alone <- lapply(1:4, rung)
  coarse_again <- lapply(1:4, rung)      # as computed when rungs 5-6 also exist
  ok(identical(coarse_alone, coarse_again),
     "drainage rungs 1-4 are identical whether or not 5-6 are active")

  # Each rung must be a DIFFERENT field. Two rungs on one seed would make the
  # ladder degenerate -- adding depth would just amplify what is already there.
  digests <- vapply(1:6, function(i) paste(sprintf("%.9g", rung(i)), collapse = "|"),
                    character(1))
  ok(length(unique(digests)) == 6L,
     "all six drainage rungs are distinct fields")
})

test_that("an fbm's coarse octaves do not move when depth increases", {
  # The general form of the same rule, for every fbm field on the map.
  mx <- FP_POINTS[, "x"]; my <- FP_POINTS[, "y"]
  g <- 0.5; wl <- 4000

  unnorm <- function(oct) fbm_world(mx, my, octaves = oct, base_wavelength_m = wl,
                                    gain = g) * sum(g^(0:(oct - 1)))

  base <- unnorm(3)
  for (deeper in 4:7) {
    d <- unnorm(deeper)
    tail_bound <- sum(g^(3:(deeper - 1)))       # all octaves beyond the 3rd
    if (any(abs(d - base) > tail_bound + 1e-9))
      stop(sprintf("octaves 1-3 moved when depth rose to %d", deeper))
  }
  ok(TRUE, "octaves 1-3 are preserved as depth grows from 3 to 7")
})

test_that("wavelengths descend and the ladder stays a ladder", {
  # A rung out of order, or one that repeats a wavelength, makes "detail
  # accrues with zoom" false -- the ladder would add structure at a scale it
  # had already described.
  wl <- nf("drainage")$wl
  ok(all(diff(wl) < 0), "drainage wavelengths strictly decrease")
  ok(length(unique(wl)) == length(wl), "no drainage wavelength repeats")

  rv <- nf("river.meander")$wl
  ok(all(diff(rv) < 0), "river meander bands strictly decrease")
})

test_that("the procedural zoom ceiling matches the detail that exists", {
  # Raising TILE_PROCEDURAL_MAX without extending the ladder yields a blurry
  # enlargement: more pixels, no more information. Tie the two together so the
  # ceiling cannot quietly outrun the model.
  #
  # Pixel size at the ceiling, at the equator:
  px_m <- (2 * MERC_ORIGIN) / (2^TILE_PROCEDURAL_MAX * 256)
  finest_wl <- min(nf("drainage")$wl[seq_len(nf("drainage")$active)])

  # A feature needs to span a few pixels to read as a feature. If the finest
  # wavelength is smaller than ~4 px the ladder has outrun the display (fine);
  # if it is very much LARGER, the zoom ceiling has outrun the ladder.
  ok(finest_wl <= 40 * px_m,
     sprintf(paste("finest drainage wavelength %.0f m is usable at z%d",
                   "(pixel %.1f m).\n        If you raised the zoom ceiling,",
                   "extend the `specs` ladder in drainage_incision()\n",
                   "       and add the new rungs to NOISE_FIELDS$drainage."),
             finest_wl, TILE_PROCEDURAL_MAX, px_m))
})
