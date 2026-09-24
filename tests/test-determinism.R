# =============================================================================
# test-determinism.R — the world is a pure function of world coordinates
#
# Two properties, both of which the whole design rests on:
#
#   SAME INPUT, SAME OUTPUT. Evaluate a field twice and get identical numbers.
#   Sounds trivial; it is not. Anything that reaches for the RNG, the clock,
#   the locale, or a tile-relative coordinate breaks it, and the symptom is a
#   map that changes when you pan away and come back.
#
#   NOTHING MOVED UNLESS YOU MEANT IT. The golden fingerprint pins every field
#   at fixed world points. A structural change -- pulling literals into the
#   registry, renaming, reorganising -- must leave all of them untouched.
#
# A golden failure is a QUESTION, not a verdict: did you mean to change what
# the world looks like? If yes, re-bless the file in the same commit
# (Rscript tests/bless-golden.R) so a reviewer sees the world change and the
# code change together.
# =============================================================================

cat("\n[determinism]\n")

source(here::here("tests/fingerprint-spec.R"))

test_that("fields are pure functions of position", {
  a <- noise_fingerprint()
  b <- noise_fingerprint()
  ok(identical(a, b),
     sprintf("%d fields identical across two evaluations", length(a)))
})

test_that("evaluation order does not matter", {
  # A field that depended on call order would betray hidden state. Evaluate the
  # points shuffled and compare pointwise.
  set.seed(1)                       # only the shuffle; not the fields
  idx <- sample(nrow(FP_POINTS))
  shuffled <- noise_fingerprint(FP_POINTS[idx, , drop = FALSE])
  straight <- noise_fingerprint()

  # Only entries that are one value PER POINT can be permuted. Some entries are
  # parameter summaries (drainage.profile pins the ladder's widths and weights),
  # which have no per-point structure to shuffle.
  npt <- nrow(FP_POINTS)
  bad <- character(0); checked <- 0L
  for (n in names(straight)) {
    s <- strsplit(straight[[n]], "|", fixed = TRUE)[[1]]
    h <- strsplit(shuffled[[n]], "|", fixed = TRUE)[[1]]
    if (length(s) != npt || length(h) != npt) next      # not a per-point field
    checked <- checked + 1L
    if (!identical(s[idx], h)) bad <- c(bad, n)
  }
  ok(length(bad) == 0,
     if (length(bad)) paste("order-dependent:", paste(bad, collapse = ", "))
     else sprintf("all %d per-point fields are independent of evaluation order",
                  checked))
})

test_that("no field has drifted from the golden fingerprint", {
  gf <- here::here("tests/golden/noise-fingerprint.rds")
  if (!file.exists(gf)) {
    skip("no golden file yet - run tests/bless-golden.R to create one")
    return(invisible(NULL))
  }
  old <- readRDS(gf)
  new <- noise_fingerprint()

  added   <- setdiff(names(new), names(old))
  removed <- setdiff(names(old), names(new))
  shared  <- intersect(names(new), names(old))
  moved   <- shared[vapply(shared, function(n) !identical(new[[n]], old[[n]]), logical(1))]

  if (length(added))
    cat(sprintf("        note: %d new field(s) not in golden: %s\n",
                length(added), paste(added, collapse = ", ")))
  if (length(removed))
    cat(sprintf("        note: %d field(s) gone from the spec: %s\n",
                length(removed), paste(removed, collapse = ", ")))

  ok(length(moved) == 0,
     if (length(moved))
       paste0(length(moved), " field(s) changed value: ",
              paste(moved, collapse = ", "),
              "\n        If this was intentional, re-bless the golden file in the\n",
              "        SAME commit: Rscript tests/bless-golden.R")
     else sprintf("all %d shared fields match the golden fingerprint", length(shared)))
})

test_that("adding octaves adds detail rather than re-rolling the surface", {
  # The property that makes zoom work: the coarse octaves of an fbm must be
  # IDENTICAL at every depth, so zooming only adds finer structure. This is
  # exactly what a student extending the ladder for z15 must not break.
  #
  # fbm_world normalises by summed amplitude, so the two depths differ by a
  # known scalar. Undo it and the coarse part must match to floating point.
  mx <- FP_POINTS[, "x"]; my <- FP_POINTS[, "y"]
  g <- 0.5
  s4 <- sum(g^(0:3)); s6 <- sum(g^(0:5))

  a4 <- fbm_world(mx, my, octaves = 4, base_wavelength_m = 4000, gain = g) * s4
  a6 <- fbm_world(mx, my, octaves = 6, base_wavelength_m = 4000, gain = g) * s6

  # a6 is a4 plus two finer octaves; their amplitude is bounded by g^4 + g^5.
  resid <- abs(a6 - a4)
  ok(all(resid <= (g^4 + g^5) + 1e-9),
     "octaves 5-6 only ADD to the surface octaves 1-4 already describe")
})
