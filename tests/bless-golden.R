# =============================================================================
# bless-golden.R — record a DELIBERATE change to what the world looks like
#
#     Rscript tests/bless-golden.R
#
# Run this only when you meant to change the world, and commit the regenerated
# tests/golden/noise-fingerprint.rds in the SAME commit as the code that caused
# it. That pairing is the whole point: a reviewer seeing golden/ move alongside
# a change to drainage_incision() understands immediately that valleys have
# moved. Seeing it move on its own, or not at all, tells them nothing.
#
# Blessing to make a red test go green, without understanding which fields
# moved and why, discards the only protection the other forks have.
# =============================================================================

suppressMessages({ library(terra); library(here) })
setwd(here::here())
suppressMessages(source(here::here("Functions/TileServer.R")))
source(here::here("tests/fingerprint-spec.R"))

gf  <- here::here("tests/golden/noise-fingerprint.rds")
new <- noise_fingerprint()
old <- if (file.exists(gf)) readRDS(gf) else list()

added   <- setdiff(names(new), names(old))
removed <- setdiff(names(old), names(new))
shared  <- intersect(names(new), names(old))
moved   <- shared[vapply(shared, function(n) !identical(new[[n]], old[[n]]), logical(1))]

cat("Blessing the golden fingerprint\n")
cat(strrep("-", 60), "\n")
cat(sprintf("  unchanged : %d\n", length(shared) - length(moved)))
cat(sprintf("  MOVED     : %d\n", length(moved)))
for (m in moved) cat("      ", m, "\n")
cat(sprintf("  added     : %d\n", length(added)))
for (a in added) cat("      ", a, "\n")
cat(sprintf("  removed   : %d\n", length(removed)))
for (r in removed) cat("      ", r, "\n")
cat(strrep("-", 60), "\n")

if (!length(moved) && !length(added) && !length(removed)) {
  cat("Nothing changed - golden file left alone.\n")
} else {
  dir.create(dirname(gf), recursive = TRUE, showWarnings = FALSE)
  saveRDS(new, gf)
  cat("Written:", gf, "\n")
  cat("\nCommit this file together with the code change that moved it, and say\n")
  cat("in the commit message WHICH fields moved and why.\n")
}
