# =============================================================================
# run-tests.R — run this before opening a pull request
#
#     Rscript tests/run-tests.R
#
# Exits non-zero on failure, so it can gate CI or a pre-push hook.
#
# What this suite is for: this map is worked on by several people at once, on
# separate forks. The changes that hurt are not the ones that conflict in git --
# those get noticed. They are the ones that MERGE CLEANLY and leave a world that
# is subtly wrong: two features sharing a noise field, a coarse octave that
# moved so someone else's baked layer no longer matches the terrain, a clamp
# removed so the coastline invents a lake.
#
# None of that produces an error at render time. These tests are the only thing
# standing between a clean merge and a broken world.
# =============================================================================

suppressMessages({
  library(terra)
  library(here)
})

setwd(here::here())
suppressMessages(source(here::here("Functions/TileServer.R")))
source(here::here("tests/helper.R"))

cat(strrep("=", 66), "\n")
cat("EART-H map conformance suite\n")
cat(strrep("=", 66), "\n")
cat(sprintf("  repo       : %s\n", here::here()))
cat(sprintf("  map root   : %s\n",
            tryCatch(map_root(), error = function(e) "(absent - render tests skip)")))
cat(sprintf("  world seed : %d\n", WORLD_SEED))
cat(sprintf("  fields     : %d registered\n", length(NOISE_FIELDS)))

files <- c("test-noise-registry.R",
           "test-provenance.R",
           "test-determinism.R",
           "test-seamlessness.R",
           "test-crosszoom.R",
           "test-canon.R",
           "test-render.R",
           "test-output.R",
           "test-annotations.R")

for (f in files) {
  p <- here::here("tests", f)
  if (!file.exists(p)) { cat(sprintf("\n[%s] MISSING\n", f)); next }
  source(p, local = new.env(parent = globalenv()))
}

passed <- test_summary()

if (!passed) {
  cat("\nA failure here is not always a bug. If you deliberately changed what\n")
  cat("the world looks like, re-bless the golden fingerprint IN THE SAME COMMIT:\n")
  cat("    Rscript tests/bless-golden.R\n")
  cat("so a reviewer sees the world change and the code change together.\n\n")
}

quit(status = if (passed) 0L else 1L)
