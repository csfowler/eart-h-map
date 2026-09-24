# =============================================================================
# helper.R — a tiny assertion harness
#
# Deliberately not testthat. This suite has to run on a student laptop that has
# just installed nine spatial packages, and adding a tenth to run the tests is
# a reason not to run them. Everything here is base R.
# =============================================================================

.TEST_STATE <- new.env(parent = emptyenv())
.TEST_STATE$pass <- 0L
.TEST_STATE$fail <- 0L
.TEST_STATE$skip <- 0L
.TEST_STATE$failures <- character(0)
.TEST_STATE$current <- "(none)"

test_that <- function(desc, code) {
  .TEST_STATE$current <- desc
  res <- tryCatch(force(code),
                  error = function(e) structure(conditionMessage(e), class = "test_error"))
  if (inherits(res, "test_error")) {
    .TEST_STATE$fail <- .TEST_STATE$fail + 1L
    .TEST_STATE$failures <- c(.TEST_STATE$failures, sprintf("%s\n      %s", desc, res))
    cat(sprintf("  FAIL  %s\n        %s\n", desc, res))
  }
  invisible(NULL)
}

ok <- function(cond, msg) {
  if (!isTRUE(cond)) stop(msg, call. = FALSE)
  .TEST_STATE$pass <- .TEST_STATE$pass + 1L
  cat(sprintf("  ok    %s\n", msg))
  invisible(TRUE)
}

skip <- function(msg) {
  .TEST_STATE$skip <- .TEST_STATE$skip + 1L
  cat(sprintf("  skip  %s\n", msg))
  invisible(TRUE)
}

#' Is the data needed for a render available on this machine?
#'
#' The VRTs are in the repo, but a VRT whose sources are unreachable OPENS and
#' CROPS without complaint and only fails at project() time -- so this forces a
#' real read rather than trusting file.exists().
have_render_data <- function() {
  v <- here::here("Input Data/HighResolution/elevation.vrt")
  if (!file.exists(v)) return(FALSE)
  r <- try({
    x <- terra::rast(v)
    terra::crop(x, terra::ext(x) * 0.001) * 1
  }, silent = TRUE)
  !inherits(r, "try-error")
}

#' Does the map artefact exist? The vector caches (roads, rivers, settlements)
#' live under it, so the feature-drawing path cannot run without one.
have_map_root <- function() {
  r <- tryCatch(map_root(), error = function(e) NULL)
  !is.null(r) && dir.exists(r)
}

test_summary <- function() {
  s <- .TEST_STATE
  cat(sprintf("\n%s\n", strrep("-", 66)))
  cat(sprintf("  %d passed   %d failed   %d skipped\n", s$pass, s$fail, s$skip))
  if (s$fail) {
    cat("\nFailures:\n")
    for (f in s$failures) cat("  - ", f, "\n", sep = "")
  }
  cat(sprintf("%s\n", strrep("-", 66)))
  invisible(s$fail == 0L)
}
