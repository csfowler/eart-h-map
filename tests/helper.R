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

#' Are the road/river, settlement and sacred-site caches built and current?
#' A render with features on would otherwise build them first (~4 minutes),
#' which is not something a test run should do by surprise.
feature_caches_warm <- function() {
  if (!have_map_root()) return(FALSE)
  warm <- function(path, stamp)
    file.exists(path) && isTRUE(cache_load(path, stamp, verbose = FALSE)$hit)
  vec <- file.exists(.tile_vec_cache()) && {
    raw <- try(readRDS(.tile_vec_cache()), silent = TRUE)
    !inherits(raw, "try-error") && is.list(raw) &&
      identical(raw$.prov_roads$combined,  .prov_roads_stamp()$combined) &&
      identical(raw$.prov_rivers$combined, .prov_rivers_stamp()$combined)
  }
  vec && warm(.tile_settle_cache(), .prov_setts_stamp()) &&
    warm(.tile_sacred_cache(), .prov_sacred_stamp())
}

#' Render one tile to a temp PNG and return its RGB values (rows = pixels).
#' Features are drawn only when their caches are warm; `features` reports
#' which it was, so a message can say what was actually tested.
render_rgb <- function(z, x, y, features = feature_caches_warm()) {
  p <- tempfile(fileext = ".png")
  render_elevation_tile(z, x, y, p, detail_amp = if (features) 1 else 0, verbose = FALSE)
  v <- terra::values(terra::rast(p))[, 1:3, drop = FALSE]
  unlink(p)
  v
}

#' lon/lat -> XYZ tile indices (XYZ y, which render_elevation_tile() takes).
lonlat_tile <- function(lon, lat, z) {
  n <- 2^z
  lat <- max(min(lat, 85.05), -85.05)
  r <- lat * pi / 180
  list(x = as.integer(floor((lon + 180) / 360 * n)),
       y = as.integer(floor((1 - log(tan(r) + 1 / cos(r)) / pi) / 2 * n)))
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
