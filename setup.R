# =============================================================================
# setup.R — install what the map needs and check the machine is ready.
#
#     source("setup.R")        # from RStudio, with eart-h-map.Rproj open
#
# Safe to re-run: it installs only what is missing, then reports. The package
# list lives here and nowhere else, so GETTING-STARTED does not repeat it.
# =============================================================================

PACKAGES <- c(
  "terra",          # rasters
  "sf",             # vectors
  "ambient",        # simplex noise: all procedural detail
  "jsonlite",       # map data files
  "here",           # project-relative paths
  "digest",         # cache provenance hashes
  "plumber",        # the tile server
  "leaflet",        # the web map
  "shiny",          # the annotation editor ...
  "leaflet.extras", # ... and its drawing tools
  "dplyr", "magrittr"
)

missing <- PACKAGES[!vapply(PACKAGES, requireNamespace, NA, quietly = TRUE)]
if (length(missing)) {
  cat("Installing:", paste(missing, collapse = ", "), "\n\n")
  install.packages(missing)
}

ok  <- function(x, msg) cat(sprintf("  %s  %s\n", if (x) "ok  " else "MISSING", msg))
cat("\nEART-H map setup check\n")

still <- PACKAGES[!vapply(PACKAGES, requireNamespace, NA, quietly = TRUE)]
ok(!length(still), if (length(still)) paste("R packages:", paste(still, collapse = ", "))
                   else sprintf("R packages (%d)", length(PACKAGES)))

root_ok <- file.exists(here::here("Functions", "MapBuilder.R"))
ok(root_ok, if (root_ok) paste("project root:", here::here())
            else "project root: open eart-h-map.Rproj first, then re-run")

# Only the first build (rebuild_tiles = TRUE) needs the GDAL command-line
# tools; MapBuilder repeats this search and says what to install.
gdal <- if (root_ok) {
  suppressMessages(source(here::here("Functions/MapBuilder.R")))
  detect_gdal_paths()
}
ok(!is.null(gdal), if (!is.null(gdal)) paste("GDAL command-line tools:", gdal$name)
                   else "GDAL command-line tools: see GETTING-STARTED step 4")

ok(nzchar(Sys.which("git")), "git (GETTING-STARTED step 1b)")
gh <- Sys.which("gh")
gh_in <- nzchar(gh) && suppressWarnings(system2(gh, c("auth", "status"), stdout = FALSE, stderr = FALSE)) == 0
ok(gh_in, if (!nzchar(gh)) "GitHub CLI 'gh' (step 1b)" else if (gh_in) "GitHub CLI 'gh', signed in"
          else "GitHub CLI 'gh' installed but NOT signed in: run  gh auth login")

py <- nzchar(Sys.which("python")) || nzchar(Sys.which("python3"))
ok(py, "Python (only for start-map, the static player map)")

if (length(still) && .Platform$OS.type == "windows")
  cat("\nIf terra or sf failed to install, install Rtools and re-run.\n")
cat("\n")
