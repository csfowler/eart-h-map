# =============================================================================
# ci-prepare.R — bring a fresh checkout to "map built, caches warm"
#
#     Rscript tests/ci-prepare.R
#
# A clean clone has no Map/, so the suite would skip the full feature pass and
# the reference renders would each pay for the cache build. This does what a
# student's first session does, minus the base tile pyramid (which needs the
# GDAL tools and nothing here reads): the vector map, then the road/river,
# settlement and sacred-site caches, by rendering one tile with features on.
#
# Idempotent. With the map already present, build_reference_map() is skipped;
# the caches rebuild only if their provenance says so, which is how CI reuses a
# base-branch map for the head branch and only pays for what the change moved.
# =============================================================================

suppressMessages({ library(terra); library(here) })
setwd(here::here())
suppressMessages(source(here::here("Functions/TileServer.R")))

t0 <- Sys.time()
el <- function() sprintf("%4.0f s", as.numeric(difftime(Sys.time(), t0, units = "secs")))

root <- map_root(require_exists = FALSE)
# Rebuild when anything build_reference_map() reads has changed, judged by
# content: a fresh checkout stamps every file with the checkout time, so an
# mtime test would call a perfectly good cached map stale on every CI run.
#
# Only what the build reads, and big rasters by size: in the pipeline repo
# Input Data/ runs to many gigabytes, and hashing it all on every run would
# cost more than the build it is trying to skip.
map_inputs <- c("Functions/MapBuilder.R", "Functions/map_template.html",
                "Input Data/continent_polygons.gpkg",
                file.path("Input Data", c("Combined/settlements_final.rds", "Roads/road_routes.rds",
                                          "Divinity/sacred_sites_attributed.RData",
                                          "Divinity/hex_hierarchy_sf.RData")),
                list.files("Input Data/Annotations", full.names = TRUE),
                list.files("Input Data/HighResolution", pattern = "\\.vrt$", full.names = TRUE),
                Sys.glob("Input Data/Continents/*/rivers.tif"),
                Sys.glob("Input Data/Continents/*/flow_accumulation.tif"))
map_inputs <- map_inputs[file.exists(map_inputs)]
big <- file.info(map_inputs)$size > 20 * 1024^2
sig <- paste(c(tools::md5sum(map_inputs[!big]),
               paste(map_inputs[big], file.info(map_inputs[big])$size)), collapse = "")
stamp <- file.path(root, ".ci-map-inputs")
if (file.exists(file.path(root, "index.html")) && file.exists(stamp) &&
    identical(readLines(stamp, warn = FALSE), sig)) {
  cat(el(), " map present and current at", root, "\n")
} else {
  cat(el(), " building vector map at", root, "\n")
  suppressMessages(build_reference_map(verbose = FALSE))
  writeLines(sig, stamp)
}

cat(el(), " road + river geometry\n");  invisible(get_tile_vectors())
s <- readRDS(here::here("Input Data/Combined/settlements_final.rds"))
i <- which(abs(s$lat) < 60)[which.max(s$population[abs(s$lat) < 60])]
n <- 2^13; r <- s$lat[i] * pi / 180
x <- floor((s$lon[i] + 180) / 360 * n); y <- floor((1 - log(tan(r) + 1 / cos(r)) / pi) / 2 * n)
cat(el(), " settlement + sacred caches\n")
invisible(render_elevation_tile(13L, x, y, tempfile(fileext = ".png")))
cat(el(), " ready\n")
