# =============================================================================
# render-reference.R — render the reference tiles with the code checked out
#
#     Rscript tests/render-reference.R <out_dir> [--tiles <tiles.csv>]
#
# Writes <out_dir>/tiles/<id>.png, <out_dir>/manifest.csv (one row per tile:
# status, seconds, error) and <out_dir>/tiles.csv (the set that was rendered).
# Run it once on the base and once on the change, then compare the two with
# tests/compare-renders.R.
#
# --tiles makes the second run render EXACTLY the first run's set, so a change
# that edits reference-tiles.R cannot quietly move the goalposts.
#
# Set EARTH_MAP_ROOT to a scratch directory. The feature caches live under the
# map root and this will build them if they are cold; pointed at a real map, a
# student's code would write its own cache into it.
# =============================================================================

suppressMessages({ library(terra); library(here) })
setwd(here::here())

a <- commandArgs(trailingOnly = TRUE)
if (!length(a)) stop("usage: Rscript tests/render-reference.R <out_dir> [--tiles <tiles.csv>]")
out <- normalizePath(a[1], winslash = "/", mustWork = FALSE)
tiles_csv <- if ("--tiles" %in% a) a[which(a == "--tiles") + 1L] else NULL

suppressMessages(source(here::here("Functions/TileServer.R")))
source(here::here("tests/reference-tiles.R"))

dir.create(file.path(out, "tiles"), recursive = TRUE, showWarnings = FALSE)
tiles <- if (!is.null(tiles_csv)) utils::read.csv(tiles_csv, stringsAsFactors = FALSE) else reference_tiles()
utils::write.csv(tiles, file.path(out, "tiles.csv"), row.names = FALSE)

# Warm every cache BEFORE the clock starts, so render times measure rendering
# and not a one-off 4-minute geometry build that only one side happened to pay.
cat(sprintf("Warming caches (map root %s)\n", map_root(require_exists = FALSE)))
t0 <- Sys.time()
invisible(get_tile_vectors())
warm <- file.path(tempdir(), "warm.png")
invisible(try(render_elevation_tile(tiles$z[1], tiles$x[1], tiles$y[1], warm), silent = TRUE))
cat(sprintf("  %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

gf <- here::here("tests/golden/noise-fingerprint.rds")
if (file.exists(gf)) invisible(file.copy(gf, file.path(out, "noise-fingerprint.rds"), overwrite = TRUE))

cat(sprintf("Rendering %d reference tiles\n", nrow(tiles)))
res <- vector("list", nrow(tiles))
for (i in seq_len(nrow(tiles))) {
  t <- tiles[i, ]
  p <- file.path(out, "tiles", paste0(t$id, ".png"))
  # Render twice and time the second. Whichever side runs first pays to pull
  # the rasters off disk and the other rides the OS cache; timing a cold pass
  # made the base look ~2x slower than an identical head, which would hide a
  # real 2x slowdown entirely.
  try(render_elevation_tile(t$z, t$x, t$y, p), silent = TRUE)
  t0 <- Sys.time()
  err <- tryCatch({ render_elevation_tile(t$z, t$x, t$y, p); "" },
                  error = function(e) conditionMessage(e))
  sec <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  status <- if (nzchar(err)) "error" else if (!file.exists(p)) "no file" else "ok"
  res[[i]] <- data.frame(id = t$id, status = status, seconds = round(sec, 3),
                         error = err, stringsAsFactors = FALSE)
  cat(sprintf("  %-5s %-28s %5.1f s%s\n", status, t$id, sec,
              if (nzchar(err)) paste0("  ", substr(err, 1, 80)) else ""))
}
man <- do.call(rbind, res)
utils::write.csv(man, file.path(out, "manifest.csv"), row.names = FALSE)
cat(sprintf("%d ok, %d failed -> %s\n", sum(man$status == "ok"), sum(man$status != "ok"), out))
