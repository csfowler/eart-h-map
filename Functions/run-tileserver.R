# =============================================================================
# run-tileserver.R — launch the procedural tile server from a shell.
#
#     Rscript Functions/run-tileserver.R [port] [procedural_max_zoom]
#
# What start-tileserver.bat / .sh run. It finds the repository from its own
# path and sets the working directory itself, so it works from any cwd. A
# non-default package library belongs in R_LIBS_USER (.Renviron), not here.
# =============================================================================

# --- locate the repository from this script's own path -----------------------
# Rscript passes --file=<path>; R CMD BATCH and source() do not, hence the
# fallback to the working directory. Functions/ is one level below the root.
.script_path <- function() {
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grepl("^--file=", a)])
  if (length(f)) normalizePath(f[1], winslash = "/", mustWork = FALSE) else NA_character_
}

.sp <- .script_path()
repo <- if (!is.na(.sp)) dirname(dirname(.sp)) else getwd()

if (!file.exists(file.path(repo, "Functions", "TileServer.R")))
  stop("Cannot find Functions/TileServer.R from ", repo,
       ".\n  Run this as: Rscript Functions/run-tileserver.R [port] [max_zoom]",
       call. = FALSE)

setwd(repo)

# --- arguments ---------------------------------------------------------------
# Validated rather than coerced. as.integer("87 65") is NA, and an NA port
# surfaces from deep inside httpuv as something that does not mention the port
# at all -- a bad argument should say so here.
a <- commandArgs(trailingOnly = TRUE)

.int_arg <- function(v, what, lo, hi) {
  n <- suppressWarnings(as.integer(v))
  if (is.na(n) || n < lo || n > hi)
    stop(what, " must be a whole number between ", lo, " and ", hi,
         "; got '", v, "'", call. = FALSE)
  n
}

port <- if (length(a) >= 1 && nzchar(a[1])) .int_arg(a[1], "port", 1L, 65535L) else 8765L

suppressMessages(source(file.path(repo, "Functions", "TileServer.R")))

# Raising the ceiling here is a RUNTIME override, and it is currently a blunt
# one: TILE_PROCEDURAL_MAX is inside .prov_render_stamp(), so changing it moves
# the render stamp and every cached procedural tile is treated as stale on its
# next request. The ceiling does not change how any existing tile was DRAWN, so
# that invalidation is over-broad -- but it is what the stamp does today, and a
# re-render of the whole pyramid is expensive enough to warn about rather than
# discover.
if (length(a) >= 2 && nzchar(a[2])) {
  TILE_PROCEDURAL_MAX <- .int_arg(a[2], "procedural_max_zoom", 9L, 20L)
  message("  procedural ceiling overridden to z", TILE_PROCEDURAL_MAX,
          " - this moves the render stamp, so cached procedural tiles will",
          " re-render on demand")
}

message("  repo: ", repo)
serve_tiles(port = port)
