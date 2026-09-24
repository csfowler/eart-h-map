# =============================================================================
# MapRoot.R — where the Map/ artefact lives
#
# The map artefact (index.html, player/, data/, tiles/) is ~2.9 GB once the
# tile pyramid is built, so it may live outside the project -- the author keeps
# it at C:/Map, out of a synced folder. Every reader and writer therefore
# resolves it through map_root() / map_path(); never build
# `file.path(here::here(), "Map", ...)` by hand.
#
# Resolution order, first hit wins:
#
#   1. EARTH_MAP_ROOT env var    — what the launchers set. Explicit, so it wins,
#                                  and a bad value ERRORS rather than falling
#                                  through: if you took the trouble to set it,
#                                  silently using somewhere else is worse.
#   2. .map-root at project root — per-machine override. One line, the path.
#                                  Same "explicit, so it errors" rule.
#   3. here::here("Map")         — the old in-repo layout. Still correct on any
#                                  machine that never moved it (the secondary
#                                  laptop), which is why this is not deleted.
#   4. MAP_ROOT_DEFAULT          — this laptop's location, C:/Map.
#
# Steps 3 and 4 are implicit guesses, so they fall through quietly when absent.
# When none exists and the caller will create the map, it goes to step 3.
#
# Why require_exists defaults to TRUE: the tile server WRITES into the pyramid.
# A wrong root therefore does not error — it silently starts a second, empty
# tile cache and re-renders hours of canon into a directory nobody reads. The
# only cheap defence is to refuse to proceed when the root is not there, and to
# print every candidate that was tried. Pass require_exists = FALSE only where
# the caller genuinely intends to CREATE the map (build_reference_map).
# =============================================================================

MAP_ROOT_DEFAULT <- "C:/Map"

.map_norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

#' Resolve the root of the Map/ artefact. See the header for the search order.
#'
#' @param require_exists Stop if no candidate directory exists. Leave TRUE
#'   unless you are about to create the map.
#' @return Absolute path, forward slashes, no trailing separator.
map_root <- function(require_exists = TRUE) {
  # --- explicit sources: honoured absolutely, wrong value is an error --------
  env <- Sys.getenv("EARTH_MAP_ROOT", "")
  if (nzchar(env)) {
    if (dir.exists(env) || !require_exists) return(.map_norm(env))
    stop("EARTH_MAP_ROOT is set to a directory that does not exist:\n  ", env,
         "\nFix the launcher that set it, or unset it to fall back to the ",
         "search order in Functions/MapRoot.R.", call. = FALSE)
  }

  proj <- tryCatch(here::here(), error = function(e) NULL)

  if (!is.null(proj)) {
    cfg <- file.path(proj, ".map-root")
    if (file.exists(cfg)) {
      ln <- trimws(readLines(cfg, warn = FALSE))
      ln <- ln[nzchar(ln) & !startsWith(ln, "#")]
      if (length(ln)) {
        if (dir.exists(ln[1]) || !require_exists) return(.map_norm(ln[1]))
        stop(".map-root points at a directory that does not exist:\n  ", ln[1],
             "\n  (read from ", cfg, ")", call. = FALSE)
      }
    }
  }

  # --- implicit guesses: fall through quietly -------------------------------
  guesses <- c(if (!is.null(proj)) file.path(proj, "Map"), MAP_ROOT_DEFAULT)
  for (g in guesses) if (dir.exists(g)) return(.map_norm(g))

  # Nothing exists yet and the caller is about to create it: put it inside the
  # project. Falling back to C:/Map here sent a fresh clone's first build to a
  # directory outside the repo (and, off Windows, to a relative "C:/Map").
  if (!require_exists) return(.map_norm(guesses[1]))

  stop("Cannot locate the EART-H Map directory. Tried, in order:\n",
       paste0("  - ", c("$EARTH_MAP_ROOT  (not set)",
                        paste0(if (!is.null(proj)) file.path(proj, ".map-root")
                               else "<.map-root>", "  (absent)"),
                        guesses), collapse = "\n"),
       "\n\nEither set EARTH_MAP_ROOT, or write the path into a .map-root file ",
       "at the project root.", call. = FALSE)
}

#' Build a path inside the map root. The only correct way to name a map file.
#'
#'   map_path("tiles", "elevation")        -> <root>/tiles/elevation
#'   map_path("data/rivers_big.geojson")   -> <root>/data/rivers_big.geojson
map_path <- function(..., require_exists = TRUE) {
  file.path(map_root(require_exists = require_exists), ...)
}

#' TRUE when the map lives outside the project tree — i.e. the current layout.
#' Used only for messages that would otherwise print a misleading "Map/...".
map_is_external <- function() {
  proj <- tryCatch(here::here(), error = function(e) NULL)
  if (is.null(proj)) return(TRUE)
  !identical(map_root(require_exists = FALSE), .map_norm(file.path(proj, "Map")))
}
