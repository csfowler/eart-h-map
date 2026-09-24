# =============================================================================
# server.R - HTTP routes and the launcher
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# serve_tiles() and its two routes. Deliberately NOT part of the render stamp: a
# change to how tiles are served over HTTP does not change what they look like,
# and should not re-render a pyramid.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Server launcher
#
# The routes live HERE, in the repository, and the router is built in code.
# They used to be a separate annotated file, <map root>/tile-server/plumber.R,
# which plumber::pr() parsed. That file was the only piece of map code in no
# version control at all: Map/ is a generated artefact, gitignored and excluded
# from every backup, so the server's HTTP surface existed as thirty-four lines
# on one disk. Nothing could regenerate it, the repo export had to copy it out
# of the map to ship it, and it then landed at <repo>/tile-server/plumber.R --
# which is not where serve_tiles() looked, so a fresh clone could not serve
# tiles at all.
#
# Annotations bought nothing here. There are two routes, no OpenAPI docs are
# served (docs = FALSE below), and the handlers need TileServer's own functions,
# which the annotated file obtained by source()ing this file back in. Building
# the router with pr_get() removes the file, the copy, and the closure over a
# re-sourced environment in one go.
# -----------------------------------------------------------------------------

#' Liveness, and what this server will synthesize.
#'
#' Also the quickest way to confirm which WORLD_SEED a running server holds --
#' a server started before a seed change keeps serving the old world until it
#' is restarted, and nothing else would say so.
.tile_route_healthz <- function() {
  list(status         = "ok",
       native_max     = TILE_NATIVE_MAX,
       procedural_max = TILE_PROCEDURAL_MAX,
       world_seed     = WORLD_SEED)
}

#' One raster tile. z <= native is a static passthrough from the pyramid;
#' z > native is synthesized on demand and written through to the same path.
#'
#' `yfile` carries the ".png" extension exactly as Leaflet requests it, so the
#' route pattern matches the URLs already inlined in index.html.
.tile_route_tile <- function(layer, z, x, yfile, res) {
  y <- suppressWarnings(as.integer(sub("\\.png$", "", yfile)))
  z <- suppressWarnings(as.integer(z)); x <- suppressWarnings(as.integer(x))
  if (anyNA(c(z, x, y))) { res$status <- 400L; return(raw(0)) }

  p <- tryCatch(tile_path(layer, z, x, y),
                error = function(e) { message("tile error: ", conditionMessage(e)); NULL })
  if (is.null(p) || !file.exists(p)) { res$status <- 404L; return(raw(0)) }
  readBin(p, "raw", n = file.info(p)$size)
}

#' Launch the plumber tile server: serves the static Map/ assets (index.html,
#' data/, inkarnate/) AND answers /tiles/<layer>/<z>/<x>/<y>.png by static
#' passthrough (z <= native) or on-demand synthesis + write-through (z > native).
#'
#' One origin replaces the Python http.server, so the existing relative tile
#' URLs in index.html ("tiles/elevation/{z}/{x}/{y}.png") transparently start
#' hitting the procedural engine once the page is allowed to request z > 8.
#'
#' The static mount is added LAST and deliberately: plumber matches endpoints
#' before mounts, so /tiles/... reaches the synthesizer while everything else
#' falls through to the file served off disk. Mounting first would shadow the
#' procedural route with a 404 from the static handler for any tile above z8,
#' which is every tile this server exists to draw.
#'
#' @param port,host  Listen address. Defaults match the old Python launcher.
serve_tiles <- function(port = 8765, host = "127.0.0.1") {
  if (!requireNamespace("plumber", quietly = TRUE))
    stop("Package 'plumber' is required: install.packages('plumber')")
  map_dir <- map_root()

  pr <- plumber::pr()
  pr <- plumber::pr_get(pr, "/healthz", .tile_route_healthz,
                        serializer = plumber::serializer_unboxed_json())
  pr <- plumber::pr_get(pr, "/tiles/<layer>/<z>/<x>/<yfile>", .tile_route_tile,
                        serializer = plumber::serializer_content_type("image/png"))
  # Serve the rest of Map/ (index.html, data/, inkarnate/, static tiles) as files.
  pr <- plumber::pr_static(pr, "/", map_dir)

  message(sprintf("EART-H tile server on http://%s:%d  (procedural z%d-%d)",
                  host, port, max(unlist(TILE_NATIVE_MAX)) + 1L, TILE_PROCEDURAL_MAX))
  message("  open http://", host, ":", port, "/index.html")
  plumber::pr_run(pr, host = host, port = port, docs = FALSE)
}
