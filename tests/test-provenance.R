# =============================================================================
# test-provenance.R — caches invalidate themselves
#
# The property under test is the one that lets two people work on this map at
# once: a cache records what it was built FROM, so a change anywhere in that
# dependency set invalidates it -- including a change made by someone who has
# never opened the file the cache lives in.
#
# The motivating case, stated concretely: student A deepens the drainage ladder
# to reach z15. Student B has baked a road layer against the terrain they could
# see at z14. Under the old integer versions those merge cleanly and B's roads
# now route around hills that no longer exist. Under provenance, A's change
# moves NOISE_FIELDS$drainage, which is inside the render stamp and (via
# `terrain`) inside the road stamp, so B's artefacts rebuild.
# =============================================================================

cat("\n[provenance]\n")

test_that("no engine file escapes the render stamp", {
  # TILE_DRAW_FILES is the one hand-kept list left, and it is the list that
  # matters: a new Functions/tiles/*.R that nobody adds to it would change the
  # map and re-render nothing -- the exact failure the file-level stamp was
  # introduced to end, just moved up a level. server.R is the single deliberate
  # exclusion, because HTTP routing is not appearance.
  on_disk <- list.files(here::here("Functions/tiles"), pattern = "\\.R$")
  known   <- c(basename(TILE_DRAW_FILES), "server.R")
  missing <- setdiff(on_disk, known)
  ok(length(missing) == 0,
     if (length(missing))
       paste0("engine file(s) in no stamp: ", paste(missing, collapse = ", "),
              "\n        Add them to TILE_DRAW_FILES in Functions/tiles/core.R.")
     else sprintf("all %d engine files are accounted for (%d hashed, server.R excluded)",
                  length(on_disk), length(TILE_DRAW_FILES)))

  stale <- setdiff(basename(TILE_DRAW_FILES), on_disk)
  ok(length(stale) == 0,
     if (length(stale)) paste("TILE_DRAW_FILES names files that do not exist:",
                              paste(stale, collapse = ", "))
     else "every file TILE_DRAW_FILES names exists")
})

test_that("prov_rfile hashes meaning, not formatting", {
  # The property that lets a whole file be a dependency without documenting it
  # costing hours of re-rendering.
  d <- file.path(tempdir(), paste0("prov-rfile-", as.integer(runif(1, 1e6, 9e6))))
  dir.create(d, showWarnings = FALSE)
  f <- file.path(d, "x.R")
  on.exit(unlink(d, recursive = TRUE), add = TRUE)

  writeLines(c("f <- function(a) {", "  a + 1", "}"), f)
  base <- prov_rfile(f)

  writeLines(c("# an explanatory comment", "f <- function(a) {",
               "", "  a + 1    # and a trailing one", "}"), f)
  ok(identical(base, prov_rfile(f)),
     "comments and blank lines do not move a file's hash")

  writeLines(c("f <- function(a) {", "  a + 2", "}"), f)
  ok(!identical(base, prov_rfile(f)), "a changed constant does move it")

  writeLines(c("f <- function(a) {", "  a + 1", "}",
               "g <- function(b) b * 2"), f)
  ok(!identical(base, prov_rfile(f)), "an added function moves it")

  writeLines("f <- function(a) {", f)                     # truncated, unparseable
  ok(nzchar(prov_rfile(f)), "an unparseable file is recorded, not an error")
  ok(!identical(base, prov_rfile(f)), "...and does not read as unchanged")

  ok(nzchar(prov_rfile(file.path(d, "definitely-absent.R"))),
     "an absent file is recorded, not an error")
})

test_that("a first stamp adopts the pyramid instead of condemning it", {
  # The bootstrap case. A pyramid built before provenance tracking has no stamp,
  # and writing the first one with the current time makes every tile older than
  # it -- so simply starting the server re-renders the lot. On this map that is
  # ~24,000 tiles and hours of work, triggered by a feature landing rather than
  # by anything about the tiles.
  #
  # Run against a throwaway map root so the real pyramid is never touched.
  root <- file.path(tempdir(), paste0("stamp-test-", as.integer(runif(1, 1e6, 9e6))))
  dir.create(file.path(root, "tiles", "elevation", "12", "100"), recursive = TRUE)
  png <- file.path(root, "tiles", "elevation", "12", "100", "314.png")
  writeBin(as.raw(0:9), png)
  Sys.setFileTime(png, Sys.time() - 86400)         # drawn yesterday

  old_env <- Sys.getenv("EARTH_MAP_ROOT", NA_character_)
  old_memo <- .prov_stamp_env$mtime
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("EARTH_MAP_ROOT") else Sys.setenv(EARTH_MAP_ROOT = old_env)
    .prov_stamp_env$mtime <- old_memo
    unlink(root, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(EARTH_MAP_ROOT = root)
  .prov_stamp_env$mtime <- NULL
  f <- file.path(root, "tiles", ".render-stamp")
  ok(!file.exists(f), "throwaway pyramid starts with no render stamp")

  suppressMessages(sync_render_stamp(force = TRUE))
  ok(file.exists(f), "first sync writes a stamp")
  ok(!tile_is_stale(png),
     sprintf("a tile drawn before the first stamp is NOT condemned (tile %s, stamp %s)",
             format(file.info(png)$mtime, "%Y-%m-%d"),
             format(file.info(f)$mtime, "%Y-%m-%d")))

  # ...and the escape hatch still condemns it on request.
  suppressMessages(invalidate_tile_cache())
  ok(tile_is_stale(png), "invalidate_tile_cache() condemns it deliberately")
})

test_that("adoption applies ONLY to the first stamp", {
  # The whole point is that normal invalidation is untouched. Once a stamp
  # exists, a moved hash must condemn the tiles that predate it -- otherwise the
  # fix for the bootstrap would quietly disable the feature.
  root <- file.path(tempdir(), paste0("stamp-test2-", as.integer(runif(1, 1e6, 9e6))))
  dir.create(file.path(root, "tiles", "elevation", "12", "100"), recursive = TRUE)
  png <- file.path(root, "tiles", "elevation", "12", "100", "314.png")
  writeBin(as.raw(0:9), png)

  old_env <- Sys.getenv("EARTH_MAP_ROOT", NA_character_)
  old_memo <- .prov_stamp_env$mtime
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("EARTH_MAP_ROOT") else Sys.setenv(EARTH_MAP_ROOT = old_env)
    .prov_stamp_env$mtime <- old_memo
    unlink(root, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(EARTH_MAP_ROOT = root)
  f <- file.path(root, "tiles", ".render-stamp")

  # A stamp from some OTHER version of the code already on disk.
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  saveRDS(prov_stamp(list(code = prov_const(v = "an older engine"))), f)
  Sys.setFileTime(f, Sys.time() - 86400)
  Sys.setFileTime(png, Sys.time() - 43200)          # tile newer than that stamp

  .prov_stamp_env$mtime <- NULL
  suppressMessages(sync_render_stamp(force = TRUE))
  ok(tile_is_stale(png),
     "a moved hash still condemns tiles drawn under the previous stamp")
})

test_that("an unreadable stamp is not mistaken for a fresh pyramid", {
  # file.exists() is true but readRDS() fails. That is damage, not a bootstrap:
  # there is no evidence the tiles are current, so it must NOT adopt.
  root <- file.path(tempdir(), paste0("stamp-test3-", as.integer(runif(1, 1e6, 9e6))))
  dir.create(file.path(root, "tiles", "elevation", "12", "100"), recursive = TRUE)
  png <- file.path(root, "tiles", "elevation", "12", "100", "314.png")
  writeBin(as.raw(0:9), png)
  Sys.setFileTime(png, Sys.time() - 86400)

  old_env <- Sys.getenv("EARTH_MAP_ROOT", NA_character_)
  old_memo <- .prov_stamp_env$mtime
  on.exit({
    if (is.na(old_env)) Sys.unsetenv("EARTH_MAP_ROOT") else Sys.setenv(EARTH_MAP_ROOT = old_env)
    .prov_stamp_env$mtime <- old_memo
    unlink(root, recursive = TRUE)
  }, add = TRUE)

  Sys.setenv(EARTH_MAP_ROOT = root)
  f <- file.path(root, "tiles", ".render-stamp")
  dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
  writeLines("not an rds file", f)

  .prov_stamp_env$mtime <- NULL
  suppressMessages(sync_render_stamp(force = TRUE))
  ok(tile_is_stale(png), "a corrupt stamp condemns rather than adopts")
})

test_that("descriptors are stable and discriminating", {
  a <- prov_const(x = 1, y = "two")
  b <- prov_const(x = 1, y = "two")
  c <- prov_const(x = 1, y = "three")
  ok(identical(a, b), "prov_const is stable across calls")
  ok(!identical(a, c), "prov_const distinguishes different values")

  f1 <- prov_fn("fbm_world")
  f2 <- prov_fn("fbm_world")
  ok(identical(f1, f2), "prov_fn is stable across calls")
  ok(!identical(f1, prov_fn("fbm_world", "add_microrelief")),
     "prov_fn distinguishes different function sets")
  ok(nzchar(prov_fn("definitely_not_a_function_here")),
     "prov_fn records an absent function instead of erroring")
})

test_that("a stamp changes when any one dependency changes", {
  base <- prov_stamp(list(a = prov_const(v = 1), b = prov_const(w = 2)))
  same <- prov_stamp(list(a = prov_const(v = 1), b = prov_const(w = 2)))
  diff <- prov_stamp(list(a = prov_const(v = 1), b = prov_const(w = 99)))

  ok(identical(base$combined, same$combined), "identical dependencies give one hash")
  ok(!identical(base$combined, diff$combined), "a changed component changes the hash")
  ok(grepl("^b changed", prov_why(base, diff)),
     sprintf("prov_why names the component that moved: '%s'", prov_why(base, diff)))
})

test_that("component order does not affect the hash", {
  # Otherwise two people listing the same dependencies in a different order
  # would invalidate each other's caches for no reason.
  x <- prov_stamp(list(alpha = prov_const(v = 1), beta = prov_const(w = 2)))
  y <- prov_stamp(list(beta = prov_const(w = 2), alpha = prov_const(v = 1)))
  ok(identical(x$combined, y$combined), "stamp is order-independent")
})

test_that("a cache round-trips and misses when its stamp moves", {
  tf <- tempfile(fileext = ".rds"); on.exit(unlink(tf), add = TRUE)
  s1 <- prov_stamp(list(code = prov_const(v = 1)))
  s2 <- prov_stamp(list(code = prov_const(v = 2)))

  cache_save(tf, list(payload = "expensive"), s1)
  h1 <- cache_load(tf, s1, verbose = FALSE)
  ok(isTRUE(h1$hit) && identical(h1$obj$payload, "expensive"),
     "a matching stamp hits and returns the payload")

  h2 <- cache_load(tf, s2, verbose = FALSE)
  ok(!h2$hit && is.null(h2$obj), "a moved stamp misses")
  ok(grepl("changed", h2$why), sprintf("the miss explains itself: '%s'", h2$why))

  h3 <- cache_load(tf, s1, force = TRUE, verbose = FALSE)
  ok(!h3$hit, "force = TRUE misses regardless")
})

test_that("a cache with no stamp is treated as stale, not trusted", {
  # Every existing cache on disk predates this mechanism. Accepting them would
  # serve geometry built by unknown code; rejecting costs one rebuild.
  tf <- tempfile(fileext = ".rds"); on.exit(unlink(tf), add = TRUE)
  saveRDS(list(setts = "legacy", version = 2L), tf)      # the OLD format
  h <- cache_load(tf, prov_stamp(list(code = prov_const(v = 1))), verbose = FALSE)
  ok(!h$hit, "a pre-provenance cache file misses")
  ok(grepl("predates", h$why), "and says why")
})

test_that("a corrupt cache misses instead of erroring", {
  tf <- tempfile(fileext = ".rds"); on.exit(unlink(tf), add = TRUE)
  writeLines("this is not an rds file", tf)
  h <- cache_load(tf, prov_stamp(list(code = prov_const(v = 1))), verbose = FALSE)
  ok(!h$hit && grepl("unreadable", h$why), "an unreadable cache misses cleanly")
})

test_that("the terrain field reaches the road and river stamps", {
  # THE cross-stage dependency, and the whole reason this file exists. Roads
  # pull their vertices down the terrain gradient, so a terrain change must
  # invalidate road geometry -- even though the person making it is editing
  # hydrology and has no reason to think about roads.
  ok(exists(".prov_roads_stamp") && exists(".prov_rivers_stamp"),
     "road and river stamps are defined")

  before_r <- .prov_roads_stamp()
  before_v <- .prov_rivers_stamp()

  saved <- NOISE_FIELDS$terrain$wl
  NOISE_FIELDS$terrain$wl <<- saved * 1.1        # simulate a terrain edit
  after_r <- .prov_roads_stamp()
  after_v <- .prov_rivers_stamp()
  NOISE_FIELDS$terrain$wl <<- saved

  ok(!identical(before_r$combined, after_r$combined),
     "changing the terrain field invalidates the ROAD cache")
  ok(!identical(before_v$combined, after_v$combined),
     "changing the terrain field invalidates the RIVER cache")
  ok(identical(.prov_roads_stamp()$combined, before_r$combined),
     "and restoring it restores the stamp (no hidden state)")
})

test_that("a hydrology change invalidates rendered tiles", {
  # Student A's change, seen from student B's tiles.
  before <- .prov_render_stamp()
  saved <- NOISE_FIELDS$drainage$wl
  NOISE_FIELDS$drainage$wl <<- c(saved[1] * 0.9, saved[-1])
  after <- .prov_render_stamp()
  NOISE_FIELDS$drainage$wl <<- saved

  ok(!identical(before$combined, after$combined),
     "deepening/altering the drainage ladder invalidates the render stamp")
  ok(grepl("fields", prov_why(before, after)),
     sprintf("and the reason names the field set: '%s'", prov_why(before, after)))
})

test_that("settlements do NOT depend on terrain", {
  # Invalidating everything on every change would be safe and useless -- it
  # would train people to ignore rebuilds. Settlement footprints genuinely do
  # not read the terrain field, so a hydrology change must leave them alone.
  before <- .prov_setts_stamp()
  saved <- NOISE_FIELDS$terrain$wl
  NOISE_FIELDS$terrain$wl <<- saved * 1.1
  after <- .prov_setts_stamp()
  NOISE_FIELDS$terrain$wl <<- saved

  ok(identical(before$combined, after$combined),
     "a terrain change leaves the settlement cache valid")
})

test_that("the render stamp covers every registered field", {
  # A field nobody's stamp reads is a field whose change renders no tile stale.
  before <- .prov_render_stamp()
  moved <- character(0)
  for (nm in names(NOISE_FIELDS)) {
    saved <- NOISE_FIELDS[[nm]]$seed_off
    NOISE_FIELDS[[nm]]$seed_off <<- saved + 10000L
    if (!identical(.prov_render_stamp()$combined, before$combined))
      moved <- c(moved, nm)
    NOISE_FIELDS[[nm]]$seed_off <<- saved
  }
  ok(length(moved) == length(NOISE_FIELDS),
     sprintf("all %d registered fields are inside the render stamp%s",
             length(NOISE_FIELDS),
             if (length(moved) < length(NOISE_FIELDS))
               paste0(" (missing: ",
                      paste(setdiff(names(NOISE_FIELDS), moved), collapse = ", "), ")")
             else ""))
})

test_that("the old version constants are gone", {
  # Leaving one behind means two mechanisms guarding one cache, and the stale
  # one wins whenever someone remembers to bump it and nothing else moved.
  gone <- c("ROAD_SHAPE_VERSION", "RIVER_SHAPE_VERSION", "SETT_CACHE_VERSION")
  still <- gone[vapply(gone, exists, logical(1))]
  ok(length(still) == 0,
     if (length(still)) paste("still defined:", paste(still, collapse = ", "))
     else "hand-bumped cache version constants have been removed")

  src <- unlist(lapply(NOISE_SCAN_FILES, function(f) {
    p <- here::here(f)
    if (file.exists(p)) readLines(p, warn = FALSE) else character(0)
  }))
  src <- src[!grepl("^\\s*#", src)]
  hits <- grep(paste(gone, collapse = "|"), src, value = TRUE)
  ok(length(hits) == 0, "and are not referenced anywhere in the tile engine")
})
