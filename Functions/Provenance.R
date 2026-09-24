# =============================================================================
# Provenance.R — caches that know what they were built from
#
# WHY THIS REPLACES THE VERSION CONSTANTS
#
# Every expensive artefact here is a cache of a pure function: meandered road
# geometry, shaped rivers, settlement footprints, a rendered tile. Each used to
# be guarded by a hand-maintained integer -- ROAD_SHAPE_VERSION, SETT_CACHE_-
# VERSION -- that a developer bumped when they changed the algorithm.
#
# That has three failure modes, and on a project with several forks being merged
# together all three are routine:
#
#   1. YOU FORGET. The commonest bug in this codebase by a distance: change a
#      shaping function, see no difference, spend an hour before realising the
#      cache was loaded instead. Nothing warns you.
#
#   2. IT MERGES WRONG. Two forks each bump 4 -> 5. Git merges to 5 with no
#      conflict. One of them now has a cache built by the OTHER algorithm,
#      carrying a version number that says it is current. There is no way to
#      detect this after the fact.
#
#   3. IT CANNOT EXPRESS A CROSS-STAGE DEPENDENCY. Roads are shaped by pulling
#      their vertices down the gradient of the TERRAIN field. Change the terrain
#      and the road cache is stale -- but ROAD_SHAPE_VERSION lives in the road
#      code, and the person editing hydrology has no reason to touch it. This is
#      exactly the conflict that motivated this file: one student deepens the
#      drainage ladder, another has baked a layer against the terrain they saw,
#      and both changes merge cleanly into a map where the roads route around
#      hills that no longer exist.
#
# A content hash has none of them. The cache records WHAT IT WAS BUILT FROM --
# input files, the deparsed source of every function involved, the noise fields
# consumed, the constants that matter. If any of that differs, the cache misses
# and rebuilds. No discipline required, and a terrain change invalidates the
# road cache automatically because the dependency is declared rather than
# remembered.
#
# WHAT A STAMP LOOKS LIKE
#
#   stamp <- prov_stamp(list(
#     source  = prov_file("Input Data/Roads/road_routes.rds"),
#     code    = prov_fn("shape_roads", "meander_lines"),
#     fields  = prov_field("terrain"),
#     consts  = prov_const(WORLD_SEED = WORLD_SEED)))
#
# It is a NAMED list of component digests plus a combined one. Keeping the
# components is what lets a miss say WHICH dependency moved instead of just
# "stale" -- see prov_why(). A cache that cannot explain itself trains people to
# delete it blindly, which is how the old tile pyramid was managed.
#
# WHAT IS DELIBERATELY NOT HASHED
#
# Comments and whitespace. prov_fn() hashes deparse(), which normalises
# formatting and drops comments, so documenting a function does not throw away
# three minutes of river meandering. Semantics are hashed; prose is not.
# =============================================================================

# -----------------------------------------------------------------------------
# Digest
# -----------------------------------------------------------------------------

#' Digest a CHARACTER vector.
#'
#' Everything is reduced to character before hashing, never serialized directly:
#' serialize() of a closure drags in its environment (and therefore the whole
#' session), and serialize() of anything embeds an R version. Both make the hash
#' vary for reasons that have nothing to do with the world.
.prov_digest <- function(chr) {
  chr <- as.character(chr)
  if (requireNamespace("digest", quietly = TRUE))
    return(digest::digest(chr, algo = "xxhash64"))
  # Base-R fallback: no new dependency on a student's first afternoon.
  tf <- tempfile(fileext = ".txt"); on.exit(unlink(tf), add = TRUE)
  writeLines(chr, tf, useBytes = TRUE)
  unname(tools::md5sum(tf))
}

# -----------------------------------------------------------------------------
# Dependency descriptors
# -----------------------------------------------------------------------------

#' A data file's identity: existence, size, mtime.
#'
#' Not a content hash -- road_routes.rds is 1.1 MB and settlements_final.rds is
#' read on every start; hashing contents on each call would cost more than the
#' cache saves. Size+mtime misses only an edit that preserves both, which does
#' not happen to generated data. Pass content = TRUE where it matters.
prov_file <- function(..., content = FALSE) {
  paths <- c(...)
  out <- character(0)
  for (p in paths) {
    full <- if (file.exists(p)) p else here::here(p)
    if (!file.exists(full)) { out <- c(out, paste0(basename(p), ":absent")); next }
    if (content) {
      out <- c(out, paste0(basename(p), ":", unname(tools::md5sum(full))))
    } else {
      fi <- file.info(full)
      out <- c(out, sprintf("%s:%.0f:%s", basename(p), fi$size,
                            format(fi$mtime, "%Y%m%d%H%M%S")))
    }
  }
  .prov_digest(out)
}

#' The SEMANTICS of one or more functions.
#'
#' deparse() normalises whitespace and drops comments, so this changes when the
#' code changes and not when the documentation does. A function that is absent
#' is recorded as such rather than erroring, so a stamp can be computed before
#' everything is sourced.
prov_fn <- function(...) {
  names_ <- c(...)
  out <- character(0)
  for (n in names_) {
    f <- tryCatch(get(n, mode = "function"), error = function(e) NULL)
    out <- c(out, if (is.null(f)) paste0(n, ":absent")
                  else paste0(n, ":", paste(deparse(f), collapse = "\n")))
  }
  .prov_digest(out)
}

#' The SEMANTICS of an entire source file.
#'
#' WHY THIS EXISTS, given prov_fn() already hashes functions.
#'
#' prov_fn() takes a LIST OF NAMES, and that list is maintained by hand. It has
#' the same failure mode as the hand-bumped version integers it replaced: you
#' forget. Before this, .prov_render_stamp() named 31 functions while
#' TileServer.R defined 73, and the gap was not harmless -- editing
#' shape_roads() (the first student project in GETTING-STARTED) rebuilt the road
#' GEOMETRY but moved nothing in the render stamp, so every cached tile kept
#' drawing the old roads and the change appeared to do nothing. Same for
#' .hash01(), which decides every building's size and orientation.
#'
#' Hashing the file removes the list. A subsystem file is the unit a person
#' edits, so "this file changed" is exactly the question the cache needs
#' answered, and a newly added function is covered the moment it is written.
#'
#' parse() drops comments and normalises whitespace, so documenting a file does
#' not invalidate hours of rendering -- the same property prov_fn() has via
#' deparse(). A file that cannot be parsed is recorded as such rather than
#' erroring, so a stamp can still be computed while something is half-edited.
prov_rfile <- function(...) {
  paths <- c(...)
  out <- character(0)
  for (p in paths) {
    full <- if (file.exists(p)) p else here::here(p)
    if (!file.exists(full)) { out <- c(out, paste0(basename(p), ":absent")); next }
    ex <- tryCatch(parse(full, keep.source = FALSE), error = function(e) NULL)
    out <- c(out, if (is.null(ex)) paste0(basename(p), ":unparseable")
                  else paste0(basename(p), ":",
                              paste(vapply(ex, function(e)
                                paste(deparse(e), collapse = "\n"), character(1)),
                                collapse = "\n")))
  }
  .prov_digest(out)
}

#' Entries from the noise-field registry.
#'
#' THE cross-stage dependency. A cache that lists `prov_field("terrain")` is
#' automatically invalidated when someone changes the terrain wavelength, seed
#' or octave depth -- including someone who has never opened the file this cache
#' lives in.
prov_field <- function(...) {
  names_ <- c(...)
  if (!exists("NOISE_FIELDS")) return(.prov_digest("NOISE_FIELDS:absent"))
  out <- character(0)
  for (n in names_) {
    f <- NOISE_FIELDS[[n]]
    out <- c(out, if (is.null(f)) paste0(n, ":absent")
                  else paste0(n, ":", paste(deparse(f[setdiff(names(f),
                                                     c("note", "used_by"))]),
                                            collapse = "")))
  }
  .prov_digest(out)
}

#' Named constants that change output.
prov_const <- function(...) {
  v <- list(...)
  .prov_digest(paste(names(v), vapply(v, function(x)
    paste(format(x, digits = 15), collapse = ","), character(1)), sep = "="))
}

# -----------------------------------------------------------------------------
# Stamps
# -----------------------------------------------------------------------------

#' Combine dependency digests into a stamp.
#'
#' Returns the named components AND a combined hash. The components are the
#' point: they are what prov_why() diffs to name the dependency that moved.
prov_stamp <- function(deps) {
  stopifnot(is.list(deps), !is.null(names(deps)), all(nzchar(names(deps))))
  deps <- deps[order(names(deps))]
  structure(list(components = deps,
                 combined = .prov_digest(paste(names(deps), unlist(deps), sep = "="))),
            class = "prov_stamp")
}

#' Which components of two stamps differ. The explanation behind every miss.
prov_why <- function(old, new) {
  if (is.null(old)) return("no stamp recorded (cache predates provenance tracking)")
  if (!inherits(old, "prov_stamp")) return("unrecognised stamp format")
  o <- old$components; n <- new$components
  gone  <- setdiff(names(o), names(n))
  added <- setdiff(names(n), names(o))
  both  <- intersect(names(o), names(n))
  moved <- both[vapply(both, function(k) !identical(o[[k]], n[[k]]), logical(1))]
  msg <- c(if (length(moved)) paste0(paste(moved, collapse = ", "), " changed"),
           if (length(added)) paste0(paste(added, collapse = ", "), " added"),
           if (length(gone))  paste0(paste(gone, collapse = ", "), " no longer tracked"))
  if (!length(msg)) "components match but the combined hash does not (report this)"
  else paste(msg, collapse = "; ")
}

`print.prov_stamp` <- function(x, ...) {
  cat("<prov_stamp ", substr(x$combined, 1, 12), ">\n", sep = "")
  for (k in names(x$components))
    cat(sprintf("  %-10s %s\n", k, substr(x$components[[k]], 1, 12)))
  invisible(x)
}

# -----------------------------------------------------------------------------
# Cache I/O
# -----------------------------------------------------------------------------

#' Read a cache, but only if it was built from the same things.
#'
#' Returns list(obj, hit, why). A miss is never an error: `obj` is NULL and
#' `why` says what moved, for the caller to report before rebuilding.
cache_load <- function(path, stamp, force = FALSE, verbose = TRUE) {
  miss <- function(why) list(obj = NULL, hit = FALSE, why = why)
  if (force) return(miss("force = TRUE"))
  if (!file.exists(path)) return(miss("no cache file"))

  raw <- try(readRDS(path), silent = TRUE)
  if (inherits(raw, "try-error")) return(miss("cache file unreadable"))
  if (!is.list(raw) || is.null(raw$.prov))
    return(miss("no stamp recorded (cache predates provenance tracking)"))

  if (!identical(raw$.prov$combined, stamp$combined)) {
    why <- prov_why(raw$.prov, stamp)
    if (verbose) message(sprintf("  cache miss [%s]: %s", basename(path), why))
    return(miss(why))
  }
  list(obj = raw$obj, hit = TRUE, why = NA_character_)
}

#' Write a cache with its stamp. Failure to write is a warning, never fatal:
#' a read-only or full disk should slow the map down, not stop it.
cache_save <- function(path, obj, stamp) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(path, ".partial")
  ok <- try({
    saveRDS(list(obj = obj, .prov = stamp, .written = Sys.time()), tmp)
    if (!file.rename(tmp, path)) { file.copy(tmp, path, overwrite = TRUE); unlink(tmp) }
  }, silent = TRUE)
  if (inherits(ok, "try-error")) {
    unlink(tmp)
    warning("could not write cache: ", path, call. = FALSE)
    return(invisible(FALSE))
  }
  invisible(TRUE)
}

#' The stamp a cache file was built with, or NULL. For diagnostics.
cache_stamp_of <- function(path) {
  if (!file.exists(path)) return(NULL)
  raw <- try(readRDS(path), silent = TRUE)
  if (inherits(raw, "try-error") || !is.list(raw)) return(NULL)
  raw$.prov
}
