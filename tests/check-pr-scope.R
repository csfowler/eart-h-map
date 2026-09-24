# =============================================================================
# check-pr-scope.R — what a pull request touches, judged from its diff
#
#     Rscript tests/check-pr-scope.R <base-ref>        (e.g. origin/main)
#
# The conformance suite judges the code as it now IS. This judges what the
# change DID, which the suite cannot see: that canon files were rewritten, that
# someone else's drawn feature was deleted, that the golden fingerprint moved
# without a word of explanation, that new code shells out or deletes files.
#
# Two kinds of finding:
#   FAIL   the PR cannot merge as it stands. Exit status 1.
#   REVIEW something the maintainer should look at before merging. Exit 0.
#
# The PR description is read from the PR_BODY environment variable (CI sets
# it). The report is printed, and appended to $GITHUB_STEP_SUMMARY when set.
# =============================================================================

suppressMessages(library(here))
`%||%` <- function(x, y) if (is.null(x)) y else x
setwd(here::here())
a <- commandArgs(trailingOnly = TRUE)
base <- if (length(a)) a[1] else "origin/main"
body <- Sys.getenv("PR_BODY", "")

git <- function(...) suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE))
st  <- git("diff", "--name-status", "--no-renames", paste0(base, "...HEAD"))
if (!length(st)) { cat("No changes against", base, "\n"); quit(status = 0L) }
ch <- data.frame(status = substr(st, 1, 1), path = sub("^[A-Z]\t", "", st), stringsAsFactors = FALSE)

FAIL <- character(0); REVIEW <- character(0)
fail   <- function(...) FAIL   <<- c(FAIL,   sprintf(...))
review <- function(...) REVIEW <<- c(REVIEW, sprintf(...))
under  <- function(dir) startsWith(ch$path, dir)

# --- canon ------------------------------------------------------------------
# Annotations are text and merge; everything else in Input Data/ is canon the
# pipeline produced, mostly binary, and rewritten by the editor's Save & push.
canon <- ch$path[under("Input Data/") & !under("Input Data/Annotations/") &
                 !under("Input Data/Inkarnate Maps/")]
if (length(canon))
  fail("Canon files changed (send only Input Data/Annotations/ and new images; the maintainer re-bakes canon): %s",
       paste(canon, collapse = ", "))
imgs <- ch[under("Input Data/Inkarnate Maps/") & ch$status != "A", ]
if (nrow(imgs))
  fail("Existing Inkarnate maps modified or deleted: %s", paste(imgs$path, collapse = ", "))

# --- tests and CI ------------------------------------------------------------
if (any(under("tests/golden/"))) {
  if (!grepl("fingerprint|golden", body, ignore.case = TRUE))
    fail("tests/golden/ changed but the PR description never mentions the fingerprint - say which fields moved and why")
  else review("The golden fingerprint was re-blessed: the world is meant to look different. Check the report.")
}
gate <- ch$path[under(".github/") | ch$path %in% c("tests/run-tests.R", "tests/helper.R",
                                                    "tests/check-pr-scope.R", "tests/compare-renders.R")]
if (length(gate))
  review("Changes the checks themselves (a green result proves less): %s", paste(gate, collapse = ", "))
tst <- ch$path[under("tests/test-") & ch$status %in% c("M", "D")]
if (length(tst)) review("Existing tests edited or removed: %s", paste(tst, collapse = ", "))

# --- the noise registry ------------------------------------------------------
if ("Functions/NoiseFields.R" %in% ch$path) {
  fields <- function(src) {
    e <- new.env()
    tryCatch({ eval(parse(text = src), envir = e); names(e$NOISE_FIELDS) }, error = function(err) NULL)
  }
  old <- fields(git("show", shQuote(paste0(base, ":Functions/NoiseFields.R"))))
  new <- fields(readLines("Functions/NoiseFields.R", warn = FALSE))
  if (is.null(new)) fail("Functions/NoiseFields.R no longer loads")
  gone <- setdiff(old, new)
  if (length(gone)) fail("Noise fields removed from the registry: %s", paste(gone, collapse = ", "))
  added <- setdiff(new, old)
  if (length(added)) review("New noise fields registered: %s", paste(added, collapse = ", "))
}

# --- risky code --------------------------------------------------------------
# Only ADDED lines, only R. None of these is wrong in itself; each is something
# a maintainer should read before running the branch on their own machine.
risky <- c("system2?\\(", "shell(\\.exec)?\\(", "download\\.file", "\\burl\\(", "unlink\\(",
           "file\\.remove", "Sys\\.setenv", "install\\.packages", "eval\\(parse", "socketConnection",
           "httr2?::", "curl::", "source\\(\\s*[\"']https?:")
diff <- git("diff", "-U0", paste0(base, "...HEAD"), "--", "*.R")
file <- ""; hits <- character(0)
for (l in diff) {
  if (startsWith(l, "+++ ")) { file <- sub("^\\+\\+\\+ b/", "", l); next }
  if (!startsWith(l, "+") || startsWith(l, "+++")) next
  code <- sub("#.*$", "", substring(l, 2))
  hit <- risky[vapply(risky, function(p) grepl(p, code, perl = TRUE), NA)]
  if (length(hit)) hits <- c(hits, sprintf("%s: `%s`", file, trimws(substr(code, 1, 90))))
}
if (length(hits)) review("New code that runs commands, deletes files, touches the network or the environment:\n%s",
                         paste0("  - ", hits, collapse = "\n"))

# --- annotations --------------------------------------------------------------
cfp <- "Input Data/Annotations/custom_features.geojson"
if (cfp %in% ch$path && file.exists(cfp)) {
  rd <- function(txt) tryCatch(jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)$features,
                               error = function(e) list())
  old <- rd(git("show", shQuote(paste0(base, ":", cfp))))
  new <- rd(readLines(cfp, warn = FALSE))
  key <- function(fs) vapply(fs, function(f) as.character(f$properties$feature_id %||% NA), "")
  ko <- key(old); kn <- key(new)
  gone <- setdiff(ko, kn)
  same <- intersect(ko, kn)
  edited <- same[vapply(same, function(k) !identical(old[[match(k, ko)]], new[[match(k, kn)]]), NA)]
  label <- function(fs, ks, keys) vapply(ks, function(k) {
    p <- fs[[match(k, keys)]]$properties
    sprintf("#%s %s '%s'", k, p$feature_type %||% "?", p$name %||% "")
  }, "")
  if (length(gone))   review("Existing drawn features deleted: %s", paste(label(old, gone, ko), collapse = "; "))
  if (length(edited)) review("Existing drawn features edited: %s",  paste(label(new, edited, kn), collapse = "; "))
  added <- setdiff(kn, ko)
  if (length(added))  review("Drawn features added: %s", paste(label(new, added, kn), collapse = "; "))
  hid <- kn[vapply(new, function(f) isTRUE(f$properties$hidden), NA)]
  if (length(hid)) fail("Features marked hidden in a public repository (hiding protects nothing here): %s",
                        paste(label(new, hid, kn), collapse = "; "))
}

# --- report --------------------------------------------------------------------
verdict <- if (length(FAIL)) "FAIL" else if (length(REVIEW)) "REVIEW" else "OK"
md <- c(sprintf("## Scope check: %s", verdict), "",
        sprintf("%d file(s) changed against `%s`.", nrow(ch), base), "",
        if (length(FAIL))   c("**Must fix:**", paste0("- ", FAIL), ""),
        if (length(REVIEW)) c("**For the maintainer:**", paste0("- ", REVIEW), ""),
        if (verdict == "OK") "Nothing outside the expected scope.")
cat(md, sep = "\n")
sf <- Sys.getenv("GITHUB_STEP_SUMMARY", "")
if (nzchar(sf)) cat(md, "", sep = "\n", file = sf, append = TRUE)
quit(status = if (length(FAIL)) 1L else 0L)
