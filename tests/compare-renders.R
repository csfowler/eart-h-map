# =============================================================================
# compare-renders.R — before/after report for a change to the map
#
#     Rscript tests/compare-renders.R <base_dir> <head_dir> <out_dir>
#             [--expect roads,settlements] [--perf-warn 1.5] [--perf-fail 2.0]
#
# Reads two render-reference.R runs and writes:
#   <out_dir>/report.html   self-contained: every changed tile before / after /
#                           difference, sorted by how much changed
#   <out_dir>/summary.md    the headline table (GitHub's job summary)
#
# Exits non-zero -- the part CI enforces -- when the change breaks a tile the
# base rendered, or makes rendering slower than --perf-fail (median ratio).
# Everything else is information for a human: tests cannot decide whether a
# change is BETTER, only show exactly what it changed.
#
# --expect lists what the change is meant to touch, in the tile tags of
# reference-tiles.R. A tile that changed but carries none of those tags is
# reported as COLLATERAL -- the single most useful line in the report. A roads
# change that moves a desert tile with no roads in it has a side effect.
# =============================================================================

suppressMessages(library(terra))
`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

a <- commandArgs(trailingOnly = TRUE)
if (length(a) < 3) stop("usage: compare-renders.R <base_dir> <head_dir> <out_dir> [--expect tags] [--perf-warn x] [--perf-fail x]")
base_dir <- a[1]; head_dir <- a[2]; out <- a[3]
opt <- function(flag, default) if (flag %in% a) a[which(a == flag) + 1L] else default
expect    <- strsplit(opt("--expect", ""), ",")[[1]]
expect    <- trimws(expect[nzchar(expect)])
perf_warn <- as.numeric(opt("--perf-warn", "1.5"))
perf_fail <- as.numeric(opt("--perf-fail", "2.0"))
CHANGED_LEVEL <- 6     # per-channel difference (0-255) below which a pixel is "the same"
CHANGED_TILE  <- 0.5   # % of pixels changed above which a tile counts as changed

dir.create(file.path(out, "diff"), recursive = TRUE, showWarnings = FALSE)
rd <- function(d, f) utils::read.csv(file.path(d, f), stringsAsFactors = FALSE)
tiles <- rd(base_dir, "tiles.csv")
mb <- rd(base_dir, "manifest.csv"); mh <- rd(head_dir, "manifest.csv")
m <- merge(tiles, merge(mb, mh, by = "id", suffixes = c("_base", "_head"), all = TRUE),
           by = "id", all.x = TRUE)

px <- function(p) {                       # 256x256x3 integer array, or NULL
  if (!file.exists(p)) return(NULL)
  v <- terra::values(terra::rast(p))
  v[, 1:3, drop = FALSE]
}

write_png <- function(rgb, path) {
  r <- terra::rast(nrows = 256, ncols = 256, nlyrs = 3, vals = rgb,
                   extent = terra::ext(0, 256, 0, 256), crs = "local")
  suppressWarnings(terra::writeRaster(r, path, datatype = "INT1U", overwrite = TRUE))
}

m$pct_changed <- NA_real_; m$mean_diff <- NA_real_
for (i in seq_len(nrow(m))) {
  b <- px(file.path(base_dir, "tiles", paste0(m$id[i], ".png")))
  h <- px(file.path(head_dir, "tiles", paste0(m$id[i], ".png")))
  if (is.null(b) || is.null(h) || nrow(b) != nrow(h)) next
  d <- apply(abs(h - b), 1, max)
  m$pct_changed[i] <- 100 * mean(d > CHANGED_LEVEL)
  m$mean_diff[i]   <- mean(d)
  if (m$pct_changed[i] > 0) {
    # Heatmap: the base dimmed to grey, changed pixels in red by magnitude.
    g <- round(0.35 * rowMeans(b))
    k <- pmin(1, d / 64)
    write_png(cbind(round(g + (255 - g) * k), round(g * (1 - k)), round(g * (1 - k))),
              file.path(out, "diff", paste0(m$id[i], ".png")))
  }
}

ok_b <- m$status_base %in% "ok"; ok_h <- m$status_head %in% "ok"
broken  <- m$id[ok_b & !ok_h]
fixed   <- m$id[!ok_b & ok_h]
changed <- ok_b & ok_h & m$pct_changed > CHANGED_TILE
m$ratio <- ifelse(ok_b & ok_h & m$seconds_base > 0, m$seconds_head / m$seconds_base, NA)
med_ratio <- stats::median(m$ratio, na.rm = TRUE)

everywhere <- any(expect %in% c("terrain", "shading", "all"))
m$collateral <- FALSE
if (length(expect) && !everywhere)
  m$collateral <- changed & !vapply(strsplit(m$tags, ","), function(t) any(t %in% expect), NA)

# Which noise fields moved, straight from the two golden fingerprints.
gb <- file.path(base_dir, "noise-fingerprint.rds"); gh <- file.path(head_dir, "noise-fingerprint.rds")
golden <- if (file.exists(gb) && file.exists(gh)) {
  ob <- readRDS(gb); oh <- readRDS(gh); sh <- intersect(names(ob), names(oh))
  list(moved = sh[!vapply(sh, function(n) identical(ob[[n]], oh[[n]]), NA)],
       added = setdiff(names(oh), names(ob)), removed = setdiff(names(ob), names(oh)))
}

verdict <- if (length(broken) || (is.finite(med_ratio) && med_ratio > perf_fail)) "FAIL" else
           if (any(m$collateral) || (is.finite(med_ratio) && med_ratio > perf_warn)) "REVIEW" else "OK"

# --- summary.md ---------------------------------------------------------------
fmt_ids <- function(x) if (length(x)) paste(sprintf("`%s`", x), collapse = ", ") else "none"
md <- c(
  sprintf("## Map change report: %s", verdict), "",
  "| | |", "|---|---|",
  sprintf("| Reference tiles | %d |", nrow(m)),
  sprintf("| Changed (> %.1f%% of pixels) | %d |", CHANGED_TILE, sum(changed, na.rm = TRUE)),
  sprintf("| Collateral (changed, not in `--expect`) | %s |",
          if (!length(expect)) "no `--expect` given" else if (everywhere) "n/a (expects change everywhere)"
          else fmt_ids(m$id[m$collateral %in% TRUE])),
  sprintf("| Broken by this change | %s |", fmt_ids(broken)),
  sprintf("| Fixed by this change | %s |", fmt_ids(fixed)),
  sprintf("| Median render time, head / base | %s |",
          if (is.finite(med_ratio)) sprintf("%.2fx (warn > %.1f, fail > %.1f)", med_ratio, perf_warn, perf_fail) else "n/a"),
  sprintf("| Noise fields moved | %s |",
          if (is.null(golden)) "fingerprint unavailable"
          else fmt_ids(c(golden$moved,
                         if (length(golden$added))   paste0("+", golden$added),
                         if (length(golden$removed)) paste0("-", golden$removed)))),
  "", "Download the `map-report` artifact and open `report.html` for every tile before and after.")
writeLines(md, file.path(out, "summary.md"))

# --- report.html -------------------------------------------------------------
img <- function(p) if (file.exists(p))
  sprintf('<img src="data:image/png;base64,%s" width="256" height="256">',
          jsonlite::base64_enc(readBin(p, "raw", file.info(p)$size))) else '<div class="none">no image</div>'
esc <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x))

ord <- order(-ifelse(is.na(m$pct_changed), 101, m$pct_changed))
rows <- character(0); quiet <- character(0)
for (i in ord) {
  r <- m[i, ]
  flag <- c(if (r$id %in% broken) "BROKEN", if (r$id %in% fixed) "fixed",
            if (isTRUE(r$collateral)) "COLLATERAL",
            if (isTRUE(r$ratio > perf_warn)) sprintf("slower %.1fx", r$ratio))
  show <- !is.na(r$pct_changed) && r$pct_changed > 0 || length(flag)
  line <- sprintf("%s: %s", esc(r$id),
                  if (is.na(r$pct_changed)) "not compared" else sprintf("%.2f%% changed", r$pct_changed))
  if (!show) { quiet <- c(quiet, esc(r$id)); next }
  rows <- c(rows, sprintf(paste0(
    '<section class="%s"><h3>%s %s</h3><p class="meta">tags: %s &middot; %s &middot; ',
    'render %.1f s &rarr; %.1f s%s</p><div class="row"><figure>%s<figcaption>base</figcaption></figure>',
    '<figure>%s<figcaption>this change</figcaption></figure><figure>%s<figcaption>difference</figcaption></figure></div></section>'),
    if (length(flag)) "flag" else "", esc(r$id), paste(sprintf('<span class="tag">%s</span>', flag), collapse = " "),
    esc(r$tags), if (is.na(r$pct_changed)) "not compared" else sprintf("%.2f%% of pixels changed, mean difference %.1f", r$pct_changed, r$mean_diff),
    r$seconds_base %||% NA, r$seconds_head %||% NA,
    if (nzchar(r$error_head %||% "")) paste0(" &middot; <b>error:</b> ", esc(r$error_head)) else "",
    img(file.path(base_dir, "tiles", paste0(r$id, ".png"))),
    img(file.path(head_dir, "tiles", paste0(r$id, ".png"))),
    img(file.path(out, "diff", paste0(r$id, ".png")))))
}
# The summary table again, as HTML: the markdown rows are "| label | value |".
rows_md <- grep("^\\| [^|-]", md, value = TRUE)
md_html <- paste0("<table>", paste(vapply(strsplit(rows_md, " \\| "), function(p) {
  p <- gsub("^\\| |\\s*\\|$", "", p); p <- gsub("`([^`]*)`", "<code>\\1</code>", esc(p))
  sprintf("<tr><th>%s</th><td>%s</td></tr>", p[1], p[2])
}, ""), collapse = ""), "</table>")
html <- c('<!doctype html><html><head><meta charset="utf-8"><title>Map change report</title><style>',
  ':root{--bg:#fbf8f2;--fg:#2b2622;--mut:#6f665c;--line:#e2d9cb;--flag:#b3261e}',
  '@media (prefers-color-scheme:dark){:root{--bg:#1d1b19;--fg:#ece6dc;--mut:#a79d90;--line:#3a3530;--flag:#f2b8b5}}',
  'body{background:var(--bg);color:var(--fg);font:15px/1.5 system-ui,sans-serif;margin:0 auto;max-width:880px;padding:16px}',
  'h1{font-size:1.4em}h3{margin:.2em 0}.meta{color:var(--mut);margin:.2em 0 .6em}',
  'section{border-top:1px solid var(--line);padding:12px 0}.row{display:flex;gap:8px;flex-wrap:wrap}',
  'figure{margin:0}figcaption{color:var(--mut);font-size:.85em;text-align:center}img{max-width:100%;height:auto;image-rendering:pixelated}',
  '.tag{background:var(--flag);color:var(--bg);border-radius:3px;padding:0 6px;font-size:.75em;vertical-align:middle}',
  'table{border-collapse:collapse;margin:8px 0 16px}th,td{text-align:left;padding:4px 12px 4px 0;border-bottom:1px solid var(--line);vertical-align:top}th{font-weight:600;color:var(--mut)}.quiet{color:var(--mut)}',
  '</style></head><body>',
  sprintf('<h1>Map change report: %s</h1>%s', verdict, md_html),
  if (length(rows)) rows else '<p>No reference tile changed.</p>',
  if (length(quiet)) sprintf('<p class="quiet">Unchanged: %s</p>', paste(quiet, collapse = ", ")),
  '</body></html>')
writeLines(html, file.path(out, "report.html"), useBytes = TRUE)

cat(md, sep = "\n")
cat(sprintf("\nReport: %s\n", normalizePath(file.path(out, "report.html"), winslash = "/")))
quit(status = if (verdict == "FAIL") 1L else 0L)
