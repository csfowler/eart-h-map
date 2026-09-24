# =============================================================================
# test-annotations.R — the hand-authored layer is well-formed
#
# Everything drawn in tools/annotation-editor.R lands in Input Data/Annotations/
# and is inlined into the map. A malformed feature does not error there: a
# point retyped as "road" or a link to an image nobody committed just renders
# wrong, or not at all. These checks run on whatever annotations the checkout
# holds, so they gate annotation pull requests the same way the rest of the
# suite gates code. (What a PR CHANGED -- edits to other people's features,
# hidden POIs in a public repo -- is tests/check-pr-scope.R's job.)
# =============================================================================

cat("\n[annotations]\n")

.ann <- here::here("Input Data/Annotations")
if (!dir.exists(.ann)) {
  skip("no Input Data/Annotations in this checkout")
} else {
  if (!exists("FEATURE_GEOMETRY")) suppressMessages(source(here::here("Functions/AnnotationBuilder.R")))
  IMAGE_MAX_MB <- 15
  .img_dir <- here::here("Input Data/Inkarnate Maps")
  .imgs    <- if (dir.exists(.img_dir)) list.files(.img_dir) else character(0)

  cf <- read_custom_features()

  test_that("custom_features.geojson is well-formed", {
    if (is.null(cf)) { skip("no drawn features"); return(invisible(NULL)) }
    ok(!anyNA(cf$feature_id) && !anyDuplicated(cf$feature_id),
       sprintf("%d features, every feature_id present and unique", nrow(cf)))
    bad <- setdiff(unique(cf$feature_type), FEATURE_TYPES)
    ok(!length(bad), sprintf("every feature_type is one the editor knows%s",
                             if (length(bad)) paste0(" - unknown: ", paste(bad, collapse = ", ")) else ""))
    geom <- sub("^MULTI", "", as.character(sf::st_geometry_type(cf)))
    want <- FEATURE_GEOMETRY[cf$feature_type]
    wrong <- which(!is.na(want) & geom != want)
    ok(!length(wrong), sprintf("every feature has its type's geometry%s",
       if (length(wrong)) paste0(" - mismatched: ", paste(sprintf("%s (#%s, %s but %s)",
         cf$name[wrong], cf$feature_id[wrong], cf$feature_type[wrong], geom[wrong]), collapse = "; ")) else ""))
    bb <- sf::st_bbox(cf)
    ok(bb["xmin"] >= -180 && bb["xmax"] <= 180 && bb["ymin"] >= -90 && bb["ymax"] <= 90,
       "all features lie within world bounds")
  })

  test_that("drawn roads and trails run over land", {
    wcf <- here::here("Input Data/HighResolution/water_class.vrt")
    lines <- if (!is.null(cf)) cf[cf$feature_type %in% c("road", "trail"), ]
    if (is.null(lines) || !nrow(lines) || !file.exists(wcf)) {
      skip("no drawn roads, or no water_class to check them against"); return(invisible(NULL))
    }
    wc <- terra::rast(wcf)
    # Points every ~0.005 degrees along the line, interpolated by hand: sf will
    # not sample lon/lat lines, and at this scale planar spacing is fine.
    along <- function(co) do.call(rbind, lapply(seq_len(nrow(co) - 1L), function(k) {
      n <- max(2L, ceiling(sqrt(sum((co[k + 1, ] - co[k, ])^2)) / 0.005))
      f <- seq(0, 1, length.out = n)
      cbind(co[k, 1] + f * (co[k + 1, 1] - co[k, 1]), co[k, 2] + f * (co[k + 1, 2] - co[k, 2]))
    }))
    for (i in seq_len(nrow(lines))) {
      co  <- sf::st_coordinates(sf::st_geometry(lines[i, ]))
      grp <- co[, ncol(co)]                         # one part per linestring
      pts <- do.call(rbind, lapply(split(seq_len(nrow(co)), grp), function(r) along(co[r, 1:2, drop = FALSE])))
      w <- terra::extract(wc, pts)[, 1]
      w <- w[!is.na(w)]
      if (!length(w)) next
      wet <- mean(w %in% c(WATER_CLASS$OCEAN, WATER_CLASS$LAKE))
      nm <- if (nzchar(lines$name[i] %||% "")) lines$name[i] else paste0("#", lines$feature_id[i])
      ok(wet < 0.25, sprintf("%s '%s' is %.0f%% over open water (max 25%%)",
                             lines$feature_type[i], nm, 100 * wet))
    }
  })

  test_that("every linked Inkarnate map is present and reasonably sized", {
    links <- c(if (!is.null(cf)) cf$inkarnate_path,
               tryCatch(utils::read.csv(SETTLEMENT_NAMES_PATH(), stringsAsFactors = FALSE)$inkarnate,
                        error = function(e) NULL))
    links <- unique(links[!is.na(links) & nzchar(links)])
    missing <- setdiff(links, .imgs)
    ok(!length(missing), sprintf("%d linked image(s), all in Input Data/Inkarnate Maps%s", length(links),
                                 if (length(missing)) paste0(" - missing: ", paste(missing, collapse = ", ")) else ""))
    mb <- file.info(file.path(.img_dir, .imgs))$size / 1024^2
    big <- .imgs[mb > IMAGE_MAX_MB]
    ok(!length(big), sprintf("no image over %d MB%s", IMAGE_MAX_MB,
                             if (length(big)) paste0(" - ", paste(big, collapse = ", ")) else ""))
  })

  test_that("settlement_names.csv refers to real settlements", {
    nm <- tryCatch(utils::read.csv(SETTLEMENT_NAMES_PATH(), stringsAsFactors = FALSE), error = function(e) NULL)
    sf_ <- here::here("Input Data/Combined/settlements_final.rds")
    if (is.null(nm) || !nrow(nm) || !file.exists(sf_)) { skip("no settlement overrides"); return(invisible(NULL)) }
    ids <- readRDS(sf_)$settlement_id
    ok(!anyDuplicated(nm$settlement_id), sprintf("%d override rows, one per settlement", nrow(nm)))
    ghost <- setdiff(nm$settlement_id, ids)
    ok(!length(ghost), sprintf("every settlement_id exists in canon%s",
                               if (length(ghost)) paste0(" - unknown: ", paste(ghost, collapse = ", ")) else ""))
  })
}
