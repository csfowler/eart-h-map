# =============================================================================
# AnnotationBuilder.R - Story-time overrides + custom features for EART-H
# =============================================================================
#
# Three classes of author-time annotation, all stored under
# Input Data/Annotations/:
#
#   1. settlement_names.csv      story names + Inkarnate links keyed by
#                                settlement_id (overrides NameBuilder procgen)
#   2. relocated_features.rds    manual lon/lat moves for settlements and
#                                sacred sites, written by the unified editor
#   3. custom_features.geojson   drawn rivers / forests / lakes / POIs
#
# Workflow:
#   - Author edits via resources/Annotation-editor.R (Shiny). All edits land
#     in the three files above; those files are the source of truth.
#   - chapters/10-languages-and-places.qmd calls apply_annotations_to_canon()
#     at the end of NameBuilder, baking (1) and (2) into the canonical files
#     (settlements_final.rds + sacred_sites_attributed.RData).
#   - MapBuilder::build_reference_map() reads canon directly. Custom features
#     (3) are rendered as a separate additive layer.
#
# Single rule: after Ch10 runs, settlements_final.rds and the sacred-sites
# RData are canon. Nothing downstream re-applies overrides.
# =============================================================================

library(sf)
library(dplyr)
library(jsonlite)

# --- Default paths --------------------------------------------------------

ANNOTATIONS_DIR       <- function() here::here("Input Data/Annotations")
SETTLEMENT_NAMES_PATH <- function() file.path(ANNOTATIONS_DIR(), "settlement_names.csv")
RELOCATIONS_PATH      <- function() file.path(ANNOTATIONS_DIR(), "relocated_features.rds")
CUSTOM_FEATURES_PATH  <- function() file.path(ANNOTATIONS_DIR(), "custom_features.geojson")
SETTLEMENTS_CANON     <- function() here::here("Input Data/Combined/settlements_final.rds")
SACRED_SITES_CANON    <- function() here::here("Input Data/Divinity/sacred_sites_attributed.RData")

# `settlement_id` is a HINT, not the key. Settlement IDs are assigned by
# `seq_len(nrow(...))` at the end of PopBuilder's assembly, so any rebuild of
# Ch8 renumbers every settlement and silently re-points every row in this file
# at a different town. `lon`/`lat` are the durable anchor: a pinned place is
# somewhere, and it is still somewhere after a rebuild. Resolution prefers the
# id, verifies it against the coordinates, and falls back to the nearest node.
#
# `phonology_seed` carries the other half of permanence. NameBuilder draws a
# community's phonology from `build_phonology(1000L + community_id)`, and
# Louvain renumbers communities on every rebuild -- so a pinned name survives
# while the names of every town around it change family, which reads worse
# than losing the pin. Recording the seed lets NameBuilder re-pin the whole
# community's phonology to the one the pinned name was drawn from.
SETTLEMENT_NAMES_SCHEMA <- c("settlement_id", "name", "population", "inkarnate",
                             "notes", "lon", "lat", "phonology_seed")

# =============================================================================
# SECTION 1 — settlement_names.csv (story-name overrides + inkarnate links)
# =============================================================================

#' Initialize settlement_names.csv with the canonical schema.
#' @export
init_settlement_names <- function(output_path = SETTLEMENT_NAMES_PATH(),
                                  verbose = TRUE) {
  dir.create(dirname(output_path), showWarnings = FALSE, recursive = TRUE)
  if (file.exists(output_path)) {
    if (verbose) cat("Settlement names file already exists:", output_path, "\n")
    return(invisible(output_path))
  }
  template <- data.frame(
    settlement_id  = integer(0),
    name           = character(0),
    population     = numeric(0),
    inkarnate      = character(0),
    notes          = character(0),
    lon            = numeric(0),
    lat            = numeric(0),
    phonology_seed = integer(0),
    stringsAsFactors = FALSE
  )
  write.csv(template, output_path, row.names = FALSE)
  if (verbose) cat("Wrote empty settlement_names.csv:", output_path, "\n")
  invisible(output_path)
}

#' Read settlement_names.csv, backfilling any missing schema columns.
#' Returns a data.frame with columns settlement_id, name, population,
#' inkarnate, notes. Files written before `population` existed load cleanly:
#' the column is backfilled as NA, which means "leave canon alone".
#' @export
load_settlement_names <- function(path = SETTLEMENT_NAMES_PATH()) {
  if (!file.exists(path)) return(NULL)
  df <- read.csv(path, stringsAsFactors = FALSE)
  for (col in SETTLEMENT_NAMES_SCHEMA) {
    if (!col %in% names(df)) {
      df[[col]] <- switch(col,
        settlement_id  = NA_integer_,
        population     = NA_real_,
        lon            = NA_real_,
        lat            = NA_real_,
        phonology_seed = NA_integer_,
        NA_character_)
    }
  }
  # Blank cells in a text-ish CSV column arrive as "" rather than NA; normalise
  # so downstream code only has to test is.na() for "no override".
  for (col in c("population", "lon", "lat", "phonology_seed")) {
    df[[col]] <- suppressWarnings(as.numeric(df[[col]]))
  }
  for (col in c("name", "inkarnate", "notes")) {
    df[[col]][is.na(df[[col]])] <- ""
  }
  df[, SETTLEMENT_NAMES_SCHEMA, drop = FALSE]
}

#' Set or update a single settlement's story-name override.
#' Writes the canonical schema; older, narrower files are upgraded on load.
#' `population = NA` (the default) leaves the canonical population untouched;
#' supply a number to override it, which is how story-scale settlements are
#' kept smaller than the pipeline's gravity-redistributed figure.
#' @export
set_settlement_name <- function(settlement_id,
                                name       = "",
                                population = NA_real_,
                                inkarnate  = "",
                                notes      = "",
                                lon        = NA_real_,
                                lat        = NA_real_,
                                phonology_seed = NA_integer_,
                                anchor     = TRUE,
                                path       = SETTLEMENT_NAMES_PATH(),
                                verbose    = TRUE) {
  if (!file.exists(path)) init_settlement_names(path, verbose = FALSE)
  df <- load_settlement_names(path)

  # Anchor the row to where the place actually is, so it survives an ID
  # reshuffle. Looked up from canon when not supplied, which is why a plain
  # set_settlement_name(817, "Paingokgo") is still enough.
  if (anchor && (is.na(lon) || is.na(lat) || is.na(phonology_seed))) {
    sc <- SETTLEMENTS_CANON()
    if (file.exists(sc)) {
      s <- readRDS(sc)
      idx <- which(as.integer(as.character(s$settlement_id)) == as.integer(settlement_id))
      if (length(idx) == 1) {
        if (is.na(lon)) lon <- s$lon[idx]
        if (is.na(lat)) lat <- s$lat[idx]
        if (is.na(phonology_seed) && "name_community" %in% names(s) &&
            !is.na(s$name_community[idx])) {
          phonology_seed <- 1000L + as.integer(s$name_community[idx])
        }
      } else if (verbose) {
        cat("  [warn] id", settlement_id, "not in canon; row written unanchored\n")
      }
    }
  }

  new_row <- data.frame(
    settlement_id  = as.integer(settlement_id),
    name           = name,
    population     = as.numeric(population),
    inkarnate      = inkarnate,
    notes          = notes,
    lon            = as.numeric(lon),
    lat            = as.numeric(lat),
    phonology_seed = as.integer(phonology_seed),
    stringsAsFactors = FALSE
  )
  idx <- which(df$settlement_id == settlement_id)
  if (length(idx) > 0) {
    df[idx, SETTLEMENT_NAMES_SCHEMA] <- new_row[, SETTLEMENT_NAMES_SCHEMA]
    if (verbose) cat("Updated:", name, "(#", settlement_id, ")\n")
  } else {
    df <- rbind(df, new_row)
    if (verbose) cat("Added:", name, "(#", settlement_id, ")\n")
  }
  write.csv(df, path, row.names = FALSE)
  invisible(df)
}

#' Remove a settlement's name override row. No-op if absent.
#' @export
delete_settlement_name <- function(settlement_id,
                                   path = SETTLEMENT_NAMES_PATH(),
                                   verbose = TRUE) {
  if (!file.exists(path)) return(invisible(NULL))
  df <- load_settlement_names(path)
  before <- nrow(df)
  df <- df[df$settlement_id != settlement_id, , drop = FALSE]
  write.csv(df, path, row.names = FALSE)
  if (verbose) cat("Removed", before - nrow(df), "row(s) for #", settlement_id, "\n")
  invisible(df)
}

# =============================================================================
# SECTION 2 — relocated_features.rds (settlement & sacred-site moves)
# =============================================================================

EMPTY_RELOCATIONS <- function() list(settlements = list(),
                                     sacred_sites = list(),
                                     saved_at = NA)

#' Load relocations RDS, returning EMPTY_RELOCATIONS() if missing/corrupt.
#' @export
load_relocations <- function(path = RELOCATIONS_PATH()) {
  if (!file.exists(path)) return(EMPTY_RELOCATIONS())
  r <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(r) || !is.list(r)) return(EMPTY_RELOCATIONS())
  if (is.null(r$settlements))  r$settlements  <- list()
  if (is.null(r$sacred_sites)) r$sacred_sites <- list()
  r
}

#' Persist a relocations list (with timestamp).
#' @export
save_relocations <- function(relocations, path = RELOCATIONS_PATH()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  relocations$saved_at <- Sys.time()
  saveRDS(relocations, path)
  invisible(path)
}

#' Print pending relocations to console.
#' @export
view_relocations <- function(path = RELOCATIONS_PATH()) {
  r <- load_relocations(path)
  cat("\n=== Pending Relocations ===\n")
  if (!is.na(r$saved_at)) cat("Saved:", as.character(r$saved_at), "\n\n")
  if (length(r$settlements) > 0) {
    cat("SETTLEMENTS:\n")
    for (id in names(r$settlements)) {
      x <- r$settlements[[id]]
      cat(sprintf("  #%-6s: (%.4f, %.4f) -> (%.4f, %.4f)\n",
                  id, x$original_lon %||% NA, x$original_lat %||% NA, x$lon, x$lat))
    }
  } else cat("SETTLEMENTS: none\n")
  cat("\n")
  if (length(r$sacred_sites) > 0) {
    cat("SACRED SITES:\n")
    for (id in names(r$sacred_sites)) {
      x <- r$sacred_sites[[id]]
      cat(sprintf("  %-20s (Tier %s): (%.4f, %.4f) -> (%.4f, %.4f)\n",
                  id, x$tier %||% "?", x$original_lon %||% NA, x$original_lat %||% NA,
                  x$lon, x$lat))
    }
  } else cat("SACRED SITES: none\n")
  cat("\n")
  invisible(r)
}

#' Erase all pending relocations.
#' @export
clear_relocations <- function(path = RELOCATIONS_PATH()) {
  if (file.exists(path)) {
    file.remove(path)
    cat("Cleared:", path, "\n")
  }
  invisible(NULL)
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# =============================================================================
# SECTION 3 — custom_features.geojson (drawn rivers / forests / lakes / POIs)
# =============================================================================

CUSTOM_FEATURES_SCHEMA <- c(
  "feature_id", "name", "feature_type", "description",
  "inkarnate_path", "style_color", "style_weight",
  "label_offset_x", "label_offset_y", "hidden"
)

# Every feature type the editor can create, and the geometry it must have.
# The editor offers FEATURE_TYPES; tests/test-annotations.R checks saved
# features against FEATURE_GEOMETRY (the editor lets you retype a feature, so a
# point can end up labelled "road").
FEATURE_GEOMETRY <- c(
  road = "LINESTRING", trail = "LINESTRING", river = "LINESTRING",
  forest = "POLYGON", lake = "POLYGON", mountain = "POLYGON", region = "POLYGON",
  label = "POINT", poi = "POINT"
)
FEATURE_TYPES <- names(FEATURE_GEOMETRY)

#' Read custom_features.geojson; returns NULL if file is missing or empty.
#' Backfills any missing schema columns so the editor can rely on a fixed shape.
#' (MapBuilder has a separate load_custom_features() that additionally splits
#' POIs from other features for rendering.)
#' @export
read_custom_features <- function(path = CUSTOM_FEATURES_PATH()) {
  if (!file.exists(path)) return(NULL)
  cf <- tryCatch(st_read(path, quiet = TRUE), error = function(e) NULL)
  if (is.null(cf) || nrow(cf) == 0) return(NULL)
  for (col in CUSTOM_FEATURES_SCHEMA) {
    if (!col %in% names(cf)) {
      cf[[col]] <-
        if (col %in% c("feature_id", "style_weight",
                       "label_offset_x", "label_offset_y")) NA_real_
        else if (col == "hidden") FALSE
        else NA_character_
    }
  }
  # hidden may be saved as 0/1, "false"/"true", logical — coerce to logical.
  cf$hidden <- as.logical(cf$hidden)
  cf$hidden[is.na(cf$hidden)] <- FALSE
  cf
}

#' Persist a custom-features sf object as GeoJSON (RFC7946, no bbox).
#' @export
save_custom_features <- function(cf, path = CUSTOM_FEATURES_PATH()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  if (is.null(cf) || nrow(cf) == 0) {
    if (file.exists(path)) file.remove(path)
    return(invisible(path))
  }
  st_write(cf, path, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE,
           layer_options = c("RFC7946=YES", "WRITE_BBOX=NO"))
  invisible(path)
}

#' Print custom features to console.
#' @export
list_features <- function(path = CUSTOM_FEATURES_PATH()) {
  cf <- read_custom_features(path)
  if (is.null(cf)) { cat("No custom features.\n"); return(invisible(NULL)) }
  cat("\n=== Custom Features ===\n\n")
  for (i in seq_len(nrow(cf))) {
    cat(sprintf("  %2s. %-25s [%-10s] %s\n",
                cf$feature_id[i] %||% i,
                cf$name[i] %||% "(unnamed)",
                cf$feature_type[i] %||% "?",
                as.character(st_geometry_type(cf[i, ]))))
  }
  cat("\nTotal:", nrow(cf), "features\n\n")
  invisible(cf)
}

# =============================================================================
# SECTION 4 — pipeline hook: bake annotations into canon files
# =============================================================================

#' Apply settlement_names.csv + relocated_features.rds into the canonical
#' settlements and sacred-sites files.
#'
#' This is the single place where author-time overrides become "real". Run as
#' the last step of chapters/10-languages-and-places.qmd after NameBuilder
#' has written settlements_final.rds. Idempotent: running twice is a no-op
#' as long as the annotation files have not changed.
#'
#' Backups (.backup_<timestamp>) are written next to each canon file by
#' default, so a stale CSV that mis-names settlements can be reverted.
#'
#' @return invisible list with applied counts.
#' @export
#' Resolve one annotation row to a row of canon.
#'
#' The id is tried first and is accepted only if it lands within `max_km` of
#' the recorded coordinates -- an id that survived a rebuild by coincidence
#' and now names a town on another continent is worse than no match, because
#' it renames a stranger and says nothing. Coordinates win when they disagree.
#'
#' Returns list(idx, how, km); idx is integer(0) when nothing matched.
#' @keywords internal
resolve_settlement_row <- function(s, id, lon, lat, max_km = 75) {
  km <- function(lo, la) {
    R <- 6371
    p1 <- la * pi / 180; p2 <- lat * pi / 180
    dp <- (lat - la) * pi / 180; dl <- (lon - lo) * pi / 180
    a <- sin(dp / 2)^2 + cos(p1) * cos(p2) * sin(dl / 2)^2
    2 * R * asin(pmin(1, sqrt(a)))
  }
  sid <- suppressWarnings(as.integer(as.character(s$settlement_id)))
  have_xy <- !is.na(lon) && !is.na(lat)

  idx <- if (!is.na(id)) which(sid == as.integer(id)) else integer(0)
  if (length(idx) == 1) {
    if (!have_xy) return(list(idx = idx, how = "id", km = NA_real_))
    d <- km(s$lon[idx], s$lat[idx])
    if (d <= max_km) return(list(idx = idx, how = "id", km = d))
  }
  if (!have_xy) return(list(idx = integer(0), how = "none", km = NA_real_))

  d <- km(s$lon, s$lat)
  j <- which.min(d)
  if (length(j) == 1 && is.finite(d[j]) && d[j] <= max_km)
    return(list(idx = j, how = "coords", km = d[j]))
  list(idx = integer(0), how = "none",
       km = if (length(j) == 1) d[j] else NA_real_)
}

apply_annotations_to_canon <- function(
    settlement_names_path = SETTLEMENT_NAMES_PATH(),
    relocations_path      = RELOCATIONS_PATH(),
    settlements_path      = SETTLEMENTS_CANON(),
    sacred_sites_path     = SACRED_SITES_CANON(),
    backup                = TRUE,
    verbose               = TRUE
) {
  applied <- list(names = 0, inkarnate = 0, population = 0,
                  settlement_relocations = 0, sacred_relocations = 0)

  if (verbose) cat("\n=== Apply annotations -> canon ===\n")

  # ----- settlements_final.rds: names + inkarnate + lon/lat -----
  if (file.exists(settlements_path)) {
    s <- readRDS(settlements_path)
    if (!"settlement_id" %in% names(s))
      stop("Missing settlement_id column in ", settlements_path)
    changed <- FALSE

    # (a) name + inkarnate from CSV
    nm <- load_settlement_names(settlement_names_path)
    if (!is.null(nm) && nrow(nm) > 0) {
      # Pre-create columns at full row count. Without this, a subscripted
      # assignment like `s$name[316] <- "Bellamy"` to a non-existent column
      # creates a 316-length vector and fails the back-assign with
      # "replacement has 316 rows, data has 1222".
      if (!"name" %in% names(s))      s$name      <- NA_character_
      if (!"inkarnate" %in% names(s)) s$inkarnate <- NA_character_
      for (i in seq_len(nrow(nm))) {
        res <- resolve_settlement_row(s, nm$settlement_id[i],
                                      nm$lon[i], nm$lat[i])
        idx <- res$idx
        if (length(idx) == 0) {
          # Loud on purpose. A pinned place that cannot be found is the one
          # failure in this file that must never pass quietly: the campaign
          # refers to it by name and the next render would drop it.
          warning(sprintf(
            "Pinned settlement '%s' (id %s) could not be matched in canon%s",
            nm$name[i], nm$settlement_id[i],
            if (is.finite(res$km)) sprintf(" (nearest node %.0f km away)", res$km) else ""),
            call. = FALSE)
          if (verbose) cat(sprintf("  [MISS] %s (id %s) -- no node within range\n",
                                   nm$name[i], nm$settlement_id[i]))
          next
        }
        if (res$how == "coords") {
          if (verbose) cat(sprintf("  [remap] %s: id %s -> %s (%.1f km from anchor)\n",
                                   nm$name[i], nm$settlement_id[i],
                                   as.character(s$settlement_id[idx]), res$km))
          # Under ~15 km the anchor and the node are the same place that moved
          # a little in the rebuild. Beyond that the pinned settlement probably
          # stopped existing and its name is being ADOPTED by a neighbour --
          # defensible, but a change to canon that must not happen quietly.
          if (is.finite(res$km) && res$km > 15) {
            warning(sprintf(
              "Pinned name '%s' adopted by a node %.0f km from its anchor; the original settlement may no longer exist",
              nm$name[i], res$km), call. = FALSE)
          }
        }
        if (nzchar(nm$name[i])) {
          s$name[idx] <- nm$name[i]; applied$names <- applied$names + 1; changed <- TRUE
        }
        if (nzchar(nm$inkarnate[i])) {
          s$inkarnate[idx] <- nm$inkarnate[i]
          applied$inkarnate <- applied$inkarnate + 1; changed <- TRUE
        }
        # Story-scale population. The combined-network stage redistributes
        # population by gravity, which can leave a settlement far larger than
        # the narrative wants it. An override here is applied last so it wins
        # over whatever Ch9 computed, and is re-applied on every pipeline run.
        if (!is.na(nm$population[i])) {
          s$population[idx] <- nm$population[i]
          applied$population <- applied$population + 1; changed <- TRUE
        }
      }
    }

    # (b) lon/lat from relocations
    rel <- load_relocations(relocations_path)
    if (length(rel$settlements) > 0) {
      for (id_str in names(rel$settlements)) {
        x   <- rel$settlements[[id_str]]
        # Anchored on where the settlement was BEFORE the drag, for the same
        # reason the name rows are: the id is not stable across a rebuild.
        idx <- resolve_settlement_row(s, as.integer(id_str),
                                      if (!is.null(x$original_lon)) as.numeric(x$original_lon) else NA_real_,
                                      if (!is.null(x$original_lat)) as.numeric(x$original_lat) else NA_real_)$idx
        if (length(idx) == 0) next
        s$lon[idx] <- x$lon; s$lat[idx] <- x$lat
        applied$settlement_relocations <- applied$settlement_relocations + 1
        changed <- TRUE
      }
    }

    if (changed) {
      if (backup) {
        backup_path <- paste0(settlements_path, ".backup_",
                              format(Sys.time(), "%Y%m%d_%H%M%S"))
        file.copy(settlements_path, backup_path)
        if (verbose) cat("  Backup:", backup_path, "\n")
      }
      saveRDS(s, settlements_path)
      # Mirror to the CSV companion (kept alongside the .rds for diffing)
      csv_path <- sub("\\.rds$", ".csv", settlements_path)
      if (file.exists(csv_path)) write.csv(s, csv_path, row.names = FALSE)
      if (verbose) cat("  Wrote:", settlements_path, "\n")
    } else if (verbose) {
      cat("  Settlements: no changes to apply\n")
    }
  }

  # ----- sacred_sites_attributed.RData: lon/lat -----
  if (file.exists(sacred_sites_path)) {
    rel <- load_relocations(relocations_path)
    if (length(rel$sacred_sites) > 0) {
      e <- new.env(); load(sacred_sites_path, envir = e)
      obj_name <- ls(e)[1]
      ss <- get(obj_name, envir = e)
      changed <- FALSE
      for (sid in names(rel$sacred_sites)) {
        x   <- rel$sacred_sites[[sid]]
        idx <- which(ss$site_id == sid)
        if (length(idx) == 0) next
        if ("longitude" %in% names(ss)) ss$longitude[idx] <- x$lon
        if ("latitude"  %in% names(ss)) ss$latitude[idx]  <- x$lat
        new_pt <- st_sfc(st_point(c(x$lon, x$lat)), crs = 4326)
        st_geometry(ss)[idx] <- new_pt
        applied$sacred_relocations <- applied$sacred_relocations + 1
        changed <- TRUE
      }
      if (changed) {
        if (backup) {
          backup_path <- paste0(sacred_sites_path, ".backup_",
                                format(Sys.time(), "%Y%m%d_%H%M%S"))
          file.copy(sacred_sites_path, backup_path)
          if (verbose) cat("  Backup:", backup_path, "\n")
        }
        assign(obj_name, ss, envir = e)
        save(list = obj_name, file = sacred_sites_path, envir = e)
        if (verbose) cat("  Wrote:", sacred_sites_path, "\n")
      }
    }
  }

  if (verbose) {
    cat(sprintf(paste0("  Applied: %d names, %d populations, %d inkarnate links, ",
                       "%d settlement moves, %d sacred-site moves\n"),
                applied$names, applied$population, applied$inkarnate,
                applied$settlement_relocations, applied$sacred_relocations))
  }
  invisible(applied)
}
