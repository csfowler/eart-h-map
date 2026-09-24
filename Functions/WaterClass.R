# WaterClass.R — canonical water classification: shared codes, thresholds and
# mask helpers. This file is the SINGLE definition of "water" for the whole
# pipeline. Every consumer (WorldBuilder, ClimateBuilder, PlantBuilder,
# PopBuilder, TradeBuilder, RoadBuilder, MapBuilder) sources this FIRST and reads
# the canonical classified raster via read_water_class().
#
# The canonical layer itself is built at the END of Chapter 2 by
# build_water_class_layers() in WorldBuilder.R (a single 0.01° classified raster
# + 0.05°/0.25° precedence-aggregated derivations). See the plan: "Unified Water
# Framework + Full Pipeline Regeneration".
#
# No heavy deps here (only terra at read time) so it can be sourced cheaply.

# --- Class codes: one integer per cell, exactly one class ---
WATER_CLASS <- list(
  LAND         = 0L,
  OCEAN        = 1L,   # sea: wet cell connected to the deep-ocean margin
  LAKE         = 2L,   # inland standing water (any elevation): landlocked basin,
                       #   lake polygon, or detected depression lake
  RIVER_NAV    = 3L,   # navigable river centerline (FA >= WATER_NAV_FA)
  STREAM_MINOR = 4L    # minor watercourse (WATER_RIVER_FA <= FA < WATER_NAV_FA)
)
# Precedence when composing / aggregating (high wins):
# OCEAN > LAKE > RIVER_NAV > STREAM_MINOR > LAND
WATER_CLASS_PRECEDENCE <- c(WATER_CLASS$OCEAN, WATER_CLASS$LAKE,
                            WATER_CLASS$RIVER_NAV, WATER_CLASS$STREAM_MINOR,
                            WATER_CLASS$LAND)

# --- Shared flow-accumulation thresholds (replace scattered 1000/10000/25000) ---
WATER_RIVER_FA         <- 1000     # a channel EXISTS (matches rivers.tif cut;
                                   #   keeping this avoids re-running hydrology)
WATER_NAV_FA           <- 10000    # NAVIGABILITY: blocks roads (ferry) and is a
                                   #   trade waterway
WATER_LAKE_MIN_CELLS   <- 1000     # min 0.01° cells for a lake to be a TRADE
                                   #   waterway (roads still block ALL lakes)
WATER_DEEP_SEED        <- -200     # elev <= this seeds the ocean flood-fill

# --- Display-only (MapBuilder river vectorization) ---
WATER_MAP_RIVER_MIN_FA <- 25000
WATER_MAP_TIER_BREAKS  <- c(big = 500000, med = 50000, small = 25000)

# --- Ocean continental-shelf DEPTH model defaults (render-only) ---
# These are FALLBACKS. The pipeline fits a*(dist/dmax)^curve + noise from real
# sunken shelves (Tusque, Fingers) via fit_shelf_params() in WorldBuilder.R and
# overrides these at build time; the fitted params are cached so the standalone
# Map/_fit_shelf_params.R script is retired.
WATER_SHELF_MAX_DEPTH   <- -1000   # deepest synthetic ocean depth (m, negative)
WATER_SHELF_MAX_DIST_KM <- 15      # distance from coast to reach max depth;
                                   #   SMALL => steep falloff near coast (user's
                                   #   initial tests had deep ocean very near shore)
WATER_SHELF_CURVE       <- 1.5     # power-law exponent
WATER_SHELF_NOISE_SD    <- 40      # m, Gaussian noise on synthetic shelf

# --- Canonical layer paths (single source of truth) ---
.water_class_paths <- function() list(
  "01"  = here::here("Input Data/HighResolution/water_class.vrt"),
  "05"  = here::here("Input Data/HighResolution/water_class_05.tif"),
  "025" = here::here("Input Data/HighResolution/water_class_025.tif")
)
water_depth_path <- function() here::here("Input Data/HighResolution/water_depth.vrt")

#' Read the canonical classified water raster.
#' @param res "01" (0.01°, canonical), "05" (0.05°) or "025" (0.25°)
read_water_class <- function(res = c("01", "05", "025")) {
  res <- match.arg(res)
  f <- .water_class_paths()[[res]]
  if (!file.exists(f))
    stop("Canonical water_class layer missing: ", f,
         "\n  Build it at the end of Chapter 2 via build_water_class_layers().")
  terra::rast(f)
}

#' Read the render-only depth layer (positive metres below local water surface).
read_water_depth <- function() {
  f <- water_depth_path()
  if (!file.exists(f)) stop("water_depth.vrt missing: ", f)
  terra::rast(f)
}

# --- Boolean masks. Accept a SpatRaster OR a numeric vector of class codes. ---
is_ocean     <- function(wc) wc == WATER_CLASS$OCEAN
is_lake      <- function(wc) wc == WATER_CLASS$LAKE
is_navigable <- function(wc) wc == WATER_CLASS$RIVER_NAV
is_stream    <- function(wc) wc == WATER_CLASS$STREAM_MINOR
is_water     <- function(wc) !is.na(wc) & wc != WATER_CLASS$LAND

# Roads: navigable water (ocean + lake + navigable river) is impassable -> ferry.
# Minor streams stay passable (cheap crossing) and only carry a cost penalty.
is_blocking_for_roads <- function(wc)
  wc %in% c(WATER_CLASS$OCEAN, WATER_CLASS$LAKE, WATER_CLASS$RIVER_NAV)

# Trade: ocean is handled separately (port/ocean network); a continental
# "waterway" (fast travel) = lake + navigable river.
is_waterway_for_trade <- function(wc)
  wc %in% c(WATER_CLASS$LAKE, WATER_CLASS$RIVER_NAV)

# --- Per-cell owner-clipped VRT mosaic -------------------------------------
# Continent placement order (first-placed = owner of any overlap cell).
WATER_PLACEMENT_ORDER <- c("Centre", "Tusque", "Salmon", "Banff", "Stockholm",
                           "Adirondaq", "Fingers", "Kiliman", "Amazonia", "Canada")

#' Build a VRT from per-continent tifs, clipped per-cell to the OWNING continent.
#'
#' Each tif is parented by its continent dir (basename(dirname(f))). A cell is
#' set to NA wherever ANOTHER continent OWNS the land there (continent_owner.tif
#' != this continent's placement index, and not NA). This resolves overlaps by
#' placement precedence (first-placed wins), fixing BOTH failure modes of a
#' plain last-wins VRT: a neighbour's ocean bleeding over the owner's land, and
#' the owner's coastal-shelf ocean bleeding over a neighbour's land. Owner is
#' land-only, so genuine inter-continent sea (owner NA) is left for everyone and
#' the mosaic order only breaks ties there (both water -> harmless).
#'
#' @param tif_files per-continent tif paths
#' @param out_vrt   output .vrt path
#' @param owner_file continent_owner.tif (must exist)
#' @param datatype  override write datatype (else inferred from source)
owner_clipped_vrt <- function(tif_files, out_vrt,
                              owner_file = here::here("Input Data/continent_owner.tif"),
                              datatype = NULL, suffix = "_owned", verbose = FALSE) {
  if (!file.exists(owner_file))
    stop("owner_clipped_vrt: continent_owner.tif missing: ", owner_file)
  owner <- terra::rast(owner_file)
  clipped <- character(0)
  for (f in tif_files) {
    idx <- match(basename(dirname(f)), WATER_PLACEMENT_ORDER)
    if (is.na(idx)) { clipped <- c(clipped, f); next }   # unknown -> leave as-is
    r  <- terra::rast(f)
    oc <- terra::resample(terra::crop(owner, terra::ext(r), snap = "out"), r, method = "near")
    rc <- terra::ifel(!is.na(oc) & oc != idx, NA, r)     # drop where another continent owns land
    dt <- if (!is.null(datatype)) datatype else terra::datatype(r)[1]
    of  <- sub("\\.tif$", paste0(suffix, ".tif"), f)
    tmp <- tempfile(fileext = ".tif")
    wargs <- list(rc, tmp, overwrite = TRUE, datatype = dt, wopt = list(gdal = c("COMPRESS=LZW")))
    if (grepl("^INT1U", dt)) wargs$NAflag <- 255         # keep code 0 (ocean) distinct from NA
    do.call(terra::writeRaster, wargs)
    if (file.exists(of)) file.remove(of)
    file.rename(tmp, of)
    clipped <- c(clipped, of)
    if (verbose) cat("    owner-clip", basename(dirname(f)), "\n")
  }
  terra::vrt(clipped, out_vrt, overwrite = TRUE)
  invisible(out_vrt)
}
