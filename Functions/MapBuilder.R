# =============================================================================
# MapBuilder.R - Generate XYZ Tiles and Standalone Reference Map for EART-H
#
# Two products:
#   1. Tile pyramids (elevation, biome, rivers) under <map root>/tiles/
#   2. A self-contained <map root>/index.html reference map with all layers
#      inlined: roads, settlements, sacred sites + quotes, hex hierarchy,
#      custom annotations, POIs with Inkarnate hand-off.
#
# Public entry point: build_reference_map()
# =============================================================================

library(terra)
library(sf)
library(jsonlite)

# The map root is no longer here::here("Map") — see Functions/MapRoot.R. The
# default arguments of build_reference_map() and build_player_bundle() call
# map_root(), so this has to be loaded before either is defined.
if (!exists("map_root")) source(here::here("Functions/MapRoot.R"))

# =============================================================================
# GDAL COMMAND-LINE TOOLS
#
# Only rebuild_tiles = TRUE needs these (gdaldem + gdal2tiles, for the z0-8
# pyramid). Everything else -- the vector build, the tile server, the tests --
# runs on terra/sf alone, so a missing GDAL is reported when the pyramid is
# asked for, not every time this file is sourced.
#
# Search order: whatever is on PATH (Homebrew, apt, conda, an OSGeo4W shell),
# then OSGeo4W's default install, then r-miniconda.
# =============================================================================

.gdal_bundles <- function() {
  lad <- Sys.getenv("LOCALAPPDATA")
  osgeo_py <- Sys.glob("C:/OSGeo4W/apps/Python*/Scripts/gdal2tiles.py")
  list(
    osgeo4w = list(python   = "C:/OSGeo4W/bin/python.exe",
                   gdal2tiles = if (length(osgeo_py)) osgeo_py[length(osgeo_py)] else "",
                   gdaldem  = "C:/OSGeo4W/bin/gdaldem.exe",
                   proj_lib = "C:/OSGeo4W/share/proj"),
    miniconda = list(python   = file.path(lad, "r-miniconda/python.exe"),
                     gdal2tiles = file.path(lad, "r-miniconda/Scripts/gdal2tiles.py"),
                     gdaldem  = file.path(lad, "r-miniconda/Library/bin/gdaldem.exe"),
                     proj_lib = file.path(lad, "r-miniconda/Library/share/proj"))
  )
}

#' Locate gdaldem and gdal2tiles. Returns NULL when either is missing.
#' `gdal2tiles` is a command prefix: either the executable itself (PATH) or
#' python + the script (bundled installs, where the .py is not executable).
detect_gdal_paths <- function(verbose = FALSE) {
  dem <- Sys.which("gdaldem")
  # A bare .py on a Windows PATH runs only if the file association is right.
  g2t <- Sys.which(if (.Platform$OS.type == "windows") "gdal2tiles"
                   else c("gdal2tiles", "gdal2tiles.py"))
  g2t <- g2t[nzchar(g2t)]
  if (nzchar(dem) && length(g2t)) {
    if (verbose) message("Using GDAL from PATH: ", dirname(dem))
    return(list(name = "PATH", gdaldem = unname(dem),
                gdal2tiles = unname(g2t[1]), proj_lib = NULL))
  }
  for (name in names(b <- .gdal_bundles())) {
    cfg <- b[[name]]
    if (all(file.exists(c(cfg$python, cfg$gdal2tiles, cfg$gdaldem,
                          file.path(cfg$proj_lib, "proj.db"))))) {
      if (verbose) message("Using GDAL from: ", name)
      return(list(name = name, gdaldem = cfg$gdaldem,
                  gdal2tiles = c(cfg$python, cfg$gdal2tiles),
                  proj_lib = cfg$proj_lib))
    }
  }
  NULL
}

.require_gdal <- function() {
  g <- detect_gdal_paths(verbose = TRUE)
  if (is.null(g))
    stop("rebuild_tiles = TRUE needs the GDAL command-line tools (gdaldem and ",
         "gdal2tiles), and neither PATH nor C:/OSGeo4W has them.\n",
         "  Windows: install OSGeo4W (https://trac.osgeo.org/osgeo4w/), Express, package 'GDAL'\n",
         "  macOS:   brew install gdal\n",
         "  Linux:   sudo apt install gdal-bin python3-gdal\n",
         "Then restart R. See GETTING-STARTED.md.", call. = FALSE)
  g
}

# Windows 8.3 names sidestep spaces in paths; elsewhere the path is used as is.
.short_path <- function(p) {
  p <- normalizePath(p, winslash = "/", mustWork = FALSE)
  if (.Platform$OS.type == "windows") utils::shortPathName(p) else p
}

#' Run a GDAL command. PROJ_LIB and PATH are set for the call only: set
#' globally, a bundled (older) proj.db poisons terra's own EPSG lookups.
.gdal_system <- function(gdal, args, verbose = TRUE) {
  old <- Sys.getenv(c("PROJ_LIB", "PATH"), unset = NA)
  on.exit({
    if (is.na(old[["PROJ_LIB"]])) Sys.unsetenv("PROJ_LIB") else Sys.setenv(PROJ_LIB = old[["PROJ_LIB"]])
    Sys.setenv(PATH = old[["PATH"]])
  }, add = TRUE)
  if (!is.null(gdal$proj_lib)) {
    Sys.setenv(PROJ_LIB = gdal$proj_lib)
    Sys.setenv(PATH = paste(dirname(gdal$gdaldem), old[["PATH"]], sep = .Platform$path.sep))
  }
  # system2 quotes the command but not its arguments.
  system2(args[1], shQuote(args[-1]),
          stdout = if (verbose) "" else FALSE,
          stderr = if (verbose) "" else FALSE)
}

# =============================================================================
# BIOME / ELEVATION PALETTES
# =============================================================================

BIOME_INFO <- list(
  "0" = list(code = 0, name = "Ocean",            color = "#08519c"),
  "1" = list(code = 1, name = "Glacial/Arctic",   color = "#f7fbff"),
  "2" = list(code = 2, name = "Taiga",            color = "#2ca25f"),
  "3" = list(code = 3, name = "Desert",           color = "#fee391"),
  "4" = list(code = 4, name = "Grassland",        color = "#c7e9b4"),
  "5" = list(code = 5, name = "Temperate Forest", color = "#238b45"),
  "6" = list(code = 6, name = "Tropical Forest",  color = "#00441b"),
  "7" = list(code = 7, name = "Mountainous",      color = "#737373"),
  "8" = list(code = 8, name = "Tundra",           color = "#d0d1e6")
)

# Warm parchment-toned elevation palette for medieval aesthetic
ELEVATION_PALETTE <- list(
  # Negative range gives the gradient seen on the sunken continents
  # (Fingers, Tusque) — we want every continent to show this pattern,
  # achieved by stochastically populating shelf bathymetry in
  # WorldBuilder::add_shelf_bathymetry (run at the end of Ch 2).
  # The -5 stop matches the hillshade-composite lake fill (#3a648c) and
  # the river overlay, so rivers entering lakes are seamless.
  breaks = c(-1000, -750, -500, -300, -200, -100,  -50,  -20,   -5,
                 0,   50,  100,  200,  300,  500,  700, 1000,
              1200, 1500, 2000, 2500, 3000, 4000, 5000, 6500),
  colors = c("#142a44", "#1d3b58", "#26506e", "#33688a", "#4179a3", "#5089b5",
             "#6499c5", "#7caacd", "#3a648c",
             "#9ba980",
             "#a3b478", "#a7bd6e", "#b1be64", "#b8b65a", "#beb058", "#c4ad5b",
             "#c0a06a",
             "#b6927a", "#a98a80", "#aa9388", "#bda194", "#d0b8a2",
             "#dfcdb8", "#ecdfc8", "#f6ede0")
)

# Hillshade exaggeration, shared by the canonical pyramid
# (generate_hillshade_composite, EPSG:4326) and the procedural tile server
# (composite_terrain, EPSG:3857). The canonical hillshade MUST be computed with
# a metric horizontal scale (-s 111120 m/deg) so slopes are physically real;
# the exaggeration then lives in z. HILLSHADE_Z_EXAG is tuned to the dramatic
# "zf60" relief look. Keep the two renderers on the SAME exaggeration so the
# z8->z9 (canonical->procedural) seam matches.
HILLSHADE_Z_EXAG  <- 60      # vertical exaggeration
HILLSHADE_DEG_SCALE <- 111120  # metres per degree (gdaldem -s, for 4326 input)

# =============================================================================
# VECTOR RIVERS
# =============================================================================

#' Vectorise per-continent rivers.tif into line segments.
#'
#' Each river pixel emits a single line segment from its centre to the
#' centre of its downstream neighbour. Downstream = the 8-connectivity
#' neighbour with the highest flow_accumulation (and itself a river or at
#' least higher-FA than the source). The flow_accumulation at the upstream
#' end of the segment is attached as `flow` so the renderer can scale line
#' weight by drainage size.
#'
#' Returns one sf with columns: continent, flow, geometry (LINESTRING).
#' Crosses-continent or unreachable pixels are dropped.
build_river_lines <- function(continent_dir = here::here("Input Data/Continents"),
                              min_flow = 1000,
                              ownership_clip = TRUE,
                              verbose = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf required")
  cont_dirs <- list.dirs(continent_dir, recursive = FALSE, full.names = TRUE)

  # Continent-transform extents OVERLAP; without clipping, BOTH continents'
  # river networks ship in the shared zones and one of them flows uphill over
  # the blended DEM (2026-07-08 hydrology audit: 54 duplicate-network
  # clusters). Keep each continent's rivers only where that continent OWNS the
  # land per continent_polygons.gpkg (unowned/offshore cells are kept).
  own_r <- NULL; own_ids <- NULL
  if (ownership_clip) {
    gpkg <- here::here("Input Data/continent_polygons.gpkg")
    if (file.exists(gpkg)) {
      # A PROJ_LIB inherited from a GDAL shell poisons terra's EPSG lookups
      # ("empty srs"). Shield this terra-only block.
      old_pl <- Sys.getenv("PROJ_LIB"); Sys.unsetenv("PROJ_LIB")
      polys <- sf::st_read(gpkg, quiet = TRUE)
      ncol_nm <- intersect(c("continent", "name"), names(polys))[1]
      own_ids <- as.character(polys[[ncol_nm]])
      pv   <- terra::vect(polys)
      tmpl <- terra::rast(terra::ext(-180, 180, -90, 90), resolution = 0.05,
                          crs = terra::crs(pv))
      own_r <- terra::rasterize(pv, tmpl, field = seq_len(nrow(polys)))
      if (nzchar(old_pl)) Sys.setenv(PROJ_LIB = old_pl)
    } else if (verbose) {
      cat("  ownership clip skipped: continent_polygons.gpkg missing\n")
    }
  }

  out_list <- list()
  for (cd in cont_dirs) {
    cname <- basename(cd)
    rfile <- file.path(cd, "rivers.tif")
    ffile <- file.path(cd, "flow_accumulation.tif")
    if (!file.exists(rfile) || !file.exists(ffile)) next
    r  <- terra::rast(rfile)
    fa <- terra::rast(ffile)
    if (!terra::compareGeom(r, fa, stopOnError = FALSE)) {
      fa <- terra::resample(fa, r, method = "bilinear")
    }
    rv  <- terra::values(r,  mat = FALSE)
    fav <- terra::values(fa, mat = FALSE)
    river_idx <- which(!is.na(rv) & rv > 0)
    if (!length(river_idx)) next

    rc <- terra::rowColFromCell(r, river_idx)
    xy <- terra::xyFromCell(r, river_idx)
    fa_here <- fav[river_idx]
    ncol_r <- terra::ncol(r); nrow_r <- terra::nrow(r)

    # 8-neighbour offsets
    drs <- c(-1,-1,-1, 0, 0, 1, 1, 1)
    dcs <- c(-1, 0, 1,-1, 1,-1, 0, 1)

    # Skip cells below min_flow upstream — they don't get vectorised at all
    # so we never pay the per-cell loop cost for tributaries we wouldn't ship.
    above <- fa_here >= min_flow
    river_idx <- river_idx[above]; rc <- rc[above, , drop = FALSE]
    xy <- xy[above, , drop = FALSE]; fa_here <- fa_here[above]

    # ownership clip: drop cells another continent owns
    if (!is.null(own_r)) {
      ov <- terra::extract(own_r, xy)[, 1]
      mine <- is.na(ov) | own_ids[ov] == cname
      if (verbose && any(!mine))
        cat(sprintf("  %s: ownership clip drops %d/%d river cells\n",
                    cname, sum(!mine), length(mine)))
      river_idx <- river_idx[mine]; rc <- rc[mine, , drop = FALSE]
      xy <- xy[mine, , drop = FALSE]; fa_here <- fa_here[mine]
    }
    n_river <- length(river_idx)
    if (n_river == 0) next

    seg_x1 <- numeric(n_river); seg_y1 <- numeric(n_river)
    seg_x2 <- numeric(n_river); seg_y2 <- numeric(n_river)
    seg_flow <- numeric(n_river); seg_keep <- logical(n_river)

    for (i in seq_len(n_river)) {
      r_i <- rc[i, 1]; c_i <- rc[i, 2]
      best_fa <- fa_here[i]; best_cell <- NA_integer_
      for (k in seq_along(drs)) {
        nr <- r_i + drs[k]; nc <- c_i + dcs[k]
        if (nr < 1 || nr > nrow_r || nc < 1 || nc > ncol_r) next
        n_cell <- terra::cellFromRowCol(r, nr, nc)
        n_fa <- fav[n_cell]
        if (is.na(n_fa) || n_fa <= best_fa) next
        best_fa <- n_fa; best_cell <- n_cell
      }
      if (is.na(best_cell)) next
      nxy <- terra::xyFromCell(r, best_cell)
      seg_x1[i] <- xy[i, 1]; seg_y1[i] <- xy[i, 2]
      seg_x2[i] <- nxy[1, 1]; seg_y2[i] <- nxy[1, 2]
      seg_flow[i] <- fa_here[i]
      seg_keep[i] <- TRUE
    }

    keep <- which(seg_keep)
    if (!length(keep)) next
    lines <- lapply(keep, function(i) sf::st_linestring(
      matrix(c(seg_x1[i], seg_y1[i], seg_x2[i], seg_y2[i]), ncol = 2, byrow = TRUE)
    ))
    df <- sf::st_sf(
      continent = cname,
      flow      = seg_flow[keep],
      geometry  = sf::st_sfc(lines, crs = 4326),
      stringsAsFactors = FALSE
    )
    out_list[[cname]] <- df
    if (verbose) cat(sprintf("  rivers: %s -> %d segments (max flow %.0f)\n",
                              cname, nrow(df), max(df$flow)))
  }
  if (length(out_list) == 0) return(NULL)
  do.call(rbind, out_list)
}

#' Chain 2-point segments into multi-point LINESTRINGs and tier by flow.
#'
#' Each tier is processed separately (per continent) through sf::st_line_merge
#' — GEOS connects head-to-tail edges into chains and correctly leaves a node
#' boundary at confluences (degree-3+ junctions). Output sf has columns
#' continent, tier ("big"/"med"/"small"), max_flow (max flow of any input
#' segment within the merged line), geometry (LINESTRING).
chain_and_tier_rivers <- function(rivers_sf,
                                  tier_breaks = c(500000, 50000, 25000),
                                  verbose = TRUE) {
  if (is.null(rivers_sf) || nrow(rivers_sf) == 0) return(NULL)
  if (!requireNamespace("sf", quietly = TRUE)) stop("sf required")

  rivers_sf$tier <- cut(rivers_sf$flow,
                        breaks = c(0, tier_breaks[3], tier_breaks[2],
                                   tier_breaks[1], Inf),
                        labels = c("dropped", "small", "med", "big"),
                        right = FALSE)
  rivers_sf <- rivers_sf[rivers_sf$tier != "dropped", ]
  rivers_sf$tier <- droplevels(rivers_sf$tier)

  conts <- unique(rivers_sf$continent)
  out <- list()
  for (cn in conts) {
    for (tier in c("big", "med", "small")) {
      sub <- rivers_sf[rivers_sf$continent == cn & rivers_sf$tier == tier, ]
      if (nrow(sub) == 0) next
      # Build one MULTILINESTRING from all 2-pt segments
      mats <- lapply(seq_len(nrow(sub)), function(i) sf::st_coordinates(sub$geometry[i])[, 1:2])
      ml   <- sf::st_multilinestring(mats)
      mlsfc <- sf::st_sfc(ml, crs = 4326)
      merged <- sf::st_line_merge(mlsfc)
      # Split MULTILINESTRING back into individual LINESTRINGs
      lines <- sf::st_cast(merged, "LINESTRING", warn = FALSE)
      # PER-CHAIN max_flow: match each source segment to its chain through a
      # shared-vertex key and take the max of its member flows. The old code
      # stamped the CONTINENT-tier max on every chain, which gave e.g. every
      # big chain on a continent an identical flow — breaking FeatureNamer's
      # largest-tributary mainstem choice and faking "duplicate basin" audits.
      cco <- sf::st_coordinates(lines)
      vkey <- paste(round(cco[, 1], 6), round(cco[, 2], 6))
      vchain <- cco[, "L1"]
      seg1 <- t(vapply(mats, function(m) m[1, ], numeric(2)))
      skey <- paste(round(seg1[, 1], 6), round(seg1[, 2], 6))
      schain <- vchain[match(skey, vkey)]
      max_flow <- rep(max(sub$flow, na.rm = TRUE), length(lines))
      ok <- !is.na(schain)
      if (any(ok)) {
        agg <- tapply(sub$flow[ok], schain[ok], max, na.rm = TRUE)
        max_flow[as.integer(names(agg))] <- as.numeric(agg)
      }
      tdf <- sf::st_sf(
        continent = cn, tier = tier, max_flow = max_flow,
        geometry  = lines, stringsAsFactors = FALSE
      )
      if (verbose) cat(sprintf("  %s/%s: %d segments -> %d chains\n",
                                cn, tier, nrow(sub), nrow(tdf)))
      out[[paste(cn, tier, sep = "_")]] <- tdf
    }
  }
  if (length(out) == 0) return(NULL)
  do.call(rbind, out)
}

# =============================================================================
# TILE PIPELINE (kept from prior implementation)
# =============================================================================

build_water_vrt <- function(continent_dir = here::here("Input Data/Continents"),
                            output_path = here::here("Input Data/HighResolution/water_mask.vrt"),
                            verbose = TRUE) {
  water_files <- list.files(continent_dir, pattern = "^transform_.*_water\\.tif$",
                            full.names = TRUE, recursive = TRUE)
  if (length(water_files) == 0) {
    warning("No water_mask.tif files found in ", continent_dir)
    return(NULL)
  }
  if (verbose) cat(sprintf("  Building water mask VRT from %d continents\n", length(water_files)))
  vrt(water_files, output_path, overwrite = TRUE)
  return(output_path)
}

generate_hillshade_composite <- function(elevation_path,
                                         water_vrt_path = NULL,
                                         output_path,
                                         z_factor = HILLSHADE_Z_EXAG,
                                         azimuth = 315,
                                         altitude = 45,
                                         blend = 0.55,
                                         force = FALSE,
                                         gdal = NULL,
                                         verbose = TRUE) {
  if (file.exists(output_path) && !force) {
    if (verbose) cat("  Hillshade composite exists, skipping\n")
    return(output_path)
  }
  if (is.null(gdal)) gdal <- .require_gdal()
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

  elev_short <- .short_path(elevation_path)
  hillshade_tif <- tempfile(fileext = "_hillshade.tif")
  hillshade_tif <- file.path(.short_path(dirname(hillshade_tif)), basename(hillshade_tif))

  # -s 111120 converts the 0.01deg (EPSG:4326) horizontal units to metres so the
  # slope (and thus the shading) is physically correct; z carries the drama.
  # Without -s, gdaldem computes slope as metres/degree -- a ~111120x
  # exaggeration that saturates the hillshade. (This was the bug that made the
  # canonical tiles look far more rugged than the metric procedural tiles.)
  if (verbose) cat("  Step 1: hillshade (z=", z_factor, ", s=", HILLSHADE_DEG_SCALE, ")\n", sep = "")
  .gdal_system(gdal, c(gdal$gdaldem, "hillshade", elev_short, hillshade_tif,
                       "-z", z_factor, "-s", HILLSHADE_DEG_SCALE,
                       "-az", azimuth, "-alt", altitude, "-compute_edges"),
               verbose = verbose)
  if (!file.exists(hillshade_tif)) stop("gdaldem hillshade failed")

  color_file <- tempfile(fileext = "_elev_colors.txt")
  create_gdal_color_file(ELEVATION_PALETTE$breaks, ELEVATION_PALETTE$colors,
                         color_file, verbose = FALSE)
  colored_tif <- tempfile(fileext = "_colored.tif")
  if (verbose) cat("  Step 2: color-relief\n")
  .gdal_system(gdal, c(gdal$gdaldem, "color-relief", elev_short,
                       .short_path(color_file), colored_tif, "-alpha"),
               verbose = verbose)
  if (!file.exists(colored_tif)) stop("gdaldem color-relief failed")

  if (verbose) cat("  Step 3: composite\n")
  hs <- rast(hillshade_tif); cr <- rast(colored_tif)
  if (!compareGeom(hs, cr, stopOnError = FALSE)) hs <- resample(hs, cr[[1]], method = "bilinear")
  hs_norm <- hs / 255
  composite <- cr
  for (b in 1:3) composite[[b]] <- cr[[b]] * (blend + (1 - blend) * hs_norm)

  # Step 4: paint ocean + lakes from the canonical water class, depth-shaded
  # (rivers are drawn separately as vector lines). Falls back to the old flat
  # water-mask paint if the canonical layer isn't built.
  if (!exists("WATER_CLASS")) source(here::here("Functions/WaterClass.R"))
  wc_path <- here::here("Input Data/HighResolution/water_class.vrt")
  if (file.exists(wc_path)) {
    if (verbose) cat("  Step 4: paint ocean/lakes (depth-shaded)\n")
    wc <- resample(rast(wc_path), cr[[1]], method = "near")
    water_mask <- !is.na(wc) & (wc == WATER_CLASS$OCEAN | wc == WATER_CLASS$LAKE)
    composite[[1]][water_mask] <- 62
    composite[[2]][water_mask] <- 104
    composite[[3]][water_mask] <- 150
    composite[[4]][water_mask] <- 255
    dpath <- here::here("Input Data/HighResolution/water_depth.vrt")
    if (file.exists(dpath)) {
      depth <- resample(rast(dpath), cr[[1]], method = "bilinear")
      dn <- clamp(depth / 1000, 0, 1)              # 0 shallow .. 1 deep (~1000 m)
      # water_depth.vrt is NA over the far ocean (outside continent transforms);
      # NA would paint BLACK (NA -> 0 under INT1U). Missing depth = deep sea —
      # same rule as TileServer::composite_terrain.
      dn <- ifel(is.na(dn), 1, dn)
      # Deep must stay unmistakably BLUE — the old (10,30,80) floor read as
      # black at world zoom. Shared with TileServer::composite_terrain.
      shallow <- c(126, 178, 214); deep <- c(38, 76, 128)
      for (b in 1:3) {
        col_b <- shallow[b] + (deep[b] - shallow[b]) * dn
        composite[[b]][water_mask] <- col_b[water_mask]
      }
      rm(depth, dn)
    }
    rm(wc)
  } else if (!is.null(water_vrt_path) && file.exists(water_vrt_path)) {
    if (verbose) cat("  Step 4: paint lakes (fallback flat)\n")
    water <- resample(rast(water_vrt_path), cr[[1]], method = "near")
    lake_mask <- !is.na(water) & water > 0
    composite[[1]][lake_mask] <- 62
    composite[[2]][lake_mask] <- 104
    composite[[3]][lake_mask] <- 150
    composite[[4]][lake_mask] <- 255
  }
  # Outside the continent transforms EVERYTHING is NA (elevation, water_class,
  # water_depth all end at the transform extents) — but that region is, by
  # definition, open ocean. NA rgb wrote OPAQUE BLACK tiles (and the alpha
  # band was all-zero), which is why the far ocean rendered black. Fill the
  # far field with the deep-water colour and make the composite fully opaque.
  if (verbose) cat("  Step 5: deep-ocean far-field fill\n")
  rm(hs, hs_norm); gc()
  deep_fill <- c(38, 76, 128)
  # subst/app stream block-wise; ifel here allocated full-globe temporaries
  # on top of the live rasters and died with std::bad_alloc.
  for (b in 1:3) composite[[b]] <- subst(composite[[b]], NA, deep_fill[b])
  composite[[4]] <- app(composite[[4]], fun = function(x) { x[] <- 255; x })

  writeRaster(composite, output_path, overwrite = TRUE, datatype = "INT1U",
              wopt = list(gdal = c("COMPRESS=LZW")))
  unlink(c(hillshade_tif, colored_tif, color_file))
  if (verbose) cat("  Hillshade composite complete\n")
  return(output_path)
}

create_gdal_color_file <- function(breaks, colors, output_path, verbose = TRUE) {
  rgb_matrix <- col2rgb(colors)
  lines <- c("nv 0 0 0 0")
  for (i in seq_along(breaks)) {
    alpha <- 255
    if (nchar(colors[i]) == 9 && substr(colors[i], 8, 9) == "00") alpha <- 0
    lines <- c(lines, sprintf("%s %d %d %d %d",
                              breaks[i], rgb_matrix[1, i], rgb_matrix[2, i],
                              rgb_matrix[3, i], alpha))
  }
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(lines, output_path)
  if (verbose) cat("  Created color file:", output_path, "\n")
  return(output_path)
}

generate_xyz_tiles <- function(raster_input, output_dir, layer_name,
                               min_zoom = 0, max_zoom = 6,
                               color_file = NULL, resampling = "average",
                               force = FALSE, gdal = NULL, verbose = TRUE) {
  tile_dir <- file.path(output_dir, "tiles", layer_name)
  if (dir.exists(tile_dir) && !force) {
    existing <- list.files(tile_dir, pattern = "\\.png$", recursive = TRUE)
    if (length(existing) > 0) {
      if (verbose) cat(sprintf("  %s: %d tiles exist, skipping\n", layer_name, length(existing)))
      return(tile_dir)
    }
  }
  if (is.null(gdal)) gdal <- .require_gdal()
  if (force && dir.exists(tile_dir)) unlink(tile_dir, recursive = TRUE)
  dir.create(tile_dir, recursive = TRUE, showWarnings = FALSE)
  if (verbose) cat(sprintf("Generating tiles for: %s\n", layer_name))

  if (inherits(raster_input, "SpatRaster")) {
    temp_tif <- tempfile(fileext = ".tif")
    writeRaster(raster_input, temp_tif, overwrite = TRUE, datatype = "FLT4S")
    raster_path <- temp_tif
  } else raster_path <- raster_input

  raster_path    <- .short_path(raster_path)
  tile_dir_short <- .short_path(tile_dir)

  if (!is.null(color_file) && file.exists(color_file)) {
    if (verbose) cat("  Applying color map\n")
    colored_tif <- tempfile(fileext = ".tif")
    .gdal_system(gdal, c(gdal$gdaldem, "color-relief", raster_path,
                         .short_path(color_file), colored_tif, "-alpha"),
                 verbose = verbose)
    if (file.exists(colored_tif)) {
      raster_path <- .short_path(colored_tif)
    } else warning("gdaldem color-relief failed, using raw raster")
  }
  g2t_args <- c(vapply(gdal$gdal2tiles, .short_path, ""),
                "-z", sprintf("%d-%d", min_zoom, max_zoom), "-w", "none",
                "-r", resampling, raster_path, tile_dir_short)
  if (verbose) cat(sprintf("  Running gdal2tiles (zoom %d-%d)...\n", min_zoom, max_zoom))
  .gdal_system(gdal, g2t_args, verbose = verbose)

  # r-miniconda's Python can fail to import _gdal ("DLL load failed") when
  # another GDAL is on PATH. The same command under Git Bash gets a clean
  # environment and works, so retry there if the native run wrote nothing.
  pre_count <- length(list.files(tile_dir, pattern = "\\.png$", recursive = TRUE))
  bash_exe  <- Sys.which("bash")
  if (pre_count == 0 && .Platform$OS.type == "windows" && nzchar(bash_exe)) {
    if (verbose) cat("  (retrying gdal2tiles under bash)\n")
    system2(bash_exe, c("-c", shQuote(paste(shQuote(g2t_args, type = "sh"),
                                            collapse = " "))),
            stdout = if (verbose) "" else FALSE,
            stderr = if (verbose) "" else FALSE)
  }
  tile_count <- length(list.files(tile_dir, pattern = "\\.png$", recursive = TRUE))
  if (verbose) cat(sprintf("  Generated %d tiles\n", tile_count))
  return(tile_dir)
}

# =============================================================================
# DATA LOADERS
# =============================================================================

#' Load canonical settlements.
#'
#' Source of truth is Combined/settlements_final.rds — name + inkarnate
#' overrides and relocations have already been baked in by
#' AnnotationBuilder::apply_annotations_to_canon() at the end of Ch10.
#' Map-builder is a pure reader.
load_settlements <- function(settlements_path, verbose = TRUE) {
  if (!file.exists(settlements_path))
    stop("Settlements file not found: ", settlements_path)
  s <- readRDS(settlements_path)
  if (!"settlement_id" %in% names(s))
    stop("Missing settlement_id column in ", settlements_path)
  if (!"inkarnate" %in% names(s)) s$inkarnate <- NA_character_
  if (verbose) cat(sprintf("  Loaded %d settlements\n", nrow(s)))
  s
}

#' Load canonical sacred sites with attached quotes.
load_sacred_sites <- function(sacred_path, verbose = TRUE) {
  if (!file.exists(sacred_path)) return(NULL)
  e <- new.env()
  load(sacred_path, envir = e)
  obj_name <- ls(e)[1]
  ss <- get(obj_name, envir = e)
  if (verbose) cat(sprintf("  Loaded %d sacred sites\n", nrow(ss)))
  ss
}

#' Load roads as sf.
load_roads <- function(roads_path, verbose = TRUE) {
  if (!file.exists(roads_path)) {
    if (verbose) cat("  Roads file not found:", roads_path, "\n")
    return(NULL)
  }
  r <- readRDS(roads_path)
  if (verbose) cat(sprintf("  Loaded %d road segments\n", nrow(r)))
  r
}

#' Load hex hierarchy (list of sf, one per tier).
load_hex_tiers <- function(hex_path, verbose = TRUE) {
  if (!file.exists(hex_path)) return(NULL)
  e <- new.env()
  load(hex_path, envir = e)
  obj_name <- ls(e)[1]
  hh <- get(obj_name, envir = e)
  if (verbose) cat(sprintf("  Loaded hex hierarchy (%d tiers)\n", length(hh)))
  hh
}

#' Load custom annotations from GeoJSON, split into POIs vs other features.
#'
#' POI = feature_type == "poi" with optional inkarnate_path field.
load_custom_features <- function(features_path, verbose = TRUE) {
  result <- list(features = NULL, pois = NULL)
  if (!file.exists(features_path)) {
    return(result)
  }
  cf <- tryCatch(st_read(features_path, quiet = TRUE),
                 error = function(e) { warning(e$message); NULL })
  if (is.null(cf) || nrow(cf) == 0) return(result)

  # Backfill the visibility flag — older geojsons predate the field.
  if (!"hidden" %in% names(cf)) cf$hidden <- FALSE
  cf$hidden <- as.logical(cf$hidden)
  cf$hidden[is.na(cf$hidden)] <- FALSE

  is_poi <- !is.na(cf$feature_type) & cf$feature_type == "poi"
  is_point <- as.character(st_geometry_type(cf)) %in% c("POINT", "MULTIPOINT")

  result$pois <- cf[is_poi & is_point, , drop = FALSE]
  result$features <- cf[!is_poi, , drop = FALSE]
  if (verbose) {
    cat(sprintf("  Loaded %d annotated features (%d POIs)\n",
                nrow(result$features), nrow(result$pois)))
  }
  result
}

#' Best-effort match: settlement name → Inkarnate jpg basename.
match_inkarnate_jpgs <- function(settlements, inkarnate_dir, verbose = TRUE) {
  if (!dir.exists(inkarnate_dir)) return(settlements)
  files <- list.files(inkarnate_dir, pattern = "\\.(jpg|jpeg|png)$",
                      ignore.case = TRUE, full.names = FALSE)
  if (length(files) == 0) return(settlements)

  # Map normalised key -> filename
  norm <- function(x) tolower(gsub("[^a-z0-9]", "", x))
  file_keys <- norm(tools::file_path_sans_ext(files))
  file_map <- setNames(files, file_keys)

  # Only fill where inkarnate is empty
  empty <- is.na(settlements$inkarnate) | !nzchar(settlements$inkarnate)
  if (any(empty)) {
    name_keys <- norm(settlements$name[empty])
    matches <- file_map[name_keys]
    settlements$inkarnate[empty] <- unname(matches)
    n_matched <- sum(!is.na(matches))
    if (verbose && n_matched > 0)
      cat(sprintf("  Matched %d settlements to Inkarnate maps by name\n", n_matched))
  }
  settlements
}

#' Compute a simple centroid label per continent (faded background labels).
extract_continent_labels <- function(continents_gpkg, verbose = TRUE) {
  if (!file.exists(continents_gpkg)) return(NULL)
  cp <- tryCatch(st_read(continents_gpkg, quiet = TRUE), error = function(e) NULL)
  if (is.null(cp) || nrow(cp) == 0) return(NULL)
  name_col <- intersect(c("name", "continent", "continent_name"), names(cp))[1]
  if (is.na(name_col)) return(NULL)
  # Toggle s2 off for centroid — continent polygons may have invalid loops
  prev_s2 <- sf_use_s2()
  on.exit(suppressMessages(sf_use_s2(prev_s2)), add = TRUE)
  suppressMessages(sf_use_s2(FALSE))
  centroids <- suppressWarnings(st_centroid(cp))
  coords <- st_coordinates(centroids)
  data.frame(name = cp[[name_col]], lon = coords[, 1], lat = coords[, 2],
             stringsAsFactors = FALSE)
}

# =============================================================================
# JSON ENCODERS
# =============================================================================

#' Convert sf to a compact GeoJSON string suitable for inlining.
sf_to_geojson_string <- function(sf_obj) {
  if (is.null(sf_obj) || nrow(sf_obj) == 0) return("null")
  tmp <- tempfile(fileext = ".geojson")
  on.exit(unlink(tmp))
  st_write(sf_obj, tmp, driver = "GeoJSON", delete_dsn = TRUE, quiet = TRUE,
           layer_options = c("RFC7946=YES", "WRITE_BBOX=NO"))
  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

#' Population thresholds defining settlement tiers. Higher tier = larger.
#' Tuned to the post-redistribution distribution (May 2026):
#'   Tier 5 (City):       pop ≥ 10,000  (~47)
#'   Tier 4 (Town):       pop ≥  5,000  (~346 cumulative)
#'   Tier 3 (Village):    pop ≥  2,000  (~798)
#'   Tier 2 (Hamlet):     pop ≥    200  (~942)
#'   Tier 1 (Settlement): anything else (~1,300; mostly promoted junctions)
SETTLEMENT_TIER_BREAKS <- c(200, 2000, 5000, 10000)
SETTLEMENT_TIER_LABELS <- c("settlement","hamlet","village","town","city")

settlement_tier <- function(pop) {
  pop[is.na(pop)] <- 0
  as.integer(findInterval(pop, SETTLEMENT_TIER_BREAKS) + 1L)
}

#' Settlements → flat record array (lighter than GeoJSON for points).
#'
#' `continent_rank` ranks each settlement within its continent by population,
#' descending (1 = largest in continent). The map's JS uses this directly
#' to keep the visible set tiny at low zooms — top 1 per continent at world
#' view, top 3 at z=3, etc. — instead of relying on absolute pop tiers
#' that always show all 43 cities (way too many at world scale).
settlements_to_records <- function(s) {
  if (is.null(s) || nrow(s) == 0) return("[]")
  pop <- as.numeric(s$population %||% 0)
  pop[is.na(pop)] <- 0
  cont <- s$continent %||% rep(NA_character_, nrow(s))
  cont_for_rank <- ifelse(is.na(cont) | !nzchar(cont), "_unassigned", cont)
  ord <- order(cont_for_rank, -pop, s$settlement_id)
  rk  <- integer(nrow(s))
  rk[ord] <- ave(seq_along(ord), cont_for_rank[ord], FUN = seq_along)
  recs <- data.frame(
    id              = s$settlement_id,
    name            = s$name,
    lon             = round(s$lon, 5),
    lat             = round(s$lat, 5),
    population      = round(pop),
    tier            = settlement_tier(pop),
    continent_rank  = rk,
    type            = s$type %||% NA_character_,
    continent       = cont,
    coastal         = as.integer(s$coastal %||% 0) == 1,
    on_river        = as.integer(s$on_river %||% 0) == 1,
    is_port         = as.logical(s$is_port %||% FALSE),
    sacred_tier     = s$sacred_tier %||% NA_integer_,
    degree          = s$degree %||% 0,
    inkarnate       = s$inkarnate,
    has_inkarnate   = !is.na(s$inkarnate) & nzchar(s$inkarnate),
    stringsAsFactors = FALSE
  )
  toJSON(recs, na = "null", auto_unbox = TRUE, dataframe = "rows")
}

#' Sacred sites → flat record array.
sacred_to_records <- function(ss) {
  if (is.null(ss) || nrow(ss) == 0) return("[]")
  coords <- st_coordinates(ss)
  recs <- data.frame(
    id        = ss$site_id,
    tier      = as.integer(ss$tier),
    lon       = round(coords[, 1], 5),
    lat       = round(coords[, 2], 5),
    talisman  = ss$talisman %||% NA_character_,
    status    = ss$status %||% NA_character_,
    quote     = ss$quote %||% NA_character_,
    stringsAsFactors = FALSE
  )
  toJSON(recs, na = "null", auto_unbox = TRUE, dataframe = "rows")
}

#' Custom POIs → flat record array.
pois_to_records <- function(pois_sf) {
  if (is.null(pois_sf) || nrow(pois_sf) == 0) return("[]")
  coords <- st_coordinates(pois_sf)
  hidden_vec <- if ("hidden" %in% names(pois_sf)) pois_sf$hidden else FALSE
  hidden_vec <- as.logical(hidden_vec)
  hidden_vec[is.na(hidden_vec)] <- FALSE
  recs <- data.frame(
    id          = pois_sf$feature_id %||% seq_len(nrow(pois_sf)),
    name        = pois_sf$name %||% "",
    lon         = round(coords[, 1], 5),
    lat         = round(coords[, 2], 5),
    description = pois_sf$description %||% "",
    inkarnate   = pois_sf$inkarnate_path %||% NA_character_,
    hidden      = hidden_vec,
    stringsAsFactors = FALSE
  )
  toJSON(recs, na = "null", auto_unbox = TRUE, dataframe = "rows")
}

#' Hex tiers → keyed object {1: <geojson>, 2: <geojson>, ...}.
hex_tiers_to_json <- function(hex_list) {
  if (is.null(hex_list) || length(hex_list) == 0) return("{}")
  parts <- character(0)
  for (i in seq_along(hex_list)) {
    tier <- hex_list[[i]]
    if (is.null(tier) || nrow(tier) == 0) next
    parts <- c(parts, sprintf('"%d": %s', i, sf_to_geojson_string(tier)))
  }
  paste0("{", paste(parts, collapse = ","), "}")
}

#' Write each hex tier to its own GeoJSON file under <output_dir>/data/ so
#' the JS side can lazy-fetch on demand instead of inlining all 9 tiers
#' (~7 MB) into the page payload. Returns a JS object literal that the
#' template inlines as `window.HEX_TIER_FILES` — just a tier→filename map.
write_hex_tier_files <- function(hex_list, output_dir, verbose = TRUE) {
  data_dir <- file.path(output_dir, "data")
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  manifest <- character(0)
  total_bytes <- 0
  for (i in seq_along(hex_list)) {
    tier <- hex_list[[i]]
    if (is.null(tier) || nrow(tier) == 0) next
    fname <- sprintf("hex_tier_%d.json", i)
    fpath <- file.path(data_dir, fname)
    js <- sf_to_geojson_string(tier)
    writeBin(charToRaw(js), fpath)
    total_bytes <- total_bytes + nchar(js, type = "bytes")
    manifest <- c(manifest, sprintf('"%d": "data/%s"', i, fname))
  }
  if (verbose)
    cat(sprintf("    hex tiers (lazy)   : %d files, %.1f KB total off-page\n",
                length(manifest), total_bytes / 1024))
  paste0("{", paste(manifest, collapse = ","), "}")
}

# Helper: NULL coalesce, like SQL COALESCE
`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  a
}

# =============================================================================
# HTML RENDERER
# =============================================================================

render_reference_html <- function(template_path,
                                  output_path,
                                  data_payload,
                                  title = "The Realm of EART-H",
                                  subtitle = "from the hand of C. Ezra Stiles, peripatetic cartographer",
                                  map_config = list(),
                                  verbose = TRUE) {
  if (!file.exists(template_path)) stop("Template not found: ", template_path)
  tmpl <- paste(readLines(template_path, warn = FALSE), collapse = "\n")

  default_cfg <- list(
    center  = c(20, 0),
    zoom    = 2,
    minZoom = 1,
    # maxZoom 14 = procedural ceiling. elevationMax doubles as the tileLayer's
    # maxNativeZoom: setting it to 14 makes Leaflet REQUEST z9-14 tiles (served
    # procedurally by Functions/serve_tiles()) instead of upscaling z8. When the
    # page is served by the plain http.server (no tile server), z>8 will 404 and
    # Leaflet falls back to the z8 tile, so this is safe either way.
    maxZoom = 14,
    tile    = list(elevationMax = 14, biomeMax = 6, riversMax = 6)
  )
  cfg <- modifyList(default_cfg, map_config)
  cfg_json <- toJSON(cfg, auto_unbox = TRUE)

  # Sacred tier dropdown options (1 = include all sites, 9 = only golden)
  sacred_opts <- paste(sapply(1:9, function(i) {
    sel <- if (i == 5) " selected" else ""
    sprintf('<option value="%d"%s>%d (%s)</option>',
            i, sel, i,
            c("granite tablet","quartz pedestal","sandstone arch","marble basin",
              "iron cube","copper obelisk","bronze brazier","silver altar",
              "golden fountain")[i])
  }), collapse = "\n")

  hex_opts <- paste(sapply(1:9, function(i) {
    sprintf('<option value="%d">Tier %d</option>', i, i)
  }), collapse = "\n")

  out <- tmpl
  out <- gsub("{{TITLE}}", title, out, fixed = TRUE)
  out <- gsub("{{SUBTITLE}}", subtitle, out, fixed = TRUE)
  out <- gsub("{{MAP_CONFIG}}", cfg_json, out, fixed = TRUE)
  out <- gsub("{{SACRED_TIER_OPTIONS}}", sacred_opts, out, fixed = TRUE)
  out <- gsub("{{HEX_TIER_OPTIONS}}", hex_opts, out, fixed = TRUE)
  out <- gsub("{{SETTLEMENTS_JSON}}", data_payload$settlements, out, fixed = TRUE)
  out <- gsub("{{ROADS_JSON}}",       data_payload$roads,       out, fixed = TRUE)
  out <- gsub("{{SACRED_JSON}}",      data_payload$sacred,      out, fixed = TRUE)
  out <- gsub("{{HEX_TIERS_JSON}}",   data_payload$hex_tiers,   out, fixed = TRUE)
  out <- gsub("{{HEX_TIER_FILES_JSON}}",
              data_payload$hex_tier_files %||% "{}", out, fixed = TRUE)
  out <- gsub("{{FEATURES_JSON}}",    data_payload$features,    out, fixed = TRUE)
  out <- gsub("{{POIS_JSON}}",        data_payload$pois,        out, fixed = TRUE)
  out <- gsub("{{CONTINENT_LABELS_JSON}}", data_payload$continents, out, fixed = TRUE)
  out <- gsub("{{RIVERS_JSON}}",      data_payload$rivers %||% "{\"type\":\"FeatureCollection\",\"features\":[]}",
              out, fixed = TRUE)

  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(out, output_path, useBytes = TRUE)
  if (verbose) {
    fsize <- file.info(output_path)$size
    cat(sprintf("  Wrote %s (%.1f MB)\n", output_path, fsize / 1024 / 1024))
  }
  invisible(output_path)
}

# =============================================================================
# PLAYER MAP DEPLOY BUNDLE
# =============================================================================

#' Assemble a self-contained, uploadable copy of the player map.
#'
#' Map/player/index.html is built to sit inside Map/ and reaches its assets with
#' "../tiles/", "../data/", "../inkarnate/". A static host needs everything under
#' one root, so this derives a flat bundle from the already-built player page
#' rather than re-running the (expensive) data assembly -- which also guarantees
#' the bundle can never drift from the page it was made from.
#'
#' Two things are deliberately NOT a straight directory copy:
#'
#'   * Only the Inkarnate images the player page actually links are included.
#'     Map/inkarnate/ also holds the images of hidden POIs, which are stripped
#'     from the player data. Copying the folder would leave those publicly
#'     fetchable by URL and undo the whole point of stripping them at build time.
#'   * River tiers whose min_zoom exceeds the bundle's ceiling are skipped;
#'     rivers_small (min_zoom 8) is 5.4 MB that a zoom-7 bundle can never load.
#'
#' @param max_zoom Highest tile zoom to ship. 7 keeps the bundle under
#'   Cloudflare Pages' 20,000-file cap; 8 needs Netlify or GitHub Pages.
build_player_bundle <- function(
    map_dir   = map_root(),
    out_dir   = map_path("player-bundle", require_exists = FALSE),
    max_zoom  = 7,
    file_cap  = 20000L,
    verbose   = TRUE
) {
  src_html <- file.path(map_dir, "player", "index.html")
  if (!file.exists(src_html))
    stop("Player map not found: ", src_html,
         "\n  Run build_reference_map() first.")

  if (verbose) {
    cat("\n=========================================\n")
    cat(" PLAYER MAP DEPLOY BUNDLE\n")
    cat("=========================================\n")
    cat("Source :", src_html, "\n")
    cat("Output :", out_dir, "\n")
    cat("Zoom   : 0 -", max_zoom, "\n\n")
  }

  if (dir.exists(out_dir)) unlink(out_dir, recursive = TRUE)
  dir.create(file.path(out_dir, "data"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(out_dir, "inkarnate"), recursive = TRUE, showWarnings = FALSE)

  html <- paste(readLines(src_html, warn = FALSE), collapse = "\n")

  # ---- 1. flatten asset paths --------------------------------------------
  html <- gsub('"../tiles/',     '"tiles/',     html, fixed = TRUE)
  html <- gsub('"../data/',      '"data/',      html, fixed = TRUE)
  html <- gsub('"../inkarnate/', '"inkarnate/', html, fixed = TRUE)

  # ---- 2. clamp TILE FETCHING to what we actually ship ---------------------
  # elevationMax is the tileLayer's maxNativeZoom: above it Leaflet upscales the
  # deepest real tile rather than requesting one that was never uploaded (which
  # is what produced grey squares). maxZoom is left higher on purpose so a
  # player can still zoom in far enough to read a small settlement — soft
  # imagery, sharp labels, and the vector roads and rivers stay crisp because
  # the template ties VECTOR_FEATURE_MAX to maxZoom when elevationMax <= 8.
  cfg_rx <- "window\\.MAP_CONFIG\\s*=\\s*(\\{.*?\\});"
  m <- regmatches(html, regexpr(cfg_rx, html))
  if (length(m)) {
    cfg <- jsonlite::fromJSON(sub(cfg_rx, "\\1", m))
    cfg$maxZoom           <- max_zoom + 3L
    cfg$tile$elevationMax <- max_zoom
    cfg$tile$biomeMax     <- min(cfg$tile$biomeMax  %||% max_zoom, max_zoom)
    cfg$tile$riversMax    <- min(cfg$tile$riversMax %||% max_zoom, max_zoom)
    html <- sub(cfg_rx,
                paste0("window.MAP_CONFIG = ",
                       jsonlite::toJSON(cfg, auto_unbox = TRUE), ";"),
                html)
  }

  # ---- 3. which river tiers can this zoom ever load? ----------------------
  keep_rivers <- character(0)
  mm <- regmatches(html, regexpr("window\\.RIVERS_MANIFEST\\s*=\\s*(\\{.*?\\});", html))
  if (length(mm)) {
    man <- jsonlite::fromJSON(sub("window\\.RIVERS_MANIFEST\\s*=\\s*(\\{.*?\\});", "\\1", mm))
    for (tier in names(man)) {
      mz <- man[[tier]]$min_zoom %||% 0
      if (mz <= max_zoom) keep_rivers <- c(keep_rivers, basename(man[[tier]]$url))
      else if (verbose) cat(sprintf("  skip %-22s (min_zoom %s > %s)\n",
                                    basename(man[[tier]]$url), mz, max_zoom))
    }
  }

  writeLines(html, file.path(out_dir, "index.html"), useBytes = TRUE)

  # ---- 4. data files the page names, minus unreachable river tiers --------
  wanted <- unique(unlist(regmatches(
    html, gregexpr('(?<=")data/[A-Za-z0-9_\\.]+', html, perl = TRUE))))
  wanted <- basename(wanted)
  all_rivers <- grep("^rivers_", wanted, value = TRUE)
  wanted <- setdiff(wanted, setdiff(all_rivers, keep_rivers))

  n_data <- 0L
  for (f in wanted) {
    src <- file.path(map_dir, "data", f)
    if (file.exists(src)) { file.copy(src, file.path(out_dir, "data", f)); n_data <- n_data + 1L }
    else if (verbose) cat("  (referenced but absent:", f, ")\n")
  }

  # ---- 5. ONLY the linked Inkarnate images --------------------------------
  avail <- list.files(file.path(map_dir, "inkarnate"), full.names = FALSE)
  linked <- avail[vapply(avail, function(a) grepl(a, html, fixed = TRUE), TRUE)]
  for (f in linked) file.copy(file.path(map_dir, "inkarnate", f),
                              file.path(out_dir, "inkarnate", f))
  excluded <- setdiff(avail, linked)
  if (verbose && length(excluded))
    cat(sprintf("  withheld %d unlinked Inkarnate image(s): %s\n",
                length(excluded), paste(excluded, collapse = ", ")))

  # ---- 6. tiles -----------------------------------------------------------
  n_tiles <- 0L
  for (z in 0:max_zoom) {
    zsrc <- file.path(map_dir, "tiles", "elevation", z)
    if (!dir.exists(zsrc)) next
    zdst <- file.path(out_dir, "tiles", "elevation", z)
    dir.create(zdst, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(zsrc, full.names = TRUE), zdst, recursive = TRUE)
    k <- length(list.files(zdst, recursive = TRUE))
    n_tiles <- n_tiles + k
    if (verbose) cat(sprintf("  z%-2d %6d tiles\n", z, k))
  }

  # ---- 7. report ----------------------------------------------------------
  files <- list.files(out_dir, recursive = TRUE, full.names = TRUE)
  total_mb <- sum(file.size(files)) / 1024 / 1024
  if (verbose) {
    cat(sprintf("\n  index.html   1 file\n  data/       %2d files\n  inkarnate/  %2d files\n  tiles/  %6d files\n",
                n_data, length(linked), n_tiles))
    cat(sprintf("\n  TOTAL: %s files, %.1f MB\n", format(length(files), big.mark = ","), total_mb))
    if (length(files) > file_cap)
      cat(sprintf("  ** %s files exceeds the %s cap (Cloudflare Pages). Use Netlify/GitHub Pages, or lower max_zoom. **\n",
                  format(length(files), big.mark = ","), format(file_cap, big.mark = ",")))
    else
      cat(sprintf("  Within the %s-file cap — deployable to Cloudflare Pages, Netlify or GitHub Pages.\n",
                  format(file_cap, big.mark = ",")))
    cat("\n  Upload the CONTENTS of", out_dir, "as the site root.\n\n")
  }

  invisible(list(dir = out_dir, files = length(files), mb = total_mb,
                 tiles = n_tiles, inkarnate_included = linked,
                 inkarnate_withheld = excluded))
}

# =============================================================================
# LAUNCHER SCRIPTS
# =============================================================================

create_map_launchers <- function(output_dir, verbose = TRUE) {
  # start-map.bat serves the PLAYER map (Map/player/index.html) over a plain
  # python http server. Two properties matter and are the whole point of keeping
  # this launcher separate from start-tileserver.bat:
  #
  #   1. A static server has no procedural engine, so it can never synthesize a
  #      tile and never writes into Map/tiles/. Handing the table this launcher
  #      cannot create new canon.
  #   2. The player page it opens has hidden POIs stripped at BUILD time, so the
  #      spoilers are absent from the file, not merely unrendered.
  #
  # The GM map (Map/index.html, zoom to 14, hidden POIs visible) is reached via
  # Map/start-tileserver.bat, which is the only launcher that writes tiles.
  #
  # The cache-buster (?t=...) is essential — the browser otherwise serves a
  # stale index.html on every launch because the URL never changes.
  bat_path <- file.path(output_dir, "start-map.bat")
  bat <- c(
    "@echo off",
    "REM Launch the EART-H PLAYER map (no hidden POIs, zoom capped at 8).",
    "REM Static server: cannot synthesize tiles, never writes to Map\\tiles.",
    "REM For the GM map use start-tileserver.bat instead.",
    "cd /d \"%~dp0\"",
    "if not exist \"player\\index.html\" (",
    "  echo Player map not found. Run build_reference_map^(^) to generate it.",
    "  pause",
    "  exit /b 1",
    ")",
    "for /f %%i in ('powershell -NoProfile -Command \"[int][double]::Parse((Get-Date -UFormat %%s))\"') do set T=%%i",
    "echo Starting EART-H PLAYER map on http://localhost:8765/player/",
    "start \"\" \"http://localhost:8765/player/index.html?t=%T%\"",
    "python -m http.server 8765"
  )
  writeLines(bat, bat_path)

  ps1_path <- file.path(output_dir, "start-map.ps1")
  ps1 <- c(
    "# Launch the EART-H PLAYER map (no hidden POIs, zoom capped at 8).",
    "# Static server only: never synthesizes or writes tiles.",
    "# For the GM map use start-tileserver.bat instead.",
    "$ErrorActionPreference = 'Stop'",
    "Set-Location -Path $PSScriptRoot",
    "if (-not (Test-Path 'player/index.html')) {",
    "  Write-Host 'Player map not found. Run build_reference_map() to generate it.'",
    "  exit 1",
    "}",
    "$t = [int][double]::Parse((Get-Date -UFormat %s))",
    "Start-Process \"http://localhost:8765/player/index.html?t=$t\"",
    "python -m http.server 8765"
  )
  writeLines(ps1, ps1_path)

  if (verbose) cat("  Wrote launchers: start-map.bat, start-map.ps1 (player map)\n")
  invisible(c(bat_path, ps1_path))
}

# =============================================================================
# PLAYER MAP
# =============================================================================

#' Strip everything flagged hidden out of a rendered data payload.
#'
#' Operates on the JSON strings the template substitutes, so the spoilers never
#' reach the player file at all. A runtime toggle would leave them sitting in
#' window.POIS for anyone who opens View Source.
strip_hidden_from_payload <- function(payload, verbose = TRUE) {
  n_poi <- 0L; n_feat <- 0L

  if (!is.null(payload$pois) && nzchar(payload$pois)) {
    recs <- tryCatch(jsonlite::fromJSON(payload$pois, simplifyDataFrame = FALSE),
                     error = function(e) NULL)
    if (!is.null(recs)) {
      keep <- Filter(function(p) !isTRUE(p$hidden), recs)
      n_poi <- length(recs) - length(keep)
      payload$pois <- jsonlite::toJSON(keep, auto_unbox = TRUE, na = "null")
    }
  }

  if (!is.null(payload$features) && nzchar(payload$features)) {
    fc <- tryCatch(jsonlite::fromJSON(payload$features, simplifyDataFrame = FALSE),
                   error = function(e) NULL)
    if (!is.null(fc) && !is.null(fc$features)) {
      keep <- Filter(function(f) !isTRUE(f$properties$hidden), fc$features)
      n_feat <- length(fc$features) - length(keep)
      fc$features <- keep
      payload$features <- jsonlite::toJSON(fc, auto_unbox = TRUE, na = "null")
    }
  }

  if (verbose)
    cat(sprintf("  Stripped %d hidden POI(s) and %d hidden feature(s)\n", n_poi, n_feat))
  attr(payload, "stripped") <- c(pois = n_poi, features = n_feat)
  payload
}

#' Render the player-facing map into <output_dir>/player/index.html.
#'
#' Differences from the GM map, all applied at build time:
#'   * hidden POIs and hidden features are absent from the file
#'   * the GM/Player toggle is removed and window.gmView pinned to false, so it
#'     cannot be switched back on from the console
#'   * zoom is capped at the static pyramid's ceiling, so the page never asks
#'     for a tile that only the procedural server could produce
#'   * tile URLs are rewritten to ../tiles/ since the page lives one level down
render_player_html <- function(template_path,
                               output_dir,
                               data_payload,
                               title,
                               subtitle,
                               tile_max_zoom = 8,
                               verbose = TRUE) {
  if (verbose) cat("\nSTEP — player map\n")
  payload <- strip_hidden_from_payload(data_payload, verbose = verbose)

  player_dir <- file.path(output_dir, "player")
  dir.create(player_dir, recursive = TRUE, showWarnings = FALSE)
  out_path <- file.path(player_dir, "index.html")

  render_reference_html(
    template_path = template_path,
    output_path   = out_path,
    data_payload  = payload,
    title         = title,
    subtitle      = subtitle,
    # Tile fetching stops at the static ceiling — nothing above tile_max_zoom
    # exists without the procedural server. But maxZoom is deliberately set
    # HIGHER: Leaflet then upscales the deepest real tile instead of refusing
    # to zoom, so a player can push in far enough to read a small settlement
    # like a hamlet. The imagery goes soft; the markers and labels stay sharp,
    # and not one extra tile is shipped.
    map_config    = list(maxZoom = tile_max_zoom + 3L,
                         tile = list(elevationMax = tile_max_zoom,
                                     biomeMax = min(6L, tile_max_zoom),
                                     riversMax = min(6L, tile_max_zoom))),
    verbose       = FALSE
  )

  html <- paste(readLines(out_path, warn = FALSE), collapse = "\n")

  # Remove the GM toggle control entirely. The (?s) flag is required: the label
  # spans four lines and "." does not cross newlines without it.
  html <- sub('(?s)<label class="layer-row" id="gm-toggle-row".*?</label>', "",
              html, perl = TRUE)
  # Pin player view. The toggle handler is defensive about a missing element
  # (it was NOT, until 2026-08 — the dead reference threw and aborted the rest
  # of init, which left the loading overlay stuck on screen), but pinning the
  # flag means even a console poke can't reveal anything: there is nothing left
  # in the payload to reveal.
  html <- sub("window.gmView = true;", "window.gmView = false;  // player build",
              html, fixed = TRUE)

  # Drop the sacred-hex tier selector. It is a GM analysis tool, not something
  # a player needs, and each tier is a multi-megabyte lazy fetch. Anchored on
  # the "Hex tier" caption so the settlement and sacred tier selectors survive.
  html <- sub('(?s)<div class="tier-select-row"><span>Hex tier</span>.*?</div>\\s*</div>',
              "</div>", html, perl = TRUE)
  # ...and empty the lazy-load manifest. Removing the control alone is not
  # enough: build_player_bundle() copies every "data/..." path it finds in the
  # HTML, so the nine hex_tier_*.json files (6.8 MB) were still being shipped
  # for a layer the player can no longer reach. renderHex() already tolerates
  # an empty manifest, and it is now unreachable in this build anyway.
  html <- sub("window\\.HEX_TIER_FILES\\s*=\\s*\\{.*?\\};",
              "window.HEX_TIER_FILES = {};  // player build: hex layer removed",
              html, perl = TRUE)
  html <- sub("window\\.HEX_TIERS\\s*=\\s*\\{.*?\\};",
              "window.HEX_TIERS = {};  // player build: hex layer removed",
              html, perl = TRUE)
  # The page sits in player/, so tiles are one level up.
  html <- gsub('"tiles/', '"../tiles/', html, fixed = TRUE)
  # Lazy-loaded side files (hex tiers, river tiers) live alongside the GM map.
  html <- gsub('"data/', '"../data/', html, fixed = TRUE)
  html <- gsub('"inkarnate/', '"../inkarnate/', html, fixed = TRUE)

  writeLines(html, out_path, useBytes = TRUE)

  if (verbose) {
    cat(sprintf("  Wrote %s (%.1f MB)\n", out_path, file.info(out_path)$size / 1024 / 1024))
    cat(sprintf("  Zoom capped at %d; GM toggle removed; gmView pinned false\n", tile_max_zoom))
  }
  invisible(out_path)
}

# =============================================================================
# INKARNATE COPY
# =============================================================================

copy_inkarnate_maps <- function(src_dir, dst_dir, verbose = TRUE) {
  if (!dir.exists(src_dir)) return(invisible(character(0)))
  dir.create(dst_dir, recursive = TRUE, showWarnings = FALSE)
  files <- list.files(src_dir, pattern = "\\.(jpg|jpeg|png)$",
                      ignore.case = TRUE, full.names = TRUE)
  if (length(files) == 0) return(invisible(character(0)))
  copied <- file.copy(files, dst_dir, overwrite = TRUE)
  if (verbose) cat(sprintf("  Copied %d Inkarnate maps to %s\n", sum(copied), dst_dir))
  invisible(file.path(dst_dir, basename(files)))
}

# =============================================================================
# MAIN ENTRY POINT
# =============================================================================

#' Build the standalone EART-H reference map.
#'
#' Produces:
#'   <output_dir>/index.html      — the map page (data inlined)
#'   <output_dir>/inkarnate/      — copied detail-map jpgs
#'   <output_dir>/start-map.bat   — one-click launcher
#'   <output_dir>/start-map.ps1   — PowerShell launcher
#'   <output_dir>/tiles/          — XYZ tile pyramid (only rebuilt if rebuild_tiles=TRUE)
#'
#' @param output_dir Directory under which to assemble the map artefact.
#' @param settlements_path Path to Combined/settlements_final.rds (post-NameBuilder + apply_annotations_to_canon).
#' @param sacred_sites_path sacred_sites_attributed.RData.
#' @param hex_hierarchy_path hex_hierarchy_sf.RData.
#' @param roads_path road_routes.rds.
#' @param custom_features_path custom_features.geojson (drawn rivers / POIs).
#' @param continents_gpkg continent_polygons.gpkg for faded continent labels.
#' @param inkarnate_dir Source Inkarnate Maps directory.
#' @param elevation_path Elevation VRT (only used if rebuild_tiles=TRUE).
#' @param biome_path Biome VRT (only used if rebuild_tiles=TRUE).
#' @param rivers_path Optional rivers raster (only used if rebuild_tiles=TRUE).
#' @param rebuild_tiles If TRUE, regenerate the elevation/biome/rivers tile pyramid.
#' @param tile_max_zoom Max native zoom for new tile pyramids.
#' @param title,subtitle Cartouche text.
#' @param verbose Print progress.
#' @export
build_reference_map <- function(
    # require_exists = FALSE: this is the one caller allowed to CREATE the map.
    output_dir            = map_root(require_exists = FALSE),
    settlements_path      = here::here("Input Data/Combined/settlements_final.rds"),
    sacred_sites_path     = here::here("Input Data/Divinity/sacred_sites_attributed.RData"),
    hex_hierarchy_path    = here::here("Input Data/Divinity/hex_hierarchy_sf.RData"),
    roads_path            = here::here("Input Data/Roads/road_routes.rds"),
    custom_features_path  = here::here("Input Data/Annotations/custom_features.geojson"),
    continents_gpkg       = here::here("Input Data/continent_polygons.gpkg"),
    inkarnate_dir         = here::here("Input Data/Inkarnate Maps"),
    elevation_path        = here::here("Input Data/HighResolution/elevation.vrt"),
    biome_path            = here::here("Input Data/HighResolution/biome.vrt"),
    rivers_path           = NULL,
    rebuild_tiles         = FALSE,
    tile_max_zoom         = 8,
    # Also emit Map/player/index.html — spoiler-free, zoom-capped, static-safe.
    build_player          = TRUE,
    title                 = "The Realm of EART-H",
    subtitle              = "from the hand of C. Ezra Stiles, peripatetic cartographer",
    template_path         = here::here("Functions/map_template.html"),
    verbose               = TRUE
) {
  if (verbose) {
    cat("\n=========================================\n")
    cat(" BUILDING EART-H REFERENCE MAP\n")
    cat("=========================================\n\n")
    cat("Output dir:", output_dir, "\n\n")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  # ----- TILES (optional) -----
  if (rebuild_tiles) {
    if (verbose) cat("STEP — tile generation\n")
    gdal <- .require_gdal()   # fail now, not an hour in
    if (file.exists(elevation_path)) {
      # Hillshade composite: gdaldem hillshade + color-relief, blended.
      # We tile the composite RGB raster directly (no color_file) so the
      # shaded relief is baked into every tile of the pyramid.
      hs_composite_path <- file.path(output_dir, "hillshade_composite.tif")
      water_vrt_path <- here::here("Input Data/HighResolution/water_mask.vrt")
      generate_hillshade_composite(
        elevation_path  = elevation_path,
        water_vrt_path  = if (file.exists(water_vrt_path)) water_vrt_path else NULL,
        output_path     = hs_composite_path,
        z_factor        = 3,
        azimuth         = 315,
        altitude        = 45,
        blend           = 0.55,
        force           = TRUE,
        gdal            = gdal,
        verbose         = verbose
      )
      generate_xyz_tiles(hs_composite_path, output_dir, "elevation",
                         max_zoom = tile_max_zoom, color_file = NULL,
                         force = TRUE, gdal = gdal, verbose = verbose)
      # Keep a colors.txt for reference / inspection only.
      elev_color_file <- file.path(output_dir, "elevation_colors.txt")
      create_gdal_color_file(ELEVATION_PALETTE$breaks, ELEVATION_PALETTE$colors,
                             elev_color_file, verbose = FALSE)
    }
    if (file.exists(biome_path)) {
      biome_color_file <- file.path(output_dir, "biome_colors.txt")
      create_gdal_color_file(0:8,
        c("#08519c","#f7fbff","#2ca25f","#fee391","#c7e9b4",
          "#238b45","#00441b","#737373","#d0d1e6"),
        biome_color_file, verbose = verbose)
      generate_xyz_tiles(rast(biome_path), output_dir, "biome",
                         max_zoom = tile_max_zoom, color_file = biome_color_file,
                         resampling = "near", force = TRUE, gdal = gdal, verbose = verbose)
    }
    if (!is.null(rivers_path) && file.exists(rivers_path)) {
      rivers_color_file <- file.path(output_dir, "rivers_colors.txt")
      create_gdal_color_file(c(0, 0.5, 1),
                             c("#00000000", "#2166ac", "#2166ac"),
                             rivers_color_file, verbose = verbose)
      generate_xyz_tiles(rast(rivers_path), output_dir, "rivers",
                         max_zoom = tile_max_zoom, color_file = rivers_color_file,
                         force = TRUE, gdal = gdal, verbose = verbose)
    }
    cat("\n")
  } else {
    if (verbose) {
      tile_count <- length(list.files(file.path(output_dir, "tiles"),
                                       pattern = "\\.png$", recursive = TRUE))
      cat(sprintf("STEP — tiles: keeping existing %d-tile pyramid (rebuild_tiles=FALSE)\n\n",
                  tile_count))
    }
  }

  # ----- DATA -----
  if (verbose) cat("STEP — loading vector data\n")
  settlements <- load_settlements(settlements_path, verbose = verbose)
  settlements <- match_inkarnate_jpgs(settlements, inkarnate_dir, verbose = verbose)
  sacred      <- load_sacred_sites(sacred_sites_path, verbose = verbose)
  hex_tiers   <- load_hex_tiers(hex_hierarchy_path, verbose = verbose)
  roads       <- load_roads(roads_path, verbose = verbose)
  cf          <- load_custom_features(custom_features_path, verbose = verbose)
  continents  <- extract_continent_labels(continents_gpkg, verbose = verbose)
  cat("\n")

  # ----- INKARNATE COPY -----
  if (verbose) cat("STEP — copying Inkarnate detail maps\n")
  copy_inkarnate_maps(inkarnate_dir, file.path(output_dir, "inkarnate"),
                      verbose = verbose)
  cat("\n")

  # ----- ENCODE -----
  if (verbose) cat("STEP — encoding inline payload\n")
  hex_manifest <- write_hex_tier_files(hex_tiers, output_dir, verbose = verbose)

  # Vector rivers: vectorise per-continent 2-point downstream segments,
  # chain into multi-point LINESTRINGs (sf::st_line_merge), tier by flow,
  # write three external GeoJSONs (big/med/small). The template lazy-loads
  # them based on zoom level.
  rivers_cache <- file.path(output_dir, "data", "rivers_vector.rds")
  rivers_tier_urls <- list(
    big   = "data/rivers_big.geojson",
    med   = "data/rivers_med.geojson",
    small = "data/rivers_small.geojson"
  )
  rivers_tier_files <- lapply(rivers_tier_urls, function(u) file.path(output_dir, u))
  dir.create(dirname(rivers_cache), recursive = TRUE, showWarnings = FALSE)
  rivers_min_flow <- 25000   # also the lower bound for the "small" tier
  need_rebuild <- rebuild_tiles ||
                  !file.exists(rivers_cache) ||
                  !all(vapply(rivers_tier_files, file.exists, logical(1)))
  if (need_rebuild) {
    if (verbose) cat("STEP — vectorising + chaining rivers (min_flow=",
                      rivers_min_flow, ")\n", sep = "")
    rs <- if (file.exists(rivers_cache)) readRDS(rivers_cache)
          else build_river_lines(min_flow = rivers_min_flow, verbose = verbose)
    if (!is.null(rs) && nrow(rs) > 0) {
      if (!file.exists(rivers_cache)) saveRDS(rs, rivers_cache)
      if (verbose) cat("  Chaining + tiering:\n")
      tiered <- chain_and_tier_rivers(rs, verbose = verbose)
      if (!is.null(tiered)) {
        for (tier in names(rivers_tier_files)) {
          sub <- tiered[tiered$tier == tier, ]
          if (nrow(sub) == 0) next
          geo_str <- sf_to_geojson_string(sub)
          writeLines(geo_str, rivers_tier_files[[tier]], useBytes = TRUE)
          # canonical (unclipped) copy: FeatureNamer names on THIS network —
          # its topology must run through lakes — then writes land-clipped
          # display geometry back to the main file.
          canon <- sub("\\.geojson$", "_canon.geojson", rivers_tier_files[[tier]])
          writeLines(geo_str, canon, useBytes = TRUE)
          if (verbose) cat(sprintf("  Wrote %s (%.1f MB, %d chains)\n",
                                    rivers_tier_files[[tier]],
                                    file.info(rivers_tier_files[[tier]])$size / 1024 / 1024,
                                    nrow(sub)))
        }
      }
    }
  }
  # Manifest: per-tier URL + min-zoom-to-load metadata for the template.
  rivers_manifest <- jsonlite::toJSON(
    list(
      big   = list(url = rivers_tier_urls$big,   min_zoom = 0),
      med   = list(url = rivers_tier_urls$med,   min_zoom = 6),
      small = list(url = rivers_tier_urls$small, min_zoom = 8)
    ),
    auto_unbox = TRUE
  )

  payload <- list(
    settlements    = settlements_to_records(settlements),
    sacred         = sacred_to_records(sacred),
    roads          = sf_to_geojson_string(roads),
    hex_tiers      = "{}",            # legacy — kept for backwards-compat
    hex_tier_files = hex_manifest,    # tier -> relative URL for lazy fetch
    features       = sf_to_geojson_string(cf$features),
    pois           = pois_to_records(cf$pois),
    continents     = if (is.null(continents)) "[]" else
      toJSON(continents, auto_unbox = TRUE, dataframe = "rows"),
    rivers         = rivers_manifest    # just {url: "data/rivers.geojson"}
  )
  if (verbose) {
    cat(sprintf("    settlements payload: %.1f KB\n",
                nchar(payload$settlements, type = "bytes") / 1024))
    cat(sprintf("    roads payload      : %.1f KB\n",
                nchar(payload$roads, type = "bytes") / 1024))
    cat(sprintf("    sacred payload     : %.1f KB\n",
                nchar(payload$sacred, type = "bytes") / 1024))
    cat(sprintf("    features payload   : %.1f KB\n",
                nchar(payload$features, type = "bytes") / 1024))
    cat(sprintf("    POIs payload       : %.1f KB\n",
                nchar(payload$pois, type = "bytes") / 1024))
    cat("\n")
  }

  # ----- HTML -----
  if (verbose) cat("STEP — rendering index.html\n")
  render_reference_html(
    template_path = template_path,
    output_path   = file.path(output_dir, "index.html"),
    data_payload  = payload,
    title         = title,
    subtitle      = subtitle,
    verbose       = verbose
  )

  # ----- PLAYER MAP -----
  # Rendered from the SAME payload rather than a second data pass, so the two
  # maps can never drift apart and the expensive assembly runs once.
  if (build_player) {
    render_player_html(
      template_path = template_path,
      output_dir    = output_dir,
      data_payload  = payload,
      title         = title,
      subtitle      = subtitle,
      tile_max_zoom = tile_max_zoom,
      verbose       = verbose
    )
  }

  # ----- LAUNCHERS -----
  create_map_launchers(output_dir, verbose = verbose)

  if (verbose) {
    cat("\n=========================================\n")
    cat(" COMPLETE\n")
    cat("=========================================\n")
    cat("GM map     : ", file.path(output_dir, "index.html"),
        "  (start-tileserver.bat — zoom 14, hidden POIs, writes tiles)\n", sep = "")
    if (build_player)
      cat("Player map : ", file.path(output_dir, "player", "index.html"),
          "  (start-map.bat — zoom ", tile_max_zoom,
          ", no hidden POIs, never writes)\n", sep = "")
    cat("\n")
  }

  invisible(list(
    output_dir  = output_dir,
    n_settlements = nrow(settlements),
    n_sacred      = if (is.null(sacred)) 0 else nrow(sacred),
    n_hex_tiers   = if (is.null(hex_tiers)) 0 else length(hex_tiers),
    n_roads       = if (is.null(roads)) 0 else nrow(roads),
    n_features    = if (is.null(cf$features)) 0 else nrow(cf$features),
    n_pois        = if (is.null(cf$pois)) 0 else nrow(cf$pois)
  ))
}
