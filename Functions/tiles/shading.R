# =============================================================================
# shading.R - Terrain composite: hillshade, palette, water paint
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# composite_terrain() -- the two-component hillshade. The smooth ANCHOR is shaded
# at full strength for cross-zoom consistency; the detailed surface only modulates
# that light within a narrow band. A single strong pass over the detailed surface
# embossed every micro-relief bump at z13+ and looked like hammered metal; the
# split exists to prevent that and new shading has to respect it.
#
# STUDENT PROJECT 2 (more three-dimensional appearance) lives here. Cast shadows
# are the biggest available win and the hardest seamlessness problem in the repo:
# a low-sun shadow can reach beyond the 8-cell read margin set in core.R. Work out
# the maximum shadow length before writing anything.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Terrain composite (hillshade + colour-relief + water paint), in-memory
# -----------------------------------------------------------------------------

#' Build the parchment hillshade composite from an in-memory elevation raster.
#' This is the on-demand twin of MapBuilder::generate_hillshade_composite(): it
#' uses the same ELEVATION_PALETTE, the same 55% colour/relief blend, and the
#' same WATER_CLASS ocean/lake depth-shaded paint, but works entirely in memory
#' on a projected (metres) SpatRaster so it can render a single tile.
#'
#' @param elev_fine   SpatRaster, elevation in metres (projected CRS).
#' @param wc_fine     SpatRaster of WATER_CLASS codes aligned to elev_fine, or NULL.
#' @param water_deep  SpatRaster 0..1 (0 shallow shore .. 1 deep) aligned to the
#'                    grid; smooth distance-to-shore shelf shading, or NULL.
#' @param z_factor    Vertical exaggeration for the hillshade (matches gdaldem -z 3).
#' @param blend       Colour share of the composite (0.55 = 55% colour, 45% relief).
#' @return 3-band (R,G,B) 0-255 SpatRaster.
#' @param elev_anchor Optional smooth coarse-anchor elevation (pre-detail).
#'   When given, shading is TWO-COMPONENT: the anchor is shaded at the full
#'   z_factor (cross-zoom / canonical-pyramid consistency), while the detailed
#'   surface is shaded at a mild detail_z and only MODULATES the anchor light
#'   within [detail_lo, detail_hi]. A single x60 pass embossed every
#'   micro-relief bump and carved river wall into saturated offset
#'   light/shadow pairs at z13+.
composite_terrain <- function(elev_fine, wc_fine = NULL, water_deep = NULL,
                              water_mask = NULL,
                              z_factor = HILLSHADE_Z_EXAG, blend = 0.55,
                              azimuth = 315, altitude = 45,
                              elev_anchor = NULL, detail_z = 12,
                              detail_lo = 0.55, detail_hi = 1.30) {
  # We render in true metres (EPSG:3857), so the vertical exaggeration applies
  # directly (elev * z_factor before terrain()). HILLSHADE_Z_EXAG is shared with
  # the canonical pyramid's gdaldem call (there paired with -s 111120) so the
  # z8->z9 seam matches. Constant in metres => a given real landform shades
  # consistently across procedural zooms.
  # Hillshade from exaggerated elevation (slope+aspect in ONE terrain pass).
  # Clamp sub-sea elevation to 0 FIRST: otherwise the steep land->deep-ocean drop
  # (down to ~ -1000 m, quantised to coarse 0.01deg cells) x z_factor 60 makes
  # coastal land maximally shadowed -> dark coarse-cell BOXES along the coast.
  # Treating ocean as flat sea level gives a realistic coastal slope. (Water is
  # painted over afterwards anyway.)
  .hs <- function(e, zf) {
    ta <- terrain(clamp(e, 0) * zf, v = c("slope", "aspect"), unit = "radians")
    h  <- terra::values(shade(ta[["slope"]], ta[["aspect"]], angle = altitude, direction = azimuth))[, 1]
    h  <- pmin(pmax(h, 0), 1); h[is.na(h)] <- sin(altitude * pi / 180)
    h
  }
  if (is.null(elev_anchor)) {
    hs <- .hs(elev_fine, z_factor)
  } else {
    flat <- sin(altitude * pi / 180)                       # shade() value on flat ground
    mod  <- pmin(pmax(.hs(elev_fine, detail_z) / flat, detail_lo), detail_hi)
    hs   <- pmin(pmax(.hs(elev_anchor, z_factor) * mod, 0), 1)
  }
  hs[is.na(hs)] <- 1

  if (is.null(water_mask) && !is.null(wc_fine)) {
    water_mask <- !is.na(wc_fine) & (wc_fine == WATER_CLASS$OCEAN | wc_fine == WATER_CLASS$LAKE)
  }
  iw <- if (is.null(water_mask)) NULL else { w <- terra::values(water_mask)[, 1]; !is.na(w) & w > 0 }

  # Colour-relief on value vectors. water_class (via water_mask) is authoritative
  # for land/water: a LAND cell must never colour as water even if its
  # (misaligned) elevation is <= 0 -> clamp the colour input up to >= 1 on land.
  # NA elevation -> deepest stop so open water reads as ocean. Hillshade uses the
  # true elevation, so relief shading is unaffected.
  ev <- terra::values(elev_fine)[, 1]
  ev[is.na(ev)] <- ELEVATION_PALETTE$breaks[1]
  if (!is.null(iw)) ev[!iw & ev < 1] <- 1
  rgbp <- grDevices::col2rgb(ELEVATION_PALETTE$colors)
  br   <- ELEVATION_PALETTE$breaks
  relief <- blend + (1 - blend) * hs
  out <- matrix(0, length(ev), 3)
  for (b in 1:3) out[, b] <- stats::approx(br, rgbp[b, ], xout = ev, rule = 2)$y * relief

  # Water paint (ocean + lakes), shaded shallow->deep by distance-to-shore.
  if (!is.null(iw) && any(iw)) {
    dn <- if (!is.null(water_deep)) terra::values(water_deep)[, 1] else rep(1, length(ev))
    dn <- pmin(pmax(dn, 0), 1); dn[is.na(dn)] <- 1   # missing -> deep (open ocean)
    shallow <- c(126, 178, 214); deep <- c(38, 76, 128)   # shared with MapBuilder
    for (b in 1:3) out[iw, b] <- shallow[b] + (deep[b] - shallow[b]) * dn[iw]
  }
  setValues(rast(elev_fine, nlyrs = 3), pmin(pmax(out, 0), 255))
}
