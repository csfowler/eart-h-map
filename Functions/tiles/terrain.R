# =============================================================================
# terrain.R - Noise, dendritic drainage, micro-relief
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# The procedural terrain itself: fbm_world(), the drainage ladder whose zero-sets
# give the valley network, valleyness, and add_microrelief() which carves the one
# into the other.
#
# STUDENT PROJECT 3 (higher resolution zoom) lives here -- the `specs` ladder in
# drainage_incision() is the documented extension point, and NOISE_FIELDS$drainage
# is where new rungs get their seeds.
#
# Shared by more than it looks: shape_roads() and shape_rivers() in linear.R pull
# their vertices down the gradient of the SAME field add_microrelief() displaces
# the ground with. That is deliberate and is why roads bend around the hills that
# appear at z12+. See NOISE_FIELDS$terrain.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Deterministic procedural detail (world-coordinate-seeded fractal noise)
# -----------------------------------------------------------------------------

# Characteristic local relief (metres, peak-to-peak-ish) the micro-relief noise
# may add, per BIOME_CODES. The coarse DEM is the anchor; this is the amplitude
# of the high-frequency texture laid on top. 0 = perfectly smooth.
# "Rolling landforms" amplitudes: distinct hills/ridges/valleys visible at z12,
# with finer octaves layered on. Vertical relief in metres for the dominant
# (lowest) octave; finer octaves add proportionally smaller texture.
BIOME_RELIEF_M <- c(
  "0" = 0,    # Ocean      - flat (water paint covers it anyway)
  "1" = 20,   # Glacial    - subtle
  "2" = 90,   # Taiga      - rolling
  "3" = 60,   # Desert     - dunes / mesas
  "4" = 40,   # Grassland  - gentle swells
  "5" = 80,   # Temp Forest- rolling hills
  "6" = 70,   # Trop Forest- rolling
  "7" = 280,  # Mountainous- rugged
  "8" = 45    # Tundra     - low relief
)

#' Fractal (fbm) simplex noise evaluated at absolute world coordinates.
#'
#' Because it is sampled at absolute mercator metres (not tile-relative
#' coordinates), adjacent tiles sample the same continuous field at their shared
#' edge -> seamless. Lower octaves are identical across zoom levels, so zooming
#' in only *adds* finer octaves rather than re-rolling the surface.
#'
#' @param mx,my              Vectors of world coordinates in EPSG:3857 metres.
#' @param octaves            Number of frequency doublings to sum.
#' @param base_wavelength_m  Wavelength of the lowest (octave 1) component.
#' @return numeric vector in roughly [-1, 1].
fbm_world <- function(mx, my, octaves = 5, base_wavelength_m = 2000,
                      lacunarity = 2, gain = 0.5, seed = WORLD_SEED) {
  total <- numeric(length(mx)); amp <- 1; sumamp <- 0
  freq <- 1 / base_wavelength_m
  for (o in seq_len(octaves)) {
    total  <- total + amp * gen_simplex(mx * freq, my * freq, seed = seed + o)
    sumamp <- sumamp + amp
    amp    <- amp * gain
    freq   <- freq * lacunarity
  }
  total / sumamp
}

#' Large-scale smoothing via aggregate -> bilinear disaggregate: O(n) and far
#' cheaper than a wide focal(mean) (which dominated per-tile cost, especially at
#' deep zoom where the window grew). `scale_m` ~ the smoothing window in metres.
regional_mean <- function(r, scale_m = 1666) {
  fact <- max(2L, as.integer(round(scale_m / res(r)[1])))
  resample(aggregate(r, fact, fun = "mean", na.rm = TRUE), r, method = "bilinear")
}

#' Valleyness in [0,1]: how far a cell sits below its ~1.5-coarse-cell
#' neighbourhood (concavity). Fine-grid fallback (NOT seamless across tiles --
#' only used if no coarse source is available).
compute_valleyness <- function(elev, drop_m = 28) {
  v <- clamp((regional_mean(elev, 1666) - elev) / drop_m, 0, 1)
  v[is.na(v)] <- 0
  v
}

#' SEAMLESS valleyness: smooth the COARSE elevation window (which extends a
#' margin beyond the tile) with a small focal, project to the fine grid, and
#' subtract the anchor. Because the smoothing happens on the world-aligned coarse
#' grid (with margin), adjacent tiles agree at their shared edge -- unlike a
#' per-tile aggregate, whose block grid is tile-relative and seams.
valleyness_from_coarse <- function(coarse_elev, template, anchor_fine, drop_m = 28) {
  reg_c <- focal(coarse_elev, w = 3, fun = "mean", na.rm = TRUE)    # ~1.5 coarse cells
  reg_f <- project(reg_c, template, method = "bilinear")
  v <- clamp((reg_f - anchor_fine) / drop_m, 0, 1)
  v[is.na(v)] <- 0
  v
}

#' Deterministic dendritic drainage incision in [0, 1] (1 = deepest valley).
#'
#' The zero-level-set of an fbm field is a connected, branching curve -- a
#' natural-looking drainage line. Summing a few octaves of thin Gaussian ridges
#' along those zero contours (exp(-(n/width)^2)) yields a dendritic valley
#' network with tributaries, deterministic in world coordinates (seamless across
#' tiles, stable across zooms). Subtracting this from the anchor carves valleys
#' and leaves the land between them as interfluve ridges -- i.e. visually
#' credible watersheds, which is what reads as "real terrain" at z12+.
drainage_incision <- function(template, fine_detail = FALSE) {
  xy <- crds(template, na.rm = FALSE)
  mx <- xy[, 1]; my <- xy[, 2]
  # The ladder lives in NOISE_FIELDS$drainage (Functions/NoiseFields.R), which
  # is its single definition -- wavelength, channel half-width in noise units,
  # and weight. Coarser channels are wider/deeper (trunk valleys); finer ones
  # are thin tributaries.
  #
  # z13+ (fine_detail): the rungs past `coarse` switch on, keeping the dendritic
  # texture alive where the coarse ones go locally planar. Adding rungs only
  # ADDS detail — the shared coarser rungs are identical across zooms, which is
  # the property tests/test-crosszoom.R exists to defend.
  L <- nf("drainage")
  keep <- seq_len(if (fine_detail) L$active else L$coarse)
  specs <- lapply(keep, function(i)
    c(wl = L$wl[i], width = L$width[i], w = L$weight[i]))
  inc <- numeric(length(mx))
  for (i in seq_along(specs)) {
    s  <- specs[[i]]
    no <- gen_simplex(mx, my, frequency = 1 / s["wl"], seed = nf_seed("drainage", i))
    inc <- inc + s["w"] * exp(-(no / s["width"])^2)
  }
  setValues(rast(template), pmin(inc, 1))
}

#' Add deterministic terrain detail to an upsampled elevation raster: a dendritic
#' drainage network incised into the anchor (the structural "watershed" relief)
#' plus a finer isotropic texture on hillslopes.
#'
#' Amplitude is set per-biome (BIOME_RELIEF_M) and boosted where the coarse
#' terrain is already steep, so ridges get rougher and plains stay calm. The
#' coarse anchor dominates the far field; canon is preserved by clamping:
#'   * water (OCEAN/LAKE) cells are never RAISED above the anchor -- the clamp
#'     is pmin(), one-sided on purpose. Relief may deepen a seabed (invisible;
#'     water paint covers it) but may not push one up into a spurious island;
#'   * inland land (anchor > coast_band_m) is never pushed below sea level, so
#'     no spurious inland lakes appear.
#'
#' @param elev_fine   Upsampled elevation (EPSG:3857 metres), the anchor.
#' @param biome_fine  Biome codes aligned to elev_fine (or NULL -> uniform).
#' @param water_mask  Logical water raster (or NULL).
#' @param amp_scale   Global multiplier on all relief (0 = Phase-1 smooth).
#' @param valley_frac Share of the relief budget spent on drainage incision vs
#'                    isotropic texture (0.65 = mostly watershed structure).
add_microrelief <- function(elev_fine, biome_fine = NULL, water_mask = NULL,
                            amp_scale = 1,
                            base_wavelength_m = nf_wl("terrain"),
                            octaves = nf_octaves("terrain", "relief"),
                            coast_band_m = 40, valley_frac = 0.65,
                            valleyness = NULL, damp = NULL) {
  if (amp_scale <= 0) return(elev_fine)

  xy <- crds(elev_fine, na.rm = FALSE)
  n  <- fbm_world(xy[, 1], xy[, 2], octaves = octaves,
                  base_wavelength_m = base_wavelength_m)
  n_rast <- setValues(rast(elev_fine), n)

  # Per-biome amplitude (metres).
  if (!is.null(biome_fine)) {
    bcodes <- as.character(0:8)
    rcl <- cbind(as.integer(bcodes), unname(BIOME_RELIEF_M[bcodes]))
    amp_m <- classify(biome_fine, rcl, others = 40)
  } else {
    amp_m <- setValues(rast(elev_fine), 40)
  }

  # Steeper coarse terrain -> rougher (0.6 .. ~1.6x).
  slope  <- terrain(elev_fine, v = "slope", unit = "degrees")
  rough  <- 0.6 + clamp(slope / 30, 0, 1)
  rough[is.na(rough)] <- 0.6

  # Structural watershed relief: carve dendritic valleys into the anchor; the
  # un-incised land between channels becomes interfluve ridges. To make the
  # network DRAIN DOWNHILL (tree-like, not a reticulated web), concentrate the
  # incision in the anchor's own hollows (valleyness) -- the coarse DEM is
  # already flow-routed, so its valleys run downhill to real outlets; channels on
  # ridges/planar slopes fade out. valleyness is computed once and shared with
  # the botany step.
  if (is.null(valleyness)) valleyness <- compute_valleyness(elev_fine)
  inc <- drainage_incision(elev_fine, fine_detail = res(elev_fine)[1] <= 20) * valleyness

  # `damp` (0..1, optional) suppresses the added relief, e.g. across river
  # floodplains where alluvium is flat.
  dmp <- if (is.null(damp)) 1 else damp
  detail <- elev_fine +
    (n_rast * amp_m * rough * (1 - valley_frac) -              # hillslope texture
     inc    * amp_m *         valley_frac) * amp_scale * dmp   # drainage incision

  # --- canon-preserving clamp ---
  # Truly-inland land (anchor well above sea level) may never be pushed below
  # sea level -> no spurious inland lakes. Cells within the coastal band
  # (|anchor| <= coast_band_m) are left free to cross zero so the coastline can
  # crenellate; the water mask downstream follows the synthesized sign there.
  if (!is.null(water_mask)) {
    is_water <- !is.na(water_mask) & water_mask
    detail[is_water] <- pmin(detail[is_water], elev_fine[is_water])  # keep water at/below anchor
  }
  inland_land <- !is.na(elev_fine) & elev_fine > coast_band_m
  below <- inland_land & (detail < 1)
  detail[below] <- 1
  detail
}
