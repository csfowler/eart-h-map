# =============================================================================
# vegetation.R - Biome edges and the botany overlay
#
# Part of the tile engine. Loaded by Functions/TileServer.R, which is now a
# loader: source that, not this.
#
# Biome boundary dithering and the whole cover model: canopy/meadow split by a
# world-seeded openness field, the lunar plant-type tint, marsh, scree, crevasses.
#
# STUDENT PROJECT 5 (more diverse plants and trees) lives here.
#
# The `suppress` argument to apply_vegetation() is the extension point for any new
# kind of clearing: produce a [0,1] raster, wobble its edge, and max() it into the
# corridor mask in render.R. You should not need to touch this file to add one.
#
# Provenance is per FILE, not per function: editing anything here moves this
# file's hash and re-renders the tiles that depend on it. There is no list to
# remember to update.
# =============================================================================

# -----------------------------------------------------------------------------
# Biome edge dithering (organic, parameter-respecting)
# -----------------------------------------------------------------------------

#' Dither biome-class boundaries by domain-warping the sampling coordinates.
#'
#' A nearest-neighbour upsample of the coarse biome leaves blocky 0.01deg steps.
#' We instead sample the (fine, blocky) biome at each cell's position perturbed
#' by a world-seeded noise warp of ~half a coarse cell, so boundaries interlock
#' organically. Because every sample is a real value read from within ~1 cell,
#' the result NEVER introduces a class absent from the local neighbourhood.
#'
#' @param coarse_biome COARSE biome window (EPSG:4326, with read margin).
#' @param template     Tile grid (EPSG:3857) to produce the biome on.
#' @param warp_m       Warp amplitude in metres (~half a coarse cell).
#'
#' Samples the COARSE window (margin beyond the tile) at warped lon/lat, so edge
#' warps don't fall off into NA -> seamless biome boundaries across tiles.
synthesize_biome <- function(coarse_biome, template, warp_m = 600,
                             base_wavelength_m = nf_wl("biome.warp.x")) {
  if (is.null(coarse_biome)) return(NULL)
  xy <- crds(template, na.rm = FALSE)
  wx <- warp_m * fbm_world(xy[, 1], xy[, 2], octaves = nf_octaves("biome.warp.x"),
                           base_wavelength_m = base_wavelength_m,
                           seed = nf_seed("biome.warp.x"))
  wy <- warp_m * fbm_world(xy[, 1], xy[, 2], octaves = nf_octaves("biome.warp.y"),
                           base_wavelength_m = base_wavelength_m,
                           seed = nf_seed("biome.warp.y"))
  ll <- merc_to_lonlat(xy[, 1] + wx, xy[, 2] + wy)
  setValues(rast(template), terra::extract(coarse_biome, cbind(ll$lon, ll$lat))[, 1])
}

# -----------------------------------------------------------------------------
# Vegetation / botany overlay
# -----------------------------------------------------------------------------

# Per-biome vegetation: `cover` = how strongly vegetation tints the terrain at
# full capacity (0 = bare ground), `canopy` = dense stand colour, `meadow` =
# open clearing / grass colour. Vegetated land is split between the two by an
# "openness" field so forest reads as a mosaic of dense stands and meadows
# rather than a monotone sheet. At z12-z14 this is canopy/meadow COVER texture,
# not single trees. (Later: cleared/urban land will subtract from `cover` and
# add their own classes.)
BIOME_VEG <- list(
  "0" = list(cover = 0.00, canopy = "#1f4a25", meadow = "#1f4a25"),  # Ocean
  "1" = list(cover = 0.00, canopy = "#7a8a6a", meadow = "#9aa888"),  # Glacial
  "2" = list(cover = 0.66, canopy = "#2f4c2c", meadow = "#7d9456"),  # Taiga
  "3" = list(cover = 0.10, canopy = "#9a8a55", meadow = "#cdbb80"),  # Desert
  "4" = list(cover = 0.38, canopy = "#6f8a44", meadow = "#a6b86c"),  # Grassland
  "5" = list(cover = 0.68, canopy = "#2c5a32", meadow = "#8fa85c"),  # Temp Forest
  "6" = list(cover = 0.74, canopy = "#1d4623", meadow = "#6f9447"),  # Trop Forest
  "7" = list(cover = 0.22, canopy = "#56624a", meadow = "#8a906a"),  # Mountainous
  "8" = list(cover = 0.26, canopy = "#6c7651", meadow = "#9aa074")   # Tundra
)
# Capacity value that counts as "full" vegetation density (clamp reference).
# capacity.vrt runs ~0-250 in practice, so the reference is ~200 (not thousands).
VEG_CAPACITY_REF <- 200

# The three lunar plant types and their per-biome mix -- MIRRORS
# PlantBuilder::PLANT_MIX (rows = biome 0-8, cols = bloom/spreader/root). Each
# type is tied to a moon: bloom<-Ganymede, spreader<-Io, root<-Deos. Vegetation
# type, and the moon-colour wash on it, vary globally with the annual lunar
# climate (Input Data/Moons/annual_moon_power_stats.tif, bands Io/Ganymede/Deos
# mean). Keep in sync with PlantBuilder if that matrix changes.
PLANT_MIX <- matrix(c(
  0.0, 0.0, 0.0,  # 0 Ocean
  0.0, 0.1, 0.9,  # 1 Glacial
  0.2, 0.3, 0.5,  # 2 Taiga
  0.2, 0.5, 0.3,  # 3 Desert
  0.4, 0.4, 0.2,  # 4 Grassland
  0.5, 0.2, 0.3,  # 5 Temperate Forest
  0.5, 0.3, 0.2,  # 6 Tropical Forest
  0.3, 0.3, 0.4,  # 7 Mountainous
  0.1, 0.2, 0.7   # 8 Tundra
), nrow = 9, ncol = 3, byrow = TRUE,
  dimnames = list(NULL, c("bloom", "spreader", "root")))

# Per-type foliage colour MODIFIER (RGB multiplier on the biome base colour) so
# stands of different lunar plant types read differently while keeping biome
# identity: bloom = brighter/warmer (flowering), spreader = lighter spreading
# groundcover, root = darker/woodier.
TYPE_MOD <- list(
  bloom    = c(1.14, 1.06, 0.78),
  spreader = c(0.97, 1.07, 0.92),
  root     = c(0.80, 0.84, 0.78)
)
# Strength of the moon-light colour wash on vegetation (0 = none).
MOON_TINT_STRENGTH <- 0.35

#' Raster smoothstep: 0 below e0, 1 above e1, smooth in between.
smoothstep_r <- function(r, e0, e1) {
  t <- clamp((r - e0) / (e1 - e0), 0, 1); t * t * (3 - 2 * t)
}

#' Composite a deterministic vegetation mosaic onto a terrain RGB raster.
#'
#' Vegetated fraction = per-biome cover x sqrt(capacity/REF). That fraction is
#' split between dense CANOPY and open MEADOW by a world-seeded "openness" field
#' (large clearings + stand-scale variation), so the cover is diverse rather
#' than uniform. Canopy gets an extra fine mottle for within-stand variation.
#' Everything is shaded by the underlying terrain luminance so hillshade relief
#' still reads through, and zeroed on water. World-seeded => stands/meadows sit
#' in the same place across tiles and zooms.
#' @param fpw       River floodplain weight 0..1 (riparian gallery forest).
#' @param water_inf Coarse water-influence 0..1 (lakeshore lushness).
#' @param waterness Continuous shore-proximity field (beach band under 0.5).
#' @param grove     Sacred-grove weight 0..1 (denser, darker canopy).
#' @param zoom      Tile zoom: gates crown bump-shading (z12+) + stipple (z13+).
apply_vegetation <- function(comp, biome_fine, capacity_fine = NULL,
                             water_mask = NULL, elev_fine = NULL, moon_fine = NULL,
                             suppress = NULL, valleyness = NULL,
                             fpw = NULL, water_inf = NULL, waterness = NULL,
                             grove = NULL, zoom = 12) {
  if (is.null(biome_fine)) return(comp)
  # Vectorised: pull every layer to numeric vectors once, do all the arithmetic
  # in plain R, write a single output raster. Avoids dozens of intermediate
  # SpatRaster allocations (this was the hottest tile phase).
  ss <- function(x, e0, e1) { t <- pmin(pmax((x - e0) / (e1 - e0), 0), 1); t * t * (3 - 2 * t) }
  vv <- function(r) if (is.null(r)) NULL else terra::values(r)[, 1]

  xy <- crds(comp, na.rm = FALSE)
  mx <- xy[, 1]; my <- xy[, 2]; N <- length(mx)
  cm <- terra::values(comp)                                  # N x 3 (R,G,B)
  bv <- vv(biome_fine)                                       # biome code 0-8 (NA)
  idx <- bv + 1L; valid <- !is.na(bv) & bv >= 0 & bv <= 8; idx[!valid] <- 1L

  # Openness -> canopy vs meadow split (wide-band warped fractal). A REGIONAL
  # octave (26 km) biases whole districts forested or open: without it the
  # 3 km mosaic is statistically identical everywhere and mid-zoom wilderness
  # reads as two-colour wallpaper.
  wx <- 750 * fbm_world(mx, my, octaves = nf_octaves("veg.openwarp.x"), base_wavelength_m = nf_wl("veg.openwarp.x"), seed = nf_seed("veg.openwarp.x"))
  wy <- 750 * fbm_world(mx, my, octaves = nf_octaves("veg.openwarp.y"), base_wavelength_m = nf_wl("veg.openwarp.y"), seed = nf_seed("veg.openwarp.y"))
  op <- fbm_world(mx + wx, my + wy, octaves = nf_octaves("veg.openness"), base_wavelength_m = nf_wl("veg.openness"),
                  gain = nf("veg.openness")$gain, seed = nf_seed("veg.openness"))
  reg <- fbm_world(mx, my, octaves = nf_octaves("veg.regional"), base_wavelength_m = nf_wl("veg.regional"), seed = nf_seed("veg.regional"))
  canopy_frac <- ss(pmin(pmax(0.5 + 0.5 * op + 0.28 * reg, 0), 1), 0.36, 0.64)

  # Regional heterogeneity fields (all world-anchored):
  #   hue  — 42 km warm<->cool colour lean (soil/geology mood);
  #   shn  — shrub/heath patches, a THIRD cover class carved from meadow;
  #   brn  — sparse barren openings (burn scars / rocky ground), kept out of
  #          riparian zones. Together they break the two-colour lock.
  hue <- fbm_world(mx, my, octaves = nf_octaves("veg.hue"), base_wavelength_m = nf_wl("veg.hue"), seed = nf_seed("veg.hue"))
  # shrub shares the openness DOMAIN WARP so heath interlocks with the canopy
  # mosaic (un-warped it stamps round 2 km dots across the wilderness)
  shn <- fbm_world(mx + wx, my + wy, octaves = nf_octaves("veg.sheen"), base_wavelength_m = nf_wl("veg.sheen"), seed = nf_seed("veg.sheen"))
  brn <- fbm_world(mx, my, octaves = nf_octaves("veg.brown"), base_wavelength_m = nf_wl("veg.brown"), seed = nf_seed("veg.brown"))

  cover_mult <- rep(1, N); ev <- rep(0, N)
  if (!is.null(elev_fine)) {
    vy <- if (!is.null(valleyness)) vv(valleyness) else vv(compute_valleyness(elev_fine))
    vy[is.na(vy)] <- 0
    canopy_frac <- canopy_frac * (1 - 0.75 * vy)             # meadows in valleys
    ev <- vv(elev_fine); ev[is.na(ev)] <- -1e4
    cover_mult <- 1 - ss(ev, 1800, 2800)                     # treeline
  }

  # Riparian / lakeshore lushness: gallery forest along rivers (fpw) and around
  # lakes (water_influence, rescaled so only the strong-influence zone counts).
  wi <- if (!is.null(water_inf)) { w <- vv(water_inf); w[is.na(w)] <- 0; ss(w, 0.55, 0.95) } else rep(0, N)
  fp <- if (!is.null(fpw))       { w <- vv(fpw);       w[is.na(w)] <- 0; w } else rep(0, N)
  lush <- pmax(fp, 0.8 * wi)
  gr <- if (!is.null(grove)) { g <- vv(grove); g[is.na(g)] <- 0; g } else rep(0, N)
  canopy_frac <- pmin(canopy_frac + 0.45 * lush + 0.6 * gr, 1)  # gallery/grove closes the canopy
  mott <- fbm_world(mx, my, octaves = nf_octaves("veg.mottle"), base_wavelength_m = nf_wl("veg.mottle"), seed = nf_seed("veg.mottle"))

  # Lunar dominance -> plant-type mix + colour wash. bloom<-Ganymede, spreader<-Io, root<-Deos.
  if (!is.null(moon_fine)) {
    mi <- terra::values(moon_fine); s3 <- mi[, 1] + mi[, 2] + mi[, 3] + 1e-6
    lw_io <- mi[, 1] / s3; lw_gan <- mi[, 2] / s3; lw_deo <- mi[, 3] / s3
  } else { lw_io <- lw_gan <- lw_deo <- rep(1/3, N) }
  pm_b <- PLANT_MIX[idx, "bloom"]; pm_s <- PLANT_MIX[idx, "spreader"]; pm_r <- PLANT_MIX[idx, "root"]
  pm_b[!valid] <- 0; pm_s[!valid] <- 0; pm_r[!valid] <- 0
  raw_b <- pm_b * lw_gan; raw_s <- pm_s * lw_io; raw_r <- pm_r * lw_deo
  den <- raw_b + raw_s + raw_r + 1e-6; mix_b <- raw_b / den; mix_s <- raw_s / den

  sel <- pmin(pmax(0.5 + 0.5 * fbm_world(mx, my, octaves = nf_octaves("veg.typesel"), base_wavelength_m = nf_wl("veg.typesel"),
                                         seed = nf_seed("veg.typesel")), 0), 1)
  edge <- 0.06; cumB <- mix_b; cumS <- mix_b + mix_s
  m_b <- 1 - ss(sel, cumB - edge, cumB + edge)
  m_r <- ss(sel, cumS - edge, cumS + edge)
  m_s <- pmin(pmax(1 - m_b - m_r, 0), 1)
  k <- MOON_TINT_STRENGTH
  tint <- list(1 + k * (lw_io - 1/3), 1 + k * (lw_gan - 1/3), 1 + k * (lw_deo - 1/3))

  cover_vec <- vapply(BIOME_VEG, function(v) v$cover, numeric(1))
  cover <- cover_vec[idx]; cover[!valid] <- 0
  capn  <- if (!is.null(capacity_fine)) pmin(pmax(vv(capacity_fine) / VEG_CAPACITY_REF, 0), 1) else rep(1, N)
  barren <- ss(brn, 0.55, 0.80) * (1 - lush)                 # rare, large scars (~2-3%)
  veg <- cover * sqrt(capn) * cover_mult * (1 + 0.12 * reg) * (1 - 0.7 * barren)
  veg <- pmin(veg * (1 + 0.7 * lush) + 0.25 * gr, 1)         # riparian/grove cover boost

  iw <- rep(FALSE, N)
  if (!is.null(water_mask)) { wm <- vv(water_mask); iw <- !is.na(wm) & wm > 0 }

  # Beach sand: the land band just under the waterness 0.5 contour (follows the
  # warped shore), on low ground only -- cliffs get no sand.
  wn <- if (!is.null(waterness)) { w <- vv(waterness); w[is.na(w)] <- 0; w } else rep(0, N)
  sand <- ss(wn, 0.40, 0.47) * (1 - ss(ev, 6, 14)); sand[iw] <- 0

  canopy_a <- veg * canopy_frac        * (1 - 0.9 * sand)
  meadow_all <- veg * (1 - canopy_frac) * 0.7 * (1 - 0.9 * sand)
  shrub_frac <- 0.5 * ss(shn, 0.0, 0.55) * (1 - 0.6 * lush)   # heath thins in riparian
  shrub_a  <- meadow_all * shrub_frac
  meadow_a <- meadow_all - shrub_a
  if (!is.null(suppress)) { sup <- vv(suppress); sup[is.na(sup)] <- 0
    canopy_a <- canopy_a * (1 - 0.85 * sup); meadow_a <- meadow_a * (1 - 0.55 * sup)
    shrub_a  <- shrub_a  * (1 - 0.7 * sup) }
  canopy_a[iw] <- 0; meadow_a[iw] <- 0; shrub_a[iw] <- 0
  canopy_a[is.na(canopy_a)] <- 0; meadow_a[is.na(meadow_a)] <- 0
  shrub_a[is.na(shrub_a)] <- 0

  # Marsh: flat strong-floodplain ground near water -> murky mottled wetland.
  marsh <- ss(fp, 0.6, 0.95) * (0.3 + 0.7 * ss(wi, 0.2, 0.8)) *
           ss(fbm_world(mx, my, octaves = nf_octaves("veg.standmix"), base_wavelength_m = nf_wl("veg.standmix"),
                        seed = nf_seed("veg.standmix")), -0.05, 0.45)
  marsh[iw] <- 0
  # Scree/rock mottle above the treeline.
  high  <- ss(ev, 2150, 2650)
  mott2 <- fbm_world(mx, my, octaves = nf_octaves("veg.mottle2"), base_wavelength_m = nf_wl("veg.mottle2"), seed = nf_seed("veg.mottle2"))
  scree <- 0.5 * high * (0.5 + 0.5 * ss(mott2, -0.3, 0.5)); scree[iw] <- 0

  # Snow above a latitude-dependent snowline (equator ~4400 m, 45deg ~2750 m,
  # poleward ~1100 m floor), with a noise-raggedized transition band; the
  # Glacial biome is ice regardless of elevation. Crevasse streaks: thin dark
  # bands along a simplex zero-set, only where the pack is solid.
  latr <- 2 * atan(exp(my / MERC_R)) - pi / 2
  slm  <- 1100 + 3300 * pmax(cos(latr), 0)^2
  snow <- ss(ev + 180 * mott2, slm - 300, slm + 200)
  snow <- pmax(snow, ifelse(!is.na(bv) & bv == 1, 0.85, 0))    # Glacial biome
  snow[iw] <- 0
  crev <- numeric(N)
  if (any(snow > 0.6)) {
    cn <- gen_simplex(mx, my, frequency = 1 / nf_wl("veg.crevasse"),
                      seed = nf_seed("veg.crevasse"))
    crev <- pmax(0, 1 - abs(cn) / 0.045) * ss(snow, 0.6, 0.9)
  }

  # Crown bump-shading: emboss a crown-scale noise field along the hillshade
  # light axis (az 315), so canopy reads as a lumpy lit surface, not flat paint.
  # Fades in as resolution approaches crown scale (full by z13, off below z12).
  xres <- res(comp)[1]
  bamp <- pmin(pmax((42 - xres) / 24, 0), 1)
  bmul <- rep(1, N)
  if (bamp > 0) {
    hb <- 9
    b1 <- fbm_world(mx, my, octaves = nf_octaves("veg.crownbump"), base_wavelength_m = nf_wl("veg.crownbump"), seed = nf_seed("veg.crownbump"))
    b2 <- fbm_world(mx - hb * 0.7071, my + hb * 0.7071, octaves = nf_octaves("veg.crownbump"),
                    base_wavelength_m = nf_wl("veg.crownbump"), seed = nf_seed("veg.crownbump"))
    bmul <- pmin(pmax(1 + 1.5 * bamp * (b1 - b2), 0.62), 1.38)
  }

  # vegetation dies out under the pack well before it is fully white
  canopy_a <- canopy_a * (1 - ss(snow, 0.15, 0.55))
  meadow_a <- meadow_a * (1 - ss(snow, 0.25, 0.7))
  shrub_a  <- shrub_a  * (1 - ss(snow, 0.2, 0.6))

  crgb <- grDevices::col2rgb(vapply(BIOME_VEG, function(v) v$canopy, character(1)))
  mrgb <- grDevices::col2rgb(vapply(BIOME_VEG, function(v) v$meadow, character(1)))
  LUSH  <- c(34, 84, 44); GROVE <- c(24, 58, 34)              # riparian / sacred-grove canopy
  SAND  <- c(213, 196, 158); MARSH <- c(94, 105, 74); ROCK <- c(141, 134, 126)
  SNOWC <- c(237, 241, 247); CREV <- c(176, 190, 208)
  # Heterogeneity colour anchors: dry meadow -> tawny, dry canopy -> olive,
  # barren wash; hue field leans everything warm or cool regionally.
  TANM <- c(181, 165, 116); OLIV <- c(96, 104, 60); BARC <- c(152, 142, 120)
  WARM <- c(1.10, 1.03, 0.86); COOL <- c(0.90, 1.00, 1.12)
  SHRB <- c(0.90, 1.00, 0.88)                                # shrub tone modifier
  hmix <- 0.5 + 0.5 * pmin(pmax(hue, -1), 1)
  dry  <- (1 - capn) * (1 - lush)                            # capacity-driven dryness
  dk   <- 0.45 * dry
  lushx <- pmin(0.45 * lush + 0.75 * gr, 0.85)               # canopy colour shift share
  lum  <- pmin(pmax((0.3 * cm[, 1] + 0.59 * cm[, 2] + 0.11 * cm[, 3]) / 160, 0.45), 1.2)
  base <- 1 - canopy_a - meadow_a - shrub_a
  sa <- 0.85 * sand; ma <- 0.55 * marsh * (1 - sand); sc <- scree * (1 - sand)
  bl <- 0.3 * barren * (1 - sand) * (1 - snow)
  out  <- cm
  for (b in 1:3) {
    tdr   <- (WARM[b] * hmix + COOL[b] * (1 - hmix))^0.8     # regional hue lean
    tmod  <- m_b * TYPE_MOD$bloom[b] + m_s * TYPE_MOD$spreader[b] + m_r * TYPE_MOD$root[b]
    shade <- lum * tmod * tint[[b]] * tdr
    crow  <- crgb[b, idx] * (1 - lushx) + (LUSH[b] * lush + GROVE[b] * gr) /
             pmax(lush + gr, 1e-6) * lushx                   # weighted lush/grove hue
    crow  <- crow * (1 - 0.30 * dry) + OLIV[b] * 0.30 * dry
    mbase <- mrgb[b, idx] * (1 - dk) + TANM[b] * dk
    ccol  <- crow * shade * (1 + 0.12 * mott) * bmul
    mcol  <- mbase * shade * (1 + 0.3 * (bmul - 1))
    scol  <- (0.55 * crgb[b, idx] + 0.45 * mrgb[b, idx]) * SHRB[b] * shade
    o <- cm[, b] * base + ccol * canopy_a + mcol * meadow_a + scol * shrub_a
    o <- o * (1 - bl) + BARC[b] * lum * bl                   # barren scar wash
    o <- o * (1 - ma) + MARSH[b] * lum * ma                  # wetland wash
    o <- o * (1 - sc) + ROCK[b]  * lum * sc                  # scree mottle
    o <- o * (1 - sa) + SAND[b]  * lum * sa                  # beach on top
    o <- o * (1 - snow) + SNOWC[b] * lum * snow              # snow / ice cap
    o <- o * (1 - 0.55 * crev) + CREV[b] * lum * 0.55 * crev # crevasse streaks
    out[, b] <- pmin(pmax(o, 0), 255)
  }

  # Tree-scale stipple (z13+): one candidate crown per world-anchored 26 m
  # lattice cell, kept with probability ~ canopy density (sparse lone trees in
  # meadows), drawn as a darkened or sunlit crown disc 1-2 px across.
  if (xres <= 20) {
    g <- 26
    cxi <- floor(mx / g); cyi <- floor(my / g)
    h1 <- .hash01(cxi, cyi, 7L); h2 <- .hash01(cxi, cyi, 8L)
    h3 <- .hash01(cxi, cyi, 9L); h4 <- .hash01(cxi, cyi, 10L)
    tx <- (cxi + 0.25 + 0.5 * h2) * g; ty <- (cyi + 0.25 + 0.5 * h3) * g
    d2 <- (mx - tx)^2 + (my - ty)^2
    crown <- pmax(4.2 + 3.4 * h4, 0.55 * xres)
    keepp <- ifelse(canopy_a > 0.06, 0.92 * canopy_a,
                    ifelse(meadow_a > 0.12, 0.06, 0))
    is_tree <- d2 < crown^2 & h1 < keepp & !iw & sand < 0.3
    if (any(is_tree)) {
      fac <- ifelse(h4 < 0.62, 0.72 + 0.2 * h4, 1.14)        # shadowed vs sunlit crowns
      for (b in 1:3) out[is_tree, b] <- pmin(out[is_tree, b] * fac[is_tree], 255)
    }
  }
  setValues(comp, out)
}
