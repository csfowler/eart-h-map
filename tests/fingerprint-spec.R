# =============================================================================
# fingerprint-spec.R — what "the map looks the same" means, numerically
#
# Every function here is a PURE function of world coordinates: given the same
# metres, it must return the same numbers forever. That is the whole contract
# the procedural engine rests on (see CLAUDE.md, "Determinism and seamlessness"),
# and it is what lets a fingerprint stand in for looking at a rendered tile.
#
# The fingerprints exist for two jobs:
#
#   1. Refactoring safety. A change that is meant to be structural — pulling
#      magic numbers into NoiseFields.R, say — must not move a single value.
#      Run this before and after; the digests must match exactly.
#
#   2. Change review. A change that IS meant to alter the world will move these,
#      which is correct. The test then fails loudly and the golden file has to be
#      re-blessed deliberately, in the same commit, where a reviewer can see it.
#
# So a failure here is not automatically a bug. It is a question: did you mean
# to change what the world looks like?
#
# Sample points are fixed, scattered across several continents, and deliberately
# NOT round numbers — a lattice of round coordinates can sit on the zero set of
# a simplex field and hide a real difference behind a wall of zeros.
# =============================================================================

# EPSG:3857 metres. Spread over the map, at awkward offsets.
FP_POINTS <- cbind(
  x = c(-6731442.7,  -6402887.3,  -5988115.1,   1204553.9,   1866201.4,
         2503118.8,   7742019.6,   8113664.2,  -1477320.5,    311882.6,
         4920775.3,  -9013664.8),
  y = c(-4183992.1,  -3771205.6,   2216340.8,   4402118.7,  -2015773.2,
          883104.5,  -1662209.4,   2904471.3,   5511028.9,  -4990337.1,
         3318260.7,    772514.6)
)

#' Digest a numeric vector at a fixed precision.
#'
#' Rounded to 9 significant digits before hashing: R's last-bit floating point
#' noise varies with BLAS build and would make the fingerprint machine-specific,
#' which would turn this test into a source of false alarms on a student laptop.
#' Nine digits is far tighter than anything visible in a rendered tile.
fp_digest <- function(v) {
  v <- as.numeric(v)
  v[!is.finite(v)] <- -99999
  paste(sprintf("%.9g", v), collapse = "|")
}

#' Evaluate every world-seeded field at FP_POINTS and digest it.
#'
#' Each entry is one FIELD, named for the thing it drives. Add an entry when you
#' add a field; that is the point at which the test starts protecting it.
#'
#' EVERY seed, wavelength and octave count below comes from NOISE_FIELDS. None
#' is written out here. That is not tidiness -- a fingerprint that restates the
#' numbers it is guarding cannot detect a change to them. The first version of
#' this file did restate them, for all but the drainage ladder, so editing a
#' wavelength in the registry moved nothing here and the suite stayed green
#' while the registry and the map disagreed.
noise_fingerprint <- function(pts = FP_POINTS) {
  mx <- pts[, "x"]; my <- pts[, "y"]
  f <- list()

  # --- the shared terrain field ------------------------------------------
  # Sampled at BOTH octave counts in use: add_microrelief takes 6, the road and
  # river terrain-pull take 4. They are the same field at different depths, and
  # both are pinned so a change to either is visible.
  f[["terrain.relief.o6"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("terrain", "relief"),
              base_wavelength_m = nf_wl("terrain", "relief"),
              seed = nf_seed("terrain")))
  f[["terrain.pull.o4"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("terrain", "pull"),
              base_wavelength_m = nf_wl("terrain", "pull"),
              seed = nf_seed("terrain")))

  # --- coastline + biome warps -------------------------------------------
  f[["coast.warp.x"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("coast.warp.x"),
              base_wavelength_m = nf_wl("coast.warp.x"),
              seed = nf_seed("coast.warp.x")))
  f[["coast.warp.y"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("coast.warp.y"),
              base_wavelength_m = nf_wl("coast.warp.y"),
              seed = nf_seed("coast.warp.y")))
  f[["biome.warp.x"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("biome.warp.x"),
              base_wavelength_m = nf_wl("biome.warp.x"),
              seed = nf_seed("biome.warp.x")))
  f[["biome.warp.y"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("biome.warp.y"),
              base_wavelength_m = nf_wl("biome.warp.y"),
              seed = nf_seed("biome.warp.y")))


  # --- vegetation ---------------------------------------------------------
  f[["veg.openwarp.x"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.openwarp.x"),
              base_wavelength_m = nf_wl("veg.openwarp.x"),
              seed = nf_seed("veg.openwarp.x")))
  f[["veg.openwarp.y"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.openwarp.y"),
              base_wavelength_m = nf_wl("veg.openwarp.y"),
              seed = nf_seed("veg.openwarp.y")))
  f[["veg.openness"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.openness"),
              base_wavelength_m = nf_wl("veg.openness"),
              gain = nf("veg.openness")$gain,
              seed = nf_seed("veg.openness")))
  f[["veg.regional"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.regional"),
              base_wavelength_m = nf_wl("veg.regional"),
              seed = nf_seed("veg.regional")))
  f[["veg.hue"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.hue"),
              base_wavelength_m = nf_wl("veg.hue"),
              seed = nf_seed("veg.hue")))
  f[["veg.sheen"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.sheen"),
              base_wavelength_m = nf_wl("veg.sheen"),
              seed = nf_seed("veg.sheen")))
  f[["veg.brown"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.brown"),
              base_wavelength_m = nf_wl("veg.brown"),
              seed = nf_seed("veg.brown")))
  f[["veg.mottle"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.mottle"),
              base_wavelength_m = nf_wl("veg.mottle"),
              seed = nf_seed("veg.mottle")))
  f[["veg.typesel"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.typesel"),
              base_wavelength_m = nf_wl("veg.typesel"),
              seed = nf_seed("veg.typesel")))
  f[["veg.standmix"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.standmix"),
              base_wavelength_m = nf_wl("veg.standmix"),
              seed = nf_seed("veg.standmix")))
  f[["veg.mottle2"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.mottle2"),
              base_wavelength_m = nf_wl("veg.mottle2"),
              seed = nf_seed("veg.mottle2")))
  f[["veg.crownbump"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("veg.crownbump"),
              base_wavelength_m = nf_wl("veg.crownbump"),
              seed = nf_seed("veg.crownbump")))
  # Raw simplex, not fbm -- crevasse streaks are a single octave by design.
  f[["veg.crevasse"]] <- fp_digest(
    ambient::gen_simplex(mx, my, frequency = 1 / nf_wl("veg.crevasse"),
                         seed = nf_seed("veg.crevasse")))


  # --- drainage: the octave ladder ---------------------------------------
  # Pinned per rung, so extending the ladder for deeper zoom (a documented
  # student project) shows up as new entries rather than as a silent change to
  # the existing structure. The coarse rungs MUST NOT move when rungs are added.
  # Wavelengths and seeds come from the REGISTRY, never from literals here. A
  # fingerprint that restates the numbers it is supposed to be guarding cannot
  # detect a change to them -- which is precisely how the first version of this
  # file let a coarse-rung wavelength change sail through green.
  for (i in seq_len(nf("drainage")$active)) {
    f[[sprintf("drainage.rung%d", i)]] <- fp_digest(
      ambient::gen_simplex(mx, my, frequency = 1 / nf_wl("drainage", i),
                           seed = nf_seed("drainage", i)))
  }
  # The incision SHAPE, not just the noise: width and weight change what gets
  # carved without touching any seed, so pin them too.
  f[["drainage.profile"]] <- fp_digest(
    c(nf("drainage")$wl, nf("drainage")$width, nf("drainage")$weight,
      nf("drainage")$coarse, nf("drainage")$active))

  # --- rivers -------------------------------------------------------------
  for (i in seq_len(nf("river.meander")$active)) {
    f[[sprintf("river.band%d", i)]] <- fp_digest(
      fbm_world(mx, my, octaves = nf_octaves("river.meander"),
                base_wavelength_m = nf_wl("river.meander", i),
                seed = nf_seed("river.meander", i)))
  }
  f[["river.breathe"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("river.breathe"),
              base_wavelength_m = nf_wl("river.breathe"),
              seed = nf_seed("river.breathe")))
  f[["river.ragged"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("river.ragged"),
              base_wavelength_m = nf_wl("river.ragged"),
              seed = nf_seed("river.ragged")))
  f[["river.sandbar"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("river.sandbar"),
              base_wavelength_m = nf_wl("river.sandbar"),
              seed = nf_seed("river.sandbar")))
  f[["river.foam"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("river.foam"),
              base_wavelength_m = nf_wl("river.foam"),
              seed = nf_seed("river.foam")))
  # The meander AMPLITUDES change the channel without touching a seed, so pin
  # them the way drainage.profile pins width and weight.
  f[["river.meander.amp"]] <- fp_digest(nf("river.meander")$amp)


  # --- roads --------------------------------------------------------------
  # The road wiggle is the TERRAIN field read at its own scale -- same seed,
  # 650 m, 3 octaves. That sharing is deliberate; see NOISE_FIELDS$terrain.
  f[["road.wiggle"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("terrain", "wiggle"),
              base_wavelength_m = nf_wl("terrain", "wiggle"),
              seed = nf_seed("terrain")))

  # --- settlements + sacred ----------------------------------------------
  f[["settle.wobble1"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("settle.wobble1"),
              base_wavelength_m = nf_wl("settle.wobble1"),
              seed = nf_seed("settle.wobble1")))
  f[["settle.wobble2"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("settle.wobble2"),
              base_wavelength_m = nf_wl("settle.wobble2"),
              seed = nf_seed("settle.wobble2")))
  f[["settle.wallring"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("settle.wallring"),
              base_wavelength_m = nf_wl("settle.wallring"),
              seed = nf_seed("settle.wallring")))
  f[["sacred.wobble"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("sacred.wobble"),
              base_wavelength_m = nf_wl("sacred.wobble"),
              seed = nf_seed("sacred.wobble")))

  # --- fields that had no fingerprint at all ------------------------------
  # The two path fields and the Worley field-cell id were registered but never
  # pinned, so a change to any of them was invisible to this suite.
  f[["path.settlement"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("path.settlement"),
              base_wavelength_m = nf_wl("path.settlement"),
              seed = nf_seed("path.settlement")))
  f[["path.pilgrim"]] <- fp_digest(
    fbm_world(mx, my, octaves = nf_octaves("path.pilgrim"),
              base_wavelength_m = nf_wl("path.pilgrim"),
              seed = nf_seed("path.pilgrim")))
  f[["settle.fieldcells"]] <- fp_digest(
    ambient::gen_worley(mx, my, frequency = 1 / nf_wl("settle.fieldcells"),
                        seed = nf_seed("settle.fieldcells"), value = "cell"))


  f
}
