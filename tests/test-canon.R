# =============================================================================
# test-canon.R — synthesis elaborates canon, it never overrides it
#
# The cardinal rule from CLAUDE.md. A coastline may wiggle within about a coarse
# cell of where the data says it is; it may not put ocean in a continental
# interior. Micro-relief may add 80 m of hills to a forest; it may not push
# inland terrain below sea level and invent a lake.
#
# Every clamp that enforces this is easy to remove by accident while chasing a
# visual effect, and the result looks fine on the tile you are staring at and
# wrong three continents away. These tests read the actual canon rasters, so
# they skip on a machine that has not got them.
# =============================================================================

cat("\n[canon respect]\n")

if (!have_render_data()) {
  skip("Input Data rasters unavailable - canon tests need a full checkout")
} else {

  test_that("the coastline warp cannot exceed one coarse cell", {
    # synthesize_coastline() reads the canonical water class at a DISPLACED
    # position. The guarantee that it never invents water is that the
    # displacement stays inside the neighbourhood -- roughly one coarse cell.
    # Widen the warp past that and the mask starts sampling unrelated terrain.
    coarse_cell_m <- 1100
    warp_default <- formals(synthesize_coastline)$warp_m
    ok(is.numeric(warp_default) && warp_default <= coarse_cell_m,
       sprintf("coast warp %.0f m stays within one coarse cell (%.0f m)",
               as.numeric(warp_default), coarse_cell_m))
  })

  test_that("micro-relief refuses to sink inland land below sea level", {
    # add_microrelief() clamps inland land above coast_band_m so noise cannot
    # carve a spurious lake. Drive it with an exaggerated amplitude over a flat
    # inland plateau: with the clamp, nothing goes negative.
    tmpl <- terra::rast(terra::ext(1.0e6, 1.0e6 + 5000, 2.0e6, 2.0e6 + 5000),
                        ncol = 64, nrow = 64, crs = "EPSG:3857")
    terra::values(tmpl) <- 120                     # flat inland land, 120 m
    biome <- terra::rast(tmpl); terra::values(biome) <- 7   # Mountainous: 280 m relief

    out <- add_microrelief(tmpl, biome_fine = biome, water_mask = NULL,
                           amp_scale = 6)          # deliberately absurd
    v <- terra::values(out)[, 1]
    v <- v[!is.na(v)]
    ok(all(v > 0),
       sprintf("inland land stays above sea level under 6x relief (min %.1f m)",
               min(v)))
  })

  test_that("relief never raises a water cell above the anchor", {
    # The clamp is pmin(), one-sided on purpose: micro-relief may DEEPEN a
    # seabed (invisible - water paint covers it) but must never push one up,
    # which would grow an island the canon does not have.
    tmpl <- terra::rast(terra::ext(1.0e6, 1.0e6 + 5000, 2.0e6, 2.0e6 + 5000),
                        ncol = 64, nrow = 64, crs = "EPSG:3857")
    terra::values(tmpl) <- -40                     # shallow sea floor
    wm <- terra::rast(tmpl); terra::values(wm) <- 1
    wm <- wm > 0                                    # logical water mask

    out <- add_microrelief(tmpl, biome_fine = NULL, water_mask = wm, amp_scale = 4)
    v <- terra::values(out)[, 1]; v <- v[!is.na(v)]
    ok(all(v <= -40 + 1e-9),
       sprintf("water stays at or below the anchor under 4x relief (max %.2f m)",
               max(v)))
  })

  test_that("biome codes stay inside the canonical range", {
    # synthesize_biome() must never introduce a class absent from the
    # neighbourhood. The weaker but checkable invariant: no code outside 0-8.
    v <- terra::rast(here::here("Input Data/HighResolution/biome.vrt"))
    s <- terra::spatSample(v, 4000, method = "regular", na.rm = TRUE)[, 1]
    s <- s[!is.na(s)]
    ok(length(s) > 0 && all(s >= 0 & s <= 8),
       sprintf("all %d sampled biome codes are within BIOME_CODES 0-8", length(s)))
  })

  test_that("water class stays inside its canonical range", {
    v <- terra::rast(here::here("Input Data/HighResolution/water_class.vrt"))
    s <- terra::spatSample(v, 4000, method = "regular", na.rm = TRUE)[, 1]
    s <- s[!is.na(s)]
    ok(length(s) > 0 && all(s >= 0 & s <= 4),
       sprintf("all %d sampled water_class values are within 0-4", length(s)))
  })

  test_that("elevation nodata is honoured, not read as terrain", {
    # The integer-conversion trap: a Float32 NaN nodata converted to Int16
    # without an explicit NAflag becomes -32768 AS DATA. The map still renders;
    # the sea floor is just 32 km down.
    v <- terra::rast(here::here("Input Data/HighResolution/elevation.vrt"))
    mm <- unlist(terra::minmax(v))
    ok(is.finite(mm[1]) && mm[1] > -12000,
       sprintf("elevation minimum %.0f m is a real depth, not a nodata sentinel",
               mm[1]))
    ok(is.finite(mm[2]) && mm[2] < 20000,
       sprintf("elevation maximum %.0f m is plausible", mm[2]))
  })
}
