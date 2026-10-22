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

  # The canonical codes are the keys of BIOME_INFO, never a literal range. The
  # taxonomy grew from 9 codes to 13 on 2026-10-18, and a hardcoded 0-8 here
  # failed every sample of Savanna, Dry-Summer Scrub, Steppe and Cold Desert --
  # about a third of all land -- on the first export of the new canon.
  .biome_codes <- sort(as.integer(names(BIOME_INFO)))

  test_that("biome codes stay inside the canonical range", {
    # synthesize_biome() must never introduce a class absent from the
    # neighbourhood. The weaker but checkable invariant: no code outside
    # BIOME_INFO.
    v <- terra::rast(here::here("Input Data/HighResolution/biome.vrt"))
    s <- terra::spatSample(v, 4000, method = "regular", na.rm = TRUE)[, 1]
    s <- s[!is.na(s)]
    stray <- setdiff(unique(s), .biome_codes)
    ok(length(s) > 0 && !length(stray),
       sprintf("all %d sampled biome codes are in BIOME_INFO (%d-%d)%s",
               length(s), min(.biome_codes), max(.biome_codes),
               if (length(stray)) paste0(" -- stray: ", paste(stray, collapse = ", "))
               else ""))
  })

  test_that("every biome in canon has a colour, a relief and a vegetation", {
    # Each lookup falls back silently -- grey, no relief, bare ground -- so a
    # biome the data carries but a table omits renders wrong without an error.
    # That is how four biomes shipped as flat grey once. Check every code the
    # 0.05-degree raster actually holds, not just the ones a sample hits.
    present <- sort(unique(terra::unique(
      terra::rast(here::here("Input Data/Climate/biome.tif")))[, 1]))
    present <- present[!is.na(present)]
    for (tbl in c("BIOME_INFO", "BIOME_RELIEF_M", "BIOME_VEG")) {
      miss <- setdiff(as.character(present), names(get(tbl)))
      ok(!length(miss),
         sprintf("%s covers all %d biome codes in canon%s", tbl, length(present),
                 if (length(miss)) paste0(" -- missing ", paste(miss, collapse = ", "))
                 else ""))
    }
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
