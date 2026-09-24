# =============================================================================
# TileServer.R - Procedural high-zoom map tiles for EART-H
#
# Serves / synthesizes Web-Mercator (EPSG:3857) XYZ tiles for the reference
# map. For z <= native (data) zoom the static pyramid under Map/tiles/ is
# authoritative. For z > native we SYNTHESIZE a tile from the coarse 0.01deg
# VRTs: a deterministic, seamless, parameter-respecting render of the sub-cell
# world that the source data never specified.
#
# Core principles (see CLAUDE.md "Aggressive Caching" + the project plan):
#   * Deterministic: all detail is a pure function of absolute world position,
#     never of the tile index, so adjacent tiles match at their seams and a
#     re-render is byte-identical.
#   * Anchored: the coarse data is the low-frequency truth; synthesis only adds
#     bounded high-frequency detail on top, so the far field always agrees with
#     canon (elevation sign, biome class, water, etc.).
#   * Write-through cache: a synthesized tile is written into the very path a
#     static tile would occupy (Map/tiles/<layer>/<z>/<x>/<y>.png), so the next
#     request is a plain file read.
#
# THIS FILE IS A LOADER. The engine lives in Functions/tiles/, one file per
# subsystem, because five people work on this map at once and they were all
# editing the same 2,659-line file. The split follows the section boundaries
# this file already had, so each of the five student projects in
# GETTING-STARTED.md now owns a file:
#
#   tiles/core.R         configuration, coordinates, coarse reads, colour ramp
#   tiles/terrain.R      noise, drainage, micro-relief        (project 3)
#   tiles/vegetation.R   biome edges, the botany overlay      (project 5)
#   tiles/shading.R      hillshade composite                  (project 2)
#   tiles/linear.R       rivers and roads, shaped then drawn  (project 1)
#   tiles/settlements.R  clearings, fields, buildings, walls  (project 4)
#   tiles/sacred.R       groves, clearings, standing stones
#   tiles/render.R       tile assembly, provenance, the cache
#   tiles/server.R       HTTP routes and the launcher
#
# Source order matters: each file may use values defined by an earlier one at
# SOURCE time (constants), though function-to-function calls resolve lazily and
# may point forwards.
#
# Rendering reuses ELEVATION_PALETTE / BIOME_INFO / WATER_CLASS from the rest of
# the pipeline so synthesized tiles are stylistically identical to the static
# ones.
# =============================================================================

suppressMessages({
  library(terra)
  library(ambient)
})

# Palettes + GDAL config live in MapBuilder.R; source it once so styling stays
# in lockstep with the static pyramid. (It is heavy but idempotent.)
if (!exists("ELEVATION_PALETTE")) {
  source(here::here("Functions/MapBuilder.R"))
}
if (!exists("WATER_CLASS")) {
  source(here::here("Functions/WaterClass.R"))
}
# map_root(): Map/ no longer lives under the project root. Sourced explicitly
# rather than relying on MapBuilder having pulled it in, because this file is
# the one that WRITES tiles and must never guess the pyramid's location.
if (!exists("map_root")) {
  source(here::here("Functions/MapRoot.R"))
}
# NOISE_FIELDS: the allocation table for every world-seeded field. Several
# functions below take nf_wl()/nf_seed()/nf_octaves() as argument defaults or
# call them in their bodies. R evaluates those lazily, at call time, so this
# only has to be loaded before the first RENDER -- but sourcing it here keeps
# the dependency visible instead of relying on some other file having pulled it
# in first.
if (!exists("NOISE_FIELDS")) {
  source(here::here("Functions/NoiseFields.R"))
}
# Content-hashed cache provenance, replacing the old hand-bumped *_VERSION
# integers. See Functions/Provenance.R for why an integer cannot express "this
# road cache was built against that terrain field".
if (!exists("prov_stamp")) {
  source(here::here("Functions/Provenance.R"))
}

# A PROJ_LIB pointing at another install's (older) proj.db poisons terra's
# bundled PROJ ("empty srs" / DATABASE.LAYOUT.VERSION warnings on EPSG lookups).
# MapBuilder now scopes its own PROJ_LIB to each GDAL CLI call, but one can
# still arrive from the environment (an OSGeo4W or conda shell). The engine
# renders entirely through terra, so clear it for this process.
Sys.unsetenv("PROJ_LIB")


# -----------------------------------------------------------------------------
# The engine
# -----------------------------------------------------------------------------

TILE_ENGINE_FILES <- c(
  "Functions/tiles/core.R",
  "Functions/tiles/terrain.R",
  "Functions/tiles/vegetation.R",
  "Functions/tiles/shading.R",
  "Functions/tiles/linear.R",
  "Functions/tiles/settlements.R",
  "Functions/tiles/sacred.R",
  "Functions/tiles/render.R",
  "Functions/tiles/server.R"
)

for (.f in TILE_ENGINE_FILES) source(here::here(.f))
rm(.f)
