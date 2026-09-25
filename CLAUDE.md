# CLAUDE.md — the EART-H map

Guidance for Claude Code working in this repository.

You are looking at the rendering half of a procedurally generated world. The
other half — the pipeline that *made* the world — lives in a separate repository
and is not here. What is here is the canon it produced, and the code that turns
that canon into a map you can zoom into.

Read the whole of this file before making a rendering change. It is mostly a
record of what has already been tried and why it failed, which is the part that
is expensive to rediscover.

**Running things.** `source("setup.R")` installs packages and checks the
machine. `build_reference_map(rebuild_tiles = TRUE)` builds the base pyramid
once and needs the GDAL command-line tools; nothing else does.
`start-tileserver.bat` / `.sh` serves the GM map, and the procedural code runs
from z9 up. `Rscript tests/run-tests.R` is the merge gate. The user is usually
a student who is new to the codebase: explain what you change, and ask them to
look at the tile, because you cannot see it.

**Git and GitHub are your job, not the student's.** They are new to both, and
GETTING-STARTED §10 tells them to ask you in plain words ("start a branch",
"commit this", "open a pull request", "why did the checks fail?"). Say what
each step does as you do it, in a sentence and without jargon. The remotes:
`origin` is the student's fork, `upstream` is the original (set up by
`gh repo fork --clone`). Pass `upstream` to `gh --repo` as OWNER/REPO, from
`git remote get-url upstream`.

- **Starting work:** `git fetch upstream`, fast-forward `main` to
  `upstream/main`, push it to `origin`, then branch from it with a short
  descriptive name. Never do the work on `main`.
- **Committing:** run `git status` first and show the student the file list
  before committing. Never stage `Map/`, `*.backup_*`, or anything under
  `Input Data/` except `Input Data/Annotations/` and NEW files in
  `Input Data/Inkarnate Maps/`. Save & push in the annotation editor rewrites
  canon files that must not be committed. Stage `tests/golden/` only after the
  student deliberately ran `bless-golden.R`, and then name the moved fields in
  the message. Messages say what changed and why.
- **Pull requests:** `git push -u origin <branch>`, then
  `gh pr create --repo <upstream> --base main --head <student>:<branch>`,
  with a body following `.github/pull_request_template.md`. Propose the
  `Expect:` line from what the change touches and ask the student to confirm
  it. Remind them to add before/after screenshots on the web page, because you
  cannot attach images.
- **Checks:** `gh pr checks <n> --repo <upstream>`. For a failure,
  `gh run view <run> --repo <upstream> --log-failed`, then explain the cause.
  The report: `gh run download <run> --repo <upstream> -n map-report`.
- **Never:** force-push, rewrite history that is already pushed, push to
  `main`, or edit or delete tests to make a check pass. If a conflict needs a
  judgment about someone else's work, show both sides and let the student
  decide.

---

## 1. The one idea that explains the architecture

**The world data stops at 0.01°. The map does not.**

At 0.01° — roughly 1.1 km — the rasters here are authoritative. They came out of
a long pipeline: tectonics, hydrology, climate, plant capacity, population,
trade, roads. Below that resolution there is no data, because none was ever
generated.

But a map you can zoom to level 14 needs detail at ~10 m. That detail is
**invented at request time**, by `Functions/TileServer.R`, and then cached.

So every pixel on this map is one of two things:

| | **Canon** | **Synthesis** |
|---|---|---|
| Where | zoom ≤ 8, from the rasters | zoom 9–14, invented |
| Source | `Input Data/` | `TileServer.R` |
| Authority | absolute | must never contradict canon |

`TILE_NATIVE_MAX` (`tiles/core.R:58`) is 8 for both elevation and biome;
`TILE_PROCEDURAL_MAX` is 14. Between them is everything interesting in this
repository.

**The cardinal rule: synthesis elaborates canon, it never overrides it.** A
coastline may wiggle within about one coarse cell of where the data says it is.
It may not put ocean in a continental interior. Micro-relief may add 80 m of
hills to a forest, but may not push inland terrain below sea level and invent a
lake. Every synthesis function in this codebase has a clamp in it for exactly
this reason, and those clamps are load-bearing.

---

## 2. Determinism and seamlessness — read this before touching any noise

This is the deepest invariant in the codebase and the easiest to break without
noticing, because breaking it produces artefacts that only show up at tile
boundaries or when you change zoom.

### All noise is sampled at absolute world coordinates

`fbm_world()` (`tiles/terrain.R:58`) takes EPSG:3857 metres — **not** tile-relative
or pixel coordinates — and evaluates fractal Brownian motion there:

```r
fbm_world(mx, my, octaves = 5, base_wavelength_m = 2000,
          lacunarity = 2, gain = 0.5, seed = WORLD_SEED)
```

Two consequences, and both are the entire point:

- **Adjacent tiles agree at their shared edge**, because they are sampling the
  same continuous field at the same world position. Nothing needs stitching.
- **Zooming in only *adds* octaves.** The low-frequency components are identical
  at every zoom, so the hill you saw at z12 is the same hill at z14, with finer
  texture on it. Detail accrues; the surface is never re-rolled.

`WORLD_SEED` is `1789L` (`tiles/core.R:43`). Changing it re-rolls the entire
sub-cell world and invalidates every cached tile.

**Every field is registered in `Functions/NoiseFields.R`, and that registry is
authoritative — not a description of the numbers, but the numbers themselves.**
There is no literal seed offset, wavelength or octave count anywhere in
`Functions/tiles/`; every one is read back out of the table. `drainage_incision()`
builds its ladder from `NOISE_FIELDS$drainage`, `shape_rivers()` builds its
meander bands from `NOISE_FIELDS$river.meander`, and `add_microrelief()`,
`shape_roads()` and `shape_rivers()` all take their shared wavelength and seed
from `NOISE_FIELDS$terrain`. The fingerprint test reads the registry too, rather
than restating the numbers. A copy is exactly the thing that drifts.

Use the accessors and never write a bare `WORLD_SEED + n` at a call site:

| Accessor | For |
|---|---|
| `nf_seed(name)` / `nf_seed(name, i)` | the seed; `i` indexes a block-allocated ladder |
| `nf_wl(name)` / `nf_wl(name, i)` | wavelength in metres; `i` is a rung index **or** a consumer name |
| `nf_octaves(name, use)` | octave depth; `use` picks among divergent consumers |
| `nf_amp(name, i)` | per-band amplitude, where a ladder carries one |

`terrain` is the field that needs the named forms: one seed read at three
depths and two scales — 6 octaves at 4000 m for relief, 4 at 4000 m for the
road and river pull, 3 at 650 m for the road wiggle. Those are
`nf_wl("terrain", "wiggle")` and friends, not three numbers in three files.

`tests/run-tests.R` scans every file that can draw (`NOISE_SCAN_FILES`, which is
every file in `Functions/tiles/`) and fails on any offset the registry does not
claim, so an unregistered field cannot reach the map from anywhere.

**To add a field:** call `noise_free_offsets()` for an unused offset, add an
entry to `NOISE_FIELDS` naming its consumers, add a line to
`tests/fingerprint-spec.R`, and run the suite.

This exists because several people work on this map at once, on separate forks.
Two people each picking "an unused offset" from what they can see will collide,
and git will merge both cleanly into a world where two unrelated features
secretly share a field. Forcing the registration means they conflict *in the
registry*, in a twenty-line table a human resolves, instead of silently.

### Sample beyond the tile, never clip at its edge

Any operation with a *neighbourhood* — smoothing, distance, warping, hillshade —
must read data from beyond the tile it is rendering, or the result will disagree
with the neighbouring tile along the seam.

`tile_setup()` (`tiles/render.R:40`) reads a **margin of 8 coarse cells, about
8.8 km** (`tiles/render.R:51`), which comfortably exceeds the widest warp (900 m)
and the valleyness smoothing radius. River valleys go further still:
`river_valley_fields()` expands another 1500 m, because a trunk river's valley
reaches ~1.1 km and a river just outside the render pad still has to carve this
tile.

There is also a 10-pixel render pad (`pad_px`) cropped off at write time, purely
so the hillshade has edge context.

When you add a neighbourhood operation, ask: *how far does its influence reach,
and is that inside the margin?* If not, widen the margin or coarsen the
computation — `river_valley_fields()` does the latter, computing distances on a
4× coarsened grid because a smooth profile hides the error.

### The failure mode looks like this

A per-tile `aggregate()` seams, because its block grid is tile-relative.
`valleyness_from_coarse()` (`tiles/terrain.R:93`) exists specifically to replace
the naive `compute_valleyness()` for this reason — the latter survives only as a
fallback when no coarse source exists, and is documented as not seamless.

---

## 3. Coastlines — the fractal technique, and why it is done backwards

This is the part most worth understanding, because the obvious approach is
wrong in a way that is not obvious.

**The obvious approach:** take elevation, threshold at zero, add noise to the
boundary. This fails badly here. `water_class` is the authoritative land/water
truth, and it correctly marks cells that have *slightly positive elevation* as
OCEAN — coastal margins, below-sea-level basins at continent edges. Re-deriving
water from the sign of elevation throws that away and puts land in the sea.

**What `synthesize_coastline()` does instead** (`tiles/core.R:210`): it leaves
the water class alone and **warps the sampling coordinates**.

```
for each output pixel:
    offset its (x, y) by world-seeded noise, amplitude ~900 m
    read the canonical, blocky water_class at that displaced position
```

Every sample is therefore a *real* water_class value from within roughly one
coarse cell. The boundary crenellates organically, with fractal structure at
every scale the noise carries — but the mask can never invent ocean inland or
land in open sea, because it never synthesises a value. It only misreads
position, slightly and smoothly.

This is **domain warping**, and it is the single most reused idea in the
codebase:

- `synthesize_coastline()` — the coast, 900 m warp
- `synthesize_biome()` (`tiles/vegetation.R:39`) — biome boundaries, ~600 m warp,
  3 octaves, so forest and grassland interlock instead of stepping on a grid
- the vegetation openness field warps its own lookup before sampling

The guarantee is stated explicitly in the biome case and holds for all of them:
*the result never introduces a class absent from the local neighbourhood.*

**Both the elevation and biome tiles use the same warp**, so their coastlines
are identical and register pixel-for-pixel.

One subtlety worth knowing: the warp can be **suppressed locally**. `warp_scale`
drives the amplitude to zero near roads, so a crenellation inlet cannot cut
across a road that the routing knows is on land.

---

## 4. Elevation — anchor plus relief

### Upsample with cubicspline, not bilinear

`tile_setup()` projects coarse elevation with `method = "cubicspline"`. This is
not a quality preference. Bilinear interpolation has piecewise-flat slope that
jumps at coarse cell edges, and since hillshade is a function of *slope*, the
result is visibly boxy — the coarse grid shows through as rectangles of shading.
Cubicspline is C1-continuous, so slope is smooth, so the hillshade is smooth.

The upsampled surface is kept as `anchor_fine` **before** anything is added to
it. The anchor matters later.

### Micro-relief: dendritic drainage plus hillslope texture

`add_microrelief()` (`tiles/terrain.R:154`) adds two things to the anchor, with the
split governed by `valley_frac = 0.65` — about two thirds of the relief budget
goes to structure, one third to texture.

**The structural component is the interesting one.** `drainage_incision()`
(`tiles/terrain.R:110`) exploits a property of fractal noise:

> The zero-level set of an fbm field is a connected, branching curve.

So: take a few octaves of noise, find where each is near zero, and lay a thin
Gaussian ridge along it — `exp(-(n/width)^2)`. Sum those. What you get is a
**dendritic valley network with tributaries**, deterministic in world
coordinates, seamless across tiles, stable across zooms. Subtract it from the
anchor and you have carved valleys; the land left between them reads as
interfluve ridges.

This is why the terrain looks like watersheds rather than noise. It is the
highest-value trick in the file, and it generalises — the same zero-set idea
drives `draw_streams()`, which paints faint creeks along the zero contours of
*the same* channel noise (seeds +503/+504), so every creek lies in a valley that
was already carved for it.

**Amplitude is per-biome**, from `BIOME_RELIEF_M` (`tiles/terrain.R:35`), in metres
of local relief for the dominant octave:

```
Ocean 0 · Glacial 20 · Taiga 90 · Desert 60 · Grassland 40
Temperate Forest 80 · Tropical Forest 70 · Mountainous 280 · Tundra 45
```

It is further boosted where the coarse terrain is already steep, so ridges get
rougher and plains stay calm.

**The clamps** (canon protection): water cells are left untouched, and inland
land above `coast_band_m` is never pushed below sea level, so no spurious inland
lakes appear.

### Two-component hillshade

`composite_terrain()` (`tiles/shading.R:47`) shades **twice**, and this solves a
real problem. A single strong hillshade pass over the detailed surface embossed
every micro-relief bump and every carved river wall into saturated light/shadow
pairs at z13+ — it looked like hammered metal.

Instead: the **anchor** is shaded at full `z_factor` (which keeps shading
consistent with the static pyramid and across zooms), and the **detailed
surface** is shaded mildly and only *modulates* that anchor light within
`[detail_lo, detail_hi]`.

Water is painted separately, shaded by smooth distance-to-shore computed on the
coarse window (`SHELF_DIST`), using a **smoothstep** rather than a linear clamp —
a linear ramp has a C0 kink where it saturates, which draws a visible hard line
parallel to straight coasts and reads as rectangular shallow patches.

---

## 5. Biome, vegetation, and how clearings work

This is the subsystem to extend if you want the map to show *land use*.

### The cover model

`apply_vegetation()` (`tiles/vegetation.R:129`) computes, per pixel:

```
vegetated fraction = BIOME_VEG[biome]$cover × sqrt(capacity / VEG_CAPACITY_REF)
```

`cover` is per-biome (Tropical Forest 0.74, Temperate Forest 0.68, Taiga 0.66,
Grassland 0.38, Tundra 0.26, Mountainous 0.22, Desert 0.10, Glacial and Ocean 0).
`capacity` is a canon raster — the plant-carrying capacity the pipeline
computed — and `VEG_CAPACITY_REF` is 200.

That fraction is then **split between dense canopy and open meadow by a
world-seeded "openness" field** (`tiles/vegetation.R:153`): a 6-octave fbm at 3000 m
base wavelength, itself domain-warped by a 1500 m warp. So forest reads as a
mosaic of stands and glades rather than a monotone sheet. Canopy gets an extra
fine mottle (140 m) for within-stand variation.

Everything is multiplied by the underlying terrain luminance, so hillshade relief
still reads through the vegetation, and zeroed on water.

The three lunar plant types (`PLANT_MIX`, mirroring `PlantBuilder`) tint the
foliage — bloom warmer, spreader lighter, root darker — and a global moon wash at
`MOON_TINT_STRENGTH = 0.35` varies with the annual lunar climate.

### Clearings: the `suppress` channel

**This is the extension point.** `apply_vegetation()` takes a `suppress`
argument in [0,1] that thins canopy. In `render_elevation_tile()`
(`tiles/render.R:223-225`) three sources are combined with `max()`:

```r
corr <- corridor_mask(elev, vec$rivers, vec$roads)   # river + road corridors
if (!is.null(sfld)) corr <- max(corr, sfld$f)        # settlement farmland belt
if (!is.null(sfx))  corr <- max(corr, sfx$clear)     # sacred-site clearings
```

So a clearing is just *anything that writes into this mask*. `ROAD_CLEAR_M` is
45 m either side of a road. Sacred sites of tier 4+ clear their surroundings;
tiers 1–3 do the opposite and get a `grove` weight that **boosts** canopy
density and darkens it. Destroyed sacred sites keep their clearing — the scar
remains — but lose the grove, which is a nice piece of world logic expressed
entirely in two rasters.

**If you want to add a new kind of clearing** — charcoal burning, a logged
coupe, a battlefield, a blight — the work is: produce a [0,1] raster on the tile
template, wobble its edge with world-seeded noise so it is not a stamped disc
(see `sacred_fields()` at `tiles/sacred.R:90` for the pattern), and `max()` it
into `corr`. You do not need to touch the vegetation code itself.

Biome is *already* doing real work here — `cover` and the canopy/meadow colours
are per-biome, so a clearing in taiga and a clearing in tropical forest look
different for free. Keep that property.

---

## 6. Rivers and roads — vectors that know about terrain

The road and river geometries come from the pipeline as **routed lines on the
0.01° grid**. Drawn as-is they are ruler-straight staircases. Three layers of
treatment fix that, and the ordering of ideas matters.

### Meander, then terrain-awareness

`meander_lines()` (`tiles/linear.R:53`) densifies each line and offsets every
vertex **perpendicular** to it by world-coordinate noise. Offsets **taper to zero
at the endpoints**, so shared nodes — junctions, confluences — stay put and the
network stays connected. That taper is not optional; without it the graph comes
apart.

But pure noise is *terrain-blind*: the line still cuts through the synthetic
hills the tiles render. So `shape_roads()` (`tiles/linear.R:89`) adds a second
term — each vertex is **pulled down the cross-road gradient of the same fbm field
that `add_microrelief()` uses** (base wavelength 4000 m). Roads therefore swing
around the very knolls that appear at z12+.

That shared field is the key structural decision in this file: **roads and hills
are computed from the same noise, so they cannot disagree.**

`shape_rivers()` (`tiles/linear.R:151`) is the same idea with much wider range —
multi-band meander (valley-scale bends, mid-scale loops, fine wiggle) plus the
same terrain pull, so rivers drift into hollows. And crucially,
`incise_rivers()` carves the valley along the **shaped** geometry, so the valley
follows the bends rather than the original straight route.

All of this is computed **once at cache build**, not per tile (`get_tile_vectors()`,
`tiles/linear.R:392`, ~3 minutes for 13k rivers). Per-tile render cost is unchanged.

### River drawing

Width is by tier (`RIVER_TIER`) but **modulated by two world-anchored noise
bands**: a slow "breathing" so reaches widen and narrow 0.6–1.5×, and a fine
raggedness so banks are not buffer-parallel canal walls. A floor keeps the
channel from pinching shut.

Three refinements worth knowing:

- **Estuaries.** Approaching the shore, the channel widens up to ~4.5× toward the
  `waterness` 0.5 contour, so rivers open into the sea instead of ending as a
  clipped stroke. Sandbars stipple the mouth at z13+.
- **Rapids.** Steep reaches get white flecks at z13+, computed from the **anchor**
  slope, not the carved surface — so foam marks real descents rather than our own
  valley walls.
- **Land clipping.** Vectorised D8 segments continue across below-sea-level
  margin cells, so trunk rivers would otherwise draw across open ocean.

### Road drawing, and the water conflict

Roads draw **above** rivers so crossings read as bridges, with `draw_bridges()`
repainting the deck as stone at z13+.

The interesting problem is roads versus the warped coastline. Roads are land-only
by construction, but the fractal coast warp can grow water a cell or two over a
shore-hugging road. Two mechanisms resolve this, and they pull in opposite
directions on purpose:

1. **The road corridor is carved OUT of the water mask** before compositing
   (`tiles/render.R:203-211`), so the warped lake edge bends *around* the road —
   it reads as a road following the shore.
2. Where a road still crosses water, its over-water pixels are **snapped to the
   nearest shoreline cell** rather than ploughing across or dead-ending.

Narrow navigable rivers are deliberately not in the water mask, so genuine
bridges still draw straight across.

**Ferries** get their own treatment (`split_ferry_edges()`, `tiles/linear.R:252`).
The pipeline's `is_ferry` is a per-*edge* flag, but a typical flagged edge is
~98% land — one 445 km shore-hugging road with a single 6.7 km lake-neck
crossing. Rendering the whole edge as ferry dashes is what made coastal routes
read as chains of fictional ferries. Now only contiguous water runs ≥ 1500 m
become crossings; the rest is ordinary road.

---

## 7. Settlements

`draw_buildings()` (`tiles/settlements.R:366`) places structures three ways, and the
combination is what makes settlements read as places rather than blobs:

1. **Roadside ribbons** — candidate sites every ~30 m along road and lane lines,
   offset to either verge, kept with probability decaying from the core. Villages
   become linear ribbons along their roads, which is how villages actually look.
2. **Lattice core** — a hash-thinned world-anchored lattice (`BLDG_LATTICE_M`,
   34 m) fills the urban core between the ribbons. For ports, density is boosted
   in the near-shore band so buildings crowd the waterfront.
3. **Farmsteads** — sparse scatter across the field belt.

Around them: `settlement_fields()` produces urban-core and farmland intensities
with **noise-wobbled radii** so the clearing edge is organic;
`apply_settlement_ground()` tints a Worley-cell patchwork of field colours over
the farmland and packed earth in the core, shaded by terrain luminance.
Settlements over `WALL_POP_MIN` (8000) get a wall ring whose radius is wobbled by
noise sampled **on a fixed circle around the centre** — a pure function of
bearing, so every tile draws the identical ring — with gates where roads cross.
Ports get deterministic piers, found by marching the `waterness` field toward
water.

Zoom gates: paths at z12 (`SETTLE_PATH_MIN_Z`), buildings at z13
(`SETTLE_BLDG_MIN_Z`), sacred stones at z13 (`SACRED_MIN_Z`).

---

## 8. Compositing order

From `render_elevation_tile()` (`tiles/render.R:186`). Order is significant —
each layer paints over the last:

```
terrain composite (hillshade + elevation palette + water paint)
  └─ vegetation            (suppressed by corridors, settlements, sacred clearings)
      └─ settlement ground (fields, packed earth)
          └─ streams       (faint creeks, under everything blue)
              └─ rivers    (variable width, estuary funnel, rapids)
                  └─ settlement lanes
                      └─ pilgrim paths
                          └─ roads (cased)
                              └─ bridges
                                  └─ buildings
                                      └─ walls
                                          └─ sacred stones
```

---

## 9. Caching — and the trap in it

**Caches invalidate themselves. You do not bump anything.**

Every expensive artefact records what it was built *from* — input files, the
deparsed source of the functions involved, the noise fields consumed, the
constants that matter. `Functions/Provenance.R` computes that as a stamp; if any
of it differs, the cache misses and rebuilds, and says which dependency moved:

```
  rebuilding road geometry: fields changed
  render code changed (built changed) - procedural tiles will re-render on demand
```

| Cache | Depends on | So it rebuilds when… |
|---|---|---|
| `tile_vectors_3857.rds` (roads) | `road_routes.rds`, shaping fns, **`terrain`** | someone changes the terrain field |
| `tile_vectors_3857.rds` (rivers) | river geojsons, shaping fns, **`terrain`**, `river.meander` | …and independently of roads |
| `tile_settlements_3857.rds` | `settlements_final.rds`, `.hash01`, lane + wobble fields | **not** on a terrain change — settlements don't read it |
| `tile_sacred_3857.rds` | sacred sites, `sacred.wobble` | (previously had no guard at all) |
| `Map/tiles/**.png` | ~30 render functions, **every** registered field, the style constants | any rendering change, on demand |

The tile pyramid works by mtime: `sync_render_stamp()` writes `.render-stamp`
only when the hash actually moves, and any PNG older than that file is
re-rendered instead of served. `tile_cache_status()` reports how much of the
pyramid the current code has superseded. Nothing is deleted; stale tiles simply
re-render when next requested.

**The first stamp adopts the pyramid rather than condemning it.** With no
`.render-stamp` there is no way to know what drew the tiles on disk, so the
first write is backdated and everything already there counts as current. After
that, a moved hash condemns normally, and an unreadable stamp is treated as
damage and condemns too. `invalidate_tile_cache()` forces a full re-render.
`adopt_tile_cache()` does the opposite, for a refactor whose output you have
*demonstrated* is unchanged. Use it only with that evidence in hand.

**What the stamp depends on is the FILE, not a list of function names.**
`.prov_render_stamp()` hashes `TILE_DRAW_FILES`, which is every file in
`Functions/tiles/` except `server.R` (HTTP routing is not appearance). A
hand-kept list of functions missed helpers and let edits go unseen. Hashing the
whole file cannot miss one, and `prov_rfile()` parses before hashing, so
comments and formatting do not count.

This matters most for the thing it is hard to notice: roads are shaped by
pulling their vertices down the **terrain** gradient. Change the terrain and the
road geometry is stale — but the person making that change is editing hydrology
and has no reason to think about roads. The dependency is declared, so it is
enforced rather than remembered. There are no hand-bumped `*_VERSION`
integers; do not add one.

---

## 10. The conformance suite

```
Rscript tests/run-tests.R
```

Run it before opening a pull request. It exists because of how this repository
is worked on: several forks at once, merged selectively. The changes that hurt
are not the ones git flags — those get noticed. They are the ones that **merge
cleanly and leave the world subtly wrong**: two features sharing a noise field,
a coarse octave that moved so someone else's baked layer no longer matches the
terrain, a clamp dropped so the coastline invents a lake. None of that errors at
render time.

| Test file | Guards |
|---|---|
| `test-noise-registry.R` | every seed offset is registered and uniquely owned; the shared terrain field still has one seed and one wavelength |
| `test-provenance.R` | caches invalidate on any dependency change; a terrain change reaches the road cache but not the settlement cache; every registered field is inside the render stamp |
| `test-determinism.R` | fields are pure functions of position, order-independent, and unchanged against the golden fingerprint |
| `test-seamlessness.R` | fields are continuous across tile boundaries; warps fit inside the read margin |
| `test-crosszoom.R` | adding octaves **adds** detail rather than re-rolling the surface; the zoom ceiling does not outrun the ladder |
| `test-canon.R` | relief never raises water above the anchor nor sinks inland land below sea level; codes stay in range; nodata is honoured |
| `test-render.R` | tiles actually render: right size, real variation, land and water both present, and the full feature pass runs when its caches are warm |
| `test-output.R` | the invariants on RENDERED pixels: no step at tile edges, z13 children average back to their z12 parent, painted water matches `water_class`, byte-identical output across R processes, and awkward places (open ocean, far north, the antimeridian) render with no opaque black. Limits are multiples of measured values; the numbers are in the file. |
| `test-annotations.R` | drawn features are well-formed: known types with the right geometry, unique ids, roads over land, linked images present |

**On a pull request, CI also runs** (`.github/workflows/map-checks.yml`):
`check-pr-scope.R` (what the diff touched: canon, golden, tests, risky calls,
other people's annotations) and a change report. `render-reference.R` renders
the reference tiles from `reference-tiles.R` on base and branch, and
`compare-renders.R` diffs them into `report.html`. `ci-prepare.R` builds the
vector map and warms the caches first. The PR's `Expect:` line names what the
change should touch; changed tiles with none of it in frame are *collateral*.
When the report shows collateral change, find the cause before calling the
work finished.

**A golden failure is a question, not a verdict.** If you deliberately changed
what the world looks like, the fingerprint *should* move. Re-bless it in the
**same commit** as the code change:

```
Rscript tests/bless-golden.R
```

so a reviewer sees the world change and the code change together. Blessing to
turn a red test green, without knowing which fields moved and why, throws away
the only protection the other forks have.

## 11. Annotations: the hand-authored layer

`tools/annotation-editor.R` (Shiny) is how people add to the world by hand:
drawn roads, trails, rivers, lakes, forests, regions, POIs and labels; renamed
or moved settlements; moved sacred sites; and Inkarnate detail maps (images in
`Input Data/Inkarnate Maps/`, linked from a settlement or a POI). GETTING-STARTED
§8 is the user-facing guide.

- **Save** writes only `Input Data/Annotations/` (`custom_features.geojson`,
  `settlement_names.csv`, `relocated_features.rds`). **Save & push** also runs
  `apply_annotations_to_canon()` (`Functions/AnnotationBuilder.R`), which
  rewrites the binary canon files, and then `build_reference_map()`. Pull
  requests should carry the annotation files and images, not the rewritten
  canon.
- **Drawn features never reach the tiles.** `MapBuilder.R` inlines
  `custom_features.geojson` into the web page as a vector overlay; nothing in
  `Functions/tiles/` reads it. A drawn road is therefore not carved, cleared or
  bridged. That is open work (GETTING-STARTED project 6; see §14), not an
  editor bug.
- `hidden: true` keeps a POI off the player map only. The repo is public.

## 12. Checklist for a rendering change

1. **Which zoom?** Nearly everything is gated. Find the gate before editing.
2. **Is it seamless?** World coordinates, not tile coordinates. Does its
   neighbourhood fit in the 8-cell margin?
3. **Does it respect canon?** Can it place water where the data says land, or
   vice versa? Add the clamp.
4. **Does it need a new noise field?** Register it in
   `Functions/NoiseFields.R` first — `noise_free_offsets()` lists unused
   offsets — then read it at the call site with `nf_seed()` / `nf_wl()` /
   `nf_octaves()`, and add a line to `tests/fingerprint-spec.R`. A bare
   `WORLD_SEED + n` in the source fails `test-noise-registry.R`.
5. **Endpoints pinned?** Any geometry change to roads or rivers must taper to
   zero at nodes.
6. **Caches handle themselves** — but read the rebuild messages. If you changed
   a shaping function and nothing rebuilt, your stamp is missing a dependency;
   add it in the relevant `.prov_*_stamp()`.
7. **Look at a tile at z12, z13 and z14.** Artefacts are zoom-specific.
8. **Run `Rscript tests/run-tests.R`** before opening a pull request.

## 13. Things that have already been tried and failed

Kept so they are not re-attempted:

- Deriving land/water from the sign of elevation. Loses the canonical ocean
  marking on slightly-positive coastal cells.
- Bilinear upsampling of elevation. Boxy hillshade.
- A single strong hillshade pass on the detailed surface. Embossed every bump at
  z13+.
- A linear clamp for the offshore shelf ramp. Visible hard line along straight
  coasts.
- Per-tile `aggregate()` for valleyness. Seams, because the block grid is
  tile-relative.
- Pure-noise meander on roads with no terrain term. Roads cut through synthetic
  hills.
- 16 m noise meander on rivers. They read as canals.
- Treating a whole `is_ferry` edge as a ferry. Coastal roads became chains of
  fictional ferries.
- Fixed-radius discs for settlement and sacred clearings. Read as stamps.

## 14. Known gaps — open work, not defects

These are deliberate absences, listed as student projects in
`GETTING-STARTED.md`. If the user is working on one, they are extending the
model, not fixing a bug — and the "where to start" notes there are the agreed
direction. Do not quietly patch around them.

| Gap | Detail |
|---|---|
| **Roads have no hierarchy** | One width for every road (`ROAD_CASE_PX`/`ROAD_FILL_PX`), unlike rivers' three `RIVER_TIER`s. No maximum grade, so roads climb any slope without switchbacks. No surface classes, embankments or cuttings. Importance is derivable from centrality/population in `Input Data/Combined/`. |
| **Shading is one light, no shadows** | `composite_terrain()` has no cast shadows, no ambient occlusion, no sky tint. Cast shadows are the biggest available win and the hardest seamlessness problem in the repo — a low-sun shadow can exceed the 8-cell margin. |
| **No detail below ~60 m** | Finest `drainage_incision()` octave is 130 m; micro-relief bottoms out near 60 m. Raising `TILE_PROCEDURAL_MAX` past 14 alone yields a blurry enlargement. The `specs` ladder is the intended extension point. Seeds `+503/+504` are shared with `draw_streams()` on purpose — do not renumber. |
| **Buildings are pixels** | 1 px at z13, 2×2 at z14. No footprint, orientation or roof; no street network inside a core, no plot subdivision, no civic buildings. Fields are Worley cells rather than open-field strips. Settlements ignore terrain steepness. |
| **Vegetation ignores slope and aspect** | `apply_vegetation()` reads elevation and valleyness but never slope or aspect, so there is no treeline, no sun/shade contrast, no scree. No within-biome species mix or individual crowns. Any slope/aspect must come from the **anchor**, as `draw_rivers()` rapids already do. |
| **Drawn features never reach the tiles** | `custom_features.geojson` is a web-page overlay only (§11). Start with roads: fold `road`/`trail` features into `get_tile_vectors()` through `shape_roads()`, add the file to `.prov_roads_stamp()`'s `source`, snap endpoints to the network, hide the page overlay from z9. A drawn lake is authored canon and may override `water_class`; noise may not. |

## 15. Repository layout

**The engine is one file per subsystem.** `Functions/TileServer.R` is a loader;
the code lives in `Functions/tiles/`. The split follows the section boundaries
the single file already had, and it exists because several people work on this
map at once and were all editing the same 2,659 lines. Each of the six projects
in `GETTING-STARTED.md` now owns a file, and each file is its own provenance
dependency — see §9.

| `Functions/tiles/…` | What | Project |
|---|---|---|
| `core.R` | seed, zoom ceilings, coordinates, coarse reads, colour ramp | shared |
| `terrain.R` | noise, the drainage ladder, micro-relief | 3 |
| `vegetation.R` | biome edges, the botany overlay | 5 |
| `shading.R` | the two-component hillshade composite | 2 |
| `linear.R` | rivers and roads: shaped, then drawn | 1, 6 |
| `settlements.R` | clearings, fields, lanes, buildings, walls | 4 |
| `sacred.R` | groves, clearings, standing stones | — |
| `render.R` | tile assembly, provenance, the cache | shared |
| `server.R` | HTTP routes and the launcher | — |

> Line citations in this file name a file and a line. They drift — the numbers
> were re-derived from the symbols they point at, not carried over. If one lands
> somewhere unrelated, search for the named function instead; the names are the
> real reference and the numbers are a convenience.

| Path | What |
|---|---|
| `Functions/TileServer.R` | loader for the engine above — source this, not the parts |
| `Functions/MapBuilder.R` | static pyramid, `build_reference_map()`, player map |
| `Functions/map_template.html` | the Leaflet page data is inlined into |
| `Functions/MapRoot.R` | resolves where `Map/` lives; never build that path by hand |
| `Functions/NoiseFields.R` | the noise-field registry (§2) |
| `Functions/Provenance.R` | cache stamps (§9) |
| `Functions/run-tileserver.R` | what `start-tileserver.bat` / `.sh` run |
| `setup.R` | package install + environment check; the one package list |
| `Functions/AnnotationBuilder.R` | author annotations → canon |
| `tools/annotation-editor.R` | Shiny editor for moving features, drawing rivers |
| `Input Data/` | the canon. Treat as read-only. |

`Map/` is **generated** and gitignored. Never commit it.

## 16. Conventions

- Paths resolve through `here::here()`; the map root through `map_root()` /
  `map_path()` from `MapRoot.R`. Do not construct map paths by hand — the map
  directory is not necessarily inside this repository.
- Rasters here are **integer copies** of Float32 originals. Elevation and water
  depth are metres, precipitation mm, biome the 0–8 codes, water_class 0–4.
  Temperature is Int16 centidegrees with a GDAL scale of 0.01 stamped on the
  band, which `terra::rast()` applies on read — you get °C.
- Comment *why*, not *what*. This codebase is dense with non-obvious choices and
  the comments explaining them are the most valuable thing in it. Match that.
