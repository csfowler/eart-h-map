# Getting started

This repository holds the map of EART-H: the code that draws it, and the world
data it draws from. You will be changing the code (adding detail, improving how
terrain and settlements are rendered) and sending your changes back as pull
requests.

You do not need to have built a map before. Setup takes about an hour, most of
it spent waiting, plus a long first build that you can leave running.

---

## 1. GitHub, and your own copy of the repository

The project lives on GitHub, a website that hosts code and tracks every change
to it. You never edit the original directly. You get your own copy (a
**fork**), make changes there, and send them back as a **pull request**: a
proposal the maintainer reviews and can merge. You will not need to learn the
git commands behind this; Claude Code runs them for you (section 10). You only
need the tools installed and signed in, once.

**a. Make a GitHub account** at <https://github.com/signup>, if you do not have
one.

**b. Install git** (the version-control tool) **and the GitHub CLI** (`gh`, which
lets Claude open pull requests and read the automatic checks for you):

| | |
|---|---|
| **Windows** | In PowerShell: `winget install --id Git.Git -e` then `winget install --id GitHub.cli -e`. Close and reopen PowerShell afterwards. |
| **macOS** | `xcode-select --install` (git), then `brew install gh` ([Homebrew](https://brew.sh)) |
| **Linux** | `sudo apt install git gh` (or your distribution's equivalent) |

**c. Sign in and tell git who you are**, once:

```bash
gh auth login
```

Choose **GitHub.com**, **HTTPS**, answer **Yes** to "authenticate Git with your
GitHub credentials", then **Login with a web browser**. Then:

```bash
git config --global user.name  "Your Name"
git config --global user.email "you@example.com"
```

Every commit you make is public and carries this name and email. If you would
rather not publish your email, use the private address GitHub gives you under
*Settings → Emails* (it ends in `@users.noreply.github.com`).

**d. Fork and download the repository.** Put it somewhere with a **short
path**. From the folder that will hold it (`C:\` on Windows, your home folder
elsewhere):

```bash
gh repo fork https://github.com/csfowler/eart-h-map --clone -- --depth 1
```

That creates your fork on GitHub and downloads it into `eart-h-map`, already
connected to both your fork and the original.

> **Windows path length.** Windows refuses paths over 260 characters, and a
> write that fails that way reports as `cannot write file`, which looks like a
> permissions problem. `C:\eart-h-map` is safe; a clone under
> `Documents\Classes\...\repos\` may not be.

## 2. Install R and RStudio

- **R 4.3 or newer**: <https://cran.r-project.org/>. On Windows, also install
  the matching **Rtools** (<https://cran.r-project.org/bin/windows/Rtools/>),
  which the spatial packages need to compile.
- **RStudio Desktop**: <https://posit.co/download/rstudio-desktop/>. Install it
  after R.

## 3. Install Claude Code

Claude Code is the assistant that does most of the writing. It runs in a
terminal, reads this repository, and edits files in place. Use the native
installer:

```powershell
irm https://claude.ai/install.ps1 | iex                  # Windows PowerShell
```
```bash
curl -fsSL https://claude.ai/install.sh | bash           # macOS / Linux
```

Check that it worked with `claude --version`. You will be asked to sign in the
first time you run it. Other install methods are listed at
<https://docs.claude.com/en/docs/claude-code/setup>.

## 4. Install GDAL

The first build draws the base map (zoom 0–8) with the GDAL command-line tools.
R's own spatial packages do not include them.

| | |
|---|---|
| **Windows** | Install [OSGeo4W](https://trac.osgeo.org/osgeo4w/) → *Express Install* → tick **GDAL**. Keep the default location, `C:\OSGeo4W`. It also brings Python. |
| **macOS** | `brew install gdal` ([Homebrew](https://brew.sh)) |
| **Linux** | `sudo apt install gdal-bin python3-gdal` (or your distribution's equivalent) |

## 5. Open the project and install the packages

Open `eart-h-map.Rproj` in RStudio. This sets the working directory, which
everything depends on. Then, in the console:

```r
source("setup.R")
```

It installs the R packages that are missing (10–20 minutes the first time) and
then checks the machine. Every line should say `ok`. If `terra` or `sf` fails
on Windows, Rtools is missing. If GDAL is `MISSING`, restart RStudio after
step 4 and run `setup.R` again.

## 6. Build the map

The **first** build renders the base tile pyramid from the rasters (about
67,000 small images), so start it and leave it running:

```r
source("Functions/MapBuilder.R")
build_reference_map(rebuild_tiles = TRUE)    # 1–3 hours, once
```

This writes `Map/`, which is gitignored and never committed. Every later build
reuses the tiles and takes a couple of minutes:

```r
build_reference_map()
```

## 7. Look at it

| | GM map | Player map |
|---|---|---|
| Windows | `start-tileserver.bat` | `start-map.bat` |
| macOS / Linux | `bash start-tileserver.sh` | `bash start-map.sh` |
| Zoom | up to 14, detail invented on demand | up to 8, static |
| Writes files? | **yes** | never |

**Use the tile server for development.** Zoom levels 9 and up are where all the
code you will write actually runs. The player map is a static export and does
not run any of it.

Open <http://127.0.0.1:8765/index.html> and zoom in on a coastline.

The tile server **saves the tiles it invents** into `Map/tiles/` as a cache.
You never manage that cache: each tile records the code that drew it, so when
you change the rendering, affected tiles re-render on their next request.
`tile_cache_status()` shows how many are waiting to catch up.

---

## 8. Adding to the world: the annotation editor

The code draws the world; the **annotation editor** is how you add to it by
hand. Use it for a road the network is missing, a river or lake, a named forest
or mountain range, a landmark, or a hand-drawn Inkarnate map of a place. It is
a point-and-click map (a Shiny app) that writes small, readable files into
`Input Data/Annotations/`.

Start it from the repository root:

```bash
Rscript tools/annotation-editor.R          # opens in your browser
```

or, in RStudio, `shiny::runApp("tools/annotation-editor.R")`. It works before
you have built the map, but without a basemap. With `start-tileserver` running
it can zoom to 14, which is what you want for placing things precisely.

| Mode | What you can do |
|---|---|
| **Draw new** | Draw a line (road, trail, river), a polygon (region, lake, forest, mountain) or a point (POI, label). Pick the type in the toolbar first; a form then asks for the name and description. |
| **Edit features** | Click anything you drew to rename it, change its type or colour, delete it, or drag a point. |
| **Settlements** | Drag a settlement to a better spot, give it a story name, or link an Inkarnate map to it. |
| **Sacred sites** | Drag a sacred site (choose the tier first). It warns you if you leave its hex. |

**Adding an Inkarnate map.** Save the image (`.jpg` or `.png`) into
`Input Data/Inkarnate Maps/` and give it a clear name, such as `Rukgokai.jpg`
or `Old Ferry Crossing.jpg`. Then, in the editor, either click a settlement
(**Settlements** mode) or draw or click a **POI**, and pick the image from the
*Inkarnate detail map* list. On the built map, clicking that place opens your
image. The list is read fresh each time, so there is no need to restart.

**Save vs. Save & push.** **Save** writes your edits to `Input Data/Annotations/`
and nothing else. **Save & push to map** also bakes them into the canon files
(`settlements_final.rds`, the sacred-site data) and rebuilds `Map/`, so you can
see the result in the map. Push as often as you like locally, but your pull
request should contain only `Input Data/Annotations/` and any new images. The
canon files are binary and will conflict with everyone else's.

**Two things to know:**

- **Drawn features are overlays on the web map. The tile engine does not read
  them.** A road you draw shows as a line at every zoom, but it is not carved
  into the terrain and gets no cleared verge or bridge in the zoom 9–14 tiles
  the way pipeline roads do. Making the renderer honour them is
  project 6 under *Things to work on* below.
- **Nothing here is secret.** The editor has a *hidden* checkbox, which keeps a
  POI off the player map. This repository is public, though, so anything you
  save can be read on GitHub.

---

## 9. Working with Claude on this repo

Start Claude Code from the repository root (`cd C:\eart-h-map`, then `claude`).
It reads `CLAUDE.md` automatically: how the map is built, why, and which
invariants must not be broken. **Read it yourself too**, so you can tell when a
suggestion is heading somewhere bad.

- **Say which zoom you are looking at.** "The coastline looks too smooth at z13"
  is actionable; "the coastline looks wrong" is not. Almost every feature is
  gated on zoom.
- **Ask for the reasoning before the edit.** `CLAUDE.md` records a lot of
  failed approaches, and a good answer will cite one.
- **You are the eyes.** Claude cannot see the map. Re-render, zoom in, take a
  screenshot, describe what you see.
- **Change one thing at a time.** The procedural fields interact on purpose
  (the noise that shapes roads also shapes hills), so with two changes you
  cannot tell which one caused what you see.

### A first task to get oriented

> Look at `BIOME_RELIEF_M` in `Functions/tiles/terrain.R`. Desert is currently 60 m
> of local relief. Real dune fields have a very different character from the
> rolling hills that number produces. Read how `add_microrelief()` uses it, then
> propose how desert terrain could get its own treatment rather than just a
> different amplitude.

Then re-render a desert tile and see whether you believe it.

---

## 10. From an idea to a pull request

Claude runs the git and GitHub commands; you decide what happens. Each piece
of work follows the same loop. The quoted lines are the kind of thing you say
to Claude, and your own words are fine.

1. **Start fresh.** *"Sync my fork and start a new branch for desert dunes."*
   Claude brings your fork up to date with the original, then makes a
   **branch**: a separate line of work, so each project is its own pull request.
   Do this every time you start something new. The original changes as other
   people's work is merged, and starting from an old copy is how merge
   conflicts happen.

2. **Work and look.** Make the change with Claude, render tiles, look at them.
   Run the tests (`Rscript tests/run-tests.R`, or *"run the tests"*).

3. **Save a checkpoint.** *"Commit this."* A **commit** is a saved snapshot
   with a message saying what changed and why. Claude lists the files going
   into it first. **Read that list.** It should hold your code, and nothing
   from `Map/` or the canon files in `Input Data/` (only
   `Input Data/Annotations/` and new images belong there). Commit whenever
   something works; small commits are easier to undo.

4. **Send it back.** *"Open a pull request."* Claude **pushes** your branch to
   your fork on GitHub and opens a pull request to the original, filling in
   the template: what changed, why, and the `Expect:` line. Check what it
   wrote, then open the link it gives you and **drag your before/after
   screenshots into the description**. Claude cannot add images.

5. **Wait for the checks.** Two checks run on the pull request (see *What
   happens to your pull request* below); the report takes about half an hour. Ask
   *"How are the checks doing?"* If one fails, *"Why did the checks fail?"*:
   Claude reads the log and explains it. *"Download the map report"* fetches
   the before/after page for you to open.

6. **Respond to review.** The maintainer may ask for changes in comments on
   the pull request. Make them on the **same branch**, then *"commit and
   push"*: the open pull request updates itself, and the checks run again.

7. **After it is merged**, go back to step 1 for the next piece of work.
   Merged is not quite final: accepted changes are re-tested against the
   full-precision world data before the next release, and that release
   replaces what is in the repository.

**Things to avoid:**

- Working directly on `main`. Always use a branch (step 1).
- Editing or deleting tests to make a check pass. The checks exist to catch
  what you cannot see, and a changed test is flagged to the maintainer anyway.
- Committing files you did not mean to change. If the list in step 3 has
  something unexpected in it, ask Claude why before going on.
- Very large new files. Inkarnate images over 15 MB fail the checks.

---

## Things to work on

Six open problems. None of them is a tidy exercise with a known answer — each
is somewhere the map is currently thinner than it should be, and each has a
shallow end you can reach in an afternoon and a deep end that could occupy a
semester.

Read the matching section of `CLAUDE.md` before starting any of them. The
invariants there are not style preferences: break determinism or seamlessness
and your change will look fine on one tile and visibly wrong at every seam.

### 1. More realistic roads

**Where it lives.** `Functions/tiles/linear.R`.

**Where it stands.** Every road on this map is the same width —
`ROAD_CASE_PX = 3.4`, `ROAD_FILL_PX = 1.7`, scaled only by zoom. Compare rivers,
which have three tiers with their own widths and valley profiles
(`RIVER_TIER`). Roads got the meander and terrain-following treatment
(`shape_roads()`) but never got a hierarchy.

**What is missing.** A trunk route between two cities looks identical to a
farm track. Roads climb any gradient without complaint — there is no maximum
grade, so a road will go straight up a 30% slope where a real one would
switchback. There is no surface distinction between a paved approach to a city
and a hill track. Bridges exist, but embankments and cuttings do not, so roads
never visibly modify the ground they cross.

**Where to start.** `RIVER_TIER` is the model to copy. The network data in
`Input Data/Combined/` carries population and centrality for every node, so
road importance is derivable rather than invented. Start by widening roads
between large settlements and see whether the map suddenly reads as a network.

**Watch for.** Endpoint pinning — any change to road geometry must taper to zero
at junctions or the network comes apart. You do not need to bump anything for
the cache: the road geometry records the functions and noise fields it was
built from, so editing `shape_roads()` invalidates it by itself. If you change
a shaping function and nothing rebuilds, the stamp is missing a dependency —
add it to `.prov_roads_stamp()` rather than deleting the cache by hand.

### 2. More three-dimensional appearance

**Where it lives.** `Functions/tiles/shading.R`.

**Where it stands.** Relief comes from a two-component hillshade in
`composite_terrain()`: the smooth anchor is shaded at full strength for
cross-zoom consistency, and the detailed surface only modulates that light
within a narrow band. The composite is 55% colour, 45% relief. One light
source, no shadows, no occlusion.

**What is missing.** Cast shadows are the single biggest available win — a low
sun throwing long shadows off ridges reads as three-dimensional instantly, and
nothing here does it. Nor is there ambient occlusion to darken valley floors and
the insides of gorges, sky-light tinting that would make shadowed slopes cooler
than lit ones, or building shadows at z14.

**Where to start.** Ambient occlusion is the easier of the two and needs only
the elevation already in memory. Cast shadows are harder and more rewarding.

**Watch for.** This is the project where seamlessness bites hardest: a shadow is
cast *from terrain outside the tile*, so the 8-cell margin may not be enough —
a long shadow at low sun can reach several kilometres. Work out the maximum
shadow length before you write anything. Also read the note in `CLAUDE.md` about
the single strong hillshade pass that already failed once by embossing every
bump; the anchor/detail split exists to prevent exactly that, and new shading
has to respect it.

### 3. Higher resolution zoom

**Where it lives.** `Functions/tiles/terrain.R` (the ladder) and
`Functions/tiles/core.R` (the ceiling).

**Where it stands.** `TILE_PROCEDURAL_MAX` is 14. At z14 one pixel is about
9.6 m at the equator.

**The catch, and it is the interesting part.** Raising that constant will not
work on its own. The finest drainage octave in `drainage_incision()` has a
wavelength of 130 m, and the micro-relief noise bottoms out around 60 m. There
is *no detail in the model* below about 60 m, so z15 and z16 would render a
smooth, blurry enlargement of z14 — more pixels, no more information.

**Where to start.** The `specs` ladder in `drainage_incision()` is already built
for this. It carries four wavelengths (7000, 3000, 1300, 600 m) and adds two
finer ones (280, 130 m) once the pixel size drops to 20 m, with the comment
that adding octaves only *adds* detail because the coarser ones are unchanged.
Extending that ladder is the intended move. Then add matching fine octaves to
`add_microrelief()`, and — the real work — invent features that only make sense
at 2 m: field boundaries, hedgerows, individual tree crowns, building
footprints, tracks.

**Watch for.** Octave seeds are `WORLD_SEED + 500 + i`, so new entries get new
seeds automatically — but `+503` and `+504` are *deliberately* shared with
`draw_streams()`, which draws creeks along the zero-set of those same octaves.
Do not renumber them. Also note that tile count grows as 4^z: z16 is sixteen
times as many tiles as z14, and they are rendered on demand and cached forever.

### 4. Settlements that look real

**Where it lives.** `Functions/tiles/settlements.R`.

**Where it stands.** Buildings are placed three ways — ribbons along roads every
~30 m, a hash-thinned 34 m lattice filling the urban core, and scattered
farmsteads — then painted as **a single pixel at z13 and a 2×2 block at z14**.
Around them sit a Worley-cell patchwork of field colours, a wall ring with gates
for settlements over 8000 people, and piers for ports.

**What is missing.** A building has no footprint, no orientation and no roof; it
is a coloured cell. There is no street network *inside* a town — only the radial
approach paths and the lattice between them — so no blocks, no plot
subdivision, no market square, no distinction between a civic building and a
cottage. The fields are Worley cells, which look like territory rather than the
long narrow strips of open-field agriculture. And settlements ignore terrain:
nothing stops a town sprawling evenly across a steep hillside.

**Where to start.** Give each building an orientation taken from the bearing of
the nearest road, then a rectangular footprint rather than a square pixel. That
one change is what makes a cluster of dots start reading as a village.

**Watch for.** Every property of every building must be a pure function of its
world position — that is what `.hash01()` is for. Derive orientation and size
from the hash, never from anything tile-relative, or buildings will change shape
as you pan.

### 5. More diverse plants and trees

**Where it lives.** `Functions/tiles/vegetation.R`.

**Where it stands.** Two classes per biome: `canopy` and `meadow`, split by a
world-seeded openness field so forest reads as stands and glades. Cover is
`BIOME_VEG[biome]$cover × sqrt(capacity / 200)`. The three lunar plant types
(bloom, spreader, root) tint the foliage via `PLANT_MIX` and `TYPE_MOD`.

**What is missing.** The biggest gap is that **vegetation ignores slope and
aspect entirely.** It reads elevation and valleyness, but never asks which way a
hillside faces or how steep it is. So there is no treeline, no contrast between
a sun-facing and a shaded slope, no bare scree on steep ground — all of which
are among the most legible features of real vegetated terrain. Within a biome
there is also no species mix, no distinction between riparian and upland
species, no age or succession structure, and no individual crowns even at z14
where a tree would be two pixels across.

**Where to start.** Aspect is one `terra::terrain()` call away, and using it to
shift the canopy/meadow balance would give you sun-facing and shade-facing
contrast immediately. A treeline is a smoothstep on elevation. Both are small
changes with a large visual return, and they compose with everything already
there.

**Watch for.** Take slope and aspect from the **anchor**, not from the carved
and micro-relieved surface. The rapids in `draw_rivers()` already do this
deliberately — they mark real descents rather than our own valley walls. Compute
vegetation from the detailed surface instead and the forest will follow the
noise you invented rather than the terrain the world actually has.

### 6. Drawn features that belong to the terrain

**Where it lives.** `Functions/tiles/linear.R` (`get_tile_vectors()` and its
provenance stamps). The drawn features themselves are in
`Input Data/Annotations/custom_features.geojson`, and
`read_custom_features()` in `Functions/AnnotationBuilder.R` reads them.

**Where it stands.** Everything drawn in the annotation editor (§8) reaches the
map only as a vector overlay on the web page. Nothing in `Functions/tiles/`
reads `custom_features.geojson`. So a hand-drawn road is a line floating over
the terrain: no cleared verge, no bridge where it crosses a river, no pull
around hills. A drawn river carves no valley. A drawn lake is not water, and a
drawn forest is not trees.

**What is missing.** Drawn features should be treated like the pipeline's own:

- roads and trails shaped and painted with the real network
- rivers incised and drawn by tier
- lakes entering the water mask
- forest polygons feeding the vegetation `suppress`/grove channel the way
  sacred groves do

Each feature type is its own sub-project, so this scales from an afternoon to a
semester.

**Where to start.** Roads. In `get_tile_vectors()`, read the `road` and `trail`
features, transform them to EPSG:3857, run them through `shape_roads()`, and
append them to `.tilevec$roads`. Then add `custom_features.geojson` to the
`source` list in `.prov_roads_stamp()`. Without that, drawing a new road never
invalidates the cached geometry, and the change looks like it did nothing.
Trails want a narrower width than roads, which is a natural first step toward
project 1's road hierarchy.

**Watch for.**

- **Endpoints.** A hand-drawn road's ends will not land exactly on the
  network, so snap them to the nearest road vertex or settlement within some
  tolerance, or the junction will show a gap at z14.
- **Shaping.** A hand-drawn line is already authored, so decide whether it
  should be meandered at all.
- **Water.** Roads are carved *out* of the water mask (`CLAUDE.md` §6). A drawn
  road across a lake would eat the lake unless its water crossing becomes a
  ferry.
- **Drawn twice.** Once features render into the tiles, the web page still
  draws its own overlay on top. Hide the overlay from z9 up in
  `Functions/map_template.html`, or everything shows twice.
- **Precedence.** A drawn lake is authored canon, so it may override the
  `water_class` raster. The procedural noise may not. Keep that distinction
  explicit in the code.

---

## Troubleshooting

**`rebuild_tiles = TRUE needs the GDAL command-line tools`.** Step 4 is not
done, or RStudio was open during the install. Restart it and run
`source("setup.R")`.

**`cannot write file`.** Almost always the Windows path limit. Move the
repository closer to the drive root.

**`Cannot locate the EART-H Map directory`.** You have not built the map yet
(step 6), or R is not running from the project. Open `eart-h-map.Rproj`.

**Water where land should be.** A raster is being read without its nodata value.
See the canon section of `CLAUDE.md`. Do not "fix" this by re-deriving water
from elevation.

**A tile looks stale after a code change.** If you added a *new* file to
`Functions/tiles/`, register it in `TILE_DRAW_FILES` (`Functions/tiles/core.R`).
To force a re-render: `tile_path(..., force = TRUE)` for one tile, or
`invalidate_tile_cache()` for all of them.

**Everything is slow.** You only need `rebuild_tiles = TRUE` once.

**Claude says `gh` is not logged in, or a push is refused.** Run
`gh auth login` again (step 1c), then ask Claude to retry.

**A merge conflict.** Your branch and the original both changed the same
lines. Ask Claude to *"bring my branch up to date with the original and walk
me through the conflicts"*. It will show each one and suggest a resolution;
you decide, especially where the other change is someone else's work. Starting
every piece of work from a synced fork (section 10, step 1) makes this rare.

## Before you hand anything back: run the tests

```bash
Rscript tests/run-tests.R
```

It must print `0 failed`. The first run takes a few minutes, because it builds
the road and river geometry cache; after that it takes about ten seconds.

The tests matter because several people change this map at once. The changes
that cause trouble are not the ones git flags. They are the ones that merge
cleanly and leave the world quietly wrong, such as two features sharing a noise
field, or a terrain change that moves the hills someone else's roads were routed
around. Nothing errors and every tile still renders. The tests are the only
thing that catches it.

**A failing test is not automatically a bug.** One test compares every noise
field against a recorded fingerprint, so it *should* fail when you deliberately
change what the world looks like. If you meant the change, re-record the
fingerprint in the same commit and put its output in the commit message:

```bash
Rscript tests/bless-golden.R
```

Blessing blindly, just to turn the test green, removes the protection everyone
else relies on. A new procedural field has to be registered in
`Functions/NoiseFields.R`; the suite shows you which offsets are free.

## What happens to your pull request

Two checks run automatically on every pull request, and both must pass
before it can merge:

- **Conformance tests.** The same `tests/run-tests.R` you ran, plus a scope
  check on what your branch changed. The scope check fails a PR that
  rewrites canon files or hides a POI. It flags anything else a reviewer
  should read: new code that runs shell commands or deletes files, edits to
  existing tests, and changes to other people's drawn features.
- **Change report.** It renders a fixed set of about 40 reference tiles (a
  walled city, a port, a sacred grove, a ferry crossing, a patch of each
  biome, and so on, at z10, z12 and z14), once with `main` and once with your
  branch, and compares them. It fails if your change breaks a tile or makes
  rendering more than twice as slow. The full before/after/difference page is
  attached to the run as `map-report`, and it is what the merge decision is
  made from.

The pull request template asks for an **`Expect:`** line: what your change is
*meant* to touch, e.g. `Expect: roads, settlements`. Any reference tile that
changed but has none of those things in frame is reported as **collateral**.
A roads change that moves a desert tile with no roads on it has a side
effect. Find out why before asking for a merge.

Merging is not the last step. Accepted changes are re-tested against the
full-precision world data before they become canon.

## What to hand back

A pull request from a branch of your fork (section 10). Include:

- **a before/after screenshot** at the zoom where the change is visible
- **the test output** showing `0 failed`
- **if the golden fingerprint moved**, which fields moved and why
- **for annotation work**, only `Input Data/Annotations/` and new images in
  `Input Data/Inkarnate Maps/`, not the canon files that Save & push rewrites
