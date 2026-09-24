# EART-H map

The map of EART-H, a procedurally generated world: the code that draws it and
the world data it draws from. Canon data stops at 0.01° (~1.1 km); below that,
a tile server invents detail on demand down to zoom 14 (~10 m per pixel) and
must never contradict the canon above it.

- **Setting up?** Read [`GETTING-STARTED.md`](GETTING-STARTED.md).
- **Changing how anything is drawn?** Read [`CLAUDE.md`](CLAUDE.md) first. It is
  the design document and lists the invariants. Claude Code reads it
  automatically.

## Layout

| Path | What |
|---|---|
| `Functions/MapBuilder.R` | builds the base tiles and the map pages |
| `Functions/TileServer.R`, `Functions/tiles/` | the procedural tile engine, one file per subsystem |
| `Functions/NoiseFields.R` | the registry of every noise field |
| `tools/annotation-editor.R` | Shiny editor for names, relocations and drawn features |
| `tests/` | the conformance suite. Run it before every pull request |
| `Input Data/` | the canon. Treat it as read-only |
| `Map/` | generated output, gitignored |

## Data notes

The rasters are integer copies of the pipeline's Float32 originals. Elevation
and water depth are in metres, precipitation in mm, biome uses codes 0–8 and
water_class 0–4. Temperature is stored as centidegrees with a GDAL scale of
0.01, so `terra::rast()` returns °C.

Two layers of the full pipeline are deliberately absent. `water_influence` is
optional to the renderer, so lake edges are slightly plainer, and
`Combined/centrality.rds` is only needed for naming, which is already baked in.

Clone with `--depth 1`: compressed rasters do not delta, so history only adds
size.

## License

Code: MIT ([`LICENSE`](LICENSE)). World data in `Input Data/`: CC BY-NC 4.0
([`DATA-LICENSE`](DATA-LICENSE)), except `Input Data/Inkarnate Maps/`, which
contains Inkarnate artwork under Inkarnate's own terms.
