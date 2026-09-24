# =============================================================================
# NoiseFields.R — the allocation table for every world-seeded noise field
#
# WHY THIS FILE EXISTS
#
# Every procedural detail on this map comes from a noise field identified by a
# seed offset from WORLD_SEED and sampled at some wavelength. Those numbers used
# to live as literals at ~40 call sites in TileServer.R. That produced two
# failure modes, and both are silent — no error, no warning, just a subtly wrong
# world:
#
#   1. SEED COLLISION. Two people working on separate forks each pick "an
#      unused offset", because neither can see the other's branch. Git merges
#      both cleanly. Two unrelated features now share a field and are correlated
#      in a way nobody intended.
#
#   2. COUPLING DRIFT. Some fields are deliberately SHARED — roads bend around
#      the same hills the terrain renders because both read the same field at
#      4000 m. That agreement was expressed by the number 4000 appearing in two
#      unrelated function signatures. Change one and nothing breaks loudly; the
#      roads simply stop following the terrain.
#
# Registering a field here does not, by itself, prevent either. What prevents
# them is that tests/test-noise-registry.R REFUSES to pass if TileServer.R uses
# a seed offset that is not in this table. So adding a field forces an edit to
# THIS file — and when two forks both add a field, they conflict HERE, in a
# twenty-line table a human can resolve, instead of merging cleanly into a
# broken map.
#
# The point is not bookkeeping. It is moving an invisible semantic conflict
# somewhere version control can see it.
#
# HOW TO ADD A FIELD
#
#   1. Pick an unused offset. `noise_free_offsets()` lists gaps.
#   2. Add an entry below, with `used_by` naming the call sites.
#   3. Add a fingerprint line in tests/fingerprint-spec.R.
#   4. Run tests/run-tests.R.
#
# HOW TO SHARE A FIELD DELIBERATELY
#
#   List every consumer in `used_by` and say why in `note`. The audit treats a
#   field with several consumers as intentional; what it rejects is an offset
#   nobody declared.
# =============================================================================

if (!exists("WORLD_SEED")) WORLD_SEED <- 1789L

# -----------------------------------------------------------------------------
# The table
# -----------------------------------------------------------------------------
# seed_off : offset from WORLD_SEED. THE identity of the field.
# wl       : base wavelength in metres (a vector for a multi-rung ladder).
# octaves  : octaves summed. A NAMED vector records a deliberate divergence
#            between consumers of the same field — see `terrain`.
# gain     : amplitude falloff per octave (fbm_world default 0.5).
# used_by  : every call site. More than one = a deliberate shared field.
NOISE_FIELDS <- list(

  # --- the shared terrain field ------------------------------------------
  # The most load-bearing entry here. add_microrelief() displaces the ground
  # with it; shape_roads() and shape_rivers() pull their vertices DOWN ITS
  # GRADIENT. That is why roads swing around the knolls that appear at z12+ --
  # they are reading the same field that made them.
  #
  # NOTE THE DIVERGENCE, which predates this table and is preserved exactly:
  # relief sums 6 octaves, the pulls sum 4. fbm_world normalises by summed
  # amplitude (1.969 vs 1.875), so the shared coarse structure reaches the
  # roads about 5% weaker, minus two octaves of fine detail. Harmless at a
  # 4000 m wavelength where the gradient is dominated by rung 1 -- but it means
  # these were never quite the same field, which is the clearest possible
  # argument for writing the coupling down. Unifying them is a real change to
  # the world and must be made deliberately, not as a tidy-up.
  terrain = list(
    seed_off = 0L,
    wl      = c(relief = 4000, pull = 4000, wiggle = 650),
    octaves = c(relief = 6L,   pull = 4L,   wiggle = 3L),
    gain = 0.5,
    used_by  = c("add_microrelief", "shape_roads(terrain pull)",
                 "shape_rivers(terrain pull)", "shape_roads(wiggle)"),
    note = paste("Shared on purpose: terrain and the roads/rivers that avoid it.",
                 "THREE consumers at three depths, all on one seed. The wiggle",
                 "reuses that seed at 650 m, so road jitter is correlated with",
                 "terrain -- currently harmless, but it is why this offset must",
                 "not be reused for anything new. Its 650 m and 3 octaves were",
                 "literals in shape_roads until they were written down here.")),

  # --- coastline and biome warps -----------------------------------------
  # Domain warping: these displace the SAMPLING COORDINATE, never the value.
  # x and y need independent fields or the warp collapses to a diagonal smear.
  coast.warp.x = list(seed_off = 303L, wl = 2000, octaves = 4L, gain = 0.5,
    used_by = "synthesize_coastline", note = "Coast crenellation, x displacement."),
  coast.warp.y = list(seed_off = 404L, wl = 2000, octaves = 4L, gain = 0.5,
    used_by = "synthesize_coastline", note = "Coast crenellation, y displacement."),
  biome.warp.x = list(seed_off = 101L, wl = 1500, octaves = 3L, gain = 0.5,
    used_by = "synthesize_biome", note = "Biome boundary dither, x."),
  biome.warp.y = list(seed_off = 202L, wl = 1500, octaves = 3L, gain = 0.5,
    used_by = "synthesize_biome", note = "Biome boundary dither, y."),

  # --- drainage ladder ----------------------------------------------------
  # Allocated as a BLOCK: drainage_incision() computes seed + 500 + i over its
  # `specs` list, so the offsets are implied by the ladder's length rather than
  # written out. Extending the ladder for deeper zoom (a documented student
  # project) consumes 507, 508, ... -- which is why the reservation runs to 520.
  # This entry is the LADDER ITSELF, not a description of it: drainage_incision()
  # builds its `specs` from these three vectors. They used to be a literal list
  # inside that function with the registry mirroring it, and a mirror is exactly
  # the thing that drifts -- a wavelength edited in one place and not the other
  # changes every valley on the map while every test still passes.
  #
  # wl     : base wavelength, metres. Strictly decreasing.
  # width  : channel half-width in NOISE units for exp(-(n/width)^2).
  # weight : contribution of this rung to the summed incision.
  # coarse : how many leading rungs are always active. The rest switch on at
  #          z13+ (fine_detail), which is what "detail accrues with zoom" means.
  drainage = list(
    seed_off = 501:520,
    wl     = c(7000, 3000, 1300, 600,  280,  130),
    width  = c(0.14, 0.11, 0.09, 0.07, 0.06, 0.05),
    weight = c(1.00, 0.58, 0.28, 0.12, 0.06, 0.030),
    coarse = 4L,
    octaves = 1L, gain = 0.5, block = TRUE, active = 6L,
    used_by = c("drainage_incision", "draw_streams(rungs 3 and 4)"),
    note = paste("Zero-level sets of these give the dendritic valley network.",
                 "draw_streams() draws creeks along the SAME zero sets as rungs",
                 "3 and 4 (offsets 503/504) so every creek lies in a valley that",
                 "was already carved for it. That alias is deliberate: do NOT",
                 "renumber the ladder. 507-520 are reserved for deeper zoom.")),

  # --- rivers -------------------------------------------------------------
  # Block-allocated like the drainage ladder: shape_rivers() computes
  # seed + 40 + i over its `bands` list, so adding a band consumes the next
  # offset. Reserved to 50. Bands are, in order: valley-scale meander (1600 m),
  # mid-scale loops (420 m), fine wiggle (130 m) -- summed, not selected.
  river.meander = list(
    seed_off = 41:50, wl = c(1600, 420, 130), amp = c(130, 45, 14),
    octaves = 3L, gain = 0.5,
    block = TRUE, active = 3L,
    used_by = "shape_rivers",
    note = paste("Multi-band perpendicular meander. Three bands summed give a",
                 "channel that bends at valley scale and wiggles at reach scale;",
                 "a single band read as a canal. `amp` is the offset in metres",
                 "per band. This entry IS the ladder: shape_rivers() builds its",
                 "`bands` from these vectors. They used to be a literal list in",
                 "that function with this table mirroring it, and a mirror is",
                 "exactly the thing that drifts.")),
  river.breathe = list(seed_off = 61L, wl = 1300, octaves = 3L, gain = 0.5,
    used_by = "draw_rivers", note = "Slow width modulation, 0.6-1.5x."),
  river.ragged = list(seed_off = 62L, wl = 170, octaves = 2L, gain = 0.5,
    used_by = "draw_rivers", note = "Bank raggedness; stops banks reading as canal walls."),
  river.sandbar = list(seed_off = 64L, wl = 120, octaves = 2L, gain = 0.5,
    used_by = "draw_rivers", note = "Estuary sandbar stipple, z13+."),
  river.foam = list(seed_off = 65L, wl = 45, octaves = 2L, gain = 0.5,
    used_by = "draw_rivers", note = "Rapids flecks, z13+. Gated on ANCHOR slope."),

  # --- paths --------------------------------------------------------------
  path.settlement = list(seed_off = 77L, wl = 260, octaves = 3L, gain = 0.5,
    used_by = "get_tile_settlements -> meander_lines", note = "Radial settlement lanes."),
  path.pilgrim = list(seed_off = 88L, wl = 160, octaves = 3L, gain = 0.5,
    used_by = "sacred_paths -> meander_lines", note = "Shrine approach paths, z13+."),

  # --- vegetation ---------------------------------------------------------
  veg.openwarp.x = list(seed_off = 730L, wl = 1500, octaves = 3L, gain = 0.5,
    used_by = "apply_vegetation", note = "Warp on the openness lookup, x."),
  veg.openwarp.y = list(seed_off = 740L, wl = 1500, octaves = 3L, gain = 0.5,
    used_by = "apply_vegetation", note = "Warp on the openness lookup, y."),
  veg.openness = list(seed_off = 710L, wl = 3000, octaves = 6L, gain = 0.55,
    used_by = "apply_vegetation",
    note = paste("Splits vegetated cover between dense canopy and open meadow.",
                 "Gain 0.55 (not 0.5) keeps clearings crisp at stand scale.")),
  veg.regional = list(seed_off = 860L, wl = 26000, octaves = 2L, gain = 0.5,
    used_by = "apply_vegetation", note = "Regional lushness drift."),
  veg.hue = list(seed_off = 862L, wl = 42000, octaves = 2L, gain = 0.5,
    used_by = "apply_vegetation", note = "Very slow hue drift across a continent."),
  veg.sheen = list(seed_off = 863L, wl = 1600, octaves = 3L, gain = 0.5,
    used_by = "apply_vegetation", note = "Foliage sheen."),
  veg.brown = list(seed_off = 864L, wl = 13000, octaves = 2L, gain = 0.5,
    used_by = "apply_vegetation", note = "Seasonal browning patches."),
  veg.mottle = list(seed_off = 800L, wl = 140, octaves = 3L, gain = 0.5,
    used_by = "apply_vegetation", note = "Within-stand canopy mottle."),
  veg.mottle2 = list(seed_off = 820L, wl = 90, octaves = 3L, gain = 0.5,
    used_by = "apply_vegetation", note = "Finer second mottle."),
  veg.typesel = list(seed_off = 760L, wl = 380, octaves = 4L, gain = 0.5,
    used_by = "apply_vegetation", note = "Picks the lunar plant type (PLANT_MIX)."),
  veg.standmix = list(seed_off = 815L, wl = 260, octaves = 3L, gain = 0.5,
    used_by = "apply_vegetation", note = "Stand composition variation."),
  veg.crownbump = list(seed_off = 830L, wl = 58, octaves = 2L, gain = 0.5,
    used_by = "apply_vegetation",
    note = paste("Crown bump-shading, z12+. Sampled TWICE at a 9 m offset to",
                 "get a gradient -- same seed by design, not a collision.")),
  veg.crevasse = list(seed_off = 825L, wl = 220, octaves = 1L, gain = 0.5,
    used_by = "apply_vegetation", note = "Glacial crevasse streaks. Raw gen_simplex."),

  # --- settlements and sacred sites ---------------------------------------
  settle.wobble1 = list(seed_off = 910L, wl = 1100, octaves = 3L, gain = 0.5,
    used_by = "settlement_fields", note = "Clearing radius wobble, coarse."),
  settle.wobble2 = list(seed_off = 911L, wl = 320, octaves = 2L, gain = 0.5,
    used_by = "settlement_fields", note = "Clearing radius wobble, fine."),
  settle.fieldcells = list(seed_off = 920L, wl = 130, octaves = 1L, gain = 0.5,
    used_by = "apply_settlement_ground",
    note = "Worley CELL ids for the field patchwork. gen_worley, not simplex."),
  settle.wallring = list(seed_off = 930L, wl = 520, octaves = 2L, gain = 0.5,
    used_by = "draw_walls",
    note = paste("Wall radius as a pure function of BEARING -- sampled on a fixed",
                 "circle around the centre so every tile draws the identical ring.",
                 "Towers reuse the same field at their own bearings.")),
  sacred.wobble = list(seed_off = 850L, wl = 160, octaves = 2L, gain = 0.5,
    used_by = "sacred_fields", note = "Grove / clearing edge wobble.")
)

# -----------------------------------------------------------------------------
# Accessors
# -----------------------------------------------------------------------------

#' Look up a field. Stops on an unknown name rather than returning NULL, because
#' a NULL seed silently becomes WORLD_SEED and every such field would collide.
nf <- function(name) {
  f <- NOISE_FIELDS[[name]]
  if (is.null(f))
    stop("Unknown noise field: '", name, "'.\n  Registered: ",
         paste(names(NOISE_FIELDS), collapse = ", "),
         "\n  Add it to Functions/NoiseFields.R before using it.", call. = FALSE)
  f
}

#' Absolute seed for a field. `i` indexes into a block-allocated ladder.
nf_seed <- function(name, i = NULL) {
  f <- nf(name)
  off <- f$seed_off
  if (is.null(i)) {
    if (length(off) > 1L)
      stop("'", name, "' is block-allocated; pass i.", call. = FALSE)
    return(WORLD_SEED + off)
  }
  if (i < 1L || i > length(off))
    stop("'", name, "' rung ", i, " is outside its reservation (1..",
         length(off), "). Widen seed_off in NoiseFields.R.", call. = FALSE)
  WORLD_SEED + off[i]
}

#' Base wavelength in metres.
#'
#' `i` is a rung index for a ladder (drainage, river.meander) OR a consumer name
#' for a field whose consumers read it at different scales -- `terrain` is one
#' field at 4000 m for relief and the terrain pull, and 650 m for the road
#' wiggle. Bare `nf_wl(name)` returns the first, which is the primary scale.
nf_wl <- function(name, i = NULL) {
  w <- nf(name)$wl
  if (is.null(i)) return(unname(w[1]))
  if (is.character(i)) {
    if (!i %in% names(w))
      stop("'", name, "' has no wavelength named '", i, "'",
           if (!is.null(names(w))) paste0(" (have: ", paste(names(w), collapse = ", "), ")"),
           call. = FALSE)
    return(unname(w[[i]]))
  }
  if (i < 1L || i > length(w))
    stop("'", name, "' rung ", i, " is outside its wavelength ladder (1..",
         length(w), ").", call. = FALSE)
  unname(w[i])
}

#' Per-band amplitude, for a ladder that carries one (river.meander).
nf_amp <- function(name, i) {
  a <- nf(name)$amp
  if (is.null(a)) stop("'", name, "' declares no amplitudes.", call. = FALSE)
  if (i < 1L || i > length(a))
    stop("'", name, "' band ", i, " is outside its amplitude ladder (1..",
         length(a), ").", call. = FALSE)
  unname(a[i])
}

#' Octave count. `use` selects among divergent consumers (see `terrain`).
nf_octaves <- function(name, use = NULL) {
  o <- nf(name)$octaves
  if (is.null(use)) {
    if (length(o) > 1L)
      stop("'", name, "' has per-consumer octaves (",
           paste(names(o), collapse = ", "), "); pass use.", call. = FALSE)
    return(as.integer(o))
  }
  if (!use %in% names(o))
    stop("'", name, "' has no consumer '", use, "'.", call. = FALSE)
  as.integer(o[[use]])
}

# -----------------------------------------------------------------------------
# Audit
# -----------------------------------------------------------------------------

#' Every offset the table claims, expanded.
noise_used_offsets <- function() {
  sort(unique(unlist(lapply(NOISE_FIELDS, function(f) f$seed_off))))
}

#' Unclaimed offsets in [0, max], for picking a new one.
noise_free_offsets <- function(max = 999L, n = 25L) {
  free <- setdiff(0:max, noise_used_offsets())
  head(free[free > 0], n)
}

#' Check the table against itself.
#'
#' Returns a character vector of problems (empty = clean). Kept separate from
#' the test file so it can be called from a console mid-edit.
noise_field_audit <- function() {
  p <- character(0)

  for (nm in names(NOISE_FIELDS)) {
    f <- NOISE_FIELDS[[nm]]
    for (req in c("seed_off", "wl", "octaves", "used_by", "note"))
      if (is.null(f[[req]])) p <- c(p, sprintf("%s: missing '%s'", nm, req))
    if (!is.null(f$seed_off) && any(f$seed_off < 0))
      p <- c(p, sprintf("%s: negative seed offset", nm))
    if (!is.null(f$wl) && any(f$wl <= 0))
      p <- c(p, sprintf("%s: non-positive wavelength", nm))
    if (isTRUE(f$block) && is.null(f$active))
      p <- c(p, sprintf("%s: block allocation must declare 'active'", nm))
  }

  # Two fields must never claim the same offset. A field with several consumers
  # is fine -- that is one field, shared -- but two ENTRIES sharing an offset
  # means someone picked a number that was already taken.
  owner <- list()
  for (nm in names(NOISE_FIELDS))
    for (o in NOISE_FIELDS[[nm]]$seed_off)
      owner[[as.character(o)]] <- c(owner[[as.character(o)]], nm)
  for (o in names(owner))
    if (length(owner[[o]]) > 1L)
      p <- c(p, sprintf("offset %s claimed by: %s -- pick a free one (see noise_free_offsets())",
                        o, paste(owner[[o]], collapse = ", ")))
  p
}

#' Every source file the enforcement scan covers.
#'
#' It used to be TileServer.R alone. That was the file every noise field HAPPENED
#' to live in, not a rule -- and the rule is what the registry is for. A student
#' adding a field in MapBuilder.R, or in a new file of their own, bypassed the
#' gate entirely and the suite stayed green. Anything that can draw is listed.
NOISE_SCAN_FILES <- c(
  # The tile engine, one file per subsystem. Listed individually rather than
  # globbed so that adding a file is a visible edit here -- the same reason the
  # field table is hand-kept.
  "Functions/tiles/core.R",
  "Functions/tiles/terrain.R",
  "Functions/tiles/vegetation.R",
  "Functions/tiles/shading.R",
  "Functions/tiles/linear.R",
  "Functions/tiles/settlements.R",
  "Functions/tiles/sacred.R",
  "Functions/tiles/render.R",
  "Functions/tiles/server.R",
  "Functions/MapBuilder.R",
  "Functions/FeatureNamer.R",
  "Functions/WaterClass.R",
  "Functions/AnnotationBuilder.R"
)

#' Scan source files for seed offsets and report any the table does not claim.
#'
#' THIS is the enforcement. A new field cannot reach the map without an entry
#' above, and that entry is where two forks collide visibly.
#'
#' Matches `WORLD_SEED + <n>` and `seed + <n>`. The `seed + 500 + i` ladder is
#' recognised by its block reservation.
#'
#' Returns a data frame of (file, offset) rather than a bare vector, because an
#' unregistered offset is only actionable if you know which file to open.
noise_scan_source <- function(paths = NOISE_SCAN_FILES) {
  claimed <- noise_used_offsets()
  # `seed + 500 + i` contributes a literal 500, which is the ladder's base.
  bases <- unlist(lapply(NOISE_FIELDS, function(f)
    if (isTRUE(f$block)) min(f$seed_off) - 1L else NULL))
  ok <- c(claimed, bases, 0L)

  out <- data.frame(file = character(0), offset = integer(0),
                    stringsAsFactors = FALSE)
  for (p in paths) {
    full <- if (file.exists(p)) p else here::here(p)
    if (!file.exists(full)) next           # not every file ships everywhere
    src <- readLines(full, warn = FALSE)
    src <- src[!grepl("^\\s*#", src)]            # skip comment-only lines
    m <- regmatches(src, gregexpr("(WORLD_SEED|seed)\\s*\\+\\s*[0-9]+", src))
    found <- unique(as.integer(sub(".*\\+\\s*", "", unlist(m))))
    un <- setdiff(found, ok)
    if (length(un))
      out <- rbind(out, data.frame(file = basename(p), offset = un,
                                   stringsAsFactors = FALSE))
  }
  out
}
