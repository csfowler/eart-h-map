# =============================================================================
# test-noise-registry.R — the merge gate for procedural fields
#
# This is the test that makes Functions/NoiseFields.R load-bearing rather than
# decorative. Without it the registry is a comment; with it, a field cannot
# reach the map without an entry, and an entry is a line two forks must both
# edit -- which is a git conflict a human resolves, instead of a clean merge
# into a world where two features quietly share a noise field.
# =============================================================================

cat("\n[noise registry]\n")

test_that("the registry is internally consistent", {
  p <- noise_field_audit()
  ok(length(p) == 0,
     sprintf("registry audit clean (%d fields, %d offsets claimed)",
             length(NOISE_FIELDS), length(noise_used_offsets())))
})

test_that("no seed offset is claimed by two fields", {
  # noise_field_audit() covers this, but state it separately: a collision here
  # is the single failure this whole apparatus exists to prevent, and a reader
  # skimming output should see it named.
  offs <- unlist(lapply(NOISE_FIELDS, function(f) f$seed_off))
  dup <- unique(offs[duplicated(offs)])
  ok(length(dup) == 0,
     if (length(dup)) paste("colliding offsets:", paste(dup, collapse = ", "))
     else "every offset has exactly one owner")
})

test_that("no source file uses an unregistered seed offset", {
  # Scans every file that can draw (NOISE_SCAN_FILES), not just TileServer.R --
  # a field added anywhere else used to slip through with the suite green.
  un <- noise_scan_source()
  scanned <- sum(file.exists(vapply(NOISE_SCAN_FILES, function(p)
    if (file.exists(p)) p else here::here(p), character(1))))
  ok(nrow(un) == 0,
     if (nrow(un))
       paste0("unregistered offsets: ",
              paste(sprintf("%s:%d", un$file, un$offset), collapse = ", "),
              "\n        Add them to Functions/NoiseFields.R. If you are adding a new\n",
              "        field, pick a free offset: ",
              paste(head(noise_free_offsets(), 8), collapse = ", "))
     else sprintf("every seed offset across %d scanned file(s) is registered",
                  scanned))
})

test_that("block allocations have room left", {
  # A ladder that has run to the end of its reservation will start overwriting
  # whatever sits above it, silently. Warn while there is still room.
  msgs <- character(0)
  for (nm in names(NOISE_FIELDS)) {
    f <- NOISE_FIELDS[[nm]]
    if (!isTRUE(f$block)) next
    if (f$active > length(f$seed_off))
      msgs <- c(msgs, sprintf("%s: %d rungs active but only %d reserved",
                              nm, f$active, length(f$seed_off)))
  }
  ok(length(msgs) == 0,
     if (length(msgs)) paste(msgs, collapse = "; ")
     else "every block allocation is within its reservation")
})

test_that("the shared terrain field is still shared", {
  # The coupling CLAUDE.md describes: roads and rivers bend around the hills
  # the terrain renders because all three read one field. If someone gives the
  # road pull its own wavelength, roads stop following terrain and nothing else
  # in this suite would notice.
  f <- nf("terrain")
  ok(f$seed_off == 0L &&
       nf_wl("terrain", "relief") == 4000 &&
       nf_wl("terrain", "pull")   == 4000 &&
       all(c("relief", "pull", "wiggle") %in% names(f$octaves)),
     "terrain field intact: one seed, relief and pull still share 4000 m")

  consumers <- f$used_by
  ok(any(grepl("microrelief", consumers)) &&
       any(grepl("shape_roads", consumers)) &&
       any(grepl("shape_rivers", consumers)),
     "terrain field still declares terrain, roads and rivers as consumers")
})

test_that("accessors reject unknown fields rather than defaulting", {
  # nf() returning NULL would make the seed WORLD_SEED + NULL -> WORLD_SEED,
  # i.e. every typo'd field would silently collide with `terrain`.
  e <- tryCatch({ nf("no.such.field"); NULL }, error = function(e) e)
  ok(!is.null(e), "nf() errors on an unknown field name")

  e2 <- tryCatch({ nf_seed("drainage"); NULL }, error = function(e) e)
  ok(!is.null(e2), "nf_seed() refuses a block field without a rung index")

  e3 <- tryCatch({ nf_octaves("terrain"); NULL }, error = function(e) e)
  ok(!is.null(e3), "nf_octaves() refuses an ambiguous multi-consumer field")
})
