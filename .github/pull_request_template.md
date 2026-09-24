## What this changes, and why

<!-- One or two sentences. Which project (1-6), or which annotations? -->

Expect: <!-- what should change, from: roads, rivers, settlements, walls, piers, ferries, sacred, coast, vegetation, terrain, shading -->

<!-- The "Change report" check flags any reference tile that changed but has
     none of these in frame. Leave the line as is for annotation-only PRs. -->

## Where to look

<!-- Zoom level and place, e.g. "z13, the river town south of Rukgokai". -->

## Before / after

<!-- A screenshot of each, at the zoom where the change is visible. -->

## Checklist

- [ ] `Rscript tests/run-tests.R` prints `0 failed`
- [ ] If `tests/golden/` changed: which noise fields moved, and why, is written above (the word "fingerprint" must appear)
- [ ] Annotation work only changes `Input Data/Annotations/` and adds new images, not the canon files Save & push rewrites
