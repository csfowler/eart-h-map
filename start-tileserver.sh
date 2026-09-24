#!/usr/bin/env bash
# EART-H GM map / procedural tile server (macOS, Linux). Windows: start-tileserver.bat.
#
#   bash start-tileserver.sh [port]
#
# THIS SERVER WRITES TILES into <map root>/tiles/ -- that is its cache.
# The map root is resolved by Functions/MapRoot.R (EARTH_MAP_ROOT, then a
# one-line .map-root file, then Map/ in the repo).
set -euo pipefail
cd "$(dirname "$0")"
PORT="${1:-8765}"

command -v Rscript >/dev/null || { echo "Rscript not found. Install R from https://cran.r-project.org/"; exit 1; }

echo "Starting EART-H tile server on http://127.0.0.1:${PORT}/index.html"
echo "(Ctrl+C to stop. This server writes tiles.)"
( sleep 3; { command -v open >/dev/null && open "http://127.0.0.1:${PORT}/index.html"; } \
           || xdg-open "http://127.0.0.1:${PORT}/index.html" ) >/dev/null 2>&1 &
exec Rscript Functions/run-tileserver.R "$PORT"
