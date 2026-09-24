#!/usr/bin/env bash
# EART-H player map (macOS, Linux). Windows: start-map.bat.
#
#   bash start-map.sh [port]
#
# A plain static server: no procedural engine, never writes tiles, and the
# player page has hidden POIs stripped at build time. Same map-root search as
# Functions/MapRoot.R.
set -euo pipefail
cd "$(dirname "$0")"
PORT="${1:-8765}"

MAPDIR="${EARTH_MAP_ROOT:-}"
if [ -z "$MAPDIR" ] && [ -f .map-root ]; then MAPDIR="$(grep -v '^#' .map-root | head -n1)"; fi
if [ -z "$MAPDIR" ]; then MAPDIR="$PWD/Map"; fi

if [ ! -f "$MAPDIR/player/index.html" ]; then
  echo "Player map not found at $MAPDIR/player/index.html"
  echo "Build it first, in R:  source(\"Functions/MapBuilder.R\"); build_reference_map()"
  exit 1
fi

PY="$(command -v python3 || command -v python || true)"
[ -n "$PY" ] || { echo "Python not found; it serves the static map."; exit 1; }

URL="http://localhost:${PORT}/player/index.html?t=$(date +%s)"   # cache-buster
echo "Starting EART-H PLAYER map on $URL  (static server - never writes tiles)"
( sleep 1; { command -v open >/dev/null && open "$URL"; } || xdg-open "$URL" ) >/dev/null 2>&1 &
cd "$MAPDIR"
exec "$PY" -m http.server "$PORT"
