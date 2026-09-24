@echo off
REM ============================================================================
REM  EART-H - PLAYER MAP (safe for the table)
REM
REM  Serves the map folder with a plain static python server and opens the
REM  PLAYER page. Two properties are the whole point of keeping this separate
REM  from start-tileserver.bat:
REM
REM    1. A static server has no procedural engine, so it can never synthesize
REM       a tile and never writes into <map root>\tiles\. Handing the table
REM       this launcher cannot create new canon.
REM    2. The player page has hidden POIs stripped at BUILD time, so spoilers
REM       are absent from the file rather than merely unrendered.
REM
REM  The map may sit inside the repository (Map\, the default) or outside it;
REM  the location is discovered, as in Functions\MapRoot.R.
REM
REM  The cache-buster (?t=...) is essential - the browser otherwise serves a
REM  stale index.html on every launch because the URL never changes.
REM ============================================================================
setlocal

cd /d "%~dp0"

set "PORT=%~1"
if "%PORT%"=="" set "PORT=8765"

REM --- where the map is ------------------------------------------------------
REM  Same search order as Functions\MapRoot.R: the environment override, then a
REM  one-line .map-root file, then Map\ inside the repo, then C:\Map. Unlike
REM  start-tileserver.bat this script needs the actual value, because it cd's
REM  there and serves it with python rather than handing the job to R.
set "MAPDIR=%EARTH_MAP_ROOT%"
if not defined MAPDIR if exist ".map-root" set /p MAPDIR=<.map-root
if not defined MAPDIR if exist "Map\player\index.html" set "MAPDIR=%CD%\Map"
if not defined MAPDIR set "MAPDIR=C:\Map"

if not exist "%MAPDIR%\player\index.html" (
  echo [X] Player map not found at "%MAPDIR%\player\index.html".
  echo     Build it first:
  echo         source^("Functions/MapBuilder.R"^); build_reference_map^(^)
  echo     If the map is somewhere else, set EARTH_MAP_ROOT or write its path
  echo     into a one-line .map-root file at the repository root.
  pause
  exit /b 1
)

cd /d "%MAPDIR%"
for /f %%i in ('powershell -NoProfile -Command "[int][double]::Parse((Get-Date -UFormat %%s))"') do set T=%%i
echo Starting EART-H PLAYER map on http://localhost:%PORT%/player/
echo   map: %MAPDIR%   (static server - never writes tiles)
start "" "http://localhost:%PORT%/player/index.html?t=%T%"
python -m http.server %PORT%
endlocal
