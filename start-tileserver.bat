@echo off
REM ============================================================================
REM  EART-H - GM MAP / PROCEDURAL TILE SERVER      start-tileserver.bat [port]
REM
REM  Two roots: the REPO (this folder: Functions\, Input Data\) and the MAP
REM  (index.html, tiles\, data\), which may live elsewhere -- see
REM  Functions\MapRoot.R. Nothing machine-specific is hardcoded: Rscript and
REM  the map are discovered, and RSCRIPT / EARTH_MAP_ROOT override them.
REM
REM  THIS SERVER WRITES TILES. Browsing above z8 synthesizes new tiles and
REM  writes them into <map root>\tiles\, permanently baking that detail into
REM  canon. That is intended - it is the tile cache - but it is why only the
REM  author should run this one. The table gets start-map.bat instead.
REM ============================================================================
setlocal

cd /d "%~dp0"

set "PORT=%~1"
if "%PORT%"=="" set "PORT=8765"

REM --- find Rscript ----------------------------------------------------------
REM  %RSCRIPT% wins, then whatever is on PATH, then the newest R under Program
REM  Files.
if not defined RSCRIPT for /f "delims=" %%i in ('where Rscript 2^>nul') do if not defined RSCRIPT set "RSCRIPT=%%i"
if not defined RSCRIPT for /f "delims=" %%i in ('dir /b /o-n "C:\Program Files\R\R-*" 2^>nul') do if not defined RSCRIPT set "RSCRIPT=C:\Program Files\R\%%i\bin\Rscript.exe"

REM --- where the map is ------------------------------------------------------
REM  This MIRRORS the search order in Functions\MapRoot.R and is used ONLY for
REM  the check and the echo below. Nothing is exported: R resolves the map root
REM  again for itself through map_root(), so this script cannot invent a value
REM  and then hand R a different one.
set "MAPDIR=%EARTH_MAP_ROOT%"
if not defined MAPDIR if exist ".map-root" set /p MAPDIR=<.map-root
if not defined MAPDIR if exist "Map\index.html" set "MAPDIR=%CD%\Map"
if not defined MAPDIR set "MAPDIR=C:\Map"

REM --- checks ----------------------------------------------------------------
if not exist "Functions\run-tileserver.R" (
  echo [X] Functions\run-tileserver.R not found.
  echo     This launcher must sit at the root of the EART-H repository.
  pause
  exit /b 1
)
if not defined RSCRIPT (
  echo [X] Rscript not found.
  echo     Install R from https://cran.r-project.org/ , or set RSCRIPT to the
  echo     full path of Rscript.exe before running this.
  pause
  exit /b 1
)
if not exist "%RSCRIPT%" (
  echo [X] Rscript not found at "%RSCRIPT%"
  pause
  exit /b 1
)
if not exist "%MAPDIR%\index.html" (
  echo [X] "%MAPDIR%\index.html" not found.
  echo     Build the map first:
  echo         source^("Functions/MapBuilder.R"^); build_reference_map^(rebuild_tiles = TRUE^)
  echo     If the map is somewhere else, set EARTH_MAP_ROOT or write its path
  echo     into a one-line .map-root file at the repository root.
  pause
  exit /b 1
)

echo Starting EART-H tile server on http://127.0.0.1:%PORT%/index.html
echo   repo : %CD%
echo   map  : %MAPDIR%
echo   R    : %RSCRIPT%
echo (Ctrl+C to stop. This server writes tiles.)
"%RSCRIPT%" "Functions\run-tileserver.R" %PORT%
endlocal
