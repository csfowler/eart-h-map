# =============================================================================
# resources/Annotation-editor.R
#   Unified Shiny editor for EART-H map annotations.
#
# Modes (top-bar selector):
#   View           Pan / inspect — no editing.
#   Settlements    Drag to relocate; click to set name override + Inkarnate link.
#   Sacred sites   Drag (per tier); warns if dragged outside its hex.
#   Draw new       Draw line / polygon / point; props captured in a modal.
#   Edit features  Click an existing custom feature to edit props or delete it.
#                  Point features (POIs) can also be dragged in this mode and
#                  in Draw mode; the drop rewrites the geometry directly.
#
# Basemap: the static pyramid under Map/tiles stops at z8. If the procedural
# tile server (start-tileserver.bat, project root) is running on :8765 it detects
# it at startup and unlocks zoom to 14; otherwise it caps at 8 rather than
# showing upscaled, blurry z8 imagery.
#
# All edits live in reactive state until you click Save, which writes to:
#   Input Data/Annotations/settlement_names.csv
#   Input Data/Annotations/relocated_features.rds
#   Input Data/Annotations/custom_features.geojson
#
# Annotations only become "real" (i.e. visible in the map render) after
# chapters/10-languages-and-places.qmd is re-rendered (or you call
# apply_annotations_to_canon() directly), which bakes name + relocation
# overrides into Combined/settlements_final.rds and the sacred-sites RData.
#
# Launch (from the project root; in the student map repo this file is
# tools/annotation-editor.R):
#   - Double-click resources/start-editor.bat
#   - or:   Rscript resources/Annotation-editor.R
#   - or in an R session:  shiny::runApp("resources/Annotation-editor.R")
#
# A non-default package library belongs in .Renviron (R_LIBS_USER), not here:
# setting it inside a running session changes nothing anyway.
# =============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(leaflet)
  library(leaflet.extras)
  library(sf)
  library(dplyr)
  library(magrittr)
})

source(here::here("Functions/AnnotationBuilder.R"))
if (!exists("map_root")) source(here::here("Functions/MapRoot.R"))  # Map/ is external

# --- Constants ---------------------------------------------------------------

ROOT               <- here::here()
SETTLEMENTS_PATH   <- file.path(ROOT, "Input Data/Combined/settlements_final.rds")
SACRED_SITES_PATH  <- file.path(ROOT, "Input Data/Divinity/sacred_sites_attributed.RData")
HEX_HIERARCHY_PATH <- file.path(ROOT, "Input Data/Divinity/hex_hierarchy_sf.RData")
ROADS_PATH         <- file.path(ROOT, "Input Data/Roads/road_routes.rds")
INKARNATE_DIR      <- file.path(ROOT, "Input Data/Inkarnate Maps")
# require_exists = FALSE: an unbuilt map should open the editor without a
# basemap (the server warns), not refuse to start. The editor only READS here.
TILES_DIR          <- map_path("tiles", require_exists = FALSE)
TILE_MAX_NATIVE_Z  <- 8   # elevation pyramid; built to z=8
RIVERS_MAX_Z       <- 6   # rivers pyramid; only generated to z=6

# The procedural tile server (Functions/TileServer.R, launched by
# start-tileserver.bat at the project root) synthesizes elevation/biome tiles above
# pyramid's z8 ceiling, up to z14. It exposes the same /tiles/<layer>/{z}/{x}/{y}
# URL shape the static resource path does, so switching between them is purely a
# matter of which base URL the tile layer is given.
TILE_SERVER_URL    <- "http://127.0.0.1:8765"
TILE_PROCEDURAL_Z  <- 14  # matches TILE_PROCEDURAL_MAX in TileServer.R

#' Probe the tile server's /healthz endpoint.
#' Returns a list(available, procedural_max) — never errors, so a missing or
#' half-started server just degrades to the static pyramid.
probe_tile_server <- function(url = TILE_SERVER_URL, timeout = 1.5) {
  out <- list(available = FALSE, procedural_max = TILE_MAX_NATIVE_Z)
  res <- tryCatch({
    con <- url(file.path(url, "healthz"), open = "rb")
    on.exit(try(close(con), silent = TRUE), add = TRUE)
    txt <- paste(readLines(con, warn = FALSE), collapse = "")
    jsonlite::fromJSON(txt)
  }, error = function(e) NULL, warning = function(w) NULL)
  if (is.null(res) || !identical(res$status, "ok")) return(out)
  out$available      <- TRUE
  out$procedural_max <- as.integer(res$procedural_max %||% TILE_PROCEDURAL_Z)
  out
}

FEATURE_TYPES <- c("road", "trail", "river", "forest", "lake", "mountain",
                   "region", "label", "poi")
FEATURE_TYPE_COLORS <- c(
  road = "#5c3a1e", trail = "#a08060", river = "#2166ac",
  forest = "#1a9850", lake = "#4393c3", mountain = "#737373",
  region = "#fee090", label = "#333333", poi = "#8B0000"
)
FEATURE_TYPE_DEFAULT_WEIGHT <- c(
  road = 3, trail = 2, river = 3, forest = 2, lake = 2,
  mountain = 2, region = 2, label = 1, poi = 2
)

# Small SVG-dot icon (data URI). Used for settlement / sacred-site markers
# because CircleMarker layers don't support draggable in Leaflet — only
# Marker layers do. The cache avoids re-encoding for every render.
.dot_icon_cache <- new.env(parent = emptyenv())

#' Data URI for a single SVG dot. Vectorised over fill/stroke so it can feed
#' leaflet::icons() directly for per-feature colouring.
dot_icon_uri <- function(fill = "#FFD700", stroke = "#000", size = 14) {
  n <- max(length(fill), length(stroke))
  fill <- rep_len(fill, n); stroke <- rep_len(stroke, n)
  r <- (size - 2) / 2
  c <- size / 2
  svg <- sprintf(
    paste0('<svg xmlns="http://www.w3.org/2000/svg" width="%d" height="%d">',
           '<circle cx="%g" cy="%g" r="%g" fill="%s" stroke="%s" ',
           'stroke-width="1"/></svg>'),
    size, size, c, c, r, fill, stroke)
  vapply(svg, function(s)
    paste0("data:image/svg+xml;utf8,", utils::URLencode(s, reserved = TRUE)),
    "", USE.NAMES = FALSE)
}

dot_icon <- function(fill = "#FFD700", stroke = "#000", size = 14) {
  key <- paste(fill, stroke, size, sep = "_")
  if (!is.null(.dot_icon_cache[[key]])) return(.dot_icon_cache[[key]])
  ic <- makeIcon(iconUrl = dot_icon_uri(fill, stroke, size),
                 iconWidth = size, iconHeight = size,
                 iconAnchorX = size / 2, iconAnchorY = size / 2)
  .dot_icon_cache[[key]] <- ic
  ic
}

# --- Helpers -----------------------------------------------------------------

list_inkarnate_files <- function() {
  if (!dir.exists(INKARNATE_DIR)) return(character(0))
  sort(list.files(INKARNATE_DIR, pattern = "\\.(jpg|jpeg|png)$",
                  ignore.case = TRUE))
}

inkarnate_choices <- function() {
  c("(none)" = "", setNames(list_inkarnate_files(), list_inkarnate_files()))
}

empty_features_sf <- function() {
  st_sf(
    feature_id     = integer(0),
    name           = character(0),
    feature_type   = character(0),
    description    = character(0),
    inkarnate_path = character(0),
    style_color    = character(0),
    style_weight   = numeric(0),
    label_offset_x = numeric(0),
    label_offset_y = numeric(0),
    hidden         = logical(0),
    geometry       = st_sfc(crs = 4326)
  )
}

# Coerce a leaflet.extras drawing event into a one-row sf with default props.
# Caller passes line_type / poly_type / point_type from the top-bar selectors
# so the user pre-chooses what they're drawing.
event_to_feature <- function(evt, next_id,
                             line_type  = "road",
                             poly_type  = "region",
                             point_type = "poi") {
  geom_type <- evt$geometry$type
  if (geom_type == "LineString") {
    coords <- matrix(unlist(evt$geometry$coordinates), ncol = 2, byrow = TRUE)
    geom <- st_linestring(coords); ftype <- line_type
  } else if (geom_type == "Polygon") {
    ring   <- evt$geometry$coordinates[[1]]
    coords <- matrix(unlist(ring), ncol = 2, byrow = TRUE)
    geom <- st_polygon(list(coords)); ftype <- poly_type
  } else if (geom_type == "Point") {
    coords <- unlist(evt$geometry$coordinates)
    geom <- st_point(coords); ftype <- point_type
  } else {
    return(NULL)
  }
  st_sf(
    feature_id     = as.integer(next_id),
    name           = sprintf("Feature_%d", next_id),
    feature_type   = ftype,
    description    = "",
    inkarnate_path = NA_character_,
    style_color    = unname(FEATURE_TYPE_COLORS[ftype]),
    style_weight   = unname(FEATURE_TYPE_DEFAULT_WEIGHT[ftype]),
    label_offset_x = 0,
    label_offset_y = 0,
    hidden         = FALSE,
    geometry       = st_sfc(geom, crs = 4326)
  )
}

# =============================================================================
# UI
# =============================================================================

ui <- fluidPage(
  tags$head(tags$style(HTML("
    html, body { height: 100vh; margin: 0; padding: 0; overflow: hidden; }
    .container-fluid { padding: 0 !important; }
    .top-bar { background: #2c3e50; color: #ecf0f1;
               padding: 8px 14px; display: flex; align-items: center;
               gap: 12px; flex-wrap: wrap; }
    .top-bar h4 { margin: 0; font-size: 16px; flex-shrink: 0; }
    .top-bar .form-group { margin: 0; }
    .top-bar label { color: #ecf0f1; margin: 0; font-weight: 500; }
    .top-bar .btn { margin: 0; }
    .top-bar select, .top-bar .selectize-input { min-height: 30px; height: 30px; }
    #status { margin-left: auto; font-style: italic; color: #95a5a6;
              max-width: 300px; text-align: right; }
    .legend-key {
      display: flex; gap: 14px; padding: 6px 14px; background: #34495e;
      color: #ecf0f1; font-size: 12px;
    }
    .legend-key .swatch {
      display: inline-block; width: 12px; height: 12px;
      vertical-align: middle; border-radius: 50%; margin-right: 4px;
      border: 1px solid #000;
    }
    /* leaflet container needs an explicit pixel height to render tiles */
    #map { height: calc(100vh - 100px); }
    .leaflet-container { background: #2c3e50; }
  "))),
  div(class = "top-bar",
      h4("Annotation Editor"),
      selectInput("mode", "Mode", width = "180px",
                  choices = c("View"          = "view",
                              "Settlements"   = "settlements",
                              "Sacred sites"  = "sacred",
                              "Draw new"      = "draw",
                              "Edit features" = "edit_features")),
      conditionalPanel(
        condition = "input.mode == 'sacred'",
        selectInput("sacred_tier", "Tier", width = "100px",
                    choices = setNames(as.character(1:9), sprintf("Tier %d", 1:9)),
                    selected = "9"),
        checkboxInput("show_hexes", "Show hexes", value = FALSE, width = "auto")
      ),
      conditionalPanel(
        condition = "input.mode == 'draw'",
        selectInput("draw_polyline_as", "Polyline →", width = "110px",
                    choices  = c("road", "river", "trail"),
                    selected = "road"),
        selectInput("draw_polygon_as", "Polygon →", width = "115px",
                    choices  = c("region", "lake", "forest", "mountain"),
                    selected = "region"),
        selectInput("draw_point_as", "Point →", width = "90px",
                    choices  = c("poi", "label"),
                    selected = "poi")
      ),
      checkboxInput("show_rivers", "Rivers", value = TRUE, width = "auto"),
      checkboxInput("show_roads",  "Roads",  value = TRUE, width = "auto"),
      actionButton("save_btn",   "Save",     class = "btn btn-success btn-sm"),
      actionButton("push_btn",   "Save & push to map",
                   class = "btn btn-primary btn-sm",
                   title = paste("Save, bake overrides into canon, and",
                                 "rebuild Map/index.html. Then reload the Map",
                                 "page (Ctrl+R) to see changes.")),
      actionButton("revert_btn", "Discard",  class = "btn btn-warning btn-sm"),
      actionButton("quit_btn",   "Quit",     class = "btn btn-danger  btn-sm"),
      div(id = "status", textOutput("status_text", inline = TRUE))
  ),
  div(class = "legend-key",
      tags$span(tags$span(class = "swatch", style = "background:#FFD700"),
                "Settlement (yellow=canon, orange=relocated, green=name override)"),
      tags$span(tags$span(class = "swatch", style = "background:#9b59b6"),
                "Sacred site"),
      tags$span(tags$span(class = "swatch", style = "background:#8B0000"),
                "POI"),
      tags$span("Click any item in its mode to edit; drag to move."),
      conditionalPanel(
        condition = "input.mode == 'draw'",
        tags$span(style = "color:#f1c40f",
                  "Tip: use ← ↑ ↓ → (arrow keys) to pan ",
                  "and +/− to zoom while drawing. Double-click to finish ",
                  "a polyline, click the first vertex to close a polygon.")
      )
  ),
  leafletOutput("map", height = "calc(100vh - 100px)")
)

# =============================================================================
# SERVER
# =============================================================================

server <- function(input, output, session) {

  cat("\n[editor] server start\n")

  # --- Tile resource path ---------------------------------------------------
  if (dir.exists(TILES_DIR)) {
    addResourcePath("tiles", TILES_DIR)
    cat(sprintf("[editor] addResourcePath('tiles', %s)\n", TILES_DIR))
  } else {
    cat(sprintf("[editor] WARN: tiles dir missing: %s\n", TILES_DIR))
    showNotification(
      sprintf("Tiles not found at %s — map will render without basemap.",
              TILES_DIR),
      type = "warning", duration = 8)
  }

  # --- Basemap detail source -------------------------------------------------
  # The static pyramid stops at z8; anything beyond that is an upscaled z8 tile,
  # which is useless for placing a POI on a riverbank. If the procedural tile
  # server is running we point at it instead and unlock z9-14.
  tsrv <- probe_tile_server()
  if (tsrv$available) {
    tile_url  <- paste0(TILE_SERVER_URL, "/tiles/elevation/{z}/{x}/{y}.png")
    tile_maxz <- tsrv$procedural_max
    cat(sprintf("[editor] tile server detected on %s\n", TILE_SERVER_URL))
    cat(sprintf("[editor] procedural tiles enabled (z%d-%d)\n",
                TILE_MAX_NATIVE_Z + 1L, tile_maxz))
  } else {
    tile_url  <- "tiles/elevation/{z}/{x}/{y}.png"
    tile_maxz <- TILE_MAX_NATIVE_Z
    cat("[editor] no tile server; static pyramid only\n")
    cat(sprintf("[editor] zoom capped at %d. Run start-tileserver.bat / .sh for detail.\n",
                TILE_MAX_NATIVE_Z))
  }

  observe({
    if (!tsrv$available) {
      showNotification(
        HTML(paste0(
          "<b>Basemap detail limited.</b><br>Static tiles stop at zoom ",
          TILE_MAX_NATIVE_Z, ". For close-in work run ",
          "<code>start-tileserver.bat</code> (or <code>.sh</code>) and reload this editor ",
          "to get procedural tiles to zoom ", TILE_PROCEDURAL_Z, ".")),
        type = "warning", duration = 12)
    }
  })

  has_rivers_tiles <- dir.exists(file.path(TILES_DIR, "rivers"))

  # --- Load canon (read-only) -----------------------------------------------
  settlements_canon <- readRDS(SETTLEMENTS_PATH)
  # Force expected types — leaflet's internal expandLimits / sprintf
  # is type-strict, and any non-numeric lon/lat or non-integer id will crash.
  settlements_canon$settlement_id <- as.integer(settlements_canon$settlement_id)
  settlements_canon$lon <- as.numeric(settlements_canon$lon)
  settlements_canon$lat <- as.numeric(settlements_canon$lat)
  if (!"name" %in% names(settlements_canon))
    settlements_canon$name <- paste0("#", settlements_canon$settlement_id)
  settlements_canon$name <- as.character(settlements_canon$name)
  if (!"inkarnate" %in% names(settlements_canon))
    settlements_canon$inkarnate <- NA_character_
  if (!"population" %in% names(settlements_canon))
    settlements_canon$population <- 0
  settlements_canon$population <- as.numeric(settlements_canon$population)
  cat(sprintf("[editor] settlements: %d rows; types lon=%s, id=%s, name=%s\n",
              nrow(settlements_canon),
              class(settlements_canon$lon),
              class(settlements_canon$settlement_id),
              class(settlements_canon$name)))

  e <- new.env(); load(SACRED_SITES_PATH, envir = e)
  sacred_sites_sf <- get(ls(e)[1], envir = e)
  cat(sprintf("[editor] sacred sites: %d rows\n", nrow(sacred_sites_sf)))

  e <- new.env(); load(HEX_HIERARCHY_PATH, envir = e)
  hex_hierarchy_sf <- get(ls(e)[1], envir = e)
  cat(sprintf("[editor] hex tiers: %d\n", length(hex_hierarchy_sf)))

  roads_sf <- if (file.exists(ROADS_PATH)) {
    r <- readRDS(ROADS_PATH)
    cat(sprintf("[editor] roads: %d segments\n", nrow(r)))
    r
  } else {
    cat("[editor] WARN: roads file not found:", ROADS_PATH, "\n")
    NULL
  }

  # --- Reactive state (the "draft" overlaid on canon) -----------------------
  pending_names       <- reactiveVal(load_settlement_names() %||%
                                       data.frame(settlement_id = integer(0),
                                                  name = character(0),
                                                  population = numeric(0),
                                                  inkarnate = character(0),
                                                  notes = character(0),
                                                  stringsAsFactors = FALSE))
  pending_relocations <- reactiveVal(load_relocations())
  pending_features    <- reactiveVal(read_custom_features() %||% empty_features_sf())
  dirty               <- reactiveVal(FALSE)

  mark_dirty  <- function() dirty(TRUE)
  reset_dirty <- function() dirty(FALSE)

  # Settlements with overrides + relocations applied (display layer).
  current_settlements <- reactive({
    s <- settlements_canon
    nm <- pending_names()
    if (nrow(nm) > 0) {
      idx <- match(nm$settlement_id, s$settlement_id)
      ok <- !is.na(idx) & nzchar(nm$name)
      if (any(ok)) s$name[idx[ok]] <- nm$name[ok]
      ok2 <- !is.na(idx) & nzchar(nm$inkarnate)
      if (any(ok2)) s$inkarnate[idx[ok2]] <- nm$inkarnate[ok2]
      # Mark which canon rows have ANY override row (name or inkarnate)
      s$has_override <- FALSE
      ok3 <- !is.na(idx) & (nzchar(nm$name) | nzchar(nm$inkarnate))
      if (any(ok3)) s$has_override[idx[ok3]] <- TRUE
    } else {
      s$has_override <- FALSE
    }
    rel <- pending_relocations()$settlements
    s$is_relocated <- FALSE
    if (length(rel) > 0) {
      for (id_str in names(rel)) {
        idx <- which(s$settlement_id == as.integer(id_str))
        if (length(idx) == 0) next
        s$lon[idx] <- as.numeric(rel[[id_str]]$lon)
        s$lat[idx] <- as.numeric(rel[[id_str]]$lat)
        s$is_relocated[idx] <- TRUE
      }
    }
    # Final type guard — any prior op (including a character relocation entry)
    # may have promoted the column to character. expandLimits will crash on it.
    s$lon <- as.numeric(s$lon)
    s$lat <- as.numeric(s$lat)
    s
  })

  current_sacred <- reactive({
    ss <- sacred_sites_sf
    rel <- pending_relocations()$sacred_sites
    ss$is_relocated <- FALSE
    if (length(rel) > 0) {
      for (sid in names(rel)) {
        idx <- which(ss$site_id == sid)
        if (length(idx) == 0) next
        new_pt <- st_sfc(st_point(c(as.numeric(rel[[sid]]$lon),
                                    as.numeric(rel[[sid]]$lat))),
                         crs = 4326)
        st_geometry(ss)[idx] <- new_pt
        ss$is_relocated[idx] <- TRUE
      }
    }
    ss
  })

  # --- Build the base map ---------------------------------------------------
  # IMPORTANT: this block must NOT read any reactiveVal that the user can
  # change (settlements, features, names, relocations). If it does, every
  # edit re-fires renderLeaflet, which rebuilds the leaflet object and wipes
  # every layer added via leafletProxy — features and POIs vanish until the
  # observer that owns them happens to re-fire.
  output$map <- renderLeaflet({
    cat("[editor] renderLeaflet firing (should happen exactly once)\n")
    cx <- mean(settlements_canon$lon, na.rm = TRUE)
    cy <- mean(settlements_canon$lat, na.rm = TRUE)
    cat(sprintf("[editor]   center=(%.2f, %.2f)\n", cx, cy))
    m <- leaflet(options = leafletOptions(minZoom = 0, maxZoom = tile_maxz)) %>%
      setView(lng = if (is.finite(cx)) cx else 0,
              lat = if (is.finite(cy)) cy else 0,
              zoom = 3) %>%
      addTiles(urlTemplate = tile_url,
               options = tileOptions(tms = TRUE,
                                     maxNativeZoom = tile_maxz,
                                     maxZoom = tile_maxz, errorTileUrl = ""),
               group = "Elevation")
    if (has_rivers_tiles) {
      # Rivers pyramid is only built to z=6 — without maxNativeZoom = 6 the
      # client requests z>6 tiles that don't exist and rivers blank out
      # exactly at the zoom levels you need them for placing things.
      m <- m %>% addTiles(
        urlTemplate = "tiles/rivers/{z}/{x}/{y}.png",
        options = tileOptions(tms = TRUE,
                              maxNativeZoom = RIVERS_MAX_Z,
                              maxZoom = tile_maxz, opacity = 0.9,
                              errorTileUrl = ""),
        group = "Rivers")
    }
    if (!is.null(roads_sf) && nrow(roads_sf) > 0) {
      ferry <- if ("is_ferry" %in% names(roads_sf))
                 isTRUE(any(as.logical(roads_sf$is_ferry))) else FALSE
      land_segs  <- roads_sf[!isTRUE(roads_sf$is_ferry %||% FALSE), ]
      ferry_segs <- if (ferry) roads_sf[as.logical(roads_sf$is_ferry), ] else NULL
      if (nrow(land_segs) > 0)
        m <- m %>% addPolylines(data = land_segs, color = "#6b4423",
                                weight = 1.4, opacity = 0.85,
                                group = "Roads")
      if (!is.null(ferry_segs) && nrow(ferry_segs) > 0)
        m <- m %>% addPolylines(data = ferry_segs, color = "#3949ab",
                                weight = 1.2, opacity = 0.7,
                                dashArray = "4,4", group = "Roads")
    }
    m
  })

  # Layer toggles
  observe({
    proxy <- leafletProxy("map")
    if (isTRUE(input$show_rivers)) proxy %>% showGroup("Rivers")
    else proxy %>% hideGroup("Rivers")
    if (isTRUE(input$show_roads))  proxy %>% showGroup("Roads")
    else proxy %>% hideGroup("Roads")
  })

  # --- Settlements layer ----------------------------------------------------
  observe({
    proxy <- leafletProxy("map")
    proxy %>%clearGroup("Settlements") %>%clearGroup("Settlement_Origins")
    sett <- current_settlements()

    # Show ghost markers at original positions for relocated settlements
    rel <- pending_relocations()$settlements
    if (length(rel) > 0) {
      orig_lon <- as.numeric(sapply(rel, `[[`, "original_lon"))
      orig_lat <- as.numeric(sapply(rel, `[[`, "original_lat"))
      proxy %>% addCircleMarkers(
        lng = orig_lon, lat = orig_lat,
        radius = 4, color = "#888", fillColor = "#ccc",
        fillOpacity = 0.5, weight = 1,
        group = "Settlement_Origins",
        label = paste0("#", names(rel), " (original)"))
    }

    is_drag_mode <- input$mode == "settlements"
    state <- ifelse(sett$is_relocated, "relocated",
                    ifelse(sett$has_override, "override", "canon"))
    icons <- iconList(
      canon     = dot_icon("#FFD700"),  # yellow
      override  = dot_icon("#27ae60"),  # green
      relocated = dot_icon("#e67e22")   # orange
    )
    sel_icons <- icons[state]
    names(sel_icons) <- NULL  # silence jsonlite named-vector deprecation

    proxy %>% addMarkers(
      lng = as.numeric(sett$lon), lat = as.numeric(sett$lat),
      icon = sel_icons,
      layerId = paste0("sett_", sett$settlement_id),
      label = sprintf("%s (#%s, pop %s)",
                      as.character(sett$name),
                      as.character(sett$settlement_id),
                      format(round(as.numeric(sett$population)),
                             big.mark = ",")),
      options = if (is_drag_mode)
                  markerOptions(draggable = TRUE) else markerOptions(),
      group = "Settlements")
  })

  # --- Sacred sites layer (only when in sacred mode) ------------------------
  observe({
    proxy <- leafletProxy("map")
    proxy %>% clearGroup("Sacred") %>% clearGroup("Sacred_Hexes") %>%
      clearGroup("Sacred_Origins")

    if (input$mode != "sacred") return()

    tier <- as.integer(input$sacred_tier)
    ss <- current_sacred()
    ss <- ss[ss$tier == tier, , drop = FALSE]
    if (nrow(ss) == 0) return()

    coords <- st_coordinates(ss)
    rel <- pending_relocations()$sacred_sites

    if (length(rel) > 0) {
      tier_relocs <- rel[sapply(rel, function(x) isTRUE(x$tier == tier))]
      if (length(tier_relocs) > 0) {
        proxy %>% addCircleMarkers(
          lng = as.numeric(sapply(tier_relocs, `[[`, "original_lon")),
          lat = as.numeric(sapply(tier_relocs, `[[`, "original_lat")),
          radius = 4, color = "#888", fillColor = "#ccc",
          fillOpacity = 0.5, weight = 1, group = "Sacred_Origins",
          label = paste0(names(tier_relocs), " (original)"))
      }
    }

    sacred_icons <- iconList(
      canon     = dot_icon("#9b59b6"),  # purple
      relocated = dot_icon("#e67e22")   # orange
    )
    sacred_state <- ifelse(ss$is_relocated, "relocated", "canon")
    sel_sacred <- sacred_icons[sacred_state]
    names(sel_sacred) <- NULL  # silence jsonlite named-vector deprecation

    proxy %>% addMarkers(
      lng = as.numeric(coords[, 1]), lat = as.numeric(coords[, 2]),
      icon = sel_sacred,
      layerId = paste0("sacred_", ss$site_id),
      label = sprintf("%s (Tier %s, hex %s)",
                      as.character(ss$site_id),
                      as.character(ss$tier),
                      as.character(ss$hex_id)),
      options = markerOptions(draggable = TRUE),
      group = "Sacred")

    if (isTRUE(input$show_hexes)) {
      hexes <- hex_hierarchy_sf[[tier]]
      if (!is.null(hexes) && nrow(hexes) > 0) {
        proxy %>%addPolygons(
          data = hexes,
          fillColor = "#3498db", fillOpacity = 0.05,
          color = "#2980b9", weight = 1,
          group = "Sacred_Hexes",
          label = ~hex_id)
      }
    }
  })

  # --- Custom features layer ------------------------------------------------
  observe({
    proxy <- leafletProxy("map")
    proxy %>%clearGroup("Features") %>%clearGroup("POIs")

    cf <- pending_features()
    if (is.null(cf) || nrow(cf) == 0) return()

    geom_types <- as.character(st_geometry_type(cf))
    is_line <- geom_types %in% c("LINESTRING", "MULTILINESTRING")
    is_poly <- geom_types %in% c("POLYGON", "MULTIPOLYGON")
    is_pt   <- geom_types %in% c("POINT", "MULTIPOINT")

    if (any(is_line)) {
      ln <- cf[is_line, , drop = FALSE]
      proxy %>% addPolylines(
        data = ln,
        color = ln$style_color,
        weight = as.numeric(ln$style_weight),
        opacity = 0.85,
        layerId = paste0("feat_", ln$feature_id),
        label = as.character(ln$name),
        group = "Features")
    }
    if (any(is_poly)) {
      pl <- cf[is_poly, , drop = FALSE]
      proxy %>% addPolygons(
        data = pl,
        fillColor = pl$style_color, fillOpacity = 0.3,
        color = pl$style_color,
        weight = as.numeric(pl$style_weight),
        opacity = 0.85,
        layerId = paste0("feat_", pl$feature_id),
        label = as.character(pl$name),
        group = "Features")
    }
    if (any(is_pt)) {
      pt <- cf[is_pt, , drop = FALSE]
      coords <- st_coordinates(pt)
      # Hidden POIs render with a dashed ring + lower opacity so the GM can
      # spot them at a glance. `%in% TRUE` is NA-safe.
      pt_hidden <- pt$hidden %in% TRUE
      # Marker (not CircleMarker) because only Marker layers support
      # draggable in Leaflet — same reason settlements and sacred sites use
      # the SVG dot icon. Dragging is enabled in the two modes where moving a
      # POI is the intent; in view/settlements/sacred modes they stay put so a
      # stray drag can't silently move one.
      poi_draggable <- input$mode %in% c("draw", "edit_features")
      fills <- pt$style_color
      fills[is.na(fills) | !nzchar(fills)] <- FEATURE_TYPE_COLORS[["poi"]]
      sel_poi <- icons(
        iconUrl = dot_icon_uri(fill   = fills,
                               stroke = ifelse(pt_hidden, "#666", "#000"),
                               size   = 16),
        iconWidth = 16, iconHeight = 16, iconAnchorX = 8, iconAnchorY = 8)

      proxy %>% addMarkers(
        lng = as.numeric(coords[, 1]),
        lat = as.numeric(coords[, 2]),
        icon = sel_poi,
        layerId = paste0("feat_", pt$feature_id),
        label = ifelse(pt_hidden,
                       paste0("[hidden] ", as.character(pt$name)),
                       as.character(pt$name)),
        options = if (poi_draggable) markerOptions(draggable = TRUE)
                  else markerOptions(),
        group = "POIs")
    }
  })

  # --- Drawing toolbar (only mounted in 'draw' mode) ------------------------
  # Re-fires whenever the polyline/polygon-as selectors change, so the
  # tool's preview color matches the type the user picked.
  observe({
    proxy <- leafletProxy("map")
    proxy %>% removeDrawToolbar(clearFeatures = FALSE)
    if (input$mode != "draw") return()
    line_type <- input$draw_polyline_as %||% "road"
    poly_type <- input$draw_polygon_as  %||% "region"
    line_col  <- unname(FEATURE_TYPE_COLORS[line_type])
    poly_col  <- unname(FEATURE_TYPE_COLORS[poly_type])
    line_w    <- unname(FEATURE_TYPE_DEFAULT_WEIGHT[line_type])
    proxy %>% addDrawToolbar(
      targetGroup = "_drawn",
      polylineOptions   = drawPolylineOptions(
        # fill=FALSE kills the translucent area-shading some leaflet.draw
        # builds render under tight polyline curves; smoothFactor reduces
        # vertex-perceptible thickness on screen.
        shapeOptions = drawShapeOptions(stroke = TRUE, color = line_col,
                                        weight = line_w, opacity = 0.85,
                                        fill = FALSE, smoothFactor = 1,
                                        clickable = FALSE),
        guidelineDistance = 8,    # shorter dashed guide to cursor
        maxGuideLineLength = 800,
        showLength = TRUE,
        metric = TRUE, feet = FALSE),
      polygonOptions    = drawPolygonOptions(
        shapeOptions = drawShapeOptions(stroke = TRUE, color = poly_col,
                                        weight = 2, opacity = 0.85,
                                        fill = TRUE, fillColor = poly_col,
                                        fillOpacity = 0.3,
                                        clickable = FALSE)),
      markerOptions     = drawMarkerOptions(),
      circleOptions     = FALSE,
      rectangleOptions  = FALSE,
      circleMarkerOptions = FALSE,
      editOptions = FALSE)
  })

  # =========================================================================
  # EDITING — modal forms
  # =========================================================================

  # ----- Settlement-edit modal (click in 'settlements' mode) ----------------
  open_settlement_modal <- function(sid) {
    s <- current_settlements()
    idx <- which(s$settlement_id == sid)
    if (length(idx) == 0) return()
    sett <- s[idx, ]
    # Lookup current override row
    nm <- pending_names()
    nm_idx <- which(nm$settlement_id == sid)
    cur_name      <- if (length(nm_idx)) nm$name[nm_idx]      else ""
    cur_inkarnate <- if (length(nm_idx)) nm$inkarnate[nm_idx] else ""
    cur_notes     <- if (length(nm_idx)) nm$notes[nm_idx]     else ""
    cur_pop       <- if (length(nm_idx)) nm$population[nm_idx] else NA_real_

    showModal(modalDialog(
      title = sprintf("Settlement #%d — %s", sid, sett$name),
      easyClose = TRUE, size = "m",
      tagList(
        tags$p(class = "text-muted",
               sprintf("Position: (%.4f, %.4f) | Population: %s | Continent: %s",
                       sett$lon, sett$lat,
                       format(round(sett$population), big.mark = ","),
                       sett$continent %||% "?")),
        textInput("sett_name_in", "Story name override",
                  value = cur_name,
                  placeholder = "(leave empty to keep procgen name)"),
        numericInput("sett_pop_in", "Population override",
                     value = if (is.na(cur_pop)) NA else cur_pop,
                     min = 0, step = 50),
        tags$p(class = "text-muted small",
               "Leave blank to keep the pipeline's gravity-redistributed figure."),
        selectInput("sett_inkarnate_in", "Inkarnate detail map",
                    choices = inkarnate_choices(),
                    selected = cur_inkarnate),
        textAreaInput("sett_notes_in", "Notes", value = cur_notes,
                      rows = 3)
      ),
      footer = tagList(
        if (length(nm_idx))
          actionButton("sett_clear_override", "Clear override",
                       class = "btn btn-warning") else NULL,
        if (isTRUE(s$is_relocated[idx]))
          actionButton("sett_revert_position", "Revert position",
                       class = "btn btn-warning") else NULL,
        modalButton("Cancel"),
        actionButton("sett_save", "Save", class = "btn btn-primary")
      )
    ))
    session$userData$current_sett <- sid
  }

  observeEvent(input$sett_save, {
    sid <- session$userData$current_sett
    nm <- pending_names()
    nm_idx <- which(nm$settlement_id == sid)
    pop_in <- suppressWarnings(as.numeric(input$sett_pop_in))
    if (length(pop_in) == 0) pop_in <- NA_real_
    new_row <- data.frame(
      settlement_id = as.integer(sid),
      name          = trimws(input$sett_name_in %||% ""),
      population    = pop_in,
      inkarnate     = input$sett_inkarnate_in %||% "",
      notes         = trimws(input$sett_notes_in %||% ""),
      stringsAsFactors = FALSE)
    # If everything is empty, drop the row
    is_empty <- !nzchar(new_row$name) && !nzchar(new_row$inkarnate) &&
                !nzchar(new_row$notes) && is.na(new_row$population)
    if (length(nm_idx)) {
      if (is_empty) nm <- nm[-nm_idx, , drop = FALSE]
      else nm[nm_idx, ] <- new_row
    } else if (!is_empty) {
      nm <- rbind(nm, new_row)
    }
    pending_names(nm)
    mark_dirty()
    removeModal()
  })

  observeEvent(input$sett_clear_override, {
    sid <- session$userData$current_sett
    nm <- pending_names()
    nm <- nm[nm$settlement_id != sid, , drop = FALSE]
    pending_names(nm)
    mark_dirty()
    removeModal()
  })

  observeEvent(input$sett_revert_position, {
    sid <- session$userData$current_sett
    rel <- pending_relocations()
    rel$settlements[[as.character(sid)]] <- NULL
    pending_relocations(rel)
    mark_dirty()
    removeModal()
  })

  # ----- Custom-feature edit modal -----------------------------------------
  open_feature_modal <- function(fid, prefill = NULL) {
    cf <- pending_features()
    feat <- if (!is.null(prefill)) prefill else cf[cf$feature_id == fid, , drop = FALSE]
    if (nrow(feat) == 0) return()
    is_new <- !is.null(prefill)
    is_poi <- feat$feature_type == "poi"

    showModal(modalDialog(
      title = if (is_new) "New feature" else sprintf("Feature #%d — %s",
                                                     fid, feat$name),
      easyClose = TRUE, size = "m",
      tagList(
        textInput("feat_name_in", "Name", value = feat$name),
        selectInput("feat_type_in", "Type",
                    choices = FEATURE_TYPES,
                    selected = feat$feature_type),
        textAreaInput("feat_desc_in", "Description",
                      value = feat$description %||% "", rows = 2),
        conditionalPanel(
          condition = "input.feat_type_in == 'poi' || input.feat_type_in == 'label'",
          selectInput("feat_inkarnate_in", "Inkarnate detail map",
                      choices = inkarnate_choices(),
                      selected = feat$inkarnate_path %||% "")
        ),
        textInput("feat_color_in", "Style color (hex)",
                  value = feat$style_color %||%
                          unname(FEATURE_TYPE_COLORS[feat$feature_type])),
        checkboxInput("feat_hidden_in",
                      "Hidden from players (visible only in GM view)",
                      value = isTRUE(feat$hidden))
      ),
      footer = tagList(
        if (!is_new)
          actionButton("feat_delete", "Delete",
                       class = "btn btn-danger") else NULL,
        modalButton("Cancel"),
        actionButton("feat_save", "Save", class = "btn btn-primary")
      )
    ))
    session$userData$current_feat    <- if (is_new) NA else fid
    session$userData$pending_new_feat <- if (is_new) prefill else NULL
  }

  observeEvent(input$feat_save, {
    cf <- pending_features()
    fid <- session$userData$current_feat
    is_new <- is.na(fid)
    new_type  <- input$feat_type_in
    new_color <- if (nzchar(input$feat_color_in %||% "")) input$feat_color_in
                 else unname(FEATURE_TYPE_COLORS[new_type])
    new_ink   <- if (new_type %in% c("poi", "label")) input$feat_inkarnate_in
                 else NA_character_
    if (isTRUE(new_ink == "")) new_ink <- NA_character_

    new_hidden <- isTRUE(input$feat_hidden_in)
    if (is_new) {
      feat <- session$userData$pending_new_feat
      next_id <- if (nrow(cf) == 0) 1L else max(cf$feature_id, na.rm = TRUE) + 1L
      feat$feature_id     <- next_id
      feat$name           <- input$feat_name_in
      feat$feature_type   <- new_type
      feat$description    <- input$feat_desc_in
      feat$inkarnate_path <- new_ink
      feat$style_color    <- new_color
      feat$style_weight   <- unname(FEATURE_TYPE_DEFAULT_WEIGHT[new_type] %||% 2)
      feat$hidden         <- new_hidden
      cf <- rbind(cf, feat)
    } else {
      idx <- which(cf$feature_id == fid)
      if (length(idx) == 1) {
        cf$name[idx]           <- input$feat_name_in
        cf$feature_type[idx]   <- new_type
        cf$description[idx]    <- input$feat_desc_in
        cf$inkarnate_path[idx] <- new_ink
        cf$style_color[idx]    <- new_color
        cf$style_weight[idx]   <- unname(FEATURE_TYPE_DEFAULT_WEIGHT[new_type] %||% 2)
        cf$hidden[idx]         <- new_hidden
      }
    }
    pending_features(cf)
    mark_dirty()
    removeModal()
  })

  observeEvent(input$feat_delete, {
    fid <- session$userData$current_feat
    if (is.na(fid)) { removeModal(); return() }
    cf <- pending_features()
    cf <- cf[cf$feature_id != fid, , drop = FALSE]
    pending_features(cf)
    mark_dirty()
    removeModal()
  })

  # =========================================================================
  # MAP EVENT HANDLERS
  # =========================================================================

  # --- Marker click (settlements / sacred / POIs) ---------------------------
  observeEvent(input$map_marker_click, {
    ev <- input$map_marker_click
    if (is.null(ev$id)) return()

    if (startsWith(ev$id, "sett_") && input$mode == "settlements") {
      sid <- as.integer(sub("sett_", "", ev$id))
      open_settlement_modal(sid)
    } else if (startsWith(ev$id, "feat_") && input$mode == "edit_features") {
      fid <- as.integer(sub("feat_", "", ev$id))
      open_feature_modal(fid)
    }
  })

  # --- Shape click (lines / polygons in edit mode) -------------------------
  observeEvent(input$map_shape_click, {
    ev <- input$map_shape_click
    if (is.null(ev$id)) return()
    if (input$mode != "edit_features") return()
    if (!startsWith(ev$id, "feat_")) return()
    fid <- as.integer(sub("feat_", "", ev$id))
    open_feature_modal(fid)
  })

  # --- Drag end (settlements / sacred sites / point features) --------------
  observeEvent(input$map_marker_dragend, {
    ev <- input$map_marker_dragend
    if (is.null(ev$id)) return()

    if (startsWith(ev$id, "sett_") && input$mode == "settlements") {
      sid <- as.integer(sub("sett_", "", ev$id))
      orig_idx <- which(settlements_canon$settlement_id == sid)
      if (length(orig_idx) == 0) return()
      orig_lon <- settlements_canon$lon[orig_idx]
      orig_lat <- settlements_canon$lat[orig_idx]
      rel <- pending_relocations()
      rel$settlements[[as.character(sid)]] <- list(
        lon = ev$lng, lat = ev$lat,
        original_lon = orig_lon, original_lat = orig_lat)
      pending_relocations(rel)
      mark_dirty()

    } else if (startsWith(ev$id, "sacred_") && input$mode == "sacred") {
      sid <- sub("sacred_", "", ev$id)
      orig <- sacred_sites_sf[sacred_sites_sf$site_id == sid, , drop = FALSE]
      if (nrow(orig) == 0) return()
      orig_coords <- st_coordinates(orig)
      tier <- as.integer(input$sacred_tier)
      hex_id <- orig$hex_id

      # Hex-boundary check
      hexes <- hex_hierarchy_sf[[tier]]
      if (!is.null(hexes)) {
        hex_poly <- hexes[hexes$hex_id == hex_id, , drop = FALSE]
        if (nrow(hex_poly) > 0) {
          new_pt <- st_sfc(st_point(c(ev$lng, ev$lat)), crs = 4326)
          inside <- st_intersects(new_pt, hex_poly, sparse = FALSE)[1, 1]
          if (!isTRUE(inside)) {
            showNotification(sprintf("⚠ %s is now outside %s", sid, hex_id),
                             type = "warning", duration = 6)
          }
        }
      }
      rel <- pending_relocations()
      rel$sacred_sites[[sid]] <- list(
        lon = ev$lng, lat = ev$lat,
        original_lon = orig_coords[1, 1],
        original_lat = orig_coords[1, 2],
        tier = tier, hex_id = hex_id)
      pending_relocations(rel)
      mark_dirty()

    } else if (startsWith(ev$id, "feat_") &&
               input$mode %in% c("draw", "edit_features")) {
      # Point features carry their position in the geometry itself rather than
      # in relocated_features.rds, so a drag rewrites the sf geometry in place.
      # Only POINT features are draggable; lines and polygons are edited with
      # the draw toolbar.
      fid <- as.integer(sub("feat_", "", ev$id))
      cf  <- pending_features()
      idx <- which(cf$feature_id == fid)
      if (length(idx) != 1) return()
      if (!inherits(st_geometry(cf)[[idx]], "POINT")) return()

      st_geometry(cf)[[idx]] <- st_point(c(ev$lng, ev$lat))
      pending_features(cf)
      mark_dirty()
    }
  })

  # --- New feature drawn ----------------------------------------------------
  observeEvent(input$map_draw_new_feature, {
    if (input$mode != "draw") return()
    cf <- pending_features()
    next_id <- if (nrow(cf) == 0) 1L else max(cf$feature_id, na.rm = TRUE) + 1L
    new_feat <- event_to_feature(
      input$map_draw_new_feature, next_id,
      line_type  = input$draw_polyline_as %||% "road",
      poly_type  = input$draw_polygon_as  %||% "region",
      point_type = input$draw_point_as    %||% "poi"
    )
    if (is.null(new_feat)) {
      showNotification("Unsupported geometry type", type = "warning")
      return()
    }
    open_feature_modal(NA, prefill = new_feat)
  })

  # =========================================================================
  # SAVE / REVERT / QUIT
  # =========================================================================

  output$status_text <- renderText({
    rel <- pending_relocations()
    n_sett   <- length(rel$settlements)
    n_sacred <- length(rel$sacred_sites)
    n_names  <- nrow(pending_names())
    n_feats  <- nrow(pending_features())
    flag <- if (dirty()) "● UNSAVED" else "✓ saved"
    sprintf("%s — %d names, %d sett moves, %d sacred moves, %d features",
            flag, n_names, n_sett, n_sacred, n_feats)
  })

  # Shared writer used by both Save and Save & push.
  do_save <- function() {
    nm <- pending_names()
    dir.create(ANNOTATIONS_DIR(), recursive = TRUE, showWarnings = FALSE)
    write.csv(nm, SETTLEMENT_NAMES_PATH(), row.names = FALSE)
    save_relocations(pending_relocations())
    save_custom_features(pending_features())
    reset_dirty()
  }

  observeEvent(input$save_btn, {
    tryCatch({
      do_save()
      showNotification(
        paste("Saved to Input Data/Annotations/.",
              "Click 'Save & push to map' (or apply_annotations_to_canon() +",
              "build_reference_map()) to see changes on the map."),
        type = "message", duration = 8)
    }, error = function(e) {
      showNotification(paste("Save failed:", e$message),
                       type = "error", duration = 10)
    })
  })

  # Save → bake into canon → rebuild Map/index.html in one shot.
  observeEvent(input$push_btn, {
    tryCatch({
      do_save()
      withProgress(message = "Pushing to map...", value = 0, {
        incProgress(0.15, detail = "baking annotations into canon")
        apply_annotations_to_canon(verbose = FALSE)
        incProgress(0.25, detail = "loading MapBuilder")
        # Lazy-source MapBuilder once; subsequent pushes reuse the namespace.
        if (!exists("build_reference_map", inherits = TRUE)) {
          suppressMessages(source(here::here("Functions/MapBuilder.R")))
        }
        incProgress(0.30, detail = "rebuilding Map/index.html")
        suppressMessages(build_reference_map(verbose = FALSE))
        incProgress(0.30, detail = "done")
      })
      showNotification(
        "Map rebuilt. Reload the Map page in your browser (Ctrl+R) to see changes.",
        type = "message", duration = 10)
    }, error = function(e) {
      showNotification(paste("Push to map failed:", e$message),
                       type = "error", duration = 12)
    })
  })

  observeEvent(input$revert_btn, {
    showModal(modalDialog(
      title = "Discard all unsaved changes?",
      "This will reload the on-disk annotation files and lose any edits ",
      "made this session.",
      footer = tagList(modalButton("Cancel"),
                       actionButton("revert_confirm", "Discard",
                                    class = "btn btn-danger"))
    ))
  })
  observeEvent(input$revert_confirm, {
    pending_names(load_settlement_names() %||% data.frame(
      settlement_id = integer(0), name = character(0),
      population = numeric(0),
      inkarnate = character(0), notes = character(0),
      stringsAsFactors = FALSE))
    pending_relocations(load_relocations())
    pending_features(read_custom_features() %||% empty_features_sf())
    reset_dirty()
    removeModal()
    showNotification("Reverted to disk.", type = "message")
  })

  observeEvent(input$quit_btn, {
    if (dirty()) {
      showModal(modalDialog(
        title = "Unsaved changes",
        "You have unsaved annotation edits. Save before quitting?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("quit_no_save", "Quit without saving",
                       class = "btn btn-danger"),
          actionButton("quit_save", "Save & quit",
                       class = "btn btn-success"))
      ))
    } else {
      stopApp()
    }
  })
  observeEvent(input$quit_save, {
    nm <- pending_names()
    write.csv(nm, SETTLEMENT_NAMES_PATH(), row.names = FALSE)
    save_relocations(pending_relocations())
    save_custom_features(pending_features())
    stopApp()
  })
  observeEvent(input$quit_no_save, { stopApp() })
}

# =============================================================================
# LAUNCH
# =============================================================================

app <- shinyApp(ui = ui, server = server)
if (!interactive()) {
  shiny::runApp(app, launch.browser = TRUE)
} else {
  app
}
