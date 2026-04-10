# Packages
library(shiny)
library(bslib)
library(mapgl)

MAP_CENTER <- c(-51.925, -14.235)
MAP_ZOOM <- 2
SOUTH_AMERICA_CENTER <- c(-60, -17)
SOUTH_AMERICA_ZOOM <- 3.2

MAP_STYLES <- c(
  "Padrão" = "standard",
  "Ruas" = "streets",
  "Ao ar livre" = "outdoors",
  "Satélite" = "satellite",
  "Escuro" = "dark"
)

WMS_LAYERS <- list(
  intense_rain_24h = list(
    label = "Chuva intensa 24h",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:Chuva_intensa_24h"
  ),
  intense_rain_48h = list(
    label = "Chuva intensa 48h",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:Chuva_intensa_48h"
  ),
  precip_forecast = list(
    label = "Mosaico precipitacao previsto",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:mosaico_precipitacao_previsto"
  ),
  fire_risk_forecast = list(
    label = "Mosaico risco fogo previsto",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:mosaico_risco_fogo_previsto"
  ),
  temp_max = list(
    label = "Mosaico temperatura max",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:mosaico_temperatura_max"
  ),
  temp_min = list(
    label = "Mosaico temperatura min",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:mosaico_temperatura_min"
  ),
  humidity_max = list(
    label = "Mosaico umidade max",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:mosaico_umidade_max"
  ),
  humidity_min = list(
    label = "Mosaico umidade min",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:mosaico_umidade_min"
  ),
  humidity_forecast = list(
    label = "Mosaico umidade prevista",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:mosaico_umidade_prevista"
  ),
  severe_weather_24h = list(
    label = "Tempo severo 24h",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:tempo_severo_24h"
  ),
  severe_weather_48h = list(
    label = "Tempo severo 48h",
    service_url = "https://ide.saude.gov.br/geoserver/INPE/wms",
    layer_name = "INPE:tempo_severo_48h"
  ),
  burn_hotspots = list(
    label = "Focos de queimadas",
    service_url = "https://ide.saude.gov.br/geoserver/geonode/wms",
    layer_name = "geonode:focos_de_queimadas_478744e1d7aa083ddedb6678ff4dccc8"
  )
)

is_dark_style <- function(style_name) {
  identical(style_name, "dark")
}

wms_layer_ids <- function() {
  names(WMS_LAYERS)
}

wms_layer_choices <- function() {
  stats::setNames(
    wms_layer_ids(),
    vapply(WMS_LAYERS, `[[`, character(1), "label")
  )
}

get_wms_layer <- function(layer_id) {
  WMS_LAYERS[[layer_id]]
}

wms_source_id <- function(layer_id) {
  paste0("wms-source-", layer_id)
}

wms_layer_id <- function(layer_id) {
  paste0("wms-layer-", layer_id)
}

goes_source_id <- function() {
  "goes-latest-source"
}

goes_layer_id <- function() {
  "goes-latest-layer"
}

wms_tiles_url <- function(layer_id) {
  layer <- get_wms_layer(layer_id)

  sprintf(
    paste0(
      layer$service_url,
      "?service=WMS&request=GetMap&version=1.1.0",
      "&layers=%s",
      "&styles=",
      "&format=image/png",
      "&transparent=true",
      "&srs=EPSG:3857",
      "&bbox={bbox-epsg-3857}",
      "&width=256&height=256"
    ),
    layer$layer_name
  )
}

wms_legend_url <- function(layer_id) {
  layer <- get_wms_layer(layer_id)

  sprintf(
    paste0(
      layer$service_url,
      "?service=WMS&version=1.1.0&request=GetLegendGraphic",
      "&format=image/png",
      "&layer=%s"
    ),
    layer$layer_name
  )
}

goes_tiles_url <- function() {
  paste0(
    "https://nowcoast.noaa.gov/geoserver/observations/satellite/wms?",
    "service=WMS&request=GetMap&version=1.1.1",
    "&layers=global_visible_imagery_mosaic",
    "&styles=reflectance",
    "&format=image/png",
    "&transparent=true",
    "&srs=EPSG:3857",
    "&bbox={bbox-epsg-3857}",
    "&width=256&height=256"
  )
}

legend_theme <- function(dark_mode) {
  list(
    background = if (isTRUE(dark_mode)) "rgba(9,15,25,0.88)" else "rgba(255,255,255,0.92)",
    border = if (isTRUE(dark_mode)) "1px solid rgba(148,163,184,0.22)" else "1px solid rgba(15,23,42,0.08)",
    shadow = if (isTRUE(dark_mode)) "0 12px 32px rgba(0,0,0,0.45)" else "0 2px 10px rgba(0,0,0,0.18)",
    text = if (isTRUE(dark_mode)) "#e2e8f0" else "#0f172a"
  )
}

wms_legend_html <- function(layer_id, dark_mode = FALSE) {
  layer <- get_wms_layer(layer_id)
  theme <- legend_theme(dark_mode)

  paste0(
    "<div style='background:", theme$background, ";",
    "color:", theme$text, ";",
    "padding:10px 12px;",
    "border-radius:12px;",
    "border:", theme$border, ";",
    "box-shadow:", theme$shadow, ";",
    "backdrop-filter:blur(10px);",
    "max-width:220px;'>",
    "<div style='font-weight:600; margin-bottom:8px;'>", layer$label, "</div>",
    "<img src='", wms_legend_url(layer_id), "'",
    " alt='Legenda da camada WMS ", layer$label, "'",
    " style='display:block; max-width:100%; height:auto;'/>",
    "</div>"
  )
}

app_css <- function() {
  "
  :root {
    --panel-bg: rgba(255, 255, 255, 0.9);
    --panel-border: rgba(15, 23, 42, 0.08);
    --panel-text: #0f172a;
    --panel-muted: #475569;
    --panel-shadow: 0 18px 40px rgba(15, 23, 42, 0.16);
    --app-font: 'Space Grotesk', 'Segoe UI', sans-serif;
  }
  body,
  button,
  input,
  select,
  textarea {
    font-family: var(--app-font);
  }
  #my_map {
    height: 100vh !important;
    width: 100vw !important;
  }
  body.dark-view {
    --panel-bg: rgba(9, 15, 25, 0.82);
    --panel-border: rgba(148, 163, 184, 0.18);
    --panel-text: #e2e8f0;
    --panel-muted: #94a3b8;
    --panel-shadow: 0 24px 48px rgba(0, 0, 0, 0.42);
  }
  .floating-panel {
    position: absolute;
    top: 20px;
    left: 20px;
    z-index: 1000;
    width: 300px;
    padding: 18px;
    border-radius: 18px;
    border: 1px solid var(--panel-border);
    background: var(--panel-bg);
    color: var(--panel-text);
    box-shadow: var(--panel-shadow);
    backdrop-filter: blur(18px);
  }
  .floating-panel h3 {
    margin: 0 0 10px 0;
    font-size: 1.1rem;
    letter-spacing: 0.02em;
  }
  .floating-panel p {
    margin-bottom: 0;
    color: var(--panel-muted);
  }
  .floating-panel .form-label,
  .floating-panel .form-check-label,
  .floating-panel .irs-from,
  .floating-panel .irs-to,
  .floating-panel .irs-single,
  .floating-panel .form-select,
  .floating-panel .irs--shiny .irs-line,
  .floating-panel .irs--shiny .irs-min,
  .floating-panel .irs--shiny .irs-max,
  .floating-panel .irs--shiny .irs-grid-text {
    color: var(--panel-text);
  }
  .floating-panel .form-select {
    background-color: rgba(255, 255, 255, 0.72);
    border-color: var(--panel-border);
  }
  body.dark-view .floating-panel .form-select {
    background-color: rgba(15, 23, 42, 0.9);
    color: var(--panel-text);
  }
  .floating-panel hr {
    margin: 14px 0;
    border-color: var(--panel-border);
    opacity: 1;
  }
  .legend-panel {
    position: absolute;
    right: 20px;
    bottom: 20px;
    z-index: 1000;
  }
  .brand-logo {
    position: absolute;
    top: 20px;
    right: 20px;
    z-index: 1000;
    padding: 10px 14px;
    border-radius: 18px;
    background: rgba(255, 255, 255, 0.9);
    box-shadow: 0 18px 40px rgba(15, 23, 42, 0.16);
    backdrop-filter: blur(18px);
  }
  .brand-logo img {
    display: block;
    width: min(240px, 30vw);
    height: auto;
  }
  body.dark-view .brand-logo {
    background: rgba(255, 255, 255, 0.78);
    box-shadow: 0 24px 48px rgba(0, 0, 0, 0.42);
  }
  "
}

dark_mode_script <- function() {
  "
  Shiny.addCustomMessageHandler('set-dark-view', function(isDark) {
    document.body.classList.toggle('dark-view', !!isDark);
  });
  "
}

add_wms_catalog <- function(map, selected_layer, wms_opacity) {
  for (layer_id in wms_layer_ids()) {
    map <- map |>
      add_raster_source(
        id = wms_source_id(layer_id),
        tiles = wms_tiles_url(layer_id),
        tileSize = 256,
        maxzoom = 9
      ) |>
      add_raster_layer(
        id = wms_layer_id(layer_id),
        source = wms_source_id(layer_id),
        raster_opacity = wms_opacity,
        visibility = if (identical(layer_id, selected_layer)) "visible" else "none"
      )
  }

  map
}

add_goes_overlay <- function(map, show_goes, goes_opacity) {
  map |>
    add_raster_source(
      id = goes_source_id(),
      tiles = goes_tiles_url(),
      tileSize = 256,
      maxzoom = 8
    ) |>
    add_raster_layer(
      id = goes_layer_id(),
      source = goes_source_id(),
      raster_opacity = goes_opacity,
      visibility = if (isTRUE(show_goes)) "visible" else "none",
      before_id = wms_layer_id(wms_layer_ids()[1])
    )
}

build_map <- function(style_name, selected_layer, wms_opacity, show_goes, goes_opacity) {
  mapboxgl(
    style = mapbox_style(style_name),
    center = MAP_CENTER,
    zoom = MAP_ZOOM
  ) |>
    add_wms_catalog(selected_layer, wms_opacity) |>
    add_goes_overlay(show_goes, goes_opacity) |>
    move_layer(
      layer_id = goes_layer_id(),
      before_id = wms_layer_id(selected_layer)
    )
}

set_active_wms_layer <- function(proxy, selected_layer) {
  for (layer_id in wms_layer_ids()) {
    proxy <- proxy |>
      set_layout_property(
        layer_id = wms_layer_id(layer_id),
        name = "visibility",
        value = if (identical(layer_id, selected_layer)) "visible" else "none"
      )
  }

  proxy |>
    move_layer(
      layer_id = goes_layer_id(),
      before_id = wms_layer_id(selected_layer)
    )
}

set_wms_opacity <- function(proxy, opacity) {
  for (layer_id in wms_layer_ids()) {
    proxy <- proxy |>
      set_paint_property(
        layer_id = wms_layer_id(layer_id),
        name = "raster-opacity",
        value = opacity
      )
  }

  proxy
}

ui <- page_fillable(
  padding = 0,
  tags$head(
    tags$link(
      rel = "preconnect",
      href = "https://fonts.googleapis.com"
    ),
    tags$link(
      rel = "preconnect",
      href = "https://fonts.gstatic.com",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;500;700&display=swap"
    ),
    tags$style(HTML(app_css())),
    tags$script(HTML(dark_mode_script()))
  ),
  mapboxglOutput("my_map"),
  div(
    class = "brand-logo",
    img(
      src = "https://shiny.icict.fiocruz.br/alertarsaude/pin_obs_horizontal.png",
      alt = "Observatorio de Clima e Saude"
    )
  ),
  uiOutput("wms_legend"),
  div(
    class = "floating-panel",
    h3("Condições climáticas"),
    selectInput("style", "Estilo do mapa", choices = MAP_STYLES),
    hr(),
    selectInput(
      "wms_layer",
      "Camada WMS",
      choices = wms_layer_choices(),
      selected = wms_layer_ids()[1]
    ),
    hr(),
    sliderInput(
      "wms_opacity",
      "Opacidade da WMS",
      min = 0,
      max = 1,
      value = 0.7,
      step = 0.05
    ),
    hr(),
    checkboxInput("show_goes", "Mostrar imagem GOES mais recente", value = FALSE),
    sliderInput(
      "goes_opacity",
      "Opacidade da GOES",
      min = 0,
      max = 1,
      value = 0.75,
      step = 0.05
    ),
    hr(),
    p("Fonte: IDE-MS")
  )
)

server <- function(input, output, session) {
  sync_dark_mode <- function() {
    session$sendCustomMessage("set-dark-view", is_dark_style(input$style))
  }

  map_proxy <- function() {
    mapboxgl_proxy("my_map")
  }

  output$my_map <- renderMapboxgl({
    build_map(
      style_name = isolate(input$style),
      selected_layer = isolate(input$wms_layer),
      wms_opacity = isolate(input$wms_opacity),
      show_goes = isolate(input$show_goes),
      goes_opacity = isolate(input$goes_opacity)
    )
  })

  output$wms_legend <- renderUI({
    div(
      class = "legend-panel",
      HTML(
        wms_legend_html(
          layer_id = input$wms_layer,
          dark_mode = is_dark_style(input$style)
        )
      )
    )
  })

  session$onFlushed(function() {
    map_proxy() |>
      fly_to(
        center = SOUTH_AMERICA_CENTER,
        zoom = SOUTH_AMERICA_ZOOM,
        speed = 0.6,
        curve = 1.35,
        essential = TRUE
      )
  }, once = TRUE)

  observe(sync_dark_mode())

  observeEvent(input$style, {
    sync_dark_mode()

    map_proxy() |>
      set_style(
        style = mapbox_style(input$style),
        preserve_layers = TRUE
      )
  }, ignoreInit = TRUE)

  observeEvent(input$wms_layer, {
    set_active_wms_layer(map_proxy(), input$wms_layer)
  }, ignoreInit = TRUE)

  observeEvent(input$wms_opacity, {
    set_wms_opacity(map_proxy(), input$wms_opacity)
  }, ignoreInit = TRUE)

  observeEvent(input$show_goes, {
    map_proxy() |>
      set_layout_property(
        layer_id = goes_layer_id(),
        name = "visibility",
        value = if (isTRUE(input$show_goes)) "visible" else "none"
      ) |>
      move_layer(
        layer_id = goes_layer_id(),
        before_id = wms_layer_id(input$wms_layer)
      )
  }, ignoreInit = TRUE)

  observeEvent(input$goes_opacity, {
    map_proxy() |>
      set_paint_property(
        layer_id = goes_layer_id(),
        name = "raster-opacity",
        value = input$goes_opacity
      )
  }, ignoreInit = TRUE)
}

shinyApp(ui, server)
