# app.R
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(qs)
library(readr)
library(ggplot2)
library(scales)   # number(), pseudo_log_trans()

# ==============================
# CONFIG: lecture depuis GitHub Raw (fallback local)
# ==============================
REMOTE_BASE <- "https://raw.githubusercontent.com/hgesdrn/FeuxSaguenay2023/main"  # branche: main
USE_REMOTE  <- FALSE   # TRUE = tente GitHub d'abord; FALSE = tente local d'abord

gh_url <- function(path_rel) paste0(REMOTE_BASE, "/", path_rel)

load_qs_safely <- function(local_path, remote_rel = NULL, verbose = FALSE) {
  try_remote <- function() {
    if (is.null(remote_rel) || remote_rel == "" || !USE_REMOTE) return(NULL)
    url <- gh_url(remote_rel)
    tf  <- tempfile(fileext = ".qs")
    ok  <- try(utils::download.file(url, tf, mode = "wb", quiet = TRUE), silent = TRUE)
    if (inherits(ok, "try-error")) return(NULL)
    tryCatch(qread(tf), error = function(e) NULL)
  }
  try_local <- function() {
    if (!file.exists(local_path)) return(NULL)
    tryCatch(qread(local_path), error = function(e) NULL)
  }
  out <- try_remote()
  if (!is.null(out)) return(out)
  try_local()
}

load_csv_safely <- function(local_path, remote_rel = NULL, verbose = FALSE) {
  try_remote <- function() {
    if (is.null(remote_rel) || remote_rel == "" || !USE_REMOTE) return(NULL)
    url <- gh_url(remote_rel)
    out <- try(readr::read_csv(url, show_col_types = FALSE), silent = TRUE)
    if (inherits(out, "try-error")) return(NULL) else out
  }
  try_local <- function() {
    if (!file.exists(local_path)) return(NULL)
    tryCatch(readr::read_csv(local_path, show_col_types = FALSE), error = function(e) NULL)
  }
  out <- try_remote()
  if (!is.null(out)) return(out)
  try_local()
}

# ==============================
# Chemins (dans le repo)
# ==============================
sag_rel  <- "data/REGION_SAG_WGS84.qs"
feux_rel <- "data/feux_2023_simpl.qs"
csv_rel  <- "data/feux_2023_table.csv"

# ===== Helpers génériques =====
canon_nofeu <- function(x) {
  x <- as.character(x)
  x <- trimws(gsub("\\s+", "", x))
  num <- suppressWarnings(as.numeric(x))
  ifelse(is.na(num), x, as.character(num))
}
sort_nofeu_numeric <- function(x) {
  x_chr <- as.character(x)
  x_num <- suppressWarnings(as.numeric(x_chr))
  ord <- order(is.na(x_num), x_num, x_chr, na.last = TRUE)
  unname(unique(x_chr[ord]))
}
expand_bbox <- function(bb, frac = 0.03) {
  xmn <- as.numeric(bb["xmin"]); xmx <- as.numeric(bb["xmax"])
  ymn <- as.numeric(bb["ymin"]); ymx <- as.numeric(bb["ymax"])
  w <- xmx - xmn; h <- ymx - ymn
  if (!is.finite(w) || w == 0) w <- 0.01
  if (!is.finite(h) || h == 0) h <- 0.01
  dx <- max(w * frac, 1e-4)
  dy <- max(h * frac, 1e-4)
  c(xmin = xmn - dx, ymin = ymn - dy, xmax = xmx + dx, ymax = ymx + dy)
}

# ===== Chargement =====
sag  <- load_qs_safely(local_path = "data/REGION_SAG_WGS84.qs", remote_rel = sag_rel)
feux <- load_qs_safely(local_path = "data/feux_2023_simpl.qs", remote_rel = feux_rel)
ftab <- load_csv_safely(local_path = "data/feux_2023_table.csv", remote_rel = csv_rel)

if (!is.null(feux)) {
  feux <- feux |>
    mutate(
      NOFEU = canon_nofeu(NOFEU),
      lid   = as.character(dplyr::row_number())  # id interne pour clic
    )
}
if (!is.null(ftab)) {
  ftab <- ftab |>
    mutate(
      NOFEU  = canon_nofeu(NOFEU),
      CAUSE  = as.character(CAUSE),
      SUP_HA = suppressWarnings(as.numeric(SUP_HA))
    )
}

# ===== Choix (intersection pour garantir la présence de géométrie) =====
choices_nofeu <- if (!is.null(feux) && !is.null(ftab)) {
  sort_nofeu_numeric(intersect(unique(feux$NOFEU), unique(ftab$NOFEU)))
} else character(0)

# ===== Axe Y =====
y_trans  <- pseudo_log_trans(base = 10, sigma = 10)
y_limits <- c(0, 100000)
y_breaks <- c(0, 10, 100, 1000, 10000, 100000)

# ===============================
# UI
# ===============================
ui <- fluidPage(
  tags$style(HTML("
    .header-title {
      background-color: #2C3E50;
      color: white;
      padding: 20px;
      font-size: 22px;
      font-weight: bold;
      text-align: left;
      text-transform: uppercase;
      margin-bottom: 20px;
      box-shadow: 2px 2px 8px rgba(0,0,0,0.2);
    }
    .box-style {
      background-color: #f9f9f9;
      border: 1px solid #ccc;
      border-radius: 8px;
      padding: 20px;
      box-shadow: 2px 2px 8px rgba(0,0,0,0.1);
      height: 700px;
      overflow-y: auto;
    }
    /* Tooltips sombres */
    .leaflet-tooltip.lbl-dark{
      background-color: rgba(20,20,20,0.92);
      color: #f1f1f1;
      border: 1px solid rgba(255,255,255,0.22);
      border-radius: 6px;
      padding: 2px 8px;
      box-shadow: 0 2px 6px rgba(0,0,0,0.35);
      white-space: nowrap;
      pointer-events: none;
      text-shadow: 0 1px 2px rgba(0,0,0,0.6);
      font-weight: 700;
      letter-spacing: 0.2px;
      line-height: 1.15;
    }
    .leaflet-tooltip.lbl-dark.lbl-dark-selected{
      border-color: #ff9900;
      box-shadow: 0 0 0 2px rgba(255,153,0,0.15), 0 2px 6px rgba(0,0,0,0.35);
    }
  ")),
  div("LES FEUX DE FORÊT AU SAGUENAY–LAC-SAINT-JEAN DE 2023", class = "header-title"),
  fluidRow(
    column(3,
           div(class = "box-style",
               selectInput(
                 "nofeu", "Numéro de feu :",
                 choices  = c("", choices_nofeu),
                 selected = ""
               ),
               sliderInput("alpha_feux", "Bouger le curseur pour ajuster la transparence des feux",
                           min = 0, max = 100, value = 75, step = 5, post = " %"),
               tags$hr(style = "border-top: 1px solid #aaa; margin-top: 20px; margin-bottom: 20px;"),
               htmlOutput("info_text", height = "60px"),
               tags$hr(style = "border-top: 1px solid #aaa; margin-top: 20px; margin-bottom: 20px;"),
               plotOutput("barplot", height = "275px")
           )
    ),
    column(9,
           div(class = "box-style",
               leafletOutput("map", height = "640px")
           )
    )
  )
)

# ===============================
# SERVER
# ===============================
server <- function(input, output, session) {
  
  clamp01 <- function(x) max(0, min(1, x))
  get_alpha <- reactive({
    val <- input$alpha_feux
    if (is.null(val) || is.na(val)) return(0.75)
    clamp01(val / 100)
  })
  
  # --- Carte de base + panes ---
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagerie") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Fond neutre") |>
      addMapPane("pane_region", zIndex = 410) |>
      addMapPane("pane_feux",   zIndex = 420) |>
      addMapPane("pane_sel",    zIndex = 430) |>
      fitBounds(-74.5, 48, -69, 53) |>
      addLayersControl(
        baseGroups    = unname(c("Imagerie","Fond neutre")),
        overlayGroups = unname(c("Feux (tous)", "Feu sélectionné")),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addScaleBar(position = "bottomleft")
  })
  
  # --- Région ---
  observe({
    req(!is.null(sag))
    leafletProxy("map") |>
      clearGroup("Région") |>
      addPolygons(
        data        = sag,
        fillColor   = "#bdc3c7",
        fillOpacity = 0.35,
        color       = "#7f8c8d",
        weight      = 1,
        opacity     = 0.8,
        group       = "Région",
        options     = pathOptions(pane = "pane_region", interactive = FALSE)
      )
  })
  
  # --- Tous les feux ---
  observe({
    req(!is.null(feux))
    opa_bg <- get_alpha()
    leafletProxy("map") |>
      clearGroup("Feux (tous)") |>
      addPolygons(
        data        = feux,
        fillColor   = "#990000",
        fillOpacity = opa_bg,
        stroke      = FALSE,
        group       = "Feux (tous)",
        options     = pathOptions(pane = "pane_feux"),
        label       = ~paste0("No. feu: ", NOFEU),
        labelOptions = labelOptions(
          direction = "auto",
          textOnly  = TRUE,
          textsize  = "14px",
          offset    = c(0, -4),
          className = "lbl-dark"
        ),
        layerId = ~lid
      )
  })
  
  # --- Clic sur polygone -> synchronise le menu ---
  observeEvent(input$map_shape_click, {
    evt <- input$map_shape_click
    if (is.null(evt$id) || is.null(feux) || nrow(feux) == 0) return()
    idx <- which(feux$lid == evt$id)
    if (length(idx) == 0) return()
    updateSelectInput(session, "nofeu", selected = unname(feux$NOFEU[idx[1]]))
  })
  
  # --- Sélections réactives ---
  feu_tbl_sel <- reactive({
    req(!is.null(ftab))
    sel <- input$nofeu
    if (is.null(sel) || sel == "") return(NULL)
    sel <- canon_nofeu(sel)
    ftab |> filter(NOFEU == sel) |> slice(1)
  })
  
  feu_sf_sel <- reactive({
    req(!is.null(feux))
    sel <- input$nofeu
    if (is.null(sel) || sel == "") return(NULL)
    sel <- canon_nofeu(sel)
    feux |> filter(NOFEU == sel)
  })
  
  # --- Feu sélectionné + zoom ---
  observeEvent(feu_sf_sel(), {
    dat <- feu_sf_sel()
    leafletProxy("map") |> clearGroup("Feu sélectionné")
    if (is.null(dat) || nrow(dat) == 0) return()
    
    opa_sel <- isolate(get_alpha())
    leafletProxy("map") |>
      addPolygons(
        data        = dat,
        #fillColor   = "#990000",
        #fillOpacity = opa_sel,
        color       = "#ff9900",
        weight      = 1,
        opacity     = 1,
        group       = "Feu sélectionné",
        options     = pathOptions(pane = "pane_sel"),
        label       = ~paste0("NOFEU: ", NOFEU),
        labelOptions = labelOptions(
          direction = "auto",
          textOnly  = TRUE,
          textsize  = "12px",
          offset    = c(0, -4),
          className = "lbl-dark lbl-dark-selected"
        ),
        highlightOptions = highlightOptions(bringToFront = TRUE)
      )
    
    bb <- sf::st_bbox(dat)
    bb <- expand_bbox(bb, frac = 0.03)
    leafletProxy("map") |>
      flyToBounds(
        lng1 = as.numeric(bb["xmin"]), lat1 = as.numeric(bb["ymin"]),
        lng2 = as.numeric(bb["xmax"]), lat2 = as.numeric(bb["ymax"])
      )
  }, ignoreInit = TRUE)
  
  # --- Opacité sélection sans re-zoom ---
  observeEvent(input$alpha_feux, {
    dat <- isolate(feu_sf_sel())
    leafletProxy("map") |> clearGroup("Feu sélectionné")
    if (is.null(dat) || nrow(dat) == 0) return()
    opa_sel <- get_alpha()
    leafletProxy("map") |>
      addPolygons(
        data        = dat,
        # fillColor   = "#990000",
        # fillOpacity = opa_sel,
        color       = "#ff9900",
        weight      = 1,
        opacity     = 1,
        group       = "Feu sélectionné",
        options     = pathOptions(pane = "pane_sel"),
        label       = ~paste0("NOFEU: ", NOFEU),
        labelOptions = labelOptions(
          direction = "auto",
          textOnly  = TRUE,
          textsize  = "12px",
          offset    = c(0, -4),
          className = "lbl-dark lbl-dark-selected"
        ),
        highlightOptions = highlightOptions(bringToFront = TRUE)
      )
  }, ignoreInit = TRUE)
  
  # --- Fiche d'info ---
  output$info_text <- renderUI({
    ft <- feu_tbl_sel()
    val_or_blank <- function(x) if (is.null(x) || length(x) == 0 || is.na(x) || x == "") "&nbsp;" else as.character(x)
    
    if (is.null(ft) || nrow(ft) == 0) {
      HTML(paste0(
        "<b>Feu sélectionné :</b> ", "&nbsp;",
        "<br><b>Cause :</b> ",        "&nbsp;",
        "<br><b>Superficie :</b> ",   "&nbsp;",
        "<br><b>Début :</b> ",        "&nbsp;",
        "<br><b>Éteint :</b> ",       "&nbsp;"
      ))
    } else {
      sup_val <- suppressWarnings(as.numeric(ft$SUP_HA[1]))
      sup_txt <- if (is.finite(sup_val)) paste0(number(sup_val, accuracy = 0.1, big.mark = " "), " ha") else ""
      to_date_txt <- function(x) {
        if (inherits(x, "Date")) return(format(x, "%Y-%m-%d"))
        out <- try(as.Date(x), silent = TRUE)
        if (inherits(out, "try-error") || is.na(out)) "" else format(out, "%Y-%m-%d")
      }
      d1_txt <- if (!is.null(ft$DATE_DEBUT[1]) && !is.na(ft$DATE_DEBUT[1])) to_date_txt(ft$DATE_DEBUT[1]) else ""
      d2_txt <- if (!is.null(ft$DATE_ETEIN[1]) && !is.na(ft$DATE_ETEIN[1])) to_date_txt(ft$DATE_ETEIN[1]) else ""
      cause_txt <- if (!is.null(ft$CAUSE[1]) && !is.na(ft$CAUSE[1])) as.character(ft$CAUSE[1]) else ""
      
      HTML(paste0(
        "<b>Feu sélectionné :</b> ", val_or_blank(ft$NOFEU[1]),
        "<br><b>Cause :</b> ",        val_or_blank(cause_txt),
        "<br><b>Superficie :</b> ",   val_or_blank(sup_txt),
        "<br><b>Début :</b> ",        val_or_blank(d1_txt),
        "<br><b>Éteint :</b> ",       val_or_blank(d2_txt)
      ))
    }
  })
  
  # --- Barplot ---
  output$barplot <- renderPlot({
    ft <- feu_tbl_sel()
    if (is.null(ft) || nrow(ft) == 0) {
      pdat <- data.frame(NOFEU = "Sélectionnez un feu", SUP_HA = 0, stringsAsFactors = FALSE)
    } else {
      pdat <- ft[1, c("NOFEU","SUP_HA"), drop = FALSE]
      pdat$NOFEU  <- as.character(pdat$NOFEU)
      pdat$SUP_HA <- suppressWarnings(as.numeric(pdat$SUP_HA))
    }
    
    tick_size  <- 12
    label_size <- tick_size + 2
    
    ggplot(pdat, aes(x = NOFEU, y = SUP_HA)) +
      geom_col(fill = "#990000", alpha = 0.6, width = 0.75) +
      labs(x = "Numéro de feu", y = "Superficie (ha)") +
      scale_y_continuous(
        trans  = y_trans,
        breaks = y_breaks,
        labels = scales::label_number(big.mark = " "),
        oob    = scales::oob_squish
      ) +
      coord_cartesian(ylim = y_limits) +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        axis.text.x  = element_text(size = tick_size,  face = "bold"),
        axis.text.y  = element_text(size = tick_size,  face = "bold"),
        axis.title.x = element_text(size = label_size, face = "bold"),
        axis.title.y = element_text(size = label_size, face = "bold")
      )
  })
  
}

shinyApp(ui, server)
