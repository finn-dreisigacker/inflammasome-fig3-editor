# =============================================================================
# Fig. 3 editor (Shiny) - move peaks, change widths/heights, plateau and texts,
# place labels by clicking. Saves to ../fig3_params.csv, exports PNG + SVG
# to "04_Fig/Fig - 03"; "Download ZIP" delivers PNG + SVG + params.
#
# Start (from the project root INFLAMMASOM/ in RStudio):
#   shiny::runApp("04_Fig/Fig - 03/fig3_app")
# Web version (Shinylive/webR, GitHub Pages): built by
#   .github/workflows/deploy.yml - there only the ZIP download is offered.
#
# Packages: shiny, shinyWidgets, dplyr, tidyr, purrr, readr, ggplot2,
#           patchwork, ggtext, ragg, svglite, zip
# =============================================================================

library(shiny)
library(shinyWidgets)

on_web <- identical(R.version$os, "emscripten")      # Shinylive / webR
# runApp sets wd = app dir; the web build copies the shared files next to app.R
app_file <- function(f) if (file.exists(f)) f else file.path("..", f)
source(app_file("fig3_functions.R"), encoding = "UTF-8")

params_path <- normalizePath(app_file("fig3_params.csv"), mustWork = FALSE)
out_dir     <- normalizePath(dirname(params_path), mustWork = TRUE)
if (!file.exists(params_path)) fig3_write_params(fig3_defaults(), params_path)
# webR has no Arial: register the metric-compatible Liberation Sans (copied
# into fonts/ by the web build) under that name, so labels land as locally
web_fonts <- file.path(app_file("fonts"),
                       c("LiberationSans-Regular.ttf", "LiberationSans-Bold.ttf"))
if (on_web && all(file.exists(web_fonts))) {
  systemfonts::register_font("Arial", plain = web_fonts[1], bold = web_fonts[2])
}

# ---- time slider: real time, stored as minutes -------------------------------
time_grid <- c(-60:60, seq(65, 180, 5), seq(195, 360, 15),
               seq(7, 24) * 60, seq(26, 48, 2) * 60)
time_lab  <- fmt_time(time_grid)
lab_to_min <- function(lab) time_grid[match(lab, time_lab)]
nearest_lab <- function(v) time_lab[which.min(abs(time_grid - v))]
time_input <- function(id, label, value) {
  sliderTextInput(id, label, choices = time_lab, selected = nearest_lab(value),
                  grid = FALSE, force_edges = TRUE, width = "100%")
}
width_note <- function(peak_min, sd, side) {
  mu <- time_to_x(peak_min)
  x  <- if (side == "rise") mu - sd else mu + sd
  paste0("1 SD reicht bis ", fmt_time(x_to_time(x)))
}

curve_choices <- c("NLRP3, ASC, caspase-1" = "complex", "GSDMD" = "gsdmd",
                   "IL-1β, IL-18" = "il", "Cellular stress" = "stress",
                   "Priming (nur Style C)" = "priming")
text_labels <- c(
  phase1_title = "Phase 1 – Titel", phase1_sub = "Phase 1 – Untertitel",
  phase2_title = "Phase 2 – Titel", phase2_sub = "Phase 2 – Untertitel",
  phase3_title = "Phase 3 – Titel", phase3_sub = "Phase 3 – Untertitel",
  bar0 = "Balken: Belastung", bar1 = "Balken: ≤15 min", bar2 = "Balken: >15 min–1 h",
  bar3 = "Balken: >1–6 h", bar4 = "Balken: >6–24 h", bar5 = "Balken: >24 h",
  x_title = "x-Achsentitel", y_title = "y-Achsentitel",
  row1_title = "Style C – Zeile 1 Titel", row1_sub = "Style C – Zeile 1 Untertitel",
  row2_title = "Style C – Zeile 2 Titel", row2_sub = "Style C – Zeile 2 Untertitel",
  row3_title = "Style C – Zeile 3 Titel", row3_sub = "Style C – Zeile 3 Untertitel",
  row4_title = "Style C – Zeile 4 Titel", row4_sub = "Style C – Zeile 4 Untertitel",
  row5_title = "Style C – Zeile 5 Titel", row5_sub = "Style C – Zeile 5 Untertitel"
)

px  <- function(mm) round(mm / 25.4 * 120)   # preview at 120 dpi = print scale
RES <- 120

# ---- UI ----------------------------------------------------------------------
ui <- fluidPage(
  tags$head(tags$style(HTML("
    .note {color:#666; font-size:12px; margin-top:-10px; margin-bottom:10px;}
    .status {color:#2A78D6; font-weight:600;}
    .well {padding-top:8px;}
    .shiny-plot-output {margin-left:0;}
  "))),
  titlePanel("Fig. 3 – Inflammasom-Kinetik · Editor"),
  sidebarLayout(
    sidebarPanel(width = 4,
      tabsetPanel(id = "tabs",
        tabPanel("Kurven",
          br(),
          selectInput("cv_key", "Kurve", choices = curve_choices),
          uiOutput("cv_ui")
        ),
        tabPanel("Plateau",
          br(),
          selectInput("sv_key", "Gestrichelter Ast (anstrengende Belastung)",
                      choices = c("NLRP3, ASC, caspase-1" = "complex",
                                  "IL-1β, IL-18" = "il")),
          div(class = "note", "Der Ast zweigt am Peak der Kurve ab und folgt dann dem Plateau."),
          uiOutput("sv_ui")
        ),
        tabPanel("Labels",
          br(),
          selectInput("lb_key", "Label", choices = NULL),
          radioButtons("click_mode", "Klick in den Plot setzt …",
                       c("Label-Position" = "pos", "Linienziel" = "target"),
                       inline = TRUE),
          uiOutput("lb_ui")
        ),
        tabPanel("Texte",
          br(),
          uiOutput("tx_ui")
        ),
        tabPanel("Allgemein",
          br(),
          uiOutput("gen_ui")
        )
      ),
      hr(),
      if (!on_web) actionButton("save",   "Speichern (fig3_params.csv)", class = "btn-primary"),
      if (!on_web) actionButton("export", "Export PNG + SVG"),
      downloadButton("download_zip", "Download PNG + SVG (ZIP)",
                     class = if (on_web) "btn-primary" else "btn-default"),
      actionButton("reset",  "Auf v4-Standard zurücksetzen"),
      br(), br(),
      textOutput("status", inline = FALSE) %>% tagAppendAttributes(class = "status")
    ),
    mainPanel(width = 8,
      plotOutput("header", height = "auto"),
      plotOutput("plot", height = "auto", click = "plot_click"),
      div(class = "note",
          "Vorschau im Druckmaßstab (183 mm). Klick in den Plot platziert das ",
          "gewählte Label bzw. setzt sein Linienziel (Tab „Labels“).")
    )
  )
)

# ---- server --------------------------------------------------------------------
server <- function(input, output, session) {

  rv <- reactiveValues(P = fig3_read_params(params_path), reset_n = 0,
                       msg = if (on_web) {
                         paste("Web-Version: Änderungen gelten nur in diesem Tab.",
                               "Das ZIP enthält PNG, SVG und fig3_params.csv.")
                       } else paste("Geladen:", params_path))
  style_rv <- reactiveVal(isolate(rv$P$settings$style))

  cur <- reactive(fig3_curves(rv$P))
  lab_set <- reactive(if (identical(style_rv(), "stages")) "labels_C" else "labels_A")

  # setters -------------------------------------------------------------------
  set_tab <- function(tab, key, param, value) {
    if (is.null(value) || length(value) == 0 || (is.numeric(value) && is.na(value))) return()
    P <- rv$P
    i <- which(P[[tab]]$key == key)
    if (length(i) != 1) return()
    if (!identical(P[[tab]][[param]][i], value)) {
      P[[tab]][[param]][i] <- value
      rv$P <- P
    }
  }
  set_setting <- function(name, value) {
    if (is.null(value)) return()
    P <- rv$P; P$settings[[name]] <- value; rv$P <- P
  }

  # ---- Kurven ------------------------------------------------------------------
  output$cv_ui <- renderUI({
    rv$reset_n
    k <- input$cv_key
    r <- isolate(rv$P$curves); r <- r[r$key == k, ]
    tagList(
      time_input("cv_peak", "Peak (Zeit nach Belastungsende; negativ = während Belastung)", r$peak_min),
      sliderInput("cv_height", "Höhe (relativ, Komplex-Peak = 1)", 0, 1.2, r$height, step = 0.01, width = "100%"),
      sliderInput("cv_rise", "Anstieg (schmal ↔ breit)", 0.1, 3, r$rise, step = 0.05, width = "100%"),
      div(class = "note", textOutput("cv_rise_note", inline = TRUE)),
      sliderInput("cv_fall", "Abfall (schmal ↔ breit)", 0.1, 3, r$fall, step = 0.05, width = "100%"),
      div(class = "note", textOutput("cv_fall_note", inline = TRUE)),
      sliderInput("cv_tail_share", "Anteil langsamer Ausläufer (0 = keiner)", 0, 1, r$tail_share, step = 0.05, width = "100%"),
      sliderInput("cv_tail_fall", "Ausläufer: Abfall (schmal ↔ breit)", 0.2, 4, r$tail_fall, step = 0.05, width = "100%"),
      if (k == "priming") div(class = "note", "Priming erscheint nur in Style C.")
    )
  })
  cur_row <- reactive({ r <- rv$P$curves; r[r$key == input$cv_key, ] })
  output$cv_rise_note <- renderText({ r <- cur_row(); req(nrow(r) == 1); width_note(r$peak_min, r$rise, "rise") })
  output$cv_fall_note <- renderText({ r <- cur_row(); req(nrow(r) == 1); width_note(r$peak_min, r$fall, "fall") })

  observeEvent(input$cv_peak,       set_tab("curves", input$cv_key, "peak_min",   lab_to_min(input$cv_peak)))
  observeEvent(input$cv_height,     set_tab("curves", input$cv_key, "height",     input$cv_height))
  observeEvent(input$cv_rise,       set_tab("curves", input$cv_key, "rise",       input$cv_rise))
  observeEvent(input$cv_fall,       set_tab("curves", input$cv_key, "fall",       input$cv_fall))
  observeEvent(input$cv_tail_share, set_tab("curves", input$cv_key, "tail_share", input$cv_tail_share))
  observeEvent(input$cv_tail_fall,  set_tab("curves", input$cv_key, "tail_fall",  input$cv_tail_fall))

  # ---- Plateau -----------------------------------------------------------------
  output$sv_ui <- renderUI({
    rv$reset_n
    k <- input$sv_key
    r <- isolate(rv$P$sustained); r <- r[r$key == k, ]
    tagList(
      time_input("sv_anchor", "Plateau erreicht bei", r$anchor_min),
      sliderInput("sv_level", "Plateau-Niveau (relativ)", 0, 1.2, r$level, step = 0.01, width = "100%"),
      sliderInput("sv_slope", "Gefälle des Plateaus (flach ↔ steil)", 0, 0.3, r$slope, step = 0.005, width = "100%"),
      time_input("sv_fall", "Abfall ab", r$fall_min),
      sliderInput("sv_fall_width", "Abfalldauer (schmal ↔ breit)", 0.1, 2, r$fall_width, step = 0.05, width = "100%"),
      div(class = "note", textOutput("sv_fall_note", inline = TRUE))
    )
  })
  output$sv_fall_note <- renderText({
    r <- rv$P$sustained; r <- r[r$key == input$sv_key, ]; req(nrow(r) == 1)
    paste0("1 SD des Abfalls reicht bis ",
           fmt_time(x_to_time(time_to_x(r$fall_min) + r$fall_width)))
  })
  observeEvent(input$sv_anchor,     set_tab("sustained", input$sv_key, "anchor_min", lab_to_min(input$sv_anchor)))
  observeEvent(input$sv_level,      set_tab("sustained", input$sv_key, "level",      input$sv_level))
  observeEvent(input$sv_slope,      set_tab("sustained", input$sv_key, "slope",      input$sv_slope))
  observeEvent(input$sv_fall,       set_tab("sustained", input$sv_key, "fall_min",   lab_to_min(input$sv_fall)))
  observeEvent(input$sv_fall_width, set_tab("sustained", input$sv_key, "fall_width", input$sv_fall_width))

  # ---- Labels ------------------------------------------------------------------
  observeEvent(list(lab_set(), rv$reset_n), {
    keys <- rv$P[[lab_set()]]$key
    sel  <- if (isTRUE(isolate(input$lb_key) %in% keys)) isolate(input$lb_key) else keys[1]
    updateSelectInput(session, "lb_key", choices = keys, selected = sel)
  })

  output$lb_ui <- renderUI({
    rv$reset_n
    k <- input$lb_key; req(k)
    set_name <- lab_set()                     # re-render when the style switches
    L <- isolate(rv$P[[set_name]]); r <- L[L$key == k, ]
    req(nrow(r) == 1)
    tagList(
      textInput("lb_text", if (r$kind == "note") "Text (Hinweis, <br> = Zeilenumbruch)" else "Hauptzeile (fett)",
                r$text, width = "100%"),
      if (r$kind != "note") textInput("lb_sub", "Untertitel (optional, <br> = Zeilenumbruch)", r$subtitle, width = "100%"),
      fluidRow(
        column(6, numericInput("lb_x", "Position x", r$x, step = 0.01)),
        column(6, numericInput("lb_y", "Position y", r$y, step = 0.01))
      ),
      radioButtons("lb_hjust", "Ausrichtung am Punkt",
                   c("links" = "0", "mitte" = "0.5", "rechts" = "1"),
                   selected = as.character(r$hjust), inline = TRUE),
      checkboxInput("lb_leader", "Hinweislinie zeigen", isTRUE(as.logical(r$leader))),
      numericInput("lb_target", "Linienziel x (Höhe wird von der Kurve gelesen)", r$target_x, step = 0.01),
      div(class = "note", textOutput("lb_target_time", inline = TRUE))
    )
  })
  output$lb_target_time <- renderText({
    req(input$lb_target)
    paste0("Linienziel = ", fmt_time(x_to_time(input$lb_target)),
           " · Label bei ", fmt_time(x_to_time(input$lb_x %||% NA)))
  })

  observeEvent(input$lb_text,   set_tab(lab_set(), input$lb_key, "text",     input$lb_text))
  observeEvent(input$lb_sub,    set_tab(lab_set(), input$lb_key, "subtitle", input$lb_sub))
  observeEvent(input$lb_x,      set_tab(lab_set(), input$lb_key, "x",        input$lb_x))
  observeEvent(input$lb_y,      set_tab(lab_set(), input$lb_key, "y",        input$lb_y))
  observeEvent(input$lb_hjust,  set_tab(lab_set(), input$lb_key, "hjust",    as.numeric(input$lb_hjust)))
  observeEvent(input$lb_leader, set_tab(lab_set(), input$lb_key, "leader",   isTRUE(input$lb_leader)), ignoreNULL = TRUE)
  observeEvent(input$lb_target, set_tab(lab_set(), input$lb_key, "target_x", input$lb_target))

  observeEvent(input$plot_click, {
    cl <- input$plot_click; k <- input$lb_key; req(k, cl$x, cl$y)
    x <- round(cl$x, 2); y <- round(cl$y, 2)
    if (identical(input$click_mode, "target")) {
      set_tab(lab_set(), k, "target_x", x)
      updateNumericInput(session, "lb_target", value = x)
      rv$msg <- paste0("Linienziel von „", k, "“ auf ", fmt_time(x_to_time(x)), " gesetzt")
    } else {
      set_tab(lab_set(), k, "x", x); set_tab(lab_set(), k, "y", y)
      updateNumericInput(session, "lb_x", value = x)
      updateNumericInput(session, "lb_y", value = y)
      rv$msg <- paste0("Label „", k, "“ verschoben")
    }
  })

  # ---- Texte -------------------------------------------------------------------
  output$tx_ui <- renderUI({
    rv$reset_n
    Tx <- isolate(rv$P$texts)
    tagList(
      div(class = "note", "<br> erzeugt einen Zeilenumbruch."),
      lapply(names(text_labels), function(k) {
        textInput(paste0("tx_", k), text_labels[[k]], Tx[[k]], width = "100%")
      })
    )
  })
  lapply(names(text_labels), function(k) {
    observeEvent(input[[paste0("tx_", k)]], {
      v <- input[[paste0("tx_", k)]]
      if (!identical(rv$P$texts[[k]], v)) { P <- rv$P; P$texts[[k]] <- v; rv$P <- P }
    })
  })

  # ---- Allgemein ---------------------------------------------------------------
  output$gen_ui <- renderUI({
    rv$reset_n
    S <- isolate(rv$P$settings)
    tagList(
      radioButtons("gen_style", "Style", c("A – Kurven" = "curves", "C – Stufen" = "stages"),
                   selected = S$style, inline = TRUE),
      checkboxInput("gen_sus", "Gestrichelte Äste (anstrengende Belastung) zeigen", isTRUE(as.logical(S$show_sustained))),
      checkboxInput("gen_stress", "Cellular-stress-Fläche zeigen", isTRUE(as.logical(S$show_stress))),
      time_input("gen_b1", "Phasengrenze 1 → 2", S$phase_b1_min),
      time_input("gen_b2", "Phasengrenze 2 → 3", S$phase_b2_min)
    )
  })
  observeEvent(input$gen_style,  { set_setting("style", input$gen_style); style_rv(input$gen_style) })
  observeEvent(input$gen_sus,    set_setting("show_sustained", isTRUE(input$gen_sus)))
  observeEvent(input$gen_stress, set_setting("show_stress",    isTRUE(input$gen_stress)))
  observeEvent(input$gen_b1,     set_setting("phase_b1_min",   lab_to_min(input$gen_b1)))
  observeEvent(input$gen_b2,     set_setting("phase_b2_min",   lab_to_min(input$gen_b2)))

  # ---- Buttons -----------------------------------------------------------------
  observeEvent(input$save, {
    tryCatch({
      fig3_write_params(rv$P, params_path)
      rv$msg <- paste("Gespeichert:", params_path, format(Sys.time(), "(%H:%M:%S)"))
    }, error = function(e) rv$msg <- paste("Fehler beim Speichern:", conditionMessage(e)))
  })
  observeEvent(input$export, {
    tryCatch({
      f <- fig3_export(rv$P, out_dir, "Fig_inflammasome_theory_app")
      rv$msg <- paste("Exportiert:", paste(basename(f), collapse = ", "))
    }, error = function(e) rv$msg <- paste("Fehler beim Export:", conditionMessage(e)))
  })
  output$download_zip <- downloadHandler(
    filename = function() paste0("Fig_inflammasome_theory_", rv$P$settings$style, ".zip"),
    content = function(file) {
      tmp <- tempfile("fig3_"); dir.create(tmp)
      f <- fig3_export(rv$P, tmp, "Fig_inflammasome_theory")
      if (on_web) {   # svglite names the registered file's family; call it Arial again
        svg <- grep("\\.svg$", f, value = TRUE)
        writeLines(gsub('font-family: "Liberation Sans";', 'font-family: "Arial";',
                        readLines(svg, warn = FALSE, encoding = "UTF-8"), fixed = TRUE),
                   svg, useBytes = TRUE)
      }
      f <- c(f, fig3_write_params(rv$P, file.path(tmp, "fig3_params.csv")))
      zip::zip(file, files = f, mode = "cherry-pick")
      rv$msg <- paste("ZIP erstellt:", paste(basename(f), collapse = ", "))
    },
    contentType = "application/zip"
  )
  observeEvent(input$reset, {
    rv$P <- fig3_defaults()
    style_rv(rv$P$settings$style)
    rv$reset_n <- rv$reset_n + 1
    rv$msg <- "Auf v4-Standard zurückgesetzt (noch nicht gespeichert)"
  })
  output$status <- renderText(rv$msg)

  # ---- Plots -------------------------------------------------------------------
  dims <- reactive({
    h <- fig3_heights(rv$P); frac <- h$rel[1] / sum(h$rel)
    list(w = px(rv$P$settings$fig_width), hdr = px(h$total * frac),
         main = px(h$total * (1 - frac)))
  })
  output$header <- renderPlot(fig3_header(rv$P),
                              width = function() dims()$w, height = function() dims()$hdr,
                              res = RES)
  output$plot <- renderPlot(fig3_main(rv$P, cur()),
                            width = function() dims()$w, height = function() dims()$main,
                            res = RES)
}

shinyApp(ui, server)
