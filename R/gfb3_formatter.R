#' Launch the gfb3 Formatter Shiny App
#'
#' Opens an interactive Shiny application for reformatting forest inventory
#' data into the Global Forest Biodiversity Initiative (gfb3) format.
#' Supports column mapping, plot-level constants, status code translation,
#' and merging of multiple censuses with automatic derivation of PrevDBH,
#' PrevYr, and Status.
#'
#' @param launch.browser Logical; whether to open the app in a browser (default TRUE).
#'
#' @return Called for its side effect (launches the app). Returns invisibly.
#'
#' @examples
#' \dontrun{
#'   gfb3_formatter()
#' }
gfb3_formatter <- function(launch.browser = TRUE) {

  # ── Column definitions ────────────────────────────────────────────────────────
  GFB3_COLS   <- c("PlotID", "Lat", "Lon", "PA", "Dmin",
                   "TreeID", "Species", "Status", "DBH", "Yr",
                   "PrevDBH", "PrevYr", "note")
  PLOT_LEVEL  <- c("Lat", "Lon", "PA", "Dmin")
  STEM_LEVEL  <- c("PlotID", "TreeID", "Species", "Status", "DBH", "Yr",
                   "PrevDBH", "PrevYr", "note")
  REQUIRED    <- c("PlotID", "TreeID", "Species", "DBH", "Yr")
  DERIVED     <- c("PrevDBH", "PrevYr", "Status")

  # ── Helpers ───────────────────────────────────────────────────────────────────
  guess_col <- function(gfb_col, input_cols) {
    patterns <- list(
      PlotID  = c("plot", "plotid", "plot_id"),
      TreeID  = c("tree", "treeid", "tree_id", "stemid", "stem_id", "id"),
      Species = c("sp", "species", "taxon", "spcode", "sp_code"),
      DBH     = c("dbh", "diameter", "diam"),
      Yr      = c("yr", "year", "date", "census", "date2"),
      Status  = c("status", "state", "condition"),
      Lat     = c("lat", "latitude", "y"),
      Lon     = c("lon", "long", "longitude", "x"),
      PA      = c("pa", "plot_area", "area"),
      Dmin    = c("dmin", "dbh_min", "threshold"),
      note    = c("note", "notes", "comment", "remarks")
    )
    pats <- patterns[[gfb_col]]
    if (is.null(pats)) return("— none —")
    match <- input_cols[tolower(input_cols) %in% pats]
    if (length(match) > 0) match[1] else "— none —"
  }

  looks_like_fractional_year <- function(x) {
    x <- suppressWarnings(as.numeric(x))
    x <- x[!is.na(x)]
    if (length(x) == 0) return(FALSE)
    all(x > 1900 & x < 2200) && any(x != round(x))
  }

  fractional_year_to_int <- function(x) {
    as.integer(floor(suppressWarnings(as.numeric(x))))
  }

  read_file <- function(path, name, sheet = 1) {
    ext <- tools::file_ext(name)
    switch(ext,
           csv     = readr::read_csv(path, show_col_types = FALSE),
           xlsx    = readxl::read_xlsx(path, sheet = sheet),
           parquet = arrow::read_parquet(path),
           stop("Unsupported file type: ", ext)
    )
  }

  library(shiny)

  # ── UI ────────────────────────────────────────────────────────────────────────
  ui <- fluidPage(
    theme = bslib::bs_theme(
      version = 5,
      bg = "#0f1117", fg = "#e8eaf0",
      primary = "#4ade80", secondary = "#334155",
      base_font    = bslib::font_google("IBM Plex Mono"),
      heading_font = bslib::font_google("Space Mono"),
      font_scale   = 0.9
    ),
    tags$head(tags$style(HTML("
      body { background: #0f1117; }
      .card { background: #1a1f2e; border: 1px solid #2d3748; border-radius: 8px; }
      .card-header { background: #141824; border-bottom: 1px solid #2d3748;
                     color: #4ade80; font-family: 'Space Mono', monospace;
                     font-size: 0.78rem; letter-spacing: 0.08em; text-transform: uppercase; }
      .nav-tabs .nav-link { color: #94a3b8; font-size: 0.8rem; }
      .nav-tabs .nav-link.active { color: #4ade80; background: #1a1f2e;
                                   border-color: #2d3748 #2d3748 #1a1f2e; }
      .btn-primary { background: #4ade80; border-color: #4ade80; color: #0f1117;
                     font-family: 'Space Mono', monospace; font-size: 0.75rem; }
      .btn-primary:hover { background: #22c55e; border-color: #22c55e; color: #0f1117; }
      .btn-outline-secondary { border-color: #4ade80; color: #4ade80; font-size: 0.75rem; }
      .btn-outline-secondary:hover { background: #4ade80; color: #0f1117; }
      .form-select, .form-control { background: #0f1117; border-color: #2d3748;
                                    color: #e8eaf0; font-size: 0.82rem; }
      .form-select:focus, .form-control:focus { background: #0f1117; color: #e8eaf0;
                                                border-color: #4ade80; box-shadow: 0 0 0 2px #4ade8033; }
      .form-label { color: #94a3b8; font-size: 0.75rem; letter-spacing: 0.05em; text-transform: uppercase; }
      .badge-gfb     { background: #4ade8022; color: #4ade80; border: 1px solid #4ade8044;
                       font-size: 0.68rem; padding: 2px 7px; border-radius: 3px; }
      .badge-req     { background: #f8717122; color: #f87171; border: 1px solid #f8717144;
                       font-size: 0.68rem; padding: 2px 7px; border-radius: 3px; }
      .badge-derived { background: #facc1522; color: #facc15; border: 1px solid #facc1544;
                       font-size: 0.68rem; padding: 2px 7px; border-radius: 3px; }
      .mapping-row   { border-bottom: 1px solid #1e2535; padding: 8px 0; align-items: center; }
      .gfb-col-name  { font-family: 'Space Mono', monospace; font-size: 0.8rem; color: #e8eaf0; }
      .section-title { font-family: 'Space Mono', monospace; font-size: 0.7rem;
                       letter-spacing: 0.12em; text-transform: uppercase; color: #4ade80;
                       border-bottom: 1px solid #2d3748; padding-bottom: 6px; margin-bottom: 14px; }
      .inventory-tag { background: #1e2535; border: 1px solid #2d3748; border-radius: 4px;
                       padding: 4px 10px; font-size: 0.75rem; color: #94a3b8;
                       display: inline-block; margin: 3px; }
      .summary-stat  { text-align: center; padding: 12px; }
      .summary-stat .val { font-size: 1.6rem; color: #4ade80; font-family: 'Space Mono'; }
      .summary-stat .lbl { font-size: 0.68rem; color: #64748b; text-transform: uppercase; letter-spacing: 0.08em; }
      hr { border-color: #2d3748; }
      .dataTables_wrapper { color: #94a3b8; font-size: 0.78rem; }
      table.dataTable { background: #0f1117 !important; }
      table.dataTable thead th { border-bottom: 1px solid #2d3748 !important; color: #4ade80; }
      table.dataTable tbody tr { background: #0f1117 !important; }
      table.dataTable tbody tr:hover td { background: #1a1f2e !important; }
      .alert-info { background: #1a1f2e; border-color: #4ade8044; color: #94a3b8; font-size: 0.8rem; }
      #app-title    { font-family: 'Space Mono', monospace; font-size: 1.1rem;
                      color: #4ade80; letter-spacing: 0.05em; }
      #app-subtitle { font-size: 0.72rem; color: #475569; letter-spacing: 0.1em; text-transform: uppercase; }
    "))),

    div(style = "padding: 18px 24px 8px;",
        div(id = "app-title",    "GFB3 Formatter"),
        div(id = "app-subtitle", "Forest inventory → Global Forest Biodiversity Initiative format")
    ),

    bslib::navset_tab(
      id = "main_tabs",

      # ── TAB 0: Plot Table ──────────────────────────────────────────────────────
      bslib::nav_panel("① Plot Table",
                       fluidRow(style = "margin-top: 14px;",
                                column(4,
                                       bslib::card(
                                         bslib::card_header("Plot Table (optional)"),
                                         bslib::card_body(
                                           fileInput("plot_file", NULL,
                                                     accept = c(".csv", ".xlsx", ".parquet"),
                                                     placeholder = "CSV / XLSX / Parquet"),
                                           uiOutput("plot_sheet_selector_ui"),
                                           hr(),
                                           div(class = "section-title", "Plot ID Column"),
                                           uiOutput("plot_join_key_ui"),
                                           hr(),
                                           div(class = "section-title", "Map Plot Columns"),
                                           uiOutput("plot_mapping_ui"),
                                           hr(),
                                           actionButton("save_plot_table_btn", "Save Plot Table", class = "btn-primary"),
                                           uiOutput("plot_table_status_ui")
                                         )
                                       )
                                ),
                                column(8,
                                       bslib::card(
                                         bslib::card_header("Plot Table Preview"),
                                         bslib::card_body(
                                           DT::DTOutput("plot_preview_tbl")
                                         )
                                       )
                                )
                       )
      ),

      # ── TAB 1: Upload & Map ────────────────────────────────────────────────────
      bslib::nav_panel("② Upload & Map",
                       fluidRow(style = "margin-top: 14px;",
                                column(4,
                                       bslib::card(
                                         bslib::card_header("Input File"),
                                         bslib::card_body(
                                           fileInput("file1", NULL,
                                                     accept = c(".csv", ".xlsx", ".parquet"),
                                                     placeholder = "CSV / XLSX / Parquet"),
                                           uiOutput("sheet_selector_ui"),
                                           uiOutput("inventory_name_ui"),
                                           hr(),
                                           div(class = "section-title", "PlotID Construction"),
                                           uiOutput("plotid_ui"),
                                           hr(),
                                           # Only shown when no plot table is saved
                                           uiOutput("plot_constants_panel_ui")
                                         )
                                       )
                                ),
                                column(8,
                                       bslib::card(
                                         bslib::card_header("Column Mapping  —  Input → GFB3"),
                                         bslib::card_body(
                                           uiOutput("mapping_ui"),
                                           hr(),
                                           uiOutput("status_mapping_ui"),
                                           hr(),
                                           actionButton("preview_btn", "Preview Output", class = "btn-primary"),
                                           downloadButton("dl_single", "Download", class = "btn-outline-secondary ms-2"),
                                           div(style = "margin-top: 6px;",
                                               checkboxGroupInput("dl_fmt_single", NULL,
                                                                  choices  = c("CSV" = "csv", "XLSX" = "xlsx", "Parquet" = "parquet"),
                                                                  selected = "csv", inline = TRUE)
                                           )
                                         )
                                       ),
                                       uiOutput("preview_panel")
                                )
                       )
      ),

      # ── TAB 2: Merge censuses ──────────────────────────────────────────────────
      bslib::nav_panel("③ Merge Censuses",
                       fluidRow(style = "margin-top: 14px;",
                                column(4,
                                       bslib::card(
                                         bslib::card_header("Formatted Inventories"),
                                         bslib::card_body(
                                           fileInput("merge_files", NULL, multiple = TRUE,
                                                     accept = c(".csv", ".xlsx", ".parquet"),
                                                     placeholder = "Upload 2+ formatted GFB3 files"),
                                           uiOutput("merge_file_list"),
                                           hr(),
                                           div(class = "section-title", "Match Key"),
                                           radioButtons("match_key", NULL,
                                                        choices  = c("TreeID only" = "TreeID",
                                                                     "PlotID + TreeID" = "PlotID_TreeID"),
                                                        selected = "TreeID"),
                                           hr(),
                                           actionButton("merge_btn", "Merge & Derive", class = "btn-primary"),
                                           br(), br(),
                                           downloadButton("dl_merged", "Download Merged", class = "btn-outline-secondary"),
                                           div(style = "margin-top: 6px;",
                                               checkboxGroupInput("dl_fmt_merged", NULL,
                                                                  choices  = c("CSV" = "csv", "XLSX" = "xlsx", "Parquet" = "parquet"),
                                                                  selected = "csv", inline = TRUE)
                                           )
                                         )
                                       )
                                ),
                                column(8,
                                       bslib::card(
                                         bslib::card_header("Merge Preview"),
                                         bslib::card_body(
                                           uiOutput("merge_summary"),
                                           DT::DTOutput("merge_preview_tbl")
                                         )
                                       )
                                )
                       )
      )
    )
  )

  # ── SERVER ────────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    # ── Plot table ───────────────────────────────────────────────────────────────
    raw_plot_data <- reactive({
      req(input$plot_file)
      sheet <- if (!is.null(input$plot_sheet_select)) input$plot_sheet_select else 1
      read_file(input$plot_file$datapath, input$plot_file$name, sheet)
    })

    output$plot_sheet_selector_ui <- renderUI({
      req(input$plot_file)
      if (tools::file_ext(input$plot_file$name) != "xlsx") return(NULL)
      sheets <- readxl::excel_sheets(input$plot_file$datapath)
      if (length(sheets) <= 1) return(NULL)
      selectInput("plot_sheet_select", "Sheet", choices = sheets, selected = sheets[1])
    })

    output$plot_join_key_ui <- renderUI({
      req(raw_plot_data())
      selectInput("plot_join_key", "PlotID column in plot table",
                  choices = names(raw_plot_data()), width = "100%")
    })

    output$plot_mapping_ui <- renderUI({
      req(raw_plot_data())
      cols <- c("— none —", names(raw_plot_data()))
      tagList(
        lapply(PLOT_LEVEL, function(col) {
          fluidRow(class = "mapping-row",
                   column(4, div(class = "gfb-col-name", col)),
                   column(8,
                          selectInput(paste0("plot_map_", col), NULL,
                                      choices  = cols,
                                      selected = guess_col(col, names(raw_plot_data())),
                                      width    = "100%")
                   )
          )
        })
      )
    })

    saved_plot_table <- eventReactive(input$save_plot_table_btn, {
      req(raw_plot_data(), input$plot_join_key)
      df  <- raw_plot_data()
      out <- dplyr::tibble(PlotID = as.character(df[[input$plot_join_key]]))
      for (col in PLOT_LEVEL) {
        mapped <- input[[paste0("plot_map_", col)]]
        out[[col]] <- if (!is.null(mapped) && mapped != "— none —") df[[mapped]] else NA
      }
      out
    })

    output$plot_table_status_ui <- renderUI({
      req(saved_plot_table())
      div(style = "margin-top: 10px;",
          div(class = "badge-gfb",
              paste0("\u2713 ", nrow(saved_plot_table()), " plots saved")))
    })

    output$plot_preview_tbl <- DT::renderDT({
      req(saved_plot_table())
      DT::datatable(saved_plot_table(),
                    options = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
                    rownames = FALSE)
    })

    # Helper: returns saved plot table or NULL without error
    get_saved_plot_table <- function() {
      tryCatch(saved_plot_table(), error = function(e) NULL)
    }

    # ── Tree data file ───────────────────────────────────────────────────────────
    output$sheet_selector_ui <- renderUI({
      req(input$file1)
      if (tools::file_ext(input$file1$name) != "xlsx") return(NULL)
      sheets <- readxl::excel_sheets(input$file1$datapath)
      if (length(sheets) <= 1) return(NULL)
      selectInput("sheet_select", "Sheet", choices = sheets, selected = sheets[1])
    })

    raw_data <- reactive({
      req(input$file1)
      sheet <- if (!is.null(input$sheet_select)) input$sheet_select else 1
      read_file(input$file1$datapath, input$file1$name, sheet)
    })

    input_cols <- reactive({
      req(raw_data())
      c("— none —", names(raw_data()))
    })

    output$inventory_name_ui <- renderUI({
      req(input$file1)
      textInput("inv_name", "Inventory label",
                value = tools::file_path_sans_ext(input$file1$name))
    })

    # ── Plot-level constants panel (hidden when plot table is saved) ──────────────
    output$plot_constants_panel_ui <- renderUI({
      pt <- get_saved_plot_table()
      if (!is.null(pt) && nrow(pt) > 0) {
        div(
          div(class = "section-title", "Plot-level Constants"),
          div(style = "font-size:0.75rem; color:#4ade80;",
              paste0("\u2713 Using saved plot table (", nrow(pt), " plots)"))
        )
      } else {
        req(input_cols())
        cols <- input_cols()
        tagList(
          div(class = "section-title", "Plot-level Constants"),
          p(style = "font-size:0.75rem; color:#64748b;",
            "Enter a value to apply to ALL rows, or map a column above."),
          lapply(PLOT_LEVEL, function(col) {
            fluidRow(class = "mapping-row",
                     column(4, div(class = "gfb-col-name", col)),
                     column(4,
                            selectInput(paste0("map_", col), NULL, choices = cols,
                                        selected = "— none —", width = "100%")
                     ),
                     column(4,
                            textInput(paste0("const_", col), NULL,
                                      placeholder = "constant value", width = "100%")
                     )
            )
          })
        )
      }
    })

    # ── PlotID construction ──────────────────────────────────────────────────────
    output$plotid_ui <- renderUI({
      req(input_cols())
      cols <- setdiff(input_cols(), "— none —")
      tagList(
        selectizeInput("plotid_cols", "Columns to combine",
                       choices = cols, multiple = TRUE,
                       options = list(placeholder = "select one or more")),
        textInput("plotid_sep", "Separator", value = "_"),
        p(style = "font-size:0.72rem; color:#64748b;",
          "If only one column selected, it is used as-is.")
      )
    })

    # ── Stem-level column mapping ────────────────────────────────────────────────
    mappable_cols <- setdiff(STEM_LEVEL, c("PlotID", DERIVED))

    output$mapping_ui <- renderUI({
      req(input_cols())
      cols <- input_cols()
      tagList(
        div(class = "section-title", "Stem-level Columns"),
        fluidRow(
          column(4, div(style = "font-size:0.68rem; color:#475569;", "GFB3 Column")),
          column(4, div(style = "font-size:0.68rem; color:#475569;", "Input Column")),
          column(4, div(style = "font-size:0.68rem; color:#475569;", "Constant / Override"))
        ),
        lapply(mappable_cols, function(col) {
          is_req <- col %in% REQUIRED
          badge  <- if (is_req) tags$span(class = "badge-req ms-1",  "required")
          else        tags$span(class = "badge-gfb ms-1", "optional")
          fluidRow(class = "mapping-row",
                   column(4, div(class = "gfb-col-name", col, badge)),
                   column(4,
                          selectInput(paste0("map_", col), NULL, choices = cols,
                                      selected = guess_col(col, names(raw_data())),
                                      width = "100%")
                   ),
                   column(4,
                          if (!col %in% c("DBH", "Yr"))
                            textInput(paste0("const_", col), NULL,
                                      placeholder = "constant value", width = "100%")
                   )
          )
        }),
        uiOutput("yr_conversion_ui"),
        div(style = "margin-top: 10px;",
            span(class = "badge-derived", "derived"), " ",
            span(style = "font-size:0.75rem; color:#64748b;",
                 "PrevDBH, PrevYr, and Status are auto-derived if multiple years are present in the data. Tab \u2462 is only needed when merging separate census files.")
        )
      )
    })

    output$yr_conversion_ui <- renderUI({
      req(input$map_Yr, raw_data())
      col <- input$map_Yr
      if (col == "— none —") return(NULL)
      vals <- raw_data()[[col]]
      if (!looks_like_fractional_year(vals)) return(NULL)
      div(style = "background:#facc1511; border:1px solid #facc1544;
                   border-radius:4px; padding:8px; margin-top:4px;",
          div(style = "font-size:0.72rem; color:#facc15;",
              "\u26a0 Fractional year format detected (e.g. 2017.1685)"),
          checkboxInput("convert_yr", "Extract integer year (floor)", value = TRUE)
      )
    })

    # ── Status mapping ───────────────────────────────────────────────────────────
    output$status_mapping_ui <- renderUI({
      req(input_cols(), raw_data())
      cols <- input_cols()
      tagList(
        div(class = "section-title", "Status Column"),
        fluidRow(
          column(6,
                 selectInput("map_Status", "Input status column",
                             choices  = cols,
                             selected = guess_col("Status", names(raw_data())),
                             width    = "100%")
          )
        ),
        uiOutput("status_code_ui")
      )
    })

    output$status_code_ui <- renderUI({
      req(input$map_Status)
      if (input$map_Status == "— none —") {
        div(style = "font-size:0.75rem; color:#64748b;",
            "No status column — will be derived at merge step.")
      } else {
        vals <- sort(unique(as.character(raw_data()[[input$map_Status]])))
        tagList(
          div(style = "font-size:0.75rem; color:#94a3b8; margin-bottom:6px;",
              "Map your codes \u2192 GFB3 (0 = alive, 1 = dead, 2 = new)"),
          fluidRow(
            column(4, selectInput("status_alive", "Alive (0)", choices = vals, width = "100%")),
            column(4, selectInput("status_dead",  "Dead (1)",  choices = vals, width = "100%")),
            column(4, selectInput("status_new",   "New (2)",   choices = vals, width = "100%"))
          )
        )
      }
    })

    # ── Build output ─────────────────────────────────────────────────────────────
    build_output <- reactive({
      req(raw_data(), input$plotid_cols)
      df <- raw_data()

      # PlotID
      if (length(input$plotid_cols) == 0) {
        stop("Please select at least one column for PlotID.")
      } else if (length(input$plotid_cols) == 1) {
        plotid_vec <- as.character(df[[input$plotid_cols]])
      } else {
        sep <- if (nchar(input$plotid_sep) == 0) "_" else input$plotid_sep
        plotid_vec <- do.call(paste, c(lapply(input$plotid_cols, function(c) df[[c]]), sep = sep))
      }

      out <- dplyr::tibble(PlotID = plotid_vec)

      # Plot-level: join from saved plot table if available, else use mapped/constant
      pt <- get_saved_plot_table()
      if (!is.null(pt) && nrow(pt) > 0) {
        out <- dplyr::left_join(out, pt, by = "PlotID")
      } else {
        for (col in PLOT_LEVEL) {
          mapped   <- input[[paste0("map_", col)]]
          constant <- input[[paste0("const_", col)]]
          if (!is.null(mapped) && mapped != "— none —") {
            out[[col]] <- df[[mapped]]
          } else if (!is.null(constant) && nchar(constant) > 0) {
            out[[col]] <- constant
          } else {
            out[[col]] <- NA
          }
        }
      }

      # Stem-level
      for (col in mappable_cols) {
        mapped   <- input[[paste0("map_", col)]]
        constant <- input[[paste0("const_", col)]]
        if (!is.null(mapped) && mapped != "— none —") {
          if (col == "Yr" && isTRUE(input$convert_yr) && looks_like_fractional_year(df[[mapped]])) {
            out[[col]] <- fractional_year_to_int(df[[mapped]])
          } else {
            out[[col]] <- df[[mapped]]
          }
        } else if (!is.null(constant) && nchar(constant) > 0) {
          out[[col]] <- constant
        } else {
          out[[col]] <- NA
        }
      }

      # Status
      if (!is.null(input$map_Status) && input$map_Status != "— none —") {
        raw_status <- as.character(df[[input$map_Status]])
        out$Status <- dplyr::case_when(
          raw_status == input$status_alive ~ 0L,
          raw_status == input$status_dead  ~ 1L,
          raw_status == input$status_new   ~ 2L,
          TRUE ~ NA_integer_
        )
      } else {
        out$Status <- NA_integer_
      }

      # Derive PrevDBH, PrevYr, Status if multiple years present
      if (dplyr::n_distinct(out$Yr, na.rm = TRUE) > 1) {
        out <- out |>
          dplyr::arrange(PlotID, TreeID, Yr) |>
          dplyr::group_by(PlotID, TreeID) |>
          dplyr::mutate(
            PrevDBH = dplyr::lag(DBH),
            PrevYr  = dplyr::lag(Yr),
            Status  = dplyr::case_when(
              !is.na(Status)                      ~ Status,        # keep if already mapped
              is.na(dplyr::lag(DBH))              ~ 2L,            # no previous record = new
              TRUE                                ~ 0L             # has previous = alive
            )
          ) |>
          dplyr::ungroup()
      } else {
        out$PrevDBH <- NA_real_
        out$PrevYr  <- NA_real_
      }


      # note
      note_map <- input$map_note
      out$note <- if (!is.null(note_map) && note_map != "— none —") df[[note_map]] else NA_character_

      out[, GFB3_COLS]
    })

    # ── Preview ──────────────────────────────────────────────────────────────────
    formatted_data <- eventReactive(input$preview_btn, { build_output() })

    output$preview_panel <- renderUI({
      req(formatted_data())
      bslib::card(
        bslib::card_header("Output Preview"),
        bslib::card_body(
          fluidRow(
            column(3, div(class = "summary-stat",
                          div(class = "val", nrow(formatted_data())),
                          div(class = "lbl", "Stems"))),
            column(3, div(class = "summary-stat",
                          div(class = "val", dplyr::n_distinct(formatted_data()$PlotID, na.rm = TRUE)),
                          div(class = "lbl", "Plots"))),
            column(3, div(class = "summary-stat",
                          div(class = "val", dplyr::n_distinct(formatted_data()$Species, na.rm = TRUE)),
                          div(class = "lbl", "Species"))),
            column(3, div(class = "summary-stat",
                          div(class = "val", sum(!is.na(formatted_data()$DBH))),
                          div(class = "lbl", "DBH records")))
          ),
          DT::DTOutput("preview_tbl")
        )
      )
    })

    output$preview_tbl <- DT::renderDT({
      req(formatted_data())
      DT::datatable(head(formatted_data(), 100),
                    options = list(scrollX = TRUE, pageLength = 10, dom = "tip",
                                   columnDefs = list(list(className = "dt-center", targets = "_all"))),
                    rownames = FALSE)
    })

    # ── Download single ──────────────────────────────────────────────────────────
    output$dl_single <- downloadHandler(
      filename = function() {
        nm   <- if (!is.null(input$inv_name) && nchar(input$inv_name) > 0) input$inv_name else "gfb3_formatted"
        fmts <- input$dl_fmt_single
        if ("csv"  %in% fmts) paste0(nm, ".csv")
        else if ("xlsx" %in% fmts) paste0(nm, ".xlsx")
        else paste0(nm, ".parquet")
      },
      content = function(file) {
        df   <- build_output()
        fmts <- input$dl_fmt_single
        if ("csv"  %in% fmts) readr::write_csv(df, file)
        else if ("xlsx" %in% fmts) writexl::write_xlsx(df, file)
        else arrow::write_parquet(df, file)
      }
    )

    # ── Merge ────────────────────────────────────────────────────────────────────
    merged_files <- reactive({
      req(input$merge_files)
      purrr::map(seq_len(nrow(input$merge_files)), function(i) {
        read_file(input$merge_files$datapath[i], input$merge_files$name[i])
      })
    })

    output$merge_file_list <- renderUI({
      req(input$merge_files)
      div(lapply(input$merge_files$name, function(n) div(class = "inventory-tag", n)))
    })

    merged_data <- eventReactive(input$merge_btn, {
      dfs <- merged_files()
      req(length(dfs) >= 2)
      key <- input$match_key

      dfs <- dfs[order(sapply(dfs, function(d) median(as.numeric(d$Yr), na.rm = TRUE)))]

      result <- list()
      for (i in seq_along(dfs)) {
        curr <- dfs[[i]]
        if (i == 1) {
          curr$PrevDBH <- NA_real_
          curr$PrevYr  <- NA_real_
          if (all(is.na(curr$Status))) curr$Status <- 0L
          result[[i]] <- curr
        } else {
          prev         <- dfs[[i - 1]]
          join_by_cols <- if (key == "PlotID_TreeID") c("PlotID", "TreeID") else "TreeID"
          prev_lookup  <- prev |>
            dplyr::select(dplyr::all_of(join_by_cols), PrevDBH = DBH, PrevYr = Yr)

          curr <- curr |>
            dplyr::left_join(prev_lookup, by = join_by_cols) |>
            dplyr::mutate(
              PrevDBH = dplyr::coalesce(PrevDBH.y, PrevDBH.x),
              PrevYr  = dplyr::coalesce(PrevYr.y,  PrevYr.x)
            ) |>
            dplyr::select(-dplyr::any_of(c("PrevDBH.x", "PrevDBH.y", "PrevYr.x", "PrevYr.y")))

          if (all(is.na(curr$Status))) {
            prev_ids <- if (key == "PlotID_TreeID") paste(prev$PlotID, prev$TreeID, sep = "_") else prev$TreeID
            curr_ids <- if (key == "PlotID_TreeID") paste(curr$PlotID, curr$TreeID, sep = "_") else curr$TreeID
            curr$Status <- dplyr::case_when(curr_ids %in% prev_ids ~ 0L, TRUE ~ 2L)

            dead_ids <- setdiff(prev_ids, curr_ids)
            if (length(dead_ids) > 0) {
              dead_rows <- prev |>
                dplyr::filter((if (key == "PlotID_TreeID") paste(PlotID, TreeID, sep = "_") else TreeID) %in% dead_ids) |>
                dplyr::mutate(Status = 1L, PrevDBH = DBH, PrevYr = Yr,
                              DBH = NA_real_, Yr = unique(curr$Yr)[1])
              curr <- dplyr::bind_rows(curr, dead_rows)
            }
          }
          result[[i]] <- curr
        }
      }

      dplyr::bind_rows(result) |>
        dplyr::arrange(PlotID, TreeID, Yr) |>
        dplyr::select(dplyr::all_of(GFB3_COLS))
    })

    output$merge_summary <- renderUI({
      req(merged_data())
      df <- merged_data()
      fluidRow(
        column(3, div(class = "summary-stat", div(class = "val", nrow(df)),                                    div(class = "lbl", "Total rows"))),
        column(3, div(class = "summary-stat", div(class = "val", dplyr::n_distinct(df$PlotID, na.rm = TRUE)), div(class = "lbl", "Plots"))),
        column(3, div(class = "summary-stat", div(class = "val", sum(df$Status == 1L, na.rm = TRUE)),          div(class = "lbl", "Dead"))),
        column(3, div(class = "summary-stat", div(class = "val", sum(df$Status == 2L, na.rm = TRUE)),          div(class = "lbl", "New")))
      )
    })

    output$merge_preview_tbl <- DT::renderDT({
      req(merged_data())
      DT::datatable(head(merged_data(), 200),
                    options = list(scrollX = TRUE, pageLength = 15, dom = "tip"),
                    rownames = FALSE)
    })

    output$dl_merged <- downloadHandler(
      filename = function() {
        fmts <- input$dl_fmt_merged
        if ("csv"  %in% fmts) "gfb3_merged.csv"
        else if ("xlsx" %in% fmts) "gfb3_merged.xlsx"
        else "gfb3_merged.parquet"
      },
      content = function(file) {
        df   <- merged_data()
        fmts <- input$dl_fmt_merged
        if ("csv"  %in% fmts) readr::write_csv(df, file)
        else if ("xlsx" %in% fmts) writexl::write_xlsx(df, file)
        else arrow::write_parquet(df, file)
      }
    )
  }

  formatted_data <- eventReactive(input$preview_btn, {
    tryCatch(
      build_output(),
      error = function(e) {
        showNotification(paste("Error:", e$message), type = "error", duration = 8)
        NULL
      }
    )
  })

  old_max <- getOption("shiny.maxRequestSize")
  options(shiny.maxRequestSize = 500 * 1024^2)
  on.exit(options(shiny.maxRequestSize = old_max), add = TRUE)

  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app, launch.browser = launch.browser)
}
