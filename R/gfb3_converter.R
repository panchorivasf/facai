#' GFB3 Data Converter
#'
#' @description
#' Launches an interactive Shiny application for converting forest inventory
#' datasets from any tabular format into the GFB3 standard used by the Global
#' Forest Biodiversity Initiative (GFBI). The app guides the user through three
#' steps:
#'
#' \enumerate{
#'   \item \strong{Upload} — load a dataset (CSV, Excel, TSV, RDS, RData, or Parquet)
#'         and optionally restore a previously saved column-mapping config.
#'   \item \strong{Map Columns} — specify the Tree ID, census year, and DBH
#'         columns for automatic consecutive-census pairing, then map the
#'         remaining GFB3 columns to source columns or constant values.
#'   \item \strong{Review & Download} — inspect a validation report and
#'         a colour-coded output preview, then download the result as CSV,
#'         Excel (.xlsx), or Parquet.
#' }
#'
#' @details
#' \strong{Supported input formats:} CSV, TSV/TXT, Excel (.xlsx/.xls),
#' R data files (.rds, .RData, .rda), and Parquet.
#'
#' \strong{Census pairing:} Given a long-format dataset with multiple censuses
#' stacked in rows, the app automatically pairs consecutive years per tree and
#' derives the \code{Status}, \code{PrevDBH}, and \code{PrevYR} columns
#' required by GFB3:
#' \itemize{
#'   \item \code{0} — tree alive in both consecutive censuses
#'   \item \code{1} — tree present in the previous census but absent
#'         (or flagged dead) in the current one
#'   \item \code{2} — new recruit: present in the current census only
#' }
#'
#' \strong{DBH units:} If the source data stores DBH in millimetres, select
#' the "mm" option and the app will divide all DBH values by 10 before output.
#'
#' \strong{Mapping configs:} Column mappings (including pairing column
#' selections and DBH unit) can be saved as a \code{.json} file and reloaded
#' in a future session to avoid re-mapping datasets in the same format.
#'
#' @param launch.browser Logical. If \code{TRUE} (default), the app opens in
#'   the system's default web browser. Set to \code{FALSE} to open in the
#'   RStudio Viewer pane instead.
#'
#' @return Called for its side effect (launching the Shiny app). Returns
#'   \code{NULL} invisibly.
#'
#' @examples
#' \dontrun{
#' # Launch the converter in the default browser
#' gfb3_converter()
#'
#' # Open in the RStudio Viewer pane instead
#' gfb3_converter(launch.browser = FALSE)
#' }
#'
#' @importFrom shiny shinyApp runApp
#' @importFrom bslib bs_theme font_google page_navbar nav_panel card
#'   card_header layout_columns
#' @importFrom dplyr rename filter select mutate left_join bind_rows
#'   group_by if_else tibble
#' @importFrom readr read_csv read_tsv write_csv
#' @importFrom readxl read_excel
#' @importFrom writexl write_xlsx
#' @importFrom arrow read_parquet write_parquet
#' @importFrom jsonlite fromJSON write_json
#' @importFrom DT datatable renderDT DTOutput formatStyle formatRound
#'   styleEqual
gfb3_converter <- function(launch.browser = TRUE) {

  # options(browser = "google-chrome")

  # ── GFB3 schema ─────────────────────────────────────────────────────────────
  GFB3_COLS <- c("PlotID", "Latitude", "Longitude", "PA",
                 "TreeID", "Species", "Status", "DBH", "YR",
                 "PrevDBH", "PrevYR", "note")

  GFB3_REQUIRED <- c("PlotID", "TreeID", "Species", "Status", "DBH", "YR")

  GFB3_DESC <- list(
    PlotID    = "Unique ID code for each plot (same across all censuses of same plot)",
    Latitude  = "Latitude in decimal degrees, WGS84",
    Longitude = "Longitude in decimal degrees, WGS84",
    PA        = "Plot area in hectares",
    TreeID    = "Unique tree ID (same across all censuses for same individual)",
    Species   = "Binomial scientific name (no authority)",
    Status    = "0 = alive in both inventories | 1 = died | 2 = new recruit",
    DBH       = "DBH in cm of current inventory",
    YR        = "Year of current inventory",
    PrevDBH   = "DBH in cm of previous inventory (blank if Status = 2)",
    PrevYR    = "Year of previous inventory (blank if Status = 2)",
    note      = "Optional: disturbances, coordinate precision notes, etc."
  )

  # ── Internal helpers ─────────────────────────────────────────────────────────

  # Values treated as "dead" across common forest inventory formats
  DEAD_VALUES <- c("1", "D", "d", "dead", "Dead", "DEAD",
                   "M", "m", "missing", "Missing", "MISSING", 1L)

  .read_input <- function(path, ext) {
    tryCatch(
      switch(ext,
             "csv"     = readr::read_csv(path, show_col_types = FALSE),
             "xls"     = ,
             "xlsx"    = readxl::read_excel(path),
             "txt"     = ,
             "tsv"     = readr::read_tsv(path, show_col_types = FALSE),
             "rds"     = readRDS(path),
             "rdata"   = ,
             "rda"     = {
               env  <- new.env(parent = emptyenv())
               load(path, envir = env)
               objs <- ls(env)
               dfs  <- Filter(function(nm) is.data.frame(get(nm, envir = env)), objs)
               if (length(dfs) == 0)
                 stop("No data frame found in .RData file. Objects present: ",
                      paste(objs, collapse = ", "))
               if (length(dfs) > 1)
                 warning("Multiple data frames in .RData (",
                         paste(dfs, collapse = ", "),
                         "). Loading the first: ", dfs[[1]])
               get(dfs[[1]], envir = env)
             },
             "parquet" = arrow::read_parquet(path),
             stop("Unsupported file type: .", ext)
      ),
      error = function(e) NULL
    )
  }

  .pair_censuses <- function(df, tree_col, year_col, dbh_col,
                             status_col = NULL) {
    df <- dplyr::rename(df,
                        .tree = !!rlang::sym(tree_col),
                        .year = !!rlang::sym(year_col),
                        .dbh  = !!rlang::sym(dbh_col)
    )

    years <- sort(unique(df$.year))
    if (length(years) < 2)
      stop("Census year column has fewer than 2 distinct values — cannot pair.")

    pairs <- lapply(seq_len(length(years) - 1), function(i) {
      prev_yr  <- years[i]
      curr_yr  <- years[i + 1]

      prev_tbl <- dplyr::filter(df, .year == prev_yr) |>
        dplyr::select(.tree, .prev_dbh = .dbh, .prev_yr = .year)

      curr_tbl <- dplyr::filter(df, .year == curr_yr)

      joined <- dplyr::left_join(curr_tbl, prev_tbl, by = ".tree") |>
        dplyr::mutate(
          .status = dplyr::if_else(!is.na(.prev_dbh), 0L, 2L)
        )

      if (is.null(status_col) || status_col == "(none)") {
        # Trees absent from current census → dead
        dead_trees <- setdiff(unique(prev_tbl$.tree), unique(curr_tbl$.tree))
        if (length(dead_trees) > 0) {
          dead_rows <- dplyr::filter(df, .year == prev_yr,
                                     .tree %in% dead_trees) |>
            dplyr::mutate(
              .prev_dbh = .dbh,
              .prev_yr  = .year,
              .year     = curr_yr,
              .dbh      = NA_real_,
              .status   = 1L
            )
          joined <- dplyr::bind_rows(joined, dead_rows)
        }
      } else {
        # User-supplied flag column overrides status for dead trees
        joined <- dplyr::mutate(
          joined,
          .status = dplyr::if_else(
            as.character(.data[[status_col]]) %in% as.character(DEAD_VALUES),
            1L, .status
          )
        )
      }
      joined
    })

    dplyr::bind_rows(pairs) |>
      dplyr::rename(
        !!tree_col := .tree,
        !!dbh_col  := .dbh,
        !!year_col := .year,
        PrevDBH    = .prev_dbh,
        PrevYR     = .prev_yr,
        Status     = .status
      )
  }

  .apply_mapping <- function(df, mapping, const_vals, dbh_unit,
                             tree_col, year_col, dbh_col, status_src) {
    df <- .pair_censuses(df, tree_col, year_col, dbh_col, status_src)
    if (is.null(df)) return(NULL)

    out <- dplyr::tibble(.rows = nrow(df))
    for (col in GFB3_COLS) {
      src   <- mapping[[col]]
      const <- const_vals[[col]]
      if (!is.null(src) && src != "(none)" && src %in% names(df)) {
        out[[col]] <- df[[src]]
      } else if (col %in% names(df)) {
        out[[col]] <- df[[col]]
      } else if (!is.null(const) && nchar(trimws(const)) > 0) {
        out[[col]] <- const
      } else {
        out[[col]] <- NA
      }
    }

    if (dbh_unit == "mm") {
      out <- dplyr::mutate(out,
                           DBH     = suppressWarnings(as.numeric(DBH))     / 10,
                           PrevDBH = suppressWarnings(as.numeric(PrevDBH)) / 10
      )
    }

    out <- dplyr::mutate(out,
                         Latitude  = suppressWarnings(as.numeric(Latitude)),
                         Longitude = suppressWarnings(as.numeric(Longitude)),
                         PA        = suppressWarnings(as.numeric(PA)),
                         DBH       = suppressWarnings(as.numeric(DBH)),
                         PrevDBH   = suppressWarnings(as.numeric(PrevDBH)),
                         YR        = suppressWarnings(as.integer(YR)),
                         PrevYR    = suppressWarnings(as.integer(PrevYR)),
                         Status    = suppressWarnings(as.integer(Status)),
                         PrevDBH   = dplyr::if_else(Status == 2L, NA_real_,    PrevDBH),
                         PrevYR    = dplyr::if_else(Status == 2L, NA_integer_,  PrevYR)
    )
    out
  }

  .validate_output <- function(df) {
    msgs <- list()
    add  <- function(level, text)
      msgs[[length(msgs) + 1]] <<- list(level = level, text = text)

    missing_req <- setdiff(GFB3_REQUIRED, names(df))
    if (length(missing_req))
      add("error", paste0("Missing required columns: ",
                          paste(missing_req, collapse = ", ")))

    if (nrow(df) == 0) {
      add("error", "Output table has 0 rows")
      return(msgs)
    }

    if ("Status" %in% names(df)) {
      bad <- df$Status[!is.na(df$Status) & !df$Status %in% c(0L, 1L, 2L)]
      if (length(bad))
        add("warn", paste0("Unexpected Status values: ",
                           paste(unique(bad), collapse = ", "),
                           " (expected 0/1/2)"))
      tab <- table(df$Status, useNA = "ifany")
      add("info", paste0("Status breakdown — ",
                         paste(names(tab), tab, sep = ": ", collapse = " | ")))
    }

    if ("DBH" %in% names(df)) {
      dbh   <- df$DBH
      n_out <- sum(!is.na(dbh) & (dbh < 1 | dbh > 500))
      if (n_out > 0)
        add("warn", paste0(n_out, " DBH value(s) outside 1–500 cm. Check units."))
      n_na <- sum(is.na(dbh) & (is.na(df$Status) | df$Status != 1L))
      if (n_na > 0)
        add("warn", paste0(n_na, " non-dead row(s) with missing DBH."))
    }

    if (all(c("Status", "PrevDBH") %in% names(df))) {
      r_prev <- sum(df$Status == 2L & !is.na(df$PrevDBH), na.rm = TRUE)
      if (r_prev > 0)
        add("warn", paste0(r_prev,
                           " recruit rows (Status=2) have PrevDBH filled in."))
    }

    if (all(c("TreeID", "YR") %in% names(df))) {
      dups <- dplyr::group_by(df, TreeID, YR) |>
        dplyr::filter(dplyr::n() > 1) |>
        nrow()
      if (dups > 0)
        add("warn", paste0(dups,
                           " rows share the same TreeID \u00d7 YR",
                           " \u2014 possible duplicates."))
    }

    if ("Species" %in% names(df)) {
      sp    <- na.omit(unique(df$Species))
      non_b <- sp[!grepl("^[A-Za-z]+ [a-z]", sp)]
      if (length(non_b) > 0)
        add("warn", paste0(length(non_b),
                           " species name(s) may not be binomial (e.g. '",
                           non_b[[1]], "')."))
    }

    if ("Latitude"  %in% names(df) &&
        any(abs(df$Latitude)  > 90,  na.rm = TRUE))
      add("warn", "Some Latitude values outside \u00b190\u00b0.")
    if ("Longitude" %in% names(df) &&
        any(abs(df$Longitude) > 180, na.rm = TRUE))
      add("warn", "Some Longitude values outside \u00b1180\u00b0.")

    add("info", paste0("Output: ", format(nrow(df), big.mark = ","),
                       " rows \u00d7 ", ncol(df), " columns"))

    if (!any(sapply(msgs, `[[`, "level") %in% c("error", "warn")))
      add("ok", "No issues detected.")

    msgs
  }

  # ── UI ───────────────────────────────────────────────────────────────────────
  ui <- bslib::page_navbar(
    title = shiny::span(
      style = "font-weight:700; letter-spacing:.03em;",
      shiny::tags$span(style = "color:#A5D6A7;", "GFB3"),
      shiny::tags$span(style = "color:white; font-weight:300;",
                       " Data Converter")
    ),
    theme = bslib::bs_theme(
      bootswatch  = "flatly",
      primary     = "#2E7D32",
      base_font   = bslib::font_google("IBM Plex Sans"),
      code_font   = bslib::font_google("IBM Plex Mono"),
      "navbar-bg" = "#1B4332"
    ),
    navbar_options = bslib::navbar_options(bg = "#1B4332", fg = "white"),

    # ── Step 1: Upload ─────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = "\u2460 Upload", icon = shiny::icon("upload"),
      bslib::layout_columns(
        col_widths = c(4, 8),
        bslib::card(
          bslib::card_header(shiny::icon("file-arrow-up"), " Upload dataset"),
          shiny::fileInput(
            "file", NULL,
            accept      = c(".csv", ".tsv", ".txt", ".xlsx", ".xls",
                            ".rds", ".rdata", ".rda", ".parquet"),
            placeholder = "CSV \u00b7 Excel \u00b7 TSV \u00b7 RDS \u00b7 RData \u00b7 Parquet",
            buttonLabel = "Browse\u2026"
          ),
          shiny::hr(),
          shiny::tags$b("Or load a saved mapping config"),
          shiny::tags$p(class = "text-muted small mb-1",
                        "Restore a previous column mapping from a .json file."),
          shiny::fileInput("config_in", NULL, accept = ".json",
                           placeholder = "Upload .json config",
                           buttonLabel = "Browse\u2026"),
          shiny::hr(),
          shiny::uiOutput("file_summary")
        ),
        bslib::card(
          bslib::card_header(
            shiny::icon("table"), " Data preview ",
            shiny::tags$small(class = "text-muted", "(first 200 rows)")
          ),
          DT::DTOutput("preview_raw")
        )
      )
    ),

    # ── Step 2: Map Columns ────────────────────────────────────────────────────
    bslib::nav_panel(
      title = "\u2461 Map Columns", icon = shiny::icon("table-columns"),
      bslib::layout_columns(
        col_widths = c(4, 8),
        bslib::card(
          bslib::card_header(shiny::icon("code-branch"), " Census pairing"),
          shiny::tags$p(
            class = "text-muted small",
            "Select the three key columns used to pair consecutive censuses.",
            "Status, PrevDBH, and PrevYR will be derived automatically."
          ),
          shiny::uiOutput("pair_ui"),
          shiny::hr(),
          bslib::card_header(shiny::icon("ruler"), " DBH units in source"),
          shiny::radioButtons(
            "dbh_unit", NULL,
            choices  = c("Centimeters (cm)"              = "cm",
                         "Millimeters (mm) \u2014 will \u00f710" = "mm"),
            selected = "cm"
          )
        ),
        bslib::card(
          bslib::card_header(
            shiny::icon("arrows-left-right"), " Column mapping",
            shiny::tags$small(class = "text-muted ms-2",
                              "Map GFB3 columns to source columns or constants.")
          ),
          shiny::tags$div(
            class = "d-flex fw-semibold text-muted small px-1 mb-1",
            shiny::tags$div(style = "width:28%", "GFB3 column"),
            shiny::tags$div(style = "width:36%", "Source column"),
            shiny::tags$div(style = "width:36%", "Constant value")
          ),
          shiny::uiOutput("mapping_ui"),
          shiny::hr(),
          bslib::layout_columns(
            col_widths = c(5, 7),
            shiny::downloadButton("save_config", "\ud83d\udcbe Save mapping",
                                  class = "btn-outline-secondary btn-sm w-100"),
            shiny::actionButton("apply_btn", "\u25b6  Apply & convert",
                                class = "btn-success w-100",
                                icon  = shiny::icon("play"))
          )
        )
      )
    ),

    # ── Step 3: Review & Download ──────────────────────────────────────────────
    bslib::nav_panel(
      title = "\u2462 Review & Download", icon = shiny::icon("download"),
      bslib::layout_columns(
        col_widths = c(3, 9),
        bslib::card(
          bslib::card_header(shiny::icon("clipboard-check"), " Validation"),
          shiny::uiOutput("validation_ui"),
          shiny::hr(),
          shiny::tags$b("Download output"),
          shiny::tags$div(
            class = "d-grid gap-2 mt-2",
            shiny::downloadButton("dl_csv",     "\u2b07  CSV",
                                  class = "btn-success"),
            shiny::downloadButton("dl_xlsx",    "\u2b07  Excel",
                                  class = "btn-outline-success"),
            shiny::downloadButton("dl_parquet", "\u2b07  Parquet",
                                  class = "btn-outline-success")
          )
        ),
        bslib::card(
          bslib::card_header(
            shiny::icon("table-list"), " GFB3 output preview ",
            shiny::tags$small(class = "text-muted", "(first 500 rows)")
          ),
          DT::DTOutput("preview_out")
        )
      )
    ),

    # ── Help ────────────────────────────────────────────────────────────────────
    bslib::nav_panel(
      title = "Help", icon = shiny::icon("circle-question"),
      bslib::layout_columns(
        col_widths = c(7, 5),
        bslib::card(
          bslib::card_header("GFB3 column definitions"),
          shiny::tags$table(
            class = "table table-sm table-striped align-middle",
            shiny::tags$thead(
              class = "table-dark",
              shiny::tags$tr(
                shiny::tags$th("Column"),
                shiny::tags$th("Required"),
                shiny::tags$th("Description")
              )
            ),
            shiny::tags$tbody(lapply(GFB3_COLS, function(col) {
              shiny::tags$tr(
                shiny::tags$td(shiny::tags$code(col)),
                shiny::tags$td(
                  if (col %in% GFB3_REQUIRED)
                    shiny::tags$span(class = "badge bg-success", "required")
                  else
                    shiny::tags$span(class = "badge bg-secondary", "optional")
                ),
                shiny::tags$td(GFB3_DESC[[col]])
              )
            }))
          )
        ),
        bslib::card(
          bslib::card_header("Workflow guide"),
          shiny::tags$ol(
            shiny::tags$li(
              shiny::tags$b("Upload"),
              " your dataset (CSV, Excel, TSV, RDS, RData, or Parquet).",
              " Optionally load a saved mapping .json."
            ),
            shiny::tags$li(
              shiny::tags$b("Map Columns:"),
              " choose Tree ID, year, and DBH columns for pairing, then",
              " map remaining GFB3 columns or enter constants",
              " (e.g. PlotID = 'BCI', PA = '50')."
            ),
            shiny::tags$li(
              shiny::tags$b("Review & Download:"),
              " check the validation report, inspect the output,",
              " and download as CSV, Excel, or Parquet."
            )
          ),
          shiny::hr(),
          shiny::tags$b("Status coding"),
          shiny::tags$ul(
            shiny::tags$li(shiny::tags$code("0"),
                           " \u2014 alive in both consecutive censuses"),
            shiny::tags$li(shiny::tags$code("1"),
                           " \u2014 present in previous census,",
                           " absent/dead in current"),
            shiny::tags$li(shiny::tags$code("2"),
                           " \u2014 new recruit: present in current census only")
          ),
          shiny::hr(),
          shiny::tags$b("Mapping configs"),
          shiny::tags$p(
            "Save your mapping as JSON to reload it instantly next time",
            " you process data in the same format."
          )
        )
      )
    )
  )

  # ── Server ───────────────────────────────────────────────────────────────────
  server <- function(input, output, session) {

    rv <- shiny::reactiveValues(
      raw     = NULL,
      out     = NULL,
      mapping = list(),
      consts  = list()
    )

    # Load data file
    shiny::observeEvent(input$file, {
      shiny::req(input$file)
      ext <- tolower(tools::file_ext(input$file$name))
      df  <- .read_input(input$file$datapath, ext)
      if (is.null(df)) {
        shiny::showNotification("Failed to read file. Check format.",
                                type = "error")
        return()
      }
      rv$raw <- as.data.frame(df)
      shiny::showNotification(
        paste0("Loaded ", format(nrow(df), big.mark = ","),
               " rows \u00d7 ", ncol(df), " columns"),
        type = "message", duration = 4
      )
    })

    # Load JSON mapping config
    shiny::observeEvent(input$config_in, {
      shiny::req(input$config_in)
      cfg <- tryCatch(
        jsonlite::fromJSON(input$config_in$datapath, simplifyVector = FALSE),
        error = function(e) NULL
      )
      if (is.null(cfg) || !all(c("mapping", "consts") %in% names(cfg))) {
        shiny::showNotification("Invalid or unrecognised config file.",
                                type = "error")
        return()
      }
      rv$mapping <- cfg$mapping
      rv$consts  <- cfg$consts
      shiny::showNotification("Mapping config loaded.", type = "message")
      if (!is.null(cfg$pair_tree))
        shiny::updateSelectInput(session, "pair_tree",   selected = cfg$pair_tree)
      if (!is.null(cfg$pair_year))
        shiny::updateSelectInput(session, "pair_year",   selected = cfg$pair_year)
      if (!is.null(cfg$pair_dbh))
        shiny::updateSelectInput(session, "pair_dbh",    selected = cfg$pair_dbh)
      if (!is.null(cfg$pair_status))
        shiny::updateSelectInput(session, "pair_status", selected = cfg$pair_status)
      if (!is.null(cfg$dbh_unit))
        shiny::updateRadioButtons(session, "dbh_unit",   selected = cfg$dbh_unit)
    })

    # File summary card
    output$file_summary <- shiny::renderUI({
      shiny::req(rv$raw)
      shiny::tagList(
        shiny::tags$p(
          shiny::tags$span(class = "badge bg-success me-1",
                           format(nrow(rv$raw), big.mark = ",")),
          "rows \u00d7",
          shiny::tags$span(class = "badge bg-secondary ms-1", ncol(rv$raw)),
          "columns"
        ),
        shiny::tags$details(
          shiny::tags$summary(class = "text-muted small", "Show column names"),
          shiny::tags$code(class = "small",
                           paste(names(rv$raw), collapse = ", "))
        )
      )
    })

    # Raw data preview
    output$preview_raw <- DT::renderDT({
      shiny::req(rv$raw)
      DT::datatable(head(rv$raw, 200),
                    options  = list(scrollX = TRUE, pageLength = 8, dom = "tip"),
                    rownames = FALSE,
                    class    = "table-sm table-striped")
    })

    # Census pairing column selectors
    output$pair_ui <- shiny::renderUI({
      shiny::req(rv$raw)
      cols <- c("(none)", names(rv$raw))
      shiny::tagList(
        shiny::selectInput(
          "pair_tree",
          shiny::tags$span(
            shiny::tags$b("Tree ID column"),
            shiny::tags$span(class = "badge bg-danger ms-1", "required")
          ),
          cols
        ),
        shiny::selectInput(
          "pair_year",
          shiny::tags$span(
            shiny::tags$b("Census year column"),
            shiny::tags$span(class = "badge bg-danger ms-1", "required")
          ),
          cols
        ),
        shiny::selectInput(
          "pair_dbh",
          shiny::tags$span(
            shiny::tags$b("DBH column"),
            shiny::tags$span(class = "badge bg-danger ms-1", "required")
          ),
          cols
        ),
        shiny::selectInput(
          "pair_status",
          shiny::tags$span(
            shiny::tags$b("Dead/status column"),
            shiny::tags$span(class = "badge bg-secondary ms-1", "optional"),
            shiny::tags$br(),
            shiny::tags$small(
              class = "text-muted fw-normal",
              "Use when dead trees remain in the table with a flag"
            )
          ),
          cols
        )
      )
    })

    # Column mapping rows
    output$mapping_ui <- shiny::renderUI({
      shiny::req(rv$raw)
      src_cols  <- c("(none)", names(rv$raw))
      auto_cols <- c("Status", "PrevDBH", "PrevYR")

      shiny::tagList(lapply(GFB3_COLS, function(col) {
        is_req     <- col %in% GFB3_REQUIRED
        is_derived <- col %in% auto_cols
        saved_m    <- rv$mapping[[col]]
        saved_c    <- rv$consts[[col]]

        shiny::div(
          class = "d-flex align-items-center gap-2 mb-1",
          shiny::div(
            style = "width:28%; min-width:90px;",
            shiny::tags$label(
              class = if (is_req) "fw-semibold text-success" else "text-muted",
              `for` = paste0("map_", col),
              col
            ),
            if (is_derived)
              shiny::tags$span(class = "badge bg-info text-dark d-block",
                               style = "font-size:.65rem;", "auto-paired")
            else if (is_req)
              shiny::tags$span(class = "badge bg-success d-block",
                               style = "font-size:.65rem;", "required")
            else
              shiny::tags$span(class = "badge bg-light text-muted d-block",
                               style = "font-size:.65rem;", "optional")
          ),
          shiny::div(
            style = "width:36%;",
            shiny::selectInput(
              paste0("map_", col), NULL,
              choices  = src_cols,
              selected = if (!is.null(saved_m) && saved_m %in% src_cols)
                saved_m else "(none)"
            )
          ),
          shiny::div(
            style = "width:36%;",
            shiny::textInput(
              paste0("const_", col), NULL,
              placeholder = if (is_derived) "auto" else "constant\u2026",
              value       = if (!is.null(saved_c)) saved_c else ""
            )
          )
        )
      }))
    })

    get_mapping <- shiny::reactive({
      shiny::req(input[[paste0("map_", GFB3_COLS[[1]])]])
      stats::setNames(
        lapply(GFB3_COLS, function(col) input[[paste0("map_", col)]]),
        GFB3_COLS
      )
    })

    get_consts <- shiny::reactive({
      stats::setNames(
        lapply(GFB3_COLS, function(col) input[[paste0("const_", col)]]),
        GFB3_COLS
      )
    })

    # Apply mapping & census pairing
    shiny::observeEvent(input$apply_btn, {
      shiny::req(rv$raw, input$pair_tree, input$pair_year, input$pair_dbh)

      if (any(c(input$pair_tree, input$pair_year,
                input$pair_dbh) == "(none)")) {
        shiny::showNotification(
          "Please select Tree ID, year, and DBH columns before converting.",
          type = "error"
        )
        return()
      }

      mapping    <- get_mapping()
      consts     <- get_consts()
      rv$mapping <- mapping
      rv$consts  <- consts

      shiny::withProgress(
        message = "Pairing censuses and converting\u2026",
        value   = 0.4, {
          out <- tryCatch(
            .apply_mapping(
              df         = rv$raw,
              mapping    = mapping,
              const_vals = consts,
              dbh_unit   = input$dbh_unit,
              tree_col   = input$pair_tree,
              year_col   = input$pair_year,
              dbh_col    = input$pair_dbh,
              status_src = input$pair_status
            ),
            error = function(e) {
              shiny::showNotification(
                paste0("Error: ", conditionMessage(e)),
                type = "error", duration = 8
              )
              NULL
            }
          )
          shiny::setProgress(1)
        }
      )

      if (!is.null(out)) {
        rv$out <- out
        shiny::updateNavbarPage(session, inputId = NULL,
                                selected = "\u2462 Review & Download")
        shiny::showNotification(
          paste0("Converted ", format(nrow(out), big.mark = ","), " rows."),
          type = "message", duration = 4
        )
      }
    })

    # Save mapping config as JSON
    output$save_config <- shiny::downloadHandler(
      filename = function() paste0("gfb3_mapping_", Sys.Date(), ".json"),
      content  = function(f) {
        jsonlite::write_json(
          list(
            mapping     = get_mapping(),
            consts      = get_consts(),
            pair_tree   = input$pair_tree,
            pair_year   = input$pair_year,
            pair_dbh    = input$pair_dbh,
            pair_status = input$pair_status,
            dbh_unit    = input$dbh_unit
          ),
          f, pretty = TRUE, auto_unbox = TRUE
        )
      }
    )

    # Validation report
    output$validation_ui <- shiny::renderUI({
      shiny::req(rv$out)
      msgs <- .validate_output(rv$out)
      shiny::tagList(lapply(msgs, function(m) {
        cls <- switch(m$level,
                      ok    = "text-success",
                      info  = "text-primary",
                      warn  = "text-warning fw-semibold",
                      error = "text-danger fw-semibold",
                      ""
        )
        ico <- switch(m$level,
                      ok = "\u2705 ", info = "\u2139\ufe0f ", warn = "\u26a0\ufe0f ",
                      error = "\u274c ", ""
        )
        shiny::tags$p(class = cls, paste0(ico, m$text))
      }))
    })

    # Output preview table
    output$preview_out <- DT::renderDT({
      shiny::req(rv$out)
      DT::datatable(
        head(rv$out, 500),
        options  = list(scrollX = TRUE, pageLength = 10, dom = "tip"),
        rownames = FALSE,
        class    = "table-sm table-striped"
      ) |>
        DT::formatStyle(
          "Status",
          backgroundColor = DT::styleEqual(
            c(0L, 1L, 2L),
            c("#e8f5e9", "#ffebee", "#e3f2fd")
          )
        ) |>
        DT::formatRound(c("DBH", "PrevDBH"), digits = 2)
    })

    # Download handlers
    output$dl_csv <- shiny::downloadHandler(
      filename = function() paste0("gfb3_output_", Sys.Date(), ".csv"),
      content  = function(f) readr::write_csv(rv$out, f, na = "")
    )
    output$dl_xlsx <- shiny::downloadHandler(
      filename = function() paste0("gfb3_output_", Sys.Date(), ".xlsx"),
      content  = function(f) writexl::write_xlsx(rv$out, f)
    )
    output$dl_parquet <- shiny::downloadHandler(
      filename = function() paste0("gfb3_output_", Sys.Date(), ".parquet"),
      content  = function(f) arrow::write_parquet(rv$out, f)
    )
  }

  # ── Launch ──────────────────────────────────────────────────────────────────
  # Raise Shiny's upload limit to 500 MB (default is 5 MB).
  # The original value is restored when the function exits.
  old_max <- getOption("shiny.maxRequestSize")
  options(shiny.maxRequestSize = 500 * 1024^2)
  on.exit(options(shiny.maxRequestSize = old_max), add = TRUE)

  app <- shiny::shinyApp(ui = ui, server = server)
  shiny::runApp(app, launch.browser = launch.browser)

  invisible(NULL)
}
