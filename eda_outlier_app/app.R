# Outlier Explorer Shiny App (reconstructed)
# -----------------------------------------

# ---- Packages ----
requireNamespace("shiny",  quietly = TRUE) || stop("Package 'shiny' is required.")
requireNamespace("plotly", quietly = TRUE) || stop("Package 'plotly' is required.")
requireNamespace("dplyr",  quietly = TRUE) || stop("Package 'dplyr' is required.")
requireNamespace("tibble", quietly = TRUE) || stop("Package 'tibble' is required.")
requireNamespace("DT",     quietly = TRUE) || stop("Package 'DT' is required.")

library(shiny)
library(plotly)
library(dplyr)
library(tibble)
library(DT)

# ---- Load Data ----
root <- ".."   # app directory is eda_outlier_app inside project root

dataset_list <- list(
  "County"           = readRDS(file.path(root, "output/county_data.rds")),
  "Municipal"        = readRDS(file.path(root, "output/municipal_data.rds")),
  "School District"  = readRDS(file.path(root, "output/school_district_data.rds")),
  "State"            = readRDS(file.path(root, "output/state_data.rds")),
  "State Aggregated" = readRDS(file.path(root, "output/state_aggregated.rds"))
)

# list CSV files in input for diff tab
file_choices <- list.files(file.path(root, "input"), pattern = "\\.csv$", ignore.case = TRUE)

# ---- Helpers ----
get_numeric_cols <- function(df) {
  num_cols <- names(df)[sapply(df, is.numeric)]
  denom_cols  <- c("population", "student_enrollment", "enrollment", "students")
  ratio_cols  <- grep("(ratio|pct|percentage|percent)", num_cols, value = TRUE, ignore.case = TRUE)
  id_cols     <- grep("(id$|_id$|geo_id|latitude|longitude)", num_cols, value = TRUE, ignore.case = TRUE)
  flag_cols   <- grep("^flg_", num_cols, value = TRUE, ignore.case = TRUE)
  exclude_cols <- c("urban_population", "pct_urban_population", "year")
  keep <- setdiff(num_cols, c(exclude_cols, denom_cols, ratio_cols, id_cols, flag_cols))
  sort(keep)
}

# ---- UI ----
ui <- navbarPage("Data Explorer",
  tabPanel("Extreme Values",
    fluidPage(
      tags$h2("Extreme Value Explorer"),
      sidebarLayout(
        sidebarPanel(
          selectInput("dataset", "Dataset", choices = names(dataset_list)),
          uiOutput("var_ui"),
          sliderInput("iqr_mult", "IQR multiplier (outlier threshold)", 1, 5, 1.5, step = 0.5),
          checkboxInput("show_outliers_only", "Show only extreme values", FALSE)
        ),
        mainPanel(
          plotlyOutput("scatter", height = "400px"),
          tags$h4("Identified Extreme Values"),
          DTOutput("outliers_table", width = "100%")
        )
      )
    )
  ),
  tabPanel("File Differences", fluidPage(
    sidebarLayout(
      sidebarPanel(
        selectInput("diff_file", "Choose file", choices = file_choices)
      ),
      mainPanel(
        verbatimTextOutput("diff_summary"),
        DTOutput("diff_table", width = "100%")
      )
    )
  ))
)

# ---- Server ----
server <- function(input, output, session) {

  # ---------- EXTREME VALUE TAB ----------
  # reactive dataset
  r_data <- reactive(dataset_list[[input$dataset]])

  # variable choices
  output$var_ui <- renderUI({
    df <- r_data()
    selectInput("variable", "Variable", choices = get_numeric_cols(df))
  })

  # denominator column name
  denom_name <- reactive({
    df <- r_data()
    if ("student_enrollment" %in% names(df) && any(!is.na(df$student_enrollment))) {
      "student_enrollment"
    } else if ("population" %in% names(df)) {
      "population"
    } else NA_character_
  })

  # compute per-capita values & keep valid rows
  r_values <- reactive({
    req(input$variable)
    df <- r_data()
    var_vec <- df[[input$variable]]

    dn <- denom_name()
    denom <- if (!is.na(dn)) df[[dn]] else NA

    if (!all(is.na(denom))) var_vec <- var_vec / denom

    valid <- is.finite(var_vec) & is.finite(denom)

    tibble(
      row_id = which(valid),
      denom  = denom[valid],
      value  = var_vec[valid],
      label  = df$entity_name[valid]
    )
  })

  # extreme mask
  extremes <- reactive({
    v <- r_values()$value
    q1 <- quantile(v, 0.25, na.rm = TRUE)
    q3 <- quantile(v, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    lower <- q1 - input$iqr_mult * iqr
    upper <- q3 + input$iqr_mult * iqr
    v < lower | v > upper
  })

  # scatter plot
  output$scatter <- renderPlotly({
    tbl <- r_values()
    mask <- extremes()
    tbl$outlier <- ifelse(mask, "Outlier", "Inlier")

    plot_df <- if (input$show_outliers_only) tbl[mask, ] else tbl

    cols <- c(Inlier = "steelblue", Outlier = "red")

    plot_ly(plot_df,
            x = ~denom, y = ~value,
            type = 'scatter', mode = 'markers',
            color = ~outlier, colors = cols,
            text = ~label,
            hovertemplate = paste0("<b>%{text}</b><br>",
                                   "Denominator: %{x}<br>Per-capita value: %{y}<extra></extra>"),
            marker = list(size = 6, opacity = 0.7)) %>%
      layout(title = paste0("Per-capita ", input$variable, " vs ", denom_name()),
             xaxis = list(title = denom_name()),
             yaxis = list(title = paste0(input$variable, " per unit")))
  })

  # outliers table
  output$outliers_table <- renderDT({
    tbl <- r_values()
    mask <- extremes()
    df  <- r_data()
    if (!any(mask)) return(NULL)

    selected_rows <- tbl$row_id[mask]
    dn <- denom_name()

    res <- data.frame(
      Entity = df$entity_name[selected_rows],
      State  = df$state_name[selected_rows],
      Year   = df$year[selected_rows],
      Denom  = round(if (!is.na(dn)) df[[dn]][selected_rows] else NA, 0),
      Raw    = round(df[[input$variable]][selected_rows], 0),
      PerCap = round(tbl$value[mask], 0),
      stringsAsFactors = FALSE
    )
    datatable(res, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })

  # ---------- DIFF TAB ----------
  output$diff_summary <- renderText({
    fname <- input$diff_file
    if (is.null(fname) || fname == "") return("Select a file")
    current <- read.csv(file.path(root, "input", fname), stringsAsFactors = FALSE)
    arch_path <- file.path(root, "input/archive", fname)
    if (!file.exists(arch_path)) {
      return("No archived version found.")
    }
    old <- read.csv(arch_path, stringsAsFactors = FALSE)
    common_cols <- intersect(names(current), names(old))
    if (length(common_cols) == 0) {
      return("Datasets share no common columns to compare.")
    }
    added   <- nrow(dplyr::anti_join(current, old, by = common_cols))
    removed <- nrow(dplyr::anti_join(old, current, by = common_cols))
    paste0("Rows added: ", added, " | Rows removed: ", removed)
  })

  output$diff_table <- renderDT({
    fname <- input$diff_file
    if (is.null(fname) || fname == "") return("Select a file")
    current <- read.csv(file.path(root, "input", fname), stringsAsFactors = FALSE)
    arch_path <- file.path(root, "input/archive", fname)
    if (!file.exists(arch_path)) return(NULL)
    old <- read.csv(arch_path, stringsAsFactors = FALSE)
    common_cols <- intersect(names(current), names(old))
    if (length(common_cols) == 0) return(NULL)
    diffs <- dplyr::anti_join(current, old, by = common_cols)
    DT::datatable(diffs, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
}

# ---- Run App ----
shinyApp(ui, server)
