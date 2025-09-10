# shiny_app.R
# Interactive explorer for AK & GA GHG data (reads 'data/AllStateGHGData.xlsx').
# - Choose State (AK/GA), Sector(s), optional Sub-Sector grouping, and Year range
# - Toggle inclusion of LULUCF
# - View chart + table + download filtered CSV
#
# Requires: shiny, readxl, dplyr, tidyr, stringr, ggplot2

suppressPackageStartupMessages({
  library(shiny)
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
})

# ---------------- Helpers to read/normalize ----------------
STATE_FILTER   <- c("AK","GA")
YEAR_MIN       <- 1990
YEAR_MAX       <- 2022
LULUCF_PATTERN <- "(?i)(lulucf|land\\s*use.*forestry)"
UNIT_Y         <- "Emissions (MMT CO2 eq.)"

detect_columns <- function(df) {
  cand <- list(
    state    = c("geo_ref","state","State","STATE","State Code","state_code","Abbreviation","ST"),
    sector   = c("econ_sector","Sector","sector","SECTOR","Economic Sector","Category","ECON_SECTOR","Econ Sector","econ sector"),
    subsector= c("Sub-Sector","Sub Sector","Subsector","Sub sector","sub_sector","subsector","SubCategory","Subcategory","Sub Category")
  )
  cols <- colnames(df)
  find_ci <- function(options, columns) {
    idx <- which(tolower(columns) %in% tolower(options))
    if (length(idx) > 0) return(columns[idx[1]])
    return(NULL)
  }
  list(
    state     = find_ci(cand$state, cols),
    sector    = find_ci(cand$sector, cols),
    subsector = find_ci(cand$subsector, cols)
  )
}

detect_year_columns <- function(df) {
  cn <- colnames(df)
  yr_idx <- grepl("(^|[^0-9])(19\\d{2}|20\\d{2})([^0-9]|$)", cn)
  yr_cols <- cn[yr_idx]
  years <- suppressWarnings(as.integer(stringr::str_extract(yr_cols, "(19\\d{2}|20\\d{2})")))
  keep <- !is.na(years) & years >= 1900 & years <= 2100
  yr_cols[keep][order(years[keep])]
}

read_normalize <- function(path, sheet = "Data by Economic Sectors") {
  req(file.exists(path))
  raw <- readxl::read_excel(path, sheet = sheet)
  cols <- detect_columns(raw)
  validate(need(!is.null(cols$state) && !is.null(cols$sector),
                "Could not detect 'state' or 'sector' columns. Check your sheet headers."))

  yr_cols <- detect_year_columns(raw)
  if (length(yr_cols) > 0) {
    df <- raw %>%
      dplyr::rename(StateRaw = !!rlang::sym(cols$state),
                    Sector   = !!rlang::sym(cols$sector)) %>%
      { if (!is.null(cols$subsector) && cols$subsector %in% names(.)) dplyr::rename(., SubSector = !!rlang::sym(cols$subsector)) else dplyr::mutate(., SubSector = NA_character_) } %>%
      tidyr::pivot_longer(tidyselect::all_of(yr_cols), names_to = "Year", values_to = "Value") %>%
      dplyr::mutate(
        Year     = suppressWarnings(as.integer(stringr::str_extract(Year, "(19\\d{2}|20\\d{2})"))),
        Value    = suppressWarnings(as.numeric(Value)),
        Sector   = trimws(as.character(Sector)),
        StateRaw = trimws(as.character(StateRaw)),
        SubSector= trimws(as.character(SubSector))
      )
  } else {
    y_cand <- intersect(c("year","Year","YEAR","Calendar Year"), names(raw))
    v_cand <- intersect(c("Emissions","Value","GHG","Total Emissions","MMT CO2e","Emissions (MMT CO2e)","Emission"), names(raw))
    validate(need(length(y_cand) > 0 && length(v_cand) > 0,
                  "No wide year columns found and could not detect 'Year'/'Value' columns."))
    df <- raw %>%
      dplyr::rename(StateRaw = !!rlang::sym(cols$state),
                    Sector   = !!rlang::sym(cols$sector),
                    Year     = !!rlang::sym(y_cand[[1]]),
                    Value    = !!rlang::sym(v_cand[[1]])) %>%
      { if (!is.null(cols$subsector) && cols$subsector %in% names(.)) dplyr::rename(., SubSector = !!rlang::sym(cols$subsector)) else dplyr::mutate(., SubSector = NA_character_) } %>%
      dplyr::mutate(
        Year     = suppressWarnings(as.integer(as.numeric(Year))),
        Value    = suppressWarnings(as.numeric(Value)),
        Sector   = trimws(as.character(Sector)),
        StateRaw = trimws(as.character(StateRaw)),
        SubSector= trimws(as.character(SubSector))
      )
  }

  df %>%
    dplyr::filter(!is.na(Year), Year >= YEAR_MIN, Year <= YEAR_MAX, !is.na(Value)) %>%
    dplyr::mutate(
      State = dplyr::case_when(
        toupper(StateRaw) %in% STATE_FILTER ~ toupper(StateRaw),
        grepl("(?i)^alaska\\b", StateRaw)   ~ "AK",
        grepl("(?i)^georgia\\b", StateRaw)  ~ "GA",
        TRUE ~ NA_character_
      ),
      # Harmonize Transport -> Transportation
      Sector = dplyr::case_when(
        tolower(Sector) %in% c("transport","transportation") ~ "Transportation",
        TRUE ~ Sector
      )
    ) %>%
    dplyr::filter(State %in% STATE_FILTER)
}

# --------------------- Shiny UI/Server ---------------------
ui <- fluidPage(
  titlePanel("AK & GA GHG Explorer"),
  sidebarLayout(
    sidebarPanel(
      helpText("This app reads 'data/AllStateGHGData.xlsx' (sheet: 'Data by Economic Sectors')."),
      selectInput("state", "State", choices = STATE_FILTER, selected = "AK"),
      checkboxInput("include_lulucf", "Include LULUCF Sector Net Total", value = FALSE),
      uiOutput("sector_select"),
      radioButtons("group_by", "Group by",
                   choices = c("Sector" = "sector", "Sub-Sector (if available)" = "subsector"),
                   selected = "sector"),
      sliderInput("yr", "Year range", min = YEAR_MIN, max = YEAR_MAX, value = c(2000, 2022), sep = ""),
      radioButtons("chart_type", "Chart type",
                   choices = c("Stacked area" = "area", "Lines" = "line"),
                   selected = "area"),
      downloadButton("dl", "Download CSV")
    ),
    mainPanel(
      plotOutput("p", height = "500px"),
      tags$hr(),
      h4("Filtered data (aggregated)"),
      tableOutput("tbl")
    )
  )
)

server <- function(input, output, session) {
  ghg_path  <- Sys.getenv("GHG_INPUT_FILE", unset = "data/AllStateGHGData.xlsx")
  ghg_sheet <- Sys.getenv("GHG_SHEET_NAME", unset = "Data by Economic Sectors")

  data_all <- reactive({
    validate(need(file.exists(ghg_path), paste0("Input not found: ", ghg_path)))
    read_normalize(ghg_path, ghg_sheet)
  })

  # Sector choices based on state filter and LULUCF toggle
  observe({
    df <- data_all() %>%
      dplyr::filter(State == input$state)
    if (!isTRUE(input$include_lulucf)) {
      df <- df %>% dplyr::filter(!grepl(LULUCF_PATTERN, Sector, perl = TRUE))
    }
    sectors <- sort(unique(df$Sector))
    output$sector_select <- renderUI({
      selectInput("sector", "Sector(s)", choices = sectors, selected = sectors, multiple = TRUE)
    })
  })

  filtered <- reactive({
    req(input$sector, input$yr)
    df <- data_all() %>% filter(State == input$state)
    if (!isTRUE(input$include_lulucf)) {
      df <- df %>% filter(!grepl(LULUCF_PATTERN, Sector, perl = TRUE))
    }
    df <- df %>% filter(Sector %in% input$sector, Year >= input$yr[1], Year <= input$yr[2])
    df
  })

  grouped_data <- reactive({
    df <- filtered()
    if (nrow(df) == 0) return(df[0,])

    if (input$group_by == "subsector" && any(!is.na(df$SubSector))) {
      df %>% group_by(Year, SubSector) %>% summarise(Emissions = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        mutate(Group = SubSector) %>% select(Year, Group, Emissions)
    } else {
      df %>% group_by(Year, Sector) %>% summarise(Emissions = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        mutate(Group = Sector) %>% select(Year, Group, Emissions)
    }
  })

  output$p <- renderPlot({
    gd <- grouped_data()
    validate(need(nrow(gd) > 0, "No data for the selected filters."))
    if (input$chart_type == "area") {
      ggplot(gd, aes(x = Year, y = Emissions, fill = Group)) +
        geom_area(position = "stack", alpha = 0.9) +
        labs(title = paste(input$state, "GHG by", ifelse(input$group_by == "subsector", "Sub-Sector", "Sector")),
             x = "Year", y = UNIT_Y, fill = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
    } else {
      ggplot(gd, aes(x = Year, y = Emissions, color = Group)) +
        geom_line(linewidth = 1) +
        labs(title = paste(input$state, "GHG by", ifelse(input$group_by == "subsector", "Sub-Sector", "Sector")),
             x = "Year", y = UNIT_Y, color = NULL) +
        theme_minimal() +
        theme(legend.position = "bottom")
    }
  })

  output$tbl <- renderTable({
    grouped_data() %>% arrange(Year, Group)
  }, striped = TRUE, bordered = TRUE, width = "100%")

  output$dl <- downloadHandler(
    filename = function() {
      paste0("ghg_", input$state, "_", input$group_by, "_", input$yr[1], "_", input$yr[2], ".csv")
    },
    content = function(file) {
      readr::write_csv(grouped_data(), file)
    }
  )
}

shinyApp(ui, server)
