#!/usr/bin/env Rscript

# all_in_one.R (unified, with fixed reduction logic)
# ------------------------------------------------------------------
# End-to-end pipeline for AK & GA:
#   - Process AllStateGHGData.xlsx (sheet "Data by Economic Sectors")
#   - Generate Tasks 1–5 tables + Figures 1–5 (labels + units)
#   - (Optional) Build AK population vs emissions scatterplots if Alaska_Population file exists
#   - Downscale AK 2022 sector totals (Industry, Transportation, Residential) using data/Downscale.xlsx
#     * Auto-detect sheet names; override via env vars if needed
#   - Project AK sector emissions (1990–2022 history -> 2023–2030 BAU) using trend-strength rules
#     (R^2 ≥ 0.4 & p<0.05 => full period, else recent 5 yrs)
#   - Reduction scenarios for Industry & Transportation:
#     **Monotonic linear ramps from the 2022 baseline to 2030 targets**
#     (−15% and −20% of 2022), applied to both sectors; no dotted lines for reductions.
#
# Configuration via env vars (all optional):
#   GHG_INPUT_FILE = "data/AllStateGHGData.xlsx"
#   GHG_SHEET_NAME = "Data by Economic Sectors"
#   POP_FILE       = "data/Alaska_Population.xlsx"   # or .csv
#   POP_SHEET      = NA (or "Sheet1")
#   DOWNSCALE_X    = "data/Downscale.xlsx"
#   DOWNSCALE_SHEET_INDUSTRY       = ""  # override exact sheet name
#   DOWNSCALE_SHEET_TRANSPORTATION = ""
#   DOWNSCALE_SHEET_RESIDENTIAL    = ""
#   OUTDIR         = "outputs"
#
# Requires: readxl, dplyr, tidyr, stringr, ggplot2, openxlsx
# ------------------------------------------------------------------

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(ggplot2)
  library(openxlsx)
})

# -------------------- Configuration --------------------
INPUT_FILE <- Sys.getenv("GHG_INPUT_FILE", unset = "data/AllStateGHGData.xlsx")
SHEET_NAME <- Sys.getenv("GHG_SHEET_NAME", unset = "Data by Economic Sectors")
OUTDIR     <- Sys.getenv("OUTDIR",         unset = "outputs")
TABLE_DIR  <- file.path(OUTDIR, "tables")
FIG_DIR    <- file.path(OUTDIR, "figures")

POP_FILE   <- Sys.getenv("POP_FILE",   unset = "data/Alaska_Population.xlsx")
POP_SHEET  <- Sys.getenv("POP_SHEET",  unset = NA_character_)

DOWNSCALE_X<- Sys.getenv("DOWNSCALE_X",unset = "data/Downscale.xlsx")
DSH_IND    <- Sys.getenv("DOWNSCALE_SHEET_INDUSTRY",       unset = "")
DSH_TRN    <- Sys.getenv("DOWNSCALE_SHEET_TRANSPORTATION", unset = "")
DSH_RES    <- Sys.getenv("DOWNSCALE_SHEET_RESIDENTIAL",    unset = "")

dir.create(OUTDIR,    showWarnings = FALSE, recursive = TRUE)
dir.create(TABLE_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FIG_DIR,   showWarnings = FALSE, recursive = TRUE)

STATE_FILTER   <- c("AK","GA")
YEAR_MIN       <- 1990
YEAR_MAX       <- 2022
YEAR_TARGET    <- 2022
LULUCF_PATTERN <- "(?i)(lulucf|land\\s*use.*forestry)"
UNIT_Y         <- "Emissions (MMT CO2 eq.)"

FORECAST_YEARS <- 2023:2030
RECENT_WINDOW  <- 5  # 2018–2022 expected

stop_if <- function(cond, msg) { if (cond) stop(msg, call. = FALSE) }

# -------------------- Column/Year Detection --------------------
detect_columns <- function(df) {
  cand <- list(
    state    = c("geo_ref","state","State","STATE","State Code","state_code","Abbreviation","ST"),
    sector   = c("econ_sector","Sector","sector","SECTOR","Economic Sector","Category","ECON_SECTOR","Econ Sector","econ sector"),
    subsector= c("Sub-Sector","Sub Sector","Subsector","Sub sector","sub_sector","subsector","SubCategory","Subcategory","Sub Category"),
    value    = c("Emissions","Value","GHG","Total Emissions","MMT CO2e","Emissions (MMT CO2e)","Emission")
  )
  cols <- colnames(df)
  find_ci <- function(options, columns) {
    idx <- which(tolower(columns) %in% tolower(options))
    if (length(idx) > 0) return(columns[idx[1]])
    return(NULL)
  }
  found <- list()
  found$state     <- find_ci(cand$state, cols)
  found$sector    <- find_ci(cand$sector, cols)
  found$subsector <- find_ci(cand$subsector, cols)
  found$value     <- find_ci(cand$value, cols)
  if (is.null(found$subsector)) found$subsector <- NA_character_
  if (is.null(found$sector)) stop("Missing required column for sector (e.g., 'econ_sector' or 'Sector').")
  if (is.null(found$state))  stop("Missing required column for state (e.g., 'geo_ref' or 'State').")
  found
}

detect_year_columns <- function(df) {
  cn <- colnames(df)
  yr_idx <- grepl("(^|[^0-9])(19\\d{2}|20\\d{2})([^0-9]|$)", cn)
  yr_cols <- cn[yr_idx]
  years <- suppressWarnings(as.integer(stringr::str_extract(yr_cols, "(19\\d{2}|20\\d{2})")))
  keep <- !is.na(years) & years >= 1900 & years <= 2100
  yr_cols <- yr_cols[keep]
  years  <- years[keep]
  ord <- order(years)
  yr_cols[ord]
}

# -------------------- Read & Normalize GHG --------------------
read_and_clean_ghg <- function(path, sheet) {
  raw <- readxl::read_excel(path, sheet = sheet)
  id <- detect_columns(raw)
  yr_cols <- detect_year_columns(raw)
  std_trim <- function(x) { trimws(as.character(x)) }

  if (length(yr_cols) > 0) {
    tmp <- raw %>%
      dplyr::rename(
        StateRaw = !!rlang::sym(id$state),
        Sector   = !!rlang::sym(id$sector)
      )
    if (!is.na(id$subsector) && id$subsector %in% names(tmp)) {
      tmp <- tmp %>% dplyr::rename(SubSector = !!rlang::sym(id$subsector))
    }
    df <- tmp %>%
      tidyr::pivot_longer(
        cols = tidyselect::all_of(yr_cols),
        names_to = "Year",
        values_to = "Value"
      ) %>%
      dplyr::mutate(
        Year     = suppressWarnings(as.integer(stringr::str_extract(Year, "(19\\d{2}|20\\d{2})"))),
        Value    = suppressWarnings(as.numeric(Value)),
        Sector   = std_trim(Sector),
        StateRaw = std_trim(StateRaw),
        SubSector= if ("SubSector" %in% names(.)) std_trim(SubSector) else NA_character_
      )
  } else {
    y_cand <- intersect(c("year","Year","YEAR","Calendar Year"), names(raw))
    v_cand <- intersect(c("Emissions","Value","GHG","Total Emissions","MMT CO2e","Emissions (MMT CO2e)","Emission"), names(raw))
    stop_if(length(y_cand) == 0 || length(v_cand) == 0,
            "Could not detect year/value columns, and no wide year columns present.")
    tmp <- raw %>%
      dplyr::rename(
        StateRaw = !!rlang::sym(id$state),
        Sector   = !!rlang::sym(id$sector),
        Year     = !!rlang::sym(y_cand[[1]]),
        Value    = !!rlang::sym(v_cand[[1]])
      )
    if (!is.na(id$subsector) && id$subsector %in% names(tmp)) {
      tmp <- tmp %>% dplyr::rename(SubSector = !!rlang::sym(id$subsector))
    }
    df <- tmp %>%
      dplyr::mutate(
        Year     = suppressWarnings(as.integer(as.numeric(Year))),
        Value    = suppressWarnings(as.numeric(Value)),
        Sector   = std_trim(Sector),
        StateRaw = std_trim(StateRaw),
        SubSector= if ("SubSector" %in% names(.)) std_trim(SubSector) else NA_character_
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
      )
    ) %>%
    dplyr::filter(State %in% STATE_FILTER)
}

# Canonicalize per (State, Sector, Year): prefer sub-sector rows if present
canonicalize_emissions <- function(df) {
  df %>%
    dplyr::group_by(State, Sector, Year) %>%
    dplyr::mutate(has_sub = any(!is.na(SubSector))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(use_row = ifelse(has_sub, !is.na(SubSector), is.na(SubSector) | is.na(SubSector))) %>%
    dplyr::filter(use_row) %>%
    dplyr::group_by(State, Sector, Year) %>%
    dplyr::summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
}

# Write multiple data frames to a workbook
save_workbook <- function(named_dfs, filepath) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(named_dfs)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, named_dfs[[nm]])
  }
  openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
}

# -------------------- Plots --------------------
plot_stacked_area <- function(df_canon, state_code, outfile) {
  pdat <- df_canon %>%
    dplyr::filter(State == state_code, !grepl(LULUCF_PATTERN, Sector, perl = TRUE))
  if (nrow(pdat) == 0) return(invisible(NULL))
  p <- ggplot(pdat, aes(x=Year, y=Value, fill=Sector)) +
    geom_area(position="stack") +
    labs(title=paste(state_code, "GHG Emissions by Sector (Stacked Area)"),
         x="Year", y=UNIT_Y) +
    theme_minimal() +
    theme(legend.position="bottom", legend.title=element_blank())
  ggsave(outfile, p, width=10, height=6, dpi=200)
}

plot_bar_means_ak <- function(df_canon, outfile) {
  pdat <- df_canon %>%
    dplyr::filter(State == "AK", !grepl(LULUCF_PATTERN, Sector, perl = TRUE)) %>%
    dplyr::group_by(Sector) %>%
    dplyr::summarise(Mean = mean(Value, na.rm=TRUE), .groups="drop") %>%
    dplyr::arrange(dplyr::desc(Mean))
  if (nrow(pdat) == 0) return(invisible(NULL))
  p <- ggplot(pdat, aes(x=reorder(Sector, Mean), y=Mean)) +
    geom_col() +
    coord_flip() +
    labs(title="Alaska Mean Emissions by Sector (1990–2022, excl. LULUCF)",
         x="Sector", y=UNIT_Y) +
    theme_minimal()
  ggsave(outfile, p, width=9, height=6, dpi=200)
}

plot_two_line <- function(df_wide, y1, y2, title, ylab, outfile) {
  pdat <- df_wide %>% tidyr::pivot_longer(cols = tidyselect::all_of(c(y1,y2)), names_to = "State", values_to = "Value")
  if (nrow(pdat) == 0) return(invisible(NULL))
  p <- ggplot(pdat, aes(x=Year, y=Value, color=State)) +
    geom_line() +
    labs(title=title, x="Year", y=ylab) +
    theme_minimal()
  ggsave(outfile, p, width=10, height=6, dpi=200)
}

# -------------------- Population helpers (optional) --------------------
detect_year_columns_loose <- function(df) {
  cn <- colnames(df)
  cn[grepl("(^|[^0-9])(19\\d{2}|20\\d{2})([^0-9]|$)", cn)]
}

read_population_ak <- function(path, sheet = NA_character_) {
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx","xls")) {
    sheets <- tryCatch(readxl::excel_sheets(path), error = function(e) character(0))
    if (!is.na(sheet) && sheet %in% sheets) {
      pop_raw <- readxl::read_excel(path, sheet = sheet)
    } else if (length(sheets) > 0) {
      pick <- sheets[1]
      pop_raw <- readxl::read_excel(path, sheet = pick)
    } else {
      pop_raw <- readxl::read_excel(path)
    }
  } else if (ext == "csv") {
    pop_raw <- read.csv(path, check.names = FALSE, stringsAsFactors = FALSE)
  } else {
    return(NULL)
  }
  nm <- tolower(names(pop_raw))
  if (all(c("year","population") %in% nm)) {
    ycol <- names(pop_raw)[which(nm == "year")[1]]
    pcol <- names(pop_raw)[which(nm == "population")[1]]
    pop <- pop_raw[, c(ycol, pcol)]
    names(pop) <- c("Year","Population")
  } else {
    yr_cols <- detect_year_columns_loose(pop_raw)
    if (length(yr_cols) == 0) return(NULL)
    pop <- pop_raw %>%
      pivot_longer(all_of(yr_cols), names_to = "Year", values_to = "Population") %>%
      mutate(Year = suppressWarnings(as.integer(stringr::str_extract(as.character(Year), "(19\\d{2}|20\\d{2})")))) %>%
      select(Year, Population)
  }
  pop %>%
    mutate(Population = suppressWarnings(as.numeric(gsub(",", "", as.character(Population))))) %>%
    filter(!is.na(Year), Year >= YEAR_MIN, Year <= YEAR_MAX, !is.na(Population)) %>%
    group_by(Year) %>%
    summarise(Population = sum(Population), .groups = "drop")
}

make_scatter <- function(df, y1, y2, outfile) {
  dd <- df %>% filter(Year >= y1, Year <= y2) %>% select(Year, Population, AK)
  if (nrow(dd) < 3) return(invisible(NULL))
  r <- suppressWarnings(cor(dd$Population, dd$AK, use = "complete.obs"))
  ann <- sprintf("Pearson r = %.3f", r)
  p <- ggplot(dd, aes(x = Population, y = AK)) +
    geom_point() +
    geom_smooth(method = "lm", se = TRUE) +
    labs(
      title = sprintf("Alaska: Population vs Emissions (No LULUCF) %d–%d", y1, y2),
      x = "Population",
      y = UNIT_Y,
      caption = ann
    ) +
    theme_minimal()
  ggsave(outfile, p, width = 9, height = 6, dpi = 200)
}

# -------------------- Downscale helpers --------------------
resolve_sector_col <- function(task1_cols, sector_label) {
  lc <- tolower(task1_cols)
  if (tolower(sector_label) == "industry") {
    idx <- which(lc == "industry")
  } else if (tolower(sector_label) %in% c("transport","transportation")) {
    idx <- which(lc %in% c("transport","transportation"))
  } else if (tolower(sector_label) == "residential") {
    idx <- which(lc == "residential")
  } else {
    idx <- integer(0)
  }
  if (length(idx) == 0) return(NA_character_)
  task1_cols[idx[1]]
}

# Guess the Downscale sheet name robustly (with optional overrides)
resolve_downscale_sheet <- function(path, target) {
  sheets <- readxl::excel_sheets(path)
  # explicit override environment variables
  if (tolower(target) == "industry"      && nzchar(DSH_IND)) return(DSH_IND)
  if (tolower(target) == "transportation"&& nzchar(DSH_TRN)) return(DSH_TRN)
  if (tolower(target) == "residential"   && nzchar(DSH_RES)) return(DSH_RES)

  pats <- switch(tolower(target),
    "industry"       = c("^\\s*industry\\s*$", "indus", "manufact"),
    "transportation" = c("^\\s*transportation\\s*$", "^\\s*transport\\s*$", "on[- ]?road", "vmt"),
    "residential"    = c("^\\s*residential\\s*$", "housing", "household", "resid")
  )
  for (p in pats) {
    m <- grep(p, sheets, ignore.case = TRUE, perl = TRUE)
    if (length(m) > 0) return(sheets[m[1]])
  }
  stop(sprintf("Sheet for '%s' not found. Available sheets: %s", target, paste(sheets, collapse = ", ")), call. = FALSE)
}

parse_num <- function(x) {
  as.numeric(gsub("[^0-9.\\-]", "", as.character(x)))
}

# ---------- Projection helpers (method you specified) ----------
fit_sector_model <- function(dat) {
  # dat: (Year, Emissions) for a single sector
  dat <- dat %>% arrange(Year)
  full_fit <- tryCatch(lm(Emissions ~ Year, data = dat), error = function(e) NULL)
  if (is.null(full_fit)) return(NULL)
  s_full <- summary(full_fit)
  r2_full <- as.numeric(s_full$r.squared)
  p_full  <- tryCatch(coef(s_full)[ "Year", "Pr(>|t|)" ], error = function(e) NA_real_)
  slope_full <- tryCatch(coef(full_fit)["Year"], error = function(e) NA_real_)
  intercept_full <- tryCatch(coef(full_fit)["(Intercept)"], error = function(e) NA_real_)

  # Recent window
  y_max <- max(dat$Year, na.rm = TRUE)
  recent_from <- y_max - (RECENT_WINDOW - 1)
  dat_recent <- dat %>% filter(Year >= recent_from)
  recent_fit <- if (nrow(dat_recent) >= 3) tryCatch(lm(Emissions ~ Year, data = dat_recent), error = function(e) NULL) else NULL

  use_full <- (!is.na(r2_full) && !is.na(p_full) && r2_full >= 0.4 && p_full < 0.05)
  method <- if (use_full) "FullPeriod_Linear" else "Recent5yr_Linear"

  if (use_full) {
    list(method = method, slope = slope_full, intercept = intercept_full, r2 = r2_full,
         p_value = p_full, window = paste0(min(dat$Year), "-", max(dat$Year)))
  } else if (!is.null(recent_fit)) {
    s_rec <- summary(recent_fit)
    list(method = method,
         slope = tryCatch(coef(recent_fit)["Year"], error = function(e) NA_real_),
         intercept = tryCatch(coef(recent_fit)["(Intercept)"], error = function(e) NA_real_),
         r2 = as.numeric(s_rec$r.squared),
         p_value = tryCatch(coef(s_rec)[ "Year", "Pr(>|t|)" ], error = function(e) NA_real_),
         window = paste0(min(dat_recent$Year), "-", max(dat_recent$Year)))
  } else {
    list(method = "FullPeriod_Linear_Fallback", slope = slope_full, intercept = intercept_full,
         r2 = r2_full, p_value = p_full, window = paste0(min(dat$Year), "-", max(dat$Year)))
  }
}

predict_years <- function(slope, intercept, years) { as.numeric(intercept + slope * years) }

# -------------------- Main --------------------
main <- function() {
  stop_if(!file.exists(INPUT_FILE), paste0("[ERROR] Input file not found: ", INPUT_FILE))

  # A) Read & normalize, canonicalize
  df_raw <- read_and_clean_ghg(INPUT_FILE, SHEET_NAME)
  df_canon <- canonicalize_emissions(df_raw)

  # B) Build Task tables
  task1 <- lapply(STATE_FILTER, function(st) {
    df_canon %>%
      filter(State == st, !grepl(LULUCF_PATTERN, Sector, perl = TRUE)) %>%
      group_by(Year, Sector) %>%
      summarise(Value = sum(Value), .groups="drop") %>%
      pivot_wider(names_from = Sector, values_from = Value) %>%
      arrange(Year)
  })
  names(task1) <- STATE_FILTER
  save_workbook(task1, file.path(TABLE_DIR, "task1_sector_totals_by_year.xlsx"))

  ak_means <- df_canon %>%
    filter(State == "AK", !grepl(LULUCF_PATTERN, Sector, perl = TRUE)) %>%
    group_by(Sector) %>%
    summarise(Mean = mean(Value, na.rm=TRUE), .groups="drop")
  save_workbook(list(AK_Means_By_Sector = ak_means), file.path(TABLE_DIR, "task2_ak_means_by_sector.xlsx"))

  lulucf <- df_canon %>%
    filter(grepl(LULUCF_PATTERN, Sector, perl = TRUE)) %>%
    group_by(Year, State) %>%
    summarise(Value = sum(Value), .groups="drop") %>%
    pivot_wider(names_from = State, values_from = Value) %>%
    arrange(Year)
  save_workbook(list(LULUCF_Net_Total = lulucf), file.path(TABLE_DIR, "task3_lulucf_net_total_ak_ga.xlsx"))

  totals_no_lulucf <- df_canon %>%
    filter(!grepl(LULUCF_PATTERN, Sector, perl = TRUE)) %>%
    group_by(Year, State) %>%
    summarise(Total_No_LULUCF = sum(Value), .groups="drop") %>%
    pivot_wider(names_from = State, values_from = Total_No_LULUCF) %>%
    arrange(Year)
  save_workbook(list(Summed_No_LULUCF = totals_no_lulucf), file.path(TABLE_DIR, "task4_summed_excl_lulucf_ak_ga.xlsx"))

  totals_long <- df_canon %>%
    filter(!grepl(LULUCF_PATTERN, Sector, perl = TRUE)) %>%
    group_by(Year, State) %>%
    summarise(SumNoL = sum(Value), .groups = "drop")
  lulucf_long <- df_canon %>%
    filter(grepl(LULUCF_PATTERN, Sector, perl = TRUE)) %>%
    group_by(Year, State) %>%
    summarise(Lulucf = sum(Value), .groups = "drop")
  adj_long <- dplyr::full_join(totals_long, lulucf_long, by = c("Year","State")) %>%
    mutate(SumNoL = tidyr::replace_na(SumNoL, 0),
           Lulucf = tidyr::replace_na(Lulucf, 0),
           Adjusted = SumNoL + Lulucf)
  adj <- adj_long %>%
    select(Year, State, Adjusted) %>%
    pivot_wider(names_from = State, values_from = Adjusted) %>%
    rename(AK_Adjusted_Total = AK, GA_Adjusted_Total = GA) %>%
    arrange(Year)
  save_workbook(list(Adjusted_Totals = adj), file.path(TABLE_DIR, "task5_adjusted_totals_ak_ga.xlsx"))

  combined <- list(
    Task1_AK = task1[["AK"]],
    Task1_GA = task1[["GA"]],
    Task2_AK_Means = ak_means,
    Task3_LULUCF = lulucf,
    Task4_Summed_No_LULUCF = totals_no_lulucf,
    Task5_Adjusted_Totals = adj
  )
  save_workbook(combined, file.path(TABLE_DIR, "All_Tasks_Summary.xlsx"))

  # C) Figures 1–5
  plot_stacked_area(df_canon, "AK", file.path(FIG_DIR, "fig1_stacked_area_AK.png"))
  plot_stacked_area(df_canon, "GA", file.path(FIG_DIR, "fig1_stacked_area_GA.png"))
  plot_bar_means_ak(df_canon, file.path(FIG_DIR, "fig2_bar_ak_means.png"))
  if (all(c("AK","GA") %in% colnames(lulucf))) {
    plot_two_line(lulucf, "AK", "GA",
                  "LULUCF Sector Net Total (AK vs GA)",
                  UNIT_Y, file.path(FIG_DIR, "fig3_trend_lulucf.png"))
  }
  if (all(c("AK","GA") %in% colnames(totals_no_lulucf))) {
    plot_two_line(totals_no_lulucf, "AK", "GA",
                  "Summed Emissions excl. LULUCF (AK vs GA)",
                  UNIT_Y, file.path(FIG_DIR, "fig4_trend_summed.png"))
  }
  if (all(c("AK_Adjusted_Total","GA_Adjusted_Total") %in% colnames(adj))) {
    plot_two_line(adj, "AK_Adjusted_Total", "GA_Adjusted_Total",
                  "Adjusted Totals (Sum + LULUCF) (AK vs GA)",
                  UNIT_Y, file.path(FIG_DIR, "fig5_trend_adjusted.png"))
  }

  # D) Optional population-vs-emissions scatterplots from Task4 AK + population (if POP_FILE exists)
  pop_ak <- read_population_ak(POP_FILE, POP_SHEET)
  if (!is.null(pop_ak) && "AK" %in% colnames(totals_no_lulucf)) {
    joined <- dplyr::inner_join(pop_ak, totals_no_lulucf %>% select(Year, AK = AK), by = "Year") %>% arrange(Year)
    utils::write.csv(joined[, c("Year","Population","AK")], file.path(TABLE_DIR, "ak_population_vs_task4.csv"), row.names = FALSE)
    make_scatter(joined, 1990, 2005, file.path(FIG_DIR, "ak_pop_vs_task4_1990_2005.png"))
    make_scatter(joined, 2006, 2022, file.path(FIG_DIR, "ak_pop_vs_task4_2006_2022.png"))
  }

  # E) Downscale AK 2022 for three sectors using data/Downscale.xlsx
  if (file.exists(DOWNSCALE_X)) {
    message("Downscale: reading sector totals for 2022 from Task1 (AK)...")
    task1_path <- file.path(TABLE_DIR, "task1_sector_totals_by_year.xlsx")
    get_2022 <- function(sector_label){
      df <- readxl::read_excel(task1_path, sheet = "AK")
      ycol <- names(df)[tolower(names(df)) == "year"][1]
      # standardize transport header
      if ("Transport" %in% names(df) && !("Transportation" %in% names(df))) {
        names(df)[names(df) == "Transport"] <- "Transportation"
      }
      scol <- resolve_sector_col(names(df), sector_label)
      stop_if(is.na(scol), paste0("Sector column for '", sector_label, "' not found in Task1 AK sheet."))
      val <- df %>% filter(.data[[ycol]] == YEAR_TARGET) %>% pull(.data[[scol]])
      stop_if(length(val) == 0 || all(is.na(val)), paste0("No ", YEAR_TARGET, " value for '", sector_label, "' in Task1 AK."))
      as.numeric(val[1])
    }
    ind_2022 <- get_2022("Industry")
    trn_2022 <- get_2022("Transportation")
    res_2022 <- get_2022("Residential")

    # Resolve downscale sheet names (auto-detect or use overrides)
    sheet_ind <- resolve_downscale_sheet(DOWNSCALE_X, "Industry")
    sheet_trn <- resolve_downscale_sheet(DOWNSCALE_X, "Transportation")
    sheet_res <- resolve_downscale_sheet(DOWNSCALE_X, "Residential")
    message(sprintf("Using Downscale sheets -> Industry: '%s' | Transportation: '%s' | Residential: '%s'",
                    sheet_ind, sheet_trn, sheet_res))

    downscale_sector_sheet <- function(sheet_name, sector_label, sector_total) {
      raw <- readxl::read_excel(DOWNSCALE_X, sheet = sheet_name)
      stop_if(ncol(raw) < 1, paste0("Sheet '", sheet_name, "' appears empty."))
      total_col <- names(raw)[tolower(names(raw)) == "total"]
      if (length(total_col) == 0) { total_col <- names(raw)[ncol(raw)]; message(sprintf("Note: using rightmost column '%s' as Total for sheet '%s'.", total_col, sheet_name)) }
      df <- raw %>% mutate(Total = parse_num(.data[[ total_col[1] ]]))
      df$Total[is.na(df$Total)] <- 0
      name_col <- names(df)[1]; if (tolower(name_col) == tolower(total_col[1]) && ncol(df) >= 2) name_col <- names(df)[2]
      Name <- df[[name_col]]; if (is.numeric(Name)) Name <- paste0("Row_", seq_len(nrow(df))) else Name <- as.character(Name)
      sum_total <- sum(df$Total, na.rm = TRUE)
      Share <- if (sum_total > 0) df$Total / sum_total else rep(0, nrow(df))
      Alloc <- Share * sector_total
      tibble(Sector = sector_label, Name = Name, Total_Proxy = df$Total, Share = Share, Emissions_2022 = Alloc) %>% arrange(desc(Emissions_2022))
    }

    ind_out <- downscale_sector_sheet(sheet_ind, "Industry",       ind_2022)
    trn_out <- downscale_sector_sheet(sheet_trn, "Transportation", trn_2022)
    res_out <- downscale_sector_sheet(sheet_res, "Residential",    res_2022)

    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "Industry");       openxlsx::writeData(wb, "Industry",       ind_out)
    openxlsx::addWorksheet(wb, "Transportation"); openxlsx::writeData(wb, "Transportation", trn_out)
    openxlsx::addWorksheet(wb, "Residential");    openxlsx::writeData(wb, "Residential",    res_out)
    openxlsx::saveWorkbook(wb, file.path(TABLE_DIR, "ak_downscale_2022_by_sector.xlsx"), overwrite = TRUE)
    utils::write.csv(bind_rows(ind_out, trn_out, res_out),
                     file.path(TABLE_DIR, "ak_downscale_2022_by_sector.csv"), row.names = FALSE)
    message("Downscale complete: wrote tables to outputs/tables/.")
  } else {
    message("Downscale skipped: file not found => ", DOWNSCALE_X)
    message("Place your workbook at data/Downscale.xlsx (sheets for Industry/Transportation/Residential; last col = Total).")
  }

  # F) Sector projections (AK): BAU 2023–2030 + Industry/Transportation scenarios (ramp reductions)
  ak_tbl <- task1[["AK"]]
  if (!is.null(ak_tbl)) {
    # fix Transport header if needed
    if ("Transport" %in% names(ak_tbl) && !("Transportation" %in% names(ak_tbl))) {
      names(ak_tbl)[names(ak_tbl) == "Transport"] <- "Transportation"
    }
    ak_long <- ak_tbl %>%
      tidyr::pivot_longer(-Year, names_to = "Sector", values_to = "Emissions") %>%
      mutate(Year = as.integer(Year), Emissions = as.numeric(Emissions)) %>%
      filter(!is.na(Year), !is.na(Emissions))

    sectors <- sort(unique(ak_long$Sector))
    metrics <- list(); proj_list <- list()

    for (s in sectors) {
      dat <- ak_long %>% filter(Sector == s) %>% arrange(Year)
      m <- fit_sector_model(dat); if (is.null(m)) next
      yhat <- predict_years(m$slope, m$intercept, FORECAST_YEARS)
      proj_list[[s]] <- tibble(Sector = s, Year = FORECAST_YEARS, Emissions = yhat, Type = "Projection_BAU")
      metrics[[s]] <- tibble(Sector = s, Method = m$method, Window = m$window, Slope = m$slope,
                             Intercept = m$intercept, R2 = m$r2, PValue_Year = m$p_value)
    }

    projections <- bind_rows(proj_list)
    metrics_tbl <- bind_rows(metrics)

    # Save BAU projections
    utils::write.csv(projections %>% arrange(Sector, Year),
                     file.path(TABLE_DIR, "ak_sector_bau_projections_2023_2030.csv"),
                     row.names = FALSE)

    # Faceted plot: historical (solid) + BAU (dotted), with unit + legend
    hist <- ak_long %>% dplyr::mutate(Type = "Historical")
    plot_data <- dplyr::bind_rows(hist, projections) %>%
      dplyr::mutate(Series = ifelse(Type == "Historical", "Historical", "BAU Projection"))

    p1 <- ggplot(plot_data, aes(x = Year, y = Emissions, linetype = Series)) +
      geom_line(size = 0.9) +
      facet_wrap(~ Sector, scales = "free_y") +
      labs(title    = "Alaska Sector Emissions: Historical (1990–2022) & BAU (2023–2030)",
           subtitle = "Solid = Historical, Dotted = BAU Projection",
           x = "Year", y = UNIT_Y, linetype = NULL) +
      scale_linetype_manual(values = c("Historical" = "solid",
                                       "BAU Projection" = "dotted")) +
      theme_minimal() +
      theme(legend.position = "bottom",
            strip.text = element_text(face = "bold", size = 12))
    ggsave(file.path(FIG_DIR, "ak_sector_bau_facets.png"), p1, width = 12, height = 8, dpi = 200)

    # === Reduction scenarios (ramp from 2022 baseline to 2030 targets) ===
    scen_sectors <- c("Industry","Transportation")
    get_baseline_2022 <- function(s) {
      v <- ak_long %>% filter(Sector == s, Year == 2022) %>% pull(Emissions)
      if (length(v) > 0 && !is.na(v[1])) return(as.numeric(v[1]))
      NA_real_
    }
    ramp_for <- function(baseline, target, years) {
      n <- 2030 - 2022
      baseline + (years - 2022) / n * (target - baseline)
    }

    scen_rows <- list()
    for (s in scen_sectors) {
      base22 <- get_baseline_2022(s)
      if (is.na(base22)) next
      tgt15 <- 0.85 * base22
      tgt20 <- 0.80 * base22
      y  <- FORECAST_YEARS
      y15 <- ramp_for(base22, tgt15, y)
      y20 <- ramp_for(base22, tgt20, y)
      scen_rows[[paste0(s,"_BAU")]] <- projections %>% filter(Sector == s) %>%
        transmute(Sector, Year, Series = "BAU", Value = Emissions)
      scen_rows[[paste0(s,"_15")]]  <- tibble(Sector = s, Year = y, Series = "-15%", Value = y15)
      scen_rows[[paste0(s,"_20")]]  <- tibble(Sector = s, Year = y, Series = "-20%", Value = y20)
    }
    scen_plot_all <- bind_rows(scen_rows) %>%
      mutate(Series = factor(Series, levels = c("BAU","-15%","-20%")))

    # Save table (scenarios IT with ramp method)
    utils::write.csv(scen_plot_all %>% arrange(Sector, Series, Year),
                     file.path(TABLE_DIR, "ak_industry_transport_scenarios_2023_2030.csv"),
                     row.names = FALSE)

    # Faceted scenarios (per sector) — reductions are NOT dotted
    p2 <- ggplot(scen_plot_all, aes(x = Year, y = Value, color = Series)) +
      geom_line(size = 1) +
      facet_wrap(~ Sector, scales = "free_y") +
      labs(title = "Industry & Transportation (AK): BAU vs −15% and −20% (2023–2030)",
           x = "Year", y = UNIT_Y, color = "Scenario") +
      theme_minimal()
    ggsave(file.path(FIG_DIR, "ak_industry_transport_scenarios.png"), p2, width = 10, height = 6, dpi = 200)

    # Combined scenarios (both sectors on one axes): color = Scenario, linetype = Sector (no dotted for reductions)
    p2c <- ggplot(scen_plot_all, aes(Year, Value, color = Series, linetype = Sector)) +
      geom_line(size = 1.2) +
      scale_linetype_manual(values = c("Industry"="solid", "Transportation"="longdash")) +
      labs(title = "AK Industry & Transportation: BAU and −15%/−20% (Ramped to 2030)",
           x = "Year", y = UNIT_Y, color = "Scenario", linetype = "Sector") +
      theme_minimal()
    ggsave(file.path(FIG_DIR, "ak_it_scenarios_combined.png"), p2c, width = 10, height = 6, dpi = 200)

    # Excel workbook for projections + metrics + scenarios
    wb <- openxlsx::createWorkbook()
    openxlsx::addWorksheet(wb, "BAU_Projections"); openxlsx::writeData(wb, "BAU_Projections", projections %>% arrange(Sector, Year))
    openxlsx::addWorksheet(wb, "Metrics");         openxlsx::writeData(wb, "Metrics", metrics_tbl %>% arrange(Sector))
    openxlsx::addWorksheet(wb, "Scenarios_IT");    openxlsx::writeData(wb, "Scenarios_IT", scen_plot_all %>% arrange(Sector, Series, Year))
    openxlsx::saveWorkbook(wb, file.path(TABLE_DIR, "ak_sector_bau_projections_2023_2030.xlsx"), overwrite = TRUE)

  } else {
    message("Projection step skipped: Task1 AK table not found in memory.")
  }

  message(sprintf("Done. Outputs written to: %s", OUTDIR))
}

# Auto-run main() even when sourced interactively, for convenience
main()
