# Alaska & Georgia GHG Pipeline — `all_in_one.R`

This repository contains a **single, reproducible R pipeline** that ingests EPA-style state GHG data, produces required analytics and visualizations for **AK** and **GA**, performs **downscaling** for Alaska (2022) by sector, and builds **trend-based projections & scenarios** (2023–2030). It also supports optional **population vs. emissions** correlation plots for Alaska.

---

## What this does

1. **Parse & normalize** the Excel workbook `AllStateGHGData.xlsx` (sheet **“Data by Economic Sectors”**).
2. Generate the required **Task 1–5** tables and **Figures 1–5** for **AK & GA**:
   - Task 1: Sector totals by year (excl. LULUCF)
   - Task 2: AK sector means (1990–2022, excl. LULUCF)
   - Task 3: LULUCF Sector Net Total for AK & GA
   - Task 4: Summed emissions across sectors (excl. LULUCF) for AK & GA
   - Task 5: Adjusted totals (Sum excl. LULUCF + LULUCF net total)
3. **Downscale Alaska 2022** for **Industry, Transportation, Residential** using `data/Downscale.xlsx` (sheet-specific shares → 2022 totals).
4. **Project AK sector emissions** to **2030** using the trend-strength method (see “Methodology”).
5. Build **scenarios** for Industry & Transportation using **monotonic ramps** to **−15% and −20% of the 2022 baseline by 2030**.  
   *No dotted lines are used for reduction series.*
6. (Optional) If `data/Alaska_Population.*` exists, create **scatterplots** of Alaska **Population vs. Total Emissions (excl. LULUCF)** for **1990–2005** and **2006–2022**.

All y-axes are labeled with **units**: `Emissions (MMT CO2 eq.)`.

---

## Repository structure (expected)

```
.
├─ all_in_one.R
├─ data/
│  ├─ AllStateGHGData.xlsx    (https://www.epa.gov/system/files/other-files/2024-09/allstateghgdata90-22_v082924.zip)
│  ├─ Downscale.xlsx 
        Transport- https://dmv.alaska.gov/media/funhkbpg/2022_registeredvehiclesbyboundaryreport.pdf
        Residential https://akdot.maps.arcgis.com/apps/dashboards/5f5ac2a61b3a4fb2be9222836cb3aa05
        Industry https://live.laborstats.alaska.gov/labforce/000000/01/ces.html?
│  └─ Alaska_Population.xlsx        # optional (or .csv)   https://worldpopulationreview.com/us-counties/alaska
└─ outputs/
   ├─ tables/                       # generated
   └─ figures/                      # generated
```

> You can change input paths via environment variables (see **Configuration**).

---

## Requirements

- R ≥ 4.1
- Packages: `readxl`, `dplyr`, `tidyr`, `stringr`, `ggplot2`, `openxlsx`

Install:
```r
install.packages(c("readxl","dplyr","tidyr","stringr","ggplot2","openxlsx"),
                 repos = "https://cloud.r-project.org")
```

---

## Data expectations

### 1) `data/AllStateGHGData.xlsx`
- Sheet: **`Data by Economic Sectors`** (default; override via `GHG_SHEET_NAME`).
- Must include:
  - State column (e.g., `geo_ref`, `state`, `State`).
  - Sector column (e.g., `econ_sector`, `Sector`).
  - Year columns in **wide** form (e.g., `Y1990`, `1990`, `1991`, …), or a `Year` + `Value` pair.
- The script auto-detects year headers and normalizes **AK** and **GA**.  
- Sector labels should include the standard set: `Agriculture, Commercial, Electric Power Industry, Industry, LULUCF Sector Net Total, Residential, Transport/Transportation`.  
  *The pipeline excludes LULUCF for Tasks 1, 2, and 4; it is handled separately in Task 3 and added in Task 5.*

### 2) `data/Downscale.xlsx` (Alaska only; 2022 downscaling)
Provide three sheets—**Industry**, **Transportation**, **Residential**—each with any columns you want, but the **rightmost column** (or a column named **`Total`**) will be used as the **proxy** for shares. The first non-Total column is treated as the **name** (borough/census area).

- The script **auto-detects** sheet names (case/spacing tolerant). You can also **override** exact sheet names:
  - `DOWNSCALE_SHEET_INDUSTRY`
  - `DOWNSCALE_SHEET_TRANSPORTATION`
  - `DOWNSCALE_SHEET_RESIDENTIAL`

### 3) `data/Alaska_Population.xlsx` (optional)
- Two-column long format: `Year, Population` **or**
- Wide year columns (e.g., `1990, 1991, ...`), which the script will melt into `Year, Population`.
- Used only to make **correlation scatterplots** (AK Population vs. Task 4 AK totals).

---

## How to run

From an R session:
```r
setwd("C:/AllStateGHG")     # adjust to your project root
source("all_in_one.R")      # auto-runs main()
# or, from a shell:
# Rscript all_in_one.R
```

### Optional configuration (env vars)
You can override inputs/sheets without editing the script:
```r
Sys.setenv(
  GHG_INPUT_FILE = "data/AllStateGHGData.xlsx",
  GHG_SHEET_NAME = "Data by Economic Sectors",
  POP_FILE       = "data/Alaska_Population.xlsx",
  POP_SHEET      = NA,  # or "Sheet1" if needed
  DOWNSCALE_X    = "data/Downscale.xlsx",
  DOWNSCALE_SHEET_INDUSTRY       = "",
  DOWNSCALE_SHEET_TRANSPORTATION = "",
  DOWNSCALE_SHEET_RESIDENTIAL    = "",
  OUTDIR         = "outputs"
)
source("all_in_one.R")
```

---

## Outputs

All outputs are written to `outputs/` (created if missing).

### Tables (`outputs/tables/`)
- `task1_sector_totals_by_year.xlsx` — two sheets: **AK**, **GA**. Annual totals **by sector** (excl. LULUCF).
- `task2_ak_means_by_sector.xlsx` — mean (1990–2022) for each AK sector (excl. LULUCF).
- `task3_lulucf_net_total_ak_ga.xlsx` — annual LULUCF **net totals** for AK & GA.
- `task4_summed_excl_lulucf_ak_ga.xlsx` — annual summed emissions across sectors **excl. LULUCF**, AK & GA.
- `task5_adjusted_totals_ak_ga.xlsx` — annual **Adjusted Totals = Sum(excl. LULUCF) + LULUCF Net Total** for AK & GA.
- `All_Tasks_Summary.xlsx` — the above in one workbook.

**Projections & scenarios**
- `ak_sector_bau_projections_2023_2030.csv` — AK sector BAU projections (2023–2030).
- `ak_sector_bau_projections_2023_2030.xlsx` — Sheets: `BAU_Projections`, `Metrics`, `Scenarios_IT`.
  - *Metrics* include method used per sector, R², p-value, and window.
- `ak_industry_transport_scenarios_2023_2030.csv` — BAU + **−15%** + **−20%** (ramped to 2030 from 2022 baseline).

**Downscale (AK 2022)**
- `ak_downscale_2022_by_sector.xlsx` — sheets for **Industry**, **Transportation**, **Residential**.
- `ak_downscale_2022_by_sector.csv` — long table with shares and allocated 2022 emissions.

**Population vs Emissions (optional)**
- `ak_population_vs_task4.csv` — joined AK Population with Task 4 AK totals (excl. LULUCF).

### Figures (`outputs/figures/`)
- `fig1_stacked_area_AK.png`, `fig1_stacked_area_GA.png` — Stacked area by sector (excl. LULUCF).  
- `fig2_bar_ak_means.png` — Bar chart of AK sector means (1990–2022, excl. LULUCF).  
- `fig3_trend_lulucf.png` — LULUCF net totals trend (AK vs GA).  
- `fig4_trend_summed.png` — Summed **excl. LULUCF** trend (AK vs GA).  
- `fig5_trend_adjusted.png` — Adjusted total trend (Sum excl. LULUCF + LULUCF).  
- `ak_sector_bau_facets.png` — AK sector historical (solid) + BAU projection (dotted), with units and bold facet labels.  
- `ak_industry_transport_scenarios.png` — Faceted: **Industry** & **Transportation** with BAU and **−15%/−20%** (no dotted lines for reductions).  
- `ak_it_scenarios_combined.png` — Combined chart (both sectors together): **color = scenario (BAU, −15%, −20%)**, **linetype = sector**.  
- (Optional) `ak_pop_vs_task4_1990_2005.png`, `ak_pop_vs_task4_2006_2022.png` — Scatterplots (Population vs Emissions).  

> All y-axes use **Emissions (MMT CO2 eq.)**.

---

## Methodology

### 1) Sector-level time series (AK)
- **Historical**: 1990–2022 by sector (excl. LULUCF).
- **Trend selection** (per sector):
  - **Strong trend**: If **R² ≥ 0.4** and **p < 0.05** with full-period linear regression, use **Full-Period Linear** (1990–2022).
  - **Weak/unstable**: Else use a **Recent 5-year** linear fit (typically 2018–2022).
- **BAU projections**: Linear fit → predictions for **2023–2030**.

### 2) Scenario design (Industry & Transportation)
- Build two reduction trajectories for **both** sectors, each a **monotonic linear ramp from 2022 baseline to a 2030 target**:
  - **−15%** of 2022 by 2030
  - **−20%** of 2022 by 2030
- The BAU series remains from the trend model; reductions **do not** use dotted lines (styling is by sector in the combined plot).

> If you prefer the ramp to start from **2023 BAU** rather than **2022 actual**, you can adjust the ramp function in the script (see `ramp_for()` and the baseline picker).

### 3) Downscaling (AK 2022)
- For each sector (**Industry**, **Transportation**, **Residential**):
  1. Read the corresponding `Downscale.xlsx` sheet.
  2. Use the **rightmost column** (or column named **`Total`**) as a **proxy** for each borough/census area.
  3. Convert proxies to **shares** and scale to match the **2022 sector total** from Task 1.
- Output includes **names**, **proxy totals**, **shares**, and **allocated 2022 emissions**.

### 4) Population vs Emissions (AK)
- Join **Task 4 AK totals** (excl. LULUCF) with Alaska **Population** by `Year`.
- Produce two scatterplots with linear trend lines (correlation shown as **Pearson r**):
  - **1990–2005**
  - **2006–2022**

---

## Troubleshooting

- **`[ERROR] Input file not found`**  
  Ensure `data/AllStateGHGData.xlsx` exists (or set `GHG_INPUT_FILE`).

- **`Sheet 'Industry' not found` in Downscale**  
  Your `Downscale.xlsx` sheet names may differ. Either rename tabs or set:  
  `DOWNSCALE_SHEET_INDUSTRY`, `DOWNSCALE_SHEET_TRANSPORTATION`, `DOWNSCALE_SHEET_RESIDENTIAL`.

- **Missing sector columns (e.g., “Transport” vs “Transportation”)**  
  The script harmonizes `Transport` → `Transportation` where appropriate. If you have nonstandard sector names, check `task1_sector_totals_by_year.xlsx` (sheet **AK**) to confirm columns.

- **Year columns like `Y1990`, `Y1991`, …**  
  The script auto-detects and melts these to long `Year, Value`. If you already have a single `Year` column, it will use that as well.

- **Units / values look off**  
  Confirm the original units in your Excel are numeric. The script strips commas and coerces to numeric; blanks become `NA` then dropped in sums.

- **Population file format**  
  Provide `Year, Population` (long), or wide years in columns. The script will detect and reshape.

- **Figures missing**  
  If a needed upstream table is empty (e.g., no AK rows), the script skips the figure. Check inputs and column names.

---

## Reproducibility & versioning

- List your package versions for reference:
```r
sessionInfo()
```
- Consider adding `{renv}` to lock versions:
```r
install.packages("renv"); renv::init()
```

---

## License

MIT (or your preferred license).

---

## Contact

Questions or tweaks (e.g., different scenario ramping rules, alternate sector groupings, start years)? Open an issue or PR with a short example of your input format.
# Drawdown_AK
This repo contains the code for the drawdown Alaska project
