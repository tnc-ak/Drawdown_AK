AK & GA GHG EXPLORER — SHINY APP WHITE PAPER
============================================
_A lightweight, reproducible UI for sectoral GHG exploration (1990–2022)_

Script: shiny_app.R  
Data source: data/AllStateGHGData.xlsx (sheet: “Data by Economic Sectors”)  
Scope: Alaska (AK) and Georgia (GA)  
Outputs: Interactive plots + downloadable CSV

---

1. EXECUTIVE SUMMARY
--------------------

This white paper documents a Shiny-based application that provides an interactive front end for exploring greenhouse gas (GHG) emissions for Alaska (AK) and Georgia (GA) using the same source workbook as the analysis pipeline. The app is designed for quick visual interrogation of sectoral patterns, optional inclusion of LULUCF, year-range filtering, and CSV export of aggregated views. The application complements the batch pipeline (all_in_one.R) by enabling stakeholders to self-serve questions without editing the raw Excel.

Key capabilities
- Read the “Data by Economic Sectors” sheet and normalize it automatically.
- Filter by state (AK/GA), sector(s), and year range (1990–2022).
- Toggle inclusion/exclusion of LULUCF Sector Net Total.
- Group by Sector or Sub-Sector (if a sub-sector column exists).
- Choose chart type: stacked area or lines; y-axis shows Emissions (MMT CO₂ eq.).
- Download the filtered, aggregated table as CSV.

---

2. APP OBJECTIVES
-----------------

1. Provide a simple, consistent interface for browsing state-level sectoral GHG time series.
2. Ensure that filters (state/sector/years/LULUCF) are applied consistently with the batch pipeline.
3. Offer quick export of the currently viewed aggregation to CSV for reuse in downstream tools.
4. Avoid modifying the source workbook; operate in read-only mode with auto-detection of column headers.

---

3. DATA MODEL & NORMALIZATION
-----------------------------

3.1 Expected Input
------------------
- Workbook: data/AllStateGHGData.xlsx
- Sheet: “Data by Economic Sectors”
- Years: 1990–2022
- Sectors: Agriculture, Commercial, Electric Power Industry, Industry, LULUCF Sector Net Total, Residential, Transport/Transportation.

3.2 Header Detection
--------------------
The app auto-detects (case-insensitive) common header names:
- State column candidates: geo_ref, state, State, STATE, etc.
- Sector column candidates: econ_sector, Sector, etc.
- Sub-Sector (optional): Sub-Sector, Subsector, etc.
- Year columns: either wide (e.g., 1990, Y1991, …) or long with Year & Value.

3.3 Normalization Steps
-----------------------
1. Melt wide years to long format (Year, Value) if needed.
2. Coerce numeric: strip commas/non-numeric characters and convert to numeric.
3. Year filter: keep 1990–2022.
4. State harmonization: map “Alaska” → AK, “Georgia” → GA (or use existing AK/GA codes).
5. Sector harmonization: map Transport → Transportation.
6. LULUCF handling: included only if user toggles it on.
7. Aggregation: sum values by year and selected grouping (Sector or Sub-Sector).

---

4. USER EXPERIENCE
------------------

4.1 Controls
------------
- State: AK or GA.
- Include LULUCF: checkbox to include LULUCF Sector Net Total in results.
- Sector(s): multi-select; defaults to all available for the chosen state and LULUCF setting.
- Group by: “Sector” or “Sub-Sector (if available)”. If no sub-sector column exists, grouping reverts to Sector.
- Year range: continuous slider (1990–2022).
- Chart type: “Stacked area” (default) or “Lines”.
- Download CSV: exports the current aggregated result table.

4.2 Visualizations
------------------
- Stacked area: useful for relative contribution and overall magnitude.
- Lines: useful for temporal comparisons across groups.
- Y-axis is consistently labeled Emissions (MMT CO₂ eq.); legend is placed at the bottom.

4.3 Table
---------
- Displays the aggregated data the plot is using (Year, Group, Emissions), ordered by Year, then Group.

---

5. ARCHITECTURE
---------------

- Frontend/Server: Single-file Shiny application (shiny_app.R).
- Data Access: Local read of data/AllStateGHGData.xlsx. No external network calls.
- Reactivity: Filters reshape and aggregate data dynamically; plot/table/CSV reflect identical subsets.

5.1 Packages
------------
- shiny, readxl, dplyr, tidyr, stringr, ggplot2, readr

---

6. INSTALLATION & OPERATION
---------------------------

6.1 Directory Layout
--------------------
your-project/
├─ shiny_app.R
└─ data/
   └─ AllStateGHGData.xlsx   (sheet: "Data by Economic Sectors")

6.2 Install Dependencies
------------------------
install.packages(c("shiny","readxl","dplyr","tidyr","stringr","ggplot2","readr"),
                 repos = "https://cloud.r-project.org")

6.3 Run Locally
---------------
setwd("path/to/your-project")
OPTIONAL OVERRIDES:
===================
SYS.SETENV(GHG_INPUT_FILE="DATA/ALLSTATEGHGDATA.XLSX",
======================================================
GHG_SHEET_NAME="DATA BY ECONOMIC SECTORS")
==========================================
shiny::runApp("shiny_app.R")

6.4 Environment Variables (Optional)
------------------------------------
- GHG_INPUT_FILE — path to workbook (default: data/AllStateGHGData.xlsx)
- GHG_SHEET_NAME — sheet name (default: Data by Economic Sectors)

---

7. DATA QUALITY & CONSISTENCY
-----------------------------

- Header consistency matters for robust auto-detection; the app uses a list of common header synonyms but cannot guess arbitrary names.
- Sector naming: ensure “Transportation” or “Transport” is used for that sector. The app normalizes “Transport” → “Transportation”.
- LULUCF: ensure the LULUCF sector is clearly labeled (e.g., contains “LULUCF” in the name).

---

8. PERFORMANCE CONSIDERATIONS
-----------------------------

- The app reads the workbook at startup; all subsequent filtering happens in-memory.
- For larger files, consider:
  - Pre-normalizing to CSV (long format).
  - Using vroom/data.table for faster IO and aggregation.
  - Caching the normalized data with reactiveVal or memoization.

---

9. ACCESSIBILITY & THEMING
--------------------------

- Default ggplot color sets are used; for improved color-vision accessibility, consider switching to viridis palettes.
- Font sizes and legend placement are selected for clarity, but can be adjusted in the ggplot calls.

---

10. SECURITY & PRIVACY
----------------------

- The app only reads local files; no uploads or network calls are made.
- No PII is expected; data pertains to public-sector emissions summaries.

---

11. VALIDATION & TESTING
------------------------

- Smoke test: App loads; default filters display a plot and a non-empty table.
- Header test: Verify the app correctly recognizes your state/sector columns.
- Sector toggle: Confirm Transport→Transportation harmonization.
- LULUCF toggle: Verify inclusion/exclusion affects both plot and table.
- CSV download: Validate that downloaded rows match the plotted aggregation.

---

12. LIMITATIONS
---------------

- If the workbook diverges from expected structure (no recognizable state/sector headers), the app will error.
- Sub-sector grouping requires the presence of a sub-sector column in the sheet.
- Aggregation is simple summation; no uncertainty or unit conversions are performed in-app.

---

13. ROADMAP & EXTENSIONS
------------------------

- Connect directly to processed outputs (e.g., Task tables from outputs/tables) for even faster loads.
- Add views for AK projections and scenarios (BAU vs reductions, 2023–2030).
- Add county/borough detail using the Downscale outputs (Alaska 2022).
- Build a package wrapper and include unit tests for header detection and normalization.
- Add a Dockerfile for containerized deployment.

---

14. LICENSE
-----------

MIT (or your preferred license).

---

15. CONTACT
-----------

Open an issue or PR with reproducible details (R version, package versions, sheet names/headers) to request features or report problems.
