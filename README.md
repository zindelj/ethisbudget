# ETH Lab Budget Monitor

A Shiny app for monitoring and forecasting lab finances across PSP accounts.

## Getting Started

### Where the app code lives vs. where your data lives

The app code (`app.R`, `renv/`, etc.) lives in this repository. **Your budget data lives in a separate folder of your choice** — anywhere on your machine, outside this repo. You pick that folder each time you start the app.

### The only file you need to provide

Place an SAP export named **`export_YYYYMMDD_HHMMSS.xlsx`** (e.g. `export_20260506_082836.xlsx`) into your data folder. Everything else is created automatically on first load. If multiple `export_*.xlsx` files are present, the app loads the most recent one (by filename timestamp).

### First run

1. Drop your `export_YYYYMMDD_HHMMSS.xlsx` into a folder of your choice.
2. Open `app.R` in RStudio and click **Run App**.
3. Go to **📁 Load Data**, click **Browse folder…**, select your data folder, then click **Load**.
4. The app will auto-create empty templates for anything missing in that same folder:
   - `Konten.xlsx` — pre-filled with all PSP IDs found in the EP
   - `Zahlungsplan.xlsx` — one empty tab per PSP
   - `Salaryplan.xlsx` — empty template
   - `Investments.xlsx` — empty template
5. Fill in details from within the App via the **📋 Konten** and **💳 Zahlungsplan** tabs, or by opening the Excel files, then save and reload.

### Optional files (place in your data folder)

| File | Purpose |
|------|---------|
| `Lohntabelle.xlsx` | Salary rates by role/year — enables PhD/Postdoc auto-fill in Salary Plan |

## Tabs

| Tab | What it does |
|-----|-------------|
| 📁 Load Data | Pick data folder, see which EP file will load, see load status |
| 📊 Monitoring (per PSP) | Income, spending and balance per grant |
| 📊 Monitoring (All PSPs) | Portfolio-level overview |
| 👥 Salary Plan | Plan personnel costs by person/month/PSP |
| 💳 Zahlungsplan | Edit expected income tranches, heatmap overview |
| 💰 Investments | Plan one-off large costs (equipment, sequencing, etc.) |
| 📋 Konten | Edit PSP account details |
| 🔮 Forecast (per PSP) | Runway forecast for individual grants |
| 🔮 Forecast (Total) | Portfolio-level runway with salary + investment toggles |

## Dependencies

```r
install.packages(c("shiny", "bslib", "readxl", "writexl", "janitor",
                   "dplyr", "tidyr", "purrr", "stringr", "lubridate",
                   "ggplot2", "plotly", "gridExtra", "scales", "here",
                   "rhandsontable", "shinyFiles"))
```

If you use `renv` (recommended), run `renv::install("shinyFiles")` then `renv::snapshot()` after pulling these changes.

## Using Claude Code

Keep your data folder **outside** this repository. The `.gitignore` blocks `data_raw/`, `data/`, `*.xlsx`, etc. as a safety net should anything land here by mistake, but the cleaner separation is to keep budget data on a path that the repo never touches.

Note that `.claudeignore` only protects paths inside the project. If you point Claude Code at your data folder explicitly, it will read it. The simplest practice is: never give Claude Code the data folder path.
