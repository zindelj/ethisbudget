# ETH Lab Budget Monitor

A Shiny app for monitoring and forecasting lab finances across PSP accounts.

## Getting Started

### The only file you need to provide

**`data_raw/Einzelpostenbericht.xlsx`** — export this from SAP. Everything else is created automatically on first load.

### First run

1. Place your Einzelpostenbericht in the `data_raw/` folder
2. Run `app.R` in RStudio
3. Go to **📁 Load Data**, select the file and click **Load**
4. The app will auto-create empty templates for anything missing:
   - `Konten.xlsx` — pre-filled with all PSP IDs found in the EP
   - `Zahlungsplan_new.xlsx` — one empty tab per PSP
   - `Salaryplan_new.xlsx` — empty template
   - `Investments_new.xlsx` — empty template
5. Fill in details from within the App via the **📋 Konten** and **💳 Zahlungsplan** tabs, or by opening the Excel files, then save and reload

### Optional files (place in `data_raw/`)

| File | Purpose |
|------|---------|
| `Lohntabelle.xlsx` | Salary rates by role/year — enables PhD/Postdoc auto-fill in Salary Plan |

## Tabs

| Tab | What it does |
|-----|-------------|
| 📁 Load Data | Upload Einzelpostenbericht, see load status |
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
                   "rhandsontable"))
```
