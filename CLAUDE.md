# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

An ETH lab budget monitoring + forecasting Shiny app. Single-user, run locally from RStudio. Reads a SAP "Einzelpostenbericht" export plus a few Excel side-files, produces per-PSP and portfolio-level monitoring, salary planning, payment-schedule heatmaps, and runway forecasts.

See `README.md` for the user-facing workflow (which file to drop where, what each tab does).

## Collaboration workflow

Claude and the user **work in the same directory** — edits to `app.R` (or any file here) are immediately visible to the user's RStudio session. Git is used for backup/history only, not for syncing changes between Claude and the user.

Implications:
- After editing `app.R`, the user can reload the Shiny app (often with `Session → Restart R` first to flush cached function definitions) and test right away. Do **not** tell them to `git pull`.
- Still commit and push when finishing a logical change so history exists, but treat that as backup, not as a step the user needs before testing.

## Critical: data confidentiality

The user's budget data lives in a folder **outside this repository**, picked at runtime via the Load Data tab (`shinyDirButton`). The repo itself contains no real data. `.gitignore` still blocks `data_raw/`, `data/`, `*.xlsx`, `*.csv` as a safety net should anything leak in.

`.claudeignore` lists `data_raw/` historically but cannot protect paths outside the project. When debugging data-shape issues, ask the user to paste redacted snippets rather than asking for the data folder path.

## Running the app

```r
# In RStudio, with the project open (renv auto-activates via .Rprofile):
shiny::runApp("app.R")
# or just open app.R and click "Run App"
```

`renv` manages dependencies; `renv.lock` is the source of truth. After pulling changes:

```r
renv::restore()
```

There are no tests, no linter config, no build step.

## Code architecture

### Two app files

- **`app.R`** — current app, includes the Investments tab + investment forecasting toggles.
- **`app_noinvestment.R`** — older variant without Investments. Kept as a working fallback. When changing logic shared between the two (loaders, forecast plots, heatmaps), confirm with the user whether the change should be ported to both — most edits should target `app.R` only.

### `app.R` is a monolithic single-file Shiny app

There are no R modules and no `R/` directory. Everything (helpers, data loading, plotting, UI, server) is concatenated in `app.R`, separated by `# ====` banner comments. Major sections in order:

| Lines (approx.) | Section | Key entry points |
|---|---|---|
| 1–22 | Library loads | shiny, bslib, here, readxl/writexl, tidyverse, lubridate, plotly, rhandsontable |
| 25–100 | Salary-plan / Lohntabelle loaders | `load_salaryplan`, `load_lohntabelle` |
| 102–145 | ID + Zahlungsplan helpers | `canonical_id`, `extract_zp_id`, `read_zahlungsplan` |
| 148–227 | Spending category mapping | `CATEGORY_MAP` (German SAP labels → grouped categories) |
| 231–432 | **Central data ingestion** | `load_all_data(ep_path)` |
| 436–545 | Per-PSP monitoring plot | `make_psp_plot` |
| 549–617 | Salary cost + balance interpolation | `compute_salary_cost`, `interpolate_balance` |
| 621–811 | Total runway forecast | `make_forecast_plot` |
| 815–1006 | Per-PSP runway forecast | `make_psp_forecast_plot` |
| 1010–1066 | Salary heatmap | `make_salary_heatmap` |
| 1070–1098 | Zahlungsplan heatmap | `make_zp_heatmap` |
| 1102–1234 | UI (`page_navbar`, 9 `nav_panel`s) | `ui` |
| 1238–2603 | Server | `server` |
| 2604 | `shinyApp(ui, server)` |

### `load_all_data()` is the data spine

This function (around line 231) is the only place that touches the filesystem during normal use. It:

1. Receives `ep_path` (the full path to the chosen `export_*.xlsx`) and derives `raw_dir <- dirname(ep_path)`. Every other file read/written by the function lives in that same folder.
2. Reads the EP Excel, cleans column names, parses dates, classifies each row into a category via `CATEGORY_MAP`.
3. **Bootstraps missing side-files** in `raw_dir` from the PSP IDs found in the EP — empty `Konten.xlsx`, per-PSP tabs in `Zahlungsplan.xlsx`, empty `Salaryplan.xlsx`, `Investments.xlsx`. This is why the README says "Everything else is created automatically on first load."
4. Returns a named list `d` (konten, ist_monthly, planned_income_m, expected_burn, salary_plan, zahlungsplan, investments, reference_date, **raw_dir**, …) that is the **single object passed into every plotting and forecast function downstream**. The `raw_dir` field is what server save/load handlers use when writing back to disk — never re-derive it elsewhere.

When adding a new field that plots or the server need, add it to this returned list rather than re-reading files elsewhere.

### Reactive data flow in the server

The server holds the `d` list inside `reactiveValues` (`rv$data`). All tabs read from that single source. The Konten / Zahlungsplan / Salary Plan / Investments tabs use `rhandsontable` for inline editing; on save they write back to the corresponding Excel under `rv$data$raw_dir` (i.e. the same folder the user picked at load time) and re-invoke `load_all_data()` so downstream tabs see the change.

### Data folder picker

The Load Data tab uses `shinyFiles::shinyDirButton` (not Shiny's `fileInput`, which would lose the original folder path due to browser upload semantics). Picking a folder triggers two reactives: `picked_data_dir()` resolves the path, and `picked_ep_file()` scans for `^export_\d{8}_\d{6}\.xlsx$` and selects the lexicographically latest match. The full EP path (`file.path(dir, ep)`) is then handed to `load_all_data()`. Selection is session-scoped — nothing persists across restarts.

### PSP IDs

PSP account IDs come in mixed forms (numeric, with whitespace, "Startup" vs "Kostenstelle" types). `canonical_id()` is the normaliser — use it whenever comparing IDs across data sources. Filtering by account type is done via `konten |> filter(typ == "Startup")` / `"Kostenstelle"` patterns repeated throughout the plotting functions.

### Paths

The data folder is **never** assumed to be `data_raw/` or any project-relative path. All disk I/O during a loaded session goes through `rv$data$raw_dir` (server side) or the local `raw_dir <- dirname(ep_path)` inside `load_all_data` itself. `here()` is no longer used for data paths. Do not reintroduce hardcoded `here("data_raw", ...)` calls — they would break the user-picked-folder model.

## Conventions worth keeping

- 2-space indentation, UTF-8, POSIX line endings, trailing whitespace stripped (see `mybudgettools_zindel.Rproj`).
- Native pipe `|>` is used throughout; match that style rather than introducing `%>%`.
- German domain terms stay in German (Einzelposten, Zahlungsplan, Konten, Lohntabelle, Buch_Dat, PSP) — these mirror the SAP source columns; do not anglicise them in code.
- Banner comments (`# ==== ... ====`) demarcate sections; preserve them when inserting new helpers so the file stays navigable.
