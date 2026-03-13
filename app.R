# ================================================================
# app.R — minimal Shiny wrapper around existing scripts
# Reads from data_raw/ at project root
# Only user input: select Einzelpostenbericht file
# ================================================================

library(shiny)
library(bslib)
library(here)
library(readxl)
library(janitor)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readr)
library(lubridate)
library(ggplot2)
library(plotly)
library(gridExtra)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

load_lohntabelle <- function(path) {
  read_excel(path) |>
    clean_names() |>
    mutate(
      rolle     = trimws(as.character(rolle)),
      jahr      = as.integer(jahr),
      monatlich = as.numeric(jahresgehalt_chf) / 12
    )
}

# ================================================================
# Helpers (from your scripts, unchanged)
# ================================================================

canonical_id <- function(x) {
  x <- as.character(x) |> str_trim()
  is_na <- is.na(x)
  x[!is_na] <- str_replace(x[!is_na], "-\\d{3}$", "")
  is_1plus6 <- (!is_na) & str_detect(x, "^1\\d{6}$")
  x[is_1plus6] <- paste0(substr(x[is_1plus6], 1, 1), "-", substr(x[is_1plus6], 2, 7))
  x
}

extract_zp_id <- function(path) {
  fn   <- basename(path)
  core <- str_match(fn, "^Zahlungsplan_PSP_(.+?)\\.xlsx$")[, 2]
  id   <- str_extract(core, "\\d-\\d{5,6}(?:-\\d{3})?|\\d{5,7}")
  canonical_id(id)
}

read_zahlungsplan <- function(f, dummy_date = as_date("2025-08-01")) {
  id <- extract_zp_id(f)
  df <- read_excel(f)
  if (nrow(df) == 0 || ncol(df) == 0)
    return(tibble(id = id, date = dummy_date, planned_income = 0))
  df <- df |> clean_names()
  if (!all(c("fallig", "betrag") %in% names(df)))
    return(tibble(id = id, date = dummy_date, planned_income = 0))
  out <- df |>
    rename(date = fallig) |>
    mutate(id = id,
           planned_income = parse_number(as.character(betrag),
                                         locale = locale(decimal_mark = ".", grouping_mark = ",")),
           date = as_date(date)) |>
    transmute(id, date, planned_income) |>
    filter(!is.na(date)) |>
    group_by(id, date) |>
    summarise(planned_income = sum(planned_income, na.rm = TRUE), .groups = "drop")
  if (nrow(out) == 0) tibble(id = id, date = dummy_date, planned_income = 0) else out
}

safe_max_date <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_Date_ else max(x) }


# ================================================================
# Spending categories
# ================================================================
CATEGORY_MAP <- c(
  # Salary
  "Lohnaufwand"             = "Salary",
  "Personalkosten"          = "Salary",
  "Pensionskasse"           = "Salary",
  "SUVA"                    = "Salary",
  "ALV"                     = "Salary",
  "AHV"                     = "Salary",
  "FAK-Beiträge"            = "Salary",
  "Fam.zul."                = "Salary",
  "Verwalt.aufw.PUBLICA"    = "Salary",

  # Consumables
  "Laborwaren"              = "Consumables",
  "Biol. Präp.& Chemika"    = "Consumables",
  "Verbrauchsmaterial"      = "Consumables",
  "Üb. Materialaufwand"     = "Consumables",
  "EDV-Verbrauchsmat."      = "Consumables",
  "IT-Verbrauchsmat."       = "Consumables",
  "Halb-&Fertigpdt.Komp"    = "Consumables",
  "Labortiere/Tierhaltg"    = "Consumables",

  # Equipment
  "Maschinen, Geräte"       = "Equipment",
  "Geräte, Maschinen"       = "Equipment",
  "Hardware bis 10'000"     = "Equipment",
  "Wäscheautomaten"         = "Equipment",
  "Unterh, Rep Mobilien"    = "Equipment",
  "Mobiliar & Einricht."    = "Equipment",

  # IT, Office & Publications
  "Software"                = "IT, Office & Publications",
  "Software (nicht akt)"    = "IT, Office & Publications",
  "IT und Telekomm."        = "IT, Office & Publications",
  "Monographien"            = "IT, Office & Publications",
  "Drucksachen, Repro"      = "IT, Office & Publications",
  "Büromaterial"            = "IT, Office & Publications",

  # Travel, Events & Training
  "Flugreisen"              = "Travel, Events & Training",
  "Bahn, ÖV-Mittel"         = "Travel, Events & Training",
  "Unterkunft"              = "Travel, Events & Training",
  "Reisekostenzurückerst."    = "Travel, Events & Training",
  "Sachtransporte"          = "Travel, Events & Training",
  "Kurier,Frachten"         = "Travel, Events & Training",
  "Seminare u. Tagungen"    = "Travel, Events & Training",
  "Aus- und Weiterbild."    = "Travel, Events & Training",
  "Aus- & Weiterbildung"    = "Travel, Events & Training",
  "ETH-interne Anlässe"     = "Travel, Events & Training",
  "Repräsentationspesen"    = "Travel, Events & Training",

  # Internal charges
  "ILV TPF bud.r.Ko-Ver"    = "Internal charges",
  "Übr. DL ETH-nah.Einh"    = "Internal charges",
  "Kostenübernahme"         = "Internal charges",
  "Gebühren"                = "Internal charges",
  "Wirtschaftsor. Fors."    = "Internal charges",
  "Schenkungen"             = "Internal charges",
  "Personalrekrutierung"    = "Internal charges",

  # Other
  "Reserve Jahresabr"       = "Other",
  "mehrere"                 = "Other"
)

CATEGORY_COLORS <- c(
  "Salary"                    = "#4682B4",
  "Consumables"               = "#52A868",
  "Equipment"                 = "#D28C3C",
  "IT, Office & Publications" = "#9664B4",
  "Travel, Events & Training" = "#C8645A",
  "Internal charges"          = "#828282",
  "Other"                     = "#B4AA96"
)

CATEGORY_ORDER <- c("Salary","Consumables","Equipment",
                    "IT, Office & Publications","Travel, Events & Training",
                    "Internal charges","Other")

# ================================================================
# Load all static data from data_raw/
# ================================================================
load_all_data <- function(ep_path) {

  raw_dir <- here("data_raw")

  # --- Konten ---
  konten <- read_excel(file.path(raw_dir, "Konten.xlsx")) |>
    clean_names() |>
    mutate(
      id           = canonical_id(id),
      bezeichnung  = trimws(bezeichnung),
      laufzeit_bis = trimws(as.character(laufzeit_bis)),
      laufzeit_date = case_when(
        tolower(laufzeit_bis) == "unendlich" ~ NA_Date_,
        suppressWarnings(!is.na(as.numeric(laufzeit_bis))) ~
          suppressWarnings(as_date(as.numeric(laufzeit_bis), origin = "1899-12-30")),
        TRUE ~ NA_Date_
      ),
      typ = trimws(as.character(typ))
    ) |>
    filter(!is.na(id), !typ %in% c("Erlöse"))

  # --- Zahlungsplan ---
  zp_files <- list.files(raw_dir, pattern = "^Zahlungsplan_PSP_.*\\.xlsx$",
                         full.names = TRUE)
  zahlungsplan <- map_df(zp_files, ~ read_zahlungsplan(.x, as_date("2025-08-01"))) |>
    group_by(id, date) |>
    summarise(planned_income = sum(planned_income, na.rm = TRUE), .groups = "drop")

  zahlungsplan_combined <- zahlungsplan |>
    left_join(konten |> select(id, bezeichnung, laufzeit_date), by = "id") |>
    group_by(id) |>
    mutate(
      last_tranche_date = max(date, na.rm = TRUE),
      fallback_laufzeit = ymd(paste0(year(last_tranche_date), "-12-31")),
      laufzeit_date     = if_else(is.na(laufzeit_date), fallback_laufzeit, laufzeit_date)
    ) |>
    ungroup() |>
    select(-last_tranche_date, -fallback_laufzeit)

  # --- Expected burn (from Zahlungsplan) ---
  expected_burn_df <- zahlungsplan_combined |>
    arrange(id, date) |>
    group_by(id) |>
    mutate(
      next_date      = lead(date),
      next_date      = if_else(is.na(next_date), laufzeit_date, next_date),
      months         = pmax(floor(interval(date, next_date) / months(1)), 1),
      burn_per_month = planned_income / months,
      burn_month     = map2(date, next_date %m-% months(1),
                            ~ seq(.x, .y, by = "1 month"))
    ) |>
    unnest(burn_month) |>
    ungroup() |>
    transmute(id, month = floor_date(burn_month, "month"), expected_burn = burn_per_month) |>
    group_by(id, month) |>
    summarise(expected_burn = sum(expected_burn, na.rm = TRUE), .groups = "drop")

  # --- Einzelposten ---
  ist_raw <- read_excel(ep_path) |>
    clean_names() |>
    rename_with(~ str_replace_all(., "\\.", "_")) |>
    mutate(
      buch_dat     = as_date(buch_dat),
      betrag_in_bw = as.numeric(betrag_in_bw),
      id           = canonical_id(kontierung),
      month        = floor_date(buch_dat, "month"),
      actual_income   = if_else(betrag_in_bw < 0, -betrag_in_bw, 0),
      actual_spending = if_else(betrag_in_bw > 0,  betrag_in_bw, 0),
      category        = coalesce(CATEGORY_MAP[kurztext], "Other")
    )

  ist_monthly <- ist_raw |>
    group_by(id, month) |>
    summarise(actual_income   = sum(actual_income,   na.rm = TRUE),
              actual_spending = sum(actual_spending, na.rm = TRUE),
              .groups = "drop")

  planned_income_m <- zahlungsplan_combined |>
    mutate(month = floor_date(date, "month")) |>
    group_by(id, month) |>
    summarise(planned_income = sum(planned_income, na.rm = TRUE), .groups = "drop")

  reference_date <- safe_max_date(ist_raw$month)

  lohn_path   <- file.path(raw_dir, "Lohntabelle.xlsx")
  lohntabelle <- if (file.exists(lohn_path)) load_lohntabelle(lohn_path) else NULL

  list(konten = konten, zahlungsplan = zahlungsplan_combined,
       expected_burn = expected_burn_df, ist_monthly = ist_monthly,
       planned_income_m = planned_income_m, reference_date = reference_date,
       ist_raw = ist_raw, lohntabelle = lohntabelle)
}

# ================================================================
# Per-PSP plot (from 02_, minimal changes)
# ================================================================
make_psp_plot <- function(psp_id, d) {
  konten          <- d$konten
  ist_monthly     <- d$ist_monthly
  planned_income_m<- d$planned_income_m
  expected_burn   <- d$expected_burn
  last_ist_month  <- d$reference_date

  startup_ids      <- konten |> filter(typ == "Startup")      |> pull(id)
  kostenstelle_ids <- konten |> filter(typ == "Kostenstelle") |> pull(id)

  if (!psp_id %in% unique(ist_monthly$id))    return(NULL)
  if (!psp_id %in% unique(planned_income_m$id)) return(NULL)

  planned_funded <- psp_id %in% c(kostenstelle_ids, startup_ids)
  balance_label  <- if (planned_funded) "Balance (planned-funded)" else "Actual balance"

  ts <- planned_income_m |>
    full_join(expected_burn, by = c("id", "month")) |>
    full_join(ist_monthly,   by = c("id", "month")) |>
    filter(id == psp_id) |>
    group_by(id, month) |>
    summarise(across(c(planned_income, expected_burn, actual_income, actual_spending),
                     ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
    arrange(month) |>
    mutate(
      across(c(planned_income, expected_burn, actual_income, actual_spending), ~ replace_na(.x, 0)),
      expected_balance = cumsum(planned_income - expected_burn),
      year = year(month),
      actual_balance = case_when(
        psp_id %in% kostenstelle_ids ~ ave(planned_income, year, FUN = cumsum) -
                                       ave(actual_spending, year, FUN = cumsum),
        psp_id %in% startup_ids      ~ cumsum(planned_income - actual_spending),
        TRUE                         ~ cumsum(actual_income  - actual_spending)
      ),
      # Fix: actual balance line ends at last IST month
      actual_balance = if_else(month <= last_ist_month, actual_balance, NA_real_)
    ) |>
    select(-year)

  if (nrow(ts) == 0) return(NULL)

  label <- konten |> filter(id == psp_id) |> pull(bezeichnung) |> first()
  title <- paste0(psp_id, if (!is.na(label)) paste0(" — ", label) else "")

  # Category spending (stacked)
  cat_monthly <- d$ist_raw |>
    filter(id == psp_id, actual_spending > 0) |>
    group_by(month, category) |>
    summarise(value = sum(actual_spending, na.rm = TRUE), .groups = "drop") |>
    mutate(category = factor(category, levels = CATEGORY_ORDER))

  # Income bars (dodged)
  income_long <- ts |>
    select(month, planned_income, actual_income) |>
    pivot_longer(-month, names_to = "series", values_to = "value") |>
    mutate(series = factor(series, levels = c("planned_income","actual_income")))

  year_lines <- tibble(x = seq(floor_date(min(ts$month), "year"),
                                ceiling_date(max(ts$month), "year"), by = "1 year"))

  p_income <- ggplot() +
    geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.4) +
    geom_col(data = income_long, aes(x = month, y = value, fill = series),
             position = "dodge", width = 25, alpha = 0.85) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
    labs(title = title, x = NULL, y = "CHF (Income)", fill = "Income") +
    theme_bw() +
    theme(plot.title = element_text(size = 14, face = "bold"),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom")

  p_spending <- ggplot() +
    geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.4) +
    geom_col(data = cat_monthly, aes(x = month, y = value, fill = category),
             position = "stack", width = 25, alpha = 0.85) +
    geom_step(data = ts, aes(x = month, y = expected_burn),
              linetype = "dashed", linewidth = 0.7, color = "grey30") +
    scale_fill_manual(values = CATEGORY_COLORS, drop = FALSE) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
    labs(x = NULL, y = "CHF (Spending)", fill = "Category") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom")

  p_balance <- ggplot() +
    geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.4) +
    geom_line(data = ts, aes(x = month, y = actual_balance,   linetype = balance_label),
              linewidth = 1.2, na.rm = TRUE) +
    geom_line(data = ts, aes(x = month, y = expected_balance, linetype = "Expected balance"),
              linewidth = 1.2) +
    geom_hline(yintercept = 0, color = "firebrick", linewidth = 0.5) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 expand = expansion(mult = c(0.01, 0.02))) +
    scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
    labs(x = "Month", y = "CHF (Balance)", linetype = "") +
    theme_bw() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
          legend.position = "bottom")

  gridExtra::grid.arrange(p_income, p_spending, p_balance, ncol = 1, heights = c(1, 1, 1))
}

# ================================================================
# New hire cost — safe, vectorised
# ================================================================
compute_extra_cost <- function(months_vec, d, new_hires, headcount, burn_window_months, burn_ref) {
  extra <- numeric(length(months_vec))
  if (is.null(d$lohntabelle) || length(new_hires) == 0) return(extra)

  lohn        <- d$lohntabelle
  today_month <- d$reference_date

  sal_pattern <- "Personalkosten|Lohnaufwand|AHV|ALV|FAK-Beitr|SUVA|Verwalt|Pensionskasse|Fam\\.zul"
  nonsalary_per_head <- tryCatch({
    if (headcount > 0 && "konto_bezeichnung" %in% names(d$ist_raw)) {
      win_start <- today_month %m-% months(burn_window_months)
      ns <- d$ist_raw |>
        filter(month > win_start, month <= today_month, actual_spending > 0,
               !str_detect(konto_bezeichnung, sal_pattern)) |>
        summarise(s = sum(actual_spending, na.rm = TRUE)) |> pull(s)
      ns / (burn_window_months * max(headcount, 1))
    } else {
      burn_ref * 0.3 / max(headcount, 1)
    }
  }, error = function(e) burn_ref * 0.3 / max(headcount, 1))

  for (h in new_hires) {
    start_cal_yr <- suppressWarnings(as.integer(h$start_jahr))
    if (is.na(start_cal_yr) || is.null(h$n) || h$n == 0) next
    start_date <- ymd(paste0(start_cal_yr, "-01-01"))
    if (is.na(start_date)) next
    active_idx <- which(months_vec >= start_date)
    if (length(active_idx) == 0) next
    max_job_yr <- max(lohn$jahr)
    for (i in active_idx) {
      job_yr <- min(year(months_vec[i]) - start_cal_yr + 1, max_job_yr)
      job_yr <- max(job_yr, 1)
      row <- lohn |> filter(rolle == h$rolle, jahr == job_yr)
      if (nrow(row) == 0) row <- lohn |> filter(rolle == h$rolle) |> arrange(desc(jahr)) |> slice(1)
      if (nrow(row) == 0) next
      extra[i] <- extra[i] + h$n * (row$monatlich[1] + nonsalary_per_head)
    }
  }
  extra
}


interpolate_balance <- function(df, by = "week") {
  date_seq <- seq(min(df$month), max(df$month), by = by)
  tibble(month = date_seq) |>
    mutate(balance = approx(df$month, df$balance, xout = month)$y)
}

# ================================================================
# Forecast / runout plot (from 04_, minimal changes)
# ================================================================
make_forecast_plot <- function(d, burn_window_months = 6,
                                exclude_ids = character(),
                                new_hires = list(), headcount = 1) {
  konten          <- d$konten
  ist_monthly     <- d$ist_monthly
  planned_income_m<- d$planned_income_m
  zahlungsplan    <- d$zahlungsplan
  last_ist_month  <- d$reference_date

  startup_ids       <- konten |> filter(typ == "Startup")      |> pull(id)
  kostenstelle_ids  <- konten |> filter(typ == "Kostenstelle") |> pull(id)
  reserve_ids       <- konten |> filter(typ == "Reserve")      |> pull(id)
  exclude_all       <- c(startup_ids, exclude_ids)

  ist_ns <- ist_monthly |> filter(!id %in% exclude_all)

  today_month <- last_ist_month
  if (is.na(today_month)) return(NULL)

  # Past income: EP for grants, ZP for Kostenstelle
  past_income_ist <- ist_ns |>
    filter(!id %in% c(kostenstelle_ids, reserve_ids)) |>
    group_by(month) |>
    summarise(income_ist = sum(actual_income, na.rm = TRUE), .groups = "drop")

  kostenstelle_income <- planned_income_m |>
    filter(id %in% c(kostenstelle_ids, reserve_ids)) |>
    group_by(month) |>
    summarise(kostenstelle_income = sum(planned_income, na.rm = TRUE), .groups = "drop")

  past_spending <- ist_ns |>
    group_by(month) |>
    summarise(spending_ist = sum(actual_spending, na.rm = TRUE), .groups = "drop")

  past_months <- tibble(month = seq(
    min(c(past_income_ist$month, kostenstelle_income$month, past_spending$month), na.rm = TRUE),
    today_month, by = "1 month"))

  ts_past <- past_months |>
    left_join(past_income_ist,    by = "month") |>
    left_join(kostenstelle_income,by = "month") |>
    left_join(past_spending,      by = "month") |>
    mutate(across(c(income_ist, kostenstelle_income, spending_ist), ~ replace_na(.x, 0)),
           total_income   = income_ist + kostenstelle_income,
           total_spending = spending_ist,
           balance        = cumsum(total_income - total_spending))

  current_surplus <- last(ts_past$balance)

  burn_ref <- ts_past |>
    filter(month < today_month) |>
    arrange(desc(month)) |>
    slice_head(n = burn_window_months) |>
    summarise(avg = mean(total_spending, na.rm = TRUE)) |>
    pull(avg)

  future_income <- planned_income_m |>
    filter(month > today_month, !id %in% exclude_all,
           !id %in% reserve_ids) |>
    group_by(month) |>
    summarise(total_income = sum(planned_income, na.rm = TRUE), .groups = "drop")

  last_income_month <- safe_max_date(future_income$month)
  horizon_end <- if (is.na(last_income_month)) today_month %m+% months(12)
                 else last_income_month %m+% months(12)
  start_future <- today_month %m+% months(1)
  if (start_future > horizon_end) horizon_end <- start_future %m+% months(24)

  future_months_vec <- seq(start_future, horizon_end, by = "1 month")
  extra_cost <- compute_extra_cost(future_months_vec, d, new_hires, headcount, burn_window_months, burn_ref)

  ts_future <- tibble(month = future_months_vec, extra = extra_cost) |>
    left_join(future_income, by = "month") |>
    mutate(total_income   = replace_na(total_income, 0),
           total_spending = burn_ref + extra,
           balance        = current_surplus + cumsum(total_income - total_spending))

  runout_row   <- ts_future |> filter(balance <= 0) |> slice_head(n = 1)
  runout_month <- if (nrow(runout_row) == 0) NA_Date_ else runout_row$month
  months_runway<- if (is.na(runout_month)) NA_real_
                  else interval(today_month, runout_month) / months(1)

  ts_all <- bind_rows(
    ts_past   |> transmute(month, total_income, total_spending, balance, period = "Past (actual)"),
    ts_future |> transmute(month, total_income, total_spending, balance, period = "Future (forecast)")
  ) |> arrange(month)

  bars_all <- ts_all |>
    select(month, total_income, total_spending) |>
    pivot_longer(c(total_income, total_spending), names_to = "series", values_to = "value") |>
    mutate(panel  = factor("Cashflow", levels = c("Cashflow","Balance")),
           series = factor(series, levels = c("total_income","total_spending")))

  bal_all <- ts_all |>
    transmute(month, series = period, value = balance,
              panel = factor("Balance", levels = c("Cashflow","Balance")))

  year_lines <- tibble(x = seq(floor_date(min(ts_all$month), "year"),
                                ceiling_date(max(ts_all$month), "year"), by = "1 year"))

  year_label_df <- tibble(year = seq(year(min(ts_all$month)), year(max(ts_all$month)))) |>
    mutate(month = as_date(paste0(year, "-05-01")), value = max(ts_all$balance) * 0.8,
           label = as.character(year),
           panel = factor("Balance", levels = c("Cashflow","Balance"))) |>
    filter(month >= min(ts_all$month), month <= max(ts_all$month))

  subtitle <- paste0(
    "Data up to: ", format(today_month, "%Y-%m"),
    " | Avg spend (last ", burn_window_months, "m): ",
    format(round(burn_ref), big.mark = "'"), " CHF",
    " | Surplus: ", format(round(current_surplus), big.mark = "'"), " CHF",
    if (!is.na(runout_month))
      paste0(" | Runout: ", format(runout_month, "%Y-%m"),
             " (", round(months_runway, 1), " months)")
    else " | No runout within horizon"
  )

  # Ribbon data for future balance (Balance facet)
  ts_fi <- interpolate_balance(ts_future)
  ribbon_pos <- ts_fi |>
    transmute(month, ymin = 0, ymax = pmax(balance, 0),
              panel = factor("Balance", levels = c("Cashflow","Balance")))
  ribbon_neg <- ts_fi |>
    transmute(month, ymin = pmin(balance, 0), ymax = 0,
              panel = factor("Balance", levels = c("Cashflow","Balance")))

  end_balance   <- last(ts_future$balance)
  end_month     <- last(ts_future$month)
  end_label     <- paste0(if (end_balance >= 0) "Expected: +" else "Expected: ",
                          format(round(end_balance), big.mark = "'"),
                          " CHF\nat end of horizon (", format(end_month, "%b %Y"), ")")
  end_label_df  <- tibble(month = end_month, value = end_balance,
                           label = end_label,
                           panel = factor("Balance", levels = c("Cashflow","Balance")))

  ggplot() +
    geom_ribbon(data = ribbon_pos, aes(x = month, ymin = ymin, ymax = ymax),
                fill = "forestgreen", alpha = 0.25, inherit.aes = FALSE) +
    geom_ribbon(data = ribbon_neg, aes(x = month, ymin = ymin, ymax = ymax),
                fill = "firebrick", alpha = 0.25, inherit.aes = FALSE) +
    geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.35) +
    geom_vline(xintercept = start_future, linetype = "dotted", linewidth = 0.8) +
    geom_col(data = bars_all, aes(x = month, y = value, fill = series),
             position = "dodge", width = 25, alpha = 0.85) +
    geom_line(data = bal_all, aes(x = month, y = value, linetype = series), linewidth = 1.2) +
    geom_hline(yintercept = 0, color = "firebrick", linewidth = 0.6) +
    geom_label(data = end_label_df, aes(x = month, y = value, label = label),
               inherit.aes = FALSE, size = 2.8, hjust = 1, vjust = 0.5,
               fill = if (end_balance >= 0) "honeydew" else "mistyrose", label.size = 0.3) +
    facet_wrap(~ panel, scales = "free_y", ncol = 1) +
    scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 expand = expansion(mult = c(0.01, 0.02))) +
    geom_text(data = year_label_df, aes(x = month, y = value, label = label),
              inherit.aes = FALSE, size = 3) +
    labs(title    = "Total Forecast (excl. Startup)",
         subtitle = subtitle,
         x = "Month", y = "CHF", fill = "Bars", linetype = "Lines") +
    theme_bw() +
    theme(plot.title       = element_text(size = 14, face = "bold"),
          plot.subtitle    = element_text(size = 9),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x      = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
          legend.position  = "bottom")
}


# ================================================================
# Per-PSP forecast plot
# ================================================================
make_psp_forecast_plot <- function(psp_id, d, burn_window_months = 6,
                                    new_hires = list(), headcount = 1) {
  konten          <- d$konten
  ist_monthly     <- d$ist_monthly
  planned_income_m<- d$planned_income_m
  today_month     <- d$reference_date

  info <- konten |> filter(id == psp_id)
  if (nrow(info) == 0) return(NULL)

  startup_ids      <- konten |> filter(typ == "Startup")      |> pull(id)
  kostenstelle_ids <- konten |> filter(typ == "Kostenstelle") |> pull(id)
  reserve_ids      <- konten |> filter(typ == "Reserve")      |> pull(id)

  planned_funded <- psp_id %in% c(kostenstelle_ids, startup_ids, reserve_ids)

  # Past
  ist_psp <- ist_monthly |> filter(id == psp_id)

  if (planned_funded) {
    past_income <- planned_income_m |> filter(id == psp_id, month <= today_month) |>
      rename(total_income = planned_income)
  } else {
    past_income <- ist_psp |> filter(month <= today_month) |>
      select(month, total_income = actual_income)
  }

  past_spending <- ist_psp |> filter(month <= today_month) |>
    select(month, total_spending = actual_spending)

  past_months <- tibble(month = seq(
    min(c(past_income$month, past_spending$month), na.rm = TRUE),
    today_month, by = "1 month"))

  ts_past <- past_months |>
    left_join(past_income,   by = "month") |>
    left_join(past_spending, by = "month") |>
    mutate(across(c(total_income, total_spending), ~ replace_na(.x, 0)),
           balance = cumsum(total_income - total_spending))

  current_surplus <- last(ts_past$balance)

  burn_ref <- ts_past |>
    filter(month < today_month) |>
    arrange(desc(month)) |>
    slice_head(n = burn_window_months) |>
    summarise(avg = mean(total_spending, na.rm = TRUE)) |>
    pull(avg)

  # Future
  future_income <- planned_income_m |>
    filter(id == psp_id, month > today_month) |>
    select(month, total_income = planned_income)

  end_date <- info$laufzeit_date[1]
  last_income_month <- safe_max_date(future_income$month)
  horizon_end <- if (!is.na(end_date)) floor_date(end_date, "month")
                 else if (!is.na(last_income_month)) last_income_month %m+% months(12)
                 else today_month %m+% months(24)
  x_limit <- horizon_end  # hard x limit

  start_future <- today_month %m+% months(1)
  if (start_future > horizon_end) horizon_end <- start_future %m+% months(12)

  future_months_vec <- seq(start_future, horizon_end, by = "1 month")
  extra_cost <- compute_extra_cost(future_months_vec, d, new_hires, headcount, burn_window_months, burn_ref)

  ts_future <- tibble(month = future_months_vec, extra = extra_cost) |>
    left_join(future_income, by = "month") |>
    mutate(total_income   = replace_na(total_income, 0),
           total_spending = burn_ref + extra,
           balance        = current_surplus + cumsum(total_income - total_spending))

  runout_row    <- ts_future |> filter(balance <= 0) |> slice_head(n = 1)
  runout_month  <- if (nrow(runout_row) == 0) NA_Date_ else runout_row$month
  months_runway <- if (is.na(runout_month)) NA_real_
                   else interval(today_month, runout_month) / months(1)

  ts_all <- bind_rows(
    ts_past   |> transmute(month, total_income, total_spending, balance, period = "Past (actual)"),
    ts_future |> transmute(month, total_income, total_spending, balance, period = "Future (forecast)")
  ) |> arrange(month)

  bars_all <- ts_all |>
    select(month, total_income, total_spending) |>
    pivot_longer(c(total_income, total_spending), names_to = "series", values_to = "value") |>
    mutate(panel  = factor("Cashflow", levels = c("Cashflow","Balance")),
           series = factor(series, levels = c("total_income","total_spending")))

  bal_all <- ts_all |>
    transmute(month, series = period, value = balance,
              panel = factor("Balance", levels = c("Cashflow","Balance")))

  year_lines <- tibble(x = seq(floor_date(min(ts_all$month), "year"),
                                ceiling_date(max(ts_all$month), "year"), by = "1 year"))

  bal_range  <- range(ts_all$balance, na.rm = TRUE)
  label_y    <- bal_range[1] + diff(bal_range) * 0.8

  year_label_df <- tibble(year = seq(year(min(ts_all$month)), year(max(ts_all$month)))) |>
    mutate(month = as_date(paste0(year, "-05-01")), value = label_y,
           label = as.character(year),
           panel = factor("Balance", levels = c("Cashflow","Balance"))) |>
    filter(month >= min(ts_all$month), month <= max(ts_all$month))

  label <- info$bezeichnung[1]
  subtitle <- paste0(
    "Data up to: ", format(today_month, "%Y-%m"),
    " | Avg spend (last ", burn_window_months, "m): ",
    format(round(burn_ref), big.mark = "'"), " CHF",
    " | Surplus: ", format(round(current_surplus), big.mark = "'"), " CHF",
    if (!is.na(runout_month))
      paste0(" | Runout: ", format(runout_month, "%Y-%m"),
             " (", round(months_runway, 1), " months)")
    else " | No runout within horizon"
  )

  # Ribbon data for future balance (Balance facet)
  ts_fi <- interpolate_balance(ts_future)
  ribbon_pos <- ts_fi |>
    transmute(month, ymin = 0, ymax = pmax(balance, 0),
              panel = factor("Balance", levels = c("Cashflow","Balance")))
  ribbon_neg <- ts_fi |>
    transmute(month, ymin = pmin(balance, 0), ymax = 0,
              panel = factor("Balance", levels = c("Cashflow","Balance")))

  end_balance  <- last(ts_future$balance)
  end_month    <- last(ts_future$month)
  end_date_lbl <- if (!is.na(end_date)) end_date else end_month
  end_label    <- paste0(if (end_balance >= 0) "Expected: +" else "Expected: ",
                          format(round(end_balance), big.mark = "'"),
                          " CHF\nat end of runtime (", format(end_date_lbl, "%b %Y"), ")")
  end_label_df <- tibble(month = end_month, value = end_balance,
                          label = end_label,
                          panel = factor("Balance", levels = c("Cashflow","Balance")))

  ggplot() +
    geom_ribbon(data = ribbon_pos, aes(x = month, ymin = ymin, ymax = ymax),
                fill = "forestgreen", alpha = 0.25, inherit.aes = FALSE) +
    geom_ribbon(data = ribbon_neg, aes(x = month, ymin = ymin, ymax = ymax),
                fill = "firebrick", alpha = 0.25, inherit.aes = FALSE) +
    geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.35) +
    geom_vline(xintercept = start_future, linetype = "dotted", linewidth = 0.8) +
    geom_col(data = bars_all, aes(x = month, y = value, fill = series),
             position = "dodge", width = 25, alpha = 0.85) +
    geom_line(data = bal_all, aes(x = month, y = value, linetype = series), linewidth = 1.2) +
    geom_hline(yintercept = 0, color = "firebrick", linewidth = 0.6) +
    geom_label(data = end_label_df, aes(x = month, y = value, label = label),
               inherit.aes = FALSE, size = 2.8, hjust = 1, vjust = 0.5,
               fill = if (end_balance >= 0) "honeydew" else "mistyrose", label.size = 0.3) +
    facet_wrap(~ panel, scales = "free_y", ncol = 1) +
    scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
    scale_fill_brewer(palette = "Set2") +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 expand = expansion(mult = c(0.01, 0.02))) +
    geom_text(data = year_label_df, aes(x = month, y = value, label = label),
              inherit.aes = FALSE, size = 3) +
    coord_cartesian(xlim = c(min(ts_all$month), x_limit)) +
    labs(title    = paste0("Forecast: ", psp_id, " — ", label),
         subtitle = subtitle,
         x = "Month", y = "CHF", fill = "Bars", linetype = "Lines") +
    theme_bw() +
    theme(plot.title       = element_text(size = 14, face = "bold"),
          plot.subtitle    = element_text(size = 9),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x      = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
          legend.position  = "bottom")
}

# ================================================================
# UI
# ================================================================
ui <- page_navbar(
  title = "Lab Budget Monitor",
  theme = bs_theme(bootswatch = "flatly"),

  nav_panel("📁 Load Data",
    card(
      card_header("Select Einzelpostenbericht"),
      helpText("All other files (Konten.xlsx, Zahlungspläne) are read automatically from data_raw/."),
      fileInput("file_ep", "Einzelpostenbericht.xlsx", accept = ".xlsx"),
      actionButton("btn_load", "Load", class = "btn-primary"),
      uiOutput("ui_load_status")
    )
  ),

  nav_panel("📊 Monitoring (per PSP)",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("ui_psp_select"),
        helpText("Actual balance ends at last available data month.")
      ),
      card(plotOutput("plot_monitoring", height = "900px"))
    )
  ),

  nav_panel("📊 Monitoring (All PSPs)",
    card(plotOutput("plot_monitoring_all", height = "900px"))
  ),

  nav_panel("🔮 Forecast (per PSP)",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("ui_psp_select_fc"),
        sliderInput("burn_window_psp", "Burn rate window (months)",
                    min = 2, max = 12, value = 6, step = 1),
        hr(),
        uiOutput("ui_newhire_controls_psp")
      ),
      card(plotOutput("plot_forecast_psp", height = "600px"))
    )
  ),

  nav_panel("🔮 Forecast (Total)",
    layout_sidebar(
      sidebar = sidebar(
        sliderInput("burn_window", "Burn rate window (months)",
                    min = 2, max = 12, value = 6, step = 1),
        helpText("Average spend over last N months used for future projection."),
        hr(),
        uiOutput("ui_newhire_controls")
      ),
      card(plotOutput("plot_forecast", height = "600px"))
    )
  )
)

# ================================================================
# Server
# ================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(data = NULL)

  observeEvent(input$btn_load, {
    req(input$file_ep)
    withProgress(message = "Loading data...", {
      tryCatch({
        rv$data <- load_all_data(input$file_ep$datapath)
        showNotification(
          paste0("✅ Loaded. Reference date: ",
                 format(rv$data$reference_date, "%d.%m.%Y")),
          type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("❌ Error:", e$message), type = "error", duration = 10)
      })
    })
  })

  output$ui_load_status <- renderUI({
    req(rv$data)
    d <- rv$data
    n_psps <- nrow(d$konten)
    n_zp   <- length(unique(d$planned_income_m$id))
    div(class = "alert alert-success mt-2",
        p(strong("Loaded successfully")),
        p(n_psps, "accounts from Konten.xlsx"),
        p(n_zp,   "Zahlungspläne found"),
        p("Reference date:", format(d$reference_date, "%d.%m.%Y"))
    )
  })

  output$ui_psp_select <- renderUI({
    req(rv$data)
    psp_ids <- rv$data$konten |>
      filter(!typ %in% c("Erlöse")) |>
      mutate(label = paste0(id, " — ", bezeichnung, " [", typ, "]")) |>
      arrange(id)
    selectInput("psp_id", "Select PSP",
                choices = setNames(psp_ids$id, psp_ids$label))
  })

  output$plot_monitoring <- renderPlot({
    req(rv$data, input$psp_id)
    p <- make_psp_plot(input$psp_id, rv$data)
    if (is.null(p)) {
      plot.new()
      text(0.5, 0.5, "No data available for this PSP", cex = 1.5)
    } else p
  })

  output$plot_monitoring_all <- renderPlot({
    req(rv$data)
    d <- rv$data

    konten          <- d$konten
    ist_monthly     <- d$ist_monthly
    planned_income_m<- d$planned_income_m
    expected_burn   <- d$expected_burn
    today_month     <- d$reference_date

    startup_ids      <- konten |> filter(typ == "Startup")      |> pull(id)
    kostenstelle_ids <- konten |> filter(typ == "Kostenstelle") |> pull(id)
    reserve_ids      <- konten |> filter(typ == "Reserve")      |> pull(id)
    exclude_ids      <- konten |> filter(typ == "Erlöse")       |> pull(id)
    exclude_all      <- c(startup_ids, exclude_ids)

    ist_ns <- ist_monthly |> filter(!id %in% exclude_all)

    past_income_ist <- ist_ns |>
      filter(!id %in% c(kostenstelle_ids, reserve_ids)) |>
      group_by(month) |>
      summarise(income = sum(actual_income, na.rm = TRUE), .groups = "drop")

    kostenstelle_income <- planned_income_m |>
      filter(id %in% c(kostenstelle_ids, reserve_ids), month <= today_month) |>
      group_by(month) |>
      summarise(kost_income = sum(planned_income, na.rm = TRUE), .groups = "drop")

    past_spending <- ist_ns |>
      group_by(month) |>
      summarise(spending = sum(actual_spending, na.rm = TRUE), .groups = "drop")

    past_months <- tibble(month = seq(
      min(c(past_income_ist$month, kostenstelle_income$month, past_spending$month), na.rm = TRUE),
      today_month, by = "1 month"))

    ts <- past_months |>
      left_join(past_income_ist,    by = "month") |>
      left_join(kostenstelle_income,by = "month") |>
      left_join(past_spending,      by = "month") |>
      mutate(across(c(income, kost_income, spending), ~ replace_na(.x, 0)),
             total_income   = income + kost_income,
             total_spending = spending,
             balance        = cumsum(total_income - total_spending))

    # Portfolio-level expected burn (sum across all non-excluded PSPs)
    exp_burn_all <- expected_burn |>
      filter(!id %in% exclude_all, month <= today_month) |>
      group_by(month) |>
      summarise(expected_burn = sum(expected_burn, na.rm = TRUE), .groups = "drop")

    ts <- ts |> left_join(exp_burn_all, by = "month") |>
      mutate(expected_burn = replace_na(expected_burn, 0))

    # Category spending across all PSPs
    cat_monthly_all <- d$ist_raw |>
      filter(!id %in% exclude_all, actual_spending > 0) |>
      group_by(month, category) |>
      summarise(value = sum(actual_spending, na.rm = TRUE), .groups = "drop") |>
      mutate(category = factor(category, levels = CATEGORY_ORDER))

    income_long <- ts |>
      select(month, total_income) |>
      pivot_longer(-month, names_to = "series", values_to = "value")

    year_lines <- tibble(x = seq(floor_date(min(ts$month), "year"),
                                  ceiling_date(max(ts$month), "year"), by = "1 year"))

    p_income <- ggplot() +
      geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.4) +
      geom_col(data = income_long, aes(x = month, y = value, fill = series),
               position = "dodge", width = 25, alpha = 0.85) +
      scale_fill_brewer(palette = "Set2") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b",
                   expand = expansion(mult = c(0.01, 0.02))) +
      scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
      labs(title = paste0("Monitoring: All PSPs (excl. Startup & Erlöse)
Data up to: ",
                          format(today_month, "%Y-%m"), " | Balance: ",
                          format(round(last(ts$balance)), big.mark = "'"), " CHF"),
           x = NULL, y = "CHF (Income)", fill = "") +
      theme_bw() +
      theme(plot.title = element_text(size = 12, face = "bold"),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom")

    p_spending <- ggplot() +
      geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.4) +
      geom_col(data = cat_monthly_all, aes(x = month, y = value, fill = category),
               position = "stack", width = 25, alpha = 0.85) +
      geom_step(data = ts, aes(x = month, y = expected_burn),
                linetype = "dashed", linewidth = 0.7, color = "grey30") +
      scale_fill_manual(values = CATEGORY_COLORS, drop = FALSE) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b",
                   expand = expansion(mult = c(0.01, 0.02))) +
      scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
      labs(x = NULL, y = "CHF (Spending)", fill = "Category") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom")

    p_balance <- ggplot() +
      geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.4) +
      geom_line(data = ts, aes(x = month, y = balance), linewidth = 1.2) +
      geom_hline(yintercept = 0, color = "firebrick", linewidth = 0.6) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b",
                   expand = expansion(mult = c(0.01, 0.02))) +
      scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
      labs(x = "Month", y = "CHF (Balance)") +
      theme_bw() +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            axis.text.x = element_text(size = 6, angle = 90, vjust = 0.5, hjust = 1),
            legend.position = "bottom")

    gridExtra::grid.arrange(p_income, p_spending, p_balance, ncol = 1, heights = c(1, 1, 1))
  })

  output$ui_psp_select_fc <- renderUI({
    req(rv$data)
    psp_ids <- rv$data$konten |>
      filter(!typ %in% c("Erlöse")) |>
      mutate(label = paste0(id, " — ", bezeichnung, " [", typ, "]")) |>
      arrange(id)
    selectInput("psp_id_fc", "Select PSP",
                choices = setNames(psp_ids$id, psp_ids$label))
  })

  # ── New hire UI (shared builder) ──────────────────────────────────────────
  safe_id <- function(x) gsub("[^A-Za-z0-9_]", "_", x)  # make valid Shiny input ID

  make_newhire_ui <- function(suffix) renderUI({
    req(rv$data)
    lohn <- rv$data$lohntabelle
    if (is.null(lohn)) return(helpText("No Lohntabelle.xlsx found in data_raw/."))
    rollen <- sort(unique(lohn$rolle))
    jahre  <- sort(unique(lohn$jahr))
    cur_yr  <- year(Sys.Date())
    cal_yrs <- as.character(cur_yr:(cur_yr + 5))  # calendar start years
    tagList(
      strong("New Hire Prediction"),
      numericInput(paste0("headcount", suffix), "Current headcount (FTE)", value = 5, min = 1, step = 1),
      lapply(rollen, function(r) {
        rid <- safe_id(r)
        div(style = "border-top:1px solid #eee;margin-top:6px;padding-top:4px;",
          strong(r),
          fluidRow(
            column(6, numericInput(paste0("nh_n_", rid, suffix), "# to hire", value = 0, min = 0, step = 1)),
            column(6, selectInput(paste0("nh_j_", rid, suffix), "Start year",
                                  choices = cal_yrs, selected = cal_yrs[1]))
          )
        )
      })
    )
  })
  output$ui_newhire_controls     <- make_newhire_ui("")
  output$ui_newhire_controls_psp <- make_newhire_ui("_psp")

  # ── Helper: read new hire inputs from UI ──────────────────────────────────
  get_new_hires <- function(suffix) {
    lohn <- rv$data$lohntabelle
    if (is.null(lohn)) return(list())
    rollen <- sort(unique(lohn$rolle))
    Filter(function(h) !is.na(h$n) && h$n > 0, lapply(rollen, function(r) {
      rid   <- safe_id(r)
      n_val <- input[[paste0("nh_n_", rid, suffix)]]
      j_val <- input[[paste0("nh_j_", rid, suffix)]]
      list(
        rolle      = r,
        n          = as.integer(if (is.null(n_val)) 0L else n_val),
        start_jahr = if (is.null(j_val)) as.character(year(Sys.Date())) else as.character(j_val)
      )
    }))
  }

  output$plot_forecast_psp <- renderPlot({
    req(rv$data, input$psp_id_fc)
    p <- make_psp_forecast_plot(
      input$psp_id_fc, rv$data, input$burn_window_psp,
      new_hires = get_new_hires("_psp"),
      headcount = max(as.integer(input$headcount_psp %||% 1), 1)
    )
    if (is.null(p)) { plot.new(); text(0.5, 0.5, "No data available", cex = 1.5) } else p
  })

  output$plot_forecast <- renderPlot({
    req(rv$data)
    nh <- get_new_hires("")
    hc <- max(as.integer(input$headcount %||% 1), 1)
    cat("DEBUG new_hires:", length(nh), "entries, headcount:", hc, "\n")
    for (h in nh) cat("  rolle:", h$rolle, "n:", h$n, "start_jahr:", h$start_jahr, "(class:", class(h$start_jahr), ")\n")
    make_forecast_plot(
      rv$data,
      burn_window_months = input$burn_window,
      new_hires = nh,
      headcount = hc
    )
  })
}

shinyApp(ui, server)
