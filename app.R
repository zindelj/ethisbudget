# ================================================================
# app.R — minimal Shiny wrapper around existing scripts
# Reads from a user-selected data folder (picked via shinyDirButton)
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
library(writexl)
library(rhandsontable)
library(shinyFiles)


load_salaryplan <- function(path) {
  sheets <- excel_sheets(path)

  # New format: multiple sheets = one per person (Month | CHF | PSP | Role | FTE)
  # Each sheet returns list(data=..., notes=...) so parse failures surface in
  # the data-health report instead of vanishing.
  if (length(sheets) > 1) {
    result <- lapply(sheets, function(nm) {
      tryCatch({
        df        <- read_excel(path, sheet = nm)
        if (ncol(df) < 2) return(list(data = NULL, notes = paste0(
          "Salaryplan sheet '", nm, "' skipped: fewer than 2 columns.")))
        col_lower <- tolower(names(df))
        month_col <- which(str_detect(col_lower, "month|datum|date"))[1]
        chf_col   <- which(str_detect(col_lower, "chf|amount|betrag|salary"))[1]
        psp_col   <- which(str_detect(col_lower, "psp|projekt|grant"))[1]
        role_col  <- which(str_detect(col_lower, "role|rolle"))[1]
        fte_col   <- which(str_detect(col_lower, "fte"))[1]
        if (is.na(month_col) || is.na(chf_col)) return(list(data = NULL, notes = paste0(
          "Salaryplan sheet '", nm, "' skipped: no Month/CHF column recognised.")))
        psp_vals  <- if (length(psp_col)  > 0 && !is.na(psp_col))  canonical_id(str_trim(as.character(df[[psp_col]])))  else rep(NA_character_, nrow(df))
        role_vals <- if (length(role_col) > 0 && !is.na(role_col)) str_trim(as.character(df[[role_col]]))                else rep("Other", nrow(df))
        fte_vals  <- if (length(fte_col)  > 0 && !is.na(fte_col))  as.numeric(df[[fte_col]])                             else rep(1, nrow(df))
        raw_month <- as.character(df[[month_col]])
        raw_chf   <- as.character(df[[chf_col]])
        tib <- tibble(
          name   = nm,
          month  = suppressWarnings(as_date(raw_month)),
          amount = suppressWarnings(as.numeric(str_remove_all(raw_chf, "[, ]"))),
          psp    = psp_vals,
          role   = role_vals,
          fte    = fte_vals
        )
        # a filled cell that failed to parse = silent data loss → report it
        bad <- (!is.na(raw_month) & raw_month != "" & is.na(tib$month)) |
               (!is.na(raw_chf)   & raw_chf   != "" & is.na(tib$amount))
        notes <- if (any(bad)) paste0(
          "Salaryplan '", nm, "': ", sum(bad), " row(s) DROPPED — unparseable ",
          "Month/CHF (first: Month='", raw_month[which(bad)[1]],
          "', CHF='", raw_chf[which(bad)[1]], "'). Use ISO dates (2026-10-01).")
        else character()
        list(data = tib |> filter(!is.na(month), !is.na(amount), amount > 0),
             notes = notes)
      }, error = function(e) list(data = NULL, notes = paste0(
        "Salaryplan sheet '", nm, "' failed to read: ", conditionMessage(e))))
    })
    out <- bind_rows(lapply(result, `[[`, "data"))
    if (nrow(out) > 0) {
      attr(out, "health") <- unlist(lapply(result, `[[`, "notes"))
      return(out)
    }
  }

  # Old/fallback: single wide sheet
  raw     <- read_excel(path, sheet = sheets[1])
  raw     <- raw[, !grepl("[.][.][.][0-9]+", names(raw)) & !is.na(names(raw)) & names(raw) != ""]
  headers <- names(raw)
  parsed  <- suppressWarnings(mdy(headers))
  if (all(is.na(parsed))) parsed <- suppressWarnings(as_date(as.numeric(headers), origin = "1899-12-30"))
  date_cols <- which(!is.na(parsed))
  if (length(date_cols) == 0) return(NULL)
  meta_cols <- seq_len(min(date_cols) - 1)
  col_lower <- tolower(str_trim(headers[meta_cols]))
  name_col  <- meta_cols[str_detect(col_lower, "name|person") | seq_along(col_lower) == 1][1]
  fte_col   <- meta_cols[str_detect(col_lower, "fte|%")]
  psp_col   <- meta_cols[str_detect(col_lower, "psp|projekt|grant")]
  role_col  <- meta_cols[str_detect(col_lower, "role|rolle")]
  raw$..name <- as.character(raw[[name_col]])
  raw$..fte  <- if (length(fte_col) > 0) {
    v <- as.numeric(str_remove(as.character(raw[[fte_col[1]]]), "%"))
    ifelse(v > 1, v / 100, v)
  } else 1
  raw$..psp  <- if (length(psp_col) > 0) canonical_id(as.character(raw[[psp_col[1]]])) else NA_character_
  raw$..role <- if (length(role_col) > 0) str_trim(as.character(raw[[role_col[1]]])) else "Other"
  raw |>
    filter(!is.na(..name), ..name != "", ..name != "NA") |>
    select(name = ..name, fte = ..fte, psp = ..psp, role = ..role, all_of(date_cols)) |>
    setNames(c("name", "fte", "psp", "role", as.character(parsed[date_cols]))) |>
    pivot_longer(-c(name, fte, psp, role), names_to = "month", values_to = "amount") |>
    mutate(month  = as_date(month),
           amount = suppressWarnings(as.numeric(str_remove_all(as.character(amount), "[, ]")))) |>
    filter(!is.na(month), !is.na(amount), amount > 0)
}


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
  if (!all(c("fallig", "betrag") %in% names(df))) {
    out <- tibble(id = id, date = dummy_date, planned_income = 0)
    attr(out, "health") <- paste0("Zahlungsplan '", id,
      "': no Fällig/Betrag columns recognised — tab treated as empty.")
    return(out)
  }
  # Dates may arrive as real Excel Date objects OR as user-typed strings in
  # day-first format (rhandsontable displays Dates as "13/1/2026", which users
  # then mimic when typing new rows). Try ISO first, fall back to dmy().
  parsed <- df |>
    rename(date_raw = fallig) |>
    mutate(id = id,
           planned_income = parse_number(as.character(betrag),
                                         locale = locale(decimal_mark = ".", grouping_mark = ",")),
           date_iso = suppressWarnings(as_date(date_raw)),
           date_dmy = suppressWarnings(dmy(as.character(date_raw))),
           date     = if_else(is.na(date_iso), date_dmy, date_iso))
  # a filled cell that failed to parse = silent data loss → report it
  bad <- (!is.na(parsed$date_raw) & as.character(parsed$date_raw) != "" & is.na(parsed$date)) |
         (!is.na(parsed$betrag)   & as.character(parsed$betrag)   != "" & is.na(parsed$planned_income))
  notes <- if (any(bad)) paste0(
    "Zahlungsplan '", id, "': ", sum(bad), " row(s) DROPPED — unparseable ",
    "Fällig/Betrag (first: Fällig='", as.character(parsed$date_raw)[which(bad)[1]],
    "', Betrag='", as.character(parsed$betrag)[which(bad)[1]], "').")
  else character()
  out <- parsed |>
    transmute(id, date, planned_income) |>
    filter(!is.na(date)) |>
    group_by(id, date) |>
    summarise(planned_income = sum(planned_income, na.rm = TRUE), .groups = "drop")
  if (nrow(out) == 0) out <- tibble(id = id, date = dummy_date, planned_income = 0)
  attr(out, "health") <- notes
  out
}

safe_max_date <- function(x) { x <- x[!is.na(x)]; if (length(x) == 0) NA_Date_ else max(x) }


# ================================================================
# Spending categories
# ================================================================
# Rules are matched on (kurztext, buchungstext). buchungstext is a regex:
#   NA              → "match any buchungstext" (kurztext-only fallback)
#   "^Foo$"         → exact match
#   "^Foo"          → starts with "Foo"
#   "Foo"           → contains "Foo" anywhere
# Specific rules (non-NA buchungstext) override the NA fallback for the
# same kurztext. Anything not matched falls through to "Other".
CATEGORY_RULES <- tibble::tribble(
  ~kurztext,                 ~buchungstext,                ~category,

  # Salary
  "Lohnaufwand",             NA,                           "Salary",
  "Personalkosten",          NA,                           "Salary",
  "Pensionskasse",           NA,                           "Salary",
  "SUVA",                    NA,                           "Salary",
  "ALV",                     NA,                           "Salary",
  "AHV",                     NA,                           "Salary",
  "FAK-Beiträge",            NA,                           "Salary",
  "Fam.zul.",                NA,                           "Salary",
  "Verwalt.aufw.PUBLICA",    NA,                           "Salary",

  # Consumables
  "Laborwaren",              NA,                           "Consumables",
  "Biol. Präp.& Chemika",    NA,                           "Consumables",
  "Verbrauchsmaterial",      NA,                           "Consumables",
  "Üb. Materialaufwand",     NA,                           "Consumables",
  "EDV-Verbrauchsmat.",      NA,                           "Consumables",
  "IT-Verbrauchsmat.",       NA,                           "Consumables",
  "Halb-&Fertigpdt.Komp",    NA,                           "Consumables",
  "Labortiere/Tierhaltg",    "^Mouse Breeding Taconic$",   "Taconic",
  "Labortiere/Tierhaltg",    "^Weiterbelastung",           "Animal purchase",
  "Labortiere/Tierhaltg",    NA,                           "Consumables",

  # Equipment
  "Maschinen, Geräte",       NA,                           "Equipment",
  "Geräte, Maschinen",       NA,                           "Equipment",
  "Hardware bis 10'000",     NA,                           "Equipment",
  "Wäscheautomaten",         NA,                           "Equipment",
  "Unterh, Rep Mobilien",    NA,                           "Equipment",
  "Mobiliar & Einricht.",    NA,                           "Equipment",

  # IT, Office & Publications
  "Software",                NA,                           "IT, Office & Publications",
  "Software (nicht akt)",    NA,                           "IT, Office & Publications",
  "IT und Telekomm.",        NA,                           "IT, Office & Publications",
  "Monographien",            NA,                           "IT, Office & Publications",
  "Drucksachen, Repro",      NA,                           "IT, Office & Publications",
  "Büromaterial",            NA,                           "IT, Office & Publications",

  # Travel, Events & Training
  "Flugreisen",              NA,                           "Travel, Events & Training",
  "Bahn, ÖV-Mittel",         NA,                           "Travel, Events & Training",
  "Unterkunft",              NA,                           "Travel, Events & Training",
  "Reisekostenzurückerst.",  NA,                           "Travel, Events & Training",
  "Sachtransporte",          NA,                           "Travel, Events & Training",
  "Kurier,Frachten",         NA,                           "Travel, Events & Training",
  "Seminare u. Tagungen",    NA,                           "Travel, Events & Training",
  "Aus- und Weiterbild.",    NA,                           "Travel, Events & Training",
  "Aus- & Weiterbildung",    NA,                           "Travel, Events & Training",
  "ETH-interne Anlässe",     NA,                           "Travel, Events & Training",
  "Repräsentationspesen",    NA,                           "Travel, Events & Training",

  # Facility costs — split by Buchungstext into FACS / EPIC / ScopeM,
  # with the NA fallback catching anything unmatched. "Staff Support ASS"
  # is a salary carve-out (booked under ILV TPF but is personnel cost).
  "ILV TPF bud.r.Ko-Ver",    "^Staff Support ASS$",        "Salary",
  "ILV TPF bud.r.Ko-Ver",    "^Staff Support FACS$",       "FACS",
  "ILV TPF bud.r.Ko-Ver",    "^high-end analyzer$",        "FACS",
  "ILV TPF bud.r.Ko-Ver",    "^Confocal$",                 "ScopeM",
  "ILV TPF bud.r.Ko-Ver",    "^Staff$",                    "ScopeM",
  "ILV TPF bud.r.Ko-Ver",    "^Sample Preparation Tool$",  "ScopeM",
  "ILV TPF bud.r.Ko-Ver",    "^TypIIL mouse$",             "EPIC",
  "ILV TPF bud.r.Ko-Ver",    "^Biopsie$",                  "EPIC",
  "ILV TPF bud.r.Ko-Ver",    "^In Vitro Fertilisation$",   "EPIC",
  "ILV TPF bud.r.Ko-Ver",    "^Embryotransfer$",           "EPIC",
  "ILV TPF bud.r.Ko-Ver",    "^Staff Support 1: Import$",  "EPIC",
  "ILV TPF bud.r.Ko-Ver",    "^Medikamente HPL$",          "EPIC",
  "ILV TPF bud.r.Ko-Ver",    NA,                           "Facility costs",

  # Internal charges
  "Übr. DL ETH-nah.Einh",    NA,                           "Internal charges",
  "Kostenübernahme",         NA,                           "Internal charges",
  "Gebühren",                NA,                           "Internal charges",
  "Wirtschaftsor. Fors.",    NA,                           "Internal charges",
  "Schenkungen",             NA,                           "Internal charges",
  "Personalrekrutierung",    NA,                           "Internal charges",

  # Year-end transfer Kostenstelle → Forschungsreserve: money moved between
  # own pots, not spent — kept out of the consumables-per-FTE calibration
  # (see consumables_per_fte_month).
  "Reserve Jahresabr",       NA,                           "Transfer Forschungsreserve",

  # Other
  "mehrere",                 NA,                           "Other"
)

classify_category <- function(kurztext_vec, buchungstext_vec, rules = CATEGORY_RULES) {
  # SAP exports occasionally pad labels with stray whitespace; the rules match
  # kurztext by exact equality, so normalise before comparing.
  kurztext_vec <- str_squish(kurztext_vec)
  out  <- rep("Other", length(kurztext_vec))
  buch <- str_squish(ifelse(is.na(buchungstext_vec), "", buchungstext_vec))
  general  <- rules[is.na(rules$buchungstext), , drop = FALSE]
  specific <- rules[!is.na(rules$buchungstext), , drop = FALSE]
  for (i in seq_len(nrow(general))) {
    out[!is.na(kurztext_vec) & kurztext_vec == general$kurztext[i]] <- general$category[i]
  }
  for (i in seq_len(nrow(specific))) {
    mask <- !is.na(kurztext_vec) &
            kurztext_vec == specific$kurztext[i] &
            str_detect(buch, specific$buchungstext[i])
    out[mask] <- specific$category[i]
  }
  out
}

# Palette borrowed from ggsci scientific-journal sets (JCO / NPG) plus a
# couple from D3 category10. Hand-picked to keep the three "facility" cool
# tones distinct (Consumables teal vs ScopeM cyan vs Facility costs cadet)
# and the three warm tones distinct (Animal purchase tan vs Equipment
# orange vs EPIC gold).
CATEGORY_COLORS <- c(
  "Salary"                    = "#0073C2",  # JCO blue
  "Consumables"               = "#00A087",  # NPG teal-green
  "Taconic"                   = "#7E6148",  # NPG saddle brown
  "Animal purchase"           = "#B09C85",  # NPG tan (paired with Taconic)
  "Equipment"                 = "#FF7F0E",  # D3 orange
  "IT, Office & Publications" = "#9467BD",  # D3 purple
  "Travel, Events & Training" = "#E64B35",  # NPG red
  "FACS"                      = "#F39B7F",  # NPG peach
  "EPIC"                      = "#EFC000",  # JCO gold
  "ScopeM"                    = "#4DBBD5",  # NPG cyan
  "Facility costs"            = "#5F9EA0",  # cadet blue (residual)
  "Internal charges"          = "#828282",  # gray
  "Transfer Forschungsreserve"= "#E377C2",  # D3 pink — deliberately loud: money moved, not spent
  "Other"                     = "#B4AA96"   # tan
)

CATEGORY_ORDER <- c("Salary","Consumables","Taconic","Animal purchase","Equipment",
                    "IT, Office & Publications","Travel, Events & Training",
                    "FACS","EPIC","ScopeM","Facility costs",
                    "Internal charges","Transfer Forschungsreserve","Other")

# ================================================================
# EP account aliasing
# ================================================================
# SAP sometimes books on the PSP-element form of an account
# ("5-029870-000" for Kostenstelle 29870). After canonical_id that survives
# as "5-029870", which matches no konto — the booking counts in lab-wide
# totals but leaks out of every per-PSP view and header. If stripping the
# "<digit>-0…" prefix of an unmatched EP id yields a known konto, remap it,
# and report the alias so nothing happens silently.
resolve_ep_id_aliases <- function(ist_raw, konten) {
  unmatched <- setdiff(unique(ist_raw$id[!is.na(ist_raw$id)]), konten$id)
  alias <- tibble(from = unmatched) |>
    mutate(to = str_replace(from, "^\\d-0*", "")) |>
    filter(from != to, to %in% konten$id)
  if (nrow(alias) == 0) return(list(ist_raw = ist_raw, notes = character()))
  ist_raw <- ist_raw |>
    left_join(alias, by = c("id" = "from")) |>
    mutate(id = coalesce(to, id)) |>
    select(-to)
  list(ist_raw = ist_raw,
       notes = paste0("Einzelposten: account '", alias$from,
                      "' read as konto '", alias$to,
                      "' (PSP-element form of the same account)."))
}

# ================================================================
# Year-end Reserve transfer mirroring
# ================================================================
# SAP books the year-end settlement ("Reserve Jahresabr") only ONCE: as a
# negative amount (= income) on the Forschungsreserve. The Kostenstelle side
# gets no expenditure row — its saldo is just zeroed inside SAP — so without
# correction the app would carry the unspent remainder as surplus forever
# AND count the Reserve income on top. Mirror every transfer income row as a
# synthetic expenditure on the source Kostenstelle (parsed from the
# Buchungstext "Jahresabrechnung KST <nr>"; falls back to the single
# Kostenstelle in Konten). The lateral move then nets to zero everywhere.
mirror_reserve_transfers <- function(ist_raw, konten) {
  notes    <- character()
  kost_ids <- konten |> filter(typ == "Kostenstelle") |> pull(id)
  transfers <- ist_raw |> filter(category == "Transfer Forschungsreserve")
  incoming  <- transfers |> filter(actual_income > 0)
  if (nrow(incoming) == 0) return(list(ist_raw = ist_raw, notes = notes))
  # If SAP ever starts booking the expenditure side itself, mirroring would
  # double it — bail out visibly instead.
  if (any(transfers$actual_spending > 0)) {
    return(list(ist_raw = ist_raw, notes = paste0(
      "Reserve transfer: the EP already contains expenditure-side booking(s) ",
      "for 'Reserve Jahresabr' — no synthetic Kostenstelle expenditure added. ",
      "Check that the transfer is not double-counted.")))
  }
  incoming <- incoming |>
    mutate(
      kst_parsed = canonical_id(str_match(buchungstext, "(?i)KST\\s*([A-Za-z0-9-]+)")[, 2]),
      target_id  = case_when(
        !is.na(kst_parsed) & kst_parsed %in% kost_ids ~ kst_parsed,
        length(kost_ids) == 1                         ~ kost_ids[1],
        TRUE                                          ~ NA_character_
      )
    )
  unmatched <- incoming |> filter(is.na(target_id))
  if (nrow(unmatched) > 0)
    notes <- c(notes, paste0(
      "Reserve transfer '", unmatched$buchungstext, "' (",
      format(unmatched$month, "%Y-%m"), "): source Kostenstelle not identified — ",
      "NO expenditure mirrored; the amount stays double in the books. ",
      "Check the Buchungstext / Konten IDs."))
  mirrored <- incoming |>
    filter(!is.na(target_id)) |>
    mutate(id              = target_id,
           actual_spending = actual_income,
           actual_income   = 0,
           betrag_in_bw    = actual_spending,
           buchungstext    = paste0(buchungstext, " (mirrored expenditure)")) |>
    select(-kst_parsed, -target_id)
  if (nrow(mirrored) > 0)
    notes <- c(notes, paste0(
      "Reserve transfer mirrored: ", format(round(mirrored$actual_spending), big.mark = "'"),
      " CHF booked as expenditure on Kostenstelle '", mirrored$id, "' (",
      format(mirrored$month, "%Y-%m"), ") — SAP books only the Reserve side."))
  list(ist_raw = bind_rows(ist_raw, mirrored), notes = notes)
}

# ================================================================
# Load all static data from dirname(ep_path)
# ================================================================
load_all_data <- function(ep_path) {

  raw_dir <- dirname(ep_path)

  # --- Einzelposten (read first so we can bootstrap other files from it) ---
  ist_raw <- read_excel(ep_path) |>
    clean_names() |>
    rename_with(~ str_replace_all(., "\\.", "_"))
  # SAP renamed "Betrag in BW" -> "Betrag in BukrsWährung" in 2026; map back so the rest of the code is unchanged.
  if (!"betrag_in_bw" %in% names(ist_raw)) {
    amt_col <- grep("^betrag_in_", names(ist_raw), value = TRUE)
    if (length(amt_col) == 1) ist_raw <- rename(ist_raw, betrag_in_bw = !!amt_col)
  }
  if (!"buchungstext" %in% names(ist_raw)) ist_raw$buchungstext <- NA_character_
  ist_raw <- ist_raw |>
    mutate(
      buch_dat     = as_date(buch_dat),
      betrag_in_bw = as.numeric(betrag_in_bw),
      id           = canonical_id(kontierung),
      month        = floor_date(buch_dat, "month"),
      actual_income   = if_else(betrag_in_bw < 0, -betrag_in_bw, 0),
      actual_spending = if_else(betrag_in_bw > 0,  betrag_in_bw, 0),
      category        = classify_category(kurztext, buchungstext)
    )

  # EP rows without a parseable date, amount or account are invisible to every
  # sum and plot — report them instead of losing them silently.
  ep_bad <- ist_raw |> filter(is.na(month) | is.na(betrag_in_bw) | is.na(id))
  ep_health <- if (nrow(ep_bad) > 0) paste0(
    "Einzelposten: ", nrow(ep_bad), " row(s) IGNORED — missing/unparseable ",
    "Buch_Dat, Betrag or Kontierung (first: Kurztext='", ep_bad$kurztext[1],
    "', Buch_Dat='", as.character(ep_bad$buch_dat[1]),
    "', Betrag='", ep_bad$betrag_in_bw[1], "'). These rows are in the export ",
    "but count in NO total or plot.") else character()

  ep_ids <- sort(unique(ist_raw$id[!is.na(ist_raw$id)]))

  # --- Konten ---
  konten_path <- file.path(raw_dir, "Konten.xlsx")
  konten_bootstrapped <- !file.exists(konten_path)

  if (!file.exists(konten_path)) {
    message("Konten.xlsx not found — bootstrapping from EP.")
    bootstrap_konten <- data.frame(
      ID           = ep_ids,
      Bezeichnung  = NA_character_,
      Typ          = NA_character_,
      Laufzeit_von = NA_character_,
      Laufzeit_bis = NA_character_,
      stringsAsFactors = FALSE
    )
    writexl::write_xlsx(list(Konten = bootstrap_konten), konten_path)
  }

  konten <- read_excel(konten_path) |>
    clean_names() |>
    mutate(
      id           = canonical_id(id),
      bezeichnung  = trimws(as.character(coalesce(bezeichnung, NA_character_))),
      laufzeit_bis = trimws(as.character(laufzeit_bis)),
      laufzeit_date = case_when(
        tolower(laufzeit_bis) == "unendlich" ~ NA_Date_,
        suppressWarnings(!is.na(as_date(laufzeit_bis))) ~
          suppressWarnings(as_date(laufzeit_bis)),
        suppressWarnings(!is.na(as.numeric(laufzeit_bis))) ~
          suppressWarnings(as_date(as.numeric(laufzeit_bis), origin = "1899-12-30")),
        TRUE ~ NA_Date_
      ),
      typ = trimws(as.character(typ))
    ) |>
    filter(!is.na(id))

  # Erlöse kontos stay in the working list: their (small) revenue counts in
  # the lab-wide totals, and downstream UI filters keep them out of the
  # per-PSP selection lists. Kontos WITHOUT a Typ can't be classified at all —
  # exclude them like before, but report it (health, below) instead of
  # dropping silently.
  typless_ids <- konten |>
    filter(is.na(typ) | tolower(typ) %in% c("", "na")) |> pull(id)
  konten <- konten |> filter(!id %in% typless_ids)

  # only append skeleton rows when bootstrapping — never silently modify an existing Konten
  if (konten_bootstrapped) {
    missing_in_konten <- ep_ids[!ep_ids %in% konten$id]
    if (length(missing_in_konten) > 0) {
      skeleton <- tibble(id = missing_in_konten, bezeichnung = NA_character_,
                         laufzeit_bis = NA_character_, laufzeit_date = NA_Date_, typ = NA_character_)
      konten <- bind_rows(konten, skeleton)
    }
  }

  # --- Resolve PSP-element ID variants against Konten ---
  # (needs konten; must run before ist_monthly and the transfer mirroring)
  aliased <- resolve_ep_id_aliases(ist_raw, konten)
  ist_raw <- aliased$ist_raw
  ep_ids  <- sort(unique(ist_raw$id[!is.na(ist_raw$id)]))

  # --- Mirror year-end Reserve transfers as Kostenstelle expenditure ---
  # (needs konten for the Typ lookup; must run before ist_monthly is built)
  mirrored_transfers <- mirror_reserve_transfers(ist_raw, konten)
  ist_raw            <- mirrored_transfers$ist_raw

  # --- Zahlungsplan ---
  zp_path <- file.path(raw_dir, "Zahlungsplan.xlsx")

  if (!file.exists(zp_path)) {
    message("Zahlungsplan.xlsx not found — creating empty file.")
    empty_df  <- data.frame(Fallig = character(), Betrag = character(),
                             Bezeichnung = character(), stringsAsFactors = FALSE)
    grant_ids <- konten |> filter(!tolower(typ) %in% c("kostenstelle","reserve","erlöse","startup")) |> pull(id)
    if (length(grant_ids) == 0) grant_ids <- ep_ids
    zp_bootstrap <- setNames(lapply(grant_ids, function(x) empty_df), grant_ids)
    writexl::write_xlsx(zp_bootstrap, zp_path)
  }

  zp_sheets <- excel_sheets(zp_path)
  zp_files_list <- lapply(zp_sheets, function(sh) {
    df  <- read_excel(zp_path, sheet = sh)
    tmp <- tempfile(pattern = paste0("Zahlungsplan_PSP_", sh, "_"), fileext = ".xlsx")
    writexl::write_xlsx(df, tmp)
    tmp
  })
  zp_reads  <- lapply(zp_files_list, read_zahlungsplan, dummy_date = as_date("2025-08-01"))
  zp_health <- unlist(lapply(zp_reads, attr, "health"))
  zahlungsplan <- bind_rows(zp_reads) |>
    group_by(id, date) |>
    summarise(planned_income = sum(planned_income, na.rm = TRUE), .groups = "drop")

  # --- Salary plan ---
  sal_path <- file.path(raw_dir, "Salaryplan.xlsx")
  if (!file.exists(sal_path)) {
    message("Salaryplan.xlsx not found — creating empty file.")
    writexl::write_xlsx(
      list(Template = data.frame(Month=character(), CHF=numeric(), PSP=character(),
                                  Role=character(), FTE=numeric(), stringsAsFactors=FALSE)),
      sal_path
    )
  }

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

  salary_plan <- tryCatch(load_salaryplan(sal_path), error = function(e) { message("Salaryplan load error: ", e$message); NULL })

  # raw konten for editing — normalize columns so dates display consistently
  # Columns may be: pure numeric (Excel serial dates), mixed numeric+text (laufzeit_bis has "unendlich"), or text
  konten_raw <- read_excel(file.path(raw_dir, "Konten.xlsx"), col_types = "text") |>
    mutate(across(-1, ~ {
      x <- .x
      # try converting numeric-looking values to dates, leave text (e.g. "unendlich") as-is
      ifelse(
        !is.na(suppressWarnings(as.numeric(x))) & suppressWarnings(as.numeric(x)) > 10000,
        suppressWarnings(as.character(as_date(as.numeric(x), origin = "1899-12-30"))),
        x
      )
    })) |>
    as.data.frame()

  # --- Investments ---
  inv_path <- file.path(raw_dir, "Investments.xlsx")
  if (!file.exists(inv_path)) {
    writexl::write_xlsx(
      list(Investments = data.frame(Date=character(), Amount=numeric(), Description=character(),
                                    PSP=character(), Category=character(), stringsAsFactors=FALSE)),
      inv_path)
  }
  empty_investments <- tibble(month=as_date(character()), amount=numeric(),
                              desc=character(), psp=character(), cat=character())
  investments <- tryCatch({
    df <- read_excel(inv_path, col_types = "text")
    if (nrow(df) == 0) {
      empty_investments
    } else {
      names(df) <- c("Date","Amount","Description","PSP","Category")
      df |>
        mutate(
          # handle both Excel serial numbers and "YYYY-MM-DD" strings
          Date = ifelse(!is.na(suppressWarnings(as.numeric(Date))) &
                          suppressWarnings(as.numeric(Date)) > 10000,
                        as.character(as_date(as.numeric(Date), origin = "1899-12-30")),
                        Date),
          month  = suppressWarnings(floor_date(as_date(Date), "month")),
          amount = suppressWarnings(as.numeric(Amount)),
          psp    = canonical_id(as.character(PSP)),
          desc   = as.character(Description),
          cat    = as.character(Category)
        ) |>
        filter(!is.na(month), !is.na(amount), amount > 0) |>
        select(month, amount, desc, psp, cat)
    }
  }, error = function(e) empty_investments)

  # --- Data health: everything that was dropped or doesn't cross-match ------
  # Collected here and surfaced on the Load Data tab so silent data loss
  # becomes visible instead of quietly skewing forecasts.
  health <- c(ep_health, aliased$notes, zp_health, attr(salary_plan, "health"),
              mirrored_transfers$notes)

  zp_unmatched <- setdiff(unique(zahlungsplan$id), konten$id)
  if (length(zp_unmatched) > 0)
    health <- c(health, paste0("Zahlungsplan tab '", zp_unmatched,
      "' matches no konto in Konten.xlsx — income counts in Forecast (Total) ",
      "but the PSP has no monitoring/forecast of its own. Check the ID."))

  if (!is.null(salary_plan) && nrow(salary_plan) > 0) {
    sp_unmatched <- setdiff(unique(salary_plan$psp[!is.na(salary_plan$psp)]), konten$id)
    if (length(sp_unmatched) > 0)
      health <- c(health, paste0("Salaryplan PSP '", sp_unmatched,
        "' matches no konto — these salary months count in Forecast (Total) ",
        "but in no per-PSP forecast. Check the ID."))
  }

  # EP bookings on an account that matches no konto: they count in the
  # lab-wide totals but in NO per-PSP view/header — usually a variant ID form
  # in the export (leading zeros, suffix) that canonical_id doesn't map.
  ep_unmatched <- setdiff(ep_ids, c(konten$id, typless_ids))
  if (length(ep_unmatched) > 0) {
    unm <- ist_raw |> filter(id %in% ep_unmatched) |>
      group_by(id) |>
      summarise(net = sum(actual_spending - actual_income, na.rm = TRUE),
                n = n(), .groups = "drop")
    health <- c(health, paste0("Einzelposten: ", unm$n, " booking(s) on '",
      unm$id, "' (net ", format(round(unm$net), big.mark = "'"),
      " CHF) match no konto in Konten.xlsx — they count in the lab-wide ",
      "totals but in NO per-PSP monitoring, forecast or header. If this ",
      "should be an existing konto, the ID form in the export differs."))
  }

  # A konto that is the PSP-element form of another konto (e.g. '5-029870'
  # next to '29870') splits one real account's bookings across two kontos —
  # the alias remap skips known kontos, so this must be flagged to the user.
  stripped_ids <- str_replace(konten$id, "^\\d-0*", "")
  looks_alias  <- konten$id[stripped_ids != konten$id & stripped_ids %in% konten$id]
  if (length(looks_alias) > 0)
    health <- c(health, paste0("Konto '", looks_alias,
      "' looks like the PSP-element form of konto '",
      str_replace(looks_alias, "^\\d-0*", ""), "'. If it is the SAME account, ",
      "delete the '", looks_alias, "' row in Konten — its bookings are ",
      "currently counted as a separate konto instead of merging."))

  no_bookings <- setdiff(konten$id, unique(ist_monthly$id))
  if (length(no_bookings) > 0)
    health <- c(health, paste0("Konto '", no_bookings,
      "' has no bookings in the Einzelposten yet (new grant?) — its past shows as zero."))

  if (length(typless_ids) > 0)
    health <- c(health, paste0("Konto '", typless_ids,
      "' has no Typ in Konten.xlsx — it is IGNORED everywhere until you set one ",
      "(Grant, Kostenstelle, Startup, Reserve or Erlöse)."))

  list(konten = konten, konten_raw = konten_raw,
       zahlungsplan = zahlungsplan_combined,
       expected_burn = expected_burn_df, ist_monthly = ist_monthly,
       planned_income_m = planned_income_m, reference_date = reference_date,
       ist_raw = ist_raw, lohntabelle = lohntabelle, salary_plan = salary_plan,
       investments = investments, raw_dir = raw_dir, health = health)
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
        # Kostenstelle: EP credits (negative bookings) are real income on top
        # of the ZP budget — SAP includes them in the year-end sweep, so the
        # balance must too, or December ends below zero by exactly that amount.
        psp_id %in% kostenstelle_ids ~ ave(planned_income + actual_income, year, FUN = cumsum) -
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
# Consumables-per-FTE rate (single source of truth)
# ================================================================
# Historical non-salary, non-EPIC spend per FTE per month, calibrated GLOBALLY
# (all grants, Startup excluded) over the burn window. EPIC (animal facility)
# is excluded because it is a background cost largely independent of FTE — it
# gets its own forecast line (see epic_monthly_avg). "Transfer Forschungs-
# reserve" (year-end Kostenstelle→Reserve transfer) is excluded because it is
# money moved between own pots, not consumed. This rate is what the
# sidebar suggests as CHF/FTE/year; the sidebar value (suggested or manually
# overridden) is exactly what the forecast multiplies future FTE by.
#
# past_fte uses the UNFILTERED plan (salary_plan_full) so person include/exclude
# toggles do not retroactively shrink the past team that actually generated the
# spend. The plan only holds rows with amount > 0 (see load_salaryplan), so each
# person counts only for months they were present/paid.
consumables_per_fte_month <- function(d, burn_window_months) {
  today_month <- as_date(d$reference_date)
  sp_full     <- d$salary_plan_full %||% d$salary_plan
  startup_ids <- d$konten |> filter(typ == "Startup") |> pull(id)

  ist_f <- d$ist_raw |> filter(!id %in% startup_ids)
  win_start <- today_month %m-% months(burn_window_months)
  nonsalary_total <- tryCatch(
    ist_f |> filter(month > win_start, month <= today_month,
                    actual_spending > 0,
                    !category %in% c("Salary", "EPIC", "Transfer Forschungsreserve")) |>
      summarise(s = sum(actual_spending, na.rm = TRUE)) |> pull(s),
    error = function(e) 0
  )

  past_fte <- if (!is.null(sp_full) && nrow(sp_full) > 0) {
    v <- sp_full |>
      mutate(month = as_date(month)) |>
      filter(month <= today_month) |>
      group_by(month) |>
      summarise(fte_total = sum(fte, na.rm = TRUE), .groups = "drop") |>
      arrange(desc(month)) |> slice_head(n = burn_window_months) |>
      summarise(avg = mean(fte_total, na.rm = TRUE)) |> pull(avg)
    if (length(v) == 0 || is.na(v)) 1 else v
  } else 1

  list(
    per_fte_month   = nonsalary_total / (burn_window_months * max(past_fte, 0.01)),
    nonsalary_total = nonsalary_total,
    past_fte        = past_fte
  )
}

# ================================================================
# EPIC background cost (animal facility)
# ================================================================
# Average monthly EPIC spend over the burn window (bills arrive quarterly, so
# the window average smooths them). Per-PSP when psp_id given, lab-wide
# otherwise. Like the consumables rate, this is the SUGGESTION shown in the
# forecast sidebar; the sidebar value is what actually enters the forecast.
epic_monthly_avg <- function(d, burn_window_months, psp_id = NULL) {
  today_month <- as_date(d$reference_date)
  win_start   <- today_month %m-% months(burn_window_months)
  ist_f <- d$ist_raw |> filter(category == "EPIC")
  if (!is.null(psp_id)) ist_f <- ist_f |> filter(id == psp_id)
  tot <- tryCatch(
    ist_f |> filter(month > win_start, month <= today_month, actual_spending > 0) |>
      summarise(s = sum(actual_spending, na.rm = TRUE)) |> pull(s),
    error = function(e) 0
  )
  tot / burn_window_months
}

# ================================================================
# Salary-plan based cost (replaces compute_extra_cost)
# ================================================================
# consumables_rate_month: CHF per FTE per month. NULL = auto-calibrate (the
# same global rate the sidebar suggests); 0 = consumables toggled off.
compute_salary_cost <- function(months_vec, d, burn_window_months, psp_id = NULL,
                                consumables_rate_month = NULL) {
  sp <- d$salary_plan

  # force all months to Date for reliable matching
  months_vec_d <- as_date(months_vec)

  if (!is.null(sp) && nrow(sp) > 0) {
    sp_f <- sp |> mutate(month = as_date(month))
    if (!is.null(psp_id)) sp_f <- sp_f |> filter(psp == psp_id)

    sal_by_month <- sp_f |>
      filter(month %in% months_vec_d) |>
      group_by(month) |>
      summarise(salary = sum(amount, na.rm = TRUE), .groups = "drop")

    fte_by_month <- sp_f |>
      filter(month %in% months_vec_d) |>
      group_by(month) |>
      summarise(fte_total = sum(fte, na.rm = TRUE), .groups = "drop")
  } else {
    sal_by_month <- tibble(month = as_date(character()), salary = numeric())
    fte_by_month <- tibble(month = as_date(character()), fte_total = numeric())
  }

  # Future salary/FTE come from the (possibly person-filtered) plan; the
  # consumables rate is either the explicit sidebar value or the shared
  # global calibration (identical to the sidebar suggestion).
  nonsalary_per_fte_month <- consumables_rate_month %||%
    consumables_per_fte_month(d, burn_window_months)$per_fte_month

  tibble(month = months_vec_d) |>
    left_join(sal_by_month, by = "month") |>
    left_join(fte_by_month, by = "month") |>
    mutate(
      salary    = replace_na(salary, 0),
      fte_total = replace_na(fte_total, 0),
      cost      = salary + fte_total * nonsalary_per_fte_month
    ) |>
    pull(cost)
}


interpolate_balance <- function(df, by = "week") {
  date_seq <- seq(min(df$month), max(df$month), by = by)
  tibble(month = date_seq) |>
    mutate(balance = approx(df$month, df$balance, xout = month)$y)
}

# ================================================================
# Forecast / runout plot (from 04_, minimal changes)
# ================================================================
# consumables_rate_month / epic_monthly: explicit sidebar values (0 = toggled
# off); NULL falls back to the auto-calibrated suggestion, so plot and sidebar
# can never disagree.
make_forecast_plot <- function(d, burn_window_months = 6,
                                exclude_ids = character(),
                                inflation_rate = 0,
                                consumables_rate_month = NULL,
                                epic_monthly = NULL) {
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

  # Past income: EP actual income for ALL accounts — grants, the Reserve
  # (year-end transfer, see mirror_reserve_transfers), and Kostenstelle
  # credits/refunds (the annual budget itself never appears as an EP income
  # row, so no double count) — plus the ZP budget for the Kostenstelle.
  # Reserve ZP entries are ignored so a manually typed transfer cannot
  # double-count on top of the EP booking.
  past_income_ist <- ist_ns |>
    group_by(month) |>
    summarise(income_ist = sum(actual_income, na.rm = TRUE), .groups = "drop")

  kostenstelle_income <- planned_income_m |>
    filter(id %in% kostenstelle_ids) |>
    group_by(month) |>
    summarise(kostenstelle_income = sum(planned_income, na.rm = TRUE), .groups = "drop")

  past_spending <- ist_ns |>
    group_by(month) |>
    summarise(spending_ist = sum(actual_spending, na.rm = TRUE), .groups = "drop")

  past_start <- suppressWarnings(
    min(c(past_income_ist$month, kostenstelle_income$month, past_spending$month), na.rm = TRUE))
  if (is.infinite(past_start)) past_start <- today_month
  past_months <- tibble(month = seq(past_start, today_month, by = "1 month"))

  ts_past <- past_months |>
    left_join(past_income_ist,    by = "month") |>
    left_join(kostenstelle_income,by = "month") |>
    left_join(past_spending,      by = "month") |>
    mutate(across(c(income_ist, kostenstelle_income, spending_ist), ~ replace_na(.x, 0)),
           total_income   = income_ist + kostenstelle_income,
           total_spending = spending_ist,
           balance        = cumsum(total_income - total_spending))

  current_surplus <- last(ts_past$balance)

  # Burn average: the mirrored Reserve transfer counts in the balance (it
  # leaves the Kostenstelle) but is money moved, not consumed — keep it out
  # of the historical burn rate.
  transfer_out_m <- d$ist_raw |>
    filter(category == "Transfer Forschungsreserve", actual_spending > 0,
           !id %in% exclude_all) |>
    group_by(month) |>
    summarise(transfer_out = sum(actual_spending, na.rm = TRUE), .groups = "drop")

  burn_ref <- ts_past |>
    left_join(transfer_out_m, by = "month") |>
    mutate(spending_burn = total_spending - replace_na(transfer_out, 0)) |>
    filter(month < today_month) |>
    arrange(desc(month)) |>
    slice_head(n = burn_window_months) |>
    summarise(avg = mean(spending_burn, na.rm = TRUE)) |>
    pull(avg)
  if (!is.finite(burn_ref)) burn_ref <- 0  # no past spending months at all

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

  future_months_vec  <- seq(start_future, horizon_end, by = "1 month")
  salary_cost        <- compute_salary_cost(future_months_vec, d, burn_window_months, psp_id = NULL,
                                            consumables_rate_month = consumables_rate_month)
  nonsalary_burn     <- if (!is.null(d$salary_plan)) 0 else burn_ref
  # EPIC only as its own line when the salary-plan model is active; the
  # burn_ref fallback is a total historical average that already contains it.
  epic_cost          <- if (is.null(d$salary_plan)) 0
                        else epic_monthly %||% epic_monthly_avg(d, burn_window_months)

  # investments: future one-off costs
  inv <- if (!is.null(d$investments) && nrow(d$investments) > 0)
    d$investments |> filter(month > today_month) |>
      group_by(month) |> summarise(inv_cost = sum(amount, na.rm=TRUE), .groups="drop")
  else tibble(month=as_date(character()), inv_cost=numeric())

  ts_future <- tibble(month = future_months_vec, salary_cost = salary_cost) |>
    left_join(future_income, by = "month") |>
    left_join(inv, by = "month") |>
    mutate(total_income   = replace_na(total_income, 0),
           inv_cost       = replace_na(inv_cost, 0),
           months_ahead   = as.numeric(interval(start_future, month) / months(1)),
           infl_mult      = (1 + inflation_rate / 100) ^ (months_ahead / 12),
           total_spending = (nonsalary_burn + salary_cost + epic_cost) * infl_mult + inv_cost,
           balance        = current_surplus + cumsum(total_income - total_spending)) |>
    select(-months_ahead, -infl_mult)

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
    if (!is.null(d$salary_plan))
      paste0(" | EPIC: ", format(round(epic_cost), big.mark = "'"), " CHF/m") else "",
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
  # Startup is excluded from the forecast, but its remaining saldo (planned
  # income minus spending to date — same convention as the Monitoring header)
  # is still spendable money, so show the end balance including it as well.
  startup_saldo <- if (length(startup_ids) > 0) {
    startup_income <- planned_income_m |>
      filter(id %in% startup_ids, month <= today_month) |>
      summarise(v = sum(planned_income, na.rm = TRUE)) |> pull(v)
    startup_spent  <- ist_monthly |>
      filter(id %in% startup_ids, month <= today_month) |>
      summarise(v = sum(actual_spending, na.rm = TRUE)) |> pull(v)
    startup_income - startup_spent
  } else 0
  end_incl_startup <- end_balance + startup_saldo
  end_label     <- paste0(if (end_balance >= 0) "Expected: +" else "Expected: ",
                          format(round(end_balance), big.mark = "'"),
                          " CHF\nat end of horizon (", format(end_month, "%b %Y"), ")",
                          if (length(startup_ids) > 0) paste0(
                            "\nincl. Startup saldo: ",
                            if (end_incl_startup >= 0) "+" else "",
                            format(round(end_incl_startup), big.mark = "'"), " CHF")
                          else "")
  end_label_df  <- tibble(month = end_month, value = end_balance,
                           label = end_label,
                           panel = factor("Balance", levels = c("Cashflow","Balance")))

  inv_bars <- ts_future |> filter(inv_cost > 0) |>
    transmute(month, value = inv_cost,
              panel = factor("Cashflow", levels = c("Cashflow","Balance")))

  ggplot() +
    geom_ribbon(data = ribbon_pos, aes(x = month, ymin = ymin, ymax = ymax),
                fill = "forestgreen", alpha = 0.25, inherit.aes = FALSE) +
    geom_ribbon(data = ribbon_neg, aes(x = month, ymin = ymin, ymax = ymax),
                fill = "firebrick", alpha = 0.25, inherit.aes = FALSE) +
    geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.35) +
    geom_vline(xintercept = start_future, linetype = "dotted", linewidth = 0.8) +
    geom_col(data = bars_all, aes(x = month, y = value, fill = series),
             position = "dodge", width = 25, alpha = 0.85) +
    geom_col(data = inv_bars, aes(x = month, y = value), fill = "#E15759",
             width = 18, alpha = 0.9, inherit.aes = FALSE) +
    geom_text(data = inv_bars, aes(x = month, y = value,
              label = paste0(round(value/1000), "k")),
              vjust = -0.4, size = 2.5, color = "#E15759",
              fontface = "bold", inherit.aes = FALSE) +
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
# consumables_rate_month / epic_monthly: explicit sidebar values (0 = toggled
# off); NULL falls back to the auto-calibrated suggestion (EPIC per-PSP here).
make_psp_forecast_plot <- function(psp_id, d, burn_window_months = 6, inflation_rate = 0,
                                   consumables_rate_month = NULL,
                                   epic_monthly = NULL) {
  konten          <- d$konten
  ist_monthly     <- d$ist_monthly
  planned_income_m<- d$planned_income_m
  today_month     <- d$reference_date

  info <- konten |> filter(id == psp_id)
  if (nrow(info) == 0) return(NULL)

  startup_ids      <- konten |> filter(typ == "Startup")      |> pull(id)
  kostenstelle_ids <- konten |> filter(typ == "Kostenstelle") |> pull(id)
  reserve_ids      <- konten |> filter(typ == "Reserve")      |> pull(id)

  # Reserve is NOT planned-funded: its income arrives as actual EP bookings
  # (year-end transfer, see mirror_reserve_transfers), not via Zahlungsplan.
  planned_funded <- psp_id %in% c(kostenstelle_ids, startup_ids)

  # Past
  ist_psp <- ist_monthly |> filter(id == psp_id)

  if (planned_funded) {
    past_income <- planned_income_m |> filter(id == psp_id, month <= today_month) |>
      rename(total_income = planned_income)
    # Kostenstelle: EP credits (negative bookings) are real income on top of
    # the ZP budget — count them, same as everywhere else.
    if (psp_id %in% kostenstelle_ids) {
      past_income <- bind_rows(
        past_income,
        ist_psp |> filter(month <= today_month, actual_income > 0) |>
          select(month, total_income = actual_income)) |>
        group_by(month) |>
        summarise(total_income = sum(total_income, na.rm = TRUE), .groups = "drop")
    }
  } else {
    past_income <- ist_psp |> filter(month <= today_month) |>
      select(month, total_income = actual_income)
  }

  past_spending <- ist_psp |> filter(month <= today_month) |>
    select(month, total_spending = actual_spending)

  # A PSP whose funding lies entirely in the future (e.g. a new grant with no
  # bookings yet) has no past rows at all — start the timeline at today instead.
  past_start <- suppressWarnings(
    min(c(past_income$month, past_spending$month), na.rm = TRUE))
  if (is.infinite(past_start)) past_start <- today_month
  past_months <- tibble(month = seq(past_start, today_month, by = "1 month"))

  ts_past <- past_months |>
    left_join(past_income,   by = "month") |>
    left_join(past_spending, by = "month") |>
    mutate(across(c(total_income, total_spending), ~ replace_na(.x, 0)),
           balance = cumsum(total_income - total_spending))

  current_surplus <- last(ts_past$balance)

  # Keep the mirrored Reserve transfer out of the burn average (money moved,
  # not consumed) — same treatment as in make_forecast_plot.
  transfer_out_m <- d$ist_raw |>
    filter(id == psp_id, category == "Transfer Forschungsreserve",
           actual_spending > 0) |>
    group_by(month) |>
    summarise(transfer_out = sum(actual_spending, na.rm = TRUE), .groups = "drop")

  burn_ref <- ts_past |>
    left_join(transfer_out_m, by = "month") |>
    mutate(spending_burn = total_spending - replace_na(transfer_out, 0)) |>
    filter(month < today_month) |>
    arrange(desc(month)) |>
    slice_head(n = burn_window_months) |>
    summarise(avg = mean(spending_burn, na.rm = TRUE)) |>
    pull(avg)
  if (!is.finite(burn_ref)) burn_ref <- 0  # no past spending months at all

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

  future_months_vec  <- seq(start_future, horizon_end, by = "1 month")
  salary_cost        <- compute_salary_cost(future_months_vec, d, burn_window_months, psp_id = psp_id,
                                            consumables_rate_month = consumables_rate_month)
  nonsalary_burn     <- if (!is.null(d$salary_plan)) 0 else burn_ref
  epic_cost          <- if (is.null(d$salary_plan)) 0
                        else epic_monthly %||% epic_monthly_avg(d, burn_window_months, psp_id)

  # investments for this PSP
  inv_psp <- if (!is.null(d$investments) && nrow(d$investments) > 0)
    d$investments |> filter(psp == psp_id, month > today_month) |>
      group_by(month) |> summarise(inv_cost = sum(amount, na.rm=TRUE), .groups="drop")
  else tibble(month=as_date(character()), inv_cost=numeric())

  ts_future <- tibble(month = future_months_vec, salary_cost = salary_cost) |>
    left_join(future_income, by = "month") |>
    left_join(inv_psp, by = "month") |>
    mutate(total_income   = replace_na(total_income, 0),
           inv_cost       = replace_na(inv_cost, 0),
           months_ahead   = as.numeric(interval(start_future, month) / months(1)),
           infl_mult      = (1 + inflation_rate / 100) ^ (months_ahead / 12),
           total_spending = (nonsalary_burn + salary_cost + epic_cost) * infl_mult + inv_cost,
           balance        = current_surplus + cumsum(total_income - total_spending)) |>
    select(-months_ahead, -infl_mult)

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
    if (!is.null(d$salary_plan))
      paste0(" | EPIC: ", format(round(epic_cost), big.mark = "'"), " CHF/m") else "",
    if (inflation_rate > 0) paste0(" | Inflation: ", inflation_rate, "%/yr") else "",
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

  inv_bars_psp <- inv_psp |> filter(inv_cost > 0) |>
    transmute(month, value = inv_cost,
              panel = factor("Cashflow", levels = c("Cashflow","Balance")))

  ggplot() +
    geom_ribbon(data = ribbon_pos, aes(x = month, ymin = ymin, ymax = ymax),
                fill = "forestgreen", alpha = 0.25, inherit.aes = FALSE) +
    geom_ribbon(data = ribbon_neg, aes(x = month, ymin = ymin, ymax = ymax),
                fill = "firebrick", alpha = 0.25, inherit.aes = FALSE) +
    geom_vline(data = year_lines, aes(xintercept = x), color = "black", linewidth = 0.35) +
    geom_vline(xintercept = start_future, linetype = "dotted", linewidth = 0.8) +
    geom_col(data = bars_all, aes(x = month, y = value, fill = series),
             position = "dodge", width = 25, alpha = 0.85) +
    geom_col(data = inv_bars_psp, aes(x = month, y = value), fill = "#E15759",
             width = 18, alpha = 0.9, inherit.aes = FALSE) +
    geom_text(data = inv_bars_psp, aes(x = month, y = value,
              label = paste0(round(value/1000), "k")),
              vjust = -0.4, size = 2.5, color = "#E15759",
              fontface = "bold", inherit.aes = FALSE) +
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
# Salary plan heatmap
# ================================================================
make_salary_heatmap <- function(d, psp_filter = NULL) {
  sp <- d$salary_plan
  if (is.null(sp) || nrow(sp) == 0) return(NULL)

  df <- if (!is.null(psp_filter)) sp |> filter(psp == psp_filter) else sp
  if (nrow(df) == 0) return(NULL)

  # Use role from plan directly; fall back to Lohntabelle classification
  if ("role" %in% names(df) && any(!is.na(df$role) & df$role != "Other" & df$role != "")) {
    person_class <- df |> group_by(name) |> summarise(role_class = first(role[role != "" & !is.na(role)]), .groups = "drop")
  } else {
    lohn <- d$lohntabelle
    phd_range  <- if (!is.null(lohn)) lohn |> filter(str_detect(tolower(rolle), "phd"))  |> pull(monatlich) else numeric()
    post_range <- if (!is.null(lohn)) lohn |> filter(str_detect(tolower(rolle), "post")) |> pull(monatlich) else numeric()
    classify <- function(amounts) {
      med <- median(amounts, na.rm = TRUE)
      if (length(phd_range)  > 0 && med >= min(phd_range)  * 0.8 && med <= max(phd_range)  * 1.2) return("PhD Student")
      if (length(post_range) > 0 && med >= min(post_range) * 0.8 && med <= max(post_range) * 1.2) return("Postdoc")
      return("Other")
    }
    person_class <- df |> group_by(name) |> summarise(role_class = classify(amount), .groups = "drop")
  }

  # One box per role (a role change is modelled as a new person, so each
  # person lives in exactly one box), tiles coloured by PSP so funding-source
  # switches show along a person's timeline.
  plot_df <- df |>
    left_join(person_class, by = "name") |>
    mutate(
      role_class = coalesce(role_class, "Other"),
      psp_lab    = if_else(is.na(psp) | psp == "", "(no PSP)", psp),
      name       = factor(name, levels = rev(sort(unique(name))))
    )

  # Role boxes ordered expensive -> cheap (top -> bottom)
  role_order <- plot_df |>
    group_by(role_class) |>
    summarise(cost = median(amount, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(cost)) |>
    pull(role_class)
  plot_df <- plot_df |> mutate(role_class = factor(role_class, levels = role_order))

  # Continuous month index so year gridlines and the "now" marker line up
  y0 <- year(min(plot_df$month))
  plot_df <- plot_df |> mutate(mx = (year(month) - y0) * 12 + month(month))
  mon_df <- plot_df |> distinct(mx, month) |> arrange(mx)
  if (nrow(mon_df) > 36) mon_df <- mon_df |> filter(month(month) %in% c(1, 4, 7, 10))
  yr_df <- plot_df |>
    group_by(yr = year(month)) |>
    summarise(mid = (min(mx) + max(mx)) / 2, .groups = "drop")
  yr_bounds <- (yr_df$yr[-1] - y0) * 12 + 0.5

  today   <- Sys.Date()
  today_x <- (year(today) - y0) * 12 + month(today) - 0.5 +
             (day(today) - 1) / as.numeric(days_in_month(today))

  # PSP colours assigned in fixed order from the FULL plan, not the filtered
  # subset, so the per-PSP filter never repaints surviving PSPs.
  # CVD-validated categorical palette (worst adjacent deutan/protan dE >= 21).
  psp_pal  <- c("#2A78D6","#1BAF7A","#EDA100","#008300","#4A3AA7","#E34948","#E87BA4","#EB6834")
  all_psps <- sort(unique(sp$psp[!is.na(sp$psp) & sp$psp != ""]))
  psp_colors <- c(setNames(rep_len(psp_pal, length(all_psps)), all_psps),
                  "(no PSP)" = "grey70")

  p <- ggplot(plot_df, aes(x = mx, y = name, fill = psp_lab)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_vline(xintercept = yr_bounds, color = "grey40", linewidth = 0.35) +
    scale_fill_manual(values = psp_colors, name = "PSP",
                      breaks = names(psp_colors)[names(psp_colors) %in% plot_df$psp_lab]) +
    scale_x_continuous(breaks = mon_df$mx, labels = month(mon_df$month),
                       expand = expansion(add = 0.5),
                       sec.axis = dup_axis(breaks = yr_df$mid, labels = yr_df$yr)) +
    facet_grid(role_class ~ ., scales = "free_y", space = "free_y") +
    labs(title = if (!is.null(psp_filter)) paste("Salary Plan -", psp_filter) else "Salary Plan - All PSPs",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x      = element_text(size = 7),
      axis.text.x.top  = element_text(size = 9, face = "bold"),
      axis.text.y      = element_text(size = 9),
      panel.grid       = element_blank(),
      panel.border     = element_rect(color = "grey75", fill = NA, linewidth = 0.4),
      panel.spacing.y  = unit(8, "pt"),
      strip.text.y     = element_text(size = 9, face = "bold", angle = 0),
      legend.position  = "bottom"
    )
  if (today_x >= min(plot_df$mx) - 0.5 && today_x <= max(plot_df$mx) + 0.5)
    p <- p + geom_vline(xintercept = today_x, color = "firebrick", linewidth = 0.9)
  p
}


# ================================================================
# Zahlungsplan heatmap
# ================================================================
make_zp_heatmap <- function(zp_data) {
  if (is.null(zp_data) || nrow(zp_data) == 0) return(NULL)
  df <- zp_data |>
    mutate(month = floor_date(date, "month"),
           year  = as.character(year(month)),
           mon   = factor(format(month, "%b"), levels = month.abb)) |>
    group_by(id, year, mon) |>
    summarise(amount = sum(planned_income, na.rm = TRUE), .groups = "drop") |>
    filter(amount > 0)
  ggplot(df, aes(x = mon, y = id, fill = amount / 1000)) +
    geom_tile(color = "white", linewidth = 0.4) +
    geom_text(aes(label = ifelse(amount > 0, paste0(round(amount/1000, 0), "k"), "")),
              size = 3.2, color = "black", fontface = "bold") +
    facet_grid(. ~ year, switch = "x") +
    scale_fill_gradient(low = "#AED6F1", high = "#1A5276", name = "kCHF") +
    labs(title = "Expected Payments (Zahlungsplan)", x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(size = 7),
      axis.text.y     = element_text(size = 9),
      panel.grid      = element_blank(),
      strip.placement = "outside",
      strip.text      = element_text(size = 9, face = "bold"),
      panel.spacing.x = unit(2, "pt"),
      legend.position = "bottom"
    )
}


# ================================================================
# EPIC monitoring plot
# ================================================================
# EPIC bills arrive quarterly, so raw monthly bars are spiky. Bars show booked
# EPIC spend per month (stacked by PSP); the step line is the trailing
# avg_window-month average — computed identically to epic_monthly_avg, so the
# right end of the line IS the number the forecast sidebar suggests at the
# same window.
make_epic_plot <- function(d, avg_window = 12) {
  df <- d$ist_raw |> filter(category == "EPIC", actual_spending != 0)
  if (nrow(df) == 0) return(NULL)

  by_psp <- df |>
    group_by(month, id) |>
    summarise(value = sum(actual_spending, na.rm = TRUE), .groups = "drop")

  grid <- tibble(month = seq(min(by_psp$month), max(by_psp$month), by = "1 month"))
  monthly <- grid |>
    left_join(by_psp |> group_by(month) |>
                summarise(value = sum(value), .groups = "drop"), by = "month") |>
    mutate(value = replace_na(value, 0),
           trail = sapply(seq_along(month), \(i)
             mean(value[max(1, i - avg_window + 1):i])))

  current_avg <- last(monthly$trail)

  ggplot() +
    geom_col(data = by_psp, aes(x = month, y = value, fill = id),
             position = "stack", width = 25, alpha = 0.85) +
    geom_step(data = monthly, aes(x = month, y = trail),
              linewidth = 1, color = "grey20") +
    scale_fill_brewer(palette = "Set2", name = "PSP") +
    scale_x_date(date_breaks = "3 months", date_labels = "%b %y") +
    scale_y_continuous(labels = scales::label_number(big.mark = "'", accuracy = 1)) +
    labs(title    = "EPIC (animal facility) costs",
         subtitle = paste0("Bars: booked per month (bills usually quarterly). ",
                           "Line: trailing ", avg_window, "-month average — currently ",
                           format(round(current_avg), big.mark = "'"), " CHF/month."),
         x = NULL, y = "CHF / month") +
    theme_bw(base_size = 12) +
    theme(plot.title    = element_text(face = "bold"),
          axis.text.x   = element_text(angle = 90, vjust = 0.5, hjust = 1),
          panel.grid.minor = element_blank(),
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
      card_header("Select data folder"),
      # App-version stamp: the desktop shortcut runs a COPY of the app in
      # Documents\ethisbudget that only updates when the installer is re-run.
      # Showing when this app.R was last modified makes a stale copy obvious.
      helpText(style = "color:#888;",
               paste0("App version (app.R last modified): ",
                      format(file.mtime("app.R"), "%Y-%m-%d %H:%M"),
                      " — if this is older than expected, re-run ",
                      "'Install ethisbudget.bat' to update.")),
      helpText("Pick the folder that contains the Einzelpostenbericht (export_YYYYMMDD_HHMMSS.xlsx). ",
               "All other files (Konten, Zahlungsplan, Salaryplan, Investments, Lohntabelle) are read from — and written to — the same folder."),
      shinyDirButton("data_dir_btn", "Browse folder…", "Select data folder", class = "btn-outline-primary"),
      uiOutput("ui_data_dir_status"),
      actionButton("btn_load", "Load", class = "btn-primary mt-2"),
      uiOutput("ui_load_status"),
      uiOutput("ui_data_health")
    )
  ),

  nav_panel("📊 Monitoring (per PSP)",
    layout_sidebar(
      sidebar = sidebar(
        uiOutput("ui_psp_select"),
        helpText("Actual balance ends at last available data month.")
      ),
      tagList(
        uiOutput("header_monitoring_psp"),
        card(plotOutput("plot_monitoring", height = "900px"))
      )
    )
  ),

  nav_panel("📊 Monitoring (All PSPs)",
    tagList(
      uiOutput("header_monitoring_all"),
      card(plotOutput("plot_monitoring_all", height = "900px"))
    )
  ),

  nav_panel("🐭 EPIC",
    layout_sidebar(
      sidebar = sidebar(width = 260,
        sliderInput("epic_avg_window", "Averaging window (months)",
                    min = 3, max = 24, value = 12, step = 1),
        helpText("EPIC charges booked in the Einzelposten, stacked by PSP. ",
                 "Bills usually arrive quarterly; the line smooths them into a ",
                 "trailing monthly average. Set the forecast's EPIC CHF/month ",
                 "from the line's current level — or higher if you expect it to grow.")
      ),
      card(plotOutput("plot_epic", height = "600px"))
    )
  ),

  nav_panel("👥 Salary Plan",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        uiOutput("ui_psp_select_sal"),
        hr(),
        actionButton("btn_add_person", "➕ Add Person", class = "btn-sm btn-outline-primary w-100 mb-2"),
        actionButton("btn_save_salplan", "💾 Save to Salaryplan.xlsx", class = "btn-sm btn-success w-100"),
        hr(),
        strong("People — click to edit"),
        selectInput("selected_person", NULL, choices = character(), selectize = FALSE,
                    width = "1px") |> (\(x) tagAppendAttributes(x, style="visibility:hidden;height:1px;margin:0;padding:0;"))(),
        uiOutput("ui_person_list")
      ),
      card(plotOutput("plot_salary_heatmap", height = "500px"))
    )
  ),

  nav_panel("💳 Zahlungsplan",
    layout_sidebar(
      sidebar = sidebar(width = 260,
        actionButton("btn_add_zp", "➕ Add Zahlungsplan", class = "btn-sm btn-outline-primary w-100 mb-2"),
        actionButton("btn_save_zp", "💾 Save Zahlungsplan.xlsx", class = "btn-sm btn-success w-100"),
        hr(),
        strong("PSPs — click to view/edit"),
        uiOutput("ui_zp_psp_list")
      ),
      card(plotOutput("plot_zp_heatmap", height = "500px"))
    )
  ),

  nav_panel("💰 Investments",
    layout_sidebar(
      sidebar = sidebar(width = 220,
        actionButton("btn_save_inv", "💾 Save Investments.xlsx", class = "btn-sm btn-success w-100"),
        hr(),
        helpText("Add planned one-off costs (equipment, sequencing, etc.).",
                 "Right-click table to add/remove rows.",
                 "Investments are deducted in forecasts and shown as red bars.")
      ),
      card(rhandsontable::rHandsontableOutput("hot_inv", height = "600px"))
    )
  ),

  nav_panel("📋 Konten",
    layout_sidebar(
      sidebar = sidebar(width = 220,
        actionButton("btn_add_konto", "➕ Add Konto", class = "btn-sm btn-outline-primary w-100 mb-2"),
        actionButton("btn_save_konten", "💾 Save Konten.xlsx", class = "btn-sm btn-success w-100 mb-1"),
        actionButton("btn_revert_konten", "↩ Revert from disk", class = "btn-sm btn-outline-secondary w-100"),
        hr(),
        helpText("Edit PSP accounts directly. Changes are saved to Konten.xlsx in the selected data folder."),
        uiOutput("ui_konten_missing")
      ),
      card(rhandsontable::rHandsontableOutput("hot_konten", height = "600px"))
    )
  ),

  nav_panel("🔮 Forecast (per PSP)",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        uiOutput("ui_psp_select_fc"),
        sliderInput("burn_window_psp", "Burn rate window (months)",
                    min = 2, max = 12, value = 6, step = 1),
        sliderInput("inflation_psp", "Inflation on spending (% / year)",
                    min = 0, max = 3, value = 0, step = 0.25),
        uiOutput("ui_cost_controls_psp"),
        hr(),
        strong("Personnel — toggle to include/exclude"),
        uiOutput("ui_person_toggles_psp"),
        hr(),
        strong("Investments — toggle to include/exclude"),
        uiOutput("ui_inv_toggles_psp")
      ),
      card(plotOutput("plot_forecast_psp", height = "600px"))
    )
  ),

  nav_panel("🔮 Forecast (Total)",
    layout_sidebar(
      sidebar = sidebar(width = 280,
        sliderInput("burn_window", "Burn rate window (months)",
                    min = 2, max = 12, value = 6, step = 1),
        sliderInput("inflation", "Inflation on spending (% / year)",
                    min = 0, max = 3, value = 0, step = 0.25),
        helpText("Salary from Salaryplan.xlsx; consumables scaled by FTE of toggled-on people; EPIC as flat background cost."),
        uiOutput("ui_cost_controls"),
        hr(),
        strong("Personnel — toggle to include/exclude"),
        uiOutput("ui_person_toggles"),
        hr(),
        strong("Investments — toggle to include/exclude"),
        uiOutput("ui_inv_toggles")
      ),
      card(plotOutput("plot_forecast", height = "600px"))
    )
  )
)

# ================================================================
# Server
# ================================================================
server <- function(input, output, session) {

  rv <- reactiveValues(data = NULL, ep_path = NULL, data_dir = NULL)

  # --- Folder picker -------------------------------------------------------
  # Roots: home + all volumes (Windows drive letters, Unix /). Session-scoped.
  shinyFiles::shinyDirChoose(
    input, "data_dir_btn",
    roots   = c(Home = path.expand("~"), shinyFiles::getVolumes()()),
    session = session
  )

  # Resolve picked directory → path; scan for latest export_YYYYMMDD_HHMMSS.xlsx
  picked_data_dir <- reactive({
    req(input$data_dir_btn)
    sel <- input$data_dir_btn
    if (is.null(sel) || identical(sel, 0L) || !is.list(sel) || is.null(sel$path)) return(NULL)
    shinyFiles::parseDirPath(
      roots = c(Home = path.expand("~"), shinyFiles::getVolumes()()),
      sel
    )
  })

  picked_ep_file <- reactive({
    dir <- picked_data_dir()
    req(dir, dir.exists(dir))
    files <- list.files(dir, pattern = "^export_\\d{8}_\\d{6}\\.xlsx$", full.names = FALSE)
    if (length(files) == 0) return(NULL)
    files[order(files, decreasing = TRUE)][1]   # filename timestamp sorts as ISO-ish
  })

  observe({
    rv$data_dir <- picked_data_dir()
  })

  output$ui_data_dir_status <- renderUI({
    dir <- picked_data_dir()
    if (is.null(dir)) {
      return(helpText("(no folder selected)"))
    }
    ep <- picked_ep_file()
    if (is.null(ep)) {
      return(div(class = "alert alert-warning mt-2",
                 p(strong("No Einzelpostenbericht found in:"), br(), tags$code(dir)),
                 p("Expected a file matching ", tags$code("export_YYYYMMDD_HHMMSS.xlsx"), ".")))
    }
    div(class = "alert alert-info mt-2",
        p(strong("Folder: "), tags$code(dir)),
        p(strong("Will load: "), tags$code(ep)))
  })

  notify_health <- function() {
    h <- rv$data$health
    if (!is.null(h) && length(h) > 0)
      showNotification(paste0("⚠️ ", length(h),
        " data notice(s) — details on the Load Data tab."),
        type = "warning", duration = 8)
  }

  reload_data <- function() {
    req(rv$ep_path)
    withProgress(message = "Reloading data...", {
      tryCatch({
        rv$data <- load_all_data(rv$ep_path)
        notify_health()
      }, error = function(e) {
        showNotification(paste("❌ Reload error:", e$message), type = "error", duration = 10)
      })
    })
  }

  observeEvent(input$btn_load, {
    dir <- picked_data_dir()
    if (is.null(dir)) {
      showNotification("Pick a data folder first.", type = "warning"); return()
    }
    ep <- picked_ep_file()
    if (is.null(ep)) {
      showNotification("No export_YYYYMMDD_HHMMSS.xlsx in that folder.", type = "error"); return()
    }
    rv$ep_path <- file.path(dir, ep)
    withProgress(message = "Loading data...", {
      tryCatch({
        rv$data <- load_all_data(rv$ep_path)
        showNotification(
          paste0("✅ Loaded. Reference date: ",
                 format(rv$data$reference_date, "%d.%m.%Y")),
          type = "message", duration = 5)
        notify_health()
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
    tagList(
      div(class = "alert alert-success mt-2",
          p(strong("Loaded successfully")),
          p(n_psps, "accounts from Konten.xlsx"),
          p(n_zp,   "Zahlungspläne found"),
          p("Reference date:", format(d$reference_date, "%d.%m.%Y")),
          p("Data folder: ", tags$code(d$raw_dir))
      )
    )
  })

  output$ui_data_health <- renderUI({
    req(rv$data)
    h <- rv$data$health
    if (is.null(h) || length(h) == 0)
      return(div(class = "alert alert-success mt-2",
                 "✅ Data health: no dropped rows, all IDs cross-match."))
    div(class = "alert alert-warning mt-2",
        p(strong(paste0("⚠️ Data health — ", length(h), " notice(s):"))),
        tags$ul(lapply(h, tags$li)))
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

  # ── helper: format CHF numbers ──────────────────────────────────────────
  fmt_chf <- function(x) format(round(x), big.mark = "'", scientific = FALSE)

  # ── Monitoring (per PSP) header ──────────────────────────────────────────
  output$header_monitoring_psp <- renderUI({
    req(rv$data, input$psp_id)
    d            <- rv$data
    psp_id       <- input$psp_id
    today_month  <- d$reference_date
    konten       <- d$konten
    kostenstelle_ids <- konten |> filter(typ == "Kostenstelle") |> pull(id)
    startup_ids      <- konten |> filter(typ == "Startup")      |> pull(id)

    cur_year     <- year(today_month)
    is_kost      <- psp_id %in% kostenstelle_ids

    # For Kostenstelle: scope all header stats to current calendar year only
    yr_filter_planned <- if (is_kost) quote(year(month) == cur_year) else quote(TRUE)
    yr_filter_ist     <- if (is_kost) quote(year(month) == cur_year) else quote(month <= today_month)

    # Budget total = planned income (current year for Kostenstelle), else actual income fallback
    budget_total <- d$planned_income_m |>
      filter(id == psp_id, !!yr_filter_planned) |>
      summarise(v = sum(planned_income, na.rm = TRUE)) |> pull(v)
    if (budget_total == 0) {
      budget_total <- d$ist_monthly |>
        filter(id == psp_id, !!yr_filter_ist) |>
        summarise(v = sum(actual_income, na.rm = TRUE)) |> pull(v)
    }

    # IST = spending (current year for Kostenstelle, all to reference date otherwise)
    ist_total <- d$ist_monthly |>
      filter(id == psp_id, !!yr_filter_ist) |>
      summarise(v = sum(actual_spending, na.rm = TRUE)) |> pull(v)
    # Kostenstelle: net the EP credits (negative bookings) into IST so the
    # number reads identically to SAP's Kostenstellenbericht "Ist" column.
    if (is_kost) {
      ist_total <- ist_total - (d$ist_monthly |>
        filter(id == psp_id, !!yr_filter_ist) |>
        summarise(v = sum(actual_income, na.rm = TRUE)) |> pull(v))
    }

    pct_spent <- if (budget_total > 0) round(100 * ist_total / budget_total, 1) else NA_real_

    # Balance = income - spending (scoped same as above)
    if (is_kost) {
      # credits are already netted into IST above — income here is ZP only,
      # so Balance == Budget-IST for the Kostenstelle, exactly like SAP
      income_so_far <- d$planned_income_m |>
        filter(id == psp_id, year(month) == cur_year) |>
        summarise(v = sum(planned_income, na.rm = TRUE)) |> pull(v)
    } else if (psp_id %in% startup_ids) {
      income_so_far <- d$planned_income_m |>
        filter(id == psp_id, month <= today_month) |>
        summarise(v = sum(planned_income, na.rm = TRUE)) |> pull(v)
    } else {
      income_so_far <- d$ist_monthly |>
        filter(id == psp_id, month <= today_month) |>
        summarise(v = sum(actual_income, na.rm = TRUE)) |> pull(v)
    }
    balance <- income_so_far - ist_total

    # FTE in current month for this PSP
    fte_now <- NA_real_
    if (!is.null(d$salary_plan) && nrow(d$salary_plan) > 0) {
      fte_now <- d$salary_plan |>
        filter(psp == psp_id, floor_date(as_date(month), "month") == floor_date(today_month, "month")) |>
        summarise(v = sum(fte, na.rm = TRUE)) |> pull(v)
      if (length(fte_now) == 0) fte_now <- NA_real_
    }

    tags$div(
      style = "display:flex; gap:2rem; padding:0.5rem 1rem; background:#f8f9fa;
               border-radius:6px; margin-bottom:0.5rem; flex-wrap:wrap; font-size:0.9rem;",
      tags$span(tags$b("Data up to: "), format(today_month, "%Y-%m")),
      tags$span(tags$b("Budget total: "), paste0(fmt_chf(budget_total), " CHF")),
      tags$span(tags$b("IST: "),          paste0(fmt_chf(ist_total),    " CHF")),
      tags$span(tags$b("Budget-IST: "),   if (!is.na(pct_spent)) paste0(fmt_chf(budget_total - ist_total), " CHF (", pct_spent, "% spent)") else "—"),
      tags$span(tags$b("Balance: "),      paste0(fmt_chf(balance),      " CHF")),
      tags$span(tags$b("FTE (this month): "), if (!is.na(fte_now) && fte_now > 0) round(fte_now, 2) else "—")
    )
  })

  # ── Monitoring (All PSPs) header ─────────────────────────────────────────
  output$header_monitoring_all <- renderUI({
    req(rv$data)
    d            <- rv$data
    today_month  <- d$reference_date
    konten       <- d$konten
    kostenstelle_ids <- konten |> filter(typ == "Kostenstelle") |> pull(id)
    startup_ids      <- konten |> filter(typ == "Startup")      |> pull(id)
    exclude_all      <- startup_ids   # Erlöse revenue counts in the totals

    cur_year <- year(today_month)

    # Budget total: Kostenstelle = current year only; all others = full zahlungsplan
    planned_ids <- d$planned_income_m |> filter(!id %in% exclude_all) |> pull(id) |> unique()
    budget_total_kost <- d$planned_income_m |>
      filter(id %in% kostenstelle_ids, year(month) == cur_year) |>
      summarise(v = sum(planned_income, na.rm = TRUE)) |> pull(v)
    budget_total_other <- d$planned_income_m |>
      filter(!id %in% exclude_all, !id %in% kostenstelle_ids) |>
      summarise(v = sum(planned_income, na.rm = TRUE)) |> pull(v)
    budget_total_nozp <- d$ist_monthly |>
      filter(!id %in% exclude_all, !id %in% planned_ids) |>
      summarise(v = sum(actual_income, na.rm = TRUE)) |> pull(v)
    budget_total <- budget_total_kost + budget_total_other + budget_total_nozp

    # IST: Kostenstelle = current year only, netted (spending - credits) to
    # match SAP's Kostenstellenbericht; others = up to reference date
    ist_kost <- d$ist_monthly |>
      filter(id %in% kostenstelle_ids, year(month) == cur_year) |>
      summarise(v = sum(actual_spending - actual_income, na.rm = TRUE)) |> pull(v)
    ist_other <- d$ist_monthly |>
      filter(!id %in% c(kostenstelle_ids, exclude_all), month <= today_month) |>
      summarise(v = sum(actual_spending, na.rm = TRUE)) |> pull(v)
    ist_total <- ist_kost + ist_other

    pct_spent <- if (budget_total > 0) round(100 * ist_total / budget_total, 1) else NA_real_

    # Balance: Kostenstelle current year (ZP budget; credits already netted
    # into IST); Startup planned; others actual income
    income_kost <- d$planned_income_m |>
      filter(id %in% kostenstelle_ids, year(month) == cur_year) |>
      summarise(v = sum(planned_income, na.rm = TRUE)) |> pull(v)
    income_actual <- d$ist_monthly |>
      filter(!id %in% c(kostenstelle_ids, exclude_all), month <= today_month) |>
      summarise(v = sum(actual_income, na.rm = TRUE)) |> pull(v)
    balance <- (income_kost + income_actual) - ist_total

    # FTE in current month across all PSPs
    fte_now <- NA_real_
    if (!is.null(d$salary_plan) && nrow(d$salary_plan) > 0) {
      fte_now <- d$salary_plan |>
        filter(floor_date(as_date(month), "month") == floor_date(today_month, "month")) |>
        summarise(v = sum(fte, na.rm = TRUE)) |> pull(v)
      if (length(fte_now) == 0) fte_now <- NA_real_
    }

    tags$div(
      style = "display:flex; gap:2rem; padding:0.5rem 1rem; background:#f8f9fa;
               border-radius:6px; margin-bottom:0.5rem; flex-wrap:wrap; font-size:0.9rem;",
      tags$span(tags$b("Data up to: "), format(today_month, "%Y-%m")),
      tags$span(tags$b("Budget total: "), paste0(fmt_chf(budget_total), " CHF (excl. Startup)")),
      tags$span(tags$b("IST: "),          paste0(fmt_chf(ist_total),    " CHF")),
      tags$span(tags$b("Budget-IST: "),   if (!is.na(pct_spent)) paste0(fmt_chf(budget_total - ist_total), " CHF (", pct_spent, "% spent)") else "—"),
      tags$span(tags$b("Balance: "),      paste0(fmt_chf(balance),      " CHF")),
      tags$span(tags$b("FTE (this month): "), if (!is.na(fte_now) && fte_now > 0) round(fte_now, 2) else "—")
    )
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
    exclude_all      <- startup_ids   # Erlöse revenue counts in the totals

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
      labs(title = paste0("Monitoring: All PSPs (excl. Startup)
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

  # ── Reactive salary plan ───────────────────────────────────────────────────
  rv_sal     <- reactiveValues(plan = NULL)
  rv_zp      <- reactiveValues(sheets = NULL)  # named list: tab_name -> data.frame
  rv_konten  <- reactiveValues(raw = NULL)
  rv_inv     <- reactiveValues(raw = NULL)

  # load investments on data load
  observeEvent(rv$data, {
    req(rv$data, isTruthy(rv$data$raw_dir))
    inv_path <- file.path(rv$data$raw_dir, "Investments.xlsx")
    rv_inv$raw <- if (file.exists(inv_path)) {
      tryCatch(
        as.data.frame(read_excel(inv_path, col_types = "text")),
        error = function(e) data.frame(Date=character(), Amount=character(),
                                        Description=character(), PSP=character(),
                                        Category=character(), stringsAsFactors=FALSE)
      )
    } else {
      data.frame(Date=character(), Amount=character(), Description=character(),
                 PSP=character(), Category=character(), stringsAsFactors=FALSE)
    }
  }, priority = -1)  # after zp/konten loads

  output$hot_inv <- rhandsontable::renderRHandsontable({
    req(rv_inv$raw)
    psp_choices <- if (!is.null(rv$data)) c("", sort(unique(rv$data$konten$id))) else character()
    cat_choices <- c("", CATEGORY_ORDER)
    rhandsontable::rhandsontable(rv_inv$raw, stretchH = "all", rowHeaders = FALSE) |>
      rhandsontable::hot_col("PSP",      type = "dropdown", source = psp_choices) |>
      rhandsontable::hot_col("Category", type = "dropdown", source = cat_choices)
  })

  observeEvent(input$btn_save_inv, {
    hot <- input$hot_inv
    if (!is.null(hot)) {
      df <- rhandsontable::hot_to_r(hot)
      df <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
      rv_inv$raw <- df
    }
    inv_path <- file.path(rv$data$raw_dir, "Investments.xlsx")
    tryCatch({
      writexl::write_xlsx(list(Investments = rv_inv$raw), inv_path)
      showNotification("✅ Investments.xlsx saved — reloading data...", type = "message", duration = 3)
      reload_data()
    }, error = function(e) {
      showNotification(paste("❌ Save failed:", e$message), type = "error", duration = 6)
    })
  })


  observeEvent(rv$data, {
    req(rv$data, isTruthy(rv$data$raw_dir))
    zp_path <- file.path(rv$data$raw_dir, "Zahlungsplan.xlsx")
    existing_sheets <- if (file.exists(zp_path)) {
      sh_names <- excel_sheets(zp_path)
      setNames(
        lapply(sh_names, function(s) as.data.frame(read_excel(zp_path, sheet = s))),
        sh_names
      )
    } else list()

    # check which konten PSPs are missing from zahlungsplan
    all_psp_ids <- rv$data$konten |> filter(!typ %in% c("Erlöse","Kostenstelle","Reserve")) |> pull(id)
    existing_ids <- sapply(names(existing_sheets), function(nm) canonical_id(nm))
    missing_ids  <- all_psp_ids[!all_psp_ids %in% existing_ids]

    empty_template <- data.frame(Fallig = as.Date(character()), Betrag = numeric(),
                                  Bezeichnung = character(), stringsAsFactors = FALSE)
    new_sheets <- setNames(
      lapply(missing_ids, function(id) empty_template),
      missing_ids
    )

    all_sheets <- c(existing_sheets, new_sheets)
    rv_zp$sheets <- all_sheets

    # if any missing were added, write back to file
    if (length(missing_ids) > 0 && length(all_sheets) > 0) {
      tryCatch({
        writexl::write_xlsx(all_sheets, zp_path)
        showNotification(
          paste0("ℹ️ Added ", length(missing_ids), " missing PSP tab(s) to Zahlungsplan.xlsx: ",
                 paste(missing_ids, collapse = ", ")),
          type = "message", duration = 6)
      }, error = function(e) NULL)
    }

    # load raw konten for editing
    rv_konten$raw <- rv$data$konten_raw
  })

  # ── Konten tab ────────────────────────────────────────────────────────────
  output$hot_konten <- rhandsontable::renderRHandsontable({
    req(rv_konten$raw)
    rhandsontable::rhandsontable(rv_konten$raw, stretchH = "all", rowHeaders = FALSE)
  })

  output$ui_konten_missing <- renderUI({
    req(rv$data, rv_zp$sheets)
    all_psp_ids  <- rv$data$konten |> filter(!typ %in% c("Erlöse","Kostenstelle","Reserve")) |> pull(id)
    existing_ids <- sapply(names(rv_zp$sheets), canonical_id)
    missing <- all_psp_ids[!all_psp_ids %in% existing_ids]
    if (length(missing) == 0) {
      div(class = "alert alert-success p-2", style = "font-size:0.8em;",
          "✅ All PSPs have a Zahlungsplan tab.")
    } else {
      div(class = "alert alert-warning p-2", style = "font-size:0.8em;",
          strong(length(missing), " PSP(s) still missing Zahlungsplan:"),
          tags$ul(lapply(missing, tags$li)))
    }
  })

  # Add Konto modal
  observeEvent(input$btn_add_konto, {
    showModal(modalDialog(
      title = "Add New Konto",
      fluidRow(
        column(4, textInput("new_konto_id",  "PSP ID (e.g. 1-012345)", "")),
        column(5, textInput("new_konto_bez", "Bezeichnung", "")),
        column(3, textInput("new_konto_typ", "Typ (e.g. Grant)", "Grant"))
      ),
      fluidRow(
        column(4, textInput("new_konto_von", "Von (YYYY-MM-DD)", as.character(Sys.Date()))),
        column(4, textInput("new_konto_bis", "Bis (YYYY-MM-DD or unendlich)", ""))
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_add_konto_confirm", "Add", class = "btn-primary btn-sm")
      )
    ))
  })

  observeEvent(input$btn_add_konto_confirm, {
    req(rv_konten$raw)
    new_id <- str_trim(input$new_konto_id)
    if (new_id == "") { showNotification("PSP ID required.", type = "warning"); return() }

    # sync column names from existing konten_raw
    template <- rv_konten$raw[0, , drop = FALSE]
    cols     <- names(template)
    new_row  <- as.data.frame(matrix(NA_character_, nrow = 1, ncol = length(cols)),
                              stringsAsFactors = FALSE)
    names(new_row) <- cols
    # fill known fields (case-insensitive match)
    cl <- tolower(cols)
    if (any(cl == "id"))           new_row[[which(cl == "id")[1]]]           <- new_id
    if (any(cl == "bezeichnung"))  new_row[[which(cl == "bezeichnung")[1]]]  <- str_trim(input$new_konto_bez)
    if (any(cl == "typ"))          new_row[[which(cl == "typ")[1]]]          <- str_trim(input$new_konto_typ)
    if (any(str_detect(cl, "von"))) new_row[[which(str_detect(cl, "von"))[1]]] <- str_trim(input$new_konto_von)
    if (any(str_detect(cl, "bis"))) new_row[[which(str_detect(cl, "bis"))[1]]] <- str_trim(input$new_konto_bis)

    rv_konten$raw <- bind_rows(rv_konten$raw, new_row)

    # cross-sync: add empty ZP tab if missing
    cid <- canonical_id(new_id)
    existing_zp_ids <- sapply(names(rv_zp$sheets %||% list()), canonical_id)
    if (!cid %in% existing_zp_ids) {
      empty_df <- data.frame(Fallig = character(), Betrag = character(),
                             Bezeichnung = character(), stringsAsFactors = FALSE)
      rv_zp$sheets[[cid]] <- empty_df
      showNotification(paste0("ℹ️ Added empty Zahlungsplan tab for ", cid), type = "message", duration = 4)
    }
    removeModal()
    showNotification(paste0("✅ Konto ", new_id, " added. Remember to Save."), type = "message", duration = 4)
  })

  observeEvent(input$btn_save_konten, {
    hot <- input$hot_konten
    if (!is.null(hot)) {
      df <- rhandsontable::hot_to_r(hot)
      df <- as.data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
      # safety check: must have an id column with real data
      cl     <- tolower(names(df))
      id_col <- which(cl == "id")[1]
      if (is.na(id_col) || sum(!is.na(df[[id_col]]) & df[[id_col]] != "NA" & nchar(df[[id_col]]) > 0) < 1) {
        showNotification("❌ Save aborted: no valid ID column found.", type = "error", duration = 6)
        return()
      }
      rv_konten$raw <- df
    }
    konten_path <- file.path(rv$data$raw_dir, "Konten.xlsx")
    # also sync any new konten ids to ZP
    tryCatch({
      writexl::write_xlsx(list(Konten = rv_konten$raw), konten_path)
      # sync ZP: add empty tabs for any new IDs
      cl  <- tolower(names(rv_konten$raw))
      id_col <- which(cl == "id")[1]
      if (!is.na(id_col)) {
        typ_col <- which(cl == "typ")[1]
        all_ids <- canonical_id(rv_konten$raw[[id_col]])
        skip_typs <- c("erlöse","kostenstelle","reserve")
        if (!is.na(typ_col)) {
          typs <- tolower(rv_konten$raw[[typ_col]])
          all_ids <- all_ids[!typs %in% skip_typs]
        }
        existing_zp_ids <- sapply(names(rv_zp$sheets %||% list()), canonical_id)
        new_missing <- all_ids[!all_ids %in% existing_zp_ids & !is.na(all_ids)]
        if (length(new_missing) > 0) {
          for (mid in new_missing) {
            rv_zp$sheets[[mid]] <- data.frame(Fallig = character(), Betrag = character(),
                                              Bezeichnung = character(), stringsAsFactors = FALSE)
          }
          zp_path <- file.path(rv$data$raw_dir, "Zahlungsplan.xlsx")
          writexl::write_xlsx(rv_zp$sheets, zp_path)
          showNotification(paste0("ℹ️ Added ZP tabs for: ", paste(new_missing, collapse=", ")),
                           type = "message", duration = 5)
        }
      }
      showNotification("✅ Konten.xlsx saved — reloading data...", type = "message", duration = 3)
      reload_data()
    }, error = function(e) {
      showNotification(paste("❌ Save failed:", e$message), type = "error", duration = 6)
    })
  })

  observeEvent(input$btn_revert_konten, {
    konten_path <- file.path(rv$data$raw_dir, "Konten.xlsx")
    if (!file.exists(konten_path)) {
      showNotification("❌ Konten.xlsx not found on disk.", type = "error"); return()
    }
    rv_konten$raw <- read_excel(konten_path, col_types = "text") |>
      mutate(across(everything(), ~ ifelse(
        !is.na(suppressWarnings(as.numeric(.x))) & suppressWarnings(as.numeric(.x)) > 10000,
        as.character(as_date(as.numeric(.x), origin = "1899-12-30")), .x
      ))) |>
      as.data.frame()
    showNotification("↩ Reverted to saved Konten.xlsx.", type = "message", duration = 3)
  })

  # heatmap
  output$plot_zp_heatmap <- renderPlot({
    req(rv$data)
    p <- make_zp_heatmap(rv$data$zahlungsplan)
    if (is.null(p)) { plot.new(); text(0.5, 0.5, "No Zahlungsplan data", cex = 1.4, col = "grey50") } else print(p)
  })

  # PSP list buttons
  output$ui_zp_psp_list <- renderUI({
    sh <- rv_zp$sheets
    if (is.null(sh)) return(helpText("Load data first."))
    konten <- if (!is.null(rv$data)) rv$data$konten else NULL
    tagList(lapply(names(sh), function(nm) {
      label <- if (!is.null(konten)) {
        bez <- konten |> filter(id == canonical_id(nm)) |> pull(bezeichnung) |> first()
        if (!is.na(bez) && length(bez) > 0) paste0(nm, " — ", bez) else nm
      } else nm
      actionButton(
        inputId = paste0("zp_btn_", gsub("[^A-Za-z0-9]", "_", nm)),
        label   = label,
        class   = "btn-sm btn-light w-100 text-start mb-1",
        style   = "border-left: 3px solid #1A5276;"
      )
    }))
  })

  # open modal when PSP button clicked
  observe({
    sh <- rv_zp$sheets
    if (is.null(sh)) return()
    lapply(names(sh), function(nm) {
      local({
        nm_ <- nm
        btn_id <- paste0("zp_btn_", gsub("[^A-Za-z0-9]", "_", nm_))
        observeEvent(input[[btn_id]], {
          showModal(modalDialog(
            title = paste("Zahlungsplan:", nm_),
            size  = "xl",
            div(style = "max-height:500px;overflow-y:auto;",
              rHandsontableOutput(paste0("zp_hot_", gsub("[^A-Za-z0-9]", "_", nm_)))
            ),
            footer = tagList(
              modalButton("Close"),
              actionButton(paste0("zp_save_", gsub("[^A-Za-z0-9]", "_", nm_)),
                           "Save", class = "btn-primary btn-sm")
            )
          ))
        }, ignoreInit = TRUE)
      })
    })
  })

  # render handsontable for each PSP
  observe({
    sh <- rv_zp$sheets
    if (is.null(sh)) return()
    lapply(names(sh), function(nm) {
      local({
        nm_  <- nm
        hot_id <- paste0("zp_hot_", gsub("[^A-Za-z0-9]", "_", nm_))
        output[[hot_id]] <- rhandsontable::renderRHandsontable({
          df <- rv_zp$sheets[[nm_]]
          rhandsontable::rhandsontable(df, stretchH = "all", rowHeaders = FALSE)
        })
      })
    })
  })

  # save individual PSP tab back to rv_zp and file
  observe({
    sh <- rv_zp$sheets
    if (is.null(sh)) return()
    lapply(names(sh), function(nm) {
      local({
        nm_    <- nm
        safe   <- gsub("[^A-Za-z0-9]", "_", nm_)
        save_id <- paste0("zp_save_", safe)
        hot_id  <- paste0("zp_hot_",  safe)
        observeEvent(input[[save_id]], {
          hot <- input[[hot_id]]
          if (!is.null(hot)) {
            df <- rhandsontable::hot_to_r(hot)
            df <- as.data.frame(lapply(df, function(x) tryCatch(x, error = function(e) as.character(x))), stringsAsFactors = FALSE)
            rv_zp$sheets[[nm_]] <- df
          }
          zp_path <- file.path(rv$data$raw_dir, "Zahlungsplan.xlsx")
          tryCatch({
            writexl::write_xlsx(rv_zp$sheets, zp_path)
            showNotification(paste0("✅ Saved tab '", nm_, "' to Zahlungsplan.xlsx"),
                             type = "message", duration = 3)
            removeModal()
            reload_data()
          }, error = function(e) {
            showNotification(paste("❌ Save failed:", e$message), type = "error", duration = 6)
          })
        }, ignoreInit = TRUE)
      })
    })
  })

  # Add Zahlungsplan modal
  observeEvent(input$btn_add_zp, {
    showModal(modalDialog(
      title = "Add New Zahlungsplan",
      textInput("new_zp_id", "PSP ID (e.g. 1-012345)", ""),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("btn_add_zp_confirm", "Add", class = "btn-primary btn-sm")
      )
    ))
  })

  observeEvent(input$btn_add_zp_confirm, {
    new_id <- canonical_id(str_trim(input$new_zp_id))
    if (is.na(new_id) || new_id == "" || new_id == "NA") {
      showNotification("PSP ID required.", type = "warning"); return()
    }
    existing_zp_ids <- sapply(names(rv_zp$sheets %||% list()), canonical_id)
    if (new_id %in% existing_zp_ids) {
      showNotification(paste0("Tab for ", new_id, " already exists."), type = "warning"); return()
    }
    # add empty ZP tab
    rv_zp$sheets[[new_id]] <- data.frame(Fallig = character(), Betrag = character(),
                                          Bezeichnung = character(), stringsAsFactors = FALSE)
    # cross-sync: add skeleton Konto row if missing
    if (!is.null(rv_konten$raw)) {
      cl     <- tolower(names(rv_konten$raw))
      id_col <- which(cl == "id")[1]
      if (!is.na(id_col)) {
        existing_konto_ids <- canonical_id(rv_konten$raw[[id_col]])
        if (!new_id %in% existing_konto_ids) {
          new_row <- as.data.frame(matrix(NA_character_, nrow=1, ncol=ncol(rv_konten$raw)),
                                   stringsAsFactors=FALSE)
          names(new_row) <- names(rv_konten$raw)
          new_row[[id_col]] <- new_id
          rv_konten$raw <- bind_rows(rv_konten$raw, new_row)
          showNotification(paste0("ℹ️ Added skeleton Konto row for ", new_id, " — fill in details and save Konten."),
                           type = "message", duration = 5)
        }
      }
    }
    removeModal()
    showNotification(paste0("✅ Zahlungsplan tab for ", new_id, " added. Remember to Save."),
                     type = "message", duration = 4)
  })

  # save all button
  observeEvent(input$btn_save_zp, {
    req(rv_zp$sheets)
    zp_path <- file.path(rv$data$raw_dir, "Zahlungsplan.xlsx")
    tryCatch({
      writexl::write_xlsx(rv_zp$sheets, zp_path)
      showNotification("✅ Zahlungsplan.xlsx saved — reloading data...", type = "message", duration = 3)
      reload_data()
    }, error = function(e) {
      showNotification(paste("❌ Save failed:", e$message), type = "error", duration = 6)
    })
  })

  observeEvent(rv$data, {
    rv_sal$plan    <- rv$data$salary_plan
  })


  output$ui_psp_select_sal <- renderUI({
    req(rv$data)
    choices <- c("All PSPs" = "__all__",
                 setNames(rv$data$konten$id, rv$data$konten$id))
    selectInput("psp_id_sal", "Filter by PSP", choices = choices)
  })

  output$plot_salary_heatmap <- renderPlot({
    req(rv$data)
    psp <- if (!is.null(input$psp_id_sal) && input$psp_id_sal != "__all__") input$psp_id_sal else NULL
    d_tmp <- rv$data
    d_tmp$salary_plan <- rv_sal$plan
    p <- make_salary_heatmap(d_tmp, psp_filter = psp)
    if (is.null(p)) {
      plot.new(); text(0.5, 0.5, "No Salaryplan.xlsx loaded", cex = 1.4, col = "grey50")
    } else print(p)
  })

  output$plot_epic <- renderPlot({
    req(rv$data)
    p <- make_epic_plot(rv$data, avg_window = input$epic_avg_window %||% 12)
    if (is.null(p)) {
      plot.new(); text(0.5, 0.5, "No EPIC bookings in the Einzelposten", cex = 1.4, col = "grey50")
    } else print(p)
  })

  # person list buttons in sidebar
  output$ui_person_list <- renderUI({
    sp <- rv_sal$plan
    if (is.null(sp) || nrow(sp) == 0) return(helpText("No people loaded."))
    req(rv$data)
    lohn       <- rv$data$lohntabelle
    phd_range  <- if (!is.null(lohn)) lohn |> filter(str_detect(tolower(rolle), "phd"))  |> pull(monatlich) else numeric()
    post_range <- if (!is.null(lohn)) lohn |> filter(str_detect(tolower(rolle), "post")) |> pull(monatlich) else numeric()
    all_roles   <- sort(unique(sp$role[!is.na(sp$role) & sp$role != ""]))
    palette     <- c("#4E79A7","#F28E2B","#59A14F","#E15759","#76B7B2","#EDC948","#B07AA1")
    role_colors <- setNames(palette[seq_along(all_roles)], all_roles)
    # use role from plan if present, else classify by salary
    if ("role" %in% names(sp) && any(!is.na(sp$role) & sp$role != "Other" & sp$role != "")) {
      people <- sp |> group_by(name) |> summarise(role = first(role[role != "" & !is.na(role)]), .groups="drop") |> arrange(name)
    } else {
      classify <- function(amounts) {
        med <- median(amounts, na.rm = TRUE)
        if (length(phd_range)  > 0 && med >= min(phd_range)  * 0.8 && med <= max(phd_range)  * 1.2) return("PhD Student")
        if (length(post_range) > 0 && med >= min(post_range) * 0.8 && med <= max(post_range) * 1.2) return("Postdoc")
        return("Other")
      }
      people <- sp |> group_by(name) |> summarise(role = classify(amount), .groups="drop") |> arrange(name)
    }
    # update hidden selector choices whenever list rebuilds
    updateSelectInput(session, "selected_person", choices = c("" , people$name), selected = "")
    tagList(lapply(seq_len(nrow(people)), function(i) {
      nm  <- people$name[i]
      col <- role_colors[people$role[i]] %||% "#888"
      actionButton(
        inputId = paste0("person_btn_", gsub("[^A-Za-z0-9]", "_", nm)),
        label   = span(
          span(style = paste0("display:inline-block;width:10px;height:10px;border-radius:50%;background:", col, ";margin-right:5px;")),
          nm, " ", span(style="font-size:0.75em;color:#888;", paste0("(", people$role[i], ")"))
        ),
        class = "btn-sm btn-light w-100 text-start mb-1",
        style = paste0("border-left: 3px solid ", col, ";")
      )
    }))
  })

  # helper: fully reset modal state
  reset_modal <- function() {
    rv_edit$name     <- NULL
    rv_modal$months  <- NULL
    rv_modal$amounts <- NULL
    rv_modal$psps    <- NULL
    rv_modal$roles   <- NULL
    rv_modal$fte     <- 1
    updateSelectInput(session, "modal_add_role", selected = "")
  }

  # Single observer: each button updates selected_person, one observeEvent opens modal
  observe({
    sp <- rv_sal$plan
    if (is.null(sp) || nrow(sp) == 0) return()
    people <- sp |> distinct(name) |> pull(name)
    lapply(people, function(nm) {
      local({
        nm_ <- nm
        btn_id <- paste0("person_btn_", gsub("[^A-Za-z0-9]", "_", nm_))
        observeEvent(input[[btn_id]], {
          updateSelectInput(session, "selected_person", selected = nm_)
        }, ignoreInit = TRUE)
      })
    })
  })

  open_person_modal <- function(nm) {
    sp_now      <- rv_sal$plan
    person_rows <- sp_now |> filter(name == nm)
    if (nrow(person_rows) == 0) return()
    reset_modal()
    rv_edit$name     <- nm
    rv_modal$months  <- as_date(person_rows$month)
    rv_modal$amounts <- person_rows$amount
    rv_modal$psps    <- person_rows$psp
    rv_modal$roles   <- if ("role" %in% names(person_rows)) person_rows$role else rep("Other", nrow(person_rows))
    rv_modal$fte     <- person_rows$fte[1] %||% 1
    psp_choices <- rv$data$konten |> filter(!typ %in% "Erlöse") |> pull(id)
    role_val <- person_rows |>
      arrange(desc(month)) |>
      filter(month <= Sys.Date()) |>
      slice(1) |> pull(role)
    if (length(role_val) == 0 || is.na(role_val[1]))
      role_val <- person_rows |> arrange(month) |> slice(1) |> pull(role)
    role_val <- (role_val %||% "Other")[1]
    removeModal()
    showModal(make_person_modal(
      person_name = nm, psp = person_rows$psp[1], role = role_val,
      psp_choices = psp_choices
    ))
  }

  observeEvent(input$selected_person, {
    nm <- input$selected_person
    if (is.null(nm) || nchar(nm) == 0) return()
    open_person_modal(nm)
    # reset selector so same person can be re-opened
    updateSelectInput(session, "selected_person", selected = "")
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # ── Person card modal helpers ───────────────────────────────────────────────
  ROLES <- c("PhD Student", "Postdoc", "Other")
  ROLE_COLORS <- c("PhD Student" = "#4E79A7", "Postdoc" = "#F28E2B", "Other" = "#59A14F")

  # Per-month state while modal open
  rv_modal <- reactiveValues(months = NULL, amounts = NULL, psps = NULL, roles = NULL, fte = 1)

  make_person_modal <- function(person_name = "", psp = "", role = "Other", psp_choices = character()) {
    modalDialog(
      title = if (person_name == "") "Add Person" else paste("Edit:", person_name),
      size  = "xl",
      fluidRow(
        column(8, textInput("modal_name", "Name", value = person_name)),
        column(4, numericInput("modal_fte", "FTE (for consumables)", value = rv_modal$fte, min = 0.1, max = 1, step = 0.1))
      ),
      hr(),
      strong("Salary cost incl. overhead (CHF/month)"),
      p(style = "font-size:0.8em;color:#888;margin-bottom:6px;",
        "Each row = one month. Edit amounts, add or remove months freely."),
      uiOutput("modal_fill_controls"),
      div(style = "max-height:380px;overflow-y:auto;margin-top:8px;border:1px solid #eee;border-radius:4px;padding:4px;",
        uiOutput("modal_month_rows")
      ),
      footer = tagList(
        if (person_name != "") actionButton("modal_delete", "\U0001f5d1 Delete person", class = "btn-danger btn-sm"),
        modalButton("Cancel"),
        actionButton("modal_save", "Save", class = "btn-primary btn-sm")
      )
    )
  }

  output$modal_month_rows <- renderUI({
    months  <- as_date(rv_modal$months)   # ensure Date class after c()
    amounts <- rv_modal$amounts
    psps    <- rv_modal$psps
    roles   <- rv_modal$roles
    psp_choices <- if (!is.null(rv$data)) rv$data$konten |> filter(!typ %in% "Erlöse") |> pull(id) else character()
    if (is.null(months) || length(months) == 0)
      return(p("No months yet — add one above.", style="color:#888;font-size:0.85em;padding:6px;"))
    ord     <- order(months)
    months  <- months[ord]
    amounts <- amounts[ord]
    psps    <- psps[ord]
    roles   <- roles[ord]
    tagList(lapply(seq_along(months), function(i) {
      m    <- months[i]
      mkey <- format(m, "%Y_%m")
      div(style = if (i %% 2 == 0) "background:#f5f5f5;padding:2px 4px;" else "padding:2px 4px;",
        fluidRow(
          column(2, p(style="margin:6px 0;font-size:0.85em;font-weight:600;", format(m, "%b %Y"))),
          column(3, numericInput(paste0("modal_m_",    mkey), NULL, value = amounts[i], min = 0, step = 100)),
          column(3, selectInput( paste0("modal_psp_",  mkey), NULL, choices = psp_choices, selected = psps[i])),
          column(3, selectInput( paste0("modal_role_", mkey), NULL, choices = sort(unique(c("PhD Student","Postdoc","Other", if(!is.null(rv_sal$plan) && "role" %in% names(rv_sal$plan)) rv_sal$plan$role))), selected = roles[i])),
          column(1, actionButton(paste0("modal_del_",  mkey), "\u2715", class = "btn-xs btn-outline-danger mt-1"))
        )
      )
    }))
  })

  output$modal_fill_controls <- renderUI({
    role <- input$modal_add_role
    if (is.null(role) || role == "") {
      # editing: derive from most recent past month in rv_modal
      if (length(rv_modal$roles) > 0 && length(rv_modal$months) > 0) {
        ms   <- as_date(rv_modal$months)
        past <- which(ms <= Sys.Date())
        idx  <- if (length(past) > 0) past[which.max(ms[past])] else which.min(ms)
        role <- rv_modal$roles[idx]
      }
    }
    lohn <- rv$data$lohntabelle
    psp_choices <- if (!is.null(rv$data)) rv$data$konten |> filter(!typ %in% "Erlöse") |> pull(id) else character()
    all_roles   <- sort(unique(c("PhD Student","Postdoc","Other",
                                 if (!is.null(rv_sal$plan) && "role" %in% names(rv_sal$plan)) rv_sal$plan$role)))
    # normalise to the three fill-control roles
    if (!is.null(role) && !is.na(role) && nchar(role) > 0) {
      if (!role %in% c("PhD Student", "Postdoc")) role <- "Other"
    } else {
      role <- NULL
    }
    yr_now <- year(Sys.Date())

    # Step 1: Role selector always shown first
    row_role <- fluidRow(
      column(4, selectInput("modal_add_role", "1. Select role", choices = c("— select —" = "", all_roles), selected = role %||% ""))
    )

    # Nothing else until role is chosen
    if (is.null(role) || role == "") return(row_role)

    # Step 2: start date + PSP (always needed)
    row_start <- fluidRow(
      column(3, numericInput("modal_add_yr",  "Start year",  value = yr_now, min=2020, max=2035, step=1)),
      column(2, numericInput("modal_add_mon", "Start month", value = 1,      min=1, max=12, step=1)),
      column(5, selectInput("modal_add_psp",  "PSP",         choices = psp_choices)),
      column(2, p(style="margin-top:10px;font-size:0.8em;color:#888;", role))
    )

    if (role == "PhD Student") {
      max_yr <- if (!is.null(lohn)) max(lohn |> filter(str_detect(tolower(rolle),"phd")) |> pull(jahr), na.rm=TRUE) else 4
      row_end <- fluidRow(
        column(3, numericInput("modal_phd_end_yr",  "End year",  value = yr_now + max_yr, min=2020, max=2035, step=1)),
        column(2, numericInput("modal_phd_end_mon", "End month", value = 12, min=1, max=12, step=1)),
        column(7, div(style="background:#eef4fb;border-radius:4px;padding:6px 10px;font-size:0.85em;margin-top:4px;",
          strong("PhD auto-fill: "), "salary from Lohntabelle year 1→", max_yr, ", capped at end date. Each cell editable after fill."
        ))
      )
      row_btn <- fluidRow(column(12, actionButton("modal_add_row", "+ Fill PhD Student plan", class="btn-sm btn-primary w-100")))
      tagList(row_role, row_start, row_end, row_btn)

    } else if (role == "Postdoc") {
      brackets <- if (!is.null(lohn)) sort(unique(lohn |> filter(str_detect(tolower(rolle),"post")) |> pull(jahr))) else 1:4
      row_end <- fluidRow(
        column(3, selectInput("modal_postdoc_bracket", "Starting bracket",
                              choices = setNames(brackets, paste("Year", brackets)))),
        column(3, numericInput("modal_post_end_yr",  "End year",  value = yr_now+3, min=2020, max=2035, step=1)),
        column(2, numericInput("modal_post_end_mon", "End month", value = 12,       min=1, max=12, step=1)),
        column(4, div(style="background:#fff8ee;border-radius:4px;padding:6px 10px;font-size:0.85em;margin-top:4px;",
          strong("Rates: "), textOutput("modal_bracket_info", inline=TRUE)
        ))
      )
      row_btn <- fluidRow(column(12, actionButton("modal_add_row", "+ Fill Postdoc plan", class="btn-sm btn-warning w-100")))
      tagList(row_role, row_start, row_end, row_btn)

    } else {
      row_end <- fluidRow(
        column(3, numericInput("modal_fill_to_yr",  "End year",  value = yr_now, min=2020, max=2035, step=1)),
        column(2, numericInput("modal_fill_to_mon", "End month", value = 12,     min=1, max=12, step=1)),
        column(4, numericInput("modal_add_amt",     "CHF/month", value = 0,      min=0, step=100)),
        column(3, br(), actionButton("modal_add_row", "+ Fill months", class="btn-sm btn-outline-secondary w-100"))
      )
      tagList(row_role, row_start, row_end)
    }
  })

  output$modal_bracket_info <- renderText({
    lohn <- rv$data$lohntabelle
    if (is.null(lohn)) return("")
    b <- as.integer(input$modal_postdoc_bracket %||% 1)
    rows <- lohn |> filter(str_detect(tolower(rolle), "post"), jahr >= b)
    if (nrow(rows) == 0) return("")
    paste(rows |> arrange(jahr) |> pull(monatlich) |> round() |> paste(collapse=" → "), "CHF/mo")
  })

  # smart add / fill
  observeEvent(input$modal_add_row, {
    req(input$modal_add_yr, input$modal_add_mon, input$modal_add_role)
    role    <- input$modal_add_role
    lohn    <- rv$data$lohntabelle
    start_m <- as_date(paste0(as.integer(input$modal_add_yr), "-",
                               sprintf("%02d", as.integer(input$modal_add_mon)), "-01"))
    psp     <- input$modal_add_psp %||% ""

    if (role == "PhD Student" && !is.null(lohn)) {
      phd_rates <- lohn |> filter(str_detect(tolower(rolle), "phd")) |> arrange(jahr)
      max_yr    <- max(phd_rates$jahr)
      end_yr    <- as.integer(input$modal_phd_end_yr  %||% (year(start_m) + max_yr))
      end_mon   <- as.integer(input$modal_phd_end_mon %||% 12)
      end_m     <- min(
        start_m %m+% months(max_yr * 12 - 1),
        as_date(paste0(end_yr, "-", sprintf("%02d", end_mon), "-01"))
      )
      new_ms    <- as_date(seq(start_m, end_m, by = "1 month"))
      new_amt <- sapply(new_ms, function(m) {
        job_yr  <- min(as.integer(interval(start_m, m) / months(1)) %/% 12 + 1, max_yr)
        row_sal <- phd_rates |> filter(jahr == job_yr)
        if (nrow(row_sal) == 0) row_sal <- phd_rates |> slice_tail(n=1)
        row_sal$monatlich[1]
      })

    } else if (role == "Postdoc" && !is.null(lohn)) {
      bracket  <- as.integer(input$modal_postdoc_bracket %||% 1)
      end_yr   <- as.integer(input$modal_post_end_yr  %||% (year(start_m) + 3))
      end_mon  <- as.integer(input$modal_post_end_mon %||% 12)
      end_m    <- as_date(paste0(end_yr, "-", sprintf("%02d", end_mon), "-01"))
      new_ms   <- as_date(seq(start_m, end_m, by = "1 month"))
      post_rates <- lohn |> filter(str_detect(tolower(rolle), "post")) |> arrange(jahr)
      max_yr     <- max(post_rates$jahr)
      new_amt <- sapply(new_ms, function(m) {
        job_yr  <- min(bracket - 1 + as.integer(interval(start_m, m) / months(1)) %/% 12 + 1, max_yr)
        row_sal <- post_rates |> filter(jahr == job_yr)
        if (nrow(row_sal) == 0) row_sal <- post_rates |> slice_tail(n=1)
        row_sal$monatlich[1]
      })

    } else {
      end_yr  <- as.integer(input$modal_fill_to_yr  %||% year(start_m))
      end_mon <- as.integer(input$modal_fill_to_mon %||% 12)
      end_m   <- as_date(paste0(end_yr, "-", sprintf("%02d", end_mon), "-01"))
      new_ms  <- as_date(seq(start_m, end_m, by = "1 month"))
      new_amt <- rep(as.numeric(input$modal_add_amt %||% 0), length(new_ms))
    }

    n        <- length(new_ms)
    new_psp  <- rep(psp,  n)
    new_role <- rep(role, n)
    existing <- as_date(rv_modal$months)
    keep     <- !existing %in% new_ms
    rv_modal$months  <- as_date(c(existing[keep], new_ms))
    rv_modal$amounts <- c(rv_modal$amounts[keep], new_amt)
    rv_modal$psps    <- c(rv_modal$psps[keep],    new_psp)
    rv_modal$roles   <- c(rv_modal$roles[keep],   new_role)
  })

  # delete individual month
  observe({
    months <- as_date(rv_modal$months)
    if (is.null(months) || length(months) == 0) return()
    lapply(months, function(m) {
      btn_id <- paste0("modal_del_", format(as_date(m), "%Y_%m"))
      observeEvent(input[[btn_id]], {
        idx <- which(as_date(rv_modal$months) == as_date(m))
        if (length(idx) > 0) {
          rv_modal$months  <- as_date(rv_modal$months)[-idx]
          rv_modal$amounts <- rv_modal$amounts[-idx]
          rv_modal$psps    <- rv_modal$psps[-idx]
          rv_modal$roles   <- rv_modal$roles[-idx]
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })

  # store which person is being edited
  rv_edit <- reactiveValues(name = NULL)

  # open blank modal for new person
  observeEvent(input$btn_add_person, {
    req(rv$data)
    rv_edit$name     <- NULL
    rv_modal$months  <- NULL
    rv_modal$amounts <- NULL
    rv_modal$psps    <- NULL
    rv_modal$roles   <- NULL
    rv_modal$fte     <- 1
    psp_choices <- rv$data$konten |> filter(!typ %in% "Erlöse") |> pull(id)
    reset_modal()
    removeModal()
    showModal(make_person_modal(psp_choices = psp_choices))
  })

  # save modal
  observeEvent(input$modal_save, {
    req(input$modal_name)
    name <- str_trim(input$modal_name)
    fte  <- as.numeric(input$modal_fte %||% 1)
    if (name == "") { removeModal(); return() }

    # read current amounts/psp/role from inputs (user may have edited them)
    months  <- as_date(rv_modal$months)
    amounts <- sapply(months, function(m) {
      as.numeric(input[[paste0("modal_m_",    format(m, "%Y_%m"))]] %||% 0)
    })
    psps  <- sapply(months, function(m) {
      input[[paste0("modal_psp_",  format(m, "%Y_%m"))]] %||% (rv_modal$psps[which(as_date(rv_modal$months) == m)[1]] %||% "")
    })
    roles <- sapply(months, function(m) {
      input[[paste0("modal_role_", format(m, "%Y_%m"))]] %||% (rv_modal$roles[which(as_date(rv_modal$months) == m)[1]] %||% "Other")
    })

    new_rows <- tibble(month = months, amount = amounts, name = name, fte = fte, psp = psps, role = roles) |>
      filter(amount > 0) |>
      select(name, fte, psp, role, month, amount)

    existing <- rv_sal$plan %||% tibble(name=character(), fte=numeric(), psp=character(), role=character(), month=as_date(character()), amount=numeric())
    # remove old rows: if editing use old name, if adding new person don't remove anything
    old_name <- rv_edit$name
    existing_kept <- if (!is.null(old_name)) existing |> filter(name != old_name) else existing
    rv_sal$plan     <- bind_rows(existing_kept, new_rows) |> arrange(name, month)
    reset_modal()
    removeModal()
  })

  # delete person
  observeEvent(input$modal_delete, {
    req(rv_edit$name, rv_sal$plan)
    rv_sal$plan     <- rv_sal$plan |> filter(name != rv_edit$name)
    reset_modal()
    removeModal()
  })

  # save to xlsx
  observeEvent(input$btn_save_salplan, {
    req(rv_sal$plan)
    sp <- rv_sal$plan
    people <- unique(sp$name)
    sheets <- lapply(people, function(nm) {
      sp |> filter(name == nm) |> arrange(month) |>
        transmute(Month = format(as_date(month), "%Y-%m-%d"), CHF = amount, PSP = psp, Role = role, FTE = fte)
    })
    names(sheets) <- people
    sal_path <- file.path(rv$data$raw_dir, "Salaryplan.xlsx")
    tryCatch({
      writexl::write_xlsx(sheets, sal_path)
      showNotification("✅ Salaryplan.xlsx saved.", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(paste("❌ Save failed:", e$message), type = "error", duration = 6)
    })
  })

  # ── Consumables & EPIC controls ───────────────────────────────────────────
  # Suggested values come from the SAME helpers the forecast falls back to, and
  # the box value (suggested or hand-edited) is passed verbatim into the plot
  # functions — so the sidebar numbers and the forecast math can never diverge.
  # Suggestions recompute (and reset any manual override) when the burn window
  # or the data changes.
  cost_controls_ui <- function(suffix, burn_window_months, psp_id = NULL) {
    d <- rv$data
    d$salary_plan_full <- rv_sal$plan   # calibrate FTE on the full team
    rate_yr <- round(consumables_per_fte_month(d, burn_window_months)$per_fte_month * 12)
    epic_m  <- round(epic_monthly_avg(d, burn_window_months, psp_id))
    div(
      hr(),
      strong("Consumables"),
      checkboxInput(paste0("consum_on", suffix), "Include consumables",
                    value = isolate(input[[paste0("consum_on", suffix)]]) %||% TRUE),
      numericInput(paste0("consum_rate", suffix), "CHF / FTE / year",
                   value = rate_yr, min = 0, step = 500),
      p(style="margin:-6px 0 6px;font-size:0.75em;color:#888;",
        paste0("Suggested ", format(rate_yr, big.mark="'"),
               " — all grants, last ", burn_window_months,
               "m, excl. Salary & EPIC. Applied × FTE of people toggled on.")),
      strong("EPIC (background cost)"),
      checkboxInput(paste0("epic_on", suffix), "Include EPIC",
                    value = isolate(input[[paste0("epic_on", suffix)]]) %||% TRUE),
      numericInput(paste0("epic_amt", suffix), "CHF / month",
                   value = epic_m, min = 0, step = 100),
      p(style="margin:-6px 0 0;font-size:0.75em;color:#888;",
        paste0("Suggested ", format(epic_m, big.mark="'"), " — ",
               if (is.null(psp_id)) "all PSPs" else "this PSP",
               ", last ", burn_window_months, "m avg. Flat per month, independent of FTE."))
    )
  }

  output$ui_cost_controls <- renderUI({
    req(rv$data)
    cost_controls_ui("_tot", input$burn_window)
  })

  output$ui_cost_controls_psp <- renderUI({
    req(rv$data, input$psp_id_fc)
    cost_controls_ui("_psp", input$burn_window_psp, psp_id = input$psp_id_fc)
  })

  # Effective values handed to the plots: 0 when toggled off, box value when
  # set, NULL (= auto, identical to the suggestion) before the UI first renders.
  effective_consum_rate_month <- function(suffix) {
    if (!isTRUE(input[[paste0("consum_on", suffix)]] %||% TRUE)) return(0)
    v <- suppressWarnings(as.numeric(input[[paste0("consum_rate", suffix)]]))
    if (length(v) == 0 || is.na(v)) NULL else v / 12
  }
  effective_epic_monthly <- function(suffix) {
    if (!isTRUE(input[[paste0("epic_on", suffix)]] %||% TRUE)) return(0)
    v <- suppressWarnings(as.numeric(input[[paste0("epic_amt", suffix)]]))
    if (length(v) == 0 || is.na(v)) NULL else v
  }

  # ── Person toggles (checkbox based — reliable re-render) ─────────────────
  make_person_choices <- function(sp, people) {
    if (is.null(sp) || length(people) == 0) return(setNames(people, people))
    palette     <- c("#4E79A7","#F28E2B","#59A14F","#E15759","#76B7B2","#EDC948","#B07AA1")
    all_roles   <- sort(unique(sp$role[!is.na(sp$role) & sp$role != ""]))
    role_colors <- setNames(palette[seq_along(all_roles)], all_roles)
    # Build HTML labels with colored dot
    lbls <- sapply(people, function(nm) {
      role <- sp |> filter(name == nm) |> pull(role) |> first() %||% "Other"
      col  <- role_colors[role] %||% "#888"
      as.character(span(
        span(style = paste0("display:inline-block;width:9px;height:9px;border-radius:50%;background:", col, ";margin-right:5px;")),
        nm
      ))
    })
    setNames(people, lbls)
  }

  output$ui_person_toggles <- renderUI({
    sp <- rv_sal$plan
    if (is.null(sp) || nrow(sp) == 0) return(helpText("No salary plan loaded."))
    people  <- sort(unique(sp$name))
    choices <- make_person_choices(sp, people)
    checkboxGroupInput("included_people", NULL,
      choiceNames  = lapply(names(choices), HTML),
      choiceValues = unname(choices),
      selected     = unname(choices)   # all on by default
    )
  })

  output$ui_person_toggles_psp <- renderUI({
    req(rv$data, input$psp_id_fc)
    sp  <- rv_sal$plan
    if (is.null(sp) || nrow(sp) == 0) return(helpText("No salary plan loaded."))
    # show people who appear on this PSP at any point
    people  <- sort(unique(sp$name[sp$psp == input$psp_id_fc]))
    if (length(people) == 0) return(helpText("No people on this PSP."))
    choices <- make_person_choices(sp, people)
    checkboxGroupInput("included_people_psp", NULL,
      choiceNames  = lapply(names(choices), HTML),
      choiceValues = unname(choices),
      selected     = unname(choices)
    )
  })

  # re-select all when plan or PSP changes
  observeEvent(rv_sal$plan, {
    sp <- rv_sal$plan
    if (is.null(sp)) return()
    updateCheckboxGroupInput(session, "included_people",
      selected = unique(sp$name))
  })
  observeEvent(input$psp_id_fc, {
    sp <- rv_sal$plan
    if (is.null(sp)) return()
    people <- unique(sp$name[sp$psp == input$psp_id_fc])
    updateCheckboxGroupInput(session, "included_people_psp", selected = people)
  })

  # filtered salary plan — uses checkbox selection
  sal_plan_active <- reactive({
    sp <- rv_sal$plan
    if (is.null(sp)) return(NULL)
    sp
  })
  sal_plan_psp <- reactive({
    sp <- rv_sal$plan
    if (is.null(sp)) return(NULL)
    inc <- input$included_people_psp
    if (is.null(inc)) return(sp)
    sp |> filter(name %in% inc)
  })
  sal_plan_tot <- reactive({
    sp <- rv_sal$plan
    if (is.null(sp)) return(NULL)
    inc <- input$included_people
    if (is.null(inc)) return(sp)
    sp |> filter(name %in% inc)
  })

  # ── Investment toggles ─────────────────────────────────────────────────────
  make_inv_choices <- function(inv, psp_filter = NULL) {
    if (is.null(inv) || nrow(inv) == 0) return(character())
    if (!is.null(psp_filter)) inv <- inv |> filter(psp == psp_filter)
    if (nrow(inv) == 0) return(character())
    df <- inv |>
      mutate(label = paste0(desc, " (", format(month, "%b %Y"), ", ",
                             format(round(amount/1000, 1), big.mark="'"), "k CHF)"),
             key   = paste0(psp, "_", format(month, "%Y%m"), "_", round(amount))) |>
      distinct(key, label)
    setNames(df$key, df$label)
  }

  output$ui_inv_toggles <- renderUI({
    inv <- rv$data$investments
    if (is.null(inv) || nrow(inv) == 0) return(helpText("No investments planned.", style="font-size:0.85em;"))
    choices <- make_inv_choices(inv)
    checkboxGroupInput("included_inv", NULL,
      choiceNames  = names(choices), choiceValues = unname(choices),
      selected     = unname(choices))
  })

  output$ui_inv_toggles_psp <- renderUI({
    req(input$psp_id_fc)
    inv <- rv$data$investments
    if (is.null(inv) || nrow(inv) == 0) return(helpText("No investments for this PSP.", style="font-size:0.85em;"))
    choices <- make_inv_choices(inv, psp_filter = input$psp_id_fc)
    if (length(choices) == 0) return(helpText("No investments for this PSP.", style="font-size:0.85em;"))
    checkboxGroupInput("included_inv_psp", NULL,
      choiceNames  = names(choices), choiceValues = unname(choices),
      selected     = unname(choices))
  })

  # re-select all when data reloads
  observeEvent(rv$data, {
    inv <- rv$data$investments
    if (is.null(inv) || nrow(inv) == 0) return()
    all_keys <- inv |>
      mutate(key = paste0(psp, "_", format(month, "%Y%m"), "_", round(amount))) |>
      pull(key)
    updateCheckboxGroupInput(session, "included_inv",     selected = all_keys)
    updateCheckboxGroupInput(session, "included_inv_psp", selected = all_keys)
  })

  # filter investments by selection
  inv_filtered_tot <- reactive({
    inv <- rv$data$investments
    if (is.null(inv) || nrow(inv) == 0) return(inv)
    sel <- input$included_inv
    if (is.null(sel)) return(inv |> filter(FALSE))  # none checked = none shown
    inv |> mutate(key = paste0(psp, "_", format(month, "%Y%m"), "_", round(amount))) |>
      filter(key %in% sel) |> select(-key)
  })

  inv_filtered_psp <- reactive({
    inv <- rv$data$investments
    if (is.null(inv) || nrow(inv) == 0) return(inv)
    sel <- input$included_inv_psp
    if (is.null(sel)) return(inv |> filter(FALSE))
    inv |> mutate(key = paste0(psp, "_", format(month, "%Y%m"), "_", round(amount))) |>
      filter(key %in% sel) |> select(-key)
  })

  output$plot_forecast_psp <- renderPlot({
    req(rv$data, input$psp_id_fc)
    d_fc <- rv$data
    d_fc$salary_plan      <- sal_plan_psp()
    d_fc$salary_plan_full <- rv_sal$plan   # unfiltered — calibrates past_fte
    d_fc$investments  <- inv_filtered_psp()
    p <- make_psp_forecast_plot(input$psp_id_fc, d_fc, input$burn_window_psp,
                                  inflation_rate = input$inflation_psp,
                                  consumables_rate_month = effective_consum_rate_month("_psp"),
                                  epic_monthly = effective_epic_monthly("_psp"))
    if (is.null(p)) { plot.new(); text(0.5, 0.5, "No data available", cex = 1.5) } else p
  })

  output$plot_forecast <- renderPlot({
    req(rv$data)
    d_fc <- rv$data
    d_fc$salary_plan      <- sal_plan_tot()
    d_fc$salary_plan_full <- rv_sal$plan   # unfiltered — calibrates past_fte
    d_fc$investments <- inv_filtered_tot()
    make_forecast_plot(d_fc, burn_window_months = input$burn_window,
                       inflation_rate = input$inflation,
                       consumables_rate_month = effective_consum_rate_month("_tot"),
                       epic_monthly = effective_epic_monthly("_tot"))
  })
}

shinyApp(ui, server)
