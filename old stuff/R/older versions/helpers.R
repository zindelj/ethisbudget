# ================================================================
# helpers.R
# ================================================================

library(dplyr)
library(stringr)
library(readxl)
library(janitor)
library(readr)
library(lubridate)

# ----------------------------------------------------------------
# Spending category map
# ----------------------------------------------------------------
CATEGORY_MAP <- c(
  # Salary & social contributions
  "Personalkosten"          = "Salary",
  "Lohnaufwand"             = "Salary",
  "Fam.zul. Lohnaufwand"    = "Salary",
  "AHV"                     = "Salary",
  "ALV"                     = "Salary",
  "FAK-Beiträge"            = "Salary",
  "SUVA"                    = "Salary",
  "Pensionskasse"           = "Salary",
  "Verwalt.aufw.PUBLICA"    = "Salary",

  "Laborwaren"              = "Consumables",
  "Biol. Präp.& Chemika"    = "Consumables",
  "Verbrauchsmaterial"      = "Consumables",
  "Üb. Materialaufwand"     = "Consumables",
  "EDV-Verbrauchsmat."      = "Consumables",
  "IT-Verbrauchsmat."       = "Consumables",
  "Halb-&Fertigpdt.Komp"    = "Consumables",
  "Labortiere/Tierhaltg"    = "Consumables",

  "Maschinen, Geräte"       = "Equipment",
  "Geräte, Maschinen"       = "Equipment",
  "Hardware bis 10'000"     = "Equipment",
  "Wäscheautomaten"         = "Equipment",
  "Unterh, Rep Mobilien"    = "Equipment",
  "Mobiliar & Einricht."    = "Equipment",

  "Software"                = "IT, Office & Publications",
  "Software (nicht akt)"    = "IT, Office & Publications",
  "IT und Telekomm."        = "IT, Office & Publications",
  "Monographien"            = "IT, Office & Publications",
  "Drucksachen, Repro"      = "IT, Office & Publications",
  "Büromaterial"            = "IT, Office & Publications",

  "Flugreisen"              = "Travel, Events & Training",
  "Bahn, ÖV-Mittel"         = "Travel, Events & Training",
  "Unterkunft"              = "Travel, Events & Training",
  "Reisekostenrückerst."    = "Travel, Events & Training",
  "Sachtransporte"          = "Travel, Events & Training",
  "Kurier,Frachten"         = "Travel, Events & Training",
  "Seminare u. Tagungen"    = "Travel, Events & Training",
  "Aus- und Weiterbild."    = "Travel, Events & Training",
  "Aus- & Weiterbildung"    = "Travel, Events & Training",
  "ETH-interne Anlässe"     = "Travel, Events & Training",
  "Repräsentationspesen"    = "Travel, Events & Training",

  "ILV TPF bud.r.Ko-Ver"    = "Internal charges",
  "Übr. DL ETH-nah.Einh"    = "Internal charges",
  "Kostenübernahme"         = "Internal charges",
  "Gebühren"                = "Internal charges",
  "Wirtschaftsor. Fors."    = "Internal charges",
  "Schenkungen"             = "Internal charges",
  "Personalrekrutierung"    = "Internal charges",

  "Reserve Jahresabr"       = "Other",
  "mehrere"                 = "Other"
)

CATEGORY_COLORS <- c(
  "Salary"                    = "rgba(70,130,180,0.85)",
  "Consumables"               = "rgba(82,168,104,0.85)",
  "Equipment"                 = "rgba(210,140,60,0.85)",
  "IT, Office & Publications" = "rgba(150,100,180,0.85)",
  "Travel, Events & Training" = "rgba(200,100,90,0.85)",
  "Internal charges"          = "rgba(130,130,130,0.85)",
  "Other"                     = "rgba(180,170,150,0.85)"
)

CATEGORY_ORDER <- c(
  "Salary", "Consumables", "Equipment",
  "IT, Office & Publications", "Travel, Events & Training",
  "Internal charges", "Other"
)

# ----------------------------------------------------------------
# Filename date parsing
# ----------------------------------------------------------------
parse_extraction_date <- function(path) {
  m <- str_match(basename(path), "_(\\d{8})_\\d{6}\\.xlsx$")
  if (is.na(m[,2])) return(NA_Date_)
  as.Date(m[,2], format = "%Y%m%d")
}

validate_extraction_dates <- function(paths) {
  dates <- sapply(paths, parse_extraction_date)
  names(dates) <- basename(paths)
  unique_dates <- unique(dates[!is.na(dates)])
  if (length(unique_dates) == 0) {
    warning("Could not parse extraction dates from filenames.")
    return(list(reference_date = Sys.Date(), mismatch = FALSE))
  }
  list(
    reference_date = min(as.Date(unique_dates)),
    mismatch       = length(unique_dates) > 1,
    dates          = dates
  )
}

# ----------------------------------------------------------------
# ID normalization
# ----------------------------------------------------------------
canonical_id <- function(x) {
  x <- str_trim(as.character(x))
  x[x == "NA"] <- NA_character_
  x <- if_else(is.na(x), NA_character_, str_replace(x, "-\\d{3}$", ""))
  mask <- !is.na(x) & str_detect(x, "^1\\d{6}$")
  x[mask] <- paste0(substr(x[mask], 1, 1), "-", substr(x[mask], 2, 7))
  x
}

# ----------------------------------------------------------------
# Laufzeit cache
# ----------------------------------------------------------------
cache_path <- here::here("app", "data", "laufzeit_cache.csv")

load_laufzeit_cache <- function() {
  if (!file.exists(cache_path)) {
    return(tibble(
      id            = character(),
      laufzeit_type = character(),
      start_date    = as.Date(character()),
      end_date      = as.Date(character())
    ))
  }
  read_csv(cache_path, col_types = cols(
    id            = col_character(),
    laufzeit_type = col_character(),
    start_date    = col_date(),
    end_date      = col_date()
  ))
}

save_laufzeit_cache <- function(cache_df) {
  dir.create(dirname(cache_path), showWarnings = FALSE, recursive = TRUE)
  write_csv(cache_df, cache_path)
}

update_laufzeit_cache <- function(new_entries) {
  cache <- load_laufzeit_cache()
  updated <- bind_rows(
    cache %>% filter(!id %in% new_entries$id),
    new_entries
  ) %>% arrange(id)
  save_laufzeit_cache(updated)
  updated
}

# ----------------------------------------------------------------
# Load Projektbericht
# ----------------------------------------------------------------
load_projektbericht <- function(path) {
  read_excel(path) %>%
    clean_names() %>%
    mutate(
      id        = canonical_id(id),
      budget    = as.numeric(budget),
      ist       = as.numeric(ist),
      remaining = as.numeric(budget_minus_ist),
      budget    = if_else(
        budget == 0 &
          (str_detect(tolower(bezeichnung), "voucher") |
           str_detect(tolower(bezeichnung), "forschungsreserve")),
        remaining,
        budget
      )
    ) %>%
    select(id, bezeichnung, budget, ist, remaining)
}

# ----------------------------------------------------------------
# Load Kostenstellenbericht
# ----------------------------------------------------------------
load_kostenstellenbericht <- function(path) {
  read_excel(path) %>%
    clean_names() %>%
    mutate(
      id        = canonical_id(id),
      budget    = as.numeric(budget),
      ist       = as.numeric(ist),
      remaining = as.numeric(budget_minus_ist)
    ) %>%
    filter(str_detect(id, "^2")) %>%
    select(id, bezeichnung, budget, ist, remaining)
}

# ----------------------------------------------------------------
# Load Einzelpostenbericht
# ----------------------------------------------------------------
load_einzelposten <- function(path) {
  read_excel(path) %>%
    clean_names() %>%
    rename_with(~ str_replace_all(., "\\.", "_")) %>%
    mutate(
      buch_dat     = as_date(buch_dat),
      betrag_in_bw = as.numeric(betrag_in_bw),
      id           = canonical_id(kontierung),
      month        = floor_date(buch_dat, "month"),
      is_income    = betrag_in_bw < 0,
      is_salary    = kurztext == "Personalkosten",
      category     = coalesce(CATEGORY_MAP[kurztext], "Other")
    ) %>%
    filter(!is.na(id), !is.na(month), !is.na(betrag_in_bw))
}

# ----------------------------------------------------------------
# Misc helpers
# ----------------------------------------------------------------
get_psp_list <- function(projects_df) {
  projects_df %>%
    select(id, bezeichnung) %>%
    mutate(label = paste0(id, " — ", bezeichnung)) %>%
    arrange(id)
}

missing_laufzeit <- function(projects_df, laufzeit_df) {
  projects_df %>% filter(!id %in% laufzeit_df$id) %>% pull(id)
}
