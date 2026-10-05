# Synthetic-data test for build_handover(): the handover file must contain
# the aggregated numbers but NO person names, konto IDs, Buchungstexte or
# Bezeichnungen. Run from the repo root: Rscript --vanilla tests/test_handover.R
suppressMessages({
  library(dplyr); library(tidyr); library(tibble); library(lubridate)
  library(stringr); library(readr); library(purrr)
  library(readxl); library(writexl); library(janitor)
})

for (e in parse("app.R", encoding = "UTF-8")) {
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) {
    nm <- gsub("`", "", deparse(e[[2]]))
    if (!nm %in% c("ui", "server")) eval(e, envir = globalenv())
  }
}
stopifnot(exists("build_handover"), exists("load_all_data"))

ok <- function(label, cond) cat(sprintf("%-60s %s\n", label, if (isTRUE(cond)) "PASS" else "FAIL"))
fails <- 0
chk <- function(label, cond) { ok(label, cond); if (!isTRUE(cond)) fails <<- fails + 1 }

dir <- file.path(tempdir(), "handover_test"); dir.create(dir, showWarnings = FALSE)
unlink(list.files(dir, full.names = TRUE))

# --- Einzelposten with names in Buchungstext, one grant + one Kostenstelle --
months <- seq(as_date("2025-01-01"), as_date("2026-06-01"), by = "1 month")
ep <- bind_rows(
  tibble(Kontierung = "1-234567", Kurztext = "Lohnaufwand", Buchungstext = "Lohn Anna Muster",
         Buch_Dat = months, `Betrag in BW` = 9000),
  tibble(Kontierung = "1-234567", Kurztext = "Laborwaren", Buchungstext = "Sigma-Aldrich Bestellung",
         Buch_Dat = months, `Betrag in BW` = 2500),
  tibble(Kontierung = "1-234567", Kurztext = "Entgelte SNF", Buchungstext = "Tranche 1",
         Buch_Dat = as_date("2025-01-15"), `Betrag in BW` = -300000),
  tibble(Kontierung = "29870", Kurztext = "ILV TPF bud.r.Ko-Ver", Buchungstext = "TypIIL mouse",
         Buch_Dat = months[c(3, 6, 9, 12)], `Betrag in BW` = 6000),
  tibble(Kontierung = "29870", Kurztext = "Flugreisen", Buchungstext = "Flug Ben Beispiel Boston",
         Buch_Dat = as_date("2026-03-10"), `Betrag in BW` = 1800)
)
write_xlsx(ep, file.path(dir, "export_20260701_120000.xlsx"))
write_xlsx(data.frame(ID = c("1-234567", "29870"),
                      Bezeichnung = c("SNF Projekt Muster Immunology", "Kostenstelle Zindel"),
                      Typ = c("Grant", "Kostenstelle"),
                      Laufzeit_von = c("2025-01-01", NA), Laufzeit_bis = c("2028-12-31", "unendlich")),
           file.path(dir, "Konten.xlsx"))
sal_months <- seq(as_date("2025-01-01"), as_date("2027-12-01"), by = "1 month")
write_xlsx(list(
  `Anna Muster` = data.frame(Month = as.character(sal_months), CHF = 7500, PSP = "1-234567",
                             Role = "PhD Student", FTE = 1),
  `Ben Beispiel` = data.frame(Month = as.character(sal_months[1:18]), CHF = 9800, PSP = "29870",
                              Role = "Postdoc", FTE = 0.8)
), file.path(dir, "Salaryplan.xlsx"))
write_xlsx(data.frame(Rolle = c("PhD Student", "PhD Student", "Postdoc"), Jahr = c(2025, 2026, 2026),
                      Jahresgehalt_CHF = c(90000, 92000, 118000)),
           file.path(dir, "Lohntabelle.xlsx"))
write_xlsx(data.frame(Date = "2027-03-01", Amount = 45000, Description = "Spectral cytometer (quote via Anna Muster)",
                      PSP = "1-234567", Category = "Equipment"),
           file.path(dir, "Investments.xlsx"))
write_xlsx(list(`1-234567` = data.frame(Fallig = c("2025-01-15", "2026-01-15", "2027-01-15"),
                                        Betrag = 300000, Bezeichnung = "Tranche")),
           file.path(dir, "Zahlungsplan.xlsx"))

d   <- load_all_data(file.path(dir, "export_20260701_120000.xlsx"))
res <- build_handover(d, stamp = "test")
md  <- paste(readLines(res$md, encoding = "UTF-8"), collapse = "\n")
key <- paste(readLines(res$key, encoding = "UTF-8"), collapse = "\n")
cat("\n---- handover file ----\n"); cat(md); cat("\n---- key ----\n"); cat(key); cat("\n\n")

# --- nothing identifying ----------------------------------------------------
for (bad in c("Anna", "Muster", "Ben", "Beispiel", "1-234567", "29870", "Zindel",
              "Sigma", "Boston", "Tranche 1", "Immunology"))
  chk(paste0("no '", bad, "' in handover"), !str_detect(md, regex(bad, ignore_case = TRUE)))
chk("no 5+ digit runs", !str_detect(md, "(?<![\\d'])\\d{5,}(?![\\d'])"))

# --- the useful content is there -------------------------------------------
chk("pseudonyms G1/K1 used", str_detect(md, "\\| G1 \\|") && str_detect(md, "\\| K1 \\|"))
chk("roles appear", str_detect(md, "PhD Student") && str_detect(md, "Postdoc"))
chk("Salary + Consumables + EPIC categories", all(str_detect(md, c("Salary", "Consumables", "EPIC"))))
chk("salary 2025 on G1 = 108'000", str_detect(md, "108'000"))
chk("consumables rate line present", str_detect(md, "CHF per FTE per month"))
chk("Lohntabelle rates present", str_detect(md, "118'000"))
chk("investment listed with redacted description",
    str_detect(md, "45'000") && str_detect(md, "Spectral cytometer") && str_detect(md, "\\[redacted\\]"))
chk("planned income 300'000 present", str_detect(md, "300'000"))
chk("key maps labels to IDs", str_detect(key, "G1 ") && str_detect(key, "1-234567") && str_detect(key, "29870"))
chk("scrub hit reported (investment description)", res$scrub_hits >= 1)
chk("years not thousand-formatted", !str_detect(md, "2'02\\d"))

# --- with Bezeichnungen: grant titles appear, person tokens still scrubbed --
res2 <- build_handover(d, include_konto_names = TRUE, stamp = "test_names")
md2  <- paste(readLines(res2$md, encoding = "UTF-8"), collapse = "\n")
chk("Bezeichnung included on request", str_detect(md2, "Immunology"))
chk("person token inside Bezeichnung still redacted", !str_detect(md2, "Muster"))
chk("Bezeichnung scrub reported in key", res2$scrub_hits >= 2)

cat(sprintf("\n%d failure(s)\n", fails))
if (fails > 0) quit(status = 1)
