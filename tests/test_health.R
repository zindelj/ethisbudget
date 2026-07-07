suppressMessages({
  library(dplyr); library(tidyr); library(tibble); library(lubridate)
  library(stringr); library(readr); library(purrr)
  library(readxl); library(writexl); library(janitor)
})

exprs <- parse("app.R")
want  <- c("%||%", "canonical_id", "extract_zp_id", "read_zahlungsplan", "load_salaryplan")
for (e in exprs) {
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) {
    nm <- gsub("`", "", deparse(e[[2]]))
    if (nm %in% want) eval(e, envir = globalenv())
  }
}

ok <- function(label, cond) cat(sprintf("%-60s %s\n", label, if (isTRUE(cond)) "PASS" else "FAIL"))
tmp <- tempdir()

# ---- Salaryplan with one bad date row and one skipped sheet ----------------
sal <- file.path(tmp, "Salaryplan.xlsx")
write_xlsx(list(
  Anna = data.frame(Month = c("2026-08-01", "01.10.2026", "2026-09-01"),
                    CHF = c("8000", "8000", "x8000"), PSP = "1-999999",
                    Role = "PhD Student", FTE = 1),
  Ben  = data.frame(Month = c("2026-08-01"), CHF = c("5000"), PSP = "K1",
                    Role = "Postdoc", FTE = 0.5),
  Broken = data.frame(Foo = 1:2, Bar = 3:4)
), sal)
sp <- load_salaryplan(sal)
h  <- attr(sp, "health")
cat("health notes:\n"); print(h)
ok("good rows kept (Anna 1 + Ben 1)", nrow(sp) == 2)
ok("Anna's 2 bad rows reported", any(grepl("Anna.*2 row", h)))
ok("Broken sheet reported", any(grepl("Broken.*skipped", h)))

# ---- Zahlungsplan with a bad date -----------------------------------------
zp <- file.path(tmp, "Zahlungsplan_PSP_1-999999.xlsx")
write_xlsx(data.frame(Fallig = c("2026-10-01", "sometime", "13/1/2027"),
                      Betrag = c("90570", "90570", "90570"),
                      Bezeichnung = "budgetiert"), zp)
z  <- read_zahlungsplan(zp)
hz <- attr(z, "health")
cat("zp health:\n"); print(hz)
ok("2 valid tranches kept (ISO + dmy)", nrow(z) == 2 && sum(z$planned_income) == 181140)
ok("1 bad Fällig reported", any(grepl("1 row.*DROPPED", hz)))

# ---- clean file → no notes -------------------------------------------------
zp2 <- file.path(tmp, "Zahlungsplan_PSP_1-888888.xlsx")
write_xlsx(data.frame(Fallig = "2026-10-01", Betrag = "100", Bezeichnung = "b"), zp2)
ok("clean ZP has no health notes", length(attr(read_zahlungsplan(zp2), "health")) == 0)
