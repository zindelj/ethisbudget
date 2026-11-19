suppressMessages({
  library(dplyr); library(tidyr); library(tibble); library(lubridate)
  library(stringr); library(readr); library(purrr)
  library(readxl); library(writexl); library(janitor)
})

exprs <- parse("app.R")
want  <- c("%||%", "canonical_id", "extract_zp_id", "parse_any_date", "read_zahlungsplan", "load_salaryplan", "filter_own_vb",
           "write_salaryplan_xlsx", "salary_history", "notes_path", "read_note", "write_note")
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
  Anna = data.frame(Month = c("2026-08-01", "01.10.2026", "46266", "2026-09-01", "sometime"),
                    CHF = c("8000", "8000", "8000", "x8000", "8000"), PSP = "1-999999",
                    Role = "PhD Student", FTE = 1),
  Ben  = data.frame(Month = c("2026-08-01"), CHF = c("5000"), PSP = "K1",
                    Role = "Postdoc", FTE = 0.5),
  Broken = data.frame(Foo = 1:2, Bar = 3:4)
), sal)
sp <- load_salaryplan(sal)
h  <- attr(sp, "health")
cat("health notes:\n"); print(h)
ok("good rows kept (Anna ISO + dmy + serial, Ben 1)", nrow(sp) == 4)
ok("serial 46266 -> 2026-09-01", as.character(as_date("2026-09-01")) %in% as.character(sp$month))
ok("Anna's 2 bad rows reported (bad CHF, bad Month)", any(grepl("Anna.*2 row", h)))
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

# ---- VB filter: other Verantwortungsbereiche dropped, Voucher exempt -------
konten_vb <- tibble(id = c("29870", "1-0ETH", "1-0VOU", "1-0SNF"),
                    typ = c("Kostenstelle", "Startup", "Startup", "Grant"),
                    keep_all_vb = c(FALSE, FALSE, TRUE, FALSE))
ist_vb <- tibble(id = c("29870", "29870", "1-0ETH", "1-0ETH", "1-0VOU", "1-0SNF", "1-0SNF"),
                 vb = c("125", "125", "125", "124", "123", "125", NA),
                 actual_income = c(0, 0, 0, 0, 40000, 0, 0),
                 actual_spending = c(10, 20, 5000, 20876, 100, 300, 7))
r_vb <- filter_own_vb(ist_vb, konten_vb)
ok("own VB read off the Kostenstelle", identical(r_vb$vb_mine, "125"))
ok("other-VB row on shared PSP dropped, Voucher + NA-VB rows kept", nrow(r_vb$ist_raw) == 6 &&
     !any(r_vb$ist_raw$id == "1-0ETH" & r_vb$ist_raw$vb %in% "124"))
ok("dropped share reported per konto/VB", any(grepl("1-0ETH VB 124.*20'876", r_vb$notes)))
ok("no VB column = untouched", nrow(filter_own_vb(ist_vb |> select(-vb), konten_vb)$ist_raw) == 7)

# ---- Leavers: write/read round trip + team history -------------------------
plan <- tibble(name = c("Ana", "Ana", "Ben"), fte = c(1, 1, 0.5), psp = c("1-000001", "1-000001", "1-000002"),
               role = c("PhD Student", "PhD Student", "Postdoc"),
               month = as_date(c("2026-01-01", "2026-02-01", "2026-01-01")), amount = c(5000, 5100, 9000))
lv_path <- file.path(tmp, "Leavers.xlsx")
write_salaryplan_xlsx(plan |> filter(name == "Ana"), lv_path)
lv <- load_salaryplan(lv_path)
ok("Leavers.xlsx round trip keeps months, CHF, PSP, role, FTE",
   nrow(lv) == 2 && all(lv$amount == c(5000, 5100)) && all(lv$psp == "1-000001") &&
   all(lv$role == "PhD Student") && all(lv$fte == 1))
hist <- salary_history(plan |> filter(name == "Ben"), lv)
ok("team history = plan + leavers", nrow(hist) == 3 && setequal(hist$name, c("Ana", "Ben")))
ok("same name+month in plan and leavers counted once",
   nrow(salary_history(plan, lv)) == 3)
ok("no leavers = plan unchanged", identical(salary_history(plan, NULL), plan))

# ---- Notes files -----------------------------------------------------------
nd <- file.path(tmp, "notes_test"); dir.create(nd, showWarnings = FALSE)
ok("note path sanitises PSP id", basename(notes_path(nd, "1-008665 / x")) == "notes_1-008665___x.txt")
ok("missing note reads as empty", read_note(nd, "ALL") == "")
ok("empty note does not create a file", !write_note(nd, "1-000009", "") && !file.exists(notes_path(nd, "1-000009")))
txt <- "Zürich: Tranche verspätet\nline 2"
write_note(nd, "1-008665", txt)
ok("note round trip (multi-line, umlauts)", identical(read_note(nd, "1-008665"), txt))
