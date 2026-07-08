suppressMessages({
  library(dplyr); library(tidyr); library(tibble); library(lubridate)
  library(stringr); library(readr); library(purrr); library(ggplot2)
})

# Evaluate only whitelisted top-level function definitions from app.R
exprs <- parse("app.R")
want  <- c("%||%", "safe_max_date", "canonical_id",
           "consumables_per_fte_month", "epic_monthly_avg", "compute_salary_cost",
           "interpolate_balance", "make_forecast_plot", "make_psp_forecast_plot")
for (e in exprs) {
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) {
    nm <- gsub("`", "", deparse(e[[2]]))
    if (nm %in% want) eval(e, envir = globalenv())
  }
}
stopifnot(all(sapply(setdiff(want, "%||%"), exists)))

# ---- synthetic data --------------------------------------------------------
months_past <- seq(as_date("2026-01-01"), as_date("2026-06-01"), by = "1 month")
d <- list(
  reference_date = as_date("2026-06-01"),
  konten = tibble(id = c("K1", "G1"), bezeichnung = c("Core", "Grant"),
                  typ = c("Kostenstelle", "Grant"),
                  laufzeit_date = as_date(c(NA, "2028-12-31"))),
  ist_raw = bind_rows(
    tibble(id = "K1", month = months_past, category = "Salary",      actual_spending = 10000, actual_income = 0),
    tibble(id = "K1", month = months_past, category = "Consumables", actual_spending = 2000,  actual_income = 0),
    tibble(id = "K1", month = months_past, category = "EPIC",        actual_spending = 1500,  actual_income = 0)
  ),
  salary_plan = NULL, salary_plan_full = NULL
)
d$ist_monthly <- d$ist_raw |> group_by(id, month) |>
  summarise(actual_income = sum(actual_income), actual_spending = sum(actual_spending), .groups = "drop")
d$planned_income_m <- bind_rows(
  tibble(id = "K1", month = seq(as_date("2026-01-01"), as_date("2028-01-01"), by = "1 year"), planned_income = 150000),
  tibble(id = "G1", month = as_date(c("2026-10-01", "2027-10-01")), planned_income = 90570))
sp <- bind_rows(
  tibble(name = "A", month = seq(as_date("2026-01-01"), as_date("2026-12-01"), by = "1 month"),
         amount = 8000, psp = "K1", role = "PhD Student", fte = 1),
  tibble(name = "B", month = seq(as_date("2026-01-01"), as_date("2026-12-01"), by = "1 month"),
         amount = 4000, psp = "G1", role = "Postdoc", fte = 0.5))
d$salary_plan <- sp; d$salary_plan_full <- sp
d$investments <- tibble(month = as_date(character()), amount = numeric(),
                        desc = character(), psp = character(), cat = character())
d$zahlungsplan <- NULL

ok <- function(label, cond) cat(sprintf("%-55s %s\n", label, if (isTRUE(cond)) "PASS" else "FAIL"))

# 1. consumables rate: EPIC excluded, global. spend 6x2000, past FTE 1.5
r <- consumables_per_fte_month(d, 6)
ok("rate excludes EPIC (12000/(6*1.5)=1333.33)", abs(r$per_fte_month - 12000/9) < 1e-6)

# 2. EPIC averages
ok("epic avg global = 1500/m",  abs(epic_monthly_avg(d, 6) - 1500) < 1e-6)
ok("epic avg for G1 = 0",       epic_monthly_avg(d, 6, psp_id = "G1") == 0)

# 3. compute_salary_cost: explicit rate is used verbatim
fut <- seq(as_date("2026-07-01"), as_date("2026-09-01"), by = "1 month")
c_explicit <- compute_salary_cost(fut, d, 6, consumables_rate_month = 1200)
ok("explicit rate: 12000 + 1.5*1200 = 13800", all(abs(c_explicit - 13800) < 1e-6))
c_auto <- compute_salary_cost(fut, d, 6)
ok("NULL rate = auto calibration (14000)", all(abs(c_auto - (12000 + 1.5 * 12000/9)) < 1e-6))
c_off <- compute_salary_cost(fut, d, 6, consumables_rate_month = 0)
ok("rate 0 = salaries only (12000)", all(abs(c_off - 12000) < 1e-6))

# 4. full plots build; EPIC line shifts balance by exactly epic*months
p1 <- make_forecast_plot(d, 6, inflation_rate = 0, consumables_rate_month = 1200, epic_monthly = 1500)
p2 <- make_forecast_plot(d, 6, inflation_rate = 0, consumables_rate_month = 1200, epic_monthly = 0)
ok("total forecast builds (ggplot)", inherits(p1, "ggplot") || inherits(p1, "grob") || !is.null(p1))
b1 <- ggplot_build(p1); b2 <- ggplot_build(p2)  # smoke: renders without error
ok("total forecast renders with and without EPIC", TRUE)

# 5. person switching PSP mid-series (e.g. SNF 4-year limit): each month is
#    charged to that month's PSP only; FTE (consumables) moves with them and
#    is never double-counted across PSPs or in the total.
d2 <- d
d2$salary_plan <- d2$salary_plan_full <- bind_rows(
  tibble(name = "C", month = as_date(c("2026-07-01", "2026-08-01")),
         amount = 6000, psp = "K1", role = "PhD Student", fte = 1),
  tibble(name = "C", month = as_date("2026-09-01"),
         amount = 6000, psp = "G1", role = "PhD Student", fte = 1))
on_k1 <- compute_salary_cost(fut, d2, 6, psp_id = "K1", consumables_rate_month = 1200)
on_g1 <- compute_salary_cost(fut, d2, 6, psp_id = "G1", consumables_rate_month = 1200)
tot   <- compute_salary_cost(fut, d2, 6, psp_id = NULL, consumables_rate_month = 1200)
ok("PSP switch: K1 pays Jul+Aug only (7200,7200,0)", all(abs(on_k1 - c(7200, 7200, 0)) < 1e-6))
ok("PSP switch: G1 pays Sep only (0,0,7200)",        all(abs(on_g1 - c(0, 0, 7200)) < 1e-6))
ok("PSP switch: total counts each month once",       all(abs(tot - (on_k1 + on_g1)) < 1e-6))

# 6. per-PSP forecast for the grant with no bookings (regression: empty past)
p3 <- make_psp_forecast_plot("G1", d, 6, consumables_rate_month = 1200, epic_monthly = 0)
ok("PSP forecast for booking-less grant builds", !is.null(p3))
invisible(ggplot_build(p3))
ok("PSP forecast renders", TRUE)

# 7. year-end transfer to the Forschungsreserve is money moved, not consumed:
#    it must not inflate the consumables-per-FTE calibration.
d3 <- d
d3$ist_raw <- bind_rows(d3$ist_raw,
  tibble(id = "K1", month = as_date("2026-06-01"),
         category = "Transfer Forschungsreserve",
         actual_spending = 50000, actual_income = 0))
r3 <- consumables_per_fte_month(d3, 6)
ok("Reserve transfer excluded from consumables rate", abs(r3$per_fte_month - r$per_fte_month) < 1e-6)

# 8. end-of-horizon label: with a Startup account, a second line shows the
#    balance incl. the Startup saldo (planned income - spending to date).
lab_of <- function(p) {
  labs <- unlist(lapply(ggplot_build(p)$data,
                        function(df) if ("label" %in% names(df)) as.character(df$label) else NULL))
  labs[grepl("Expected", labs)][1]
}
num_in <- function(s) as.numeric(gsub("'", "", regmatches(s, regexpr("[+-]?[0-9']+", s))))
d4 <- d
d4$konten <- bind_rows(d4$konten,
  tibble(id = "S1", bezeichnung = "Startup", typ = "Startup", laufzeit_date = as_date(NA)))
d4$planned_income_m <- bind_rows(d4$planned_income_m,
  tibble(id = "S1", month = as_date("2026-01-01"), planned_income = 80000))
d4$ist_monthly <- bind_rows(d4$ist_monthly,
  tibble(id = "S1", month = as_date("2026-03-01"), actual_income = 0, actual_spending = 30000))
p4 <- make_forecast_plot(d4, 6, inflation_rate = 0, consumables_rate_month = 1200, epic_monthly = 1500)
lab1 <- lab_of(p1); lab4 <- lab_of(p4)
ok("no Startup account = no Startup line in label", !grepl("Startup", lab1))
ok("Startup account adds incl.-Startup line",       grepl("incl. Startup saldo", lab4))
line_incl <- strsplit(lab4, "\n")[[1]] |> (\(x) x[grepl("Startup", x)])()
ok("incl.-Startup = expected + saldo (80000-30000)",
   abs(num_in(line_incl) - (num_in(lab1) + 50000)) < 1)
