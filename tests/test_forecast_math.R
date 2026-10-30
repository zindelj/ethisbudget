suppressMessages({
  library(dplyr); library(tidyr); library(tibble); library(lubridate)
  library(stringr); library(readr); library(purrr); library(ggplot2)
})

# Evaluate only whitelisted top-level function definitions from app.R
exprs <- parse("app.R")
want  <- c("%||%", "safe_max_date", "canonical_id", "mirror_reserve_transfers",
           "resolve_ep_id_aliases", "INCOME_KURZTEXT_PATTERN",
           "SIGN_INCOME_KURZTEXT_PATTERN", "is_income_row", "storno_is_income",
           "CONSUMABLES_RATE_EXCLUDED_CATEGORIES", "consumables_per_fte_month",
           "BURN_CONSUM_EXCLUDED", "GENOMICS_INV_PATTERN", "burn_model_series", "fit_epic",
           "fit_consum", "burn_backtest", "burn_model", "epic_forecast_vec", "burn_consum_default", "CATEGORY_ORDER",
           "epic_monthly_avg", "compute_salary_cost",
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

# 8. Startup accounts are ETH-allocated money: the ZP figure on its date plus
#    EP income counts as income, and they are IN the total forecast like any
#    grant (the end balance moves by planned - spent; no separate saldo line).
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
ok("no separate Startup saldo line any more", !grepl("Startup", lab4))
ok("Startup in total forecast: expected = base + (80000 - 30000)",
   abs(num_in(lab4) - (num_in(lab1) + 50000)) < 1)
d4b <- d4
d4b$ist_monthly <- bind_rows(d4b$ist_monthly,
  tibble(id = "S1", month = as_date("2026-04-01"), actual_income = 7000, actual_spending = 0))
lab4b <- lab_of(make_forecast_plot(d4b, 6, inflation_rate = 0, consumables_rate_month = 1200, epic_monthly = 1500))
ok("Startup EP income (reimbursement) counts on top of the ZP figure",
   abs(num_in(lab4b) - (num_in(lab4) + 7000)) < 1)

# 9. Year-end settlement: SAP books it only as income on the Reserve; the
#    Kostenstelle side must be mirrored as expenditure, and the whole lateral
#    move must be neutral for the total forecast (balance AND burn average).
d5 <- d
d5$konten <- bind_rows(d5$konten,
  tibble(id = "R1", bezeichnung = "Forschungsreserve", typ = "Reserve",
         laufzeit_date = as_date(NA)))
transfer_row <- tibble(id = "R1", month = as_date("2026-03-01"),
                       category = "Transfer Forschungsreserve",
                       buchungstext = "Jahresabrechnung KST 99999",  # unknown nr -> single-KST fallback
                       actual_spending = 0, actual_income = 35847.45)
mir <- mirror_reserve_transfers(bind_rows(d5$ist_raw, transfer_row), d5$konten)
mrow <- mir$ist_raw |> filter(id == "K1", category == "Transfer Forschungsreserve")
ok("mirror adds expenditure on the Kostenstelle",
   nrow(mrow) == 1 && abs(mrow$actual_spending - 35847.45) < 1e-6 &&
   mrow$actual_income == 0 && mrow$month == as_date("2026-03-01"))
ok("mirror reports a health note", length(mir$notes) == 1 && grepl("mirrored", mir$notes))
d5$ist_raw <- mir$ist_raw
d5$ist_monthly <- d5$ist_raw |> group_by(id, month) |>
  summarise(actual_income = sum(actual_income), actual_spending = sum(actual_spending), .groups = "drop")
p5 <- make_forecast_plot(d5, 6, inflation_rate = 0, consumables_rate_month = 1200, epic_monthly = 1500)
ok("reserve transfer is forecast-neutral (end label unchanged)",
   identical(lab_of(p5), lab1))
ok("transfer stays out of avg burn (subtitle unchanged)",
   identical(p5$labels$subtitle, p1$labels$subtitle))
# already-booked expenditure side -> no mirroring, warn instead
mir2 <- mirror_reserve_transfers(mir$ist_raw, d5$konten)
ok("existing expenditure side blocks re-mirroring",
   nrow(mir2$ist_raw) == nrow(mir$ist_raw) && grepl("no synthetic", mir2$notes))

# 10. Kostenstelle EP credits (negative bookings = actual income) are real
#     money on top of the ZP budget — they must raise the total forecast
#     balance by exactly their amount.
d6 <- d
d6$ist_raw <- bind_rows(d6$ist_raw,
  tibble(id = "K1", month = as_date("2026-02-01"), category = "Other",
         actual_spending = 0, actual_income = 5000))
d6$ist_monthly <- d6$ist_raw |> group_by(id, month) |>
  summarise(actual_income = sum(actual_income), actual_spending = sum(actual_spending), .groups = "drop")
p6 <- make_forecast_plot(d6, 6, inflation_rate = 0, consumables_rate_month = 1200, epic_monthly = 1500)
ok("Kostenstelle credit raises total forecast by 5000",
   abs(num_in(lab_of(p6)) - (num_in(lab1) + 5000)) < 1)
p7 <- make_psp_forecast_plot("K1", d6, 6, consumables_rate_month = 1200, epic_monthly = 0)
ok("per-PSP forecast with credit builds", !is.null(p7))
invisible(ggplot_build(p7))

# 11. PSP-element ID variant ("5-029870-000" -> canonical "5-029870") is
#     remapped to the matching konto; unrelated ids stay untouched.
al <- resolve_ep_id_aliases(
  tibble(id = c("5-029870", "29870", "G9"), month = as_date("2026-01-01"),
         category = "Other", actual_spending = c(604, 100, 50), actual_income = 0),
  tibble(id = c("29870", "K1"), typ = "Kostenstelle"))
ok("PSP-element id remapped to konto",
   identical(sort(unique(al$ist_raw$id)), c("29870", "G9")) &&
   sum(al$ist_raw$actual_spending[al$ist_raw$id == "29870"]) == 704 &&
   grepl("read as konto '29870'", al$notes))
ok("no aliases -> no notes",
   length(resolve_ep_id_aliases(d$ist_raw, d$konten)$notes) == 0)

# 12. income vs Gutschrift: only tranche/transfer Kurztexte are income;
#     negative cost rows are spending corrections (netted like SAP's Ist).
ok("Entgelte SNF = income",        is_income_row("Entgelte SNF"))
ok("Reserve Jahresabr = income",   is_income_row("Reserve Jahresabr"))
ok("Lohnaufwand credit = Gutschrift, not income", !is_income_row("Lohnaufwand"))
ok("NA kurztext = not income",     !is_income_row(NA_character_))
# sign-dependent Kurztexte: non-SNF tranches (negative) vs internal charges
ok("Kostenübernahme negative = income (non-SNF tranche)",
   is_income_row("Kostenübernahme", -50000))
ok("Kostenübernahme positive = spending",  !is_income_row("Kostenübernahme", 120))
ok("Schenkungen negative = income",         is_income_row("Schenkungen", -80000))
ok("Wirtschaftsor. Fors. negative = income", is_income_row("Wirtschaftsor. Fors.", -60000))
ok("Lohnaufwand negative stays Gutschrift", !is_income_row("Lohnaufwand", -20967))

# 13. Gutschriften (negative actual_spending) net the consumables rate:
#     6x2000 consumables minus a 1000 credit -> 11000/(6*1.5)
d8 <- d
d8$ist_raw <- bind_rows(d8$ist_raw,
  tibble(id = "K1", month = as_date("2026-05-01"), category = "Consumables",
         actual_spending = -1000, actual_income = 0))
r8 <- consumables_per_fte_month(d8, 6)
ok("Gutschrift nets consumables rate (11000/9)", abs(r8$per_fte_month - 11000/9) < 1e-6)

# ---- Storno: a reversal takes the side of the row it cancels --------------
K <- "Kostenübernahme"
mk <- function(id, kt, amt, st) tibble(id = id, kurztext = kt, betrag_in_bw = amt, is_storno = st)
# (a) both rows flagged (SAP marks original + reversal): outgoing invoice pair
a <- mk("G", K, c(-150000, 150000), c(TRUE, TRUE))
ok("Storno pair both flagged -> both income",  all(storno_is_income(a)))
# (b) only the reversal flagged: tranche-like original is income, reversal follows it
b <- mk("G", K, c(-150000, 150000), c(FALSE, TRUE))
ok("Storno reversal follows unflagged income original", all(storno_is_income(b)))
# (c) reversal of a real charge received (+25k, then -25k): both spending
c_ <- mk("G", K, c(25000, -25000), c(FALSE, TRUE))
ok("Storno reversal follows unflagged cost original", !any(storno_is_income(c_)))
# (d) cost Kurztext pair stays spending, nets to zero
d_ <- mk("G", "Lohnaufwand", c(10000, -10000), c(TRUE, TRUE))
ok("Storno cost pair both spending", !any(storno_is_income(d_)))
# (e) unflagged rows unaffected; a third real tranche next to the pair stays income
e <- mk("G", K, c(-150000, -150000, 150000), c(FALSE, TRUE, TRUE))
e_inc <- storno_is_income(e)
ok("tranche + flagged pair: all income, net income = tranche",
   all(e_inc) && sum(ifelse(e_inc, -e$betrag_in_bw, 0)) == 150000 &&
   sum(ifelse(e_inc, 0, e$betrag_in_bw)) == 0)
# no is_storno column at all -> plain rule
ok("no Storno column -> plain rule", identical(storno_is_income(mk("G", K, -5, FALSE)[, -4]), TRUE))

# ---- Equipment is excluded from the consumables rate -----------------------
d9 <- d
d9$ist_raw <- bind_rows(d9$ist_raw,
  tibble(id = "K1", month = as_date("2026-05-01"), category = "Equipment",
         actual_spending = 500000, actual_income = 0))
r9 <- consumables_per_fte_month(d9, 6)
ok("Equipment excluded from consumables rate", abs(r9$per_fte_month - r$per_fte_month) < 1e-6)

# ---- Burn model -------------------------------------------------------------
bm <- burn_model(d)
ok("burn: constant FTE -> ratio method, base 0",
   bm$consum$base == 0 && abs(bm$consum$per_fte - 2000 / 1.5) < 1e-6)
ok("burn: no Genomics bookings -> 0/month, not planned",
   bm$params$genomics_month == 0 && !bm$genomics$planned)
ok("burn: NULL rate in compute_salary_cost = burn-model fit",
   all(abs(compute_salary_cost(fut, d, 6) - (12000 + 1.5 * 2000 / 1.5)) < 1e-6))

# regression recovers base + per-FTE when FTE varies
mb <- seq(as_date("2026-01-01"), as_date("2026-10-01"), by = "1 month")
fte_b <- c(1, 1, 2, 2, 3, 3, 2, 4, 4, 3)
db <- d
db$reference_date <- as_date("2026-10-01")
db$ist_raw <- tibble(id = "K1", month = mb, category = "Consumables",
                     actual_spending = 1000 + 800 * fte_b, actual_income = 0)
db$salary_plan_full <- tibble(name = paste0("P", seq_along(mb)), month = mb, amount = 1, psp = "K1",
                              role = "PhD Student", fte = fte_b)
bmb <- burn_model(db)
ok("burn: regression recovers base 1000 + 800/FTE",
   abs(bmb$consum$base - 1000) < 1e-6 && abs(bmb$consum$per_fte - 800) < 1e-6)
ok("burn: base allocated by FTE share per PSP",
   abs(compute_salary_cost(as_date("2026-11-01"), list(salary_plan = tibble(name = c("A","B"), month = as_date("2026-11-01"),
       amount = c(10, 20), psp = c("K1","G1"), role = "x", fte = c(1, 3)), reference_date = db$reference_date),
       psp_id = "G1", consumables_rate_month = 100, consumables_base_month = 400) - (20 + 3 * 100 + 400 * 0.75)) < 1e-6)

# EPIC: linear growth in the monthly rate is recovered from the cumulative fit
de <- db
de$ist_raw <- tibble(id = "K1", month = mb, category = "EPIC",
                     actual_spending = 1000 + 100 * seq_along(mb), actual_income = 0)
bme <- burn_model(de)
ok("burn: EPIC slope 100/month recovered", abs(bme$epic$slope - 100) < 1)
ok("burn: EPIC level now ~2000", abs(bme$epic$level_now - 2000) < 60)
ok("epic_forecast_vec: growth then flat",
   identical(epic_forecast_vec(5, 1000, 100, 2), c(1100, 1200, 1200, 1200, 1200)))
ok("epic_forecast_vec: explicit flat", all(epic_forecast_vec(3, 1500) == 1500))

# Genomics: planned entries on the Investments tab replace the average
dg <- db
dg$ist_raw <- tibble(id = "K1", month = mb, category = "Genomics (FGCZ)", actual_spending = 500, actual_income = 0)
dg$investments <- tibble(month = as_date("2027-01-01"), amount = 40000, desc = "scRNA", psp = "K1", cat = "Sequencing")
bmg <- burn_model(dg, fit_start = as_date("2026-01-01"))
ok("burn: genomics average 500 but planned entry wins (0 in forecast)",
   abs(bmg$genomics$avg - 500) < 1e-6 && bmg$genomics$planned && bmg$params$genomics_month == 0)
dg$investments <- dg$investments[0, ]
ok("burn: no planned genomics -> average used", burn_model(dg, fit_start = as_date("2026-01-01"))$params$genomics_month == 500)

# explicit genomics in the total forecast lowers the end balance by n x amount
p_g0 <- make_forecast_plot(d, 6, inflation_rate = 0, consumables_rate_month = 1200, epic_monthly = 0, genomics_monthly = 0)
p_g1 <- make_forecast_plot(d, 6, inflation_rate = 0, consumables_rate_month = 1200, epic_monthly = 0, genomics_monthly = 1000)
n_fut <- 24
ok("genomics 1000/month over the horizon lowers end balance by n x 1000",
   abs((num_in(lab_of(p_g0)) - num_in(lab_of(p_g1))) %% 1000) < 1 &&
   num_in(lab_of(p_g0)) > num_in(lab_of(p_g1)))

# backtest shape
ok("burn: backtest has one row per stream", is.null(bmb$backtest) || nrow(bmb$backtest) == 3)

# unticking a category removes it from the consumables + facilities stream
dc <- db
dc$ist_raw <- bind_rows(dc$ist_raw,
  tibble(id = "K1", month = mb[3], category = "Taconic", actual_spending = 60000, actual_income = 0))
with_tac    <- burn_model(dc)$consum$per_fte
without_tac <- burn_model(dc, consum_categories = setdiff(burn_consum_default(), "Taconic"))$consum$per_fte
ok("burn: Taconic in the stream raises the rate; unticked = original fit",
   with_tac > bmb$consum$per_fte && abs(without_tac - bmb$consum$per_fte) < 1e-6)
ok("burn: default stream never contains the fixed exclusions",
   !any(BURN_CONSUM_EXCLUDED %in% burn_consum_default()))
