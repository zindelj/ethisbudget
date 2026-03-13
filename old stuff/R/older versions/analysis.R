# ================================================================
# analysis.R
# ================================================================

build_psp_timeseries <- function(projects_df, einzelposten, laufzeit_df,
                                 psp_id         = "all",
                                 burn_from      = 6,
                                 burn_to        = 1,
                                 horizon_years  = 4,
                                 mode           = "actual",
                                 reference_date = Sys.Date(),
                                 current_headcount = 1,
                                 new_hires      = NULL,   # tibble: rolle, start_jahr
                                 lohntabelle    = NULL) {

  reference_date  <- as.Date(reference_date)
  reference_month <- floor_date(reference_date, "month")
  current_jan     <- floor_date(reference_date, "year")

  startup_ids   <- laufzeit_df %>% filter(laufzeit_type == "startup")   %>% pull(id)
  unlimited_ids <- laufzeit_df %>% filter(laufzeit_type == "unlimited") %>% pull(id)
  annual_ids    <- laufzeit_df %>% filter(laufzeit_type == "annual")    %>% pull(id)
  fixed_ids     <- laufzeit_df %>% filter(laufzeit_type == "fixed")     %>% pull(id)

  # ----------------------------------------------------------------
  # Global consumables per head (all non-startup PSPs, burn window)
  # Computed once, used for all new-hire calculations
  # ----------------------------------------------------------------
  window_end_global   <- floor_date(reference_date, "month") %m-% months(burn_to)
  window_start_global <- floor_date(reference_date, "month") %m-% months(burn_from)

  global_consumables_per_head <- {
    if (!is.null(new_hires) && nrow(new_hires) > 0 &&
        !is.null(lohntabelle) && current_headcount > 0) {
      ep_global <- einzelposten %>%
        filter(!id %in% startup_ids, !is_income, !is_salary,
               month >= window_start_global, month <= window_end_global)
      avg <- ep_global %>%
        group_by(month) %>%
        summarise(s = sum(betrag_in_bw, na.rm = TRUE), .groups = "drop") %>%
        summarise(avg = mean(s, na.rm = TRUE)) %>% pull(avg)
      if (is.na(avg)) 0 else avg / current_headcount
    } else 0
  }

  # ----------------------------------------------------------------
  # Compute monthly new-hire cost from reference_month onward
  # Returns named vector: month -> extra_cost
  # ----------------------------------------------------------------
  compute_newhire_cost <- function(months_vec) {
    if (is.null(new_hires) || nrow(new_hires) == 0 ||
        is.null(lohntabelle) || current_headcount <= 0) {
      return(setNames(rep(0, length(months_vec)), as.character(months_vec)))
    }

    consumables_per_head <- global_consumables_per_head

    # For each new hire, compute monthly cost trajectory
    extra <- sapply(months_vec, function(m) {
      months_since_ref <- as.integer(interval(reference_month, m) / months(1))
      total <- 0
      for (i in seq_len(nrow(new_hires))) {
        hire      <- new_hires[i, ]
        hire_year <- hire$start_jahr
        # which salary bracket year are we in?
        bracket_year <- min(hire_year + floor(months_since_ref / 12),
                            max(lohntabelle$jahr))
        sal_row <- lohntabelle %>%
          filter(rolle == hire$rolle, jahr == bracket_year)
        if (nrow(sal_row) == 0) next
        total <- total + sal_row$monatlich + consumables_per_head
      }
      total
    })
    setNames(extra, as.character(months_vec))
  }

  # ----------------------------------------------------------------
  # Resolve per-PSP info
  # ----------------------------------------------------------------
  resolve_psp <- function(pid) {
    row <- laufzeit_df %>% filter(id == pid)
    if (nrow(row) == 0) stop("No laufzeit configured for PSP: ", pid)
    type <- row$laufzeit_type
    p    <- projects_df %>% filter(id == pid)

    if (type == "fixed") {
      start <- row$start_date; end <- row$end_date
      if (is.na(start) || is.na(end)) stop("Fixed PSP ", pid, " needs start + end date.")
      n_months <- max(interval(floor_date(start, "month"),
                               floor_date(end,   "month")) / months(1) + 1, 1)
      list(type = "fixed", monthly_expected = p$budget / n_months,
           start_date = start, end_date = end, total_budget = p$budget)
    } else if (type == "annual") {
      list(type = "annual", monthly_expected = p$budget / 12,
           start_date = current_jan,
           end_date   = current_jan %m+% years(horizon_years),
           total_budget = p$budget * horizon_years)
    } else {
      list(type = type, monthly_expected = NA_real_,
           start_date = current_jan, end_date = current_jan %m+% years(horizon_years),
           total_budget = p$budget)
    }
  }

  # ----------------------------------------------------------------
  # ALL view
  # ----------------------------------------------------------------
  if (psp_id == "all") {
    proj_ids <- projects_df %>% filter(!id %in% startup_ids) %>% pull(id)

    x_start <- current_jan
    x_end   <- if (mode == "actual") reference_month
                else current_jan %m+% years(horizon_years)

    all_months   <- tibble(month = seq(x_start, x_end, by = "1 month"))
    window_end   <- reference_month %m-% months(burn_to)
    window_start <- reference_month %m-% months(burn_from)

    ts_list <- lapply(proj_ids, function(pid) {
      info   <- resolve_psp(pid)
      ep_pid <- einzelposten %>% filter(id == pid, !is_income)

      actual_m <- ep_pid %>%
        group_by(month) %>%
        summarise(actual_spending = sum(betrag_in_bw, na.rm = TRUE), .groups = "drop")

      if (info$type == "fixed") {
        spend_before_jan <- ep_pid %>%
          filter(month < current_jan) %>%
          summarise(s = sum(betrag_in_bw, na.rm = TRUE)) %>% pull(s)
        opening_balance <- info$total_budget - spend_before_jan
        months_left     <- max(interval(current_jan, floor_date(info$end_date, "month")) / months(1), 1)
        monthly_exp     <- opening_balance / months_left
        budget_ref      <- opening_balance
        end_date        <- info$end_date
      } else {
        opening_balance <- info$total_budget
        monthly_exp     <- info$monthly_expected
        budget_ref      <- info$total_budget
        end_date        <- info$end_date
      }

      all_months %>%
        left_join(actual_m, by = "month") %>%
        mutate(
          actual_spending = replace_na(actual_spending, 0),
          expected_burn   = if_else(
            !is.na(monthly_exp) & (info$type != "fixed" | month <= floor_date(end_date, "month")),
            monthly_exp, 0
          ),
          pid       = pid,
          budget_ref = budget_ref
        )
    })

    ep_all      <- einzelposten %>% filter(id %in% proj_ids, !id %in% startup_ids)
    burn_window <- ep_all %>% filter(month >= window_start, month <= window_end)
    newhire_vec <- compute_newhire_cost(
      months_vec = all_months$month[all_months$month > window_end]
    )

    ts_agg <- bind_rows(ts_list) %>%
      group_by(month) %>%
      summarise(
        actual_spending = sum(actual_spending, na.rm = TRUE),
        expected_burn   = sum(expected_burn,   na.rm = TRUE),
        total_budget    = sum(budget_ref,      na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(month) %>%
      mutate(
        is_forecast = month > window_end,
        burn_rate   = {
          bm <- ep_all %>% filter(!is_income) %>%
            group_by(month) %>% summarise(s = sum(betrag_in_bw, na.rm = TRUE), .groups = "drop")
          br <- bm %>% filter(month >= window_start, month <= window_end) %>%
            summarise(avg = mean(s, na.rm = TRUE)) %>% pull(avg)
          if (is.na(br) || br == 0) mean(expected_burn) else br
        },
        newhire_cost    = c(rep(0, sum(!is_forecast)),
                            newhire_vec[as.character(month[is_forecast])]),
        expected_balance = total_budget - cumsum(expected_burn),
        actual_balance   = total_budget - cumsum(
          if_else(is_forecast, burn_rate + newhire_cost, actual_spending)
        )
      )

    return(ts_agg)
  }

  # ----------------------------------------------------------------
  # Single PSP
  # ----------------------------------------------------------------
  proj <- projects_df %>% filter(id == psp_id)
  if (nrow(proj) == 0) stop("PSP not found: ", psp_id)

  info <- resolve_psp(psp_id)

  if (info$type == "fixed") {
    x_start <- floor_date(info$start_date, "month")
    x_end   <- if (mode == "actual") reference_month
                else floor_date(info$end_date, "month")
  } else {
    x_start <- current_jan
    x_end   <- if (mode == "actual") reference_month
                else current_jan %m+% years(horizon_years)
  }

  all_months   <- tibble(month = seq(x_start, x_end, by = "1 month"))
  window_end   <- reference_month %m-% months(burn_to)
  window_start <- reference_month %m-% months(burn_from)

  ep          <- einzelposten %>% filter(id == psp_id)
  burn_window <- ep %>% filter(month >= window_start, month <= window_end)

  actual_monthly <- ep %>%
    filter(!is_income) %>%
    group_by(month) %>%
    summarise(actual_spending = sum(betrag_in_bw, na.rm = TRUE), .groups = "drop")

  burn_rate <- ep %>%
    filter(!is_income, month >= window_start, month <= window_end) %>%
    group_by(month) %>%
    summarise(s = sum(betrag_in_bw, na.rm = TRUE), .groups = "drop") %>%
    summarise(avg = mean(s, na.rm = TRUE)) %>% pull(avg)
  if (is.na(burn_rate) || burn_rate == 0) burn_rate <- info$monthly_expected %||% 0

  newhire_vec <- compute_newhire_cost(
    months_vec = all_months$month[all_months$month > window_end]
  )

  total_budget <- if (info$type == "annual") proj$budget else info$total_budget

  ts <- all_months %>%
    left_join(actual_monthly, by = "month") %>%
    mutate(
      actual_spending  = replace_na(actual_spending, 0),
      expected_burn    = if_else(
        !is.na(info$monthly_expected) &
          (info$type != "fixed" | month <= floor_date(info$end_date, "month")),
        info$monthly_expected, 0
      ),
      is_forecast      = month > window_end,
      newhire_cost     = c(rep(0, sum(!is_forecast)),
                           newhire_vec[as.character(month[is_forecast])]),
      expected_balance = total_budget - cumsum(expected_burn),
      actual_balance   = total_budget - cumsum(
        if_else(is_forecast, burn_rate + newhire_cost, actual_spending)
      )
    )

  ts
}
