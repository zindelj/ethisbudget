# ================================================================
# plots.R
# ================================================================

plot_psp <- function(ts, mode = "actual", title = "", einzelposten = NULL,
                     laufzeit_type = "fixed") {

  ts_plot <- if (mode == "actual") ts %>% filter(!is_forecast) else ts

  x_min      <- min(ts_plot$month)
  x_max      <- max(ts_plot$month)
  x_axis_min <- x_min %m-% days(15)
  x_axis_max <- max(x_max %m+% days(15), x_min %m+% months(12) %m+% days(15))

  has_forecast <- any(ts_plot$is_forecast)
  axis_font    <- list(family = "Arial", size = 11, color = "#444444")

  # ---- Forecast shading ----
  shapes <- list()
  if (mode == "prediction" && has_forecast) {
    fc_start <- min(ts_plot$month[ts_plot$is_forecast])
    shapes[[1]] <- list(
      type      = "rect", xref = "x", yref = "paper",
      x0        = format(fc_start %m-% days(15)), x1 = format(x_axis_max),
      y0        = 0, y1 = 1,
      fillcolor = "rgba(200,200,200,0.15)", line = list(width = 0), layer = "below"
    )
  }

  # ---- TOP: balance lines ----
  has_expected <- "expected_balance" %in% names(ts_plot) &&
                  any(!is.na(ts_plot$expected_balance))

  bal_vals <- c(ts_plot$actual_balance,
                if (has_expected) ts_plot$expected_balance else NULL)
  top_min  <- min(c(bal_vals, 0), na.rm = TRUE) * 1.05
  top_max  <- max(c(bal_vals, 0), na.rm = TRUE) * 1.05

  p_top <- plot_ly() %>%
    { if (has_expected)
        add_trace(., data = ts_plot, x = ~month, y = ~expected_balance,
                  type = "scatter", mode = "lines",
                  name = "Expected remaining",
                  line = list(color = "steelblue", width = 2, dash = "solid"),
                  hovertemplate = "%{x|%b '%y}<br>Expected: CHF %{y:,.0f}<extra></extra>")
      else . } %>%
    add_trace(data = ts_plot, x = ~month, y = ~actual_balance,
              type = "scatter", mode = "lines",
              name = if (mode == "actual") "Actual remaining" else "Actual / Forecast remaining",
              line = list(color = "firebrick", width = 2, dash = "dash"),
              hovertemplate = "%{x|%b '%y}<br>Remaining: CHF %{y:,.0f}<extra></extra>") %>%
    layout(
      yaxis  = list(title = "CHF remaining", tickformat = ",.0f",
                    range = list(top_min, top_max),
                    tickfont = axis_font, gridcolor = "rgba(0,0,0,0.06)"),
      xaxis  = list(range = list(x_axis_min, x_axis_max),
                    showticklabels = FALSE, gridcolor = "rgba(0,0,0,0.04)"),
      shapes        = if (length(shapes) > 0) shapes else NULL,
      legend        = list(orientation = "h", y = -0.02,
                           font = list(family = "Arial", size = 11)),
      plot_bgcolor  = "white",
      paper_bgcolor = "white"
    )

  # ---- BOTTOM: spending bars ----
  actual_months <- ts_plot %>% filter(!is_forecast) %>% pull(month)

  x_axis_bot <- list(
    range = list(x_axis_min, x_axis_max),
    tickformat = "%b '%y", dtick = "M1", tickangle = -90,
    tickfont = axis_font, gridcolor = "rgba(0,0,0,0.04)", showticklabels = TRUE
  )

  # burn_rate as constant for expected line in bottom panel
  burn_rate <- if ("burn_rate" %in% names(ts_plot)) ts_plot$burn_rate[1]
               else mean(ts_plot$actual_spending[!ts_plot$is_forecast], na.rm = TRUE)

  if (mode == "actual" && !is.null(einzelposten)) {

    cat_monthly <- einzelposten %>%
      filter(!is_income, month %in% actual_months) %>%
      mutate(category = coalesce(CATEGORY_MAP[kurztext], "Other")) %>%
      group_by(month, category) %>%
      summarise(amount = sum(betrag_in_bw, na.rm = TRUE), .groups = "drop")

    hover_by_month <- cat_monthly %>%
      group_by(month) %>%
      summarise(
        total      = sum(amount),
        hover_text = paste0(
          "<b>", format(first(month), "%b '%y"), "</b><br>",
          paste0(category, ": CHF ", format(round(amount), big.mark = "'"), collapse = "<br>"),
          "<br><b>Total: CHF ", format(round(sum(amount)), big.mark = "'"), "</b>"
        ),
        .groups = "drop"
      )

    bot_max <- max(c(hover_by_month$total, burn_rate), na.rm = TRUE) * 1.1

    p_bot <- plot_ly()

    if (!laufzeit_type %in% c("unlimited", "startup")) {
      p_bot <- p_bot %>%
        add_trace(x = list(x_axis_min, x_axis_max),
                  y = list(burn_rate, burn_rate),
                  type = "scatter", mode = "lines",
                  name = "Expected burn",
                  line = list(color = "steelblue", width = 2, dash = "dot"),
                  hoverinfo = "skip")
    }

    for (cat in CATEGORY_ORDER) {
      cat_data <- cat_monthly %>% filter(category == cat)
      if (nrow(cat_data) == 0) next
      p_bot <- p_bot %>%
        add_bars(data = cat_data, x = ~month, y = ~amount,
                 name = cat, marker = list(color = CATEGORY_COLORS[[cat]]),
                 hoverinfo = "skip")
    }

    p_bot <- p_bot %>%
      add_bars(data = hover_by_month, x = ~month, y = ~total,
               name = "Total", showlegend = FALSE,
               marker = list(color = "rgba(0,0,0,0)", line = list(width = 0)),
               text = ~hover_text, hoverinfo = "text",
               hoverlabel = list(bgcolor = "white",
                                 bordercolor = "rgba(0,0,0,0.2)",
                                 font = list(family = "Arial", color = "#1a1917", size = 12))
      ) %>%
      layout(
        barmode = "stack",
        yaxis   = list(title = "CHF / month", tickformat = ",.0f",
                       range = list(0, bot_max), tickfont = axis_font,
                       gridcolor = "rgba(0,0,0,0.06)"),
        xaxis         = x_axis_bot,
        shapes        = if (length(shapes) > 0) shapes else NULL,
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )

  } else {
    # Prediction: grouped bars
    actual_bars <- ts_plot %>% filter(!is_forecast)
    bot_max <- max(c(burn_rate, actual_bars$actual_spending), na.rm = TRUE) * 1.1

    p_bot <- plot_ly() %>%
      { if (!laufzeit_type %in% c("unlimited", "startup"))
          add_bars(., data = ts_plot, x = ~month, y = ~forecast_spending,
                   name = "Expected burn",
                   marker = list(color = "rgba(123,175,212,0.8)"),
                   hovertemplate = "%{x|%b '%y}<br>Expected: CHF %{y:,.0f}<extra></extra>")
        else . } %>%
      { if (nrow(actual_bars) > 0)
          add_bars(., data = actual_bars, x = ~month, y = ~actual_spending,
                   name = "Actual spending",
                   marker = list(color = "rgba(232,116,97,0.8)"),
                   hovertemplate = "%{x|%b '%y}<br>Actual: CHF %{y:,.0f}<extra></extra>")
        else . } %>%
      layout(
        barmode = "group",
        yaxis   = list(title = "CHF / month", tickformat = ",.0f",
                       range = list(0, bot_max), tickfont = axis_font,
                       gridcolor = "rgba(0,0,0,0.06)"),
        xaxis         = x_axis_bot,
        shapes        = if (length(shapes) > 0) shapes else NULL,
        plot_bgcolor  = "white",
        paper_bgcolor = "white"
      )
  }

  # ---- Year annotations ----
  years <- seq(year(x_min), year(x_max))
  year_annotations <- lapply(years, function(y) {
    list(x = as.character(as.Date(paste0(y, "-07-01"))),
         y = 1, yref = "paper", yanchor = "top",
         text = as.character(y), showarrow = FALSE,
         font = list(size = 11, color = "grey50", family = "Arial"),
         xanchor = "center")
  })

  # ---- Combine ----
  subplot(p_top, p_bot, nrows = 2, heights = c(0.55, 0.45),
          shareX = TRUE, titleY = TRUE) %>%
    layout(
      title       = list(text = title, font = list(size = 16, family = "Arial")),
      shapes      = if (length(shapes) > 0) shapes else NULL,
      annotations = year_annotations,
      margin      = list(t = 60)
    ) %>%
    config(scrollZoom = TRUE, displayModeBar = TRUE,
           modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"))
}
