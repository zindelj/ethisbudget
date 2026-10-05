#!/usr/bin/env Rscript
# ================================================================
# handover.R — write an anonymised budget handover file (no Shiny needed)
# ================================================================
# Usage:
#   Rscript handover.R <data folder> [--names] [--window N]
#   Rscript handover.R                 (Windows: a folder picker opens)
#
# Reads the newest export_YYYYMMDD_HHMMSS.xlsx in the folder exactly like
# the app does, then writes handover_<date>.md (safe to share: no person
# names, Buchungstexte or account numbers) and handover_<date>_KEY.txt
# (label -> account mapping, keep local) into the same folder.
#   --names     include konto Bezeichnungen (grant titles) in the summary
#   --window N  months for the run-rate section (default 12)
# The logic lives in app.R (build_handover); this script only loads it.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(purrr); library(stringr); library(readr)
  library(lubridate); library(readxl); library(writexl); library(janitor); library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
include_names <- "--names" %in% args
window <- 12
if (any(args == "--window")) {
  i <- which(args == "--window")[1]
  if (i < length(args)) window <- as.integer(args[i + 1]) else stop("--window needs a number")
  args <- args[-c(i, i + 1)]
}
args <- args[args != "--names"]
data_dir <- if (length(args) >= 1) args[1] else NULL

if (is.null(data_dir) || !nzchar(data_dir)) {
  if (.Platform$OS.type == "windows") {
    data_dir <- utils::choose.dir(caption = "Select the budget data folder")
    if (is.na(data_dir)) quit(status = 1)
  } else {
    stop("Usage: Rscript handover.R <data folder> [--names] [--window N]")
  }
}
if (!dir.exists(data_dir)) stop("Folder not found: ", data_dir)

# app.R sits next to this script (works via Rscript --file= and via source())
script_path <- sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE))
app_dir <- if (length(script_path) == 1) dirname(normalizePath(script_path)) else getwd()
app_file <- file.path(app_dir, "app.R")
if (!file.exists(app_file)) stop("app.R not found next to handover.R (looked in ", app_dir, ")")

# Load every top-level definition from app.R except the Shiny UI/server and
# the library() calls (shiny, plotly, ... are not needed for the export).
for (e in parse(app_file, encoding = "UTF-8")) {
  if (is.call(e) && as.character(e[[1]]) %in% c("<-", "=")) {
    nm <- gsub("`", "", deparse(e[[2]]))
    if (!nm %in% c("ui", "server")) eval(e, envir = globalenv())
  }
}

files <- list.files(data_dir, pattern = "^export_\\d{8}_\\d{6}\\.xlsx$")
if (length(files) == 0) stop("No export_YYYYMMDD_HHMMSS.xlsx in ", data_dir)
ep_path <- file.path(data_dir, files[order(files, decreasing = TRUE)][1])
message("Reading ", basename(ep_path), " ...")

d   <- load_all_data(ep_path)
res <- build_handover(d, include_konto_names = include_names, window_months = window)

cat("\nShare this:  ", res$md, "\n", sep = "")
cat("Keep local:  ", res$key, "\n", sep = "")
if (res$scrub_hits > 0) {
  cat("\nWARNING: the scrub pass replaced ", res$scrub_hits,
      " string(s) that looked like identifiers. Review the KEY file before sharing.\n", sep = "")
}
