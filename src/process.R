#!/usr/bin/env Rscript
# Reads EIA-814 xlsx files from data/raw/ and writes a tidy combined CSV
# to data/processed/imports.csv. Optionally filters rows by PORT_STATE.

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(readr)
})

here <- function(...) {
  root <- tryCatch(
    normalizePath(file.path(dirname(sys.frame(1)$ofile), "..")),
    error = function(e) getwd()
  )
  file.path(root, ...)
}

raw_dir       <- here("data", "raw")
processed_dir <- here("data", "processed")
dir.create(processed_dir, recursive = TRUE, showWarnings = FALSE)

read_one <- function(path) {
  df <- read_excel(path, sheet = "IMPORTS")
  df$source_file <- basename(path)
  df
}

process_imports <- function(port_state = NULL,
                            out_file = NULL,
                            files = NULL) {
  if (is.null(files)) {
    files <- list.files(raw_dir, pattern = "\\.xlsx$", full.names = TRUE)
  }
  if (!length(files)) stop("No .xlsx files found in ", raw_dir)

  message(sprintf("reading %d file(s)", length(files)))
  rows <- lapply(files, function(f) {
    message("  ", basename(f))
    read_one(f)
  })
  df <- bind_rows(rows)

  if (!is.null(port_state)) {
    states <- toupper(port_state)
    before <- nrow(df)
    df <- df[toupper(df$PORT_STATE) %in% states, , drop = FALSE]
    message(sprintf("filter PORT_STATE in {%s}: %d -> %d rows",
                    paste(states, collapse = ","), before, nrow(df)))
  }

  if (is.null(out_file)) {
    suffix <- if (is.null(port_state)) "" else
      paste0("_", tolower(paste(port_state, collapse = "-")))
    out_file <- file.path(processed_dir, sprintf("imports%s.csv", suffix))
  }

  write_csv(df, out_file)
  message(sprintf("wrote %d rows to %s", nrow(df), out_file))
  invisible(df)
}

if (!interactive() && identical(sys.nframe(), 0L)) {
  args <- commandArgs(trailingOnly = TRUE)
  port_state <- if (length(args) >= 1 && nzchar(args[1])) {
    strsplit(args[1], ",")[[1]]
  } else NULL
  out_file <- if (length(args) >= 2 && nzchar(args[2])) args[2] else NULL
  process_imports(port_state = port_state, out_file = out_file)
}
