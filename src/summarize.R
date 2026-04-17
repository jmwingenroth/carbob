#!/usr/bin/env Rscript
# Summarizes a processed imports CSV by summing QUANTITY over user-chosen
# grouping variables. Adds a YEAR column (from RPT_PERIOD) before grouping so
# YEAR can be used as a group.
#
# Usage:
#   Rscript src/summarize.R CNTRY_NAME PROD_NAME YEAR
#   Rscript src/summarize.R --input=data/processed/imports_california.csv YEAR
#   Rscript src/summarize.R --output=data/processed/by_country.csv CNTRY_NAME
#
# With no grouping vars, sums QUANTITY across the whole file.

suppressPackageStartupMessages({
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

processed_dir  <- here("data", "processed")
summaries_dir  <- here("data", "summaries")

summarize_imports <- function(group_vars = character(),
                              input  = file.path(processed_dir, "imports.csv"),
                              output = NULL) {
  dir.create(summaries_dir, recursive = TRUE, showWarnings = FALSE)
  df <- read_csv(input, show_col_types = FALSE)

  if ("RPT_PERIOD" %in% names(df)) {
    df$YEAR <- as.integer(format(as.Date(df$RPT_PERIOD), "%Y"))
  }

  missing <- setdiff(group_vars, names(df))
  if (length(missing)) {
    stop("grouping vars not found in data: ", paste(missing, collapse = ", "))
  }

  out <- df |>
    group_by(across(all_of(group_vars))) |>
    summarise(QUANTITY = sum(QUANTITY, na.rm = TRUE), .groups = "drop")

  if (is.null(output)) {
    suffix <- if (!length(group_vars)) "total" else
      tolower(paste(group_vars, collapse = "-"))
    output <- file.path(summaries_dir, sprintf("summary_%s.csv", suffix))
  }
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  write_csv(out, output)
  message(sprintf("wrote %d rows to %s", nrow(out), output))
  invisible(out)
}

if (!interactive() && identical(sys.nframe(), 0L)) {
  args <- commandArgs(trailingOnly = TRUE)
  input  <- file.path(processed_dir, "imports.csv")
  output <- NULL
  group_vars <- character()
  for (a in args) {
    if (startsWith(a, "--input="))  input  <- sub("^--input=",  "", a)
    else if (startsWith(a, "--output=")) output <- sub("^--output=", "", a)
    else group_vars <- c(group_vars, a)
  }
  summarize_imports(group_vars = group_vars, input = input, output = output)
}
