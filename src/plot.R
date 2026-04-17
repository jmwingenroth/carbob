#!/usr/bin/env Rscript
# Creates one PNG line chart per PROD_NAME from a summary CSV.
#   x = RPT_PERIOD if present, else YEAR; y = QUANTITY (thousands of barrels);
#   color = CNTRY_NAME.
#
# Usage:
#   Rscript src/plot.R --input=data/summaries/summary.csv
#   Rscript src/plot.R --input=... --output-dir=figures

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})

here <- function(...) {
  root <- tryCatch(
    normalizePath(file.path(dirname(sys.frame(1)$ofile), "..")),
    error = function(e) getwd()
  )
  file.path(root, ...)
}

figures_dir <- here("figures")

slug <- function(s) {
  s <- tolower(s)
  s <- gsub("[^a-z0-9]+", "_", s)
  sub("^_+|_+$", "", s)
}

# Countries separated by '|' (since names may contain commas, e.g. "KOREA, SOUTH").
parse_countries <- function(s) {
  xs <- trimws(unlist(strsplit(s, "\\|")))
  xs[nzchar(xs)]
}

make_plots <- function(input, output_dir = figures_dir, keep_countries = NULL) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  df <- read_csv(input, show_col_types = FALSE)

  required <- c("PROD_NAME", "CNTRY_NAME", "QUANTITY")
  missing <- setdiff(required, names(df))
  if (length(missing)) {
    stop("input missing required columns: ", paste(missing, collapse = ", "))
  }
  time_col <- if ("RPT_PERIOD" %in% names(df)) "RPT_PERIOD" else
              if ("YEAR" %in% names(df)) "YEAR" else
              stop("summary must contain RPT_PERIOD or YEAR")
  if (time_col == "RPT_PERIOD") df$RPT_PERIOD <- as.Date(df$RPT_PERIOD)

  ylab <- if (time_col == "YEAR") "Quantity (thousands of barrels per year)"
          else "Quantity (thousands of barrels per month)"

  if (!is.null(keep_countries) && length(keep_countries)) {
    df$CNTRY_NAME <- ifelse(df$CNTRY_NAME %in% keep_countries,
                            df$CNTRY_NAME, "Other")
    df <- df |>
      group_by(across(all_of(c("PROD_NAME", time_col, "CNTRY_NAME")))) |>
      summarise(QUANTITY = sum(QUANTITY, na.rm = TRUE), .groups = "drop")
  }

  for (p in sort(unique(df$PROD_NAME))) {
    sub <- df[df$PROD_NAME == p, , drop = FALSE]
    present <- unique(sub$CNTRY_NAME)
    country_levels <- c(sort(setdiff(present, "Other")),
                        intersect("Other", present))
    sub$CNTRY_NAME <- factor(sub$CNTRY_NAME, levels = country_levels)

    g <- ggplot(sub, aes(x = .data[[time_col]], y = QUANTITY,
                         color = CNTRY_NAME, group = CNTRY_NAME)) +
      geom_line() +
      geom_point(size = 1) +
      labs(
        title = p,
        x = NULL,
        y = ylab,
        color = "Country"
      ) +
      theme_minimal(base_size = 12)

    out <- file.path(output_dir, paste0(slug(p), ".png"))
    ggsave(out, g, width = 9, height = 5, dpi = 150)
    message(sprintf("wrote %s (%d countries)", out, length(country_levels)))
  }
}

if (!interactive() && identical(sys.nframe(), 0L)) {
  args <- commandArgs(trailingOnly = TRUE)
  input <- NULL
  output_dir <- figures_dir
  keep_countries <- NULL
  for (a in args) {
    if (startsWith(a, "--input="))           input <- sub("^--input=", "", a)
    else if (startsWith(a, "--output-dir=")) output_dir <- sub("^--output-dir=", "", a)
    else if (startsWith(a, "--keep-countries=")) {
      keep_countries <- parse_countries(sub("^--keep-countries=", "", a))
    }
  }
  if (is.null(input)) stop("--input=<summary csv> is required")
  make_plots(input, output_dir, keep_countries = keep_countries)
}
