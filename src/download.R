#!/usr/bin/env Rscript
# Downloads EIA-814 company-level import.xlsx files into data/raw/.
# Files are published monthly at:
#   https://www.eia.gov/petroleum/imports/companylevel/archive/YYYY/YYYY_MM/data/import.xlsx

suppressPackageStartupMessages({
  library(httr)
})

here <- function(...) {
  root <- tryCatch(
    normalizePath(file.path(dirname(sys.frame(1)$ofile), "..")),
    error = function(e) getwd()
  )
  file.path(root, ...)
}

raw_dir <- here("data", "raw")
dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

build_url <- function(year, month) {
  sprintf(
    "https://www.eia.gov/petroleum/imports/companylevel/archive/%d/%d_%02d/data/import.xlsx",
    year, year, month
  )
}

dest_path <- function(year, month) {
  file.path(raw_dir, sprintf("import_%d_%02d.xlsx", year, month))
}

download_one <- function(year, month, overwrite = FALSE) {
  dest <- dest_path(year, month)
  if (!overwrite && file.exists(dest) && file.info(dest)$size > 0) {
    message(sprintf("skip  %d-%02d (exists)", year, month))
    return(invisible("skipped"))
  }
  url <- build_url(year, month)
  resp <- tryCatch(
    GET(url, write_disk(dest, overwrite = TRUE), timeout(60)),
    error = function(e) e
  )
  if (inherits(resp, "error")) {
    if (file.exists(dest)) file.remove(dest)
    message(sprintf("error %d-%02d: %s", year, month, conditionMessage(resp)))
    return(invisible("error"))
  }
  if (status_code(resp) != 200) {
    if (file.exists(dest)) file.remove(dest)
    message(sprintf("miss  %d-%02d (HTTP %d)", year, month, status_code(resp)))
    return(invisible("missing"))
  }
  message(sprintf("ok    %d-%02d", year, month))
  invisible("ok")
}

download_range <- function(start_year = 2009, start_month = 1,
                           end_year = NULL, end_month = NULL,
                           overwrite = FALSE) {
  if (is.null(end_year) || is.null(end_month)) {
    today <- Sys.Date()
    end_year  <- as.integer(format(today, "%Y"))
    end_month <- as.integer(format(today, "%m"))
  }
  start <- start_year * 12 + (start_month - 1)
  end   <- end_year  * 12 + (end_month  - 1)
  for (idx in start:end) {
    y <- idx %/% 12
    m <- (idx %% 12) + 1
    download_one(y, m, overwrite = overwrite)
  }
}

if (!interactive() && identical(sys.nframe(), 0L)) {
  args <- commandArgs(trailingOnly = TRUE)
  parse_ym <- function(s) {
    parts <- strsplit(s, "[-_/]")[[1]]
    list(y = as.integer(parts[1]), m = as.integer(parts[2]))
  }
  if (length(args) >= 2) {
    s <- parse_ym(args[1]); e <- parse_ym(args[2])
    download_range(s$y, s$m, e$y, e$m)
  } else if (length(args) == 1) {
    s <- parse_ym(args[1])
    download_range(s$y, s$m)
  } else {
    download_range()
  }
}
