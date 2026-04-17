# Makefile for the EIA-814 imports pipeline.
# Run from this directory (`make -C src` from the repo root also works).
#
# Override any variable on the command line, e.g.:
#   make START=2020-01 END=2023-12
#   make PORT_STATE=TEXAS
#   make GROUP_VARS="YEAR CNTRY_NAME"

START           ?= 2020-01
END             ?= 2025-12
PORT_STATE      ?= CALIFORNIA
GROUP_VARS      ?= YEAR PROD_NAME CNTRY_NAME
PROD_CODES      ?= 118
# Countries to keep individually; all others collapse into "Other".
# Separator is '|' because country names may contain commas (e.g. "KOREA, SOUTH").
KEEP_COUNTRIES  ?= KOREA, SOUTH|JAPAN|INDIA|NETHERLANDS

ROOT       := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/..)
RSCRIPT    ?= Rscript

# Filenames produced by process.R / summarize.R (match their naming rules).
PORT_STATE_LC := $(shell echo $(PORT_STATE) | tr '[:upper:]' '[:lower:]')
GROUP_VARS_LC := $(shell echo $(GROUP_VARS) | tr '[:upper:] ' '[:lower:]-')

PROCESSED := $(ROOT)/data/processed/imports_$(PORT_STATE_LC).csv
SUMMARY   := $(ROOT)/data/summaries/summary_$(GROUP_VARS_LC).csv
FIGURES_DIR := $(ROOT)/figures
FIGURES_STAMP := $(FIGURES_DIR)/.stamp

.PHONY: all download process summarize plot clean print-config

all: $(FIGURES_STAMP)

download:
	cd $(ROOT) && $(RSCRIPT) src/download.R $(START) $(END)

process: $(PROCESSED)
$(PROCESSED): | download
	cd $(ROOT) && $(RSCRIPT) src/process.R $(PORT_STATE)

summarize: $(SUMMARY)
$(SUMMARY): $(PROCESSED)
	cd $(ROOT) && $(RSCRIPT) src/summarize.R \
		--input=$(PROCESSED) --output=$(SUMMARY) \
		--prod-codes=$(PROD_CODES) $(GROUP_VARS)

plot: $(FIGURES_STAMP)
$(FIGURES_STAMP): $(SUMMARY)
	cd $(ROOT) && $(RSCRIPT) src/plot.R \
		--input=$(SUMMARY) --output-dir=$(FIGURES_DIR) \
		--keep-countries="$(KEEP_COUNTRIES)"
	@touch $(FIGURES_STAMP)

clean:
	rm -f $(PROCESSED) $(SUMMARY)
	rm -rf $(FIGURES_DIR)

print-config:
	@echo "START           = $(START)"
	@echo "END             = $(END)"
	@echo "PORT_STATE      = $(PORT_STATE)"
	@echo "GROUP_VARS      = $(GROUP_VARS)"
	@echo "PROD_CODES      = $(PROD_CODES)"
	@echo "KEEP_COUNTRIES  = $(KEEP_COUNTRIES)"
	@echo "PROCESSED       = $(PROCESSED)"
	@echo "SUMMARY         = $(SUMMARY)"
	@echo "FIGURES_DIR     = $(FIGURES_DIR)"
