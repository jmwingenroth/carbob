# Makefile for the EIA-814 imports pipeline.
# Run from this directory (`make -C src` from the repo root also works).
#
# Override any variable on the command line, e.g.:
#   make START=2020-01 END=2023-12
#   make PORT_STATE=TEXAS
#   make GROUP_VARS="YEAR CNTRY_NAME"

START           ?= 2024-01
END             ?= 2025-12
PORT_STATE      ?= CALIFORNIA
GROUP_VARS      ?= YEAR PROD_CODE PROD_NAME CNTRY_NAME
PROD_CODES      ?= 138,117,118,130,139

ROOT       := $(abspath $(dir $(lastword $(MAKEFILE_LIST)))/..)
RSCRIPT    ?= Rscript

# Filenames produced by process.R / summarize.R (match their naming rules).
PORT_STATE_LC := $(shell echo $(PORT_STATE) | tr '[:upper:]' '[:lower:]')
GROUP_VARS_LC := $(shell echo $(GROUP_VARS) | tr '[:upper:] ' '[:lower:]-')

PROCESSED := $(ROOT)/data/processed/imports_$(PORT_STATE_LC).csv
SUMMARY   := $(ROOT)/data/summaries/summary_$(GROUP_VARS_LC).csv

.PHONY: all download process summarize clean print-config

all: $(SUMMARY)

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

clean:
	rm -f $(PROCESSED) $(SUMMARY)

print-config:
	@echo "START           = $(START)"
	@echo "END             = $(END)"
	@echo "PORT_STATE      = $(PORT_STATE)"
	@echo "GROUP_VARS      = $(GROUP_VARS)"
	@echo "PROD_CODES      = $(PROD_CODES)"
	@echo "PROCESSED       = $(PROCESSED)"
	@echo "SUMMARY         = $(SUMMARY)"
