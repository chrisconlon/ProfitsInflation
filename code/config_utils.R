# Shared configuration and path utilities for R scripts
# Source this at the top of each R script:
#   source(here::here("code", "config_utils.R"))

library(here)
library(ini)

config <- read.ini(here::here("config.ini"))

# WRDS credentials (used by: 01, 03, 04, 05, 06, 08, 09)
wrds_user     <- config$wrds$wrds_user
wrds_password <- config$wrds$wrds_password

# FRED API key (used by: 10)
fred_api_key  <- config$fred$fred_api_key

# Project-wide directory paths (relative to repo root)
raw_dir    <- here::here("raw_data")
proc_dir   <- here::here("proc_data")
output_dir <- here::here("tables_and_figures")
