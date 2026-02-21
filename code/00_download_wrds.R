# Download all Compustat data from WRDS and save locally as parquet files.
# Run this script once before running any analysis scripts (01, 03-05, 08-09).

library(RPostgres)
library(DBI)
library(arrow)
library(here)
source(here::here("code", "config_utils.R"))

wrds <- dbConnect(RPostgres::Postgres(),
                  dbname = "wrds",
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  user = wrds_user,
                  password = wrds_password)

# 1. Compustat Quarterly Fundamentals (COMP.FUNDQ) ----------------------------
cat("Downloading COMP.FUNDQ...\n")

res <- dbSendQuery(wrds, "
  select
    GVKEY,
    CONM,
    DATACQTR,
    DATADATE,
    INDFMT,
    DATAFMT,
    CONSOL,
    IBQ,
    OIBDPQ,
    SALEQ,
    COGSQ,
    FIC,
    CURCDQ
  from COMP.FUNDQ
  where date_part('year', datadate) >= 2010
")

fundq <- dbFetch(res)
dbClearResult(res)

cat(paste0("  FUNDQ: ", nrow(fundq), " rows\n"))
write_parquet(fundq, file.path(raw_dir, "compustat_fundq.parquet"))
cat("  Saved to raw_data/compustat_fundq.parquet\n")

# 2. Compustat Annual Fundamentals (COMP.FUNDA) -------------------------------
cat("Downloading COMP.FUNDA...\n")

res <- dbSendQuery(wrds, "
  select
    GVKEY,
    CONM,
    DATADATE,
    DATAFMT,
    INDFMT,
    CONSOL,
    NAICSH,
    IB,
    OIBDP,
    SALE
  from COMP.FUNDA
  where date_part('year', datadate) >= 2010
")

funda <- dbFetch(res)
dbClearResult(res)

cat(paste0("  FUNDA: ", nrow(funda), " rows\n"))
write_parquet(funda, file.path(raw_dir, "compustat_funda.parquet"))
cat("  Saved to raw_data/compustat_funda.parquet\n")

dbDisconnect(wrds)
cat("Done.\n")
