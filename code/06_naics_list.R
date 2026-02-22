library(tidyverse)
library(RPostgres)
library(DBI)
library(glue)
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)
library(data.table)
library(lfe)
library(openxlsx)

library(here)
source(here::here("code", "config_utils.R"))

output_root <- output_dir

wrds <- dbConnect(RPostgres::Postgres(),
                  dbname = "wrds",
                  host = "wrds-pgdata.wharton.upenn.edu",
                  port = 9737,
                  user = wrds_user,
                  password = wrds_password)

data_import <- read.csv(file.path(proc_dir, "05_compustat_build.csv"), colClasses = "character")
naics_xwalk_import <- openxlsx::read.xlsx(file.path(raw_dir, "2-6 digit_2017_Codes.xlsx"), sheet = "tbl_2017_title_description_coun")
colnames(naics_xwalk_import) <- c("no", "naics_code_import", "naics_title", "extra1", "extra2")

data_1 <- data_import %>%
  select(-c(X)) %>%
  mutate_at(vars(ibq, saleq, cogsq, year_merge_c, naicsh, profit_margin_c, flag_c, sales_wgt_c, cogs_wgt_c), as.numeric) %>%
  mutate(datadate = as.Date(datadate),
         qtr_c = as.yearqtr(qtr_c))

nrow(data_1) == nrow(data_1 %>%
                       distinct(gvkey, conm, qtr_c))

data_2 <- data_1 %>%
  filter(qtr_c == "2019 Q4") %>%
  rename("firm_rev_c" = "saleq")

data_3 <- data_2 %>%
  group_by(naicsh, industry_c) %>%
  mutate(ind_sum_rev_c = sum(firm_rev_c, na.rm = T)) %>%
  ungroup()

data_4 <- data_3 %>%
  group_by(naicsh, industry_c) %>%
  slice_max(firm_rev_c, n = 3) %>%
  ungroup()

data_5 <- data_4 %>%
  mutate(flag_supermarket_c = case_when(
    industry_c == "supermarket" ~ 1,
    T ~ 0
  ),
  flag_consumer_products_c = case_when(
    industry_c == "consumer_products" ~ 1,
    T ~ 0
  ),
  flag_food_c = case_when(
    industry_c == "food" ~ 1,
    T ~ 0
  )) %>%
  select(-c(industry_c))

naics_xwalk_1 <- naics_xwalk_import %>%
  select(-c(no, extra1, extra2)) %>%
  filter(!is.na(naics_code_import)) %>%
  mutate(naics_code = as.numeric(naics_code_import))

naics_xwalk_1 %>%
  filter(is.na(naics_code))

data_1 %>%
  filter(naicsh < 100) %>%
  distinct(naicsh)

naics_xwalk_2 <- naics_xwalk_1 %>%
  filter(!is.na(naics_code)) %>%
  select(-c(naics_code_import))

nrow(naics_xwalk_2) == nrow(naics_xwalk_2 %>%
                              distinct(naics_code))

data_6 <- data_5 %>%
  left_join(naics_xwalk_2, by = c("naicsh" = "naics_code"))

data_7 <- data_6 %>%
  arrange(desc(ind_sum_rev_c), desc(firm_rev_c)) %>%
  select("naics_name" = "naics_title",
         "naics_code" = "naicsh",
         "industry_rev_c" = "ind_sum_rev_c",
         "gvkey",
         "firm_name" = "conm",
         "firm_rev_c",
         "flag_supermarket_c",
         "flag_consumer_products_c",
         "flag_food_c")

wb <- createWorkbook()
addWorksheet(wb, "Sheet1")
writeData(wb, sheet = 1, x = data_7)
setColWidths(wb, 1, cols = 1:ncol(data_7), widths = "auto")
saveWorkbook(wb, file.path(output_root, "06_naics_compustat.xlsx"), overwrite = T)
