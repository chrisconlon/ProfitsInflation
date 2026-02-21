library(tidyverse)
library(arrow)
library(glue)
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)
library(data.table)
library(lfe)
library(here)
source(here::here("code", "config_utils.R"))

output_root <- output_dir

# Read local data (produced by 00_download_wrds.R)
fundq_raw <- read_parquet(file.path(raw_dir, "compustat_fundq.parquet"))
funda_raw <- read_parquet(file.path(raw_dir, "compustat_funda.parquet"))

data_raw <- fundq_raw %>%
  select(gvkey, conm, datacqtr, datadate, indfmt, datafmt, consol, ibq, oibdpq, saleq, cogsq, fic, curcdq)

nrow(data_raw) == nrow(data_raw %>%
                         distinct(gvkey, conm, datacqtr, datadate))

data_1 <- data_raw %>%
  mutate(qtr_c = as.yearqtr(datacqtr)) %>%
  filter(qtr_c >= "2017 Q1") %>%
  mutate(year_merge_c = year(datadate))

naics_xwalk_raw <- funda_raw %>%
  distinct(gvkey, conm, datadate, naicsh)

naics_xwalk_1 <- naics_xwalk_raw %>%
  filter(!is.na(naicsh)) %>%
  mutate(year_merge_c = year(datadate)) %>%
  group_by(gvkey, conm, year_merge_c) %>%
  mutate(max_date_c = max(datadate)) %>%
  ungroup() %>%
  filter(datadate == max_date_c) %>%
  select(gvkey, conm, year_merge_c, naicsh)

data_2 <- data_1 %>%
  left_join(naics_xwalk_1, by = c("gvkey", "conm", "year_merge_c"))

nrow(data_2 %>%
       filter(is.na(ibq) & is.na(saleq) & is.na(cogsq)))

nrow(data_2 %>%
       filter(is.na(ibq) | is.na(saleq) | is.na(cogsq)))

nrow(data_2 %>%
       filter(is.na(ibq) | is.na(saleq) | is.na(cogsq),
              is.na(naicsh)))

nrow(data_2 %>%
       filter(is.na(ibq) | is.na(saleq) | is.na(cogsq) | ibq == 0 | saleq <= 0 | cogsq <= 0))

nrow(data_2 %>%
       filter(is.na(ibq) | is.na(saleq) | is.na(cogsq) | ibq == 0 | saleq <= 0 | cogsq <= 0,
              is.na(naicsh)))

data_3 <- data_2 %>%
  filter(!is.na(ibq),
         !is.na(saleq),
         !is.na(cogsq),
         !is.na(oibdpq),
         ibq != 0,
         oibdpq != 0,
         saleq > 0,
         cogsq > 0) %>%
  filter(consol == "C") %>%
  mutate(profit_margin_c = oibdpq/saleq) %>%
  mutate(industry_c = case_when(
    naicsh == "445110" ~ "supermarket",
    substr(naicsh, 1, 3) == "322" | substr(naicsh, 1, 4) == "3256" ~ "consumer_products",
    substr(naicsh, 1, 3) == "311" | substr(naicsh, 1, 5) == "31211" ~ "food",
    T ~ "other"
  )) %>%
  mutate(flag_c = 1)

nrow(data_3) == nrow(data_3 %>%
                       distinct(gvkey, qtr_c))

nrow(data_3) == nrow(data_3 %>%
                       filter(indfmt == "INDL",
                              datafmt == "STD"))

# Check top firms
for (i in c("supermarket", "consumer_products", "food")) {

  print(glue("{i}"))

  print(data_3 %>%
          filter(industry_c == glue("{i}")) %>%
          group_by(conm, fic, curcdq) %>%
          summarize(avg_sale_c = mean(saleq)) %>%
          ungroup() %>%
          arrange(desc(avg_sale_c)))

}

weights_xwalk <- data_3 %>%
  filter(as.numeric(substr(datacqtr, 1, 4)) <= 2019) %>%
  group_by(gvkey, conm) %>%
  summarize(sales_wgt_c = mean(saleq, na.rm = T),
            cogs_wgt_c = mean(cogsq, na.rm = T)) %>%
  ungroup()

data_4 <- data_3 %>%
  left_join(weights_xwalk, by = c("gvkey", "conm"))

# write.csv(data_4, "/scratch/rg4649/05_compustat_build.csv")

data_trimmed <- data_4 %>%
  mutate(bottom_c = quantile(profit_margin_c, c(0.005)),
         top_c = quantile(profit_margin_c, 0.995)) %>%
  filter(profit_margin_c >= bottom_c,
         profit_margin_c <= top_c) %>%
  filter(!is.na(sales_wgt_c),
         !is.na(cogs_wgt_c))

n_firms <- scales::comma(n_distinct(data_trimmed$gvkey))

run_reg <- function(df, sample, wgt, wgt_lab) {

  reg <- felm(profit_margin_c ~ as.factor(qtr_c) | gvkey, data = df, weights = df[[glue("{wgt}")]])

  as.data.frame(summary(reg)$coefficients) %>%
    rownames_to_column(., var = "qtr_temp") %>%
    mutate(qtr_c = as.yearqtr(gsub("as.factor\\(qtr_c\\)", "", qtr_temp))) %>%
    select(qtr_c, Estimate) %>%
    mutate(Wgt = glue("{wgt_lab}"),
           Sample = glue("{sample}"))

}

data_reg <- list()

data_reg[["full"]] <- data_trimmed %>%
  arrange(gvkey, qtr_c)

for (i in c("supermarket", "consumer_products", "food")) {

  data_reg[[glue("{i}")]] <- data_trimmed %>%
    filter(industry_c == glue("{i}")) %>%
    arrange(gvkey, qtr_c)

}

labels <- list()
labels[["full"]] <- "Full sample"

for (i in c("supermarket", "consumer_products", "food")) {

  labels[[glue("{i}")]] <- gsub("_", " ", str_to_title(glue("{i}")))

}

unweighted_list <- list()
sales_list <- list()
cogs_list <- list()

for (i in c("full", "supermarket", "consumer_products", "food")) {

  unweighted_list[[glue("{i}")]] <- run_reg(
    df = data_reg[[glue("{i}")]],
    sample = labels[[glue("{i}")]],
    wgt = "flag_c",
    wgt_lab = "Unweighted"
  )

  sales_list[[glue("{i}")]] <- run_reg(
    df = data_reg[[glue("{i}")]],
    sample = labels[[glue("{i}")]],
    wgt = "sales_wgt_c",
    wgt_lab = "Weighted by 2017-19 sales"
  )

  cogs_list[[glue("{i}")]] <- run_reg(
    df = data_reg[[glue("{i}")]],
    sample = labels[[glue("{i}")]],
    wgt = "cogs_wgt_c",
    wgt_lab = "Weighted by 2017-19 COGS"
  )

}

unweighted_stacked <- bind_rows(unweighted_list)
sales_stacked <- bind_rows(sales_list)
cogs_stacked <- bind_rows(cogs_list)
weighted_stacked <- rbind(sales_stacked, cogs_stacked)

make_plot <- function(df) {

  ggplot(data = df, aes(x = qtr_c, y = Estimate, color = Sample, linetype = Wgt)) +
    geom_line() +
    scale_x_yearqtr(n = 29, expand = c(0, 0), format = "%YQ%q") +
    labs(title = "Time dummies from regression of profit margin (gross) on firm and year-qtr dummies", caption = glue("Note: \n Data from Compustat. 2017Q1 onwards. {n_firms} firms. Restricted to positive sales and COGS and non-zero profits. Trimmed to middle 99% margins. \n Supermarkets are NAICS 445110 (Supermarkets and Other Grocery (except Convenience) Stores). Consumer products are NAICS 322 (Paper Manufacturing) and 3256 (Soap, Cleaning Compound, and \n Toilet Preparation Manufacturing). Food is NAICS 311 (Food Manufacturing) and 31211 (Soft Drink and Ice Manufacturing). \n Source: Compustat, calculations by cconlon@stern.nyu.edu")) +
    xlab(label = "") +
    ylab(label = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom", legend.title = element_blank(), plot.caption = element_text(hjust = 0))

}

plots <- list()
plots[[1]] <- make_plot(unweighted_stacked)
plots[[2]] <- make_plot(weighted_stacked)

pdf(glue("{output_root}/supermarket_goods_time_dummies_gross.pdf"), width = 12, height = 8.5)
print(plots)
dev.off()
