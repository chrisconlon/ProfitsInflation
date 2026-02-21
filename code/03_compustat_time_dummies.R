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

# Weber-Wasner (2023) firms — GVKEYs from Compustat
weber_wasner_ids <- c("016486", "034443", "004060", "004503", "008030", "023978",
                      "064410", "065609", "003144", "003835", "005071", "005073",
                      "005680", "006829", "008479", "008762", "025434", "010793")

data_raw <- fundq_raw %>%
  select(gvkey, conm, datacqtr, datadate, indfmt, datafmt, consol, ibq, saleq, cogsq)

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
         ibq != 0,
         saleq > 0,
         cogsq > 0) %>%
  filter(consol == "C") %>%
  mutate(profit_margin_c = ibq/saleq) %>%
  mutate(flag_weber_c = case_when(
    gvkey %in% weber_wasner_ids ~ 1,
    T ~ 0
  )) %>%
  mutate(flag_c = 1)

nrow(data_3) == nrow(data_3 %>%
                       distinct(gvkey, qtr_c))

nrow(data_3) == nrow(data_3 %>%
                       filter(indfmt == "INDL",
                              datafmt == "STD"))

data_3 %>%
  filter(flag_weber_c == 1) %>%
  distinct(conm)

data_trimmed <- data_3 %>%
  mutate(bottom_c = quantile(profit_margin_c, c(0.005)),
         top_c = quantile(profit_margin_c, 0.995)) %>%
  filter(profit_margin_c >= bottom_c,
         profit_margin_c <= top_c)

n_firms <- scales::comma(n_distinct(data_trimmed$gvkey))

data_reg <- data_trimmed %>%
  arrange(gvkey, qtr_c)

data_reg_weber <- data_trimmed %>%
  filter(flag_weber_c == 1) %>%
  arrange(gvkey, qtr_c)

run_reg <- function(df, sample, wgt, wgt_lab) {

  reg <- felm(profit_margin_c ~ as.factor(qtr_c) | gvkey, data = df, weights = df[[glue("{wgt}")]])

  as.data.frame(summary(reg)$coefficients) %>%
    rownames_to_column(., var = "qtr_temp") %>%
    mutate(qtr_c = as.yearqtr(gsub("as.factor\\(qtr_c\\)", "", qtr_temp))) %>%
    select(qtr_c, Estimate) %>%
    mutate(lab = glue("{sample}, {wgt_lab}"),
           Sample = glue("{sample}"))

}

unweighted_full <- run_reg(
  df = data_reg,
  sample = "Full sample",
  wgt = "flag_c",
  wgt_lab = "Unweighted"
)

unweighted_weber <- run_reg(
  df = data_reg_weber,
  sample = "WW firms",
  wgt = "flag_c",
  wgt_lab = "Unweighted"
)

sales_full <- run_reg(
  df = data_reg,
  sample = "Full sample",
  wgt = "saleq",
  wgt_lab = "Weighted by sales"
)

sales_weber <- run_reg(
  df = data_reg_weber,
  sample = "WW firms",
  wgt = "saleq",
  wgt_lab = "Weighted by sales"
)

cogs_full <- run_reg(
  df = data_reg,
  sample = "Full sample",
  wgt = "cogsq",
  wgt_lab = "Weighted by COGS"
)

cogs_weber <- run_reg(
  df = data_reg_weber,
  sample = "WW firms",
  wgt = "cogsq",
  wgt_lab = "Weighted by COGS"
)

unweighted <- rbind(unweighted_full, unweighted_weber)
weighted <- rbind(sales_full, cogs_full, sales_weber, cogs_weber)

make_plot <- function(df, colors) {

  ggplot(data = df, aes(x = qtr_c, y = Estimate, color = lab, linetype = Sample)) +
    geom_line(linewidth = 1) +
    scale_x_yearqtr(n = 29, expand = c(0, 0), format = "%YQ%q") +
    labs(title = "Time dummies from regression of profit margin on firm and year-qtr dummies", caption = glue("Note: 2017Q1 onwards. {n_firms} firms. Restricted to positive sales and COGS and non-zero profits. Trimmed to middle 99% margins.\nSource: Compustat, calculations by cconlon@stern.nyu.edu")) +
    xlab(label = "") +
    ylab(label = "") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom", legend.title = element_blank(), plot.caption = element_text(hjust = 0)) +
    scale_color_manual(name = "lab", values = colors)

}

plots <- list()
plots[[1]] <- make_plot(unweighted, colors = c("blue", "darkred"))
plots[[2]] <- make_plot(weighted, colors = c("darkgreen", "blue", "darkorange", "darkred"))

pdf(glue("{output_root}/compustat_time_dummies.pdf"), width = 12, height = 8.5)
print(plots)
dev.off()
