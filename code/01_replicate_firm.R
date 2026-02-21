library(tidyverse)
library(arrow)
library(glue)
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)
library(here)
source(here::here("code", "config_utils.R"))

# Read local data (produced by 00_download_wrds.R)
fundq_raw <- read_parquet(file.path(raw_dir, "compustat_fundq.parquet"))
funda_raw <- read_parquet(file.path(raw_dir, "compustat_funda.parquet"))

# Get company IDs
search_name <- function(name) {

  search_result <- fundq_raw %>%
    filter(grepl(name, conm),
           !grepl("MARION", conm),
           !grepl("PRE.*FASB", conm),
           !grepl("PRO.*FORMA", conm)) %>%
    distinct(gvkey, conm)

  print(search_result)
  list(search_result$conm, search_result$gvkey)

}

names <- list()
ids <- list()
num <- 0

for (n in c("BOISE CASCADE CO", "DOW INC", "DUPONT DE NEMOURS INC", "EXXON MOBIL CORP", "NUCOR CORP", "UNITED STATES STEEL CORP", "CARMAX INC", "C H ROBINSON", "COCA-COLA CO", "DEERE & CO", "GENERAL MILLS INC", "GENERAL MOTORS CO", "HOME DEPOT INC", "LOWE% COS INC", "PEPSICO INC", "PROCTER & GAMBLE CO", "STARBUCKS CO", "TYSON FOODS INC")) {

  num <- num + 1
  print(glue("{num}"))

  # Convert SQL LIKE pattern (%) to regex (.*)
  name_regex <- gsub("%", ".*", n)
  result <- search_name(name_regex)
  names[[num]] <- result[[1]]
  ids[[num]] <- result[[2]]

}

# Confirm A.P. Moller not in data
for (n in c("ARNOLD PETER", "A\\. P\\.", "A\\.P\\.", "A P M", "A PM", "AP M", "APM", "MOLLER", "MAERSK")) {

  search_name(n)

}

fundq_raw %>%
  filter(grepl("103292", gvkey)) %>%
  distinct(gvkey, conm)

ids_search <- unlist(ids)

data_raw <- fundq_raw %>%
  filter(gvkey %in% ids_search) %>%
  select(gvkey, conm, datacqtr, datadate, ibq, saleq)

data_1 <- data_raw %>%
  mutate(qtr_c = as.yearqtr(datacqtr)) %>%
  filter(qtr_c >= "2017 Q1") %>%
  mutate(profit_margin_c = ibq/saleq)

make_plot <- function(firm) {

  data_plot <- data_1 %>%
    filter(conm == glue("{firm}"))

  scale_factor <- diff(range(data_plot$ibq))/diff(range(data_plot$profit_margin_c))

  ggplot(data = data_plot, aes(x = qtr_c)) +
    geom_col(aes(y = ibq, fill = "Profit")) +
    geom_line(aes(y = profit_margin_c*scale_factor, color = "Profit margin")) +
    scale_x_yearqtr(n = 29, expand = c(0, 0), format = "%YQ%q") +
    scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~ ./scale_factor, labels = scales::percent, name = "Profit margin")) +
    labs(title = glue("{firm}")) +
    xlab(label = "") +
    ylab(label = "Profit (million USD)") +
    scale_fill_manual(name = "", values = c("Profit" = "navyblue")) +
    scale_color_manual(name = "", values = c("Profit margin" = "darkorange")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom")

}

plots <- list()
num <- 0

for (f in names) {

  num <- num + 1
  plots[[num]] <- make_plot(glue("{f}"))

}

# pdf(glue("{output_dir}/replicate_firm.pdf"), width = 11, height = 8.5)
# print(plots)
# dev.off()

# Annual margins
annual_data_raw <- funda_raw %>%
  filter(gvkey %in% ids_search) %>%
  select(gvkey, conm, datadate, datafmt, ib, sale)

annual_summary_data <- annual_data_raw %>%
  mutate(year_c = year(datadate)) %>%
  filter(datafmt == "STD",
         year_c >= 2017 & year_c <= 2025) %>%
  mutate(profit_margin_c = ib/sale) %>%
  mutate(year_grouped_c = case_when(
    year_c < 2020 ~ "2017-19 (avg)",
    T ~ as.character(year_c)
  )) %>%
  group_by(conm, year_grouped_c) %>%
  summarize(profit_margin_c = mean(profit_margin_c)) %>%
  ungroup() %>%
  pivot_wider(names_from = year_grouped_c, values_from = profit_margin_c) %>%
  mutate_if(is.numeric, ~ .*100) %>%
  mutate_if(is.numeric, round, 2)

names_df <- as.data.frame(unlist(names))
colnames(names_df) <- c("Firm")

annual_summary <- names_df %>%
  left_join(annual_summary_data, by = c("Firm" = "conm"))

## List of firms in the Weber Wasner (2023) paper.
pdf(glue("{output_dir}/annual_margins.pdf"), width = 11, height = 8.5)
grid.arrange(top = textGrob("Annual profit margin (%)", gp = gpar(fontsize = 15), vjust = 10), tableGrob(annual_summary))
dev.off()
