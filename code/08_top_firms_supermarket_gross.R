library(tidyverse)
library(arrow)
library(glue)
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)
library(here)
source(here::here("code", "config_utils.R"))

output_root <- output_dir

# Read local data (produced by 00_download_wrds.R)
fundq_raw <- read_parquet(file.path(raw_dir, "compustat_fundq.parquet"))
funda_raw <- read_parquet(file.path(raw_dir, "compustat_funda.parquet"))

# Get company IDs
supermarket <- funda_raw %>%
  filter(year(datadate) >= 2017,
         naicsh == "445110",
         !is.na(sale)) %>%
  group_by(conm, gvkey) %>%
  summarize(avg = mean(sale), .groups = "drop") %>%
  arrange(desc(avg)) %>%
  head(5)

consumer_products <- funda_raw %>%
  filter(year(datadate) >= 2017,
         (substr(as.character(naicsh), 1, 3) == "322" | substr(as.character(naicsh), 1, 4) == "3256"),
         !is.na(sale)) %>%
  group_by(conm, gvkey) %>%
  summarize(avg = mean(sale), .groups = "drop") %>%
  arrange(desc(avg)) %>%
  head(10)

food <- funda_raw %>%
  filter(year(datadate) >= 2017,
         (substr(as.character(naicsh), 1, 3) == "311" | substr(as.character(naicsh), 1, 5) == "31211"),
         !is.na(sale)) %>%
  group_by(conm, gvkey) %>%
  summarize(avg = mean(sale), .groups = "drop") %>%
  arrange(desc(avg)) %>%
  head(10)

all_firms <- rbind(supermarket, consumer_products, food) %>%
  select(c(conm, gvkey))

names <- list(all_firms$conm)[[1]]
ids_search <- all_firms$gvkey

data_raw <- fundq_raw %>%
  filter(gvkey %in% ids_search) %>%
  select(gvkey, conm, datacqtr, datadate, ibq, oibdpq, saleq)

data_1 <- data_raw %>%
  mutate(qtr_c = as.yearqtr(datacqtr)) %>%
  filter(qtr_c >= "2017 Q1") %>%
  mutate(profit_margin_c = oibdpq/saleq) %>%
  filter(conm != "UNILEVER PLC" | qtr_c > "2019 Q2") #drop dups with different gvkey and values for "UNILEVER PLC"

make_plot <- function(firm) {

  data_plot <- data_1 %>%
    filter(conm == glue("{firm}")) %>%
    filter(!is.na(ibq),
           !is.na(oibdpq))

  scale_factor <- diff(range(data_plot$oibdpq))/diff(range(data_plot$profit_margin_c))

  ggplot(data = data_plot, aes(x = qtr_c)) +
    geom_col(aes(y = oibdpq, fill = "Profit (gross)")) +
    geom_line(aes(y = profit_margin_c*scale_factor, color = "Profit margin (gross)")) +
    geom_point(aes(y = profit_margin_c*scale_factor, color = "Profit margin (gross)")) +
    scale_x_yearqtr(n = 29, expand = c(0, 0), format = "%YQ%q") +
    scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~ ./scale_factor, labels = scales::percent, name = "Profit margin (gross)")) +
    labs(title = glue("{firm}")) +
    xlab(label = "") +
    ylab(label = "Profit (gross, million USD)") +
    scale_fill_manual(name = "", values = c("Profit (gross)" = "navyblue")) +
    scale_color_manual(name = "", values = c("Profit margin (gross)" = "darkorange")) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom")

}

plots <- list()
num <- 0

for (f in names) {

  num <- num + 1
  plots[[num]] <- make_plot(glue("{f}"))

}

pdf(glue("{output_root}/supermarket_goods_quarterly_plots_gross.pdf"), width = 11, height = 8.5)
print(plots)
dev.off()

# Annual margins
annual_data_raw <- funda_raw %>%
  filter(gvkey %in% ids_search) %>%
  select(gvkey, conm, datadate, datafmt, ib, oibdp, sale)

annual_summary_data <- annual_data_raw %>%
  mutate(year_c = year(datadate)) %>%
  filter(datafmt == "STD",
         year_c >= 2017 & year_c <= 2025) %>%
  filter(conm != "UNILEVER PLC" | year_c > 2018) %>% #drop dups with different gvkey and values for "UNILEVER PLC"
  mutate(profit_margin_c = oibdp/sale) %>%
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

pdf(glue("{output_root}/supermarket_goods_annual_table_gross.pdf"), width = 11, height = 8.5)
grid.arrange(top = textGrob("Annual profit margin (gross, %)", gp = gpar(fontsize = 15), vjust = 4), tableGrob(annual_summary))
dev.off()
