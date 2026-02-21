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

# Weber-Wasner (2023) firms — GVKEYs from Compustat
weber_wasner_firms <- tribble(
  ~gvkey,   ~conm,
  "016486", "BOISE CASCADE CO",
  "034443", "DOW INC",
  "004060", "DUPONT DE NEMOURS INC",
  "004503", "EXXON MOBIL CORP",
  "008030", "NUCOR CORP",
  "023978", "UNITED STATES STEEL CORP",
  "064410", "CARMAX INC",
  "065609", "C H ROBINSON WORLDWIDE INC",
  "003144", "COCA-COLA CO",
  "003835", "DEERE & CO",
  "005071", "GENERAL MILLS INC",
  "005073", "GENERAL MOTORS CO",
  "005680", "HOME DEPOT INC",
  "006829", "LOWE'S COS INC",
  "008479", "PEPSICO INC",
  "008762", "PROCTER & GAMBLE CO",
  "025434", "STARBUCKS CORP",
  "010793", "TYSON FOODS INC  -CL A"
)

names <- weber_wasner_firms$conm
ids_search <- weber_wasner_firms$gvkey

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

plots <- lapply(names, function(f) make_plot(f))

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

names_df <- data.frame(Firm = names)

annual_summary <- names_df %>%
  left_join(annual_summary_data, by = c("Firm" = "conm"))

## List of firms in the Weber Wasner (2023) paper.
pdf(glue("{output_dir}/annual_margins.pdf"), width = 11, height = 8.5)
grid.arrange(top = textGrob("Annual profit margin (%)", gp = gpar(fontsize = 15), vjust = 10), tableGrob(annual_summary))
dev.off()
