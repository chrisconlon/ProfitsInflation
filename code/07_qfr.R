library(tidyverse)
library(glue)
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)
library(xlsx)
library(here)
source(here::here("code", "config_utils.R"))

raw_root <- file.path(raw_dir, "qfr")
output_root <- output_dir

import_data <- function(start, stop) {
  
  read.csv(glue("{raw_root}/QFR-mf.csv"), skip = start, nrows = stop)
  
}

data_raw <- import_data(276, -1)
naics_xwalk_raw <- import_data(1, 52)
var_xwalk_raw <- import_data(57, 46)
time_xwalk_raw <- import_data(162, 100)

data_1 <- data_raw %>%
  filter(et_idx == 0) %>%
  left_join(naics_xwalk_raw, by = c("cat_idx")) %>%
  left_join(var_xwalk_raw, by = c("dt_idx")) %>%
  left_join(time_xwalk_raw, by = c("per_idx")) %>%
  select(val, cat_desc, dt_desc, per_name)

nrow(data_1) == nrow(data_1 %>%
                       distinct(cat_desc, dt_desc, per_name))

get_vars <- function(var_short, var_long) {
  
  clean <- data_1 %>%
    filter(dt_desc == glue("{var_long}")) %>%
    select(cat_desc, per_name, val)
  
  colnames(clean) <- c("cat_desc", "per_name", glue("{var_short}"))
  
  print(nrow(clean))
  
  print(nrow(clean) == nrow(clean %>%
                              distinct(cat_desc, per_name)))
  
  clean
  
}

sales <- get_vars("sales", "Net Sales, Receipts, and Operating Revenues")
depr_etc <- get_vars("depr_etc", "Depreciation, Depletion, and Amortization of Property, Plant, and Equipment")
other_cost <- get_vars("other_cost", "All Other Operating Costs and Expenses")
profit <- get_vars("profit", "Income (Loss) After Income Taxes")

data_2 <- sales %>%
  inner_join(depr_etc, by = c("cat_desc", "per_name")) %>%
  inner_join(other_cost, by = c("cat_desc", "per_name")) %>%
  inner_join(profit, by = c("cat_desc", "per_name"))

nrow(data_2)

nrow(data_2) == nrow(data_2 %>%
                       distinct(cat_desc, per_name))

data_3 <- data_2 %>%
  mutate(qtr_c = as.yearqtr(per_name, format = "Q%q-%Y"))

replicate_data <- data_3 %>%
  mutate(industry = case_when(
    cat_desc == "All Retail Trade" ~ "all_retail",
    cat_desc == "Food and Beverage Stores" ~ "food_beverage_retail",
    T ~ "other"
  )) %>%
  filter(industry != "other") %>%
  select(industry, qtr_c, sales, other_cost) %>%
  pivot_wider(names_from = industry, values_from = c(sales, other_cost)) %>%
  mutate(sales_other_retail = sales_all_retail - sales_food_beverage_retail,
         other_cost_other_retail = other_cost_all_retail - other_cost_food_beverage_retail) %>%
  mutate(ratio_food_beverage_retail = (sales_food_beverage_retail - other_cost_food_beverage_retail)/sales_food_beverage_retail,
         ratio_other_retail = (sales_other_retail - other_cost_other_retail)/sales_other_retail) %>%
  mutate(ma_food_beverage_retail = rollapply(ratio_food_beverage_retail, 4, mean, fill = NA, align = "right"),
         ma_other_retail = rollapply(ratio_other_retail, 4, mean, fill = NA, align = "right")) %>%
  select(c(qtr_c, starts_with("ma_"))) %>%
  pivot_longer(cols = starts_with("ma_"), names_to = "industry", names_prefix = "ma_", values_to = "ma_c") %>%
  mutate(industry_c = gsub("d b", "d and b", gsub("_", " ", str_to_title(industry))))

ggplot(replicate_data, aes(x = qtr_c, y = ma_c, color = industry_c)) +
  geom_line() +
  scale_x_yearqtr(n = 13, expand = c(0, 0), format = "%YQ%q") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Replicate QFR plot", subtitle = "(net sales - other operating costs)/net sales\nFour quarter moving average") +
  xlab(label = "") +
  ylab(label = "") +
  scale_color_manual(values = c("blue", "orange")) +
  theme(legend.title = element_blank())

pdf(glue("{output_root}/replicate_plot.pdf"), width = 14, height = 8.5)
print(last_plot())
dev.off()

profits_margins_1 <- data_3 %>%
  filter(qtr_c >= "2017 Q1") %>%
  filter(cat_desc %in% c("All Manufacturing", "All Nondurable Manufacturing", "Food", "Paper", "Petroleum and Coal Products", "Pharmaceuticals and Medicines", "All Durable Manufacturing", "Computer and Electronic Products", "Computer and Peripheral Equipment", "Motor Vehicles and Parts", "All Mining", "All Wholesale Trade", "All Retail Trade", "Food and Beverage Stores", "All Information", "All Professional and Technical Services, Except Legal Services")) %>%
  mutate(profit_margin_c = profit/sales) %>%
  group_by(cat_desc) %>%
  mutate(profit_base_c = first(profit, order_by = qtr_c)) %>%
  ungroup() %>%
  mutate(perc_diff_profit_c = (profit - profit_base_c)/abs(profit_base_c))

profits_margins_2 <- list()

profits_margins_2[["Sector"]] <- profits_margins_1 %>%
  filter(grepl("All", cat_desc),
         cat_desc != "All Mining",
         cat_desc != "All Professional and Technical Services, Except Legal Services")

profits_margins_2[["Sector (cont'd)"]] <- profits_margins_1 %>%
  filter(cat_desc == "All Mining" | cat_desc == "All Professional and Technical Services, Except Legal Services")

profits_margins_2[["Industry"]] <- profits_margins_1 %>%
  filter(!grepl("All", cat_desc))

plots <- list()

for (l in c("Sector", "Sector (cont'd)", "Industry")) {
  
  plots[["profit"]][[glue("{l}")]] <- ggplot(profits_margins_2[[glue("{l}")]], aes(x = qtr_c, y = profit, color = cat_desc)) +
    geom_line() +
    scale_x_yearqtr(n = 52, expand = c(0, 0), format = "%YQ%q") +
    scale_y_continuous(labels = scales::comma) +
    labs(title = "QFR data - profits", subtitle = glue("{l}")) +
    xlab(label = "") +
    ylab(label = "Million USD") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom", legend.title = element_blank())
  
}

for (l in c("Sector", "Sector (cont'd)", "Industry")) {
  
  plots[["margin"]][[glue("{l}")]] <- ggplot(profits_margins_2[[glue("{l}")]], aes(x = qtr_c, y = profit_margin_c, color = cat_desc)) +
    geom_line() +
    scale_x_yearqtr(n = 52, expand = c(0, 0), format = "%YQ%q") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "QFR data - profit margins", subtitle = glue("{l}")) +
    xlab(label = "") +
    ylab(label = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom", legend.title = element_blank())
  
}

for (l in c("Sector", "Sector (cont'd)", "Industry")) {
  
  plots[["perc_diff_profit"]][[glue("{l}")]] <- ggplot(profits_margins_2[[glue("{l}")]], aes(x = qtr_c, y = perc_diff_profit_c, color = cat_desc)) +
    geom_line() +
    scale_x_yearqtr(n = 52, expand = c(0, 0), format = "%YQ%q") +
    scale_y_continuous(labels = scales::percent) +
    labs(title = "QFR data - profit growth", subtitle = glue("{l}"), caption = "Profit growth = (profit,t - profit,2017Q1)/abs(profit,2017Q1)") +
    xlab(label = "") +
    ylab(label = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom", legend.title = element_blank())
  
}

pdf(glue("{output_root}/qfr_profits.pdf"), width = 14, height = 8.5)
print(plots)
dev.off()
