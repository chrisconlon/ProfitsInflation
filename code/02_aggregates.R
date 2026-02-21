library(tidyverse)
library(glue)
library(zoo)
library(ggplot2)
library(gridExtra)
library(grid)
library(openxlsx)
library(here)
source(here::here("code", "config_utils.R"))

raw_root <- raw_dir
output_root <- output_dir

ggsave_c <- function(filepath, plots_list) {
  
  pdf(glue("{filepath}"), width = 14, height = 8.5)
  print(plots_list)
  dev.off()
  
}

# Clean data -------------------------------------------------------------------
price_cost_profit_raw <- read.xlsx(glue("{raw_root}/price_costs_profit_per_unit.xlsx"), sheet = "Table", rows = 8:16, colNames = FALSE) %>%
  select(-c(X1))

colnames(price_cost_profit_raw) <- c("var", as.character(seq(as.Date("2017-01-01"), as.Date("2024-01-01"), by = "quarter")))

profit_ind_raw <- read.xlsx(glue("{raw_root}/corporate_profits_by_industry.xlsx"), sheet = "Table", rows = 16:40, colNames = FALSE) %>%
  select(-c(X1))

colnames(profit_ind_raw) <- c("industry", as.character(seq(as.Date("2017-01-01"), as.Date("2024-01-01"), by = "quarter")))

go_ind_raw <- read.xlsx(glue("{raw_root}/gross_output_by_industry.xlsx"), sheet = "Table", rows = 8:108, colNames = FALSE) %>%
  select(-c(X1))

colnames(go_ind_raw) <- c("industry", as.character(seq(as.Date("2018-01-01"), as.Date("2024-01-01"), by = "quarter")))

price_cost_profit_1 <- price_cost_profit_raw %>%
  pivot_longer(cols = !var, names_to = "qtr_temp", values_to = "value") %>%
  mutate(qtr = as.yearqtr(as.Date(qtr_temp))) %>%
  select(-c(qtr_temp)) %>%
  mutate(var_clean = case_when(
    grepl("Price per unit of real gross value added of nonfinancial corporate business1", var) ~ "Price",
    grepl("Compensation of employees", var) ~ "Cost - labor",
    grepl("Consumption of fixed capital", var) ~ "Cost - capital",
    grepl("Taxes on production and imports less subsidies plus business current transfer payments", var) ~ "Cost - net taxes",
    grepl("Corporate profits with IVA and CCAdj", var) ~ "Profits with IVA and CCAdj (before tax)",
    grepl("Profits after tax with IVA and CCAdj", var) ~ "Profits with IVA and CCAdj (after tax)",
    T ~ ""
  )) %>%
  filter(var_clean != "")

process_ind_data <- function(data, var) {
  
  data %>%
    pivot_longer(cols = !industry, names_to = "qtr_temp", values_to = var) %>%
    mutate(qtr = as.yearqtr(as.Date(qtr_temp))) %>%
    select(-c(qtr_temp)) %>%
    mutate(industry_clean = gsub("[0-9]", "", gsub("^ +", "", industry)))
    
}

profit_ind_1 <- process_ind_data(data = profit_ind_raw, var = "profit") %>%
  filter(!(industry_clean %in% c("Federal Reserve banks", "Other financial", "Durable goods", "Other durable goods", "Nondurable goods", "Other nondurable goods", "Other nonfinancial", "Rest of the world")))

profit_industries <- profit_ind_1 %>%
  distinct(industry_clean) %>%
  mutate(flag = 1)

go_ind_1 <- process_ind_data(data = go_ind_raw, var = "go") %>%
  left_join(profit_industries, by = c("industry_clean")) %>%
  filter(grepl(glue("Private industries|Finance, insurance, real estate, rental, and leasing"), industry_clean) | flag == 1) %>%
  mutate_at(vars(go), as.numeric)

profit_manuf_subind <- profit_ind_1 %>%
  filter(grepl("                ", industry)) %>%
  distinct(industry_clean, qtr, profit)

go_manuf_subind <- go_ind_1 %>%
  inner_join(profit_manuf_subind, by = c("industry_clean", "qtr")) %>%
  select(industry_clean, qtr, go)

# Make plots -------------------------------------------------------------------
plots_unit <- list()

price_labor <- price_cost_profit_1 %>%
  filter(var_clean == "Price" | var_clean == "Cost - labor")

other_cost_profit <- price_cost_profit_1 %>%
  filter(var_clean != "Price" & var_clean != "Cost - labor")

make_plot_unit <- function(df, subtitlee) {
  
  ggplot(df, aes_string(x = "qtr", y = "value", color = "var_clean")) +
    geom_line() +
    scale_x_yearqtr(n = 29, expand = c(0, 0), format = "%YQ%q") +
    labs(title = "Price, costs, profits per unit real gross value added", subtitle = glue("Nonfinancial business\n{subtitlee}"), caption = "Source: BEA, calculations by cconlon@stern.nyu.edu") +
    xlab(label = "") +
    ylab(label = "") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom", legend.title = element_blank())
  
}

plots_unit[["price_labor"]] <- make_plot_unit(df = price_labor, subtitlee = "Price, labor costs")
plots_unit[["other_cost_profit"]] <- make_plot_unit(df = other_cost_profit, subtitlee = "Profits, other costs")

data <- list()

data[["profit"]][["tot"]] <- profit_ind_1 %>%
  filter(industry_clean == "Domestic industries" | industry_clean == "Nonfinancial")

data[["go"]][["tot"]] <- go_ind_1 %>%
  filter(industry_clean == "Private industries")

broad_string_search <- "Utilities|Manufacturing|Wholesale trade|Retail trade|Transportation and warehousing|Information"

data[["profit"]][["sector"]] <- profit_ind_1 %>%
  filter(grepl(glue("Financial|{broad_string_search}"), industry_clean))

data[["go"]][["sector"]] <- go_ind_1 %>%
  filter(grepl(glue("Finance, insurance, real estate, rental, and leasing|{broad_string_search}"), industry_clean))

data[["profit"]][["manuf_subind"]] <- profit_manuf_subind
data[["go"]][["manuf_subind"]] <- go_manuf_subind

make_plot_ind <- function(df, var, titlee, subtitlee) {
  
  ggplot(df, aes_string(x = "qtr", y = glue("{var}"), color = "industry_clean")) +
    geom_line() +
    scale_x_yearqtr(n = 29, expand = c(0, 0), format = "%YQ%q") +
    scale_y_continuous(label = scales::comma) +
    labs(title = glue("{titlee}"), subtitle = glue("{subtitlee}"), caption = "Source: BEA, calculations by cconlon@stern.nyu.edu") +
    xlab(label = "") +
    ylab(label = "Billion USD") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom", legend.title = element_blank())
  
}

make_plots_ind <- function(var, titlee) {
  
  plots_temp <- list()
  
  plots_temp[["tot"]] <- make_plot_ind(df = data[[glue("{var}")]][["tot"]], var = glue("{var}"), titlee = glue("{titlee}"), subtitlee = "Total")
  plots_temp[["sector"]] <- make_plot_ind(df = data[[glue("{var}")]][["sector"]], var = glue("{var}"), titlee = glue("{titlee}"), subtitlee = "Sector")
  plots_temp[["manuf_subind"]] <- make_plot_ind(df = data[[glue("{var}")]][["manuf_subind"]], var = glue("{var}"), titlee = glue("{titlee}"), subtitlee = "Manufacturing subindustries")
  
  plots_temp
  
}

plots_profit <- make_plots_ind(var = "profit", titlee = "Corporate profits with IVA (before tax)")
plots_go <- make_plots_ind(var = "go", titlee = "Gross output")
plots <- list(plots_unit, plots_profit, plots_go)

ggsave_c(glue("{output_root}/aggregates.pdf"), plots)
