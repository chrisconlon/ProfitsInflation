library(tidyverse)
library(glue)
library(zoo)
library(ggplot2)
library(fredr)
library(here)
source(here::here("code", "config_utils.R"))

fredr_set_key(fred_api_key)
output_root <- output_dir

data_list <- list()
num <- 0

for (s in c("PCEC96", "DGDSRX1", "PCESC96", "PCEDGC96", "PCNDGC96", "DMOTRX1Q020SBEA", "DFDHRX1Q020SBEA")) {
  
  num <- num + 1
  
  raw <- fredr(
    series_id = glue("{s}"),
    observation_start = as.Date("2017-01-01"),
    frequency = "q"
  )
  
  title <- fredr_series(series_id = glue("{s}"))$title
  
  df <- raw %>%
    mutate(cat_c = glue("{title}"))
  
  data_list[[num]] <- df
  
}

data_1 <- bind_rows(data_list)

data_2 <- data_1 %>%
  mutate(qtr_c = as.yearqtr(date))

nrow(data_2) == nrow(data_2 %>%
                       distinct(cat_c, qtr_c))

data_3 <- data_2 %>%
  group_by(cat_c) %>%
  mutate(base_c = first(value, order_by = qtr_c)) %>%
  ungroup() %>%
  mutate(index_c = (value/base_c)*100) %>%
  filter(qtr_c < "2025 Q4") %>%
  mutate(cat_clean_c = gsub("^Real Personal Consumption Expenditures$", "Total", gsub("Real Personal Consumption Expenditures: |Real personal consumption expenditures: ", "", cat_c)))

fn <- "Source: FRED. Seasonally adjusted."

ggplot(data_3, aes(x = qtr_c, y = index_c, color = cat_clean_c)) +
  geom_line(linewidth = 1) +
  scale_x_yearqtr(n = 52, expand = c(0, 0), format = "%YQ%q") +
  labs(title = "Real personal consumption expenditures", caption = glue("{fn}")) +
  xlab(label = "") +
  ylab(label = "Index") +
  scale_color_discrete(breaks = c("Total", "Goods", "Services", "Durable Goods", "Nondurable Goods", "Durable goods: Motor vehicles and parts", "Durable goods: Furnishings and durable household equipment")) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5), legend.position = "bottom", legend.title = element_blank())

pdf(glue("{output_root}/pce.pdf"), width = 14, height = 8.5)
print(last_plot())
dev.off()

fredr_series(series_id = "PSAVERT")

savings_raw <- fredr(
  series_id = "PSAVERT",
  observation_start = as.Date("2017-01-01"),
  frequency = "q"
)

savings <- savings_raw %>%
  mutate(qtr_c = as.yearqtr(date),
         saving_rate_c = value/100) %>%
  filter(qtr_c < "2025 Q4")

ggplot(savings, aes(x = qtr_c, y = saving_rate_c)) +
  geom_line(linewidth = 1) +
  scale_x_yearqtr(n = 52, expand = c(0, 0), format = "%YQ%q") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Personal saving rate", subtitle = "Percentage of disposable personal income", caption = glue("{fn}")) +
  xlab(label = "") +
  ylab(label = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

pdf(glue("{output_root}/personal_savings.pdf"), width = 14, height = 8.5)
print(last_plot())
dev.off()