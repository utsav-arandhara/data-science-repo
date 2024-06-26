#! /usr/bin/env R
#! TB_analysis.R

# loading libraries
library(ggplot2)
library(dplyr)
library(readr)
library(tidyverse)

# Loading in the datasets
tb_mortality <- read.csv("D:/Private/Dihang/Bioinformatics/data-science-repo/data-science-repo/Datasets/TB Mortality.csv") # nolint: line_length_linter.
tb_trt_cov <- read.csv("D:/Private/Dihang/Bioinformatics/data-science-repo/data-science-repo/Datasets/TB-Trt Coverage.csv") # nolint: line_length_linter.
tb_trt_success <- read.csv("D:/Private/Dihang/Bioinformatics/data-science-repo/data-science-repo/Datasets/Trt-Success TB.csv") # nolint: line_length_linter.

# Creating key to filter available data in each data set
tb_mortality <- tb_mortality %>%
  mutate(Key =  paste(Countries..territories.and.areas, Year, sep = "_"))
tb_trt_cov <- tb_trt_cov %>%
  mutate(Key =  paste(Countries..territories.and.areas, Year, sep = "_"))
tb_trt_success <- tb_trt_success %>%
  mutate(Key =  paste(Countries..territories.and.areas, Year, sep = "_"))

# Joining data sets
tb_mort_trt_df <- tb_mortality %>%
  inner_join(tb_trt_cov, by = "Key") %>%
  select(-c(Countries..territories.and.areas.y, Year.y))

tb_mort_trt_df <- tb_mort_trt_df %>%
  inner_join(tb_trt_success, by = "Key") %>%
  select(-c(Countries..territories.and.areas, Year))

# Removing datasets
remove(tb_mortality)
remove(tb_trt_cov)
remove(tb_trt_success)

# All countries
tb_mort_trt_df <- tb_mort_trt_df %>%
  separate(Number.of.deaths.due.to.tuberculosis..excluding.HIV, into = c("TB_D excl HIV", "Range of TB_D excl HIV"), sep = "\\s") %>% # nolint: line_length_linter.
  separate(Deaths.due.to.tuberculosis.among.HIV.negative.people..per.100.000.population., into = c("MR_TB_HIV_ng", "Range_MR_TB_HIV_ng"), sep = "\\s") %>% # nolint: line_length_linter.
  separate(Tuberculosis.treatment.coverage, into = c("TB_TRT_COV", "TB_TRT_COV_Range"), sep = "\\s") # nolint: line_length_linter.

tb_mort_trt_df %>%
  filter(Year.x == 2021) %>%
  ggplot(aes(x = as.integer(`TB_D excl HIV`), y = as.integer(MR_TB_HIV_ng), color = as.integer(TB_TRT_COV), label = Countries..territories.and.areas.x)) + # nolint: line_length_linter, line_length_linter.
  geom_point() +
  geom_text(vjust = 1, hjust = 1, size = 3) +
  scale_x_log10() +
  labs(
    title = "Mortality Rate vs Number of Deaths, accounting for Treatment Coverage in 2021", # nolint
    x = "Total number of deaths due to TB excl. HIV (Log 10 Scale)",
    y = "Mortality Rates for TB excl. HIV"
  ) +
  guides(
    color = guide_legend(title = "TB Treatment Coverage")
  ) +
  theme_bw()


# BRICS-specific analysis
tb_mort_trt_df_BRICS <- tb_mort_trt_df %>% # nolint: object_name_linter.
  filter(`Countries..territories.and.areas.x` %in% c("India", "China", "Brazil", "Russian Federation", "South Africa")) # nolint: line_length_linter. # nolint: line_length_linter.

# Simple plots
tb_mort_trt_df_BRICS %>%
  ggplot(aes(x = Year.x, y = as.integer(`TB_D excl HIV`), group = Countries..territories.and.areas.x, color = Countries..territories.and.areas.x)) + # nolint: line_length_linter.
  geom_line() +
  theme_bw() +
  labs(
    title = "Number of Deaths in BRICS countries due to TB excl. HIV",
    x = "Year",
    y = "Number of deaths"
  ) +
  guides(
    color = guide_legend(title = "Countries")
  )

tb_mort_trt_df_BRICS %>%
  ggplot(aes(x = Year.x, y = as.integer(TB_TRT_COV), group = Countries..territories.and.areas.x, color = Countries..territories.and.areas.x)) + # nolint: line_length_linter.
  geom_line() +
  theme_bw() +
  labs(
    title = "TB Treatment Coverage rates in BRICS countries",
    x = "Year",
    y = "Treatment Coverage rates"
  ) +
  scale_y_log10() +
  guides(
    color = guide_legend(title = "Countries")
  )

tb_mort_trt_df_BRICS %>%
  ggplot(aes(x = Year.x, y = as.integer(Treatment.success.rate..new.TB.cases), group = Countries..territories.and.areas.x, color = Countries..territories.and.areas.x)) + # nolint: line_length_linter.
  geom_line() +
  theme_bw() +
  labs(
    title = "Treatment Success rates in BRICS countries",
    x = "Year",
    y = "Treatment success rates"
  ) +
  scale_y_log10() +
  guides(
    color = guide_legend(title = "Countries")
  )

tb_mort_trt_df_BRICS %>%
  ggplot(aes(x = Year.x, y = as.integer(Treatment.success.rate.for.patients.treated.for.MDR.TB....), group = Countries..territories.and.areas.x, color = Countries..territories.and.areas.x)) +  # nolint: line_length_linter.
  geom_line() +
  theme_bw() +
  labs(
    title = "Treatment Success rates for patients with MDR-TB in BRICS countries", # nolint: line_length_linter.
    x = "Year",
    y = "Treatment success rates for MDR-TB"
  ) +
  scale_y_log10() +
  guides(
    color = guide_legend(title = "Countries")
  )
