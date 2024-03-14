#! /usr/bin/env R
#! gender_bias_ucb_admissions.R

# Load essential libraries
library(tidyverse)
library(broom)
library(dplyr)
library(scales)
library(forcats)

# Load UCBAdmissions dataset
data("UCBAdmissions")

# Print dataset to console
print(UCBAdmissions)

# Load broom package
library(broom)

# Convert UCBAdmissions to tidy format
ucb_tidy <- tidy(UCBAdmissions)

# Print tidy dataset to console
print(ucb_tidy)

# Aggregate over department
ucb_tidy_aggregated <- ucb_tidy %>%
  group_by(Admit, Gender) %>%
  summarize(n = sum(n)) %>%
  ungroup() %>%
  group_by(Gender) %>% 
  mutate(prop = n/sum(n)) %>%
  filter(Admit == "Admitted")

# Print aggregated dataset
print(ucb_tidy_aggregated)

# Prepare the bar plot
gg_bar <- ucb_tidy_aggregated %>%
    ggplot(aes(x = Gender, y = prop, fill = Gender)) +
    geom_col() +
    geom_text(aes(label = percent(prop)), vjust = -1) +
    labs(title = "Acceptance rate of male and female applicants",
         subtitle = "University of California, Berkeley (1973)",
         y = "Acceptance rate") +
    scale_y_continuous(labels = percent, limits = c(0, 0.5)) +
    guides(fill = FALSE)

# Print the bar plot
print(gg_bar)

# Analysis by department

# Calculate acceptance/rejection rate
ucb_by_dept <- ucb_tidy %>% 
    group_by(Gender, Dept) %>% 
    mutate(prop = n/sum(n)) %>% 
    filter(Admit == "Admitted")

# Print the dataset
print(ucb_by_dept)

# Prepare the bar plot for each department
gg_bar_faceted <- ucb_by_dept %>% 
  ggplot(aes(Gender, prop, fill = Gender)) +
  geom_col() +
  geom_text(aes(label = percent(prop)), vjust = -1) +
  labs(title = "Acceptance rate of male and female applicants",
       subtitle = "University of California, Berkeley (1973)",
       y = "Acceptance rate") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  facet_wrap(~Dept) +
  guides(fill = FALSE)

# Print the bar plot for each department
print(gg_bar_faceted)

# Define function that repeats each row in each column n times
multiply_rows <- function(column, n) {
  rep(column, n)
}

# Create new de-aggregated data frame using the multiply_rows function
ucb_full <- data.frame(Admit = multiply_rows(ucb_tidy$Admit, ucb_tidy$n),
                      Gender = multiply_rows(ucb_tidy$Gender, ucb_tidy$n),
                      Dept = multiply_rows(ucb_tidy$Dept, ucb_tidy$n))

# Check the number of rows equals the number of students
print(nrow(ucb_full))

# Reverse the coding of the Admit variable
ucb_full$Admit <- fct_relevel(ucb_full$Admit, "Rejected", "Admitted")

# Run the regression
glm_gender <- glm(Admit ~ Gender, data = ucb_full, family = "binomial")

# Summarize the results
print(summary(glm_gender))

# Run the regression, including Dept as an explanatory variable
glm_genderdept <- glm(Admit ~ Gender + Dept, data = ucb_full, family = "binomial")

# Summarize the results
print(summary(glm_genderdept))

# Filter for Department A
dept_a <- ucb_full %>%
filter(Dept == "A")

# Run the regression
glm_gender_depta <- glm(Admit ~ Gender, data = dept_a, family = "binomial")

# Summarize result
print(summary(glm_gender_depta))
