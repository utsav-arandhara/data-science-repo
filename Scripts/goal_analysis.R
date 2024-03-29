# Loading libraries
library(tidyverse)
library(ggplot2)

# Importing data sets
mens_results <- read_csv("D:/Private/Dihang/Bioinformatics/data-science-repo/data-science-repo/Datasets/men_results.csv")

womens_results <- read_csv("D:/Private/Dihang/Bioinformatics/data-science-repo/data-science-repo/Datasets/women_results.csv")

# Checking format data set
head(mens_results)

# Calculating total scores for performing the hypothesis test
mens_results <- mens_results %>%
mutate(total_score = home_score + away_score) %>%
mutate(status_tag = "Men")

womens_results <- womens_results %>%
mutate(total_score = home_score + away_score) %>%
mutate(status_tag = 'Women')

# Combining the data sets
final_data_set <- rbind(womens_results, mens_results)

# Visualizing scoring trends in both data sets
final_data_set %>% 
filter(tournament != "Friendly") %>%
ggplot(aes(x = date, y = total_score, color = status_tag)) + 
geom_point() +
labs(
    title = "Goal distribution ",
    x = "Year",
    y = "Total goals scored"
)

# Changing the time-frame to post Jan 2002 since there is no data available for women's football pre-1969
womens_results %>%
arrange(date)

final_data_set %>%
filter(date >= "2002-01-01") %>%
ggplot(aes(x = date, y = total_score, color = status_tag)) +
geom_point() +
labs(
    title = "Goal Distribution post January 2002",
    x = "Date/Year",
    y = "Total Goal Scored"
)

# Performing the hypothesis test and storing the results in a variable
result_df <- t.test(date = final_data_set, final_data_set$total_score ~ as.factor(final_data_set$status_tag), alternate = "greater", conf.level = 0.1)
result_df
