setwd('/Users/deeksha.m/Library/CloudStorage/OneDrive-Personal/college/3rd SEMESTER/BOOT CAMP_2/R')
getwd()
install.packages("lubridate")
install.packages("fitdistrplus")
#### 1. Load Required Libraries ####
library(readr)
library(readxl)
library(dplyr)
library(lubridate)
library(fitdistrplus)
library(ggplot2)
library(stringr)
#### 2. Import the Datasets ####
ipl_data <- read_csv("~/Desktop/IPL_ball_by_ball_updated till 2024.csv")
salary_data <- read_excel("~/Desktop/IPL SALARIES 2024.xlsx")
#### 3. Data Preparation ####
ipl_data <- ipl_data %>%
  mutate(Date = dmy(Date),
         year = year(Date)) %>%
  rename(
    runs_scored = runs_scored,
    wicket = wicket_confirmation,
    Striker = Striker,
    Bowler = Bowler,
    Match_id = ⁠ Match id ⁠
  )
#### 4. Grouped Data per Player ####
grouped_data <- ipl_data %>%
  group_by(Season = year, ⁠ Innings No ⁠, Striker, Bowler) %>%
  summarise(
    runs_scored = sum(runs_scored, na.rm = TRUE),
    wicket_confirmation = sum(wicket, na.rm = TRUE),
    .groups = "drop"
  )
print(head(grouped_data))
#### 5. Top 3 Run-Getters per Season ####
player_runs <- ipl_data %>%
  group_by(Season = year, Striker) %>%
  summarise(runs_scored = sum(runs_scored, na.rm = TRUE), .groups = "drop")
top_run_getters <- player_runs %>%
  group_by(Season) %>%
  arrange(desc(runs_scored)) %>%
  slice_head(n = 3)
cat("Top Three Run Getters:\n")
print(top_run_getters)
#### 6. Top 3 Wicket-Takers per Season ####
player_wickets <- ipl_data %>%
  group_by(Season = year, Bowler) %>%
  summarise(wicket_confirmation = sum(wicket, na.rm = TRUE), .groups = "drop")
top_wicket_takers <- player_wickets %>%
  group_by(Season) %>%
  arrange(desc(wicket_confirmation)) %>%
  slice_head(n = 3)
cat("Top Three Wicket Takers:\n")
print(top_wicket_takers)
#### 7. Top Run-Getters from 2023 Sorted ####
player_runs_2023 <- player_runs %>%
  filter(Season == 2023) %>%
  arrange(desc(runs_scored))
print(player_runs_2023)
#### 8. Fit Distribution Function ####
fit_best_distribution <- function(data_vector, player_name) {
  cat("\nPlayer:", player_name, "\n")
  fit <- fitdist(data_vector, "norm")  # You can change to "gamma", "weibull", etc.
  print(summary(fit))
  plot(fit)
}
#### 9. Fit Distribution for Top Batsmen (2022–2024) ####
last_3_years <- c(2022, 2023, 2024)
for (yr in last_3_years) {
  top3 <- top_run_getters %>% filter(Season == yr)
  for (player in top3$Striker) {
    scores <- ipl_data %>%
      filter(year == yr, Striker == player) %>%
      group_by(Match_id) %>%
      summarise(runs = sum(runs_scored), .groups = "drop") %>%
      pull(runs)
    if (length(scores) >= 5) {
      fit_best_distribution(scores, player)
    }
  }
}
#### 10. Fit Distribution for Top Bowlers (2022–2024) ####
for (yr in last_3_years) {
  top3 <- top_wicket_takers %>% filter(Season == yr)
  for (player in top3$Bowler) {
    wickets <- ipl_data %>%
      filter(year == yr, Bowler == player) %>%
      group_by(Match_id) %>%
      summarise(wkts = sum(wicket), .groups = "drop") %>%
      pull(wkts)
    
    if (length(wickets) >= 5) {
      fit_best_distribution(wickets, player)
    }
  }
}
#### 11. Fit Distribution for JJ Bumrah (2024) ####
sai_data <- ipl_data %>%
  filter(Striker == "JJ Bumrah", year == 2024) %>%
  group_by(Match_id) %>%
  summarise(runs = sum(runs_scored), .groups = "drop") %>%
  pull(runs)
if (length(sai_data) >= 5) {
  fit_best_distribution(sai_data, "JJ Bumrah")
}
#### 12 Salary Matching & Correlation ####
# Step 1: Filter performance for 2024
R2024 <- total_run_each_year %>%
  filter(year == 2024)
# Step 2: Ensure salary column is clean
salary_data$Rs <- as.numeric(gsub(",", "", salary_data$Rs))
# Step 3: Fuzzy name match function
match_names <- function(name, names_list) {
  match <- amatch(name, names_list, method = "jw", maxDist = 0.2)
  if (is.na(match)) return(NA) else return(names_list[match])
}
# Step 4: Apply fuzzy matching
salary_data$Matched_Player <- sapply(salary_data$Player, match_names, R2024$Striker)
# Step 5: Merge salary and run data
df_merged <- merge(salary_data, R2024, by.x = "Matched_Player", by.y = "Striker")

# Step 6: Remove incomplete pairs (NA salary or runs)
df_merged <- df_merged[!is.na(df_merged$Rs) & !is.na(df_merged$runs_scored), ]
# Step 7: Calculate and print correlation
correlation <- cor(df_merged$Rs, df_merged$runs_scored, use = "complete.obs")
cat(" Correlation between Salary and Runs in IPL 2024:", round (correlation, 4), "\n")