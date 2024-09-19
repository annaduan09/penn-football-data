library(tidyverse)
library(ggplot2)
library(conflicted)
library(pander)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Load the data
max_24 <- read_csv("data/spring_2024_maxes_long.csv", col_select = c(Athlete, Position, Exercise, Max)) %>%
  mutate(Max = as.numeric(Max)) %>%
  pivot_wider(names_from = Exercise, values_from = Max) %>%
  # make column names safe
  rename(power_clean = `BB Power Clean`,
         back_squat = `BB Back Squat`,
         bench_press = `BB Bench Press`)


# Summary
max_24[3:5] %>%
  na.omit() %>%
  summary() %>%
  pander("Max Stats: Team")

max_24 %>%
  na.omit() %>%
  group_by(Position) %>%
  summarize(power_clean = mean(power_clean, na.rm = TRUE),
            back_squat = mean(back_squat, na.rm = TRUE),
            bench_press = mean(bench_press, na.rm = TRUE)) %>%
  arrange(desc(power_clean)) %>%
  pander("Max Stats: Position")

# calculate z-scores
max_24 <- max_24 %>%
  mutate(power_clean_z = (power_clean - mean(power_clean, na.rm = TRUE)) / sd(power_clean, na.rm = TRUE),
         back_squat_z = (back_squat - mean(back_squat, na.rm = TRUE)) / sd(back_squat, na.rm = TRUE),
         bench_press_z = (bench_press - mean(bench_press, na.rm = TRUE)) / sd(bench_press, na.rm = TRUE))

# plot avg maxes by position
max_24 %>%
  na.omit() %>%
  group_by(Position) %>%
  summarize(power_clean = mean(power_clean, na.rm = TRUE),
            back_squat = mean(back_squat, na.rm = TRUE),
            bench_press = mean(bench_press, na.rm = TRUE)) %>%
  arrange(desc(power_clean)) %>%
  pivot_longer(cols = -Position, names_to = "Exercise", values_to = "Max") %>%
  ggplot(aes(x = Position, y = Max, fill = Exercise)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Maxes by Position",
       x = "Position",
       y = "Max",
       fill = "Exercise") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
