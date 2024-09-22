library(tidyverse)
library(ggplot2)
library(conflicted)
library(pander)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# Load the data
max_22 <- read_csv("data/spring_2022_maxes.csv") %>%
  mutate(Athlete = tolower(Athlete),
         across(c(Height, Wing, Weight, Bodyfat, Pre_225, Bench, Squat, Deadlift, Total, `Combine_%_of_BW`,
                         `10yd_Official`, Vertical_Jump, Broad_Jump), as.numeric)) 
  

max_24_offseason <- read_csv("data/spring_2024_offseason.csv")


max_24 <- read_csv("data/spring_2024_maxes_long.csv", col_select = c(Athlete, Position, Exercise, Max)) %>%
  mutate(Max = as.numeric(Max),
         Athlete = tolower(Athlete),
         Athlete = str_remove_all(Athlete, " jr"),
         Athlete = str_remove_all(Athlete, " iv"),
         last_name = str_extract(Athlete, "[a-z]+$"),
         first_initial = substr(str_extract(Athlete, "^[a-z]+"), 1, 1),
         name_match = paste0(last_name, ", ", first_initial, "."),
         name_match = ifelse(name_match == "villiers, c.", "de villiers, c.", name_match)) %>%
  pivot_wider(names_from = Exercise, values_from = Max) %>%
  # make column names safe
  rename(power_clean = `BB Power Clean`,
         squat = `BB Back Squat`,
         bench = `BB Bench Press`) %>%
  mutate(power_clean_z = (power_clean - mean(power_clean, na.rm = TRUE)) / sd(power_clean, na.rm = TRUE),
         squat_z = (squat - mean(squat, na.rm = TRUE)) / sd(squat, na.rm = TRUE),
         bench_z = (bench - mean(bench, na.rm = TRUE)) / sd(bench, na.rm = TRUE))




# Summary
max_24[3:5] %>%
  na.omit() %>%
  summary() %>%
  pander("Team Stats, 2024")

max_24 %>%
  na.omit() %>%
  group_by(Position) %>%
  summarize(power_clean = mean(power_clean, na.rm = TRUE),
            squat = mean(squat, na.rm = TRUE),
            bench = mean(bench, na.rm = TRUE)) %>%
  arrange(desc(power_clean)) %>%
  pander("Position Stats, 2024")



# plot avg maxes by position
max_24 %>%
  na.omit() %>%
  group_by(Position) %>%
  summarize(`Power Clean` = mean(power_clean, na.rm = TRUE),
            `Squat` = mean(squat, na.rm = TRUE),
            `Bench` = mean(bench, na.rm = TRUE)) %>%
  pivot_longer(cols = -Position, names_to = "Exercise", values_to = "Max") %>%
  ggplot(aes(x = reorder(Position, Max), y = Max, fill = Exercise)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Position Stats, 2024",
       x = "Position",
       y = "Max",
       fill = "Exercise") +
  facet_wrap(~Exercise, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "None")
