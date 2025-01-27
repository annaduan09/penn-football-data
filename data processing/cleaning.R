library(tidyverse)
library(ggplot2)
library(conflicted)
library(pander)
library(purrr)
library(jsonlite)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

#### Functions ####

# 6'1.5" height notation
parse_ft_in <- function(ft_in_vec) {
  ft_in_vec <- gsub("''", "", ft_in_vec) 
  ft_in_vec <- gsub("\"", "", ft_in_vec) 
  is_ft_in <- grepl("'", ft_in_vec) 
  
  result <- ifelse(
    is_ft_in,
    {
      parts <- strsplit(ft_in_vec[is_ft_in], "'")
      feet <- as.numeric(sapply(parts, `[`, 1))
      inches <- as.numeric(sapply(parts, `[`, 2))
      feet * 12 + inches
    },
    as.numeric(ft_in_vec) * 12
  )
  
  return(result)
}

# 6015 height notation
parse_height_notation <- function(height_str) {
  feet <- as.numeric(substr(height_str, 1, 1))  
  inches_full <- as.numeric(substr(height_str, 2, 3))
  inches_frac <- as.numeric(substr(height_str, 4, 4))/10
  height_inches <- (feet * 12) + inches_full + inches_frac
  return(height_inches)
}

# 6.1 height notation (accidental)
parse_feet_decimal_notation <- function(length_vec) {
  length_vec <- as.character(length_vec)
  
  is_decimal <- grepl("\\.", length_vec)
  
  result <- ifelse(
    is_decimal,
    {
      parts <- strsplit(length_vec[is_decimal], "\\.")
      feet <- as.numeric(sapply(parts, `[`, 1))
      inches <- as.numeric(sapply(parts, `[`, 2))
      feet * 12 + inches
    },
    as.numeric(length_vec) * 12
  )
  
  return(result)
}


#### Raw data ####
max_20 <- read_csv("data/2020_maxes_wide.csv") 
max_22 <- read_csv("data/spring_2022_maxes_long.csv") 
max_23 <- read_csv("data/2023_maxes_long.csv")
max_24 <- read_csv("data/spring_2024_maxes_long.csv", col_select = c(Athlete, Position, Exercise, Max)) 
max_24_offseason <- read_csv("data/spring_2024_offseason_bests.csv") 

#### Clean data #### (No 2021 data because Covid)
max_20_clean <- max_20 %>%
  mutate(across(where(is.character), ~ gsub("[‘’]", "'", .)),
         across(
           -c(name, position),
           ~ ifelse(grepl("[A-Za-z]", .), NA, .)
         ),
         across(c(wing, broad_jump), parse_ft_in),
         year = 2020) %>%
  select(-`...11`, -`...12`, -`...13`, -inches, -`5-10-5`, -`3 Cone`) 

max_22_clean <- max_22 %>%
  mutate(across(where(is.character), ~ gsub("[‘’]", "'", .)),
         across(
           -c(Athlete, Position),
           ~ ifelse(grepl("[A-Za-z]", .), NA, .)
         )) %>% 
  mutate(name = tolower(Athlete),
         across(c(Height, Wing, Weight, Pre_225, Bench, Squat, Deadlift, Total,
                  `10yd_Official`, Vertical_Jump), as.numeric),
         broad_jump = case_when(
           grepl("\\.", Broad_Jump) | as.numeric(Broad_Jump) < 40 ~ parse_feet_decimal_notation(Broad_Jump), # Two different notations present: 6'1 and 6.1. These mean the same thing.
           grepl("'", Broad_Jump) ~ parse_ft_in(Broad_Jump),
           TRUE ~ as.numeric(Broad_Jump)
         ),
         height = parse_height_notation(Height),
         height = ifelse(name == "maurcus mcdaniel", 72, height),
         bench_225 = case_when(
           Pre_225 > 0 ~ Pre_225,
           Pre_225 == 0 & (Bench >= 225 | is.na(Bench)) ~ NA_real_,
           Pre_225 == 0 & Bench < 225 ~ 0,
           TRUE ~ Pre_225
         )) %>%
  select(name, position = Position, height, wing = Wing, weight = Weight, bench_225, bench = Bench, squat = Squat,
         sprint_10y = `10yd_Official`, vertical_jump = Vertical_Jump, broad_jump) %>%
  mutate(year = 2022)

max_23_clean <- max_23 %>%
  mutate(Name = tolower(Name),
         Name = str_remove_all(Name, "'"),
         Name = str_remove_all(Name, " jr"),
         Name = str_remove_all(Name, " iv"),
         Name = str_replace(Name, "soefker, l.", "soefker, w."),
         Position = ifelse(Name == "barone, d.", "TE", Position),
         Position = ifelse(Name == "mulatu, j.", "RB", Position),
         Position = ifelse(Name == "soefker, w.", "DB", Position),
         Position = ifelse(Position == "P", "SP", Position),
         Position = ifelse(Position == "CB", "DB", Position),
         Test = str_replace_all(`Test Name`, "Wing Span", "Wingspan")) %>%
  select(Name, Position, Test, Best) %>%
  na.omit(Best) %>%
  group_by(Name, Position, Test) %>%
  reframe(
    Best = ifelse(
      Test == "Vertical Jump", max(Best, na.rm = TRUE),
      if_else(Test == "10 yard sprint", min(Best, na.rm = TRUE), Best))) %>%
  unique() %>%
  pivot_wider(names_from = Test, values_from = Best) %>%
  as.data.frame() %>%
  mutate(across(
    -c(Name, Position),
    ~ ifelse(grepl("[A-Za-z]", .), NA, .)),
    Height = ifelse(Height == 0, NA, Height),
    Wingspan = ifelse(Wingspan == 0, NA, Wingspan),
    `225 Bench` = ifelse(`225 Bench` == 0, NA, `225 Bench`)) %>%
  rename(name = Name, position = Position, sprint_10y = `10 yard sprint`, height = Height, vertical_jump = `Vertical Jump`, 
         weight = Weight, bench_225 = `225 Bench`, bench = `BB Bench`, squat = `Back Squat`, broad_jump = `Broad Jump`, 
         L_drill = `L-Drill`, pro_agility = `Pro Agility`, hang_clean = `Hang Clean`, wing = Wingspan) %>%
  mutate(year = 2023)

  
max_24_offseason_clean <- max_24_offseason %>%
  mutate(across(
    -c(athlete, position),
    ~ ifelse(grepl("[A-Za-z]", .), NA, .)),
    athlete = tolower(athlete),
         athlete = str_remove_all(athlete, "'"),
         athlete = str_replace_all(athlete, "ttt", "tt"),
         athlete = str_replace_all(athlete, "narisse", "narcisse"),
         athlete = str_replace_all(athlete, "dochatt", "dochat"),
         athlete = str_replace_all(athlete, "christian, c.", "carter, c."),
         athlete = str_replace_all(athlete, "brown, dj.", "brown, d."),
         athlete = str_replace_all(athlete, "casilli, c.", "casilli, j."),
         athlete = str_replace_all(athlete, "drayton, j", "drayton, j."),
         athlete = str_replace_all(athlete, "holiday", "holliday"),
         athlete = str_replace_all(athlete, "obrien, l", "obrien, l."),
         athlete = str_replace_all(athlete, "ostland", "ostlund"),
         athlete = str_replace_all(athlete, "zach, m", "zack, m."),
         position = ifelse(position %in% c("IWR", "OWR", "WR"), "WR", position),
         position = ifelse(position == "CB", "DB", position),
         position = ifelse(position == "S", "DB", position),
         broad_jump = parse_ft_in(broad_jump),
         height = parse_ft_in(height),
         wing = str_remove_all(wing, '"'),
         # replace all 0 with NA
         across(c(wing, weight, height, flying_10, `10y_sprint`, vert_vertec, broad_jump,
                  vert_mat, bench_225, shuttle_60y, L_drill, pro_agil), ~ifelse(. == 0, NA, .)),
         across(c(wing, weight, height, flying_10, `10y_sprint`, vert_vertec, broad_jump,
                  vert_mat, bench_225, shuttle_60y, L_drill, pro_agil), as.numeric)) %>%
  select(name = athlete, position, wing, weight, height, flying_10, `10y_sprint`, broad_jump, vert_mat, 
         bench_225, shuttle_60y, L_drill, pro_agil) %>%
  filter(!is.na(athlete)) %>%
  rename(sprint_10y = `10y_sprint`, pro_agility = pro_agil, vertical_jump = vert_mat)


max_24_season_clean <- max_24 %>% 
  mutate(Max = ifelse(grepl("[A-Za-z]", Max), NA, Max),
         Max = ifelse(Max == "?", NA, Max),
         Max = as.numeric(Max),
         Athlete = tolower(Athlete),
         Athlete = str_remove_all(Athlete, "'"),
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
         bench = `BB Bench Press`,
         position = Position) %>%
  mutate(position = ifelse(position %in% c("IWR", "OWR", "WR"), "WR", position),
         position = ifelse(position == "CB", "DB", position),
         position = ifelse(position == "S", "DB", position),
         across(c(power_clean, squat, bench), as.numeric)) %>%
  select(-last_name, -first_initial, -Athlete)

max_24_clean <- max_24_season_clean %>%
  full_join(max_24_offseason_clean, by = c("name_match" = "athlete", "position")) %>%
  rename(name = name_match) %>%
  mutate(year = 2024)

#### Merge years #### 
# Merge each year's cleaned max data
max_all_clean = bind_rows(max_20_clean, max_22_clean, max_23_clean, max_24_clean) %>%
  select(-name)


names(max_all_clean) <- c("Height", "Position", "Wingspan", "Weight", "225lb Bench", "Bench", 
                          "Squat", "Vertical Jump", "Broad Jump", "10-Yard Sprint", "Year", 
                          "Hang Clean", "L Drill", "Pro Agility", "Power Clean", "Flying 10", 
                          "60-Yard Shuttle")

#### Write to json ####
max_all_json <- max_all_clean %>%
  group_by(Position) %>%
  nest() %>%
  mutate(data = map(data, ~ summarise_all(.x, ~ list(na.omit(.))))) %>%
  mutate(data = map(data, ~ set_names(.x, names(max_all_clean)[-2]))) %>%
  deframe()


write_json(max_all_json, path = "stats_2020_2024.json", pretty = TRUE)


#### Group means ####
# max_22_group <- max_22_clean %>%
# group_by(Position) %>%
#   summarize(height = mean(Height, na.rm = TRUE),
#             wing = mean(Wing, na.rm = TRUE),
#             weight = mean(Weight, na.rm = TRUE),
#             bodyfat = mean(Bodyfat, na.rm = TRUE),
#             bench_225 = mean(Pre_225, na.rm = TRUE),
#             bench = mean(Bench, na.rm = TRUE),
#             squat = mean(Squat, na.rm = TRUE),
#             deadlift = mean(Deadlift, na.rm = TRUE),
#             dash_10y = mean(`10yd_Official`, na.rm = TRUE),
#             vertical_jump = mean(Vertical_Jump, na.rm = TRUE),
#             broad_jump = mean(Broad_Jump, na.rm = TRUE))
# 
# max_24_group <- max_24_clean %>%
#   group_by(position) %>%
#   summarize(power_clean = mean(power_clean, na.rm = TRUE),
#             squat = mean(squat, na.rm = TRUE),
#             bench = mean(bench, na.rm = TRUE))
# 
# max_24_off_group <- max_24_offseason_clean %>%
#   group_by(position) %>%
#   summarize(wing = mean(wing, na.rm = TRUE),
#             weight = mean(weight, na.rm = TRUE),
#             height = mean(height, na.rm = TRUE),
#             flying_10 = mean(flying_10, na.rm = TRUE),
#             dash_10y = mean(dash_10y, na.rm = TRUE),
#             vert_vertec = mean(vert_vertec, na.rm = TRUE),
#             vert_mat = mean(vert_mat, na.rm = TRUE),
#             broad_jump = mean(broad_jump, na.rm = TRUE),
#             bench_225 = mean(bench_225, na.rm = TRUE),
#             shuttle_60y = mean(shuttle_60y, na.rm = TRUE),
#             l_drill = mean(l_drill, na.rm = TRUE),
#             pro_agil = mean(pro_agil, na.rm = TRUE))
# 
# dat_24 <- max_24_group %>%
#   left_join(max_24_off_group, by = "position")
#   
# dat_24_json <- toJSON(dat_24)
# write_json(dat_24, "data/max_24_group.json")
#   
# # Summary
# max_24[3:5] %>%
#   na.omit() %>%
#   summary() %>%
#   pander("Team Stats, 2024")
# 
# max_24 %>%
#   na.omit() %>%
#   group_by(Position) %>%
#   summarize(power_clean = mean(power_clean, na.rm = TRUE),
#             squat = mean(squat, na.rm = TRUE),
#             bench = mean(bench, na.rm = TRUE)) %>%
#   arrange(desc(power_clean)) %>%
#   pander("Position Stats, 2024")


# 
# # plot avg maxes by position
# max_24 %>%
#   na.omit() %>%
#   group_by(Position) %>%
#   summarize(`Power Clean` = mean(power_clean, na.rm = TRUE),
#             `Squat` = mean(squat, na.rm = TRUE),
#             `Bench` = mean(bench, na.rm = TRUE)) %>%
#   pivot_longer(cols = -Position, names_to = "Exercise", values_to = "Max") %>%
#   ggplot(aes(x = reorder(Position, Max), y = Max, fill = Exercise)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Position Stats, 2024",
#        x = "Position",
#        y = "Max",
#        fill = "Exercise") +
#   facet_wrap(~Exercise, scales = "free_y") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1),
#         legend.position = "None")
