library(tidyverse)
library(ggplot2)
library(conflicted)
library(pander)
library(purrr)
library(jsonlite)
library(readxl)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

#### Helper functions ####

# 6'1.5" height notation
parse_ft_in <- function(ft_in_vec) {
  ft_in_vec <- gsub("''", "", ft_in_vec)
  ft_in_vec <- gsub("\"", "", ft_in_vec)
  is_ft_in <- grepl("'", ft_in_vec)
  
  result <- ifelse(is_ft_in, {
    parts <- strsplit(ft_in_vec[is_ft_in], "'")
    feet <- as.numeric(sapply(parts, `[`, 1))
    inches <- as.numeric(sapply(parts, `[`, 2))
    feet * 12 + inches
  }, as.numeric(ft_in_vec) * 12)
  
  return(result)
}

# 6015 height notation
parse_height_notation <- function(height_str) {
  feet <- as.numeric(substr(height_str, 1, 1))
  inches_full <- as.numeric(substr(height_str, 2, 3))
  inches_frac <- as.numeric(substr(height_str, 4, 4)) / 10
  height_inches <- (feet * 12) + inches_full + inches_frac
  return(height_inches)
}

# 6.1 height notation (accidental)
parse_feet_decimal_notation <- function(length_vec) {
  length_vec <- as.character(length_vec)
  
  is_decimal <- grepl("\\.", length_vec)
  
  result <- ifelse(is_decimal, {
    parts <- strsplit(length_vec[is_decimal], "\\.")
    feet <- as.numeric(sapply(parts, `[`, 1))
    inches <- as.numeric(sapply(parts, `[`, 2))
    feet * 12 + inches
  }, as.numeric(length_vec) * 12)
  
  return(result)
}



#### Roster data ####
roster <- read_csv("data/roster_19_25.csv") %>%
  mutate(position = str_replace(position, "OLB", "LB"),
         position = str_replace(position, "K", "SP"),
         position = str_replace(position, "LB/LS", "LB"),
         position = str_replace(position, "DL/LS", "DL"),
         position = str_replace(position, "P", "SP"),
         position = str_replace(position, "LS", "SP"),
         position = str_replace(position, "P/K", "SP"),
         position = str_replace(position, "K/P", "SP"),
         position = str_replace(position, "OL/DL", "OL"),
         name_last_first_initial = ifelse(name == "logan nash", "nash, lo", name_last_first_initial),
         name_last_first_initial = ifelse(name == "lawson nash", "nash, la", name_last_first_initial))
         

#### Testing data ####
max_20 <- read_csv("data/raw_testing/2020_maxes_wide.csv")
max_22 <- read_csv("data/raw_testing/spring_2022_maxes_long.csv")
max_23 <- read_csv("data/raw_testing/2023_maxes_long.csv")
max_24 <- read_csv("data/raw_testing/spring_2024_maxes_long.csv",
                   col_select = c(Athlete, Position, Exercise, Max))
max_24_offseason <- read_csv("data/raw_testing/spring_2024_offseason_bests.csv")
max_25 <- read_csv("data/raw_testing/spring_2025_maxes_long.csv")

##### 2020 #####
max_20_clean <- max_20 %>%
  mutate(name = tolower(name),
         name = ifelse(name == "thomspson, miles", "thompson, miles", name),
         name = ifelse(name == "joesph, mitzseen", "joseph, mitzseen", name),
         name = ifelse(name == "mottillo, christopher", "mottillo, chris", name),
         name = ifelse(name == "zelma, ethan", "zemla, ethan", name),
         name = ifelse(name == "starkey, rory", "starkey, jr., rory", name),
         name = ifelse(name == "mccleod, jason", "mccleod, jr., jason", name),
         across(where(is.character), ~ gsub("[‘’]", "'", .)),
         across(-c(name, position), ~ ifelse(grepl("[A-Za-z]", .), NA, .)),
         across(c(wing, broad_jump), parse_ft_in),
         test_year = 2020) %>%
  rename(name_last_first = name) %>%
  select(-`...11`, -`...12`, -`...13`, -inches, -`5-10-5`, -`3 Cone`) %>%
  left_join(roster[,c("name", "name_last_first", "grad_year", "number", "status")], by = "name_last_first")

##### 2022 #####
max_22_clean <- max_22 %>%
  mutate(across(where(is.character), ~ gsub("[‘’]", "'", .)), across(-c(Athlete, Position), ~ ifelse(grepl("[A-Za-z]", .), NA, .))) %>%
  mutate(
    name = tolower(Athlete),
    name = ifelse(name == "will bergin", "william bergin", name),
    name = ifelse(name == "nicholas fryhoff", "nick fryhoff", name),
    name = ifelse(name == "nick ostlund", "nicholas ostlund", name),
    name = ifelse(name == "cam hegarty", "cameron polemeni-hegarty", name),
    name = ifelse(name == "comizio kobe", "kobe comizio", name),
    name = ifelse(name == "josh casilli", "joshua casilli", name),
    name = ifelse(name == "rory starkey", "rory starkey, jr.", name),
    across(
      c(
        Height,
        Wing,
        Weight,
        Pre_225,
        Bench,
        Squat,
        Deadlift,
        Total,
        `10yd_Official`,
        Vertical_Jump
      ),
      as.numeric
    ),
    broad_jump = case_when(
      grepl("\\.", Broad_Jump) |
        as.numeric(Broad_Jump) < 40 ~ parse_feet_decimal_notation(Broad_Jump),
      # Two different notations present: 6'1 and 6.1. These mean the same thing.
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
    )
  ) %>%
  select(
    name,
    position = Position,
    height,
    wing = Wing,
    weight = Weight,
    bench_225,
    bench = Bench,
    squat = Squat,
    sprint_10y = `10yd_Official`,
    vertical_jump = Vertical_Jump,
    broad_jump
  ) %>%
  mutate(test_year = 2022)  %>%
  left_join(roster[,c("name", "grad_year", "number", "status")], by = "name")

##### 2023 #####
max_23_clean <- max_23 %>%
  mutate(
    Name = tolower(Name),
    Name = str_remove_all(Name, "\\."),
    Name = str_remove_all(Name, "'"),
    # manual corrections for roster join
    Name = ifelse(Name == "bing, j", "bing, jr., j", Name),
    Name = ifelse(Name == "williams, s", "williams, jr., s", Name),
    Name = ifelse(Name == "tremble, cj", "tremble, c", Name),
    Name = ifelse(Name == "ruvo, j", "ruvo iv, j", Name),
    Name = ifelse(Name == "oconnell, r", "o'connell, r", Name),
    Name = ifelse(Name == "obrien, l", "o'brien, l", Name),
    Name = ifelse(Name == "brown, dj", "brown, d", Name),
    Name = ifelse(Name == "christian, c", "carter, c", Name),
    Name = ifelse(Name == "narisse, j", "narcisse, j", Name),
    Name = str_replace(Name, "soefker, l", "soefker, w"),
    Position = ifelse(Name == "barone, d", "TE", Position), # Recoding to broader position categories for cross-year compatibility
    Position = ifelse(Name == "mulatu, j", "RB", Position),
    Position = ifelse(Name == "soefker, w", "DB", Position),
    Position = ifelse(Position == "P", "SP", Position),
    Position = ifelse(Position == "CB", "DB", Position),
    Test = str_replace_all(`Test Name`, "Wing Span", "Wingspan")
  ) %>%
  select(Name, Position, Test, Best) %>%
  na.omit(Best) %>%
  group_by(Name, Position, Test) %>%
  reframe(Best = ifelse(
    Test == "Vertical Jump",
    max(Best, na.rm = TRUE),
    if_else(Test == "10 yard sprint", min(Best, na.rm = TRUE), Best)
  )) %>%
  unique() %>%
  pivot_wider(names_from = Test, values_from = Best) %>%
  as.data.frame() %>%
  mutate(
    across(-c(Name, Position), ~ ifelse(grepl("[A-Za-z]", .), NA, .)),
    Height = ifelse(Height == 0, NA, Height),
    Wingspan = ifelse(Wingspan == 0, NA, Wingspan),
    `225 Bench` = ifelse(`225 Bench` == 0, NA, `225 Bench`)
  ) %>%
  rename(
    name_last_first_initial = Name,
    position = Position,
    sprint_10y = `10 yard sprint`,
    height = Height,
    vertical_jump = `Vertical Jump`,
    weight = Weight,
    bench_225 = `225 Bench`,
    bench = `BB Bench`,
    squat = `Back Squat`,
    broad_jump = `Broad Jump`,
    L_drill = `L-Drill`,
    pro_agility = `Pro Agility`,
    hang_clean = `Hang Clean`,
    wing = Wingspan
  ) %>%
  mutate(test_year = 2023) %>%
  left_join(roster[,c("name", "name_last_first_initial", "grad_year", "number", "status")], by = "name_last_first_initial")

##### 2024 #####
max_24_offseason_clean <- max_24_offseason %>%
  mutate(
    across(-c(athlete, position), ~ ifelse(grepl("[A-Za-z]", .), NA, .)),
    athlete = tolower(athlete),
    athlete = str_remove_all(athlete, "'"),
    athlete = str_replace_all(athlete, "ttt", "tt"),
    athlete = str_replace_all(athlete, "narisse", "narcisse"),
    athlete = str_replace_all(athlete, "dochatt", "dochat"),
    athlete = str_replace_all(athlete, "christian, c", "carter, c"),
    athlete = str_replace_all(athlete, "brown, dj", "brown, d"),
    athlete = str_replace_all(athlete, "casilli, c", "casilli, j"),
    athlete = str_replace_all(athlete, "drayton, j", "drayton, j"),
    athlete = str_replace_all(athlete, "holiday", "holliday"),
    athlete = str_replace_all(athlete, "ostland", "ostlund"),
    athlete = str_replace_all(athlete, "zach, m", "zack, m"),
    athlete = str_remove(athlete, "\\."),
    position = ifelse(position %in% c("IWR", "OWR", "WR"), "WR", position),
    position = ifelse(position == "CB", "DB", position),
    position = ifelse(position == "S", "DB", position),
    broad_jump = parse_ft_in(broad_jump),
    height = parse_ft_in(height),
    wing = str_remove_all(wing, '"'),
    # replace all 0 with NA
    across(
      c(
        wing,
        weight,
        height,
        flying_10,
        `10y_sprint`,
        vert_vertec,
        broad_jump,
        vert_mat,
        bench_225,
        shuttle_60y,
        L_drill,
        pro_agil
      ),
      ~ ifelse(. == 0, NA, .)
    ),
    across(
      c(
        wing,
        weight,
        height,
        flying_10,
        `10y_sprint`,
        vert_vertec,
        broad_jump,
        vert_mat,
        bench_225,
        shuttle_60y,
        L_drill,
        pro_agil
      ),
      as.numeric
    )
  ) %>%
  select(
    name = athlete,
    position,
    wing,
    weight,
    height,
    flying_10,
    `10y_sprint`,
    broad_jump,
    vert_mat,
    bench_225,
    shuttle_60y,
    L_drill,
    pro_agil
  ) %>%
  filter(!is.na(name)) %>%
  rename(
    sprint_10y = `10y_sprint`,
    pro_agility = pro_agil,
    vertical_jump = vert_mat
  )


max_24_season_clean <- max_24 %>%
  mutate(
    Max = ifelse(grepl("[A-Za-z]", Max), NA, Max),
    Max = ifelse(Max == "?", NA, Max),
    Max = as.numeric(Max),
    Athlete = tolower(Athlete),
    Athlete = str_remove_all(Athlete, "'"),
    Athlete = str_remove_all(Athlete, " jr"),
    Athlete = str_remove_all(Athlete, " iv"),
    last_name = str_extract(Athlete, "[a-z]+$"),
    first_initial = substr(str_extract(Athlete, "^[a-z]+"), 1, 1),
    name_match = paste0(last_name, ", ", first_initial),
    name_match = ifelse(name_match == "villiers, c", "de villiers, c", name_match)
  ) %>%
  pivot_wider(names_from = Exercise, values_from = Max) %>%
  # make column names safe
  rename(
    power_clean = `BB Power Clean`,
    squat = `BB Back Squat`,
    bench = `BB Bench Press`,
    position = Position
  ) %>%
  mutate(
    position = ifelse(position %in% c("IWR", "OWR", "WR"), "WR", position),
    position = ifelse(position == "CB", "DB", position),
    position = ifelse(position == "S", "DB", position),
    across(c(power_clean, squat, bench), as.numeric)
  ) %>%
  select(-last_name, -first_initial, -Athlete)

max_24_clean <- max_24_season_clean %>%
  full_join(max_24_offseason_clean, by = c("name_match" = "name", "position")) %>%
  rename(name_last_first_initial = name_match) %>%
  mutate(test_year = 2024,
         # manual fix for roster join
         name_last_first_initial = str_replace(name_last_first_initial, "obrien", "o'brien"),
         name_last_first_initial = str_replace(name_last_first_initial, "oconnell", "o'connell"),
         name_last_first_initial = str_replace(name_last_first_initial, "williams, s", "williams, jr., s"),
         name_last_first_initial = str_replace(name_last_first_initial, "ruvo", "ruvo iv"),
         name_last_first_initial = str_replace(name_last_first_initial, "bing, j", "bing, jr., j"),
         name_last_first_initial = str_replace(name_last_first_initial, "hegarty, c", "polemeni-hegarty, c")) %>%
  left_join(roster[,c("name", "name_last_first_initial", "grad_year", "number", "status")], by = "name_last_first_initial")

##### 2025 #####
max_25_clean <- max_25
names(max_25_clean) <- c("first", "last", "position", "bench", "squat", "vertical_jump", 
                         "hang_clean", "weight", "sprint_10y", "bench_225")
max_25_clean <- max_25_clean %>%
  mutate(position = word(str_replace(position, "/", " "), 1),
         name = tolower(paste(first, last, sep = " ")),
         name = str_replace(name, "michael fernicola", "mike fernicola"),
         name = str_replace(name, "liam forester", "liam forster"),
         name = str_replace(name, "alex armour", "alexandro armour, jr."),
         name = str_replace(name, "alexander jordan", "alex jordan"),
         name = str_replace(name, "jamal bing jr", "jamal bing, jr."),
         name = str_replace(name, "sean williams jr", "sean williams, jr."),
         test_year = 2025) %>%
  left_join(roster[,c("name", "grad_year", "number", "status")], by = "name")

##### Merge years #####
# Merge each year's cleaned max data
max_all_clean = bind_rows(max_20_clean, max_22_clean, max_23_clean, max_24_clean, max_25_clean) %>%
  select(name, 
         position, 
         number, 
         status,
         height, 
         wing, 
         weight, 
         bench_225, 
         bench, 
         squat, 
         vertical_jump, 
         broad_jump,
         sprint_10y, 
         hang_clean, 
         L_drill, 
         pro_agility,
         power_clean, 
         flying_10,
         shuttle_60y, 
         test_year, 
         grad_year)

names(max_all_clean) <- c(
  "Name",
  "Position",
  "Number",
  "Status",
  "Height",
  "Wingspan",
  "Weight",
  "225lb Bench",
  "Bench",
  "Squat",
  "Vertical Jump",
  "Broad Jump",
  "10Y Sprint",
  "Hang Clean",
  "L Drill",
  "Pro Agility",
  "Power Clean",
  "Flying 10",
  "60Y Shuttle",
  "Test Year",
  "Grad Year"
)



latest_tests <- max_all_clean %>%
  filter(!is.na(Name)) %>%
  arrange(desc(`Test Year`)) %>%
  group_by(Name) %>%
  summarize(across(
    everything(),
    ~ first(.[!is.na(.)])
  ), .groups = "drop") %>%
  ungroup() %>%
  filter(!is.na(Status))

# count NAs in each column
observations <- latest_tests %>%
  summarise_all( ~ sum(!is.na(.)))

##### Feb 2025 Junior Day ##### 
jr_day <- read_csv("data/junior_day_2.25.csv", skip = 2) %>%
  mutate(Name = paste(`First Name`, `Last Name`, sep = " "),
         Name = tolower(Name),
         Name = str_remove(Name, "\\."),
         Height = parse_ft_in(Height),
         Wingspan = parse_ft_in(`Wing Span`),
         Position = "DB",
         Status = "HS"
         ) %>%
  select(Name, Height, Weight, Wingspan,Position, Status)

##### Write to JSON #####
max_all_clean <- max_all_clean %>%
  select(-Name, -Number, -`Test Year`, -`Grad Year`)
  
max_all_json <- max_all_clean %>%
  group_by(Position) %>%
  nest() %>%
  mutate(data = map(data, ~ summarise_all(.x, ~ list(na.omit(
    .
  ))))) %>%
  mutate(data = map(data, ~ set_names(.x, names(max_all_clean)[-1]))) %>%
  deframe()


write_json(max_all_json, path = "stats_2020_2024.json", pretty = TRUE)


latest_tests_json <-  latest_tests %>%
  as.list()

write_json(latest_tests_json, path = "latest_tests_2020_2024.json", pretty = TRUE)

jr_day_json <- jr_day %>%
  as.list()

write_json(jr_day_json, path = "jr_day_2025.json", pretty = TRUE)

#### EDA ####
##### RSI ##### 
rsi_test <- latest_tests %>%
  select(Name, Position, Status, Weight, Bench, Squat, `Power Clean`) %>%
  mutate(RSI = (Bench + Squat) / (2 * Weight))

ggplot(rsi_test %>% filter(!is.na(RSI)) %>% arrange(desc(RSI)) %>% head(10)) +
  geom_bar(aes(y = reorder(Name, RSI), x = RSI), fill = "navy", stat = "identity") +
  labs(title = "Top 10 RSI athletes", y = "", x = "Relative Strength Index (RSI)", caption = "RSI calculated using Bench and Squat statistics from athletes' latest spring testing. Excludes athletes with missing tests.") +
  theme_minimal() 

rsi_test %>%
  group_by(Position) %>%
  summarize(RSI = median(RSI, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(y = reorder(Position, RSI), x = RSI), fill = "navy", stat = "identity") +
  labs(title = "Median RSI by Position Group", y = "", x = "Relative Strength Index (RSI)") +
  theme_minimal()

rsi_test %>%
  group_by(Status) %>%
  summarize(RSI = median(RSI, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(y = reorder(Status, RSI), x = RSI), fill = "navy", stat = "identity") +
  labs(title = "Median RSI by Year", y = "", x = "Relative Strength Index (RSI)") +
  theme_minimal()

  ggplot(rsi_test) +
  geom_point(aes(x = Weight, y = RSI)) +
  geom_smooth(aes(x = Weight, y = RSI), se = FALSE) +
  labs(title = "RSI calculation penalizes heavier athletes", y = "Relative Strength Index", subtitle = "n = 129") +
  theme_minimal()
  
  ggplot(rsi_test) +
    geom_point(aes(x = Weight, y = Squat)) +
    geom_smooth(aes(x = Weight, y = Squat), se = FALSE) +
    labs(title = "Squat by Weight", y = "Squat", subtitle = "n = 133") +
    theme_minimal()
  
  ggplot(rsi_test) +
    geom_point(aes(x = Weight, y = Bench)) +
    geom_smooth(aes(x = Weight, y = Bench), se = FALSE) +
    labs(title = "Bench by Weight", y = "Bench", subtitle = "n = 133") +
    theme_minimal()
  
  ggplot(rsi_test) +
    geom_point(aes(x = Weight, y = `Power Clean`)) +
    geom_smooth(aes(x = Weight, y = `Power Clean`), se = FALSE) +
    labs(title = "Power Clean by Weight", y = "Power Clean", subtitle = "n = 133") +
    theme_minimal()

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
