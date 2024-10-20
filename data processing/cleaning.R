library(tidyverse)
library(ggplot2)
library(conflicted)
library(pander)
library(purrr)
library(jsonlite)

conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

parse_ft_in <- function(ft_in_str) {
  
  # Use ifelse() to handle multiple inputs
  is_ft_in <- grepl("'", ft_in_str)
  
  total_inches <- ifelse(
    is_ft_in,
    {
      parts <- strsplit(ft_in_str, "'")
      feet <- as.numeric(sapply(parts, "[", 1))
      inches <- as.numeric(sapply(gsub("\"", "", sapply(parts, "[", 2)), as.numeric))
      (feet * 12) + inches
    },
    as.numeric(ft_in_str)
  )
  
  return(total_inches)
}

parse_height_notation <- function(height_str) {
  # Separate the feet, inches, and decimal part
  feet <- as.numeric(substr(height_str, 1, 1))  # First digit for feet
  inches_full <- as.numeric(substr(height_str, 2, 5)) / 100  # Next four digits as inches
  
  # Convert total height to inches
  total_inches <- (feet * 12) + inches_full
  
  return(total_inches)
}

#### Load the data ####
max_22 <- read_csv("data/spring_2022_maxes.csv") 
max_24 <- read_csv("data/spring_2024_maxes_long.csv", col_select = c(Athlete, Position, Exercise, Max)) 
max_24_offseason <- read_csv("data/spring_2024_offseason_bests.csv")

#### Prep data ####
max_22_clean <- max_22%>%
  mutate(Athlete = tolower(Athlete),
         Broad_Jump = parse_ft_in(Broad_Jump),
         Height = parse_height_notation(Height),
         across(c(Height, Wing, Weight, Bodyfat, Pre_225, Bench, Squat, Deadlift, Total, `Combine_%_of_BW`,
                         `10yd_Official`, Vertical_Jump, Broad_Jump), as.numeric)) 
  

max_24_offseason_clean <- max_24_offseason %>%
  mutate(athlete = tolower(athlete),
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
         position = str_replace(position, "CB", "DB"),
         broad_jump = parse_ft_in(broad_jump),
         height = parse_ft_in(height),
         wing = str_remove_all(wing, '"'),
         # replace all 0 with NA
         across(c(wing, weight, height, flying_10, `10y_sprint`, vert_vertec, broad_jump,
                  vert_mat, bench_225, shuttle_60y, L_drill, pro_agil), ~ifelse(. == 0, NA, .)),
         across(c(wing, weight, height, flying_10, `10y_sprint`, vert_vertec, broad_jump,
                  vert_mat, bench_225, shuttle_60y, L_drill, pro_agil), as.numeric)) %>%
  select(athlete, position, wing, weight, height, flying_10, `10y_sprint`, vert_vertec, broad_jump, vert_mat, 
         bench_225, shuttle_60y, L_drill, pro_agil) %>%
  filter(!is.na(athlete)) %>%
  rename(dash_10y = `10y_sprint`,
         l_drill = L_drill)


max_24_clean <- max_24 %>%
  mutate(Max = as.numeric(Max),
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
         position = str_replace(position, "CB", "DB"),
         across(c(power_clean, squat, bench), as.numeric)) %>%
  select(-last_name, -first_initial, -Athlete)

#### Athlete level stats ####
stats_24 <- max_24_clean %>%
  full_join(max_24_offseason_clean, by = c("name_match" = "athlete", "position")) %>%
  select(-name_match)

json_structure <- stats_24 %>%
  group_by(position) %>%
  nest() %>%
  mutate(data = map(data, ~ summarise_all(.x, ~ list(na.omit(.))))) %>%
  mutate(data = map(data, ~ set_names(.x, names(stats_24)[-1]))) %>%
  deframe()

write_json(json_structure, path = "stats_position_2024.json", pretty = TRUE)

#### Group means ####
max_22_group <- max_22_clean %>%
group_by(Position) %>%
  summarize(height = mean(Height, na.rm = TRUE),
            wing = mean(Wing, na.rm = TRUE),
            weight = mean(Weight, na.rm = TRUE),
            bodyfat = mean(Bodyfat, na.rm = TRUE),
            bench_225 = mean(Pre_225, na.rm = TRUE),
            bench = mean(Bench, na.rm = TRUE),
            squat = mean(Squat, na.rm = TRUE),
            deadlift = mean(Deadlift, na.rm = TRUE),
            dash_10y = mean(`10yd_Official`, na.rm = TRUE),
            vertical_jump = mean(Vertical_Jump, na.rm = TRUE),
            broad_jump = mean(Broad_Jump, na.rm = TRUE))

max_24_group <- max_24_clean %>%
  group_by(position) %>%
  summarize(power_clean = mean(power_clean, na.rm = TRUE),
            squat = mean(squat, na.rm = TRUE),
            bench = mean(bench, na.rm = TRUE))

max_24_off_group <- max_24_offseason_clean %>%
  group_by(position) %>%
  summarize(wing = mean(wing, na.rm = TRUE),
            weight = mean(weight, na.rm = TRUE),
            height = mean(height, na.rm = TRUE),
            flying_10 = mean(flying_10, na.rm = TRUE),
            dash_10y = mean(dash_10y, na.rm = TRUE),
            vert_vertec = mean(vert_vertec, na.rm = TRUE),
            vert_mat = mean(vert_mat, na.rm = TRUE),
            broad_jump = mean(broad_jump, na.rm = TRUE),
            bench_225 = mean(bench_225, na.rm = TRUE),
            shuttle_60y = mean(shuttle_60y, na.rm = TRUE),
            l_drill = mean(l_drill, na.rm = TRUE),
            pro_agil = mean(pro_agil, na.rm = TRUE))

dat_24 <- max_24_group %>%
  left_join(max_24_off_group, by = "position")
  
dat_24_json <- toJSON(dat_24)
write_json(dat_24, "data/max_24_group.json")
  
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
