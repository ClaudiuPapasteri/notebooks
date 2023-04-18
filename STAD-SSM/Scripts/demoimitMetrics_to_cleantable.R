# library(tidyverse)

folder <- "C:/Users/Mihai/Desktop/R Notebooks/notebooks/STAD-demoimitMetrics"
setwd(folder)

#################

# Read clean table
data_clean_list <- readRDS("data_clean_output_v2.RDS")
data_clean <- data_clean_list$data_clean

diff_list <- data_clean_list$diff_list

# Filter only recordings that do not have problems
data_clean <-
  data_clean %>%
  dplyr::filter(problem != 1) 


########################################################

# Read lists of recordings with demoimitMetrics
sessions_clean_data <- readRDS("sessions_clean80.RDS")


# Define function
list_to_nested <- function(list, group_var){
  list %>%
    do.call(dplyr::bind_rows, .) %>%
    dplyr::group_by( {{ group_var }} ) %>%
    tidyr::nest()
}

# Lists of recordings to nested data frames
sessions_clean_data_df <- list_to_nested(sessions_clean_data, file)

# Merge nested data frames into clean table
full_data <-
  data_clean %>%
  dplyr::left_join(sessions_clean_data_df, by = c("output_file_clean" = "file")) 


# Data for export
full_data_output <- 
  full_data %>%
  dplyr::select(-problem) %>%
  unnest(cols = data)

# Some additional tidying
vars_exclude <- c(
  # General    
  "InputY", "InputX", "xPos", "yPos", "zPos", "xRot", "yRot", "zRot", "wRot", "markerX", "markerY", "markerZ",
  # Demo
  "demoMetrics_playerPosition_x", "demoMetrics_playerPosition_y", "demoMetrics_playerPosition_z", "demoMetrics_markerPosition_x",  
  "demoMetrics_markerPosition_y", "demoMetrics_markerPosition_z", "demoMetrics_distanceFromMarker", 
  # Imit
  "imitMetrics_playerPosition_x", "imitMetrics_playerPosition_y", "imitMetrics_playerPosition_z", "imitMetrics_markerPosition_x", 
  "imitMetrics_markerPosition_y", "imitMetrics_markerPosition_z", "imitMetrics_distanceFromMarker", 
  # syncKey
  "syncKey"
)

full_data_output_clean <- 
  full_data_output %>%
  dplyr::select(-all_of(vars_exclude))


full_data_output_clean <- 
  full_data_output_clean %>%
  dplyr::mutate(demoimitState = dplyr::case_when(newGameState_f %in% 5:11 ~ "Demo",
                                                 newGameState_f %in% 12:20 ~ "Imit",
                                                 TRUE ~ NA_character_)) %>%
  dplyr::mutate(timeStamp_demoimit = dplyr::coalesce(timeStamp_demo, timeStamp_imit), 
                playerType = dplyr::coalesce(demoMetrics_playerType, imitMetrics_playerType),
                markerType = dplyr::coalesce(demoMetrics_markerType, imitMetrics_markerType),
                score = dplyr::coalesce(demoMetrics_score, imitMetrics_score)) %>%
  dplyr::mutate(who = dplyr::case_when(stringr::str_detect(output_file_clean, "PLAYER_1") ~ 1L,
                                       stringr::str_detect(output_file_clean, "PLAYER_2") ~ 2L,
                                       TRUE ~ NA_integer_)) %>%
  dplyr::select(-c(demoMetrics_playerType, imitMetrics_playerType, demoMetrics_markerType, imitMetrics_markerType, demoMetrics_score, imitMetrics_score,
                   timeStamp_demo, timeStamp_imit))


# Keep only data from the actual player (where playerType == who)
full_data_output_clean <-
  full_data_output_clean %>%
  dplyr::filter(playerType == who)


# Exclude multiple timpeStamp_demoimit per timeStamp match (only adjusting tolerance will lose data as matches are not exact)
full_data_output_clean <-
  full_data_output_clean %>%
  dplyr::mutate(timeStamp_diff = abs(timeStamp - timeStamp_demoimit)) %>%
  dplyr::group_by(id, condition, timeStamp_demoimit) %>%
  dplyr::slice_min(timeStamp_diff) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(id, condition, timeStamp) %>%
  dplyr::slice_min(timeStamp_diff) %>%
  dplyr::ungroup()

# Make newGameState_clean --- a bit hacky 

full_data_output_clean <-
  full_data_output_clean %>%
  dplyr::mutate(
    newGameState_clean = 
      dplyr::case_when(newGameState_f == "9" ~ 10L,             # is 9 or 14 when it should be 10 or 15
                       newGameState_f == "14" ~ 15L,
                       newGameState_f %in% as.character(c(10, 15:19)) ~ as.integer(newGameState_f),
                       TRUE ~ NA_integer_
      ),
    newGameState_clean =                                 # some matched the previous frame so GameStates repeat
      dplyr::if_else(newGameState_clean == dplyr::lag(newGameState_clean) & !is.na(dplyr::lag(newGameState_clean)), 
                     newGameState_clean + 1L, 
                     newGameState_clean
     ),
    newGameState_clean =                                 # GameState 10 following another 10 now became 11
      dplyr::if_else(newGameState_clean == 11L, 10L, newGameState_clean)
  ) %>%                                          # !CAREFUL columns computed based on newGameState_f like demoimitState now may be wrong
  dplyr::mutate(demoimitState_clean = dplyr::case_when(newGameState_clean %in% 5:11 ~ "Demo",
                                                       newGameState_clean %in% 12:20 ~ "Imit",
                                                       TRUE ~ NA_character_))
  

# Check data for bot and social
data_bot <- full_data_output_clean %>% dplyr::filter(condition == "bot")
data_soc <- full_data_output_clean %>% dplyr::filter(condition == "social")

check_bot <-
  data_bot %>%
  dplyr::group_by(id, condition) %>%
  dplyr::mutate(grp = as.integer(gl(n(), 6, n()))) %>%    # group every 6 rows
  dplyr::group_by(id, condition, grp) %>%
  dplyr::summarise(pattern = paste0(newGameState_f, collapse = " "),
                   pattern_clean = paste0(newGameState_clean, collapse = " "))

check_soc <-
  data_soc %>%
  dplyr::group_by(id, condition) %>%
  dplyr::mutate(grp = as.integer(gl(n(), 6, n()))) %>%    # group every 6 rows
  dplyr::group_by(id, condition, grp) %>%
  dplyr::summarise(pattern = paste0(newGameState_f, collapse = " "),
                   pattern_clean = paste0(newGameState_clean, collapse = " "))


  



