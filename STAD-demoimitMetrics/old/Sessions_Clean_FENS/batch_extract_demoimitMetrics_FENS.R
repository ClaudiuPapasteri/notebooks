# library(tidyverse)

main_folder <- "C:/Users/Mihai/Desktop/R Notebooks/notebooks/STAD-demoimitMetrics/Sessions_Clean_demoimitMectrics"
setwd(main_folder)

#################

# Read clean table
data_clean_list <- readRDS("data_clean_output.RDS")
data_clean <- data_clean_list$data_clean

diff_list <- data_clean_list$diff_list

# Fix some stuff in clean table
pattern_files_botrec <- "^(?=.*TestSession|config)(?=.*botrec)"                        # contains "TestSession" or "config", contains ".botrec"

data_clean2 <-    # 1. fisier_obtinut3 need to be coalesced to fisier_obtinut1 or fisier_obtinut2
  data_clean %>%
  dplyr::mutate(
    across(.cols = contains("fisier_obtinut"),                                  # anything not compling to botrec file pattern to NA
           .fns = ~ if_else(
             grepl(pattern = pattern_files_botrec, .x, perl = TRUE), .x, NA_character_) 
    )   
  ) %>%
  dplyr::mutate(fisier_obtinut1 = dplyr::coalesce(fisier_obtinut1, fisier_obtinut3)) %>% # coalesce fisier_obtinut3 recordings in fisier_obtinut1 --- THIS WILL PROBALY CHANCE IN FUTURE
  dplyr::mutate(conditie1 = dplyr::coalesce(conditie1, conditie3))        # do the same for conditie --- THIS WILL PROBABLY CHANGE

data_clean2 <-    # 2. fisier_obtinut*_sec column store botrec files for sessions that have multiple files and need column to code for these problems --- THIS WILL PROBALY CHANCE IN FUTURE
  data_clean2 %>% 
  dplyr::mutate(
    problem = if_any(.cols = matches("fisier_obtinut.*\\_sec"),   # if the is nonNA in colums containing "sec", tag it in problem column
                     .fns = ~ !is.na(.x)),
    problem = as.numeric(problem)
  )

# 3. if botrec file is listed in diff_list, tag it in problme column (probably the file does not exist, but tag it any way)
# ---- THIS WILL CHANCE
diff_df <- as.data.frame(diff_list)
match(data_clean2$fisier_obtinut2, diff_df$fisier_obtinut2) 
idx <- which(data_clean2$fisier_obtinut2 %in%  diff_df$fisier_obtinut2)

data_clean2$problem[idx] <- 1    # tag as problematic 


# Some more cleaning
data_clean2 <-
  data_clean2 %>%
  dplyr::mutate(id = toupper(id),
                conditie1 = toupper(conditie1),
                conditie2 = toupper(conditie2),
                partener1 = toupper(partener1),
                partener2 = toupper(partener2)) %>%
  dplyr::filter(id != "TEST") %>%
  dplyr::filter(problem != 1) %>%
  dplyr::select(-c("nume", "contact", "subiect_eeg1", "subiect_eeg2", "ora1", "ora2", "ora3", "casca1", "casca2", "casca3",
                   "observatii1", "observatii2", "observatii3", "inreg_folosita3", "fisier_obtinut3", "data3_in_caz_de_erori_la_data1_sau2",
                   "fisier_obtinut2_sec", "conditie3", "asistenti3", "config_folosita3"))

data_clean2 <-
  data_clean2 %>%
  dplyr::select(id, problem,
                conditie1, partener1, fisier_obtinut1, 
                conditie2, partener2, fisier_obtinut2) %>%
  dplyr::filter(!is.na(fisier_obtinut1), !is.na(fisier_obtinut2))

data_clean2 <-
  data_clean2 %>%    # THIS WILL PROBABLY NEED CHANGING -- THE ONLINE TABLE HAS MERGED ROWS!!!!!!
  replace_na(list(conditie1 = "SOCIAL", conditie2 = "SOCIAL"))      # all missing are social condition

########################################################
# FENS conf

fens_folder <- "C:/Users/Mihai/Desktop/R Notebooks/notebooks/STAD-demoimitMetrics/Sessions_Clean_FENS"
setwd(fens_folder)

# # Read lists of recordings with demoimitMetrics
fisier_obtinut1 <- readRDS("fisier_obtinut1_FENS.RDS")
fisier_obtinut2 <- readRDS("fisier_obtinut2_FENS.RDS")

# Define function
list_to_nested <- function(list, group_var){
  list %>%
    do.call(dplyr::bind_rows, .) %>%
    dplyr::group_by( {{ group_var }} ) %>%
    tidyr::nest()
}

# Lists of recordings to nested data frames
fisier_obtinut1_df <- list_to_nested(fisier_obtinut1, file)
fisier_obtinut2_df <- list_to_nested(fisier_obtinut2, file)

# Merge nested data frames into clean table
full_data <-
  data_clean2 %>%
  dplyr::left_join(fisier_obtinut1_df, by = c("fisier_obtinut1" = "file")) %>%
  dplyr::rename(data_fisier_obtinut1 = data) %>%
  dplyr::left_join(fisier_obtinut2_df, by = c("fisier_obtinut2" = "file")) %>%
  dplyr::rename(data_fisier_obtinut2 = data)



# Data for export
full_data_output <- full_data %>%
  dplyr::select(-problem)



old_names <- c("conditie1", "partener1", "fisier_obtinut1", "conditie2", "partener2", "fisier_obtinut2", "data_fisier_obtinut1", "data_fisier_obtinut2")
new_names <- c("conditie_1", "partener_1", "fisier_obtinut_1", "conditie_2", "partener_2", "fisier_obtinut_2", "data_fisier_obtinut_1", "data_fisier_obtinut_2")

full_data_output_nest <-
  full_data_output %>%
  dplyr::rename_at(all_of(old_names), ~ new_names) %>%
  tidyr::pivot_longer(cols = -id, names_to = c(".value", "order"), names_pattern = "(.*)_(\\d+)$", values_to = "data")


full_data_output_clean <-
  full_data_output_nest %>%
  unnest(cols = data_fisier_obtinut)


# Keep only rows where distance between timeStamp and timeStamp_demo or timeStamp_imit are minimal --- to remove duplicates from matching
full_data_output_clean2 <-
  full_data_output_clean %>%
  dplyr::mutate(timeStamp_demoimit = dplyr::coalesce(timeStamp_demo, timeStamp_imit),
                timeStamp_dist = abs(timeStamp - timeStamp_demoimit)) %>%
  dplyr::group_by(fisier_obtinut, timeStamp_demoimit) %>%
  slice(which.min(timeStamp_dist)) 
  


# Some additional tidying
vars_exclude <- c(
  # General    
  "InputY", "InputX", "xPos", "yPos", "zPos", "xRot", "yRot", "zRot", "wRot", "markerX", "markerY", "markerZ",
  # Demo
  "demoMetrics_playerPosition_x", "demoMetrics_playerPosition_y", "demoMetrics_playerPosition_z", "demoMetrics_markerPosition_x",  
  "demoMetrics_markerPosition_y", "demoMetrics_markerPosition_z", "demoMetrics_distanceFromMarker", "timeStamp_demo",
  # Imit
  "imitMetrics_playerPosition_x", "imitMetrics_playerPosition_y", "imitMetrics_playerPosition_z", "imitMetrics_markerPosition_x", 
  "imitMetrics_markerPosition_y", "imitMetrics_markerPosition_z", "imitMetrics_distanceFromMarker", "timeStamp_imit",
  # syncKey
  "syncKey"
)

full_data_output_clean2 <- 
  full_data_output_clean2 %>%
  dplyr::select(-all_of(vars_exclude))


full_data_output_clean2 <- 
  full_data_output_clean2 %>%
  dplyr::mutate(demoimitState = dplyr::case_when(newGameState_f %in% 5:11 ~ "Demo",
                                                 newGameState_f %in% 12:20 ~ "Imit",
                                                 TRUE ~ NA_character_)) %>%
  dplyr::mutate(playerType = dplyr::coalesce(demoMetrics_playerType, imitMetrics_playerType),
                markerType = dplyr::coalesce(demoMetrics_markerType, imitMetrics_markerType),
                score = dplyr::coalesce(demoMetrics_score, imitMetrics_score)) %>%
  dplyr::mutate(who = dplyr::case_when(stringr::str_detect(fisier_obtinut, "PLAYER_1") ~ 1L,
                                       stringr::str_detect(fisier_obtinut, "PLAYER_2") ~ 2L,
                                       TRUE ~ NA_integer_)) %>%
  dplyr::select(-c(demoMetrics_playerType, imitMetrics_playerType, demoMetrics_markerType, imitMetrics_markerType, demoMetrics_score, imitMetrics_score))

####
# Hack solve for bot recordings --- THIS WILL NEED TO BE INVESTIGATED AND CHANGED
full_data_output_clean_social <-
  full_data_output_clean2 %>%
  dplyr::filter(conditie == "SOCIAL")

full_data_output_clean_bot <-
  full_data_output_clean2 %>%
  dplyr::filter(conditie == "BOT") %>%
  dplyr::group_by(fisier_obtinut) %>%
  dplyr::mutate(
    row_num = dplyr::row_number(),
    timeStamp_order = dplyr::case_when(timeStamp - lag(timeStamp, n = 1) >= 0 ~ "increase",   # DOESNT WORK BECAUSE DATA ALREADY SORTED IN ASCENDING ORDER :(
                                       timeStamp - lag(timeStamp, n = 1) < 0 ~ "decrease",
                                       TRUE ~ NA_character_)
    )
  
                

# full_data_output_clean_bot <-
#   full_data_output_clean_bot %>%
#   dplyr::filter(newGameState_f %in% c(10, 15:19)) %>%   # drop rows with GameStates that should not exist
#####

   
# Keep only data from the actual player (where playerType == who)
full_data_output_clean3 <-
  dplyr::bind_rows(full_data_output_clean_social, full_data_output_clean_bot) %>%
  dplyr::filter(playerType == who)





