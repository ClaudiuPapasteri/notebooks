library(tidyverse)
library(googledrive)
library(tidyverse)
library(rio)
# library(googlesheets4)

folder <- "C:/Users/Mihai/Desktop/R Notebooks/notebooks/STAD-demoimitMetrics"
setwd(folder)

#################
# Read clean table

# data_clean_list <- readRDS("data_clean_output_ieeg.RDS")
# data_clean <- data_clean_list$data_clean
# diff_list <- data_clean_list$diff_list

# We would but there is no clean table for updates ... this section is from sort_botrec_files_ieeg specially for updates

# Get the data from Google Drive
sheet_url <- "https://docs.google.com/spreadsheets/d/1OLTjD36e_SWbf_kTKR7H13kPsJPmi-150uJPVo_avig/edit?usp=sharing"
sheet_name_ic <- "date SSM Claudiu IC"
file_name <- paste0("STAD-inregistrari_", Sys.Date())

###############################
if(interactive()){
  want_dl <- readline("Want to re-download data? 1-YES 2-NO: ")
  if (want_dl == 1) {
    # Call Google Drive authentication forcing interactive login and save in cache 
    googledrive::drive_auth(use_oob = TRUE, cache = TRUE)      
    
    # Reuse token to Sheet authentification 
    # googlesheets4::gs4_auth(token = googledrive::drive_token())    # the file is .xlsx not google sheet
    
    # Download the .xlsx from google drive
    googledrive::drive_download(sheet_url, path = file.path(folder, file_name), overwrite = TRUE)
  }
}  
##################################################################################################################


# Read the latest .xlsx file for ieeg
file_to_read <- 
  dir(folder, pattern = ".*xlsx", full.names = TRUE) %>% 
  file.info() %>%
  dplyr::arrange(dplyr::desc(ctime)) %>%
  dplyr::slice(1) %>%
  row.names()

data <- rio::import(file = file_to_read, which = sheet_name_ic)


# Filter to IDs that are updates
data <- 
  data %>%
  dplyr::filter((id == "98IC" & condition == "bot") | (id %in% c("109IC", "109PIC")))
  


##################################################################################################################
# Clean contents of columns with .botrec file names 
data_clean <- 
  data %>% 
  tidyr::separate(col = "output_file", 
                  into = c("output_file", "output_file_sec"),
                  sep = "\\sSI\\s|\\s\\+\\s") %>%                               # some have multiple file names separated by " SI " or " + "
  dplyr::select(where(~!all(is.na(.x)))) %>%                                    # remove if created columns that have only NAs
  dplyr::mutate(
    across(.cols = contains("output_file"),
           .fns = ~ gsub("\\s", "", .x))                                        # remove white spaces
  ) %>%
  dplyr::mutate(
    across(.cols = contains("output_file"),
           .fns = ~ if_else(is.na(.x), "-", .x))                                # replace NAs with "-"
  ) %>%
  dplyr::mutate(
    across(.cols = contains("output_file"),                                  # paste ".botrec" extension to names that contains "TestSession" or "config" but do not end with "botrec"
           .fns = ~ if_else(
             grepl(pattern = "^(?=.*TestSession|config|Config_)(?!.*botrec)", .x, perl = TRUE), paste0(.x, ".botrec"), .x)
    )   
  ) %>%
  dplyr::mutate(
    across(.cols = contains("bot_recording"),                                  # paste ".botrec" extension to names that contains "TestSession" or "config" but do not end with "botrec"
           .fns = ~ if_else(
             grepl(pattern = "^(?=.*TestSession|config|Config_)(?!.*botrec)", .x, perl = TRUE), paste0(.x, ".botrec"), .x)
    )  
  )

#########################################################################################

# Log recordings with problems
# -- there are no problems for updates
data_clean <-    
  data_clean %>% 
  dplyr::mutate(problem = rep(0, nrow(.))) 


#########################################
# Add info: who, output_file_clean

data_clean <-    
  data_clean %>% 
  dplyr::mutate(who = dplyr::case_when(stringr::str_detect(output_file, "PLAYER_1") ~ 1L,
                                       stringr::str_detect(output_file, "PLAYER_2") ~ 2L,
                                       TRUE ~ NA_integer_)) %>%
  dplyr::mutate(base_name = stringr::str_remove(output_file, "\\.botrec"),
                base_name = dplyr::if_else(base_name == "-", NA_character_, base_name),
                output_file_clean = stringr::str_c(base_name, "_clean", ".botrec")
  ) %>%
  dplyr::select(-base_name)


# Filter only recordings that do not have problems  -- there were no problem recordings
# data_clean <-
#   data_clean %>%
#   dplyr::filter(problem != 1) 


########################################################

# Read lists of recordings with demoimitMetrics
sessions_clean_data <- readRDS("sessions_clean_ieeg_update1.RDS")


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
  dplyr::select(-any_of("problem")) %>%
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
  dplyr::select(-any_of(vars_exclude))


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
  

# Export update ieeg data
full_data_output_clean %>%
  tidyr::separate(id, into = c("num_id", "code"), "(?<=[0-9])(?=[a-zA-Z])", remove = FALSE) %>%
  dplyr::mutate(id = as.factor(id),
                code = as.factor(code)
  ) %>%
  rio::export(., "STAD-demoimitMetrics-ieeg_update1.xlsx")



