library(tidyverse)
library(jsonlite)
library(fuzzyjoin)

##################################################################################################################
# Function for data extraction
##################################################################################################################

extract_botrec <- function(file, path) {

  botrec_file_name <- file.path(path, file)
  
  botrec_file_size <- file.info(botrec_file_name)$size
  
  botrec_file_con <- file(botrec_file_name, "rb")
  
  check0 <- readChar(botrec_file_con, nchars = 4)
  
  # Header JSON
  header_len <- readBin(botrec_file_con, integer(), size = 4)     # these are 4 bits as integer
  header <- readChar(botrec_file_con, nchars = header_len)
  header_list <- jsonlite::fromJSON(header, simplifyVector = FALSE)
  # file_version <- header_list[["fileVersion"]]
  sessioncfg_list <- header_list[["sessionConfig"]]
  # recordchunks_list <- header_list[["recordingChunks"]]
  
  check1 <- readChar(botrec_file_con, nchars = 4)
  
  # Record JSON
  record <- readChar(botrec_file_con, nchars = botrec_file_size - 12 - header_len)
  record_list <- jsonlite::fromJSON(record, simplifyVector = FALSE)
  # schema <- record_list[["_schemaVersion"]]
  frames_list <- record_list[["frames"]]
  
  # Close and delete nuissance
  close(botrec_file_con)
  rm(header_list, record_list)
  
  # Extract Metrics
  demoMetrics_df <- 
    sessioncfg_list %>%
    purrr::pluck("demoMetrics") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(playerPosition, names_sep = "_") %>%
    tidyr::unnest_wider(markerPosition, names_sep = "_") %>%
    dplyr::rename_with(.fn = ~ paste0("demoMetrics_", .), .cols = -c("timeStamp"))
  
  imitationMetrics_df <- 
    sessioncfg_list %>%
    purrr::pluck("imitationMetrics") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(playerPosition, names_sep = "_") %>%
    tidyr::unnest_wider(markerPosition, names_sep = "_") %>%
    dplyr::rename_with(.fn = ~ paste0("imitMetrics_", .), .cols = -c("timeStamp")) 
  
  # Unpack frames
  frames_df <- 
    frames_list %>% 
    tibble::enframe(name = "frame", value = "value") %>% 
    tidyr::unnest_wider(value) %>%
    tidyr::unnest(inputManagerStates, keep_empty = TRUE) %>%
    tidyr::unnest(syncedProperties, keep_empty = TRUE) %>%
    tidyr::unnest_wider(syncedProperties) %>%
    tidyr::unnest_wider(inputManagerStates)
  
  vars_to_fill <- c("InputY", "InputX", "xPos",   "yPos", "zPos", "xRot", "yRot", "zRot", "wRot")
  
  frames_df_wide <- 
    frames_df %>%
    tidyr::pivot_wider(names_from = name, values_from =  value) %>%
    dplyr::select(-c(`NA`)) %>%
    tidyr::pivot_wider(names_from = inputId, values_from =  axisValue) %>%
    dplyr::select(-c(`NA`)) %>%
    tidyr::fill(dplyr::all_of(vars_to_fill), .direction = "down") %>%
    dplyr::mutate(timeStamp = as.numeric(sub(",", ".", timeStamp))) %>%   # timeStamp is encoded as char and needs to be converted (see doc)
    dplyr::mutate(previousGameState_f = previousGameState, newGameState_f = newGameState) %>%
    tidyr::fill(previousGameState_f, newGameState_f, .direction = "down") 
  
  # Merge frames with demoMetrics and imitationMetrics
    demo_col_names <- colnames(demoMetrics_df)
    demo_col_names[demo_col_names == "timeStamp"] <- "timeStamp_demo"
  
    imit_col_names <- colnames(imitationMetrics_df)
    imit_col_names[imit_col_names == "timeStamp"] <- "timeStamp_imit"
  
  frames_df_wide_2 <- 
    frames_df_wide %>%
    fuzzyjoin::difference_left_join(demoMetrics_df, by = "timeStamp", max_dist = 10^-2) %>%    # changed from max_dist = 10^-4
    dplyr::rename(timeStamp = timeStamp.x, timeStamp_demo = timeStamp.y) %>%
    fuzzyjoin::difference_left_join(imitationMetrics_df, by = "timeStamp", max_dist = 10^-2) %>%  # changed from max_dist = 10^-4
    dplyr::rename(timeStamp = timeStamp.x, timeStamp_imit = timeStamp.y) 
  
  
  # Check -- checks are fine but dont add much
  # sum(!is.na(frames_df_wide_2$timeStamp_demo)) == length(unique(demoMetrics_df$timeStamp))
  # sum(!is.na(frames_df_wide_2$timeStamp_imit)) == length(unique(imitationMetrics_df$timeStamp))
  
  # idx_demo <- which(!is.na(frames_df_wide_2$timeStamp_demo) & frames_df_wide_2$demoMetrics_playerType == 1)  
  # check_1 <-
  #   all.equal(
  #     frames_df_wide_2[idx_demo, c("xPos", "yPos", "zPos")],
  #     frames_df_wide_2[idx_demo, c("demoMetrics_playerPosition_x", "demoMetrics_playerPosition_y", "demoMetrics_playerPosition_z")],
  #     check.names = FALSE
  #   )
  # if(is.logical(check_1) && check_1) cat("Check 1 completed succesfully ") else cat(check_1)
  # 
  # idx_imit <- which(!is.na(frames_df_wide_2$timeStamp_imit) & frames_df_wide_2$imitMetrics_playerType == 1 & frames_df_wide_2$newGameState != 18) 
  # check_2 <- 
  #   all.equal(
  #     frames_df_wide_2[idx_imit, c("xPos", "yPos", "zPos")],
  #     frames_df_wide_2[idx_imit, c("imitMetrics_playerPosition_x", "imitMetrics_playerPosition_y", "imitMetrics_playerPosition_z")],
  #     check.names = FALSE
  #   )    # in GameState 18 the playerPosition is the SugestionMarker position; this can also be seen in "imitMetrics_playerPosition_y" that is 0.05 (not 0.08)
  # if(is.logical(check_2) && check_2) cat("Check 2 completed succesfully ", "\n") else cat(check_2, "\n")
  
  
  # Keep only necesary stuff --- this will probably need changing
  file_name <- basename(botrec_file_name)
  folder_name <- sub(pattern = ".*\\/", replacement = "", x = dirname(botrec_file_name))
  
  frames_df_wide_2 <- 
    frames_df_wide_2 %>%
    dplyr::select(-c("demoMetrics_epoch", "imitMetrics_epoch")) %>%
    dplyr::filter((!is.na(timeStamp_demo) | !is.na(timeStamp_imit) & !is.na(timeStamp))) %>%
    dplyr::mutate(file = rep(file_name, nrow(.)),
                  folder = rep(folder_name, nrow(.))) %>%
    dplyr::relocate(file, folder)

  cat("File completed:", file_name, "\n")
  
  frames_df_wide_2
}


# Way to run
# test_path <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/test runs 28.04.2022"
# test_file <- dir(test_path)[grep("*.botrec$", dir(test_path))][1]
# test_df <- extract_botrec(test_file, test_path)

# test_path <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Example for Sabin"
# test_file <- "TestSession_10.11.2021_19.08_ROG_LAPTOP_PLAYER_2_True_.botrec"
# test_df <- extract_botrec(test_file, test_path)

##################################################################################################################
##################################################################################################################

##################################################################################################################
# Function for running in batch
##################################################################################################################

batch_extract <- function(folder, only_clean = TRUE){     # outside loop call: do.call(dplyr::bind_rows, data_list)
  if(isTRUE(only_clean)) {
    file_names <- list.files(path = folder, pattern = "*_clean\\.botrec")
  } else {
    file_names <- list.files(path = folder, pattern = "*\\.botrec")
  }

  df_list = list()
  for(i in seq_len(length(file_names))){
    df <- extract_botrec(file = file_names[i], path = folder) 
    df_list[[i]] <- df
  }
  return(df_list)
}

# Way to run
# test_list <- batch_extract("C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/test runs 28.04.2022")
##################################################################################################################
##################################################################################################################


##################################################################################################################
# Batch processing and output saving
##################################################################################################################

botrec_clean_folder <- "/media/claudiu/Seagate Expansion Drive/Sessions_SSM_Clean/Clean_Sessions/output_file"
out_folder <- "/media/claudiu/Seagate Expansion Drive/Sessions_SSM_Clean/Clean_Sessions"
setwd(botrec_clean_folder)

file_paths <- dir(botrec_clean_folder, full.names = TRUE, recursive = FALSE, pattern = "*_clean\\.botrec")
file_names <- basename(file_paths)

sessions_clean80 <- batch_extract(botrec_clean_folder)

# saveRDS(sessions_clean80, file = file.path(out_folder, "sessions_clean80.RDS"))

########################################################3
# 2batrani

# botrec_clean_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_2batrani"
# out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_2batrani"
# setwd(botrec_clean_folder)
# 
# file_paths <- dir(botrec_clean_folder, full.names = TRUE, recursive = FALSE, pattern = "*_clean\\.botrec")
# file_names <- basename(file_paths)
# 
# sessions_clean2batrani <- batch_extract(botrec_clean_folder)

# saveRDS(sessions_clean2batrani, file = file.path(out_folder, "sessions_clean2batrani.RDS"))