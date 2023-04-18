library(tidyverse)
library(jsonlite)
library(rrapply)
library(data.table)
# library(fuzzyjoin)    # not needed any more

##################################################################################################################
# Function for data extraction
##################################################################################################################

extract_botrec <- function(file, path) {

tictoc::tic()
  # Read contents of botrec file --------------------------------------------
  
  botrec_file_name <- file.path(path, file)
  
  botrec_file_size <- file.info(botrec_file_name)$size
  
  botrec_file_con <- file(botrec_file_name, "rb")
  
  check0 <- readChar(botrec_file_con, nchars = 4)
  
  # Header JSON
  header_len <- readBin(botrec_file_con, integer(), size = 4)     # these are 4 bits as integer
  header <- readChar(botrec_file_con, nchars = header_len)
  header_list <- jsonlite::fromJSON(header, simplifyVector = FALSE)
  # file_version <- header_list[["fileVersion"]]                  # not needed
  sessioncfg_list <- header_list[["sessionConfig"]]
  # recordchunks_list <- header_list[["recordingChunks"]]         # not needed
  
  check1 <- readChar(botrec_file_con, nchars = 4)
  
  # Record JSON
  record <- readChar(botrec_file_con, nchars = botrec_file_size - 12 - header_len)
  record_list <- jsonlite::fromJSON(record, simplifyVector = FALSE)
  # schema <- record_list[["_schemaVersion"]]
  frames_list <- record_list[["frames"]]
  
  # Close and delete nuissance
  close(botrec_file_con)
  rm(header_list, record_list)
  
  
  # Unpack frames -----------------------------------------------------------

  # Inefficient way
      # frames_df <- 
      #   frames_list %>% 
      #   tibble::enframe(name = "frame", value = "value") %>% 
      #   tidyr::unnest_wider(value) %>%
      #   tidyr::unnest(inputManagerStates, keep_empty = TRUE) %>%
      #   tidyr::unnest(syncedProperties, keep_empty = TRUE) %>%
      #   tidyr::unnest_wider(syncedProperties) %>%
      #   tidyr::unnest_wider(inputManagerStates)
  
  # Still inefficient
      # frames_df_t2 <- 
      #   frames_list %>% 
      #   tibble::enframe(name = "frame", value = "value") %>% 
      #   cbind(., do.call("rbind", .$value)) %>%                             # faster alterantive to tidyr::unnest_wider(value)
      #   dplyr::select(-value) %>%
      #   dplyr::mutate(
      #     inputManagerStates = ifelse(purrr::map_lgl(inputManagerStates, purrr::is_empty),    # replacing empty lists to NA helps with speed
      #                                 NA,
      #                                 inputManagerStates
      #     )) %>%
      #   dplyr::mutate(
      #     syncedProperties = ifelse(purrr::map_lgl(syncedProperties, purrr::is_empty),
      #                               NA,
      #                               syncedProperties
      #     )) %>%
      #   tidyr::unnest(inputManagerStates, keep_empty = TRUE) %>%
      #   tidyr::unnest(syncedProperties, keep_empty = TRUE) %>%
      #   tidyr::unnest_wider(syncedProperties) %>%
      #   tidyr::unnest_wider(inputManagerStates)
  
  # New efficient way
  unnest_rrapply <- function(frames_list = frames_list) {
    
    ## bind individual data.frames
    states <- rrapply(
      frames_list, 
      condition = \(x, .xparents) "inputManagerStates" %in% .xparents, 
      how = "bind", 
      options = list(coldepth = 4, namecols = TRUE)
    )
    
    properties <- rrapply(
      frames_list, 
      condition = \(x, .xparents) "syncedProperties" %in% .xparents, 
      how = "bind", 
      options = list(coldepth = 4, namecols = TRUE)
    )
    
    times <- rrapply(
      frames_list,
      condition = \(x, .xname) .xname == "time",
      how = "bind",
      options = list(namecols = TRUE)
    )
    
    ## merge into single data.frame
    out <- merge(times, properties[, -c(2, 3)], all = TRUE, by = "L1") |>
      merge(states[, -c(2, 3)], all = TRUE, by = "L1")
    
    # rename and sort by frame
    names(out)[names(out) == "L1"] <- "frame"
    out[["frame"]] <- as.integer(out[["frame"]])
    out <- out[sort.list(out[["frame"]]), ]
    rownames(out) <- NULL
    
    return(out)
    
  }
  
  frames_df <- 
    frames_list %>%
    unnest_rrapply()
  
 
  # Cast to wide format & fill in values downward (locf)
  vars_to_fill <- c("InputY", "InputX", "xPos",   "yPos", "zPos", "xRot", "yRot", "zRot", "wRot")

  frames_df_wide <- 
    frames_df %>%
    tidyr::pivot_wider(names_from = name, values_from =  value) %>%
    dplyr::select(-c(`NA`)) %>%
    tidyr::pivot_wider(names_from = inputId, values_from =  axisValue) %>%
    dplyr::select(-c(`NA`)) %>%
    tidyr::fill(dplyr::all_of(vars_to_fill), .direction = "down") %>%
    dplyr::mutate(timeStamp = as.numeric(sub(",", ".", timeStamp))) %>%   # timeStamp is encoded as char and needs to be converted (see doc)
    dplyr::mutate(previousGameState_f = as.integer(previousGameState),    # newGameState was left character
                  newGameState_f = as.integer(newGameState)) %>%
    tidyr::fill(previousGameState_f, newGameState_f, .direction = "down") 

  

  # Add columns from sessioncfg_list (flag, objects, positions) -------------

  selectedObjectMarkers_df <- 
    sessioncfg_list %>%
    purrr::pluck("selectedObjectMarkers") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(`_transformPosition`) %>%
    dplyr::rename_with(.fn = ~ paste0("selectedObjectMarkers_", .), .cols = dplyr::everything())
  
  selectedObservationPointMarkers_df <- 
    sessioncfg_list %>%
    purrr::pluck("selectedObservationPointMarkers") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(`_transformPosition`) %>%
    dplyr::rename_with(.fn = ~ paste0("selectedObservationPointMarkers_", .), .cols = dplyr::everything())
  
  demoImitationObjectMarkerPool_vec <- 
    sessioncfg_list %>%
    purrr::pluck("demoImitationObjectMarkerPool") %>%
    unlist()
  
  demoObservationMarkerPool_vec <- 
    sessioncfg_list %>%
    purrr::pluck("demoObservationMarkerPool") %>%
    unlist()
  
  startMarkerPool_df <- 
    sessioncfg_list %>%
    purrr::pluck("startMarkerPool") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    dplyr::rename_with(.fn = ~ paste0("startMarkerPool_", .), .cols = dplyr::everything())
    
  flagMarkerPool_df <- 
    sessioncfg_list %>%
    purrr::pluck("flagMarkerPool") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    dplyr::rename_with(.fn = ~ paste0("flagMarkerPool_", .), .cols = dplyr::everything())  
    
    
                  
  
  
  
  # -------
  
  
  # Add game metrics (Demo and Imitation) -------------------------------
  
  demoMetrics_df <- 
    sessioncfg_list %>%
    purrr::pluck("demoMetrics") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(playerPosition, names_sep = "_") %>%
    tidyr::unnest_wider(markerPosition, names_sep = "_") %>%
    dplyr::rename_with(.fn = ~ paste0("demoMetrics_", .), .cols = -c("timeStamp")) %>%
    dplyr::mutate(timeStamp_demo = timeStamp)
  
  imitationMetrics_df <- 
    sessioncfg_list %>%
    purrr::pluck("imitationMetrics") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(playerPosition, names_sep = "_") %>%
    tidyr::unnest_wider(markerPosition, names_sep = "_") %>%
    dplyr::rename_with(.fn = ~ paste0("imitMetrics_", .), .cols = -c("timeStamp")) %>%
    dplyr::mutate(timeStamp_imit = timeStamp)
  
  # Merge frames with demoMetrics and imitationMetrics
  # Old way with tollerance matching - difference is variable and may lead to matching to multiple frames, not the best one
      # tictoc::tic()
      # frames_df_wide_2 <-       
      #   frames_df_wide %>%
      #   fuzzyjoin::difference_left_join(demoMetrics_df, by = "timeStamp", max_dist = 10^-2) %>%    
      #   dplyr::rename(timeStamp = timeStamp.x, timeStamp_demo = timeStamp.y) %>%
      #   dplyr::mutate(timeStamp_demo_diff = abs(timeStamp - timeStamp_demo)) %>%
      #   dplyr::group_by(frame) %>%
      #   dplyr::filter(timeStamp_demo_diff == min(timeStamp_demo_diff) | is.na(timeStamp_demo_diff)) %>%
      #   dplyr::ungroup() %>%
      #   fuzzyjoin::difference_left_join(imitationMetrics_df, by = "timeStamp", max_dist = 10^-2) %>%    
      #   dplyr::rename(timeStamp = timeStamp.x, timeStamp_imit = timeStamp.y) %>%
      #   dplyr::mutate(timeStamp_imit_diff = abs(timeStamp - timeStamp_imit)) %>%
      #   dplyr::group_by(frame) %>%
      #   dplyr::filter(timeStamp_imit_diff == min(timeStamp_imit_diff) | is.na(timeStamp_imit_diff)) %>%
      #   dplyr::ungroup()
      # tictoc::toc()

  # New way with rolling nearest match left join
  demoMetrics_df <- data.table::setDT(demoMetrics_df)
  imitationMetrics_df <- data.table::setDT(imitationMetrics_df)
  frames_df_wide <- data.table::setDT(frames_df_wide)  
  
      # demo_cols_join <- names(demoMetrics_df)[names(demoMetrics_df) != "timeStamp"]
      # demo_cols_join_expr <- paste0(demo_cols_join, " = ", paste0("i.", demo_cols_join), collapse = ", ")
      # imit_cols_join <- names(imitationMetrics_df)[names(imitationMetrics_df) != "timeStamp"]
      # imit_cols_join_expr <- paste0(imit_cols_join, " = ", paste0("i.", imit_cols_join), collapse = ", ")


  # demoMetrics left join: leading table on the left
  frames_df_wide[
    demoMetrics_df, roll = "nearest", on = "timeStamp", 
    # assign desired values explicitly -- could also evaluate demo_cols_join_expr inside `:=`()
    `:=`(demoMetrics_epoch = i.demoMetrics_epoch, 
         demoMetrics_playerType = i.demoMetrics_playerType, 
         demoMetrics_markerType = i.demoMetrics_markerType, 
         demoMetrics_playerPosition_x = i.demoMetrics_playerPosition_x, 
         demoMetrics_playerPosition_y = i.demoMetrics_playerPosition_y, 
         demoMetrics_playerPosition_z = i.demoMetrics_playerPosition_z, 
         demoMetrics_markerPosition_x = i.demoMetrics_markerPosition_x, 
         demoMetrics_markerPosition_y = i.demoMetrics_markerPosition_y, 
         demoMetrics_markerPosition_z = i.demoMetrics_markerPosition_z, 
         demoMetrics_distanceFromMarker = i.demoMetrics_distanceFromMarker, 
         demoMetrics_score = i.demoMetrics_score, 
         timeStamp_demo = i.timeStamp_demo
    )]

  frames_df_wide[
    imitationMetrics_df, roll = "nearest", on = "timeStamp", 
    # assign desired values explicitly -- could also evaluate imit_cols_join_expr inside `:=`()
    `:=`(imitMetrics_epoch = i.imitMetrics_epoch, 
         imitMetrics_playerType = i.imitMetrics_playerType, 
         imitMetrics_markerType = i.imitMetrics_markerType, 
         imitMetrics_playerPosition_x = i.imitMetrics_playerPosition_x, 
         imitMetrics_playerPosition_y = i.imitMetrics_playerPosition_y, 
         imitMetrics_playerPosition_z = i.imitMetrics_playerPosition_z, 
         imitMetrics_markerPosition_x = i.imitMetrics_markerPosition_x, 
         imitMetrics_markerPosition_y = i.imitMetrics_markerPosition_y, 
         imitMetrics_markerPosition_z = i.imitMetrics_markerPosition_z, 
         imitMetrics_distanceFromMarker = i.imitMetrics_distanceFromMarker, 
         imitMetrics_score = i.imitMetrics_score, 
         timeStamp_imit = i.timeStamp_imit
    )]
  
  frames_df_wide <- as.data.frame(frames_df_wide)
  
  # Checks on merge
  cat("Merged ", sum(!is.na(frames_df_wide$timeStamp_demo)), " demoMetrics, out of a total of ", sum(!is.na(demoMetrics_df$timeStamp_demo)), ".\n", sep = "")  
  cat("Merged ", sum(!is.na(frames_df_wide$timeStamp_imit)), " imitationMetrics, out of a total of ", sum(!is.na(imitationMetrics_df$timeStamp_imit)), ".\n", sep = "")  
  
  
  tictoc::toc()
  
  
  
  
  tictoc::tic()
  # Clean
  file_name <- basename(botrec_file_name)
  folder_name <- sub(pattern = ".*\\/", replacement = "", x = dirname(botrec_file_name))

  full_data_output_clean <- 
    frames_df_wide %>%
    dplyr::mutate(file = rep(file_name, nrow(.)),
                  folder = rep(folder_name, nrow(.))) %>%
    dplyr::mutate(State_demoimit = 
                  dplyr::case_when(newGameState_f %in% 5:11 ~ "Demo",
                                   newGameState_f %in% 12:20 ~ "Imit",
                                   TRUE ~ NA_character_)) %>%
    # coalesce demoMetrics and imitationMetrics values within same columns
    dplyr::mutate(timeStamp_demoimit = dplyr::coalesce(timeStamp_demo, timeStamp_imit),
                  playerPosition_x_demoimit = dplyr::coalesce(demoMetrics_playerPosition_x, imitMetrics_playerPosition_x),
                  playerPosition_y_demoimit = dplyr::coalesce(demoMetrics_playerPosition_y, imitMetrics_playerPosition_y),
                  playerPosition_z_demoimit = dplyr::coalesce(demoMetrics_playerPosition_z, imitMetrics_playerPosition_z),
                  markerPosition_x_demoimit = dplyr::coalesce(demoMetrics_markerPosition_x, imitMetrics_markerPosition_x),
                  markerPosition_y_demoimit = dplyr::coalesce(demoMetrics_markerPosition_y, imitMetrics_markerPosition_y),
                  markerPosition_z_demoimit = dplyr::coalesce(demoMetrics_markerPosition_z, imitMetrics_markerPosition_z),
                  distanceFromMarker_demoimit = dplyr::coalesce(demoMetrics_distanceFromMarker, imitMetrics_distanceFromMarker), 
                  playerType_demoimit = dplyr::coalesce(demoMetrics_playerType, imitMetrics_playerType),
                  markerType_demoimit = dplyr::coalesce(demoMetrics_markerType, imitMetrics_markerType),
                  score_demoimit = dplyr::coalesce(demoMetrics_score, imitMetrics_score)) %>%
    dplyr::mutate(who = dplyr::case_when(stringr::str_detect(file_name, "PLAYER_1") ~ 1L,
                                         stringr::str_detect(file_name, "PLAYER_2") ~ 2L,
                                         TRUE ~ NA_integer_)) %>%
    # "newGameState_seg" codes with incremental values for every change in newGameState; newGameState was not filled in (locf) so it codes for gameState change 
    # similar to (but if 2 adjacent frames have same gameState result would be different): mutate(newGameState_seg = data.table::rleid(newGameState_f))
    dplyr::mutate(                                # old code, alternative to reild() solution below
      newGameState_seg = cumsum(
      (!is.na(newGameState)) & (newGameState != dplyr::lag(ifelse(is.na(newGameState), "0", newGameState), default = "0"))
    )) %>%
    # "newGameState_cycl", "newGameState_cycln" code for number of each particular newGameState type (counter by group)
    dplyr::group_by(newGameState_f) %>%
    dplyr::mutate(
      newGameState_cycln = data.table::rleid(newGameState_seg),
      newGameState_cycl = paste0(newGameState_f, "_", newGameState_cycln)
    ) %>%
    dplyr::ungroup() %>%

  
  

  

  

    # exclude redundant columns
    dplyr::select(
      -dplyr::all_of(c(
      # Demo
      "demoMetrics_epoch",
      "demoMetrics_playerPosition_x", "demoMetrics_playerPosition_y", "demoMetrics_playerPosition_z", 
      "demoMetrics_markerPosition_x", "demoMetrics_markerPosition_y", "demoMetrics_markerPosition_z", 
      "demoMetrics_distanceFromMarker", "demoMetrics_playerType", "demoMetrics_markerType", 
      "timeStamp_demo", "demoMetrics_score",  
      # Imit
      "imitMetrics_epoch", 
      "imitMetrics_playerPosition_x", "imitMetrics_playerPosition_y", "imitMetrics_playerPosition_z", 
      "imitMetrics_markerPosition_x", "imitMetrics_markerPosition_y", "imitMetrics_markerPosition_z", 
      "imitMetrics_distanceFromMarker", "imitMetrics_playerType", "imitMetrics_markerType", 
      "timeStamp_imit", "imitMetrics_score"
    ))) %>%
    #dplyr::relocate(syncKey, .after = dplyr::last_col()) %>%
    dplyr::relocate(
      dplyr::all_of(c(
        "file",  "folder", "frame", "time", "buttonState", 
        "previousGameState", "previousGameState_f", "newGameState", "newGameState_f", 
        "newGameState_seg", "newGameState_cycln", "newGameState_cycl",
        "InputX", "InputY",
        "xPos", "yPos", "zPos", 
        "markerX", "markerY", "markerZ",
        "xRot", "yRot", "zRot", "wRot"
    ))) %>%
    # demoimit matching on timeStamp sometimes matches the correct frame, some times not => fill values inside the newGameState_f group
    # only timeStamp_demoimit not filled so can easy track frame of nearest match
    dplyr::group_by(newGameState_cycln) %>%
    tidyr::fill(
      dplyr::all_of(c(
        "playerPosition_x_demoimit", "playerPosition_y_demoimit", "playerPosition_z_demoimit", 
        "markerPosition_x_demoimit", "markerPosition_y_demoimit", "markerPosition_z_demoimit", 
        "distanceFromMarker_demoimit", "playerType_demoimit", 
        "markerType_demoimit", "score_demoimit"
      )), 
      .direction = "updown"
    ) %>%
    dplyr::ungroup()
    
  tictoc::toc()  
  

# Validation checks -------------------------------------------------------
  # DONT RUN
  test_timeStamp <- dplyr::filter(full_data_output_clean, !is.na(timeStamp_demoimit))
  all.equal(test_timeStamp[, "timeStamp"], test[, "timeStamp_demoimit"], tolerance = 10^2)
  
  # even if timeStamps match well, it can match the frame before or the frame after => demoimitMetrics may not match in that frame
  # here we find best match based on xPos & zPos value
  test_demoimit1 <- data.table::setDT(
    full_data_output_clean [, c(
      "newGameState_f",
      "timeStamp", 
      "xPos", "yPos", "zPos",
      "markerX", "markerY", "markerZ"
  )])[, `:=`(timeStamp_frame = timeStamp, X = xPos, Z = zPos)]
  test_demoimit2 <- data.table::setDT(
    full_data_output_clean [, c(
      "newGameState_f",
      "timeStamp_demoimit", 
      "playerPosition_x_demoimit", "playerPosition_y_demoimit", "playerPosition_z_demoimit", 
      "markerPosition_x_demoimit", "markerPosition_y_demoimit", "markerPosition_z_demoimit"
  )])[, `:=`(timeStamp = timeStamp_demoimit, X = playerPosition_x_demoimit, Z = playerPosition_z_demoimit)]
  
  test_demoimit <-  
    test_demoimit1[
      test_demoimit2, roll = "nearest", on = .(timeStamp, X, Z),
      `:=`(timeStamp_demoimit = i.timeStamp_demoimit,
           playerPosition_x_demoimit = i.playerPosition_x_demoimit,
           playerPosition_y_demoimit = i.playerPosition_y_demoimit,
           playerPosition_z_demoimit = i.playerPosition_z_demoimit,
           markerPosition_x_demoimit = i.markerPosition_x_demoimit,
           markerPosition_y_demoimit = i.markerPosition_y_demoimit,
           markerPosition_z_demoimit = i.markerPosition_z_demoimit)]
                
  test_demoimit <- 
    test_demoimit %>%
    dplyr::filter(!is.na(playerPosition_x_demoimit) | !is.na(playerPosition_z_demoimit))
  # must include test for "playerType" and "markerTyp" after sessioncfg data imported
# -------------------------------------------------------------------------

      
        
   

  # Finish
  cat("File completed:", file_name, ".\n")
  
  frames_df_wide
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

