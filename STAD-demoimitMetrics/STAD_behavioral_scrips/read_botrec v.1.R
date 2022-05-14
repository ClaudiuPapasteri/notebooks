### STAD
library(jsonlite)
library(tidyverse)

setwd("C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Comportament STAD")

# Reading ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
botrec_file_name <- dir()[grep("*.botrec$", dir())]

botrec_file_size <- file.info(botrec_file_name)$size

botrec_file_con <- file(botrec_file_name, "rb")

check0 <- readChar(botrec_file_con, nchars = 4)

# Header JSON
header_len <- readBin(botrec_file_con, integer(), size = 4)     # these are 4 bits as integer
header <- readChar(botrec_file_con, nchars = header_len)
header_list <- jsonlite::fromJSON(header, simplifyVector = FALSE)
file_version <- header_list[["fileVersion"]]
sessioncfg_list <- header_list[["sessionConfig"]]
recordchunks_list <- header_list[["recordingChunks"]]

check1 <- readChar(botrec_file_con, nchars = 4)

# Record JSON
record <- readChar(botrec_file_con, nchars = botrec_file_size - 12 - header_len)
record_list <- jsonlite::fromJSON(record, simplifyVector = FALSE)
schema <- record_list[["_schemaVersion"]]
frames_list <- record_list[["frames"]]




close(botrec_file_con)

rm(header_list, record_list)


# Edits ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# RecordingChunks (needed only for bot, not for analyses)
# recordchunks_df <- do.call(rbind.data.frame, recordchunks_list)    # no need for this

# Configuration
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
  
  
# Recording (game frames)
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
  dplyr::mutate(timeStamp = as.numeric(sub(",", ".", timeStamp)))    # timeStamp is encoded as char and needs to be converted (see doc)
  

# Merge with demoMetrics and imitationMetrics
frames_df_wide_2 <- 
  frames_df_wide %>%
  fuzzyjoin::difference_left_join(demoMetrics_df, by = "timeStamp", max_dist = 10^-5) %>%
  fuzzyjoin::difference_left_join(imitationMetrics_df, by = "timeStamp", max_dist = 10^-5)
  










































frames_list[599]
frames_list[600]
frames_list[601]


test_list <- frames_list[c(599:601, 1211)]


library(tidyverse)
bla <- 
test_list %>% 
  tibble::enframe(name = "epoch", value = "value") %>% 
  tidyr::unnest_wider(value) %>%
  tidyr::unnest(inputManagerStates, keep_empty = TRUE) %>%
  tidyr::unnest(syncedProperties, keep_empty = TRUE) %>%
  tidyr::unnest_wider(syncedProperties) %>%
  tidyr::unnest_wider(inputManagerStates)
  

  
  
 

