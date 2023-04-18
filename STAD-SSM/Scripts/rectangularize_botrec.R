# Rectangularize botrec files

# Prerequisites -----------------------------------------------------------

##################################################################################################################
# Packages
##################################################################################################################

# Package names
packages <- c("data.table", "rrapply", "jsonlite",  "tidyverse")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load
invisible(lapply(packages, library, character.only = TRUE))

##################################################################################################################
# Unchanging definitions
##################################################################################################################

marker_def_vef <- c(     # meant to be used with dplyr::recode() 
  "0"  = "Gamepad", 
  "1"  = "RubicCube", 
  "2"  = "Sandwich", 
  "3"  = "BowlingPin", 
  "4"  = "Heart", 
  "5"  = "Bananas", 
  "6"  = "Extinguisher", 
  "7"  = "Laptop", 
  "8"  = "Arcade", 
  "9"  = "Pen", 
  "10" = "Trophy", 
  "11" = "Book", 
  "12" = "Lock", 
  "13" = "Diamond", 
  "14" = "Apple", 
  "15" = "Key", 
  "16" = "Scissors", 
  "17" = "Milk", 
  "18" = "Ventilator", 
  "19" = "Coin", 
  "20" = "Start", 
  "21" = "Flag"
)

# GameStates used for merging sessioncfg with frames
matchGameStates_list <- list(
  # Initial
  intial_start  = 1L,             # Start  - initial_StartMarker_df
  intial_flag   = 2L,             # Flag   - initial_FlagMarker_df
  intial_object = 3L,             # Object - initial_ObjectMarker_df
  
  # Demo
  demo_observe  = (6L:10L),       # Observation - demo_ObservationMarker_df
  demo_start    = 7L,             # Start  - demo_StartMarker_df
  demo_flag     = 8L,             # Flag   - demo_FlagMarker_df
  demo_object   = c(9L:11L),      # Object - demo_ObjectMarker_df
  
  # Imit
  imit_observe  = c(13L:19L),     # Observation - imit_ObservationMarker_df
  imit_start    = 14L,            # Start  - imit_StartMarker_df
  imit_flag     = 15L,            # Flag   - imit_FlagMarker_df
  imit_object    = c(16L:20L)     # Object - imit_ObjectMarker_df
)

# only first GameState and will fill down (locf) the rest? - NO, DON'T USE 
# (newGameState_cycln should be the same for adjacent GameStates because they are in the same cycle)
# matchGameStates_list <- lapply(matchGameStates_list, "[[", 1)

##################################################################################################################
# Helper functions
##################################################################################################################
# Efficient function for unpacking frames
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

# Helper function that recycles numeric vector bounded by a upper limit 
# used to generate index from nrow(df), cycling through another data frame (so vec always starts with 1 cause it's an index)
bounded_vec_recyc <- function(vec, n) {
  if(n == 1) {
    rep(1, length.out = length(vec)) 
  } else {
    (vec - 1) %% n + 1
  }
}
# use: bounded_vec_recyc(c(1, 1, 2, 2, 3, 3, 4, 5, 5, 6, 7, 8, 9, 9), n = 5)

# Helper function that recycles vector after value corresponding to maximum index
# NOT USED HERE
# maxidx_vec_recyc <- function(vec, n) {
#   vec[(seq_along(vec) - 1) %% n + 1]
# }

# Function that merges columns from processed sessioncfg (rectangularized "sessionConfig") with frames_df (rectangularized "frames")
# frames_df must include column `newGameState_cycln` that is data.table::rleid(newGameState_seg), `newGameState_seg` and newGameState_f
# also, columns that should be joined need to be already declared in frames_df and empty (NA)
# note that newGameState_cycln should be the same for adjacent GameStates (e.g. a cycle of 15-16-17-18-19)
merge_cfg_cols <- function(frames_df = frames_df, def_df, matchGameStates) {
  idx <- frames_df[frames_df[, "newGameState_f"] == matchGameStates, ][["newGameState_cycln"]]
  # for some gameStates (e.g. 1 - Start in Initial - there are less rows in definition dfs than occurences in game, so needs to recycle)
  idx <- bounded_vec_recyc(idx, n = nrow(def_df))
  # merge mutate
  cols <- colnames(def_df)
  frames_df[frames_df[, "newGameState_f"] == matchGameStates, ][, cols] <- def_df[idx, ]
  
  frames_df
}
# way to run: dat <- merge_cfg_cols(frames_df = data, def_one, matchGameStates = 1)


##################################################################################################################
# Function for data extraction
##################################################################################################################

rectang_botrec <- function(file, path) {

  # Read contents of botrec file --------------------------------------------

  botrec_file_path <- file.path(path, file)
  
  botrec_file_size <- file.info(botrec_file_path)$size
  
  botrec_file_con <- file(botrec_file_path, "rb")
  
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
  
  # Store file names
  file_name <- basename(botrec_file_path)
  folder_name <- sub(pattern = ".*\\/", replacement = "", x = dirname(botrec_file_path))
  
  #--------------------------------------------------------------------------
  # Unpack frames -----------------------------------------------------------
  #--------------------------------------------------------------------------
  
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
                  newGameState_f = as.integer(newGameState),
                  timeStamp_f = timeStamp) %>%
    tidyr::fill(previousGameState_f, newGameState_f, timeStamp_f, .direction = "down") 

  
  #--------------------------------------------------------------------------
  # Add columns from sessioncfg_list (flag, objects, positions) -------------
  #--------------------------------------------------------------------------
  
  # Unpack sessioncfg_list

  # -----------------------
  
  # selectedObjectMarkers is for Initial phase - Start/Flag/Object definition and order
  selectedObjectMarkers_df <-      # ObjectMarkers_subType and their corresponding positions (x,y,z) are CONSTANT
    sessioncfg_list %>%
    purrr::pluck("selectedObjectMarkers") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(`_transformPosition`) %>%
    dplyr::select(-1) 
  
  ObjectMarker_df <-             # this and initial_ObjectMarker_df are the same; kept initial_ObjectMarker_df to clarify it codes order in Initial
    selectedObjectMarkers_df %>%
    dplyr::filter(!subType %in% c(20L, 21L)) %>%
    dplyr::rename_with(.fn = ~ paste0("ObjectMarker_", .), .cols = dplyr::everything()) %>%
    dplyr::rename(ObjectMarker = ObjectMarker_subType) 
  
  initial_ObjectMarker_df <- ObjectMarker_df      # initial_ObjectMarker_df is definition and order of Object in Initial
    
  initial_StartMarker_df <-        # initial_StartMarker_df is definition and order of Start in Initial
    selectedObjectMarkers_df %>%
    dplyr::filter(subType == 20L) %>%
    dplyr::rename_with(.fn = ~ paste0("StartMarker_", .), .cols = dplyr::everything()) %>%
    dplyr::rename(StartMarker = StartMarker_subType) 
      
  
  initial_FlagMarker_df <-         # initial_FlagMarker_df is definition and order of Flag in Initial
    selectedObjectMarkers_df %>%
    dplyr::filter(subType == 21L) %>%
    dplyr::rename_with(.fn = ~ paste0("FlagMarker_", .), .cols = dplyr::everything()) %>%
    dplyr::rename(FlagMarker = FlagMarker_subType)  
  
  rm(selectedObjectMarkers_df)
  
  # -----------------------
  
  # selectedObservationPointMarkers = Observation Types definition and order
  selectedObservationPointMarkers_df <-    # ObservationPointMarkers_subType and their corresponding positions (x,y,z) are CONSTANT
    sessioncfg_list %>%
    purrr::pluck("selectedObservationPointMarkers") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    tidyr::unnest_wider(`_transformPosition`) %>%
    dplyr::select(-1) %>% 
    dplyr::rename_with(.fn = ~ paste0("ObservationMarker_", .), .cols = dplyr::everything()) %>%
    dplyr::rename(ObservationMarker = ObservationMarker_subType)  
  
  # demoObservationMarkerPool = Observation Type order 
  demoimit_ObservationMarker_vec <-       
    sessioncfg_list %>%
    purrr::pluck("demoObservationMarkerPool") %>%
    unlist()
  
  # Bring order and definition together in data frame  
  # ObservationPointMarker codes order differently - first is Demo, then Imit/Demo-Imit/Demo, last is only Imit
  demoimit_ObservationPointMarker_df <-
    demoimit_ObservationMarker_vec %>%
    dplyr::tibble("ObservationMarker" = .) %>%
    # match with definition
    dplyr::left_join(selectedObservationPointMarkers_df, by = "ObservationMarker") 
  
  # for Demo exclude the last row (Demo - Imit/Demo - Imit/Demo - ... - Imit/Demo - Imit)
  demo_ObservationMarker_df <- demoimit_ObservationPointMarker_df[-nrow(demoimit_ObservationPointMarker_df),]  
  
  # for Imit exclude the first row (Demo - Imit/Demo - Imit/Demo - ... - Imit/Demo - Imit)
  imit_ObservationMarker_df <- demoimit_ObservationPointMarker_df[-1,]           
    
  rm(selectedObservationPointMarkers_df, demoimit_ObservationMarker_vec, demoimit_ObservationPointMarker_df)
  
  # -----------------------
  
  # demoImitationObjectMarkerPool = Object Types order
  # in docs: even for Demo, odd for Imit - but here is revirsed because Python index start at 0
  demoImitationObjectMarkerPool_vec <-    # this codes distinct values for Demo and Imit in order Demo-Imit-Demo-Imit
    sessioncfg_list %>%
    purrr::pluck("demoImitationObjectMarkerPool") %>%
    unlist()
  
  # Bring order and definition together in data frame
  demo_ObjectMarker_df <-          # demo_ObjectMarker_df is definition and order for Object in Demo 
    # keep only odd for Demo
    demoImitationObjectMarkerPool_vec[seq(1, length(demoImitationObjectMarkerPool_vec), by = 2)] %>%   
    dplyr::tibble("ObjectMarker" = .) %>%
    # match with definition
    dplyr::left_join(ObjectMarker_df, by = "ObjectMarker") 
  
  imit_ObjectMarker_df <-          # imit_ObjectMarker_df is definition and order for Object in Imit
    # keep only even for Imit
    demoImitationObjectMarkerPool_vec[seq(2, length(demoImitationObjectMarkerPool_vec), by = 2)] %>%   
    dplyr::tibble("ObjectMarker" = .) %>%
    # match with definition
    dplyr::left_join(ObjectMarker_df, by = "ObjectMarker") 

  rm(demoImitationObjectMarkerPool_vec)
  
  # -----------------------
  
  # startMarkerPool = Start definition and order
  demoimit_startMarker_df <-           # this codes same values for Demo and Imit in order Demo-Imit-Demo-Imit
    sessioncfg_list %>%           # so we will keep only odd rows in order
    purrr::pluck("startMarkerPool") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    dplyr::select(-1) %>%
    dplyr::rename_with(.fn = ~ paste0("StartMarker_", .), .cols = dplyr::everything()) %>%
    # every second row is duplicated: startMarkerPool_df[, -1] %>% duplicated()
    dplyr::slice(seq(1, nrow(.), by = 2)) %>%          # keep only odd rows in order
    dplyr::mutate(StartMarker = rep(20L, nrow(.))) %>%
    dplyr::relocate(StartMarker) 
  
  demo_StartMarker_df <- demoimit_startMarker_df[seq(1, nrow(demoimit_startMarker_df), by = 2), ] # keep only odd
  
  imit_StartMarker_df <- demoimit_startMarker_df[seq(2, nrow(demoimit_startMarker_df), by = 2), ] # keep only even  
    
  # flagMarkerPool = Flag definition and order  
  demoimit_flagMarker_df <-           # this codes same values for Demo and Imit in order Demo-Imit-Demo-Imit
    sessioncfg_list %>%          # so we will keep only odd rows in order
    purrr::pluck("flagMarkerPool") %>%
    tibble::enframe(name = "epoch", value = "value") %>%
    tidyr::unnest_wider(value) %>%
    dplyr::select(-1) %>%
    dplyr::rename_with(.fn = ~ paste0("FlagMarker_", .), .cols = dplyr::everything()) %>%
    # every second row is duplicated: flagMarkerPool_df[, -1] %>% duplicated()
    dplyr::slice(seq(1, nrow(.), by = 2)) %>%          # keep only odd rows in order
    dplyr::mutate(FlagMarker = rep(21L, nrow(.))) %>%
    dplyr::relocate(FlagMarker) 

  demo_FlagMarker_df <- demoimit_flagMarker_df[seq(1, nrow(demoimit_flagMarker_df), by = 2), ] # keep only odd
  
  imit_FlagMarker_df <- demoimit_flagMarker_df[seq(2, nrow(demoimit_flagMarker_df), by = 2), ] # keep only even 
  
  rm(demoimit_startMarker_df, demoimit_flagMarker_df)
  
  
  # -----------------------

  # Merge with data by cycling
  # add helper columns for iteration and other info
  frames_df_wide <- 
    frames_df_wide %>%
    dplyr::mutate(file = rep(file_name, nrow(.)),
                  folder = rep(folder_name, nrow(.))) %>%
    dplyr::mutate(State_demoimit = 
                    dplyr::case_when(newGameState_f %in% 5:11 ~ "Demo",
                                     newGameState_f %in% 12:20 ~ "Imit",
                                     TRUE ~ NA_character_)) %>%
    # "newGameState_seg" codes with incremental values for every change in newGameState; newGameState was not filled in (locf) so it codes for gameState change 
    dplyr::mutate(                      # this is like data.table::rleid() but ignores NAs which is important here                  
      newGameState_seg = cumsum(
        (!is.na(newGameState)) & (newGameState != dplyr::lag(ifelse(is.na(newGameState), "0", newGameState), default = "0"))
      )) %>%
    # "newGameState_cycl", "newGameState_cycln" code for number of each particular newGameState type (counter by group)
    dplyr::group_by(newGameState_f) %>%
    dplyr::mutate(
      newGameState_cycln = data.table::rleid(newGameState_seg),
      newGameState_cycl = paste0(newGameState_f, "_", newGameState_cycln)
    ) %>%
    dplyr::ungroup()                 
  
  # add empty columns with names of vars from processed sessioncfg_list definition & order data frames
  config_var_names <- c(
    "StartMarker", "StartMarker_x", "StartMarker_y", "StartMarker_z",                           # colnames(demo_StartMarker_df)
    "FlagMarker", "FlagMarker_x", "FlagMarker_y", "FlagMarker_z",                               # colnames(demo_FlagMarker_df)
    "ObservationMarker", "ObservationMarker_x", "ObservationMarker_y", "ObservationMarker_z",   # colnames(demo_ObservationMarker_df)
    "ObjectMarker", "ObjectMarker_x", "ObjectMarker_y", "ObjectMarker_z"                        # colnames(demo_ObjectMarker_df)
  )  
  
  frames_df_wide[, config_var_names[seq(1, 16, by = 4)]]  <- NA_integer_
  frames_df_wide[, config_var_names[-seq(1, 16, by = 4)]] <- NA_real_
  
  # Merge
  frames_df_wide <- 
    frames_df_wide %>%
    # Initial
    merge_cfg_cols(def_df = initial_StartMarker_df, matchGameStates = matchGameStates_list$intial_start) %>%
    merge_cfg_cols(def_df = initial_FlagMarker_df, matchGameStates = matchGameStates_list$intial_flag) %>%
    merge_cfg_cols(def_df = initial_ObjectMarker_df, matchGameStates = matchGameStates_list$intial_object) %>%
    # Demo
    # Observe
    merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[1]]) %>%
    merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[2]]) %>%
    merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[3]]) %>%
    merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[4]]) %>%
    merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[5]]) %>%
    # Start & Flag
    merge_cfg_cols(def_df = demo_StartMarker_df, matchGameStates = matchGameStates_list$demo_start) %>%
    merge_cfg_cols(def_df = demo_FlagMarker_df, matchGameStates = matchGameStates_list$demo_flag) %>%
    # Object
    merge_cfg_cols(def_df = demo_ObjectMarker_df, matchGameStates = matchGameStates_list$demo_object[[1]]) %>%
    merge_cfg_cols(def_df = demo_ObjectMarker_df, matchGameStates = matchGameStates_list$demo_object[[2]]) %>%
    merge_cfg_cols(def_df = demo_ObjectMarker_df, matchGameStates = matchGameStates_list$demo_object[[3]]) %>%
    # Imit
    # Observe
    merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[1]]) %>%
    merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[2]]) %>%
    merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[3]]) %>%
    merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[4]]) %>%
    merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[5]]) %>%
    merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[6]]) %>%
    merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[7]]) %>%
    # Start & Flag
    merge_cfg_cols(def_df = imit_StartMarker_df, matchGameStates = matchGameStates_list$imit_start) %>%
    merge_cfg_cols(def_df = imit_FlagMarker_df, matchGameStates = matchGameStates_list$imit_flag) %>%
    # Object
    merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[1]]) %>% 
    merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[2]]) %>% 
    merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[3]]) %>% 
    merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[4]]) %>% 
    merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[5]]) %>%
    # Recode Start, Flag & Object from integer code to word
    dplyr::mutate(ObjectMarker = dplyr::recode(ObjectMarker, !!!marker_def_vef),
                  StartMarker = dplyr::recode(StartMarker, !!!marker_def_vef),
                  FlagMarker = dplyr::recode(FlagMarker, !!!marker_def_vef)) %>%
    # Coalesce marker types and their positions
    dplyr::mutate(Marker_type = dplyr::coalesce(StartMarker, FlagMarker, ObjectMarker),
                  Marker_x = dplyr::coalesce(StartMarker_x, FlagMarker_x, ObjectMarker_x),
                  Marker_y = dplyr::coalesce(StartMarker_y, FlagMarker_y, ObjectMarker_y),
                  Marker_z = dplyr::coalesce(StartMarker_z, FlagMarker_z, ObjectMarker_z))
  
  
  #--------------------------------------------------------------------------
  # Add game metrics (Demo and Imitation) -------------------------------
  #--------------------------------------------------------------------------
  
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
  
  # Just check matching frames - DON'T USE
  # demo_match_frames <-
  #   demoMetrics_df[
  #     frames_df_wide, roll = "nearest", on = "timeStamp",
  #     `:=`(frame = i.frame,
  #          timeStamp_frame = i.timeStamp
  #   )]

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
  
  
  # Checks on merge
  cat("Merged ", sum(!is.na(frames_df_wide$timeStamp_demo)), " demoMetrics, out of a total of ", sum(!is.na(demoMetrics_df$timeStamp_demo)), ".\n", sep = "")  
  cat("Merged ", sum(!is.na(frames_df_wide$timeStamp_imit)), " imitationMetrics, out of a total of ", sum(!is.na(imitationMetrics_df$timeStamp_imit)), ".\n", sep = "")  
  


  
  # Clean and some transforms
  frames_df_wide <- 
    frames_df_wide %>%
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
        "timeStamp", "timeStamp_f",
        "InputX", "InputY",
        "xPos", "yPos", "zPos", 
       # "markerX", "markerY", "markerZ",              # some files don't have `markerX` `markerY`, `markerZ`
        "xRot", "yRot", "zRot", "wRot"
    ))) # %>%
    # demoimit matching on timeStamp sometimes matches the correct frame, some times not => fill values inside the newGameState_f group
    # only timeStamp_demoimit not filled so can easy track frame of nearest match
    # dplyr::group_by(newGameState_cycl) %>%      # DON'T DO THIS HERE, IF NEEDED TO IT AFTERWARDS
    # tidyr::fill(
    #   dplyr::all_of(c(
    #     "playerPosition_x_demoimit", "playerPosition_y_demoimit", "playerPosition_z_demoimit", 
    #     "markerPosition_x_demoimit", "markerPosition_y_demoimit", "markerPosition_z_demoimit", 
    #     "distanceFromMarker_demoimit", "playerType_demoimit", 
    #     "markerType_demoimit", "score_demoimit"
    #   )), 
    #   .direction = "updown"
    # ) %>%
    # dplyr::ungroup()
    


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
# Function for batch extract - DON'T USE
##################################################################################################################

batch_extract <- function(folder, only_clean = TRUE){     # outside loop call: do.call(dplyr::bind_rows, data_list)
  if(isTRUE(only_clean)) {
    file_names <- list.files(path = folder, pattern = "*_clean\\.botrec")
  } else {
    file_names <- list.files(path = folder, pattern = "*\\.botrec")
  }

  df_list = list()
  for(i in seq_len(length(file_names))){
    df <- rectang_botrec(file = file_names[i], path = folder)
    df_list[[i]] <- df
  }
  return(df_list)
}

# Way to run
# test_list <- batch_extract("C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/test runs 28.04.2022")
##################################################################################################################
##################################################################################################################

##################################################################################################################
# Batch processing and output saving - DON'T USE
##################################################################################################################

# botrec_clean_folder <- "/media/claudiu/Seagate Expansion Drive/Sessions_SSM_Clean/Clean_Sessions/output_file"
# out_folder <- "/media/claudiu/Seagate Expansion Drive/Sessions_SSM_Clean/Clean_Sessions"
# setwd(botrec_clean_folder)
# 
# file_paths <- dir(botrec_clean_folder, full.names = TRUE, recursive = FALSE, pattern = "*_clean\\.botrec")
# file_names <- basename(file_paths)
# 
# sessions_clean80 <- batch_extract(botrec_clean_folder)
# 
# # saveRDS(sessions_clean80, file = file.path(out_folder, "sessions_clean80.RDS"))

##################################################################################################################


##################################################################################################################
# Function for batch extract and export as individual RDS
##################################################################################################################
batch_rds_export <- function(input_folder, output_folder, output = c("both", "short", "full"), only_clean = TRUE) {     
  if(isTRUE(only_clean)) {
    file_names <- list.files(path = input_folder, pattern = "*_clean\\.botrec")
  } else {
    file_names <- list.files(path = input_folder, pattern = "*\\.botrec")
  }
  
  output <- match.arg(output)

  for(i in seq_len(length(file_names))){
    cat("File number:", i, ": ", file_names[i], ".\n")
    file_name <- gsub(".botrec", "", file_names[i])
    
    df <- rectang_botrec(file = file_names[i], path = input_folder)
    
    if(output %in% c("full", "both")) {
      rds_name <- paste0(file_name, ".RDS")
      rds_filepath <- file.path(output_folder, rds_name)
      saveRDS(df, rds_filepath)
    }
    
    if(output %in% c("short", "both")) {
      rds_name_sh <- paste0(file_name, "_Short",".RDS")
      rds_filepath_sh <- file.path(output_folder, rds_name_sh)
      df_short <-
        df %>%
        dplyr::group_by(newGameState_cycl) %>%
        dplyr::filter(dplyr::row_number() == 1 |                              # first frame of gameState
                      return == "True" |                                      # any frame the participant hit Enter
                      !is.na(timeStamp_demoimit) |  
                      dplyr::row_number() == ceiling(dplyr::n() / 2) |        # middle frame of gameState
                      dplyr::row_number() == (dplyr::n() - 1) |               # frame before the last 
                      dplyr::row_number() == dplyr::n()                       # last frame of gameState
        ) %>%          
        dplyr::ungroup()
      saveRDS(df_short, rds_filepath_sh)  
    }
  }
}

# Way to run
# inp_out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/test_new_script"
# batch_rds_export(inp_out_folder, inp_out_folder, output = "both", only_clean = TRUE)

# Easy and fast to read 
# rdsfile_list <- list.files(path = inp_out_folder, full.names = TRUE, pattern = "*\\.RDS")
# rdsfile_list_full <- list.files(path = inp_out_folder, full.names = TRUE, pattern = "*_clean\\.RDS") 
# rdsfile_list_sh <- list.files(path = inp_out_folder, full.names = TRUE, pattern = "*_Short\\.RDS") 
# rds_list_sh <- lapply(rdsfile_list_sh, readRDS)
# data <- data.table::rbindlist(rds_list_sh)


##################################################################################################################


##################################################################################################################
# Function for batch convert RDS data frame to csv
##################################################################################################################
batch_rds_to_csv <- function(input_folder, output_folder) {
  library(readr)
  
  file_names_paths <- list.files(path = input_folder, full.names = TRUE, pattern = "*\\.RDS")
  file_names <- basename(file_names_paths)
  
  for(i in seq_len(length(file_names))){
    cat("File number:", i, ".\n")
    file_name <- gsub(".RDS", "", file_names[i])
    file_csv_path <- file.path(output_folder, paste0(file_name, ".csv"))
    
    df <- readRDS(file_names_paths[i])
    readr::write_csv(df, file = file_csv_path)
  }  
}

# Way to use
# ieeg_in_folder_rds <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_RDS"; dir.exists(ieeg_in_folder_rds)
# ieeg_out_folder_csv <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_csv"; dir.exists(ieeg_out_folder_csv)
# batch_rds_to_csv(ieeg_in_folder_rds, ieeg_out_folder_csv)
##################################################################################################################


##################################################################################################################
# Batch processing and RDS output saving 
##################################################################################################################

# in_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file"
# out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file_RDS"
# batch_rds_export(in_folder, out_folder, output = "both", only_clean = TRUE)

# in_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file"
# out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file_RDS_Shorts"
# batch_rds_export(in_folder, out_folder, output = "short", only_clean = TRUE)

# in_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file"
# out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file_RDS_Shorts_2"
# batch_rds_export(in_folder, out_folder, output = "short", only_clean = TRUE)

in_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/output_file"
out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/output_file_RDS"
batch_rds_export(in_folder, out_folder, output = "both", only_clean = TRUE)


# iEEG
# ieeg_in_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file"; dir.exists(ieeg_in_folder)
# ieeg_out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_RDS"; dir.exists(ieeg_out_folder)
# batch_rds_export(ieeg_in_folder, ieeg_out_folder, output = "both", only_clean = TRUE)

# ieeg_in_folder_rds <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_RDS"; dir.exists(ieeg_in_folder_rds)
# ieeg_out_folder_csv <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_csv"; dir.exists(ieeg_out_folder_csv)
# batch_rds_to_csv(ieeg_in_folder_rds, ieeg_out_folder_csv)

# iEEG 110,112,113
# ieeg_in_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_110,112,113"; dir.exists(ieeg_in_folder)
# ieeg_out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_RDS_110,112,113"; dir.exists(ieeg_out_folder)
# batch_rds_export(ieeg_in_folder, ieeg_out_folder, output = "both", only_clean = TRUE)

# ieeg_in_folder_rds <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_RDS_110,112,113"; dir.exists(ieeg_in_folder_rds)
# ieeg_out_folder_csv <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_csv_110,112,113"; dir.exists(ieeg_out_folder_csv)
# batch_rds_to_csv(ieeg_in_folder_rds, ieeg_out_folder_csv)

# iEEG 116
# ieeg_in_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_116"; dir.exists(ieeg_in_folder)
# ieeg_out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_RDS_116"; dir.exists(ieeg_out_folder)
# batch_rds_export(ieeg_in_folder, ieeg_out_folder, output = "both", only_clean = TRUE)
# 
# ieeg_in_folder_rds <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_RDS_116"; dir.exists(ieeg_in_folder_rds)
# ieeg_out_folder_csv <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_ieeg/output_file_csv_116"; dir.exists(ieeg_out_folder_csv)
# batch_rds_to_csv(ieeg_in_folder_rds, ieeg_out_folder_csv)
##################################################################################################################




















###########################################################################################
# Validation checks -------------------------------------------------------
# DON'T RUN

# Other Checks
# bla <- 
# rds_list_sh[[1]] %>%
#   group_by(newGameState_cycl) %>%
#   dplyr::filter(dplyr::row_number() == 1, newGameState %in% c(10, 14:19)) %>%
#   dplyr::select(frame, timeStamp, newGameState_f, 
#                 Marker_type, markerType_demoimit,
#                 xPos, playerPosition_x_demoimit, zPos, playerPosition_z_demoimit,  
#                 Marker_x, markerPosition_x_demoimit, Marker_z, markerPosition_z_demoimit)

# Short_2
# tfolder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/test_new_script/justone"
# 
# # batch_rds_export(tfolder, tfolder, output = "full", only_clean = TRUE)
# 
# bla <- readRDS(list.files(tfolder, patter = ".RDS", full.names = TRUE))
# 
# blabla <-
#   bla %>%
#   dplyr::group_by(newGameState_cycl) %>%
#   dplyr::filter(!is.na(timeStamp_demoimit) | return == "True") %>%
#   dplyr::filter(newGameState_f %in% c(10, 15:19)) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(frame, newGameState_f, newGameState_cycl,
#                 return,
#                 timeStamp, timeStamp_demoimit,
#                 Marker_type, markerType_demoimit,
#                 xPos, playerPosition_x_demoimit, zPos, playerPosition_z_demoimit,
#                 Marker_x, markerPosition_x_demoimit, Marker_z, markerPosition_z_demoimit)
# 
# sum(as.logical(blabla$return), na.rm = TRUE)
# 
# blablabla <-
#   bla %>%
#   dplyr::filter(!is.na(timeStamp_demoimit)) %>%
#   dplyr::select(frame, newGameState_f, newGameState_cycl,
#                 return,
#                 timeStamp, timeStamp_demoimit,
#                 Marker_type, markerType_demoimit,
#                 xPos, playerPosition_x_demoimit, 
#                 zPos, playerPosition_z_demoimit, 
#                 Marker_x, markerPosition_x_demoimit, 
#                 Marker_z, markerPosition_z_demoimit)


###########################################################################################
# Validation checks -------------------------------------------------------
# DON'T RUN
mergetest <-
  frames_df_wide %>%
  dplyr::group_by(newGameState_cycl) %>%
  dplyr::filter(row_number() == n()) %>%          # or the first by: | row_number()==1 
  dplyr::ungroup()


mergetest_check <- 
  mergetest %>%            # cant check player position because Metrics are filled by frame
  dplyr::select(c(
    "newGameState_f", 
    "Marker_type", "markerType_demoimit",                
    "Marker_x", "markerPosition_x_demoimit",  # "FlagMarker_x", "FlagMarker_z", # Flags have wrong position in Metrics 
    "Marker_z", "markerPosition_z_demoimit"
  )) %>% 
  dplyr::filter(newGameState_f %in% c(10, 15:19))   # Start doesnt have Metrics  


mergetest_check %>%
  print(n = Inf)

all.equal(mergetest_check[[4]], mergetest_check[[5]])
all.equal(mergetest_check[[6]], mergetest_check[[7]])
# -------------------------------------------------------------------------
###########################################################################################






###########################################################################################
# Validation checks -------------------------------------------------------
# DON'T RUN
test_timeStamp <- dplyr::filter(full_data_output_clean, !is.na(timeStamp_demoimit))
all.equal(test_timeStamp[["timeStamp"]], test_timeStamp[["timeStamp_demoimit"]], tolerance = 10^2)

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
###########################################################################################




###########################################################################################
# Minimal example of strategy for matching through cycling based on newGameState_f
# DO NOT RUN
# states <- as.integer(c(1:19, 7:19, 15:19, 18:19, 18, 18, 19, 19))
# nas <- c(rep(NA_integer_, each = length(states) - 1), 19L)
# df <- data.frame(newGameState = as.character(rbind(states, nas, nas))) # minimal data has several inconsistencies to test the pipeline 
# 
# 
# df <- 
#   df %>%  
#   dplyr::mutate(newGameState_f = as.integer(newGameState)) %>%
#   tidyr::fill(newGameState_f, .direction = "down") %>%
#   # "newGameState_seg" codes with incremental values for every change in newGameState; newGameState was not filled in (locf) so it codes for gameState change 
#   dplyr::mutate(                      # this is like data.table::rleid() but ignores NAs which is important here                  
#     newGameState_seg = cumsum(
#       (!is.na(newGameState)) & (newGameState != dplyr::lag(ifelse(is.na(newGameState), "0", newGameState), default = "0"))
#     )) %>%
#   # "newGameState_cycl", "newGameState_cycln" code for number of each particular newGameState type (counter by group)
#   dplyr::group_by(newGameState_f) %>%
#   dplyr::mutate(
#     newGameState_cycln = data.table::rleid(newGameState_seg),
#     newGameState_cycl = paste0(newGameState_f, "_", newGameState_cycln)
#   ) %>%
#   dplyr::ungroup() 
# 
# # add empty columns with names of vars from processed sessioncfg_list definition & order data frames
# config_var_names <- c(
#   "StartMarker", "StartMarker_x", "StartMarker_y", "StartMarker_z",                           # colnames(demo_StartMarker_df)
#   "FlagMarker", "FlagMarker_x", "FlagMarker_y", "FlagMarker_z",                               # colnames(demo_FlagMarker_df)
#   "ObservationMarker", "ObservationMarker_x", "ObservationMarker_y", "ObservationMarker_z",   # colnames(demo_ObservationMarker_df)
#   "ObjectMarker", "ObjectMarker_x", "ObjectMarker_y", "ObjectMarker_z"                        # colnames(demo_ObjectMarker_df)
# )  
# 
# df[, config_var_names[seq(1, 16, by = 4)]]  <- NA_integer_
# df[, config_var_names[-seq(1, 16, by = 4)]] <- NA_real_
# 
# # merge
# df <- 
#   df %>%
#   as.data.frame() %>%
#   # Initial
#   merge_cfg_cols(def_df = initial_StartMarker_df, matchGameStates = matchGameStates_list$intial_start) %>%
#   merge_cfg_cols(def_df = initial_FlagMarker_df, matchGameStates = matchGameStates_list$intial_flag) %>%
#   merge_cfg_cols(def_df = initial_ObjectMarker_df, matchGameStates = matchGameStates_list$intial_object) %>%
#   # Demo
#   # Observe
#   merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[1]]) %>%
#   merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[2]]) %>%
#   merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[3]]) %>%
#   merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[4]]) %>%
#   merge_cfg_cols(def_df = demo_ObservationMarker_df, matchGameStates = matchGameStates_list$demo_observe[[5]]) %>%
#   # Start & Flag
#   merge_cfg_cols(def_df = demo_StartMarker_df, matchGameStates = matchGameStates_list$demo_start) %>%
#   merge_cfg_cols(def_df = demo_FlagMarker_df, matchGameStates = matchGameStates_list$demo_flag) %>%
#   # Object
#   merge_cfg_cols(def_df = demo_ObjectMarker_df, matchGameStates = matchGameStates_list$demo_object[[1]]) %>%
#   merge_cfg_cols(def_df = demo_ObjectMarker_df, matchGameStates = matchGameStates_list$demo_object[[2]]) %>%
#   merge_cfg_cols(def_df = demo_ObjectMarker_df, matchGameStates = matchGameStates_list$demo_object[[3]]) %>%
#   # Imit
#   # Observe
#   merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[1]]) %>%
#   merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[2]]) %>%
#   merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[3]]) %>%
#   merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[4]]) %>%
#   merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[5]]) %>%
#   merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[6]]) %>%
#   merge_cfg_cols(def_df = imit_ObservationMarker_df, matchGameStates = matchGameStates_list$imit_observe[[7]]) %>%
#   # Start & Flag
#   merge_cfg_cols(def_df = imit_StartMarker_df, matchGameStates = matchGameStates_list$imit_start) %>%
#   merge_cfg_cols(def_df = imit_FlagMarker_df, matchGameStates = matchGameStates_list$imit_flag) %>%
#   # Object
#   merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[1]]) %>% 
#   merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[2]]) %>% 
#   merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[3]]) %>% 
#   merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[4]]) %>% 
#   merge_cfg_cols(def_df = imit_ObjectMarker_df, matchGameStates = matchGameStates_list$imit_object[[5]]) 
# 
# 
# df %>%
#   dplyr::mutate(ObjectMarker = dplyr::recode(ObjectMarker, !!!marker_def_vef)) %>%
#   dplyr::mutate(StartMarker = dplyr::recode(StartMarker, !!!marker_def_vef)) %>%
#   dplyr::mutate(FlagMarker = dplyr::recode(FlagMarker, !!!marker_def_vef))
###########################################################################################
