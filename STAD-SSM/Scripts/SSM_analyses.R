# Preliminary analyses SSM

# Prerequisites -----------------------------------------------------------

##################################################################################################################
# Packages
##################################################################################################################

# Package names
packages <- c(
  "data.table", "tidyverse", "ggstatsplot", "lme4", "report"
)

# Dev versions of these packages may be necesary
# remotes::install_github("IndrajeetPatil/statsExpressions")
# remotes::install_github("IndrajeetPatil/ggstatsplot")

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



##################################################################################################################
# Settings
##################################################################################################################

data_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file_RDS_Shorts_2"
save_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01"
graphs_out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/graphs"

setwd(save_folder)

##################################################################################################################
# Functions
##################################################################################################################

# Function that computes euclidean distance 
eucl_dist <- function(x1, x2, y1, y2) {
  sqrt((x1-x2)^2 + (y1 - y2)^2)
}

# Function that converts list to nested data frame
list_to_nested <- function(list, group_var){
  list %>%
    do.call(dplyr::bind_rows, .) %>%
    dplyr::group_by( {{ group_var }} ) %>%
    tidyr::nest()
}





# Individual growth
plot_growth_state_id <- function(data, cond, gamestate, trial_lim = 20) {
  data %>%
    dplyr::filter(condition == cond, newGameState_f == gamestate) %>%
    group_by(id) %>%
    dplyr::mutate(trial = dplyr::row_number()) %>%
    dplyr::filter(trial < trial_lim) %>%  
    ggplot(aes(x = trial, y = score)) +
    geom_line() +
    facet_wrap(~id) +
    ylim(0, 50) +
    scale_x_continuous(breaks = 1:14) +
    ggtitle(paste0("Condition: ", cond, ", ", "Game state: ", gamestate))
}

# e.g.
# full_df %>%
#   dplyr::filter(condition == "bot", newGameState_f == "10") %>%
#   group_by(id) %>%
#   dplyr::mutate(trial = dplyr::row_number()) %>%
#   ggplot(aes(x = trial, y = score)) +
#   geom_line() +
#   facet_wrap(~id)

# Trend plots
plot_growth_state_loess <- function(data, cond, gamestate, trial_lim = 20, method = "loess") {
  data %>%
    dplyr::filter(condition == cond, newGameState_f == gamestate) %>%
    group_by(id) %>%
    dplyr::mutate(trial = dplyr::row_number()) %>%
    dplyr::filter(trial < trial_lim) %>% 
    dplyr::ungroup() %>% 
    ggplot(aes(x = trial, y = score)) +
    geom_line(aes(color = as.factor(id)), alpha = .5) +
    {if(method == "loess")
      geom_smooth(method = "loess", formula = "y ~ x", color = "red", fill = "red") 
    } +
    {if(method == "spline")
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, knots = seq(2 , 16, by = 2), degree = 1), 
                se = FALSE, color = "black", fill = "gray", alpha = 0.8) 
    } + 
    ylim(0, 50) +
    scale_x_continuous(breaks = 1:14) +
    ggtitle(paste0("Condition: ", cond, ", ", "Game state: ", gamestate)) +
    labs(color = "id")  
}

# e.e.
# full_df %>%
# dplyr::filter(condition == "bot", newGameState_f == 15) %>%
# group_by(id) %>%
# dplyr::mutate(trial = dplyr::row_number()) %>%
# ggplot(aes(x = trial, y = eucl_dist)) +
# geom_line(aes(color = as.factor(id)), alpha = .5) +
# geom_smooth(method = "loess", formula = "y ~ x")

plot_growth_state1017_id <- function(data, conds = c("social", "bot"), trial_lim = 20, method = NULL) {
  
  if(is.null(method)) {
    data %>%
      dplyr::filter(condition %in% conds, newGameState_f %in% c(10, 17)) %>%
      dplyr::mutate(newGameState_f = as.factor(newGameState_f)) %>%  
      dplyr::group_by(id, condition) %>%
      dplyr::mutate(trial = dplyr::row_number()) %>%
      dplyr::filter(trial < trial_lim) %>% 
      ggplot(aes(x = trial, y = score, color = condition)) +
      geom_line() +
      geom_point(aes(shape = newGameState_f)) + 
      facet_wrap(~id) +
      ylim(0, 50) +
      scale_x_continuous(breaks = 1:14) +
      ggtitle("Alternating GameState 10 and 17") + 
      theme(legend.position = "right") +
      theme_bw()
  } else if(method == "lm") {
    data %>%
      dplyr::filter(condition %in% conds, newGameState_f %in% c(10, 17)) %>%
      dplyr::mutate(newGameState_f = as.factor(newGameState_f)) %>%  
      dplyr::group_by(id, condition) %>%
      dplyr::mutate(trial = dplyr::row_number()) %>%
      dplyr::filter(trial < trial_lim) %>% 
      ggplot(aes(x = trial, y = score, color = condition)) +
      geom_smooth(method = "lm", formula = "y ~ x", aes(color = condition), se = FALSE) +
      geom_line(alpha = 0.15) +
      geom_point(aes(shape = newGameState_f), alpha = 0.15) +
      facet_wrap(~id) +
      ylim(0, 50) +
      scale_x_continuous(breaks = 1:14) +
      ggtitle("Alternating GameState 10 and 17") + 
      theme(legend.position = "right") 
  } 
}


# Within stats & plot for publication quality
my_ggwithinstats2 <- function(data, title = NULL, x, y, gamestate, trial_lim = 20, outlier.label, xlab, ylab, 
                              outlier.tagging = FALSE, results.subtitle = TRUE, 
                              centrality.label.args = TRUE, point.path = TRUE,
                              type = "parametric", 
                              ...) {  # ... for limits and breaks
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  outlier.label <- rlang::enquo(outlier.label)
  
  if(is.null(title)) {
    title <- paste0("Game state: ", gamestate)
  }
  
  data <-
    data %>%
    dplyr::select(!!outlier.label, !!x, !!y, newGameState_f) %>%      # newGameState_clean is hardcoded here
    dplyr::filter(newGameState_f == gamestate) %>%
    group_by(!!outlier.label, !!x) %>%
    dplyr::mutate(trial = dplyr::row_number()) %>%
    dplyr::filter(trial < trial_lim) %>%   
    dplyr::summarise(mean_score = mean(!!y, na.rm = TRUE))
  
  if(centrality.label.args){
    centrality.label.args <- list(size = 3, nudge_x = 0.2, segment.linetype = 5, fill = "#FFF8E7")
  }else{
    centrality.label.args <- list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0) # very hacky way of not showing label
  }
  
  data %>%
    ggstatsplot::ggwithinstats(
      x = !!x,
      y = mean_score,              # here we have the mean score for id, not !!y
      title = title,
      xlab = xlab,
      ylab = ylab,
      outlier.tagging = outlier.tagging,                    # whether outlines need to be tagged
      outlier.label = !!outlier.label,                      # variable to be used for tagging outliers
      outlier.coef = 2,
      pairwise.comparisons = TRUE,
      pairwise.display = "all",
      results.subtitle = results.subtitle,
      type = type,
      bf.message = FALSE, 
      p.adjust.method = "none",
      point.path = point.path,
      ggtheme = ggprism::theme_prism(),
      # package = "RColorBrewer",  # "ggsci",
      # palette = "Dark",         # "default_jco",
      violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
      centrality.plotting = TRUE,
      centrality.type = "parameteric",
      centrality.point.args = list(size = 5, color = "darkred"),
      centrality.label.args = centrality.label.args,
      ggplot.component = list(
        theme(
          plot.title = element_text(hjust = 0, size = 16),
          plot.subtitle = element_text(hjust = 0, size = 12), 
          plot.caption = element_text(hjust = 0, size = 12), 
          text = element_text(size = 14)
        ))
    ) + scale_colour_grey(start = 0.2, end = 0.2) +  # hacky way to change point color
    ylim(0, 50) +
    scale_y_continuous(...)
}

# e.g.
# my_ggwithinstats2(full_data_output_clean, x = condition, y = score, outlier.label = id, gamestate = 10,
#                   time_lim = 20, xlab = "Condition", ylab = "score")



# Grouped between stats & plot for publication quality
my_grouped_ggbetweenstats2 <- function(data, title = NULL, x, y, group, gamestate, trial_lim = 20, outlier.label, xlab, ylab, 
                                       outlier.tagging = FALSE, results.subtitle = TRUE, 
                                       centrality.label.args = TRUE, point.path = TRUE,
                                       type = "parametric", 
                                       ...) {  # ... for limits and breaks
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)
  outlier.label <- rlang::enquo(outlier.label)
  
  if(is.null(title)) {
    title <- paste0("Game state: ", gamestate)
  }
  
  data <-
    data %>%
    dplyr::select(!!outlier.label, !!x, !!y, !!group, newGameState_f) %>%      # newGameState_clean is hardcoded here
    dplyr::filter(newGameState_f == gamestate) %>%
    group_by(!!outlier.label, !!x, !!group) %>%
    dplyr::mutate(trial = dplyr::row_number()) %>%
    dplyr::filter(trial < trial_lim) %>%   
    dplyr::summarise(mean_score = mean(!!y, na.rm = TRUE)) %>%
    dplyr::ungroup() 
  
  if(centrality.label.args){
    centrality.label.args <- list(size = 3, nudge_x = 0.2, segment.linetype = 5, fill = "#FFF8E7")
  }else{
    centrality.label.args <- list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0) # very hacky way of not showing label
  }
  
  data %>%
    ggstatsplot::grouped_ggbetweenstats(
      x = !!x,
      y = mean_score,                                      # here we have the mean score for id, not !!y
      grouping.var = !!group,
      # xlab = xlab,
      # ylab = ylab,
      outlier.tagging = outlier.tagging,                    # whether outlines need to be tagged
      outlier.label = !!outlier.label,                      # variable to be used for tagging outliers
      annotation.args  = list(title = title),
      outlier.coef = 2,
      pairwise.comparisons = TRUE,
      pairwise.display = "all",
      results.subtitle = results.subtitle,
      type = type,
      bf.message = FALSE, 
      p.adjust.method = "none",
      point.path = point.path,
      ggtheme = ggprism::theme_prism(),
      violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
      centrality.plotting = TRUE,
      centrality.type = "parameteric",
      centrality.point.args = list(size = 5, color = "darkred"),
      centrality.label.args = centrality.label.args,
      ggplot.component = list(
        theme(
          plot.title = element_text(hjust = 0, size = 16),
          plot.subtitle = element_text(hjust = 0, size = 12),
          plot.caption = element_text(hjust = 0, size = 12),
          text = element_text(size = 14)
        ))
    ) + 
    ylim(0, 50) 
  
}

# e.g.
# my_grouped_ggbetweenstats2(metrics_df, x = gender, y = score, group = condition, outlier.label = id, gamestate = 19, time_lim = 20)



# Fast ggsave - saves plot with filename of R plot object
fast_ggsave <- function(plot, device = "png", path = NULL,
                        units = "in", dpi = 300, width = 5, height = 5, ...){ 
  plot_name <- deparse(substitute(plot))
  ggplot2::ggsave(filename = paste0(plot_name, ".", device), plot = plot,
                  device = device, path = path,
                  units = units, dpi = dpi,
                  width = width, height = height,
                  ...
  )
  
} # use: fast_ggsave(plot_1, path = graphs_out_folder)

##################################################################################################################
# Read participant table
##################################################################################################################

part_data_list <- readRDS("data_clean_output_2022.09.01.RDS")
full_part_table_df <- part_data_list$data_clean

# Fill in participant level data
full_part_table_df <- 
  full_part_table_df %>%
  group_by(id) %>%
  tidyr::fill(gender, age, .direction = "downup") %>%
  dplyr::ungroup()


# Exclude participants
part_table_df <-
  full_part_table_df %>%
  dplyr::filter(problem != 1) %>%       # don't have single botrec file (not split in two)
  dplyr::filter(id != "41.0") %>%       # participant has "1bis" bot order so two alone conditions, no social ... exclude
  dplyr::filter(id != "15.0") %>%       # participant has short bot condition recording
  group_by(id) %>%
  dplyr::filter(dplyr::n() > 1) %>%   # don't have both conditions
  dplyr::ungroup()

##################################################################################################################
# Read session data
##################################################################################################################

# Read all files into list
# rdsfile_list <- list.files(path = data_folder, full.names = TRUE, pattern = "*\\.RDS")
# rdsfile_list_full <- list.files(path = data_folder, full.names = TRUE, pattern = "*_clean\\.RDS")
rdsfile_list_sh <- list.files(path = data_folder, full.names = TRUE, pattern = "*_Short\\.RDS")
rds_list_sh <- lapply(rdsfile_list_sh, readRDS)

# One data frame (use fill=TRUE, some older botrecs still have "syncKey" column)
# data <- data.table::rbindlist(rds_list_sh, fill = TRUE) # Item 7 has 61 columns

# List of recordings to nested data frames
sessions_data_df <- list_to_nested(rds_list_sh, file)


##################################################################################################################
# Merge participant data with session data
##################################################################################################################

# Merge nested data frames into participant table
full_df <-
  part_table_df %>%
  dplyr::select(-c(id2, notes, problem)) %>% 
  dplyr::left_join(sessions_data_df, by = c("output_file_clean" = "file"))

full_df <- 
  full_df %>%
  tidyr::unnest(cols = data) %>%
  dplyr::select(-syncKey)


##################################################################################################################

##################################################################################################################
# Compute new columns - recomputed Eucl Dist - CORRECT BUT LAZY APPROACH
# strategy: recomputed Eucl Dist (based on x, z from Metrics & Marker, real Flag positions)
##################################################################################################################

full_recomputed_df <- 
  full_df %>%
  dplyr::group_by(output_file, newGameState_cycl) %>%      
  tidyr::fill(
    dplyr::all_of(c(
      "playerPosition_x_demoimit", "playerPosition_y_demoimit", "playerPosition_z_demoimit",
      "markerPosition_x_demoimit", "markerPosition_y_demoimit", "markerPosition_z_demoimit",
      "distanceFromMarker_demoimit", "playerType_demoimit",
      "markerType_demoimit", "score_demoimit"
    )),
    .direction = "updown"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(timeStamp_demoimit)) %>%      # READ BELOW WHY THESE FILTERS ARE CORRECT
  dplyr::filter(yPos < 1) %>%                        # this is the same as who == playerType_demoimit
  dplyr::mutate(markerType_demoimit = dplyr::recode(markerType_demoimit, !!!marker_def_vef)) %>%
  dplyr::mutate(eucl_dist = eucl_dist(x1 = playerPosition_x_demoimit, x2 = Marker_x, y1 = playerPosition_z_demoimit, y2 = Marker_z))

##############
# Important checks
full_lframe_df <- 
  full_df %>%
  dplyr::group_by(output_file, newGameState_cycl) %>%      
  tidyr::fill(
    dplyr::all_of(c(
      "playerPosition_x_demoimit", "playerPosition_y_demoimit", "playerPosition_z_demoimit",
      "markerPosition_x_demoimit", "markerPosition_y_demoimit", "markerPosition_z_demoimit",
      "distanceFromMarker_demoimit", "playerType_demoimit",
      "markerType_demoimit", "score_demoimit"
    )),
    .direction = "updown"
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(markerType_demoimit = dplyr::recode(markerType_demoimit, !!!marker_def_vef)) %>%
  dplyr::mutate(eucl_dist = eucl_dist(x1 = playerPosition_x_demoimit, x2 = playerPosition_z_demoimit, y1 = Marker_x, y2 = Marker_z)) %>%
  dplyr::filter(!is.na(timeStamp_demoimit))    


# check1 = gameStates with flag have Marker Type and Position as Object
full_lframe_df %>%
  dplyr::select("output_file", "frame", "newGameState_f", "Marker_type", "markerType_demoimit") %>%
  print(n = 50)

# check2 = when (1) yPos > 1 (8 means player is at high ObservationPoint) or newGameState_f == 18 (it's the other player) then Pos != playerPosition 
full_lframe_df %>%
  dplyr::select("output_file", "frame", "newGameState_f", "yPos", 
                "xPos", "playerPosition_x_demoimit",
                "zPos", "playerPosition_z_demoimit") %>%
  print(n = 50)

# check3 = playerType_demoimit & who - basically when playerType_demoimit == who => yPos < 1
full_lframe_df %>%
  dplyr::select("output_file", "frame", "newGameState_f", "yPos", "who", "playerType_demoimit") %>%
  print(n = 50)

all((full_lframe_df$yPos > 1) == (full_lframe_df$who != full_lframe_df$playerType_demoimit))
all((full_lframe_df$yPos < 1) == (full_lframe_df$who == full_lframe_df$playerType_demoimit))

# after check1 & check 2 => use Marker_type, Marker_x, Marker_z from recomputed AND playerPosition_x_demoimit, playerPosition_z_demoimit from Metrics 
# but only for frames in !is.na(timeStamp_demoimit) & (yPos < 1); where (yPos < 1) is equivalent to playerType_demoimit == who 


# check 4 = all markerType_demoimit == Marker_type, except for Gamestate 14/15 where Start/Flag are considered Objects in demoimit -- use Marker_type 
test4 <- full_recomputed_df %>%
  dplyr::select("output_file", "frame", "newGameState_f", "markerType_demoimit", "Marker_type") %>%
  dplyr::mutate(check = markerType_demoimit == Marker_type) %>%
  dplyr::filter(!newGameState_f %in% c(14, 15))
all(test4$check)
  
# check 5 = distanceFromMarker_demoimit == eucl_dist
test5 <- full_recomputed_df %>%
  dplyr::select("output_file", "frame", "newGameState_f", "distanceFromMarker_demoimit", "eucl_dist") %>%
  dplyr::mutate(check = abs(distanceFromMarker_demoimit - eucl_dist) < 0.01) %>%
  dplyr::filter(!newGameState_f %in% c(14, 15))
sum(!test5$check)

# check 6 = Player x, z from frames and Player x, z from demoimit are equal for gameStates excluding 18 that stores position for 'other'
test6 <- full_recomputed_df %>%
  dplyr::select("output_file", "frame", "newGameState_f", "xPos", "playerPosition_x_demoimit", "zPos", "playerPosition_z_demoimit") %>%
  dplyr::mutate(
                check_x = abs(playerPosition_x_demoimit - xPos) < 0.01,
                check_z = abs(playerPosition_z_demoimit - zPos) < 0.01,
                check_both = check_x + check_z) %>%
  dplyr::filter(newGameState_f != 18)
sum(test6$check_both < 2)       # this is the total no of frames that have missmatching x & z


# check 7 =
test7 <- full_recomputed_df %>%
  dplyr::select("output_file", "frame", "newGameState_f", "Marker_x", "markerPosition_x_demoimit", "Marker_z", "markerPosition_z_demoimit") %>%
  dplyr::mutate(
    check_x = abs(markerPosition_x_demoimit - Marker_x) < 0.01,
    check_z = abs(markerPosition_z_demoimit - Marker_z) < 0.01,
    check_both = check_x + check_z) %>%
  dplyr::filter(!newGameState_f %in% c(14, 15))
sum(test7$check_both < 2)

# sabin <- full_recomputed_df %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "return",
#                 "who", "playerType_demoimit",
#                 "xPos", "playerPosition_x_demoimit",
#                 "zPos", "playerPosition_z_demoimit",
#                 "yPos", "playerPosition_y_demoimit",
#                 "Marker_x", "markerPosition_x_demoimit",
#                 "Marker_z", "markerPosition_z_demoimit",
#                 "Marker_y", "markerPosition_y_demoimit",
#                 "eucl_dist", "distanceFromMarker_demoimit"
#     )
#   rio::export(sabin, "sabin.xlsx")
##############


##############
# For tests
# names_cols_compare <- c("output_file", "frame", "newGameState_f", "newGameState_cycl", "return", "yPos",
#                         "timeStamp", "timeStamp_demoimit",
#                         "Marker_type", "markerType_demoimit",
#                         "xPos", "playerPosition_x_demoimit",
#                         "zPos", "playerPosition_z_demoimit",
#                         "Marker_x", "markerPosition_x_demoimit",
#                         "Marker_z", "markerPosition_z_demoimit")

# An old test
# bla <-
#   full_lframe_df %>%
#   dplyr::filter(output_file == "config_4Mks30Reps_15.09.2021_17.42_DESKTOP-PJTKC0O_PLAYER_1_True_.botrec") %>%
#   dplyr::select(all_of(names_cols_compare)) %>%
#   dplyr::mutate(return = as.logical(return)) %>%
#   dplyr::group_by(output_file, newGameState_cycl) %>%
#   dplyr::mutate(last_frame = dplyr::if_else(row_number() == n(), TRUE, FALSE)) %>%
#   dplyr::mutate(first_return_frame = return & !duplicated(return),   # tags with TRUE only first TRUE return 
#                 first_return_frame = dplyr::na_if(first_return_frame, FALSE)) %>%
#   dplyr::ungroup() %>% 
#   dplyr::filter(first_return_frame | last_frame,
#                 newGameState_f != 18,
#                 yPos < 1.3) %>%
#   dplyr::mutate(diff_x = xPos - playerPosition_x_demoimit,
#                 diff_z = zPos - playerPosition_z_demoimit)
# 
# sum(!is.na(bla$return))
# sum(!is.na(bla$timeStamp_demoimit))
# bla[, c("newGameState_cycl", "diff_x", "diff_z")]

# bla2 <- 
#   full_lframe_df %>%
#   dplyr::filter(output_file == "config_4Mks30Reps_15.09.2021_17.42_DESKTOP-PJTKC0O_PLAYER_1_True_.botrec") %>%
#   dplyr::mutate(return = as.logical(return)) %>%
#   dplyr::group_by(output_file, newGameState_cycl) %>%
#   dplyr::mutate(last_frame = dplyr::if_else(row_number() == n(), TRUE, FALSE)) %>%
#   dplyr::mutate(first_return_frame = return & !duplicated(return),   # tags with TRUE only first TRUE return 
#                 first_return_frame = dplyr::na_if(first_return_frame, FALSE)) %>%
#   dplyr::ungroup()
# rio::export(bla2, "C:/Users/claud/Desktop/ex_botrec.xlsx")
##############

##################################################################################################################
# Compute new columns - REAL DEAL - COMPLETE AFTERWARDS
##################################################################################################################

full_df <- 
  full_df %>%
  dplyr::mutate(eucl_dist = eucl_dist(x1 = xPos, x2 = zPos, y1 = Marker_x, y2 = Marker_z)) %>%
  dplyr::mutate(return = as.logical(return)) %>%            # this works out of the box
  dplyr::group_by(output_file, newGameState_cycl, return) %>%        # by outputfile (identifies participant&session), by cycl (identifies particular gameState)
  dplyr::mutate(last_frame = dplyr::if_else(row_number() == n(), TRUE, FALSE)) %>%
  dplyr::mutate(first_return_frame = return & !duplicated(return),   # tags with TRUE only first TRUE return 
                first_return_frame = dplyr::na_if(first_return_frame, FALSE)) %>%
  dplyr::ungroup()

# Check -- good
# full_df$eucl_dist[2]
# rbind(
#   full_df[2, c("xPos", "Marker_x")] %>% unlist(),
#   full_df[2, c("zPos", "Marker_z")] %>% unlist()
# ) %>% dist(method = "euclidean")
# full_df[, c("newGameState_f", "return", "first_return_frame")] %>% print(n = 200)   # row 129 and 131 nonconsecutive true returns in same gameState


# Data frame with only one frame per gameState -- HERE
full_onegs_df <-
  full_df %>%
  dplyr::group_by(id, newGameState_cycl) %>%
  dplyr::filter(dplyr::row_number() == dplyr::n() |                      # last frame of gameState
                first_return_frame)                                      # first TRUE return of gameState



##################################################################################################################
# Sample characteristics 
##################################################################################################################

# Sample count
unique(full_part_table_df$id) %>% length()  # total participant unique ids
unique(part_table_df$id) %>% length()       # have complete data with no issues

unique(full_part_table_df$id) %>% length() - unique(part_table_df$id) %>% length()   # excluded


# Data frame for descriptives
desc_part_table_df <-
  part_table_df %>%
  # keep only one record for each participant
  dplyr::group_by(id) %>%
  dplyr::filter(dplyr::row_number() == 1) %>%
  dplyr::ungroup() %>%
  # do rest
  dplyr::mutate(age_categ = cut(age, 
                                breaks = c(19, 30, 40, 50, 60), 
                                labels = c("19-29","30-39","40-49", "50-59"), 
                                right = FALSE)) %>%            #  select(id, age, age_categ)  %>% group_by(age_categ) %>% group_split() %>% print(n = Inf)
  dplyr::mutate(age = as.factor(age),
                gender = as.factor(gender)) %>% 
  dplyr::mutate(gender = forcats::fct_recode(gender, "Female" = "F", "Male" = "M"))


# Age
age_plot <- 
  part_table_df %>%
    # keep only one record for each participant
    dplyr::group_by(id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    # do rest
    # ggplot(aes(age)) +
    # geom_histogram() +
    # scale_x_continuous(breaks = seq(20, 60, by = 5))
    dplyr::mutate(age = factor(age, levels = c(19:55))) %>%    # fill in unused factor levels
    dplyr::count(age, .drop = FALSE) %>%         # Group by, then count number in each group (dont drop 0 counts)
    dplyr::mutate(pct = prop.table(n)) %>%                           # Calculate percent within each var
    ggplot(aes(x = age, y = pct, label = scales::percent(pct))) + 
    geom_col(position = position_dodge(preserve = "single"), stat = "identity") +    # Don't drop zero count
    geom_text(position = position_dodge(width = .9),      # move to center of bars
              vjust = -0.5,                               # nudge above top of bar
              size = 3) + 
    scale_y_continuous(labels = scales::percent) +
    ggtitle("") +
    xlab("age") + ylab("Percentage %") + 
    theme_bw()

# fast_ggsave(age_plot, path = graphs_out_folder)

# Gender
gender_plot <- 
  part_table_df %>%
    # keep only one record for each participant
    dplyr::group_by(id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    # do rest
    dplyr::mutate(gender = as.factor(gender)) %>% 
    dplyr::mutate(gender = forcats::fct_recode(gender, "Female" = "F", "Male" = "M")) %>%
    dplyr::group_by(gender) %>%
    dplyr::summarise(counts = n()) %>%
    dplyr::mutate(prop = round(counts*100/sum(counts), 1),
           lab.ypos = cumsum(prop) - .5*prop,
           Percent = paste0(prop, " %")) %>% 
    ggpubr::ggpie(x = "prop", label = "Percent",
                  fill = "gender", color = "white", 
                  lab.pos = "in", lab.font = list(color = "white"),
                  palette = "grey")

# fast_ggsave(gender_plot, path = graphs_out_folder)

# Gender and Age
genderage_plot <- 
  part_table_df %>%
    # keep only one record for each participant
    dplyr::group_by(id) %>%
    dplyr::filter(dplyr::row_number() == 1) %>%
    dplyr::ungroup() %>%
    # do rest
    dplyr::mutate(age_categ = cut(age, 
                                  breaks = c(19, 30, 40, 50, 60), 
                                  labels = c("19-29","30-39","40-49", "50-59"), 
                                  right = FALSE)) %>%            #  select(id, age, age_categ)  %>% group_by(age_categ) %>% group_split() %>% print(n = Inf)
    dplyr::mutate(age = as.factor(age),
                  gender = as.factor(gender)) %>% 
    dplyr::mutate(gender = forcats::fct_recode(gender, "Female" = "F", "Male" = "M")) %>%
    dplyr::count(age_categ, gender, .drop = FALSE) %>%         # Group by, then count number in each group (dont drop 0 counts)
    dplyr::mutate(pct = prop.table(n)) %>%                           # Calculate percent within each var
    ggplot(aes(x = age_categ, y = pct, fill = gender, label = scales::percent(pct))) + 
    geom_col(position = position_dodge(preserve = "single"), stat = "identity",) +    # Don't drop zero count
    geom_text(position = position_dodge(width = .9),      # move to center of bars
              vjust = -0.5,                               # nudge above top of bar
              size = 3) + 
    scale_y_continuous(labels = scales::percent) +
    ggtitle("") +
    xlab("age") + ylab("Percentage %") + 
    guides(fill = guide_legend(title = "gender", ncol = 1)) + 
    scale_fill_grey(start = 0.8, end = 0.2, na.value = "red", aesthetics = "fill") +
    theme(legend.position = "right", legend.direction = "vertical", 
          legend.justification = c(0, 1), panel.border = element_rect(fill = NA, colour = "black")) +
    theme_bw()

# fast_ggsave(genderage_plot, path = graphs_out_folder)

# Condition order

# 1. Condition, order 
part_table_df %>%
  dplyr::select(condition, order) %>%
  table()

part_table_df %>%
  dplyr::select(condition, order) %>%
  GGally::ggpairs() +
  theme_bw()

# 2. Condition, order, gender
part_table_df %>%
  dplyr::select(condition, order, gender) %>%
  dplyr::mutate(gender = forcats::fct_recode(gender, "Female" = "F", "Male" = "M")) %>%
  table()

part_table_df %>%
  dplyr::select(condition, order, gender) %>%
  dplyr::mutate(gender = forcats::fct_recode(gender, "Female" = "F", "Male" = "M")) %>%
  GGally::ggpairs(aes(color = gender)) +
  theme_bw()

# 3. Condition, order, gender, age
part_table_df %>%
  dplyr::select(condition, order, gender, age) %>%
  dplyr::mutate(gender = forcats::fct_recode(gender, "Female" = "F", "Male" = "M")) %>%
  dplyr::mutate(age_categ = cut(age,
                                breaks = c(19, 30, 40, 50, 60),
                                labels = c("19-29","30-39","40-49", "50-59"),
                                right = FALSE)) %>%
  dplyr::select(-age) %>%
  table()

pairs_plot <- 
  part_table_df %>%
    dplyr::select(condition, order, gender, age) %>%
    dplyr::mutate(gender = forcats::fct_recode(gender, "Female" = "F", "Male" = "M")) %>%
    GGally::ggpairs(aes(fill = gender),
                    upper = list(continuous = "smooth", combo = "box", discrete = "facetbar", na = "na"),
                    lower = list(continuous = "smooth_loess", combo = "facethist", discrete = "count", na = "na"), # "facetdensity"
                    diag = list(continuous = GGally::wrap("densityDiag", alpha=0.3), discrete = "barDiag", na = "naDiag")) +
    scale_fill_grey(start = 0.5, end = 0.1, na.value = "red", aesthetics = "fill") +
    theme_bw()

# fast_ggsave(pairs_plot, path = graphs_out_folder)

##################################################################################################################

##################################################################################################################
# Game States 
##################################################################################################################



##################################################################################################################

##################################################################################################################
# Analyses on scores from Metrics
##################################################################################################################

# Data
metrics_df <- 
  full_df %>%
  dplyr::filter(!is.na(timeStamp_demoimit)) %>%
  dplyr::rename(score = score_demoimit) %>%
  dplyr::mutate(gender = as.factor(gender))


# Sample trends by gameState
growth_10 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(metrics_df, "bot", 10) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(metrics_df, "social", 10) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_10, path = graphs_out_folder)

growth_17 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(metrics_df, "bot", 17) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(metrics_df, "social", 17) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_17, path = graphs_out_folder)

growth_19 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(metrics_df, "bot", 19) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(metrics_df, "social", 19) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_19, path = graphs_out_folder)



# Alternating GameStates 10 and 17 for each participant by condition
growth_1017 <- 
  metrics_df %>%
    plot_growth_state1017_id() + theme_classic()
# fast_ggsave(growth_1017, path = graphs_out_folder, width = 14, height = 10)

growth_1017_slope <-
  metrics_df %>%
    plot_growth_state1017_id(method = "lm") + theme_classic()
# fast_ggsave(growth_1017_slope, path = graphs_out_folder, width = 14, height = 10)

growth_1017_slope_10trial <-
  metrics_df %>%
  plot_growth_state1017_id(method = "lm", trial_lim = 10) + theme_classic()
# fast_ggsave(growth_1017_slope_10trial, path = graphs_out_folder, width = 14, height = 10)



# Individual average scores (mean) - social vs bot
botsoc_10 <- 
  my_ggwithinstats2(metrics_df, x = condition, y = score, outlier.label = id, gamestate = 10,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
    suppressMessages()
# fast_ggsave(botsoc_10, path = graphs_out_folder, width = 8, height = 8)

botsoc_17 <- 
  my_ggwithinstats2(metrics_df, x = condition, y = score, outlier.label = id, gamestate = 17,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
    suppressMessages()
# fast_ggsave(botsoc_17, path = graphs_out_folder, width = 8, height = 8)

# my_ggwithinstats2(metrics_df, x = condition, y = score, outlier.label = id, gamestate = 18,
#                   trial_lim = 20, xlab = "Condition", ylab = "score") %>%
#   suppressMessages()

botsoc_19 <-
  my_ggwithinstats2(metrics_df, x = condition, y = score, outlier.label = id, gamestate = 19,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
    suppressMessages()
# fast_ggsave(botsoc_19, path = graphs_out_folder, width = 8, height = 8)


# Trial level - Male vs Female
gender_10 <- 
  metrics_df %>% 
  my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 10, time_lim = 20)
# fast_ggsave(gender_10, path = graphs_out_folder, width = 14, height = 10)  
  
gender_17 <- 
  metrics_df %>% 
  my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 17, time_lim = 20)
# fast_ggsave(gender_17, path = graphs_out_folder, width = 14, height = 10)

gender_19 <- 
  metrics_df %>%
  my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 19, time_lim = 20)
# fast_ggsave(gender_19, path = graphs_out_folder, width = 14, height = 10)  


###########################
# LMM

lmm_mod_1 <-
  metrics_df %>% 
  dplyr::filter(newGameState_f %in% c(10)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + condition * order + (1 | id), data = .)

summary(lmm_mod_1)

report::report(lmm_mod_1)


lmm_mod_2 <-          # condition x gender interaction - just a trend
  metrics_df %>% 
  dplyr::filter(newGameState_f %in% c(10)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + order + condition * gender + (1 | id), data = .)

summary(lmm_mod_2)

report::report(lmm_mod_2)

sjPlot::plot_model(lmm_mod_2, type = "int", terms = c("condition", "gender"), ci.lvl = 0.95)
sjPlot::plot_model(lmm_mod_2, type = "pred", terms = c("condition", "gender"), ci.lvl = 0.95)


# lmm_mod <-
#   metrics_df %>% 
#     dplyr::filter(newGameState_f %in% c(10)) %>%
#     group_by(id, condition) %>%
#     dplyr::mutate(trial = dplyr::row_number()) %>%
#     dplyr::ungroup() %>%
#     lme4::lmer(score ~  1 + condition + trial + order + (1 + trial | id), data = .)
# 
# summary(lmm_mod)
# 
# report::report(lmm_mod)


##################################################################################################################

##################################################################################################################
# Analyses on recomputed Eucl Dist (based on x, z from Metrics & Marker, real Flag positions)
##################################################################################################################

# Data
recomputed_df <- 
  full_recomputed_df %>%
  dplyr::rename(score = eucl_dist) %>%
  dplyr::mutate(gender = as.factor(gender))


# Sample trends by gameState
growth_10 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 10) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 10) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_10, path = graphs_out_folder)

growth_17 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 17) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 17) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_17, path = graphs_out_folder)

growth_19 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 19) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 19) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_19, path = graphs_out_folder)



# Alternating GameStates 10 and 17 for each participant by condition
growth_1017 <- 
  recomputed_df %>%
  plot_growth_state1017_id() + theme_classic()
# fast_ggsave(growth_1017, path = graphs_out_folder, width = 14, height = 10)

growth_1017_slope <-
  recomputed_df %>%
  plot_growth_state1017_id(method = "lm") + theme_classic()
# fast_ggsave(growth_1017_slope, path = graphs_out_folder, width = 14, height = 10)

growth_1017_slope_10trial <-
  recomputed_df %>%
  plot_growth_state1017_id(method = "lm", trial_lim = 10) + theme_classic()
# fast_ggsave(growth_1017_slope_10trial, path = graphs_out_folder, width = 14, height = 10)



# Individual average scores (mean) - social vs bot
botsoc_10 <- 
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 10,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_10, path = graphs_out_folder, width = 8, height = 8)

botsoc_17 <- 
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 17,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_17, path = graphs_out_folder, width = 8, height = 8)

# my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 18,
#                   trial_lim = 20, xlab = "Condition", ylab = "score") %>%
#   suppressMessages()

botsoc_19 <-
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 19,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_19, path = graphs_out_folder, width = 8, height = 8)


# Trial level - Male vs Female
gender_10 <- 
  recomputed_df %>% 
  my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 10, time_lim = 20)
# fast_ggsave(gender_10, path = graphs_out_folder, width = 14, height = 10)  

gender_17 <- 
  recomputed_df %>% 
  my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 17, time_lim = 20)
# fast_ggsave(gender_17, path = graphs_out_folder, width = 14, height = 10)

gender_19 <- 
  recomputed_df %>%
  my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 19, time_lim = 20)
# fast_ggsave(gender_19, path = graphs_out_folder, width = 14, height = 10)  


###########################
# LMM

lmm_mod_1 <-
  recomputed_df %>% 
  dplyr::filter(newGameState_f %in% c(10)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + condition * order + (1 | id), data = .)

summary(lmm_mod_1)

report::report(lmm_mod_1)


lmm_mod_2 <-          # condition x gender interaction - just a trend
  recomputed_df %>% 
  dplyr::filter(newGameState_f %in% c(10)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + order + condition * gender + (1 | id), data = .)

summary(lmm_mod_2)

report::report(lmm_mod_2)

sjPlot::plot_model(lmm_mod_2, type = "int", terms = c("condition", "gender"), ci.lvl = 0.95)
sjPlot::plot_model(lmm_mod_2, type = "pred", terms = c("condition", "gender"), ci.lvl = 0.95)


# lmm_mod <-
#   recomputed_df %>% 
#     dplyr::filter(newGameState_f %in% c(10)) %>%
#     group_by(id, condition) %>%
#     dplyr::mutate(trial = dplyr::row_number()) %>%
#     dplyr::ungroup() %>%
#     lme4::lmer(score ~  1 + condition + trial + order + (1 + trial | id), data = .)
# 
# summary(lmm_mod)
# 
# report::report(lmm_mod)

##################################################################################################################































##################################################
#### some test on full botrec data

allframes_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file_RDS"
allframes_test_file <- "config_4Mks30Reps_01.12.2021_10.48_DESKTOP-PJTKC0O_PLAYER_1_True__clean.RDS"

allframes_df <- readRDS(file.path(allframes_folder, allframes_test_file))

unique(allframes_df$playerType_demoimit)
plot(allframes_df$time, allframes_df$timeStamp)










############# old stuff
# metrics_df %>% 
#   dplyr::filter(newGameState_f == 19) %>% 
#   group_by(id, gender, condition) %>%
#   dplyr::mutate(trial = dplyr::row_number()) %>%
#   # dplyr::filter(trial < trial_lim) %>%   
#   dplyr::summarise(mean_score = mean(score, na.rm = TRUE)) %>%
#   dplyr::ungroup() %>%
#   ggstatsplot::grouped_ggbetweenstats(x = gender, y = mean_score, outlier.label = id,
#                                       grouping.var = condition,
#                                       annotation.args  = list(title = "Gamestate 19"),
#                                       ggtheme = ggprism::theme_prism(),
#                                       violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
#                                       centrality.plotting = TRUE,
#                                       centrality.type = "parameteric",
#                                       centrality.point.args = list(size = 5, color = "darkred"),
#                                       # centrality.label.args = TRUE,
#                                       ggplot.component = list(
#                                         theme(
#                                           plot.title = element_text(hjust = 0, size = 16),
#                                           plot.subtitle = element_text(hjust = 0, size = 12), 
#                                           plot.caption = element_text(hjust = 0, size = 12), 
#                                           text = element_text(size = 14)
#                                         ))
#   ) + # scale_colour_grey(start = 0.2, end = 0.2) +  # hacky way to change point color
#   ylim(0, 50) 
# 
#############