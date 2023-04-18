# Preliminary analyses SSM

# Prerequisites -----------------------------------------------------------

##################################################################################################################
# Packages
##################################################################################################################

# Package names
packages <- c(
  "data.table", "tidyverse", "ggstatsplot", "lme4", "report", "broom"
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



# Game State Labels
gamestate_lab <- c(     # meant to be used with dplyr::recode(as.character(newGameState_f), !!!gamestate_lab) 
  "0"  = "0", 
  "1"  = "1", 
  "2"  = "2", 
  "3"  = "3", 
  "4"  = "4", 
  "5"  = "5", 
  "6"  = "6", 
  "7"  = "7", 
  "8"  = "8", 
  "9"  = "RO", 
  "10" = "10", 
  "11" = "11", 
  "12" = "12", 
  "13" = "13", 
  "14" = "RSf",  
  "15" = "RYf", 
  "16" = "R1O",
  "17" = "S", 
  "18" = "R2O", 
  "19" = "19", 
  "20" = "20", 
  "21" = "21"
)


##################################################################################################################



##################################################################################################################
# Settings
##################################################################################################################

# data_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/output_file_RDS_Shorts_2"
# save_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01"
# graphs_out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_2022.09.01/graphs"

data_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/output_file_RDS"
save_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022"
# graphs_out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/graphs"
graphs_out_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/graphs_nooutlier"

part_data_rds <- file.path(save_folder, "data_clean_output_01.09-24.11.2022.RDS")

setwd(save_folder)

##################################################################################################################
# Functions
##################################################################################################################

# Function that computes euclidean distance 
eucl_dist <- function(x1, x2, y1, y2) {
  sqrt((x1-x2)^2 + (y1 - y2)^2)
}


# Function that computes slope of line between two 2D points
# points_to_slope <- function(x1, x2, y1, y2) {
#   if (x2 - x1 != 0 | is.na(x2 - x1)) {
#     (y2 - y1)/(x2 - x1)
#   } else {
#     .Machine$integer.max
#   } 
# }
# points_to_slope(1, 2, 3, 4); points_to_slope(3, 4, 1, 2)
# points_to_slope(4, 2, 2, 5) # should give -1.5

# slopes_to_obliqueangle <- function(m1, m2) {
#   angle <- abs((m2 - m1) / (1 + m1 * m2))  # tan value of the angle
#   ret <- atan(angle)                       # calculate tan inverse of the angle
#   val <- (ret * 180) / pi                  # convert the angle from radian to degree
#   val
# }
# slopes_to_angle(1.75, 0.27)    # should give 45.1455

# Inner angle (<= 180 deg) of two lines (P1L1(x1, y1), P2L1(x2, y2) and P1L1(x1, y1), P2L3(x2, y3)) 
points_to_innerangle <- function(x1, y1, x2, y2, x3, y3) {
  # https://stackoverflow.com/questions/2946327/inner-angle-between-two-lines
  dx21 = x2-x1
  dx31 = x3-x1
  dy21 = y2-y1
  dy31 = y3-y1
  m12 = sqrt( dx21*dx21 + dy21*dy21 )
  m13 = sqrt( dx31*dx31 + dy31*dy31 )
  rad = acos( (dx21*dx31 + dy21*dy31) / (m12 * m13) )
  deg = rad * 180.0 / pi
  deg
} 
# points_to_angle(x1 = 215, y1 = 294, x2 = 174, y2 = 228, x3 = 303, y3 = 294) # should give rad = 2.12667   deg = 121.849
# points_to_angle(x1 = 153, y1 = 457, x2 = 19, y2 = 457, x3 = 15, y3 = 470) # should give rad = 0.0939257 deg = 5.38155


# Function that converts list to nested data frame
list_to_nested <- function(list, group_var){
  list %>%
    do.call(dplyr::bind_rows, .) %>%
    dplyr::group_by( {{ group_var }} ) %>%
    tidyr::nest()
}


# Individual growth
plot_growth_state_id <- function(data, cond, gamestate, trial_lim = 20) {
  gamestate_lab <- dplyr::recode(gamestate, !!!gamestate_lab)
  data %>%
    dplyr::filter(condition == cond, newGameState_f == gamestate) %>%
    group_by(id) %>%
    dplyr::mutate(trial = dplyr::row_number()) %>%
    dplyr::filter(trial < trial_lim) %>%  
    ggplot(aes(x = trial, y = score)) +
    geom_line() +
    facet_wrap(~id) +
    ylim(0, 40) +
    scale_x_continuous(breaks = 1:14) +
    ggtitle(paste0("Condition: ", cond, ", ", "Game state: ", gamestate_lab)) 
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
plot_growth_state_loess <- function(data, cond, gamestate, trial_lim = 20, method = "loess", grouping = NULL) {
  gamestate_lab <- dplyr::recode(gamestate, !!!gamestate_lab)
  if(!is.null(grouping)) {
  facet_formula <- as.formula(paste("~", grouping))  
  }
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
    ylim(0, 40) +
    {if(!is.null(grouping))
      facet_wrap(facet_formula, nrow = 2, labeller = "label_both")
    } +
    scale_x_continuous(breaks = 1:14) +
    ggtitle(paste0("Condition: ", cond, ", ", "Game state: ", gamestate_lab)) +
    labs(color = "id") + 
    theme_classic() + 
    theme(legend.position = "none")
} # plot_growth_state_loess(recomputed_df, "bot", 9, grouping = "order") 

# e.e.
# full_df %>%
# dplyr::filter(condition == "bot", newGameState_f == 15) %>%
# group_by(id) %>%
# dplyr::mutate(trial = dplyr::row_number()) %>%
# ggplot(aes(x = trial, y = eucl_dist)) +
# geom_line(aes(color = as.factor(id)), alpha = .5) +
# geom_smooth(method = "loess", formula = "y ~ x")

plot_growth_state916_id <- function(data, conds = c("social", "bot"), trial_lim = 20, method = NULL) {
  gamestate_lab <- dplyr::recode(c(9, 16), !!!gamestate_lab)
  if(is.null(method)) {
    data %>%
      dplyr::filter(condition %in% conds, newGameState_f %in% c(9, 16)) %>%
      dplyr::mutate(newGameState_f = as.factor(newGameState_f)) %>%  
      dplyr::group_by(id, condition) %>%
      dplyr::mutate(trial = dplyr::row_number()) %>%
      dplyr::filter(trial < trial_lim) %>% 
      ggplot(aes(x = trial, y = score, color = condition)) +
      geom_line() +
      geom_point(aes(shape = newGameState_f)) + 
      ylim(0, 40) +
      scale_x_continuous(breaks = 1:14) +
      # ggtitle("Alternating GameState 9 and 16") +
      ggtitle(paste("Alternating GameState", gamestate_lab[1], "and", gamestate_lab[2])) +
      theme(legend.position = "right") +
      theme_bw() +
      facet_wrap(~id) 
  } else if(method == "lm") {
    data %>%
      dplyr::filter(condition %in% conds, newGameState_f %in% c(9, 16)) %>%
      dplyr::mutate(newGameState_f = as.factor(newGameState_f)) %>%  
      dplyr::group_by(id, condition) %>%
      dplyr::mutate(trial = dplyr::row_number()) %>%
      dplyr::filter(trial < trial_lim) %>% 
      ggplot(aes(x = trial, y = score, color = condition)) +
      geom_smooth(method = "lm", formula = "y ~ x", aes(color = condition), se = FALSE) +
      geom_line(alpha = 0.15) +
      geom_point(aes(shape = newGameState_f), alpha = 0.15) +
      ylim(0, 40) +
      scale_x_continuous(breaks = 1:14) +
      # ggtitle("Alternating GameState 9 and 16") + 
      ggtitle(paste("Alternating GameState", gamestate_lab[1], "and", gamestate_lab[2])) +
      theme(legend.position = "right") +
      facet_wrap(~id)    
  } 
}  
# plot_growth_state916_id(recomputed_df, method = "lm") + theme_classic()

# Within stats & plot for publication quality
my_ggwithinstats2 <- function(data, title = NULL, x, y, gamestate, trial_lim = 20, outlier.label, xlab, ylab, 
                              outlier.tagging = FALSE, results.subtitle = TRUE, 
                              centrality.label.args = TRUE, point.path = TRUE,
                              type = "parametric", 
                              ...) {  # ... for limits and breaks
  
  gamestate_lab <- dplyr::recode(gamestate, !!!gamestate_lab)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  outlier.label <- rlang::enquo(outlier.label)
  
  if(is.null(title)) {
    title <- paste0("Game state: ", gamestate_lab)
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
    ylim(0, 40) +
    scale_y_continuous(...)
}
# my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 10,
#                   time_lim = 20, xlab = "Condition", ylab = "score")



# Grouped between stats & plot for publication quality
my_grouped_ggbetweenstats2 <- function(data, title = NULL, x, y, group, gamestate, trial_lim = 20, outlier.label, xlab, ylab, 
                                       outlier.tagging = FALSE, results.subtitle = TRUE, 
                                       centrality.label.args = TRUE, point.path = TRUE,
                                       type = "parametric", 
                                       ...) {  # ... for limits and breaks
  
  gamestate_lab <- dplyr::recode(gamestate, !!!gamestate_lab)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)
  outlier.label <- rlang::enquo(outlier.label)
  
  if(is.null(title)) {
    title <- paste0("Game state: ", gamestate_lab)
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
        scale_y_continuous(limits = c(0, 30)),
        theme(
          plot.title = element_text(hjust = 0, size = 16),
          plot.subtitle = element_text(hjust = 0, size = 12),
          plot.caption = element_text(hjust = 0, size = 12),
          text = element_text(size = 14)
        ))
    ) 
}
# e.g.
# my_grouped_ggbetweenstats2(data = recomputed_df, x = gender, y = score, group = condition, outlier.label = id, gamestate = 9, time_lim = 20)



# Grouped between stats & plot for publication quality
my_grouped_ggwithinstats2 <- function(data, title = NULL, x, y, group, gamestate, trial_lim = 20, outlier.label, xlab, ylab, 
                                       outlier.tagging = FALSE, results.subtitle = TRUE, 
                                       centrality.label.args = TRUE, point.path = TRUE,
                                       type = "parametric", 
                                       ...) {  # ... for limits and breaks
  
  gamestate_lab <- dplyr::recode(gamestate, !!!gamestate_lab)
  x <- rlang::enquo(x)
  y <- rlang::enquo(y)
  group <- rlang::enquo(group)
  outlier.label <- rlang::enquo(outlier.label)
  
  if(is.null(title)) {
    title <- paste0("Game state: ", gamestate_lab)
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
    ggstatsplot::grouped_ggwithinstats(
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
        scale_y_continuous(limits = c(0, 30)),
        theme(
          plot.title = element_text(hjust = 0, size = 16),
          plot.subtitle = element_text(hjust = 0, size = 12),
          plot.caption = element_text(hjust = 0, size = 12),
          text = element_text(size = 14)
        ))
    ) 
}
# e.g.
# my_grouped_ggwithinstats2(recomputed_df, x = condition, y = score, group = gender, outlier.label = id, gamestate = 9, time_lim = 20)



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

# Fast save to png
fast_png <- function(plot, path = NULL,
                     width = 5, height = 5, units = "in", res = 300, ...) {
  plot_name <- deparse(substitute(plot))
  png(filename = file.path(path, paste0(plot_name, ".png")), 
      width = width, height = height, units = units, res = res                     
  )
  plot(plot)
  dev.off()
}  

##################################################################################################################
# Read participant table
##################################################################################################################

# part_data_list <- readRDS("data_clean_output_2022.09.01.RDS")
part_data_list <- readRDS(part_data_rds)
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

# Check that all output_file in participant table have corresponding file in the read .RDS files
if(all(is.element(full_part_table_df$output_file_clean, sessions_data_df$file))) print("Check succesful")

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

demoimit_var_names <-
  c("playerPosition_x_demoimit", "playerPosition_y_demoimit", "playerPosition_z_demoimit",
  "markerPosition_x_demoimit", "markerPosition_y_demoimit", "markerPosition_z_demoimit",
  "distanceFromMarker_demoimit", "playerType_demoimit",
  "markerType_demoimit", "score_demoimit")


full_recomputed_df <- 
  full_df %>%
  dplyr::mutate(return = as.logical(return)) %>%
  dplyr::mutate(cond_ord = 
    dplyr::case_when(condition == "social" & order == "1.0" ~ "Social First",
                     condition == "social" & order == "2.0" ~ "Bot First",
                     condition == "bot" & order == "1.0" ~ "Bot First",
                     condition == "bot" & order == "2.0" ~ "Social First"
                     )                
  ) %>%
  dplyr::group_by(output_file, newGameState_cycl) %>%      
  tidyr::fill(
    dplyr::all_of(c(            # dont fill the demo metrics, only the return
      "return"
    )),
    .direction = "updown"
  ) %>%
  dplyr::filter(!is.na(timeStamp_demoimit) |         # matching frame of metrics (matches on return = True if there is one)
                dplyr::row_number() == dplyr::n()    # last frame of gameState
  ) %>%                                              # READ BELOW WHY THESE FILTERS ARE CORRECT
  dplyr::ungroup() %>%
  dplyr::group_by(output_file) %>%
  dplyr::mutate(next_frame_metric = dplyr::lead(frame) - frame == 1) %>%     # tag TRUE frames in order (last frame of gameState + first frame of gameState has Metrics)
  dplyr::mutate(
    playerPosition_x_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(playerPosition_x_demoimit), playerPosition_x_demoimit),
    playerPosition_y_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(playerPosition_y_demoimit), playerPosition_y_demoimit),
    playerPosition_z_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(playerPosition_z_demoimit), playerPosition_z_demoimit),
    markerPosition_x_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(markerPosition_x_demoimit), markerPosition_x_demoimit),
    markerPosition_y_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(markerPosition_y_demoimit), markerPosition_y_demoimit),
    markerPosition_z_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(markerPosition_z_demoimit), markerPosition_z_demoimit),
    distanceFromMarker_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(distanceFromMarker_demoimit), distanceFromMarker_demoimit),
    playerType_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(playerType_demoimit), playerType_demoimit),
    markerType_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(markerType_demoimit), markerType_demoimit),
    score_demoimit = dplyr::if_else(next_frame_metric, dplyr::lead(score_demoimit), score_demoimit),
  ) %>%
  dplyr::group_by(output_file, newGameState_cycl) %>%
  dplyr::filter(dplyr::row_number() == dplyr::n()) %>%    # after this, the last frame in group will be the correct one
  dplyr::ungroup() %>%
  dplyr::filter(yPos < 1) %>%                        # this is the same as who == playerType_demoimit
  dplyr::mutate(markerType_demoimit = dplyr::recode(markerType_demoimit, !!!marker_def_vef)) %>%
  dplyr::mutate(eucl_dist = eucl_dist(x1 = playerPosition_x_demoimit, x2 = Marker_x, y1 = playerPosition_z_demoimit, y2 = Marker_z)) %>%
  dplyr::mutate(newGameState_lab = dplyr::recode(as.character(newGameState_f), !!!gamestate_lab))


check_recomputed <- 
  full_recomputed_df %>%
    dplyr::select("output_file", "frame", "newGameState_f", "return", "next_frame_metric",
                  "who", "playerType_demoimit",
                  "xPos", "playerPosition_x_demoimit",
                  "zPos", "playerPosition_z_demoimit",
                  "yPos", "playerPosition_y_demoimit",
                  "Marker_x", "markerPosition_x_demoimit",
                  "Marker_z", "markerPosition_z_demoimit",
                  "Marker_y", "markerPosition_y_demoimit",
                  "eucl_dist", "distanceFromMarker_demoimit",
                  "Marker_type", "markerType_demoimit"
    ) %>%
  dplyr::filter(!is.na(playerPosition_x_demoimit) | !is.na(playerPosition_y_demoimit))


# distanceFromMarker_demoimit == eucl_dist except in gameStates 14 & 15
check_recomputed %>%
  dplyr::filter(!newGameState_f %in% c(14, 15)) %>%
  dplyr::mutate(check = abs(distanceFromMarker_demoimit - eucl_dist) < 0.02) %>%
  dplyr::pull(check) %>%
  all()


# all markerType_demoimit == Marker_type, except for Gamestate 14/15 where Start/Flag are considered Objects in demoimit -- use Marker_type 
check_recomputed %>%
  dplyr::filter(!newGameState_f %in% c(14, 15)) %>%
  dplyr::mutate(check = markerType_demoimit == Marker_type) %>%
  dplyr::pull(check) %>%
  all()


# Player Pos x, z from frames and Player pos x, z from demoimit are equal for gameStates excluding 17 that stores position for 'other'
check_recomputed %>%
  dplyr::filter(newGameState_f != 17) %>%
  dplyr::mutate(
    check_x = abs(playerPosition_x_demoimit - xPos) < 0.01,
    check_z = abs(playerPosition_z_demoimit - zPos) < 0.01,
    check_both = check_x + check_z == 2) %>%
  dplyr::pull(check_both) %>%
  all()


# Data for Analyses
recomputed_df <- 
  full_recomputed_df %>%
  dplyr::rename(score = eucl_dist) %>%
  dplyr::mutate(gender = as.factor(gender)) %>%
  dplyr::filter(!is.na(playerPosition_x_demoimit) | !is.na(playerPosition_y_demoimit))

##############
# # Important checks
# full_lframe_df <- 
#   full_df %>%
#   dplyr::group_by(output_file, newGameState_cycl) %>%      
#   tidyr::fill(
#     dplyr::all_of(c(
#       "playerPosition_x_demoimit", "playerPosition_y_demoimit", "playerPosition_z_demoimit",
#       "markerPosition_x_demoimit", "markerPosition_y_demoimit", "markerPosition_z_demoimit",
#       "distanceFromMarker_demoimit", "playerType_demoimit",
#       "markerType_demoimit", "score_demoimit"
#     )),
#     .direction = "updown"
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(markerType_demoimit = dplyr::recode(markerType_demoimit, !!!marker_def_vef)) %>%
#   dplyr::mutate(eucl_dist = eucl_dist(x1 = playerPosition_x_demoimit, x2 = playerPosition_z_demoimit, y1 = Marker_x, y2 = Marker_z)) %>%
#   dplyr::filter(!is.na(timeStamp_demoimit))    
# 
# 
# # check0 = Metrics timeStamp matches the frame with return = True, but when there is no return = True in gameState, it matches last frame of previous gameState
# full_df %>%
#   dplyr::filter(!is.na(timeStamp_demoimit)) %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "return", "xPos", "playerPosition_x_demoimit", "zPos", "playerPosition_z_demoimit")
# 
# 
# # check1 = gameStates with flag have Marker Type and Position as Object
# full_lframe_df %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "Marker_type", "markerType_demoimit") %>%
#   print(n = 50)
# 
# # check2 = when (1) yPos > 1 (8 means player is at high ObservationPoint) or newGameState_f == 18 (it's the other player) then Pos != playerPosition 
# full_lframe_df %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "yPos", 
#                 "xPos", "playerPosition_x_demoimit",
#                 "zPos", "playerPosition_z_demoimit") %>%
#   print(n = 50)
# 
# # check3 = playerType_demoimit & who - basically when playerType_demoimit == who => yPos < 1
# full_lframe_df %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "yPos", "who", "playerType_demoimit") %>%
#   print(n = 50)
# 
# all((full_lframe_df$yPos > 1) == (full_lframe_df$who != full_lframe_df$playerType_demoimit))
# all((full_lframe_df$yPos < 1) == (full_lframe_df$who == full_lframe_df$playerType_demoimit))
# 
# # after check1 & check 2 => use Marker_type, Marker_x, Marker_z from recomputed AND playerPosition_x_demoimit, playerPosition_z_demoimit from Metrics 
# # but only for frames in !is.na(timeStamp_demoimit) & (yPos < 1); where (yPos < 1) is equivalent to playerType_demoimit == who 
# 
# 
# # check 4 = all markerType_demoimit == Marker_type, except for Gamestate 14/15 where Start/Flag are considered Objects in demoimit -- use Marker_type 
# test4 <- full_recomputed_df %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "markerType_demoimit", "Marker_type") %>%
#   dplyr::mutate(check = markerType_demoimit == Marker_type) %>%
#   dplyr::filter(!newGameState_f %in% c(14, 15))
# all(test4$check)
#   
# # check 5 = distanceFromMarker_demoimit == eucl_dist
# test5 <- full_recomputed_df %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "distanceFromMarker_demoimit", "eucl_dist") %>%
#   dplyr::mutate(check = abs(distanceFromMarker_demoimit - eucl_dist) < 0.01) %>%
#   dplyr::filter(!newGameState_f %in% c(14, 15))
# sum(!test5$check)
# 
# # check 6 = Player x, z from frames and Player x, z from demoimit are equal for gameStates excluding 18 that stores position for 'other'
# test6 <- full_recomputed_df %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "xPos", "playerPosition_x_demoimit", "zPos", "playerPosition_z_demoimit") %>%
#   dplyr::mutate(
#                 check_x = abs(playerPosition_x_demoimit - xPos) < 0.01,
#                 check_z = abs(playerPosition_z_demoimit - zPos) < 0.01,
#                 check_both = check_x + check_z) %>%
#   dplyr::filter(newGameState_f != 18)
# sum(test6$check_both < 2)       # this is the total no of frames that have missmatching x & z
# 
# 
# # check 7 =
# test7 <- full_recomputed_df %>%
#   dplyr::select("output_file", "frame", "newGameState_f", "Marker_x", "markerPosition_x_demoimit", "Marker_z", "markerPosition_z_demoimit") %>%
#   dplyr::mutate(
#     check_x = abs(markerPosition_x_demoimit - Marker_x) < 0.01,
#     check_z = abs(markerPosition_z_demoimit - Marker_z) < 0.01,
#     check_both = check_x + check_z) %>%
#   dplyr::filter(!newGameState_f %in% c(14, 15))
# sum(test7$check_both < 2)

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
# Exclude Outliers (nooutlier)
# only for RO, R1O, S (R2O can be quick confirm of R1O)
##################################################################################################################
recomputed_df2 <-
  recomputed_df %>%
  dplyr::group_by(id, condition) %>%                      # grouping is not necesary but to be safe
  dplyr::mutate(timeStamp_RT = timeStamp_f - lag(timeStamp_f)) %>%
  dplyr::filter(!(newGameState_f %in% c(9, 16, 17) & (timeStamp_RT <= 2 & !is.na(timeStamp_RT)) )) %>%    
  dplyr::ungroup() 
  
# recomputed_df_backup <- recomputed_df
# recomputed_df <- recomputed_df2


##################################################################################################################
# Sample characteristics 
##################################################################################################################

# Sample count
unique(full_part_table_df$id) %>% length()  # total participant unique ids
unique(part_table_df$id) %>% length()       # have complete data with no issues

unique(full_part_table_df$id) %>% length() - unique(part_table_df$id) %>% length()   # excluded



# In New Data some age are "?"
part_table_df <- 
  part_table_df %>%
  dplyr::mutate(age = na_if(age, "?"),         
                age = as.numeric(age))   


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
# fast_ggsave(age_plot, path = graphs_out_folder, width = 10, height = 5)

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


# 4. Social: Partner's gender
partner_df <- 
  part_table_df %>%
  dplyr::select(id, condition, order, partner, bot_recording, output_file, gender)

partner_soc_full_df <- 
  full_part_table_df %>%               # pull full table for this to match bot_recordings
  dplyr::select(id, condition, order, partner, bot_recording, output_file, gender) %>%
  dplyr::filter(condition == "social") 

# Social
partner_soc_df <- 
  partner_df %>%
  dplyr::filter(condition == "social")

partner_soc_df <-
  partner_soc_df %>%
  dplyr::left_join(partner_soc_full_df, by = c("partner" = "id"), keep = TRUE)

partner_dyad_soc_df <-
  partner_soc_df %>%
  dplyr::select(id.x, partner.x, gender.x, id.y, partner.y, gender.y) %>%
  dplyr::mutate(
    dyad_name = dplyr::case_when(as.numeric(id.x) < as.numeric(id.y) ~ paste(id.x, " - ", id.y),
                                   as.numeric(id.x) > as.numeric(id.y) ~ paste(id.y, " - ", id.x),
                                   TRUE ~ "not_matched"),
    gender_mix = dplyr::case_when(gender.x == gender.y & gender.x == "F" & gender.y == "F" ~ "both Female",
                                  gender.x == gender.y & gender.x == "M" & gender.y == "M" ~ "both Male",
                                  gender.x != gender.y ~ "gender mix", 
                                  is.na(gender.x) | is.na(gender.y) ~ "?",
                                  TRUE ~ "?")
  )

partner_dyad_soc_df %>%
  dplyr::count(dyad_name, gender_mix) %>%
  dplyr::count(gender_mix) %>%
  kableExtra::kbl(caption = "Social - dyad gender", digits = 2) %>%
  kableExtra::kable_classic(full_width = F, html_font = "Times New Roman")


# Bot
partner_bot_df <- 
  partner_df %>%
  dplyr::filter(condition == "bot") %>%
  dplyr::select(-partner)

partner_bot_instructor_df <-
  dplyr::tibble(
    bot_recording = c("TestSession_22.10.2021_14.19_DESKTOP-PJTKC0O_PLAYER_1_True_.botrec",
                      "TestSession_22.10.2021_14.19_ROG_LAPTOP_PLAYER_2_False_.botrec",
                      "TestSession_06.05.2022_11.07_DESKTOP-PJTKC0O_PLAYER_1_False_.botrec",
                      "TestSession_06.05.2022_11.07_ROG_LAPTOP_PLAYER_2_True_.botrec"),
    instructor = c("Sofi",
                   "Beni",
                   "Stefan",
                   "Andrei"),
    gender_instruc = c("F",
               "M",
               "M",
               "M")
  )  

partner_bot_matched_df <-            # HERE
  partner_bot_df %>%
  dplyr::left_join(partner_soc_full_df, by = c("bot_recording" = "output_file"), keep = TRUE) %>%
  dplyr::left_join(partner_bot_instructor_df, by = c("bot_recording.x" = "bot_recording")) %>%
  dplyr::mutate(gender.y = dplyr::coalesce(gender.y, gender_instruc))

partner_dyad_bot_df <-
  partner_bot_matched_df %>%
  dplyr::select(id.x, gender.x, id.y, gender.y, instructor) %>%
  dplyr::mutate(
    dyad_name = dplyr::case_when(as.numeric(id.x) < as.numeric(id.y) ~ paste(id.x, " - ", id.y),
                                 as.numeric(id.x) > as.numeric(id.y) ~ paste(id.y, " - ", id.x),
                                 TRUE ~ "not_matched"),
    gender_mix = dplyr::case_when(gender.x == gender.y & gender.x == "F" & gender.y == "F" ~ "both Female",
                                  gender.x == gender.y & gender.x == "M" & gender.y == "M" ~ "both Male",
                                  gender.x != gender.y ~ "gender mix", 
                                  is.na(gender.x) | is.na(gender.y) ~ "?",
                                  TRUE ~ "?")
  )

partner_dyad_bot_df %>%
  dplyr::count(gender_mix) %>%
  kableExtra::kbl(caption = "Bot - dyad gender", digits = 2) %>%
  kableExtra::kable_classic(full_width = F, html_font = "Times New Roman")

##################################################################################################################


##################################################################################################################
# Analyses on recomputed Eucl Dist (based on x, z from Metrics & Marker, real Flag positions)
##################################################################################################################


# Sample trends by gameState
growth_9 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 9) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 9) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_9, path = graphs_out_folder)

growth_14 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 14) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 14) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_14, path = graphs_out_folder)

growth_15 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 15) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 15) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_15, path = graphs_out_folder)

growth_16 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 16) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 16) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_16, path = graphs_out_folder)

growth_17 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 17) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 17) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_17, path = graphs_out_folder)

growth_18 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 18) + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 18) + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_18, path = graphs_out_folder)



# Sample trends by gameState and by order
growth_ord_9 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 9, grouping = "order") + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 9, grouping = "order") + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_ord_9, path = graphs_out_folder, height = 10)

growth_ord_14 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 14, grouping = "order") + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 14, grouping = "order") + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_ord_14, path = graphs_out_folder, height = 10)

growth_ord_15 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 15, grouping = "order") + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 15, grouping = "order") + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_ord_15, path = graphs_out_folder, height = 10)

growth_ord_16 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 16, grouping = "order") + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 16, grouping = "order") + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_ord_16, path = graphs_out_folder, height = 10)

growth_ord_17 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 17, grouping = "order") + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 17, grouping = "order") + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_ord_17, path = graphs_out_folder, height = 10)

growth_ord_18 <- 
  cowplot::plot_grid(
    plot_growth_state_loess(recomputed_df, "bot", 18, grouping = "order") + theme_classic() + theme(legend.position="none") ,
    plot_growth_state_loess(recomputed_df, "social", 18, grouping = "order") + theme_classic() + theme(legend.position="none"),
    nrow = 2
  )
# fast_ggsave(growth_ord_18, path = graphs_out_folder, height = 10)



# Alternating GameStates 9 and 16 for each participant by condition
growth_916 <- 
  recomputed_df %>%
  plot_growth_state916_id() + theme_classic()
# fast_ggsave(growth_916, path = graphs_out_folder, width = 14, height = 10)

growth_916_slope <-
  recomputed_df %>%
  plot_growth_state916_id(method = "lm") + theme_classic()
# fast_ggsave(growth_916_slope, path = graphs_out_folder, width = 14, height = 10)

growth_916_slope_10trial <-
  recomputed_df %>%
  plot_growth_state916_id(method = "lm", trial_lim = 10) + theme_classic()
# fast_ggsave(growth_916_slope_10trial, path = graphs_out_folder, width = 14, height = 10)


growth_916_slope_condord <-
  ggpubr::ggarrange(
    recomputed_df %>%
      dplyr::filter(cond_ord == "Social First") %>%
      plot_growth_state916_id(method = "lm") + 
      labs(subtitle = "Social First") +
      theme_classic(),
    recomputed_df %>%
      dplyr::filter(cond_ord == "Bot First") %>%
      plot_growth_state916_id(method = "lm") + 
      labs(subtitle = "Bot First") +
      theme_classic(),
    common.legend = TRUE
  )
# fast_ggsave(growth_916_slope_condord, path = graphs_out_folder, width = 18, height = 12)


# t test of slopes between conditions
indiv_slope_df <- 
  recomputed_df %>%
    dplyr::filter(newGameState_f %in% c(9, 16)) %>%
    dplyr::mutate(newGameState_f = as.factor(newGameState_f)) %>%  
    dplyr::group_by(id, condition) %>%
    dplyr::mutate(trial = dplyr::row_number()) %>%
    # dplyr::filter(trial < 8) %>%                       # add trial_lim?
    dplyr::ungroup() %>%
    dplyr::group_nest(output_file, id, condition, cond_ord, order) %>%  # break out the data by this columns using dplyr::group_nest()
    dplyr::mutate(model = map(data, ~ lm(score ~ trial, data = .x))) %>%   # the purrr::map() function can create individual models for each id
    dplyr::mutate(coef = map(model, broom::tidy)) %>%                             # collect the coefficients for each of these models, use broom::tidy() 
    tidyr::unnest(cols = c(coef)) %>%                                         # to convert them to a consistent data frame format so that they can be unnested
    dplyr::filter(term == "trial") 
# rio::export(indiv_slope_df, file = file.path(graphs_out_folder, "ROR1O_indiv_slopes.xlsx"))
# rio::export(indiv_slope_df, file = file.path(graphs_out_folder, "ROR1O_indiv_slopes_8triallim.xlsx"))

indiv_slope_ttest <- 
  indiv_slope_df %>%
    ggstatsplot::ggbetweenstats(
      x = condition,
      y = estimate,
      xlab = "Condition",
      ylab = "slope estimate"
    )
# fast_ggsave(indiv_slope_ttest, path = graphs_out_folder)


indiv_slope_ttest_condord1 <-            # indiv_slope_ttest_condord1 makes more sense than indiv_slope_ttest_condord2
  indiv_slope_df %>%
  ggstatsplot::grouped_ggbetweenstats(
    x = condition,
    y = estimate,
    grouping.var = order,
    xlab = "Condition",
    ylab = "slope estimate"
  )
# fast_png(indiv_slope_ttest_condord1, path = graphs_out_folder, width = 14, height = 10)

indiv_slope_ttest_condord2 <- 
  indiv_slope_df %>%
    ggstatsplot::grouped_ggbetweenstats(
      x = condition,
      y = estimate,
      grouping.var = cond_ord,
      xlab = "Condition",
      ylab = "slope estimate"
    )
# fast_png(indiv_slope_ttest_condord2, path = graphs_out_folder, width = 14, height = 10)





# Individual average scores (mean) - social vs bot
botsoc_9 <- 
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 9,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_9, path = graphs_out_folder, width = 8, height = 8)

botsoc_14<- 
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 14,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_14, path = graphs_out_folder, width = 8, height = 8)

botsoc_15 <- 
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 15,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_15, path = graphs_out_folder, width = 8, height = 8)

botsoc_16 <- 
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 16,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_16, path = graphs_out_folder, width = 8, height = 8)

botsoc_17 <- 
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 17,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_17, path = graphs_out_folder, width = 8, height = 8)

botsoc_18 <-
  my_ggwithinstats2(recomputed_df, x = condition, y = score, outlier.label = id, gamestate = 18,
                    trial_lim = 20, xlab = "Condition", ylab = "score") %>%
  suppressMessages()
# fast_ggsave(botsoc_18, path = graphs_out_folder, width = 8, height = 8)



# First Social vs First Bot
botsoc_condord_9 <-
  recomputed_df %>% 
  my_grouped_ggbetweenstats2(x = condition, y = score, group = cond_ord, outlier.label = id, gamestate = 9, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_condord_9, path = graphs_out_folder, width = 14, height = 10)

botsoc_condord_14 <-
  recomputed_df %>% 
  my_grouped_ggbetweenstats2(x = condition, y = score, group = cond_ord, outlier.label = id, gamestate = 14, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_condord_14, path = graphs_out_folder, width = 14, height = 10)

botsoc_condord_15 <-
  recomputed_df %>% 
  my_grouped_ggbetweenstats2(x = condition, y = score, group = cond_ord, outlier.label = id, gamestate = 15, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_condord_15, path = graphs_out_folder, width = 14, height = 10)

botsoc_condord_16 <-
  recomputed_df %>% 
  my_grouped_ggbetweenstats2(x = condition, y = score, group = cond_ord, outlier.label = id, gamestate = 16, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_condord_16, path = graphs_out_folder, width = 14, height = 10)

botsoc_condord_17 <-
  recomputed_df %>% 
  my_grouped_ggbetweenstats2(x = condition, y = score, group = cond_ord, outlier.label = id, gamestate = 17, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_condord_17, path = graphs_out_folder, width = 14, height = 10)

botsoc_condord_18 <-
  recomputed_df %>% 
  my_grouped_ggbetweenstats2(x = condition, y = score, group = cond_ord, outlier.label = id, gamestate = 18, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_condord_18, path = graphs_out_folder, width = 14, height = 10)


###############
# #  Male vs Female
# gender_9 <- 
#   recomputed_df %>% 
#   my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 9, time_lim = 20)
# # fast_ggsave(gender_9, path = graphs_out_folder, width = 14, height = 10)  
# 
# gender_14 <- 
#   recomputed_df %>% 
#   my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 14, time_lim = 20)
# # fast_ggsave(gender_14, path = graphs_out_folder, width = 14, height = 10)
# 
# gender_15 <- 
#   recomputed_df %>% 
#   my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 15, time_lim = 20)
# # fast_ggsave(gender_15, path = graphs_out_folder, width = 14, height = 10)
# 
# gender_16 <- 
#   recomputed_df %>% 
#   my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 16, time_lim = 20)
# # fast_ggsave(gender_16, path = graphs_out_folder, width = 14, height = 10)
# 
# gender_17 <- 
#   recomputed_df %>% 
#   my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 17, time_lim = 20)
# # fast_ggsave(gender_17, path = graphs_out_folder, width = 14, height = 10)
# 
# gender_18 <- 
#   recomputed_df %>%
#   my_grouped_ggbetweenstats2(x = gender, y = score, group = condition, outlier.label = id, gamestate = 18, time_lim = 20)
# # fast_ggsave(gender_18, path = graphs_out_folder, width = 14, height = 10)  
#################

# Bot vs Social by gender
botsoc_gender_9 <-
  recomputed_df %>%
  my_grouped_ggbetweenstats2(x = condition, y = score, group = gender, outlier.label = id, gamestate = 9, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_gender_9, path = graphs_out_folder, width = 14, height = 10)

botsoc_gender_14 <-
  recomputed_df %>%
  my_grouped_ggbetweenstats2(x = condition, y = score, group = gender, outlier.label = id, gamestate = 14, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_gender_14, path = graphs_out_folder, width = 14, height = 10)

botsoc_gender_15 <-
  recomputed_df %>%
  my_grouped_ggbetweenstats2(x = condition, y = score, group = gender, outlier.label = id, gamestate = 15, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_gender_15, path = graphs_out_folder, width = 14, height = 10)

botsoc_gender_16 <-
  recomputed_df %>%
  my_grouped_ggbetweenstats2(x = condition, y = score, group = gender, outlier.label = id, gamestate = 16, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_gender_16, path = graphs_out_folder, width = 14, height = 10)

botsoc_gender_17 <-
  recomputed_df %>%
  my_grouped_ggbetweenstats2(x = condition, y = score, group = gender, outlier.label = id, gamestate = 17, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_gender_17, path = graphs_out_folder, width = 14, height = 10)

botsoc_gender_18 <-
  recomputed_df %>%
  my_grouped_ggbetweenstats2(x = condition, y = score, group = gender, outlier.label = id, gamestate = 18, time_lim = 20, centrality.label.args = FALSE)
# fast_png(botsoc_gender_18, path = graphs_out_folder, width = 14, height = 10)


###########################
# LMM
library(merTools)
library(lmerTest)
library(performance)
library(sjPlot)   
library(redres) # devtools::install_github("goodekat/redres")
library(cowplot)

# Random intercept with fixed mean -- (1 | group)
lmm_mod_1 <-
  recomputed_df %>% 
  dplyr::filter(newGameState_f %in% c(9)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + condition * order + (1 | id), data = .) %>%
  lmerTest::as_lmerModLmerTest()

lmerTest:::summary.lmerModLmerTest(lmm_mod_1)

# png(filename = file.path(graphs_out_folder, "plot_lmm_mod_1.png"), width = 12, height = 14, units = "in", res = 300)
  performance::check_model(lmm_mod_1, theme = ggplot2::theme_bw())    # diagnostic plots
# dev.off()

report::report(lmm_mod_1)

merTools::predictInterval(lmm_mod_1)                   # for various model predictions, possibly with new data
merTools::FEsim(lmm_mod_1)                             # mean, median and sd of the fixed effect estimates
merTools::REsim(lmm_mod_1)                             # mean, median and sd of the random effect estimates

plot_lmm_mod_1_eff <- 
  cowplot::plot_grid(
    merTools::plotFEsim(merTools::FEsim(lmm_mod_1)) + ggtitle("Fixed Effects"),
    merTools::plotREsim(merTools::REsim(lmm_mod_1)) + ggtitle("Random Effects") 
  )
# fast_ggsave(plot_lmm_mod_1_eff, path = graphs_out_folder, width = 10, height = 5) 




lmm_mod_gender <-          # condition x gender interaction - just a trend
  recomputed_df %>% 
  dplyr::filter(newGameState_f %in% c(9)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + order + condition * gender + (1 | id), data = .) %>%
  lmerTest::as_lmerModLmerTest()

lmerTest:::summary.lmerModLmerTest(lmm_mod_gender)
report::report(lmm_mod_gender)

lmm_interac_plot1 <- sjPlot::plot_model(lmm_mod_gender, type = "int", terms = c("condition", "gender"), ci.lvl = 0.95) + 
  ggtitle("Interaction Plot", subtitle = "Predicted values") +
  theme_bw()
# fast_ggsave(lmm_interac_plot1, path = graphs_out_folder)



# Correlated random slope and intercept   -- x + (x | g) 
lmm_mod_2 <-
  recomputed_df %>% 
  dplyr::filter(newGameState_f %in% c(9, 16)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + condition * order + gender + (1 + trial | id), data = .) %>%
  lmerTest::as_lmerModLmerTest()

lmerTest:::summary.lmerModLmerTest(lmm_mod_2)

# png(filename = file.path(graphs_out_folder, "plot_lmm_mod_2.png"), width = 12, height = 14, units = "in", res = 300)
performance::check_model(lmm_mod_2, theme = ggplot2::theme_bw())    # diagnostic plots
# dev.off()

report::report(lmm_mod_2)

merTools::predictInterval(lmm_mod_2)                   # for various model predictions, possibly with new data
merTools::FEsim(lmm_mod_2)                             # mean, median and sd of the fixed effect estimates
merTools::REsim(lmm_mod_2)                             # mean, median and sd of the random effect estimates

plot_lmm_mod_2_eff <- 
  cowplot::plot_grid(
    merTools::plotFEsim(merTools::FEsim(lmm_mod_2)) + ggtitle("Fixed Effects"),
    merTools::plotREsim(merTools::REsim(lmm_mod_2)) + ggtitle("Random Effects") 
  )
# fast_ggsave(plot_lmm_mod_2_eff, path = graphs_out_folder, width = 10, height = 5) 

plot_lmm_mod_2_interac <- sjPlot::plot_model(lmm_mod_2, type = "int", terms = c("condition", "order"), ci.lvl = 0.95) + 
  ggtitle("Interaction Plot", subtitle = "Predicted values") +
  theme_bw()
# fast_ggsave(plot_lmm_mod_2_interac, path = graphs_out_folder)



# Uncorrelated random slope and intercept -- x + (x || g)
# THIS IS WORSE THAN lmm_mod_2
lmm_mod_3 <-
  recomputed_df %>% 
  dplyr::filter(newGameState_f %in% c(9, 16)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + condition * order + gender + (1 + trial || id), data = .) %>%
  lmerTest::as_lmerModLmerTest()

lmerTest:::summary.lmerModLmerTest(lmm_mod_3)

# png(filename = file.path(graphs_out_folder, "plot_lmm_mod_3.png"), width = 12, height = 14, units = "in", res = 300)
performance::check_model(lmm_mod_3, theme = ggplot2::theme_bw())    # diagnostic plots
# dev.off()

report::report(lmm_mod_3)

merTools::predictInterval(lmm_mod_3)                   # for various model predictions, possibly with new data
merTools::FEsim(lmm_mod_3)                             # mean, median and sd of the fixed effect estimates
merTools::REsim(lmm_mod_3)                             # mean, median and sd of the random effect estimates

plot_lmm_mod_3_eff <- 
  cowplot::plot_grid(
    merTools::plotFEsim(merTools::FEsim(lmm_mod_3)) + ggtitle("Fixed Effects"),
    merTools::plotREsim(merTools::REsim(lmm_mod_3)) + ggtitle("Random Effects") 
  )
# fast_ggsave(plot_lmm_mod_3_eff, path = graphs_out_folder, width = 10, height = 5) 

plot_lmm_mod_3_interac <- sjPlot::plot_model(lmm_mod_3, type = "int", terms = c("condition", "order"), ci.lvl = 0.95) + 
  ggtitle("Interaction Plot", subtitle = "Predicted values") +
  theme_bw()
# fast_ggsave(plot_lmm_mod_3_interac, path = graphs_out_folder)






# GLMM Correlated random slope and intercept   -- x + (x | g) 
glmm_mod_2 <-
  recomputed_df2 %>% 
  dplyr::filter(newGameState_f %in% c(9, 16)) %>%
  dplyr::mutate(Marker_type = as.factor(Marker_type),
                order = as.factor(order),
                id = as.factor(id)) %>%
  dplyr::mutate(score = scale(score, center = FALSE)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  dplyr::select(id, condition, order, gender, trial, score, Marker_type) %>%
  lme4::glmer(score ~  1 + trial + condition * order + gender + (1 + trial | id),   # (1 + trial | Marker_type)
              family = "Gamma", data = .) %>%     
  lmerTest::as_lmerModLmerTest() 

summary(glmm_mod_2)
lmerTest:::summary.lmerModLmerTest(glmm_mod_2)

# png(filename = file.path(graphs_out_folder, "plot_glmm_mod_2.png"), width = 12, height = 14, units = "in", res = 300)
performance::check_model(glmm_mod_2, theme = ggplot2::theme_bw())    # diagnostic plots
# dev.off()



##################################################################################################################
# Justin Plots

# Justin Plots
plot_justin_soc <- 
  recomputed_df %>%
    dplyr::filter(condition == "social") %>%
    dplyr::filter(newGameState_f %in% c(16, 17, 18)) %>%
    dplyr::mutate(similarity = 1 / (1 + score)) %>%
    dplyr::group_by(output_file) %>%
    dplyr::mutate(trial = ceiling(newGameState_cycln / 2)) %>%      # cycln to 1:n (cycle can be odds 1,3,5 or evens 2,4,6)
    dplyr::ungroup() %>%
    dplyr::select(id, output_file, condition, order, newGameState_f, newGameState_cycl, newGameState_cycln, trial, score) %>%
    tidyr::pivot_wider(id_cols = c(id, condition, order, output_file, newGameState_cycln, trial), 
                       names_from = newGameState_f, values_from = score, names_prefix = "gameState_") %>%
    dplyr::mutate(diff_1617 = gameState_16 - gameState_17,
                  diff_1618 = gameState_16 - gameState_18) %>%
    ggplot(aes(x = diff_1617, y = diff_1618, color = trial)) +
      geom_point(alpha = 0.8, size = 2) +
      facet_wrap(~ id) +
      geom_hline(yintercept = 0, lty = "dotted") +
      geom_vline(xintercept = 0, lty = "dotted") +
      xlab("Helpfulness of Suggestions (R1O-S)") +
      ylab("Improvement post-Suggestions (R1O-R2O)") +
      ggtitle("Social") +
      scale_color_gradient(low = "yellow", high = "red", na.value = NA) +
      theme_bw()
# fast_ggsave(plot_justin_soc, path = graphs_out_folder, width = 14, height = 12)

plot_justin_bot <- 
  recomputed_df %>%
    dplyr::filter(condition == "bot") %>%
    dplyr::filter(newGameState_f %in% c(16, 17, 18)) %>%
    dplyr::mutate(similarity = 1 / (1 + score)) %>%
    dplyr::group_by(output_file) %>%
    dplyr::mutate(trial = ceiling(newGameState_cycln / 2)) %>%      # cycln to 1:n (cycle can be odds 1,3,5 or evens 2,4,6)
    dplyr::ungroup() %>%
    dplyr::select(id, output_file, condition, order, newGameState_f, newGameState_cycl, newGameState_cycln, trial, score) %>%
    tidyr::pivot_wider(id_cols = c(id, condition, order, output_file, newGameState_cycln, trial), 
                       names_from = newGameState_f, values_from = score, names_prefix = "gameState_") %>%
    dplyr::mutate(diff_1617 = gameState_16 - gameState_17,
                  diff_1618 = gameState_16 - gameState_18) %>%
    ggplot(aes(x = diff_1617, y = diff_1618, color = trial)) +
      geom_point(alpha = 0.8, size = 2) +
      facet_wrap(~ id) +
      geom_hline(yintercept = 0, lty = "dotted") +
      geom_vline(xintercept = 0, lty = "dotted") +
      xlab("Helpfulness of Suggestions (R1O-S)") +
      ylab("Improvement post-Suggestions (R1O-R2O)") +
      ggtitle("Bot") +
      scale_color_gradient(low = "yellow", high = "red", na.value = NA) +
      theme_bw()
# fast_ggsave(plot_justin_bot, path = graphs_out_folder, width = 14, height = 12)


justin_data <-
  recomputed_df %>%
  dplyr::filter(newGameState_f %in% c(16, 17, 18)) %>%
  dplyr::mutate(similarity = 1 / (1 + score)) %>%
  dplyr::group_by(output_file) %>%
  dplyr::mutate(trial = ceiling(newGameState_cycln / 2)) %>%      # cycln to 1:n (cycle can be odds 1,3,5 or evens 2,4,6)
  dplyr::ungroup() %>%
  dplyr::select(id, output_file, condition, order, gender, newGameState_f, newGameState_cycl, newGameState_cycln, trial, score) %>%
  tidyr::pivot_wider(id_cols = c(id, condition, order, gender, output_file, newGameState_cycln, trial), 
                     names_from = newGameState_f, values_from = score, names_prefix = "gameState_") %>%
  dplyr::mutate(diff_1617 = gameState_16 - gameState_17,
                diff_1618 = gameState_16 - gameState_18) 

justin_slopes <-
  justin_data %>%
  dplyr::group_by(output_file, id, condition, order, gender) %>%
  tidyr::nest() %>% 
  dplyr::mutate(model = purrr::map(data, ~lm(diff_1618 ~ 1 + diff_1617, data = .x) %>% broom::tidy())) %>% 
  tidyr::unnest(model) %>% 
  dplyr::filter(term == "diff_1617") %>%
  dplyr::rename(slope = estimate) %>%
  dplyr::ungroup()

plot_justin_slopes <- 
  ggstatsplot::ggwithinstats(
    data = justin_slopes,
    x = condition,
    y = slope,
    outlier.tagging = TRUE,
    outlier.coef = 1.1,     # default is 1.5
    outlier.label = id,
    ggtheme = ggprism::theme_prism(),
    # package = "RColorBrewer",  # "ggsci",
    # palette = "Dark",         # "default_jco",
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12), 
        plot.caption = element_text(hjust = 0, size = 12), 
        text = element_text(size = 14)
      ))
  ) + scale_colour_grey(start = 0.2, end = 0.2)
# fast_png(plot_justin_slopes, path = graphs_out_folder, width = 8, height = 8)

plot_justin_slopes_nooutlier <- 
  justin_slopes %>%
    dplyr::filter(!id %in% c("40.0", "45.0")) %>%
    ggstatsplot::ggwithinstats(
      data = .,
      x = condition,
      y = slope,
      outlier.tagging = TRUE,
      outlier.coef = 1.1,     # default is 1.5
      outlier.label = id,
      ggtheme = ggprism::theme_prism(),
      # package = "RColorBrewer",  # "ggsci",
      # palette = "Dark",         # "default_jco",
      violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
      centrality.plotting = TRUE,
      centrality.type = "parameteric",
      centrality.point.args = list(size = 5, color = "darkred"),
      centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
      ggplot.component = list(
        theme(
          plot.title = element_text(hjust = 0, size = 16),
          plot.subtitle = element_text(hjust = 0, size = 12), 
          plot.caption = element_text(hjust = 0, size = 12), 
          text = element_text(size = 14)
        ))
    ) + scale_colour_grey(start = 0.2, end = 0.2)
# fast_png(plot_justin_slopes_nooutlier, path = graphs_out_folder, width = 8, height = 8)



plot_justin_slopes_nooutlier_gender <-
  justin_slopes %>%
  dplyr::filter(!id %in% c("40.0", "45.0")) %>%
  ggstatsplot::grouped_ggwithinstats(
    x = condition,
    y = slope, 
    grouping.var = gender, 
    ggtheme = ggprism::theme_prism(),
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      scale_y_continuous(limits = c(-0.3, 1.2)),
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 12),
        text = element_text(size = 14)
      ))
  ) 
# fast_png(plot_justin_slopes_nooutlier_gender, path = graphs_out_folder, width = 14, height = 10)

##################################################################################################################
# Start-Flag Plots (Agngle between (Rs - Tf)(Rs - Rf))


# Data
startflag_data <-
  recomputed_df %>%
  dplyr::filter(newGameState_f %in% c(14, 15)) %>%
  dplyr::group_by(output_file) %>%
  dplyr::mutate(trial = ceiling(newGameState_cycln / 2)) %>%      # cycln to 1:n (cycle can be odds 1,3,5 or evens 2,4,6)
  dplyr::ungroup() %>%
  dplyr::select(id, output_file, condition, order, gender, newGameState_f, newGameState_cycl, newGameState_cycln, trial, score, 
                xPos, zPos, Marker_x, Marker_z, Marker_type) %>%
  tidyr::pivot_wider(id_cols = c(id, condition, order, gender, output_file, newGameState_cycln, trial), 
                     names_from = newGameState_f, 
                     values_from = c(xPos, zPos, Marker_x, Marker_z, Marker_type), 
                     names_prefix = "") %>%
  rowwise() %>%
  dplyr::mutate(
    angle = points_to_innerangle(x1 = xPos_14, y1 = zPos_14,             # Rs
                                 x2 = Marker_x_15, y2 = Marker_z_15,     # Tf
                                 x3 = xPos_15, y3 = zPos_15),            # Rf
    cos_angle = cos(angle)
  )


plot_startflag_medangl <- 
  startflag_data %>%
  dplyr::filter(trial < 9) %>%
  group_by(id, condition, order, gender) %>%
  dplyr::summarise(median_angle = median(angle)) %>%
  ggstatsplot::ggbetweenstats(
    x = condition,
    y = median_angle,
    outlier.tagging = FALSE,
    outlier.coef = 1.1,     # default is 1.5
    outlier.label = id,
    ggtheme = ggprism::theme_prism(),
    # package = "RColorBrewer",  # "ggsci",
    # palette = "Dark",         # "default_jco",
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      scale_y_continuous(limits = c(0, 180)),
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 12),
        text = element_text(size = 14)
      ))
  ) 
# fast_png(plot_startflag_medangl, path = file.path(graphs_out_folder, "startflag"), width = 10, height = 10)

plot_startflag_medangl_ord <- 
  startflag_data %>%
  dplyr::filter(trial < 9) %>%
  group_by(id, condition, order, gender) %>%
  dplyr::summarise(median_angle = median(angle)) %>%
  ggstatsplot::grouped_ggbetweenstats(
    x = condition,
    y = median_angle,
    grouping.var = order,
    outlier.tagging = FALSE,
    outlier.coef = 1.1,     # default is 1.5
    outlier.label = id,
    ggtheme = ggprism::theme_prism(),
    # package = "RColorBrewer",  # "ggsci",
    # palette = "Dark",         # "default_jco",
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      scale_y_continuous(limits = c(0, 180)),
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 12),
        text = element_text(size = 14)
      ))
  ) 
# fast_png(plot_startflag_medangl_ord, path = file.path(graphs_out_folder, "startflag"), width = 14, height = 10)

plot_startflag_medangl_gender <- 
  startflag_data %>%
  dplyr::filter(trial < 9) %>%
  group_by(id, condition, order, gender) %>%
  dplyr::summarise(median_angle = median(angle)) %>%
  ggstatsplot::grouped_ggbetweenstats(
    x = condition,
    y = median_angle,
    grouping.var = gender,
    outlier.tagging = FALSE,
    outlier.coef = 1.1,     # default is 1.5
    outlier.label = id,
    ggtheme = ggprism::theme_prism(),
    # package = "RColorBrewer",  # "ggsci",
    # palette = "Dark",         # "default_jco",
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      scale_y_continuous(limits = c(0, 180)),
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 12),
        text = element_text(size = 14)
      ))
  ) 
# fast_png(plot_startflag_medangl_gender, path = file.path(graphs_out_folder, "startflag"), width = 14, height = 10)

innerangle_plot <- 
  ggplot(startflag_data, aes(x = trial, y = angle)) +
    geom_line(aes(color = as.factor(id)), alpha = .5) +
    geom_smooth(method = "loess", formula = "y ~ x", color = "red", fill = "red") +
    geom_hline(yintercept = 0, lty = "dashed") + 
    facet_wrap(~order + condition, nrow = 2, labeller = "label_both") +
    scale_x_continuous(breaks = 1:14) +
    labs(color = "id") + 
    theme_classic() +
    theme(legend.position = "none")
# fast_png(innerangle_plot, path = file.path(graphs_out_folder, "startflag") , width = 14, height = 10)

innerangle_cos_plot <- 
  ggplot(startflag_data, aes(x = trial, y = cos_angle)) +
  geom_line(aes(color = as.factor(id)), alpha = .5) +
  geom_smooth(method = "loess", formula = "y ~ x", color = "red", fill = "red") +
  geom_hline(yintercept = 0, lty = "dashed") + 
  facet_wrap(~order + condition, nrow = 2, labeller = "label_both") +
  scale_x_continuous(breaks = 1:14) +
  labs(color = "id") + 
  theme_classic() +
  theme(legend.position = "none")
# fast_png(innerangle_cos_plot, path = file.path(graphs_out_folder, "startflag") , width = 14, height = 10)


# Angle plots
ntrials <- 7
grid <- data.frame(x = seq(-25, 25, by = 1), y = seq(-25, 25, by = 1))
startflag_soc_data <-
  startflag_data %>%
  dplyr::filter(condition == "social", trial <= ntrials) %>%
  dplyr::mutate(angle_lab = paste0("\u03b1=", round(angle, 0), ", cos(\u03b1)=", round(cos_angle, 2), sep = " "))
startflag_bot_data <-
  startflag_data %>%
  dplyr::filter(condition == "bot", trial <= ntrials) %>%
  dplyr::mutate(angle_lab = paste0("\u03b1=", round(angle, 0), ", cos(\u03b1)=", round(cos_angle, 2), sep = " "))

# Social
plot_angleline_soc <-
  ggplot(grid, aes(x = x, y = y)) +
    geom_vline(xintercept = 0, lty = "dashed") + 
    geom_hline(yintercept = 0, lty = "dashed") +
    theme_classic() +
    geom_point(data = startflag_soc_data, aes(x = Marker_x_15, y = Marker_z_15, color = trial, group = trial), shape = 15, size = 2) + # square Tf
    geom_segment(data = startflag_soc_data, aes(x = xPos_14, y = zPos_14, xend = Marker_x_15, yend = Marker_z_15, colour = trial, group = trial)) +  # Rs-Tf
    geom_point(data = startflag_soc_data, aes(x = xPos_15, y = zPos_15, color = trial, group = trial), shape = 16, size = 2) +         # round Rf
    geom_segment(data = startflag_soc_data, aes(x = xPos_14, y = zPos_14, xend = xPos_15, yend = zPos_15, colour = trial, group = trial)) +          # Rs-Rf
    geom_text(data = startflag_soc_data, x = 0, y = 22,
              aes(label = angle_lab, group = trial), parse = FALSE) +
    facet_wrap(~id) +
    # scale_colour_steps(low = "yellow", high = "red", na.value = NA)    # scale_color_gradient for continuous
    binned_scale(aesthetics = "color", scale_name = "stepsn", 
                 palette = function(x) viridis::viridis(ntrials, direction = -1),
                 breaks = seq(1, ntrials, by = 1),
                 limits = c(1, ntrials),
                 show.limits = TRUE, 
                 guide = "colorsteps") 
    
plot_angleline_soc_anim <- 
  plot_angleline_soc + 
  gganimate::transition_states(trial, transition_length = 4, state_length = 4)
    
# gganimate::animate(plot_angleline_soc_anim, height = 1100, width = 1600)
# gganimate::anim_save("plot_angleline_soc.gif", animation = gganimate::last_animation(), path = file.path(graphs_out_folder, "startflag"))

# Bot
plot_angleline_bot <-
  ggplot(grid, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, lty = "dashed") + 
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_classic() +
  geom_point(data = startflag_bot_data, aes(x = Marker_x_15, y = Marker_z_15, color = trial, group = trial), shape = 15, size = 2) + # square Tf
  geom_segment(data = startflag_bot_data, aes(x = xPos_14, y = zPos_14, xend = Marker_x_15, yend = Marker_z_15, colour = trial, group = trial)) +  # Rs-Tf
  geom_point(data = startflag_bot_data, aes(x = xPos_15, y = zPos_15, color = trial, group = trial), shape = 16, size = 2) +         # round Rf
  geom_segment(data = startflag_bot_data, aes(x = xPos_14, y = zPos_14, xend = xPos_15, yend = zPos_15, colour = trial, group = trial)) +          # Rs-Rf
  geom_text(data = startflag_bot_data, x = 0, y = 22,
            aes(label = angle_lab, group = trial), parse = FALSE) +
  facet_wrap(~id) +
  # scale_colour_steps(low = "yellow", high = "red", na.value = NA)    # scale_color_gradient for continuous
  binned_scale(aesthetics = "color", scale_name = "stepsn", 
               palette = function(x) viridis::viridis(ntrials, direction = -1),
               breaks = seq(1, ntrials, by = 1),
               limits = c(1, ntrials),
               show.limits = TRUE, 
               guide = "colorsteps")  

plot_angleline_bot_anim <- 
  plot_angleline_bot + 
  gganimate::transition_states(trial, transition_length = 4, state_length = 4)

# gganimate::animate(plot_angleline_bot_anim, height = 1100, width = 1600)
# gganimate::anim_save("plot_angleline_bot.gif", animation = gganimate::last_animation(), path = file.path(graphs_out_folder, "startflag"))





##################################################################################################################
#  Object Colaboration direction (Agngle between (R1O - O)(R1O - R2O), where O is True Object Location)
# R1O-MarkerPoz, R1O-R2O (makes sense because active Player is locked to R1O pos until Suggestion is given)
# Careful: NaNs appear in cases when Player doesn't move from R1O to R2O (pos R1O == pos R2O)
#  ---- is.nan(angle) means that S was disregarded = noncompliant, !is.nan(angle) = compliant sort of ...

# Data
objanglcolab_data <-
  recomputed_df %>%
  dplyr::filter(newGameState_f %in% c(16, 18)) %>%
  dplyr::group_by(output_file) %>%
  dplyr::mutate(trial = ceiling(newGameState_cycln / 2)) %>%      # cycln to 1:n (cycle can be odds 1,3,5 or evens 2,4,6)
  dplyr::ungroup() %>%
  dplyr::select(id, output_file, condition, order, gender, newGameState_f, newGameState_cycl, newGameState_cycln, trial, score, 
                xPos, zPos, Marker_x, Marker_z, Marker_type) %>%
  tidyr::pivot_wider(id_cols = c(id, condition, order, gender, output_file, newGameState_cycln, trial), 
                     names_from = newGameState_f, 
                     values_from = c(xPos, zPos, Marker_x, Marker_z, Marker_type), 
                     names_prefix = "") %>%
  rowwise() %>%
  dplyr::mutate(
    angle = points_to_innerangle(x1 = xPos_16, y1 = zPos_16,             # R1O
                                 x2 = Marker_x_18, y2 = Marker_z_18,     # O    -- Marker_x_18 == Marker_x_16, Marker_z_18 == Marker_z_16 
                                 x3 = xPos_18, y3 = zPos_18),            # R2O
    cos_angle = cos(angle)
  ) %>%
  dplyr::filter(!is.nan(angle), !is.na(angle))    # there are also NAs


plot_objanglcolab_medangl <- 
  objanglcolab_data %>%
  dplyr::filter(trial < 9) %>%
  group_by(id, condition, order, gender) %>%
  dplyr::summarise(median_angle = median(angle)) %>%
  ggstatsplot::ggbetweenstats(
    x = condition,
    y = median_angle,
    outlier.tagging = FALSE,
    outlier.coef = 1.1,     # default is 1.5
    outlier.label = id,
    ggtheme = ggprism::theme_prism(),
    # package = "RColorBrewer",  # "ggsci",
    # palette = "Dark",         # "default_jco",
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      scale_y_continuous(limits = c(0, 180)),
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 12),
        text = element_text(size = 14)
      ))
  ) 
# fast_png(plot_objanglcolab_medangl, path = file.path(graphs_out_folder, "objanglcolab"), width = 10, height = 10)

plot_objanglcolab_medangl_ord <- 
  objanglcolab_data %>%
  dplyr::filter(trial < 9) %>%
  group_by(id, condition, order, gender) %>%
  dplyr::summarise(median_angle = median(angle)) %>%
  ggstatsplot::grouped_ggbetweenstats(
    x = condition,
    y = median_angle,
    grouping.var = order,
    outlier.tagging = FALSE,
    outlier.coef = 1.1,     # default is 1.5
    outlier.label = id,
    ggtheme = ggprism::theme_prism(),
    # package = "RColorBrewer",  # "ggsci",
    # palette = "Dark",         # "default_jco",
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      scale_y_continuous(limits = c(0, 180)),
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 12),
        text = element_text(size = 14)
      ))
  ) 
# fast_png(plot_objanglcolab_medangl_ord, path = file.path(graphs_out_folder, "objanglcolab"), width = 14, height = 10)

plot_objanglcolab_medangl_gender <- 
  objanglcolab_data %>%
  dplyr::filter(trial < 9) %>%
  group_by(id, condition, order, gender) %>%
  dplyr::summarise(median_angle = median(angle)) %>%
  ggstatsplot::grouped_ggbetweenstats(
    x = condition,
    y = median_angle,
    grouping.var = gender,
    outlier.tagging = FALSE,
    outlier.coef = 1.1,     # default is 1.5
    outlier.label = id,
    ggtheme = ggprism::theme_prism(),
    # package = "RColorBrewer",  # "ggsci",
    # palette = "Dark",         # "default_jco",
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      scale_y_continuous(limits = c(0, 180)),
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 12),
        text = element_text(size = 14)
      ))
  ) 
# fast_png(plot_objanglcolab_medangl_gender, path = file.path(graphs_out_folder, "objanglcolab"), width = 14, height = 10)

innerangle_plot <- 
  ggplot(objanglcolab_data, aes(x = trial, y = angle)) +
  geom_line(aes(color = as.factor(id)), alpha = .5) +
  geom_smooth(method = "loess", formula = "y ~ x", color = "red", fill = "red") +
  geom_hline(yintercept = 0, lty = "dashed") + 
  facet_wrap(~order + condition, nrow = 2, labeller = "label_both") +
  scale_x_continuous(breaks = 1:14) +
  labs(color = "id") + 
  theme_classic() +
  theme(legend.position = "none")
# fast_png(innerangle_plot, path = file.path(graphs_out_folder, "objanglcolab") , width = 14, height = 10)

innerangle_cos_plot <- 
  ggplot(objanglcolab_data, aes(x = trial, y = cos_angle)) +
  geom_line(aes(color = as.factor(id)), alpha = .5) +
  geom_smooth(method = "loess", formula = "y ~ x", color = "red", fill = "red") +
  geom_hline(yintercept = 0, lty = "dashed") + 
  facet_wrap(~order + condition, nrow = 2, labeller = "label_both") +
  scale_x_continuous(breaks = 1:14) +
  labs(color = "id") + 
  theme_classic() +
  theme(legend.position = "none")
# fast_png(innerangle_cos_plot, path = file.path(graphs_out_folder, "objanglcolab") , width = 14, height = 10)


# Angle plots
ntrials <- 7
grid <- data.frame(x = seq(-25, 25, by = 1), y = seq(-25, 25, by = 1))
objanglcolab_soc_data <-
  objanglcolab_data %>%
  dplyr::filter(condition == "social", trial <= ntrials) %>%
  dplyr::mutate(angle_lab = paste0("\u03b1=", round(angle, 0), ", cos(\u03b1)=", round(cos_angle, 2), sep = " "))
objanglcolab_bot_data <-
  objanglcolab_data %>%
  dplyr::filter(condition == "bot", trial <= ntrials) %>%
  dplyr::mutate(angle_lab = paste0("\u03b1=", round(angle, 0), ", cos(\u03b1)=", round(cos_angle, 2), sep = " "))

# Social
plot_angleline_soc <-
  ggplot(grid, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, lty = "dashed") + 
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_classic() +
  geom_point(data = objanglcolab_soc_data, aes(x = Marker_x_18, y = Marker_z_18, color = trial, group = trial), shape = 15, size = 2) + # square O
  geom_segment(data = objanglcolab_soc_data, aes(x = xPos_16, y = zPos_16, xend = Marker_x_18, yend = Marker_z_18, colour = trial, group = trial)) +  # R1O-O
  geom_point(data = objanglcolab_soc_data, aes(x = xPos_18, y = zPos_18, color = trial, group = trial), shape = 16, size = 2) +         # round R2O
  geom_segment(data = objanglcolab_soc_data, aes(x = xPos_16, y = zPos_16, xend = xPos_18, yend = zPos_18, colour = trial, group = trial)) +          # R1O-R2O
  geom_text(data = objanglcolab_soc_data, x = 0, y = 22,
            aes(label = angle_lab, group = trial), parse = FALSE) +
  facet_wrap(~id) +
  # scale_colour_steps(low = "yellow", high = "red", na.value = NA)    # scale_color_gradient for continuous
  binned_scale(aesthetics = "color", scale_name = "stepsn", 
               palette = function(x) viridis::viridis(ntrials, direction = -1),
               breaks = seq(1, ntrials, by = 1),
               limits = c(1, ntrials),
               show.limits = TRUE, 
               guide = "colorsteps") 

plot_angleline_soc_anim <- 
  plot_angleline_soc + 
  gganimate::transition_states(trial, transition_length = 4, state_length = 4)

# gganimate::animate(plot_angleline_soc_anim, height = 1100, width = 1600)
# gganimate::anim_save("plot_angleline_soc.gif", animation = gganimate::last_animation(), path = file.path(graphs_out_folder, "objanglcolab"))

# Bot
plot_angleline_bot <-
  ggplot(grid, aes(x = x, y = y)) +
  geom_vline(xintercept = 0, lty = "dashed") + 
  geom_hline(yintercept = 0, lty = "dashed") +
  theme_classic() +
  geom_point(data = objanglcolab_soc_data, aes(x = Marker_x_18, y = Marker_z_18, color = trial, group = trial), shape = 15, size = 2) + # square O
  geom_segment(data = objanglcolab_soc_data, aes(x = xPos_16, y = zPos_16, xend = Marker_x_18, yend = Marker_z_18, colour = trial, group = trial)) +  # R1O-O
  geom_point(data = objanglcolab_soc_data, aes(x = xPos_18, y = zPos_18, color = trial, group = trial), shape = 16, size = 2) +         # round R2O
  geom_segment(data = objanglcolab_soc_data, aes(x = xPos_16, y = zPos_16, xend = xPos_18, yend = zPos_18, colour = trial, group = trial)) +          # R1O-R2O
  geom_text(data = objanglcolab_bot_data, x = 0, y = 22,
            aes(label = angle_lab, group = trial), parse = FALSE) +
  facet_wrap(~id) +
  # scale_colour_steps(low = "yellow", high = "red", na.value = NA)    # scale_color_gradient for continuous
  binned_scale(aesthetics = "color", scale_name = "stepsn", 
               palette = function(x) viridis::viridis(ntrials, direction = -1),
               breaks = seq(1, ntrials, by = 1),
               limits = c(1, ntrials),
               show.limits = TRUE, 
               guide = "colorsteps")  

plot_angleline_bot_anim <- 
  plot_angleline_bot + 
  gganimate::transition_states(trial, transition_length = 4, state_length = 4)

# gganimate::animate(plot_angleline_bot_anim, height = 1100, width = 1600)
# gganimate::anim_save("plot_angleline_bot.gif", animation = gganimate::last_animation(), path = file.path(graphs_out_folder, "objanglcolab"))


##### Update Carcea
#  Object Colaboration direction (Agngle between (O - R1O)(O - R2O), where O is True Object Location)
# metric meaning not clear ???

# Data
objanglcolab2_data <-
  recomputed_df %>%
  dplyr::filter(newGameState_f %in% c(16, 18)) %>%
  dplyr::group_by(output_file) %>%
  dplyr::mutate(trial = ceiling(newGameState_cycln / 2)) %>%      # cycln to 1:n (cycle can be odds 1,3,5 or evens 2,4,6)
  dplyr::ungroup() %>%
  dplyr::select(id, output_file, condition, order, gender, newGameState_f, newGameState_cycl, newGameState_cycln, trial, score, 
                xPos, zPos, Marker_x, Marker_z, Marker_type) %>%
  tidyr::pivot_wider(id_cols = c(id, condition, order, gender, output_file, newGameState_cycln, trial), 
                     names_from = newGameState_f, 
                     values_from = c(xPos, zPos, Marker_x, Marker_z, Marker_type), 
                     names_prefix = "") %>%
  rowwise() %>%
  dplyr::mutate(
    angle = points_to_innerangle(x1 = Marker_x_18, y1 = Marker_z_18,     # O    -- Marker_x_18 == Marker_x_16, Marker_z_18 == Marker_z_16 
                                 x2 = xPos_16, y2 = zPos_16,             # R1O
                                 x3 = xPos_18, y3 = zPos_18),            # R2O
    cos_angle = cos(angle)
  ) %>%
  dplyr::filter(!is.nan(angle), !is.na(angle))    # there are also NAs

plot_objanglcolab2_medangl <- 
  objanglcolab2_data %>%
  dplyr::filter(trial < 9) %>%
  group_by(id, condition, order, gender) %>%
  dplyr::summarise(median_angle = median(angle)) %>%
  ggstatsplot::ggbetweenstats(
    x = condition,
    y = median_angle,
    outlier.tagging = FALSE,
    outlier.coef = 1.1,     # default is 1.5
    outlier.label = id,
    ggtheme = ggprism::theme_prism(),
    # package = "RColorBrewer",  # "ggsci",
    # palette = "Dark",         # "default_jco",
    violin.args = list(width = 0.9, alpha = 0.2, size = 1, color = "black"),
    centrality.plotting = TRUE,
    centrality.type = "parameteric",
    centrality.point.args = list(size = 5, color = "darkred"),
    centrality.label.args = list(size = 0, nudge_x = 10, segment.linetype = 0, alpha = 0), # very hacky way of not showing label
    ggplot.component = list(
      scale_y_continuous(limits = c(0, 180)),
      theme(
        plot.title = element_text(hjust = 0, size = 16),
        plot.subtitle = element_text(hjust = 0, size = 12),
        plot.caption = element_text(hjust = 0, size = 12),
        text = element_text(size = 14)
      ))
  ) 
# fast_png(plot_objanglcolab2_medangl, path = file.path(graphs_out_folder, "objanglcolab"), width = 10, height = 10)


##################################################################################################################
# Compliance = dist(S, R2O)  --- compliance by ignorance (you don't comply if you know true MarkerPos, still confounded)
# Compliance_categ = (dist(S, R2O) < dist(R1O, R2O))
# playerPosition_x_demoimit & playerPosition_z_demoimit store correct value for gameState 17 ... xPos &  zPos don't
complcateg_df <- 
  recomputed_df %>%
  dplyr::filter(newGameState_f %in% c(16, 17, 18)) %>%     # "16" = "R1O", "17" = "S", "18" = "R2O"
  dplyr::group_by(output_file) %>%
  dplyr::mutate(trial = ceiling(newGameState_cycln / 2)) %>%      # cycln to 1:n (cycle can be odds 1,3,5 or evens 2,4,6)
  dplyr::ungroup() %>%
  dplyr::select(id, output_file, condition, order, gender, newGameState_f, newGameState_cycl, newGameState_cycln, trial, score, 
                playerPosition_x_demoimit, playerPosition_z_demoimit, Marker_x, Marker_z, Marker_type) %>%
  tidyr::pivot_wider(id_cols = c(id, condition, order, gender, output_file, newGameState_cycln, trial), 
                     names_from = newGameState_f, 
                     values_from = c(playerPosition_x_demoimit, playerPosition_z_demoimit, Marker_x, Marker_z, Marker_type, score), 
                     names_prefix = "") %>%
  dplyr::mutate(dist_S = eucl_dist(x1 = playerPosition_x_demoimit_17, x2 = playerPosition_x_demoimit_16, y1 = playerPosition_z_demoimit_17, y2 = playerPosition_z_demoimit_16),
                dist_R1o = eucl_dist(x1 = playerPosition_x_demoimit_16, x2 = playerPosition_x_demoimit_18, y1 = playerPosition_z_demoimit_16, y2 = playerPosition_z_demoimit_18),
                compl_categ = dist_S < dist_R1o) %>%
  dplyr::filter(!is.na(compl_categ))

complcateg_df %>%
  dplyr::group_by(condition, compl_categ) %>%
  rstatix::get_summary_stats(score_18)

complcateg_df %>%
  dplyr::group_by(condition) %>%
  rstatix::t_test(score_18 ~ compl_categ, detailed = TRUE)


# ... should continue this

##################################################################################################################
# Justin's idea
# dist(R1, S), dist(R1, R2) -- in ICeeg r=.8 R2 changes changes based on S ... S acts as 'negative' feedback 

sr1r2_df <- 
  recomputed_df %>%
  dplyr::filter(newGameState_f %in% c(16, 17, 18)) %>%     # "16" = "R1O", "17" = "S", "18" = "R2O"
  dplyr::group_by(output_file) %>%
  dplyr::mutate(trial = ceiling(newGameState_cycln / 2)) %>%      # cycln to 1:n (cycle can be odds 1,3,5 or evens 2,4,6)
  dplyr::ungroup() %>%
  dplyr::select(id, output_file, condition, order, gender, newGameState_f, newGameState_cycl, newGameState_cycln, trial, score, 
                playerPosition_x_demoimit, playerPosition_z_demoimit, Marker_x, Marker_z, Marker_type) %>%
  tidyr::pivot_wider(id_cols = c(id, condition, order, gender, output_file, newGameState_cycln, trial), 
                     names_from = newGameState_f, 
                     values_from = c(playerPosition_x_demoimit, playerPosition_z_demoimit, Marker_x, Marker_z, Marker_type, score), 
                     names_prefix = "") %>%
  dplyr::mutate(dist_R1O_S = eucl_dist(x1 = playerPosition_x_demoimit_16, x2 = playerPosition_x_demoimit_16, y1 = playerPosition_z_demoimit_16, y2 = playerPosition_z_demoimit_17),
                dist_R1O_R2O = eucl_dist(x1 = playerPosition_x_demoimit_16, x2 = playerPosition_x_demoimit_18, y1 = playerPosition_z_demoimit_16, y2 = playerPosition_z_demoimit_18)
  ) 

sr1r2_df %>%
  dplyr::filter(condition == "social") %>%
  dplyr::select(dist_R1O_R2O, dist_R1O_S) %>%
  correlation::correlation(method = "spearman")

plot_justin_sr1r2 <-
  sr1r2_df %>%
    ggstatsplot::grouped_ggscatterstats(
      dist_R1O_R2O, dist_R1O_S,
      grouping.var = condition
    )

plot_justin_sr1r2_color <-
  sr1r2_df %>%
  dplyr::rename(dist_R1O_O = score_16) %>%
  ggstatsplot::grouped_ggscatterstats(
    dist_R1O_R2O, dist_R1O_S,
    grouping.var = condition,
    point.args = list(aes(color = dist_R1O_O)),
    ggplot.component = ggplot2::scale_color_gradient(low = "gray100", high = "black"),
    bf.message = FALSE,
    xfill = "gray", yfill = "gray"
  ) 
# fast_png(plot_justin_sr1r2_color, path = graphs_out_folder, width = 14, height = 10)

##################################################################################################################
# Distribution of score (euclidean distance)

# Individual
recomputed_df %>%
  dplyr::filter(newGameState_f == 16, condition == "social") %>%
  ggpubr::gghistogram(x = "score", add = "mean", rug = TRUE, fill = "gray") +
  facet_wrap(~ id) +
  ggtitle("R1O Social")

recomputed_df %>%
  dplyr::filter(newGameState_f == 16, condition == "bot") %>%
  ggpubr::gghistogram(x = "score", add = "mean", rug = TRUE, fill = "gray") +
  facet_wrap(~ id) +
  ggtitle("R1O Bot")

# Aggregated
recomputed_df %>%
  dplyr::filter(newGameState_f == 9) %>%     # "9"  = "RO", "16" = "R1O", "17" = "S", "18" = "R2O"
  # dplyr::mutate(score = sqrt(score)) %>%
  ggstatsplot::grouped_gghistostats(
    x = score,
    subtitle = "RO",
    grouping.var = condition,
    results.subtitle = FALSE
  )

recomputed_df %>%
  dplyr::filter(newGameState_f == 16) %>% 
  # dplyr::mutate(score = sqrt(score)) %>%
  ggstatsplot::grouped_gghistostats(
    x = score,
    subtitle = "R1O",
    grouping.var = condition,
    results.subtitle = FALSE
  )

recomputed_df %>%
  dplyr::filter(newGameState_f == 18) %>%
  # dplyr::mutate(score = sqrt(score)) %>%
  ggstatsplot::grouped_gghistostats(
    x = score,
    subtitle = "R2O",
    grouping.var = condition,
    results.subtitle = FALSE
  )


# Distribution?
recomputed_df %>%
  dplyr::filter(newGameState_f == 9) %>%
  dplyr::pull(score) %>%
  fitdistrplus::descdist(., discrete = FALSE, boot = 1000)  # orange values around the blue point are based on bootstrapping

recomputed_df %>%
  dplyr::filter(newGameState_f == 9) %>%
  dplyr::pull(score) %>%
  goft::gamma_test()

# recomputed_df %>%
#   dplyr::filter(newGameState_f == 9) %>%
#   dplyr::pull(score) %>%
#   fitdistrplus::fitdist(., "gamma") -> gammafit_GS9

recomputed_df %>%
  dplyr::filter(newGameState_f %in% c(16, 18)) %>%
  dplyr::pull(score) %>%
  fitdistrplus::descdist(., discrete = FALSE, boot = 1000)  # orange values around the blue point are based on bootstrapping

# Transform
recomputed_df %>%
  dplyr::filter(newGameState_f %in% c(9, 16, 18)) %>%
  dplyr::mutate(score = sqrt(score)) %>%
  dplyr::group_by(condition, newGameState_f) %>%
  rstatix::shapiro_test(score)



# Correlated random slope and intercept   -- x + (x | g) 
# THIS IS GOOD, sqrt normalizes the residuals
lmm_mod_4 <-
  recomputed_df2 %>%                        # CAREFUL, THIS IS WITH NOOUTLIER DATA
  dplyr::filter(newGameState_f %in% c(9, 16)) %>%
  dplyr::mutate(score = sqrt(score)) %>%
  dplyr::mutate(Marker_type = as.factor(Marker_type),
                order = factor(order)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%   # row_number() - 1 => would start time with 0, but doesn't change results
  dplyr::ungroup() %>%
  lme4::lmer(score ~  1 + trial + condition * order + gender + (1 + trial | id), data = .) %>%  # + (1 + Marker_type | id)
  lmerTest::as_lmerModLmerTest() 

lmerTest:::summary.lmerModLmerTest(lmm_mod_4)

# png(filename = file.path(graphs_out_folder, "plot_lmm_mod_4.png"), width = 12, height = 14, units = "in", res = 300)
performance::check_model(lmm_mod_4, theme = ggplot2::theme_bw())    # diagnostic plots
# dev.off()

report::report(lmm_mod_4)

merTools::predictInterval(lmm_mod_4)                   # for various model predictions, possibly with new data
merTools::FEsim(lmm_mod_4)                             # mean, median and sd of the fixed effect estimates
merTools::REsim(lmm_mod_4)                             # mean, median and sd of the random effect estimates

plot_lmm_mod_4_eff <- 
  cowplot::plot_grid(
    merTools::plotFEsim(merTools::FEsim(lmm_mod_4)) + ggtitle("Fixed Effects"),
    merTools::plotREsim(merTools::REsim(lmm_mod_4)) + ggtitle("Random Effects") 
  )
# fast_ggsave(plot_lmm_mod_4_eff, path = graphs_out_folder, width = 10, height = 5) 

plot_lmm_mod_4_interac <- sjPlot::plot_model(lmm_mod_4, type = "int", terms = c("condition", "order"), ci.lvl = 0.95) + 
  ggtitle("Interaction Plot", subtitle = "Predicted values") +
  theme_bw()
# fast_ggsave(plot_lmm_mod_4_interac, path = graphs_out_folder)

##################################################################################################################









#####################################################
# Demo confounding
set.seed(42)

# Construct a correlation matrix for two uncorrelated normal variables
mat <- matrix(c(1, 0, 0, 0, 1, 0,  0, 0, 1), byrow = TRUE, nrow = 3)
means = c(0, 0, 0)

# Simulate 200 data points for x1, x2, x3 that are normally distributed and uncorrelated 
data <-  MASS::mvrnorm(n = 200, mu = means, Sigma = m, empirical = TRUE)
data <- as.data.frame(data)
colnames(data) <- c("x1", "x2", "x3")

correlation::correlation(data)

data$var1 <- data$x1 - data$x3
data$var2 <- data$x2 - data$x3

correlation::correlation(data[, c("var1", "var2", "x3")])  


# XiXian
# colab dist = second r - s
# s
#####################################################








# ##### 
# justing_reg <- 
#   justin_data %>%
#   ggstatsplot::grouped_ggscatterstats(
#     data = .,
#     x                = diff_1617,
#     y                = diff_1618,
#     grouping.var     = paste(condition, order),
#     label.var        = id,
#     label.expression = diff_1617 > 200,
#     xlab             = "Helpfulness of Suggestions (16-17)",
#     ylab             = "Improvement post-Suggestions (16-18)",
#     # ggplot.component = list(ggplot2::scale_x_continuous(breaks = seq(2, 9, 1), limits = (c(2, 9)))),
#     # plotgrid.args    = list(nrow = 1),
#     annotation.args  = list(title = "Relationship between Suggestion and Decisive")
#   )
# #####







#####
# LMM diag plots
# redres::plot_redres(lmm_mod_1, type = "std_cond")  
# cowplot::plot_grid(
#   cowplot::ggdraw() + 
#   draw_label(
#     "Miles per gallon decline with displacement and horsepower",
#     fontface = 'bold',  x = 0, hjust = 0
#   ),
#   redres::plot_redres(lmm_mod_1, type = "std_mar", xvar = "condition"),
#   redres::plot_redres(lmm_mod_1, type = "std_mar", xvar = "order")
# )











