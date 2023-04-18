# new SSm
require(optimx)
options(device = "RStudioGD")
folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/ssm_map/graphs/"



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

# Package names
packages <- c(
  "tidyverse", "ggplot2", "ggforce", "scales",
  "crosstalk", "plotly", "DT",
  "ggstatsplot"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Load
invisible(lapply(packages, library, character.only = TRUE))


# Read data
folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/ssm_map"
setwd(folder)

data <- readRDS("recomputed_df.RDS")   # recomputed_df is from SSM_analyses_v2_nooutlier.R (without outlier exclusion)


# -------------------------------------------------------------------------
# Classify trials based on ObjectMarker location
data <-
  data %>%
  dplyr::mutate(
    ObjectMarker_cls = 
      dplyr::if_else(
        abs(ObjectMarker_x) <= 5 & abs(ObjectMarker_z) <= 5,          
        "center", "periphery"
      )
  )

# Test with graph
df_ObjectMarker <-
  data %>%
  dplyr::select("id", "condition", "order", "frame", "newGameState_f",
                "xPos", "yPos", "zPos",
                "Marker_type", "ObjectMarker_x", "ObjectMarker_z", "ObjectMarker_cls",
                "score") %>%
  dplyr::filter(! Marker_type %in% c("Start", "Flag")) %>%
  dplyr::distinct(Marker_type, ObjectMarker_x, ObjectMarker_z, ObjectMarker_cls) %>%
  dplyr::mutate(rowid = dplyr::row_number())  # 138

df_ObjectMarker_score <- 
  data %>%
  dplyr::select("id", "condition", "order", "frame", "newGameState_f",
                "xPos", "yPos", "zPos",
                "Marker_type", "ObjectMarker_x", "ObjectMarker_z", "ObjectMarker_cls",
                "score") %>%
  dplyr::filter(newGameState_f %in% c(9, 16, 17, 18)) %>%  # "9"  = "RO", "16" = "R1O", "17" = "S", "18" = "R2O" 
  dplyr::mutate(newGameState_c = dplyr::recode(as.character(newGameState_f), !!!gamestate_lab))  %>%
  dplyr::select(-newGameState_f) %>%
  dplyr::group_by(id, condition, Marker_type, newGameState_c, ObjectMarker_x, ObjectMarker_z, ObjectMarker_cls) %>%
  dplyr::summarise(mean_score = mean(score, na.rm = TRUE)) %>%
  tidyr::pivot_wider(names_from = newGameState_c, values_from = mean_score) %>%  # id 51.0 bot has only 2 objects => 488/4=122
  dplyr::ungroup() %>%
  dplyr::mutate(rowid = dplyr::row_number())  # 486

df_ObservationMarker <- 
  data %>%
  dplyr::select("id", "condition", "order", "frame", 
                "ObservationMarker", "ObservationMarker_x", "ObservationMarker_z") %>%
  dplyr::distinct(ObservationMarker, ObservationMarker_x, ObservationMarker_z)   # 12 constant Observation points

circleFun <- function(center = c(0,0), r = 1, npoints = 100){
  tt <- seq(0,2*pi, length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

circ_r20 <- circleFun(r = 20, npoints = 100)
circ_r17 <- circleFun(r = 17, npoints = 100)
circ_r7 <- circleFun(r = 7, npoints = 100)
circ_r5 <- circleFun(r = 5, npoints = 100)

plot <- 
  ggplot(data = df_ObjectMarker) +
  geom_path(data = circ_r20, aes(x = x, y = y, radius = 20)) +
  geom_path(data = circ_r17, aes(x = x, y = y, radius = 17)) +
  geom_path(data = circ_r7, aes(x = x, y = y, radius = 7)) +
  geom_path(data = circ_r5, aes(x = x, y = y, radius = 5)) +
  geom_point(data = df_ObservationMarker, aes(x = ObservationMarker_x, y = ObservationMarker_z, type = ObservationMarker), size = 3) +
  scale_x_continuous(breaks = seq(-25, 25, 5)) +
  scale_y_continuous(breaks = seq(-25, 25, 5)) +
  coord_fixed() +
  theme_bw() +
  geom_point(data = df_ObjectMarker, aes(x = ObjectMarker_x, y = ObjectMarker_z, type = Marker_type, color = ObjectMarker_cls))
# png(filename = file.path(folder, "plot_object_map.png"), width = 5, height = 5, units = "in", res = 300)
plot
# dev.off()


# -------------------------------------------------------------------------

# Correlated random slope and intercept   -- x + (x | g) 
# sqrt normalizes the residuals

lmm_mod_5 <-
  data %>%                        
  dplyr::filter(newGameState_f %in% c(9, 16)) %>%
  dplyr::mutate(score = sqrt(score)) %>%
  dplyr::mutate(Marker_type = as.factor(Marker_type),
                order = factor(order),
                age = dplyr::na_if(age, "?"),
                age = as.numeric(age),
                ObjectMarker_cls = as.factor(ObjectMarker_cls)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%   # row_number() - 1 => would start time with 0, but doesn't change results
  dplyr::ungroup() %>%
  lme4::lmer(
    formula = 
      score ~  1 + trial + 
      condition * order + 
      gender +  
      age + 
      ObjectMarker_cls +
      (1 + trial | id), 
    data = .,
    REML = FALSE,
    control = lme4::lmerControl(
      optimizer = "optimx", optCtrl=list(method = "nlminb") # solve failed convergence
    )
  ) 
  
lmm_mod_5 %>%
  lmerTest::as_lmerModLmerTest() %>%
  lmerTest:::summary.lmerModLmerTest()

# png(filename = file.path(folder, "plot_lmm_mod_5.png"), width = 12, height = 14, units = "in", res = 300)
performance::check_model(lmm_mod_5, theme = ggplot2::theme_bw()) #; windows(width = 12, height = 10)    # diagnostic plots
# dev.off()

report::report(lmm_mod_5)

merTools::predictInterval(lmm_mod_5)                   # for various model predictions, possibly with new data
merTools::FEsim(lmm_mod_5)                             # mean, median and sd of the fixed effect estimates
merTools::REsim(lmm_mod_5)                             # mean, median and sd of the random effect estimates

plot_lmm_mod_5_eff <- 
  cowplot::plot_grid(
    merTools::plotFEsim(merTools::FEsim(lmm_mod_5)) + ggtitle("Fixed Effects"),
    merTools::plotREsim(merTools::REsim(lmm_mod_5)) + ggtitle("Random Effects") 
  )
# png(filename = file.path(folder, "plot_lmm_mod_5_eff.png"), width = 10, height = 5, units = "in", res = 300)
plot_lmm_mod_5_eff
# dev.off()

plot_lmm_mod_5_interac <- sjPlot::plot_model(lmm_mod_5, type = "int", terms = c("condition", "order"), ci.lvl = 0.95) + 
  ggtitle("Interaction Plot", subtitle = "Predicted values") +
  theme_bw()
# png(filename = file.path(folder, "plot_lmm_mod_5_interac.png"), width = 5, height = 5, units = "in", res = 300)
plot_lmm_mod_5_interac
# dev.off()

# -------------------------------------------------------------------------
# a test model for narrative

data %>%                        
  dplyr::filter(newGameState_f %in% c(9, 16)) %>%
  dplyr::mutate(score = sqrt(score)) %>%
  dplyr::mutate(Marker_type = as.factor(Marker_type),
                order = factor(order),
                age = dplyr::na_if(age, "?"),
                age = as.numeric(age),
                ObjectMarker_cls = as.factor(ObjectMarker_cls)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%   # row_number() - 1 => would start time with 0, but doesn't change results
  dplyr::ungroup() %>%
  lm(
    formula = 
      score ~  1 + trial + 
      condition * order + 
      gender +  
      age + 
      ObjectMarker_cls
  ) %>%
  summary()

# -------------------------------------------------------------------------
# Order
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
    dplyr::filter(newGameState_f %in% gamestate) %>%
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

plot_empirorder <-
  data %>% 
  my_grouped_ggbetweenstats2(x = order, y = score, group = condition, 
                             outlier.label = id, gamestate = c(9, 16), time_lim = 20, 
                             centrality.label.args = FALSE, title = "Game states: RO and R1O"
  )
# png(filename = file.path(folder, "plot_empirorder.png"), width = 13, height = 7, units = "in", res = 300)
plot_empirorder
# dev.off()


