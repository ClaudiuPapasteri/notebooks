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

# Join with SRQ data









# lmm_mod_6 = lmm_mod_5 + SRQ fixed effects
# Correlated random slope and intercept   -- x + (x | g) 
# sqrt normalizes the residuals

lmm_mod_6 <-
  data %>%                        # CAREFUL, THIS IS WITH NOOUTLIER DATA
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

lmm_mod_6 %>%
  lmerTest::as_lmerModLmerTest() %>%
  lmerTest:::summary.lmerModLmerTest()

# png(filename = file.path(folder, "plot_lmm_mod_6.png"), width = 12, height = 14, units = "in", res = 300)
performance::check_model(lmm_mod_6, theme = ggplot2::theme_bw()) #; windows(width = 12, height = 10)    # diagnostic plots
# dev.off()

report::report(lmm_mod_6)

merTools::predictInterval(lmm_mod_6)                   # for various model predictions, possibly with new data
merTools::FEsim(lmm_mod_6)                             # mean, median and sd of the fixed effect estimates
merTools::REsim(lmm_mod_6)                             # mean, median and sd of the random effect estimates

plot_lmm_mod_6_eff <- 
  cowplot::plot_grid(
    merTools::plotFEsim(merTools::FEsim(lmm_mod_6)) + ggtitle("Fixed Effects"),
    merTools::plotREsim(merTools::REsim(lmm_mod_6)) + ggtitle("Random Effects") 
  )
# png(filename = file.path(folder, "plot_lmm_mod_6_eff.png"), width = 10, height = 5, units = "in", res = 300)
plot_lmm_mod_6_eff
# dev.off()

plot_lmm_mod_6_interac <- sjPlot::plot_model(lmm_mod_6, type = "int", terms = c("condition", "order"), ci.lvl = 0.95) + 
  ggtitle("Interaction Plot", subtitle = "Predicted values") +
  theme_bw()
# png(filename = file.path(folder, "plot_lmm_mod_6_interac.png"), width = 5, height = 5, units = "in", res = 300)
plot_lmm_mod_6_interac
# dev.off()