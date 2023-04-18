require(optimx)
options(device = "RStudioGD")
folder_out <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/lmm_mod_6/graphs/"

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


# -------------------------------------------------------------------------
# Read data
folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean/Clean_Sessions_01.09-24.11.2022/lmm_mod_6"
setwd(folder)   # rstudioapi::getActiveDocumentContext()$path
 
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
file_srq <- "Date_SSMpg_SRQ.xlsx"
data_srq <- rio::import(file.path(folder, file_srq))
data_srq$id <- paste0(as.character(data_srq$ID), ".0")

# checks
all(unique(data$id) %in% unique(data_srq$id))
identical(setdiff(unique(data$id), unique(data_srq$id)), character(0)) 

data <- 
  data %>%
  dplyr::left_join(data_srq, by = "id")

# checks
sum(is.na(data$ID)) == 0
all.equal(as.numeric(data$age), data$Age)       # age in original data has "?" converted to NA; but there also OTHER mismatches
idx_mismatch_age <- which(!(as.numeric(data$age) == data$Age))    
data$id[idx_mismatch_age] %>% unique()
data$age[idx_mismatch_age]; data$Age[idx_mismatch_age]
data %>% dplyr::select(id, age, Age) %>% group_by(id) %>% slice(1) %>% print(n = Inf)
all.equal(data$gender, as.factor(data$Gender))  # gender in original data has 
idx_mismatch_gender <- which(!(data$gender == as.factor(data$Gender))) 
data$id[idx_mismatch_gender] %>% unique()
data %>% dplyr::select(id, gender, Gender) %>% group_by(id) %>% slice(1) %>% print(n = Inf)


# -------------------------------------------------------------------------
# lmm_mod_6 = lmm_mod_5 + SRQ fixed effects
# Correlated random slope and intercept   -- x + (x | g) 
# sqrt normalizes the residuals
lmm_mod_6_df <-
  data %>%                        
  dplyr::filter(newGameState_f %in% c(9, 16)) %>%
  dplyr::mutate(score = sqrt(score)) %>%
  dplyr::mutate(Marker_type = as.factor(Marker_type),
                order = factor(order),
                # age = dplyr::na_if(age, "?"),
                # age = as.numeric(age),
                gender = as.factor(Gender),  # updated to use gender from data_srq that has correct data for ids "75.0" and "82.0"
                age = Age,                   # updated to use age from data_srq which doesn't have NAs and other typos
                ObjectMarker_cls = as.factor(ObjectMarker_cls)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%   # row_number() - 1 => would start time with 0, but doesn't change results
  dplyr::ungroup() 
  
lmm_mod_6 <-  
  lme4::lmer(
    data = lmm_mod_6_df,
    formula = 
      score ~  1 + trial + 
      condition * order + 
      gender + 
      age + 
      ObjectMarker_cls + 
      Prosoc +            # only Prosoc Interaction (kind, reciprocal rels: I enjoy treating others fairly) out of: Admir + NegSocPot + Pass +  SexRel + Soc +
      (1 + trial | id), 
    REML = FALSE,
    control = lme4::lmerControl(
      optimizer = "optimx", optCtrl=list(method = "nlminb") # solve failed convergence
    )
  ) 

lmm_mod_6 %>%
  lmerTest::as_lmerModLmerTest() %>%
  lmerTest:::summary.lmerModLmerTest() 

# png(filename = file.path(folder_out, "plot_lmm_mod_6.png"), width = 12, height = 14, units = "in", res = 300)
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
# png(filename = file.path(folder_out, "plot_lmm_mod_6_eff.png"), width = 10, height = 5, units = "in", res = 300)
plot_lmm_mod_6_eff
# dev.off()

plot_lmm_mod_6_interac <- sjPlot::plot_model(lmm_mod_6, type = "int", terms = c("condition", "order"), ci.lvl = 0.95) + 
  ggtitle("Interaction Plot", subtitle = "Predicted values") +
  theme_bw()
# png(filename = file.path(folder_out, "plot_lmm_mod_6_interac.png"), width = 5, height = 5, units = "in", res = 300)
plot_lmm_mod_6_interac
# dev.off()

# Fitted line per group
lmm_mod_6_gr <- emmeans::ref_grid(lmm_mod_6, cov.reduce = mean, cov.keep = c("condition", "order", "trial"))
lmm_mod_6_emm <- emmeans::emmeans(lmm_mod_6_gr, spec = c("condition", "order", "trial"), level = 0.95) %>%
  as.data.frame()
lmm_mod_6_emm_extremes <- lmm_mod_6_emm %>% filter(trial == min(trial) | trial == max(trial))

plot_lmm_mod_6_fitted <- 
  ggplot(data = lmm_mod_6_df, aes(x = trial, y = score)) +
  geom_point(alpha = 0.1, show.legend = FALSE) +
  geom_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "aquamarine3") + # linear regression
  geom_ribbon(data = lmm_mod_6_emm, aes(ymin = lower.CL, ymax = upper.CL, y = NULL), fill = "red1", alpha = 0.2) +
  geom_line(data = lmm_mod_6_emm, aes(y = emmean), color = "red") +
  facet_wrap(~condition + order) +
  ggrepel::geom_text_repel(data = lmm_mod_6_emm_extremes, aes(y = emmean, label = round(emmean, 2)), 
                           nudge_y = 0.5, size = 3, fontface = "bold", color = "red") +
  ggtitle("LMM fitted line (red) and linear regression on empirical data (aquamarine)") +
  theme_bw()
# png(filename = file.path(folder_out, "plot_lmm_mod_6_fitted.png"), width = 7, height = 7, units = "in", res = 300)
plot_lmm_mod_6_fitted
# dev.off()












# -------------------------------------------------------------------------
###### lmm_mod_5 for reference & update to age & gender
# Correlated random slope and intercept   -- x + (x | g) 
# sqrt normalizes the residuals
lmm_mod_5_df <-
  data %>%                        
  dplyr::filter(newGameState_f %in% c(9, 16)) %>%
  dplyr::mutate(score = sqrt(score)) %>%
  dplyr::mutate(Marker_type = as.factor(Marker_type),
                order = factor(order),
                # age = dplyr::na_if(age, "?"),
                # age = as.numeric(age),
                gender = as.factor(Gender),  # updated to use gender from data_srq that has correct data for ids "75.0" and "82.0"
                age = Age,                   # updated to use age from data_srq which doesn't have NAs and other typos
                ObjectMarker_cls = as.factor(ObjectMarker_cls)) %>%
  group_by(id, condition) %>%
  dplyr::mutate(trial = dplyr::row_number()) %>%   # row_number() - 1 => would start time with 0, but doesn't change results
  dplyr::ungroup() 

lmm_mod_5 <- 
  lme4::lmer(
    data = lmm_mod_5_df,
    formula = 
      score ~  1 + trial + 
      condition * order + 
      gender + 
      age + 
      ObjectMarker_cls + 
      (1 + trial | id), 
    REML = FALSE,
    control = lme4::lmerControl(
      optimizer = "optimx", optCtrl=list(method = "nlminb") # solve failed convergence
    )
  ) 

lmm_mod_5 %>%
  lmerTest::as_lmerModLmerTest() %>%
  lmerTest:::summary.lmerModLmerTest()

# png(filename = file.path(folder_out, "plot_lmm_mod_5.png"), width = 12, height = 14, units = "in", res = 300)
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
# png(filename = file.path(folder_out, "plot_lmm_mod_5_eff.png"), width = 10, height = 5, units = "in", res = 300)
plot_lmm_mod_5_eff
# dev.off()

plot_lmm_mod_5_interac <- sjPlot::plot_model(lmm_mod_5, type = "int", terms = c("condition", "order"), ci.lvl = 0.95) + 
  ggtitle("Interaction Plot", subtitle = "Predicted values") +
  theme_bw()
# png(filename = file.path(folder_out, "plot_lmm_mod_5_interac.png"), width = 5, height = 5, units = "in", res = 300)
plot_lmm_mod_5_interac
# dev.off()

# Fitted line per group
lmm_mod_5_gr <- emmeans::ref_grid(lmm_mod_5, cov.reduce = mean, cov.keep = c("condition", "order", "trial"))
lmm_mod_5_emm <- emmeans::emmeans(lmm_mod_5_gr, spec = c("condition", "order", "trial"), level = 0.95) %>%
  as.data.frame()
lmm_mod_5_emm_extremes <- lmm_mod_5_emm %>% filter(trial == min(trial) | trial == max(trial))

plot_lmm_mod_5_fitted <- 
  ggplot(data = lmm_mod_5_df, aes(x = trial, y = score)) +
    geom_point(alpha = 0.1, show.legend = FALSE) +
    geom_smooth(formula = y ~ x, method = "lm", se = FALSE, color = "aquamarine3") + # linear regression
    geom_ribbon(data = lmm_mod_5_emm, aes(ymin = lower.CL, ymax = upper.CL, y = NULL), fill = "red1", alpha = 0.2) +
    geom_line(data = lmm_mod_5_emm, aes(y = emmean), color = "red") +
    facet_wrap(~condition + order) +
    ggrepel::geom_text_repel(data = lmm_mod_5_emm_extremes, aes(y = emmean, label = round(emmean, 2)), 
                             nudge_y = 0.5, size = 3, fontface = "bold", color = "red") +
    ggtitle("LMM fitted line (red) and linear regression on empirical data (aquamarine)") +
    theme_bw()
# png(filename = file.path(folder_out, "plot_lmm_mod_5_fitted.png"), width = 7, height = 7, units = "in", res = 300)
plot_lmm_mod_5_fitted
# dev.off()