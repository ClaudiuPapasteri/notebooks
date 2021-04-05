# INFO ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# UG 1:
#   https://agile-river-52629.herokuapp.com/
#   Download https://agile-river-52629.herokuapp.com/download/4twyt536wq
# UG 2:
#   https://tranquil-lowlands-63526.herokuapp.com/
#   Download: https://tranquil-lowlands-63526.herokuapp.com/download/4twyt536wq
# 
# 
# Analiza ANOVA 2X2 (Pre-Post X FairRate-UnfairRate)
# Folosim doar answer.csv (fiecare om trece prin 6 fair si 6 unfair)
# - Nu folosim alocator
# - Decident: unfair = 17-24 (6 oferte diferite); fair = 49-54 (6 oferte diferite) 
# - deci avem categ fair/unfair
# - Calc acceptance rate ... cate au fost acceptate pe fair, si cate pe unfair pt fiecare om in parte (deci 100% = 6 ca 6 traieluri sunt)
# - deci o sa avem un procentaj de acceptance pt fiecare om - una pe fair, una pe unfair
# - Calc media feedback ... cat e media fair pe fair, si pe unfair pt fiecare om in parte 
# - 0 = Deloc corect, 100 = Extrem de corect
# - deci o medie feedback pt fiecare om - una pe fair, una pe unfair
#
# verif: 
#   - fiecare are 6, exclus test run (bica.andreea21@gmail.com, ioana.r.podina@gmail.com, test@ro, rozetadraghici@gmail.com), 
# - outlier missmatch acceptance rate
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Packages ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("pacman")) install.packages("pacman")
packages <- c("tidyverse", "tidylog", "readr", "rio")
pacman::p_load(char = packages)
rm(packages)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Read in data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
setwd("E:/Cinetic idei noi/UG met")
pre <- readr::read_csv("PRE_answers.csv")
post <- readr::read_csv("POST_answers.csv")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Exclude known test-IDs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
excluded_id <- c("bica.andreea21@gmail.com", "ioana.r.podina@gmail.com", "test@ro", "rozetadraghici@gmail.com", "ioana.podina@fpse.unibuc.ro",
                 "cociaioana@gmail.com")

pre <-
  pre %>%
  dplyr::filter(!Username %in% excluded_id)

post <-
  post %>%
  dplyr::filter(!Username %in% excluded_id)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check & Exclude IDs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pre %>%
  count(Username) %>%
  print(n = Inf)   # "luciana.haloiu@invingemautismul.ro" has 24 trials instead of 12

post %>%
  count(Username) %>%
  print(n = Inf)   # all have 12 trials = is fine

pre <- 
  pre %>%
  dplyr::group_by(Username) %>%             # exclude last 12 trials of "luciana.haloiu@invingemautismul.ro"
  dplyr::filter(!(Username == "luciana.haloiu@invingemautismul.ro" & dplyr::row_number() %in% 13:24)) 
pre$Timestamp[pre$Username == "luciana.haloiu@invingemautismul.ro"]  # check: 12th trial is at "2020-11-23 16:40:59 UTC"

pre_ids <- data.frame(pre = unique(pre$Username))
post_ids <- data.frame(post = unique(post$Username))
list_ids <- dplyr::full_join(pre_ids, post_ids, by = c("pre" = "post"), keep = TRUE)
list_ids                   

complete_ids <-
  list_ids %>%
  tidyr::drop_na() %>%
  dplyr::mutate(pre = as.character(pre)) %>%
  dplyr::pull(pre)
  
# Keep only IDs that have both PRE and POST
pre <-
  pre %>%
  dplyr::filter(Username %in% complete_ids)

post <-
  post %>%
  dplyr::filter(Username %in% complete_ids)

# Compute scores ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# utilizam doar Decident: unfair = 17-24 (6 oferte diferite); fair = 49-54 (6 oferte diferite)

# Define Fair/Unfair
pre <-
  pre %>%
  dplyr::mutate(Type = cut(Decident, breaks = c(-Inf, 30, Inf), labels = c("Unfair", "Fair")))

post <-
  post %>%
  dplyr::mutate(Type = cut(Decident, breaks = c(-Inf, 30, Inf), labels = c("Unfair", "Fair")))

# Compute Percent scores
pre_scores <-
  pre %>%
  dplyr::add_count(Username, Type, name = "n_Type") %>%
  dplyr::count(Username, Type, Accepted, n_Type, name = "n_Accepted_Type", .drop = FALSE) %>%   # need .drop = FALSE for missing factor levels leading to 0% Percentages
  dplyr::mutate(Percent = 100 * n_Accepted_Type / n_Type) %>%  
  dplyr::ungroup() %>%
  tidyr::complete(Username, Type, Accepted, fill = list(n_Type = 0, n_Accepted_Type = 0, Percent = 0)) %>%  # for missing factor levels leading to 0% Percentages
  dplyr::filter(Accepted == "Y") %>%
  dplyr::mutate(Cond = rep("Pre", n())) %>%
  dplyr::mutate(Cond = factor(Cond, levels = c("Pre", "Post"))) 

post_scores <-
  post %>%
  dplyr::add_count(Username, Type, name = "n_Type") %>%
  dplyr::count(Username, Type, Accepted, n_Type, name = "n_Accepted_Type", .drop = FALSE) %>%   # need .drop = FALSE for missing factor levels leading to 0% Percentages
  dplyr::mutate(Percent = 100 * n_Accepted_Type / n_Type) %>%  
  dplyr::ungroup() %>%
  tidyr::complete(Username, Type, Accepted, fill = list(n_Type = 0, n_Accepted_Type = 0, Percent = 0)) %>%  # for missing factor levels leading to 0% Percentages
  dplyr::filter(Accepted == "Y") %>%
  dplyr::mutate(Cond = rep("Post", n())) %>%
  dplyr::mutate(Cond = factor(Cond, levels = c("Pre", "Post")))
  


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Full scoring procedures but has problems with 0% cases (e.g. no Y answers for Fair ... this should be 0% but is not in data)
# This can be fixed in order to compute Percentages, but there is no way to fix it for Mean_Feedback because there is no Feedback for that combination
# pre_scores_df <-      
#   pre %>%
#   dplyr::add_count(Username, Type, name = "n_Type") %>%
#   dplyr::ungroup() %>%
#   dplyr::add_count(Username, Accepted, Type, name = "n_Accepted_Type") %>%
#   dplyr::mutate(Percent = 100 * n_Accepted_Type / n_Type) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Username, Accepted, Type) %>% 
#   dplyr::mutate(Mean_Feedback = mean(Feedback, na.rm = TRUE)) %>%  
#   dplyr::ungroup()
# 
# pre_scores <- 
#   pre_scores_df %>%
#   dplyr::group_by(Username, Accepted, Type, n_Type, n_Accepted_Type, Percent, Mean_Feedback) %>%
#   dplyr::summarise() %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(Accepted == "Y") %>%
#   dplyr::mutate(Cond = rep("Pre", n()))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Alternative scoring - DON'T USE ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# pre_scores1 <-    # full scoring procedure, not excluding Unaccepted 
#   pre %>%
#   dplyr::group_by(Username, Type, Accepted) %>%
#   dplyr::summarise(n = n(), Mean_Feedback = mean(Feedback)) %>%
#   dplyr::mutate(Percent = n / 12)            # 12 is 100% of trials
# # rio::export(head(pre_scores1, 10), "ex1.xlsx")
# 
# pre_scores2 <-       # scoring procedure with exclusion of Unaccepted
#   pre %>%
#   dplyr::filter(Accepted == "Y") %>%
#   dplyr::group_by(Username, Type) %>%
#   dplyr::summarise(n = n(), Mean_Feedback = mean(Feedback)) %>%
#   dplyr::mutate(Percent = n / sum(n))  
# # rio::export(head(pre_scores2, 7), "ex2.xlsx")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~  
  




# Stats
library(ggstatsplot)
library(rstatix)
library(ggpubr)
library(emmeans)

merged_scores <- rbind(pre_scores, post_scores) 


grouped_ggwithinstats(
  data = merged_scores, 
  x = Cond,
  y = Percent,
  grouping.var = Type,
  type = "parametric",
  pairwise.comparisons = TRUE,
  pairwise.display = "all"
)



# Descriptives
merged_scores %>%
  group_by(Cond, Type) %>%
  rstatix::get_summary_stats(Percent, type = "mean_sd")

# Outliers
merged_scores %>%
  dplyr::group_by(Cond, Type) %>%
  rstatix::identify_outliers(Percent)

# Normality assumption
model <- lm(Percent ~ Cond * Type, data = merged_scores)
ggpubr::ggqqplot(residuals(model))

# Compute Shapiro-Wilk test of normality
rstatix::shapiro_test(residuals(model))

# Check normality assumption by groups
merged_scores %>%
  dplyr::group_by(Cond, Type) %>%
  rstatix::shapiro_test(Percent)

ggpubr::ggqqplot(merged_scores, "Percent", ggtheme = theme_bw()) +
  ggplot2::facet_grid(Cond ~ Type)

# Homogneity of variance assumption
merged_scores %>%
  rstatix::levene_test(Percent ~ Cond * Type)

# 2X2 ANOVA
res_aov <- 
  merged_scores %>% 
  rstatix::anova_test(Percent ~ Cond * Type)
res_aov

# Pairwise comparisons  ---- AICI INCEPE PROBL
pwc <- 
  merged_scores %>% 
  group_by(Type) %>%
  rstatix::emmeans_test(Percent ~ Cond, p.adjust.method = "bonferroni") 
pwc

# Visualization: box plots with p-values
bxp <- 
  merged_scores %>%
  ggpubr::ggboxplot(x = "Cond", y = "Percent", color = "Type", palette = "jco")

pwc <- 
  pwc %>% 
  add_xy_position(x = "Cond")

bxp +
  stat_pvalue_manual(pwc) +
  labs(
    subtitle = get_test_label(res_aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
