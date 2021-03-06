## O.2 Analyses for Frontiers Article

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!require("pacman")) install.packages("pacman")
packages <- c(
  "tidyverse",      # best thing that happend to me
  "psych",          # general purpose toolbox for personality, psychometric theory and experimental psychology
  "papaja",         # for APA style
  "broom",          # for tidy modelling
  "ggplot2",        # best plots
  "ggpubr",         # ggplot2 to publication quality
  "DT",             # nice searchable and downloadable tables
  "summarytools",
  "plyr", 
  "rio"
  # , ...
)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = packages)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read and Merge
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wd <- "E:/Cinetic idei noi/EXPERIMENTE OGL Frontiers (O.2 & O.0.3 & O.0.2)"
setwd(wd)

DataTrust <- rio::import(file.path(wd, "O.2 REZULTATE\\O.2 Date PrelucrareSPSS cu NEO si STAI", "O.2 Date pt Trust BUN cu NEO si STAIY.xlsx"))
DataDG <- rio::import(file.path(wd, "O.2 REZULTATE\\O.2 Date PrelucrareSPSS cu NEO si STAI", "O.2 Date pt DG BUN cu NEO si STAIY.xlsx"))
DataVAS <- rio::import(file.path(wd, "O.2 REZULTATE\\O.2 VAS,IOS", "O.2 Date PrelucrareSPSS.xlsx"))
DataBIO <- rio::import(file.path(wd, "O.2 BIO", "O.2 Ox si Cortizol.xlsx"))
  
Data_merge1 <- merge(DataVAS, DataTrust)  
Data_merge2 <- merge(Data_merge1, DataDG) 
Data_merge3 <- merge(Data_merge2, DataBIO)

Data <- Data_merge3

test_names <- unique(unlist(lapply(list(DataTrust, DataDG, DataVAS, DataBIO), names)))
merge_names <- names(Data)

if(identical(merge_names[order(merge_names)], test_names[order(test_names)])){    # the order matters in identical()
  cat("Merge was succesful")
  rm("Data_merge1", "Data_merge2", "Data_merge3", "DataBIO", "DataDG", "DataTrust", "DataVAS", "test_names", "merge_names")
}else cat("Merge unsuccesful")  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Derive new variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Data$D_VasS_Poz <- Data[, "VasS_postPoz"] - Data[, "VasS_prePoz"] 
Data$D_VasS_Neg <- Data[, "VasS_postNeg"] - Data[, "VasS_preNeg"]
Data$D_VasB_Poz <- Data[, "VasB_postPoz"] - Data[, "VasB_prePoz"] 
Data$D_VasB_Neg <- Data[, "VasB_postNeg"] - Data[, "VasB_preNeg"]
Data$D_IOS_Poz <- Data[, "IOS_postPoz"] - Data[, "IOS_prePoz"] 
Data$D_IOS_Neg <- Data[, "IOS_postNeg"] - Data[, "IOS_preNeg"]

Data$D_Sam1_Poz <- Data[, "Sam1_postPoz"] - Data[, "Sam1_prePoz"] 
Data$D_Sam1_Neg <- Data[, "Sam1_postNeg"] - Data[, "Sam1_preNeg"]
Data$D_Sam2_Poz <- Data[, "Sam2_postPoz"] - Data[, "Sam2_prePoz"] 
Data$D_Sam2_Neg <- Data[, "Sam2_postNeg"] - Data[, "Sam2_preNeg"]
Data$D_Sam3_Poz <- Data[, "Sam3_postPoz"] - Data[, "Sam3_prePoz"] 
Data$D_Sam3_Neg <- Data[, "Sam3_postNeg"] - Data[, "Sam3_preNeg"]

Data$D_DG_Poz <- Data[, "DG_postPozTot"] - Data[, "DG_prePozTot"] 
Data$D_DG_Neg <- Data[, "DG_postNegTot"] - Data[, "DG_preNegTot"]

Data$D_TrustMin_Poz <- Data[, "TrustMinPozPost"] - Data[, "TrustMinPozPre"] 
Data$D_TrustMin_Neg <- Data[, "TrustMinNegPost"] - Data[, "TrustMinNegPre"]
Data$D_TrustTot_Poz <- Data[, "TrustTotPozPost"] - Data[, "TrustTotPozPre"] 
Data$D_TrustTot_Neg <- Data[, "TrustTotNegPost"] - Data[, "TrustTotNegPre"]

Data$D_Cort_Poz <- Data[, "Cort_post_Poz"] - Data[, "Cort_pre_Poz"] 
Data$D_Cort_Neg <- Data[, "Cort_post_Neg"] - Data[, "Cort_pre_Neg"]
Data$D_Ox_Poz <- Data[, "Ox_post_Poz"] - Data[, "Ox_pre_Poz"] 
Data$D_Ox_Neg <- Data[, "Ox_post_Neg"] - Data[, "Ox_pre_Neg"]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define Function for mining correlations
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function for p-value significance -- both for func_ancova_multibox(), Get_Top_Relationships() and Correlations_With_One()
stars_signif <- function(pval) {
  stars = "ns"
  if(pval <= 0.001)
    stars = "***"
  if(pval > 0.001 & pval <= 0.01)
    stars = "**"
  if(pval > 0.01 & pval <= 0.05)
    stars = "*"
  if(pval > 0.05 & pval <= 0.1)
    stars = "."
  stars
}

## Function that returns correlations of all variables in descending order.
# Arg for threshold with default at .3 will keep only correlantions above .3 and below -.3. Also has threshhold for p-value. 
Get_Top_Relationships <- function(data_set, 
                                  correlation_abs_threshold=0.3,
                                  pvalue_threshold=0.05) {
  require(psych)
  require(dplyr)
  feature_names <- names(data_set)
  # strip var names to index for pair-wise identification
  names(data_set) <- seq(1:ncol(data_set))
  # calculate correlation and significance numbers
  cor_data_df <- psych::corr.test(data_set)
  # apply var names to correlation matrix over index
  rownames(cor_data_df$r) <- feature_names
  colnames(cor_data_df$r) <- feature_names
  # top cor and sig
  relationships_set <- cor_data_df$ci[,c('r','p')]
  # apply var names to data over index pairs
  relationships_set$feature_1 <- feature_names[as.numeric(sapply(strsplit(rownames(relationships_set), "-"), `[`, 1))]
  relationships_set$feature_2 <- feature_names[as.numeric(
    sapply(strsplit(rownames(relationships_set), "-"), `[`, 2))]
  relationships_set <- dplyr::select(relationships_set, feature_1, feature_2, r, p) %>% dplyr::rename(correlation = r, p.value = p)
  # return only the most insteresting relationships
  return(filter(relationships_set, abs(correlation) > correlation_abs_threshold &
                  p.value < pvalue_threshold) %>% 
        arrange(p.value) %>%
        mutate(p.signif = sapply(p.value, function(x) stars_signif(x))))
}

## Function that returns all correlation between numeric variables and one specific variable
Correlations_With_One <- function(data_set,
                            variable,
                            correlation_abs_threshold=0.3,
                            pvalue_threshold=0.05) {
  require(psych)
  require(dplyr)
  # use all numeric columns only
  numeric_cols <- unlist(lapply(data_set, is.numeric))
  data_set <- data_set[, numeric_cols]                               
  # calculate correlation and significance numbers
  cor_data_df <- psych::corr.test(data_set[, names(data_set) != variable], data_set[, variable], minlength = 20, adjust="none")
  # top cor and sig
  relationships_set <- as.data.frame(cbind(cor_data_df$r, cor_data_df$p))     # same as  cor_data_df$ci[,c('r','p')]
  relationships_set <- tibble::rownames_to_column(relationships_set, "Variable")   # relationships_set$Variable <- rownames(relationships_set)
  colnames(relationships_set) <- c("Variable", "correlation", "p.value")
  # return only the most insteresting relationships
  cat("Correlations with ", variable, "\n")
  return(filter(relationships_set, abs(correlation) > correlation_abs_threshold &
                  p.value < pvalue_threshold) %>% 
           arrange(p.value) %>%
           mutate(p.signif = sapply(p.value, function(x) stars_signif(x)))) %>%
           tibble::as.tibble()
}  


## Function for ploting correlation data frames resulting from Get_Top_Relationships and Correlations_With_One()
func_dotplot_cor <- function(df){                                        # https://www.r-pkg.org/pkg/ggpubr
  dotplotcor_scale_fill <- function(...){                                # Fix colors to signif factor levels even if missing
    ggplot2:::manual_scale(                                   
      'color', 
      values = setNames(
        c("darkgreen", "green3", "lawngreen", "yellow", "red"), 
        c("***", "**", "*", ".", "ns")), 
      ...
    )
  }                                           
  
  dtoplot_theme <- 
    ggpubr::theme_pubr() +
    theme(axis.text.y = element_text(size = 10))
  
  if(!"Variable" %in% colnames(df)){                                             # in oder to work for both Get_Top_Relationships and Correlations_With_One()
  df <- 
    df %>%                                            
      unite(cor_between, c("feature_1", "feature_2"), sep = " X ")               # unite 2 columns to x name from plot
  }else df <- df %>% dplyr::rename(cor_between = Variable)                       # change Variable to x name from plot
  
  df %>%
    ggpubr::ggdotchart(x = "cor_between", y = "correlation",
                       color = "p.signif",                                       # Color by sig
                       #   palette = c("#00AFBB", "#E7B800", "#FC4E07"),         # Custom color palette
                       sorting = "descending",                                   # Sort value in descending order
                       add = "segments",                                         # Add segments from y = 0 to dots
                       add.params = list(color = "lightgray", size = 2),         # Change segment color and size
                       group = "p.signif",                                       # Order by groups
                       dot.size = 8,                                             # Large dot size
                       xlab = "",
                       rotate = TRUE,                                            # Rotate vertically
                       label = round(.$correlation, 1),                          # Add mpg values as dot labels
                       font.label = list(color = "white", size = 9, 
                                         vjust = 0.5),                           # Adjust label parameters
                       ggtheme = dtoplot_theme) +                                # ggplot2 theme
    dotplotcor_scale_fill() +                                            # Fix colors to signif factor levels even if missing
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray")
}

  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define Function for Pre-Post Plots, t Change and ANCOVA Post
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Func t test si boxplot simplu
func_t_box <- function(df, ind, pre_var, post_var){
  df_modif <-
    df %>%
    select(ind, pre_var, post_var) %>% 
    tidyr::drop_na() %>%
    gather(pre_var, post_var, key = "Cond", value = "value") %>% 
    mutate_at(vars(c(1, 2)), funs(as.factor)) %>% 
    mutate(Cond = factor(Cond, levels = c(pre_var, post_var))) 
  
  stat_comp <- ggpubr::compare_means(value ~ Cond, data = df_modif, method = "t.test", paired = TRUE)
  
  plot <- 
    ggpubr::ggpaired(df_modif, x = "Cond", y = "value", id = ind, 
                     color = "Cond", line.color = "gray", line.size = 0.4,
                     palette = c("#00AFBB", "#FC4E07"), legend = "none") +
      stat_summary(fun.data = mean_se,  colour = "darkred") +
      ggpubr::stat_compare_means(method = "t.test", paired = TRUE, label.x = as.numeric(df_modif$Cond)-0.4, label.y = max(df_modif$value)+0.5) + 
      ggpubr::stat_compare_means(method = "t.test", paired = TRUE, label = "p.signif", comparisons = list(c(pre_var, post_var)))
  
  cat(paste0(pre_var, " ", post_var, "\n", "\n"))
  print(stat_comp)
  print(plot)
}


func_ancova_multibox <- function(df, ind, pre_var_c1, post_var_c1, pre_var_c2, post_var_c2){
  
  diff_score_c1 <- paste0(post_var_c1, " - ", pre_var_c1)
  diff_score_c2 <- paste0(post_var_c2, " - ", pre_var_c2)
  
  ## Plots and p-values for t tests
  df_modif <-
    df %>%
    select(ind, pre_var_c1, post_var_c1, pre_var_c2, post_var_c2) %>% 
    tidyr::drop_na() %>%
    gather(pre_var_c1, post_var_c1, pre_var_c2, post_var_c2, key = "Cond", value = "value") %>% 
    mutate_at(vars(c(1, 2)), funs(as.factor)) %>% 
    mutate(Cond = factor(Cond, levels = c(pre_var_c1, post_var_c1, pre_var_c2, post_var_c2))) 
  
  stat_comp <- ggpubr::compare_means(value ~ Cond, data = df_modif, method = "t.test", paired = TRUE, p.adjust.method = "holm")
  
  plot <-
    ggpubr::ggpaired(df_modif, x = "Cond", y = "value", id = ind, 
                     color = "Cond", line.color = "gray", line.size = 0.4,
                     palette = c("#00AFBB", "#FC4E07", "#00AFBB", "#FC4E07"), legend = "none") +
    stat_summary(fun.data = mean_se,  colour = "darkred") +
    ggpubr::stat_compare_means(method = "t.test", paired = TRUE, label = "p.signif", 
                               label.y = c(max(df_modif$value) + 0.1*IQR(df_modif$value),
                                           max(df_modif$value) + 0.1*IQR(df_modif$value),
                                           seq(max(df_modif$value) + 0.3*IQR(df_modif$value), 
                                               max(df_modif$value) + 0.9*IQR(df_modif$value), length.out = 4)),  
                               comparisons = list(c(pre_var_c1, post_var_c1),
                                                  c(pre_var_c2, post_var_c2),
                                                  c(post_var_c1, pre_var_c2),
                                                  c(pre_var_c1, pre_var_c2),
                                                  c(post_var_c1, post_var_c2),
                                                  c(pre_var_c1, post_var_c2)))
  
  ## For ttestChange or ANCOVAChange - we do ttestChange (Post-Pre) here, but it isnt very important
  df_modif2 <-                                 
    df %>%
    select(ind, pre_var_c1, post_var_c1, pre_var_c2, post_var_c2) %>%
    tidyr::drop_na() 
  df_modif2[diff_score_c1] <- df_modif2[, post_var_c1] - df_modif2[, pre_var_c1]
  df_modif2[diff_score_c2] <- df_modif2[, post_var_c2] - df_modif2[, pre_var_c2]
  
  tChange <- t.test(df_modif2[, diff_score_c1], df_modif2[, diff_score_c2], paired = TRUE)
  
  ## For descriptives by 2 factors (PrePost and PozNeg)
  df_modif3 <-
    df %>%
    select(ind, pre_var_c1, post_var_c1, pre_var_c2, post_var_c2) %>%
    tidyr::drop_na() %>%
    gather(pre_var_c1, post_var_c1, pre_var_c2, post_var_c2, key = "Cond", value = "value") %>%
    mutate(PrePost = case_when(stringr::str_detect(.$"Cond", "pre|Pre") ~ "Pre",
                               stringr::str_detect(.$"Cond", "post|Post") ~ "Post",
                               TRUE ~ NA_character_),
           PozNeg = case_when(stringr::str_detect(.$"Cond", "poz|Poz") ~ "Poz",
                              stringr::str_detect(.$"Cond", "neg|Neg") ~ "Neg",
                              TRUE ~ NA_character_)) 
  
  ## For ANCOVAPost - this is what we use
  df_modif4 <-
    df_modif3 %>%
    select(-"Cond") %>%
    spread("PrePost", "value")
  
  ## Models (here we use ANCOVAPost)    # https://m-clark.github.io/docs/mixedModels/anovamixed.html#introduction
  mod_ancovaPost = lm(Post ~ Pre + PozNeg, data = df_modif4)
  # mod_ancovaPost = lm(post ~ pre + treat)      # exactly the same with aov(post ~ pre + treat)
  # summary(mod_ancovaPost)
  # 
  # mod_anovaRM = aov(score ~ treat*time + Error(id), dflong)
  # summary(mod_anovaRM)
  # 
  # mod_lme = lme4::lmer(score ~ treat*time + (1|id), data=dflong)
  # anova(lmeModel)
  
  ## Output
  print(plot)
  cat(paste0(pre_var_c1, " ", post_var_c1, " ", pre_var_c2, " ", post_var_c2, "\n", "\n"))
  
  cat("Descriptives")
  psych::describeBy(df_modif3[, "value"], list(df_modif3[, "PrePost"], df_modif3[, "PozNeg"]), mat = TRUE) %>% 
    as.tibble() %>%
    print()
  cat("\n")
  
  print(stat_comp)
  cat("\n")
  
  cat("t Change")
  tidy(tChange) %>% print()
  cat("\n")
  
  cat("ANCOVA Post")
  # print(list(summary(mod_ancovaPost), coef(mod_ancovaPost)))
  tidy(mod_ancovaPost) %>% 
    mutate(p.signif = sapply(p.value, function(x) stars_signif(x))) %>% 
    print()
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Correlations between Diffrence Scores with other variables
Correlations_With_One(Data[,-c(1:7)], variable = "D_Ox_Poz", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # D_DG_Poz, Neo_C4, Neo_E3
Correlations_With_One(Data[,-c(1:7)], variable = "D_Ox_Neg", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_Cort_Poz", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05) # D_Sam1_Poz, D_TrustTot_Poz, StaiS
Correlations_With_One(Data[,-c(1:7)], variable = "D_Cort_Neg", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_VasS_Poz", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_VasS_Neg", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # D_TrustTot_Neg
Correlations_With_One(Data[,-c(1:7)], variable = "D_VasB_Poz", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # D_Sam1_Poz
Correlations_With_One(Data[,-c(1:7)], variable = "D_VasB_Neg", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # D_Sam1_Neg
Correlations_With_One(Data[,-c(1:7)], variable = "D_IOS_Poz", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_IOS_Neg", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_DG_Poz", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_DG_Neg", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_TrustMin_Poz", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_TrustMin_Neg", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_TrustTot_Poz", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # nimic
Correlations_With_One(Data[,-c(1:7)], variable = "D_TrustTot_Neg", correlation_abs_threshold = 0.2, pvalue_threshold = 0.05)  # D_VasS_Neg

## Simple before-after analyses with t test
func_t_box(Data, "Indicativ", "Ox_pre_Poz", "Ox_post_Poz")  # sig  
func_t_box(Data, "Indicativ", "Ox_pre_Neg", "Ox_post_Neg")  # sig
func_t_box(Data, "Indicativ", "Cort_pre_Poz", "Cort_post_Poz")  # nu  
func_t_box(Data, "Indicativ", "Cort_pre_Neg", "Cort_post_Neg")  # sig

func_t_box(Data, "Indicativ", "VasS_prePoz", "VasS_postPoz")  # sig  
func_t_box(Data, "Indicativ", "VasS_preNeg", "VasS_postNeg")  # nu
func_t_box(Data, "Indicativ", "VasB_prePoz", "VasB_postPoz")  # sig  
func_t_box(Data, "Indicativ", "VasB_preNeg", "VasB_postNeg")  # nu
func_t_box(Data, "Indicativ", "Sam1_prePoz", "Sam1_postPoz")  # sig  
func_t_box(Data, "Indicativ", "Sam1_preNeg", "Sam1_postNeg")  # nu
func_t_box(Data, "Indicativ", "Sam2_prePoz", "Sam2_postPoz")  # nu  
func_t_box(Data, "Indicativ", "Sam2_preNeg", "Sam2_postNeg")  # nu
func_t_box(Data, "Indicativ", "IOS_prePoz", "IOS_postPoz")  # sig  
func_t_box(Data, "Indicativ", "IOS_preNeg", "IOS_postNeg")  # nu

func_t_box(Data, "Indicativ", "DG_prePozTot", "DG_postPozTot")  # nu  
func_t_box(Data, "Indicativ", "DG_preNegTot", "DG_postNegTot")  # sig

func_t_box(Data, "Indicativ", "TrustMinPozPre", "TrustMinPozPost")  # nu  
func_t_box(Data, "Indicativ", "TrustMinNegPre", "TrustMinNegPost")  # nu
func_t_box(Data, "Indicativ", "TrustTotPozPre", "TrustTotPozPost")  # nu  
func_t_box(Data, "Indicativ", "TrustTotNegPre", "TrustTotNegPost")  # nu

## tChance and ANCOVAPost 
func_ancova_multibox(Data, "Indicativ", "Ox_pre_Poz", "Ox_post_Poz", "Ox_pre_Neg", "Ox_post_Neg")
func_ancova_multibox(Data, "Indicativ", "Cort_pre_Poz", "Cort_post_Poz", "Cort_pre_Neg", "Cort_post_Neg")

func_ancova_multibox(Data, "Indicativ", "VasS_prePoz", "VasS_postPoz", "VasS_preNeg", "VasS_postNeg")
func_ancova_multibox(Data, "Indicativ", "VasB_prePoz", "VasB_postPoz", "VasB_preNeg", "VasB_postNeg")
func_ancova_multibox(Data, "Indicativ", "Sam1_prePoz", "Sam1_postPoz", "Sam1_preNeg", "Sam1_postNeg")
func_ancova_multibox(Data, "Indicativ", "Sam2_prePoz", "Sam2_postPoz", "Sam2_preNeg", "Sam2_postNeg")
func_ancova_multibox(Data, "Indicativ", "IOS_prePoz", "IOS_postPoz", "IOS_preNeg", "IOS_postNeg") 

func_ancova_multibox(Data, "Indicativ", "DG_prePozTot", "DG_postPozTot", "DG_preNegTot", "DG_postNegTot") 

func_ancova_multibox(Data, "Indicativ", "TrustMinPozPre", "TrustMinPozPost", "TrustMinNegPre", "TrustMinNegPost") 
func_ancova_multibox(Data, "Indicativ", "TrustTotPozPre", "TrustTotPozPost", "TrustTotNegPre", "TrustTotNegPost") 




## Mining Correlations with Oxy
df_OxyAll_cor <-
  Get_Top_Relationships(Data[,-c(1:7)], correlation_abs_threshold = 0.2, pvalue_threshold = 0.05) %>%
    dplyr::as_tibble() %>%
    dplyr::filter_all(any_vars(grepl("Ox", .)))                           # only Oxy, but both Pre and Post Oxy

df_OxyAll_cor %>%              
    print(n = Inf)                                     
df_OxyAll_cor %>%
  func_dotplot_cor()
 
# Correlations only on Pre measures with Oxy -- nothing
df_OxyPre_cor <- 
  Get_Top_Relationships(Data[,-c(1:7)], correlation_abs_threshold = 0.2, pvalue_threshold = 0.1) %>%
    dplyr::as_tibble() %>%
    filter_at(vars(feature_1, feature_2), all_vars(grepl("pre|Pre", .))) %>%
    dplyr::filter_all(any_vars(grepl("Ox", .))) 

df_OxyPre_cor %>%
  print(n = Inf)

# Correlations only on Post measures with Oxy
df_OxyPost_cor <-
  Get_Top_Relationships(Data[,-c(1:7)], correlation_abs_threshold = 0.2, pvalue_threshold = 0.1) %>%
    dplyr::as_tibble() %>%
    filter_at(vars(feature_1, feature_2), all_vars(grepl("post|Post", .))) %>%
    dplyr::filter_all(any_vars(grepl("Ox", .))) 

df_OxyPost_cor %>%
    print(n = Inf)
df_OxyPost_cor %>% 
  func_dotplot_cor()
  



