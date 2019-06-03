# R code A13 - teatru & psiho

## Load packages
if (!require("pacman")) install.packages("pacman")
packages <- c(
  "tidyverse", "plyr",      
  "psych", "PerformanceAnalytics",          
  "broom", "rstatix",
  "summarytools", "tadaatoolbox",           
  "ggplot2", "ggpubr", "scales",        
  "rio"
  # , ...
)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = packages)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read, Clean, Recode, Unite
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Read files
folder_teatru <- "E:/Cinetic idei noi/A13/Date grup teatru"
folder_psiho <- "E:/Cinetic idei noi/A13/Date grup psiho"
file_teatru <- "A13 Tabel date COMPLET28ian.xlsx"
file_psiho <- "A13P Tabel centralizat.xlsx"

setwd(folder_teatru)
Data_teatru <- rio::import(file.path(folder_teatru, file_teatru),
                           skip = 1)
setwd(folder_psiho)
Data_psiho <- rio::import(file.path(folder_psiho, file_psiho),
                           skip = 1)


## Tidy up data
# Function coalesce rows: colapse when NA, unite with "_" when not NA
coalesce2 <- function(...) {
  Reduce(function(x, y) {
    i <- which(is.na(x))
    j <- which(!is.na(x) & !is.na(y))
    x[i] <- y[i]
    x[j] <- paste(x[j], y[j], sep = "_")
    x},
    list(...))
}

colnames(Data_teatru) <- coalesce2(Data_teatru[2,], Data_teatru[3,])
Data_teatru <- Data_teatru[-c(1:3),]

colnames(Data_psiho) <- coalesce2(Data_psiho[2,], Data_psiho[3,])
Data_psiho <- Data_psiho[-c(1:3),]


## Check if both data sets have exact same column names
all.equal(colnames(Data_teatru), colnames(Data_psiho))           # not the same
diff_colname <- setdiff(colnames(Data_teatru), colnames(Data_psiho))
ind <- which(colnames(Data_teatru) ==  diff_colname)

colnames(Data_psiho)[ind] <- diff_colname                       # replace with same colname from Data_teatru
all.equal(colnames(Data_teatru), colnames(Data_psiho))           # now they are the same


## Solve duplicate names due to excel double header
# Function to paste a string before column name if it doesnt already start with that string
paste_tocolnames <- function(vec_colnames, string_paste){
  ind <- grep(pattern = string_paste, vec_colnames)                   # ignore column that already has string patterm
  vec_colnames[-ind] <- paste0(string_paste, vec_colnames[-ind])      # paste pattern to the rest of them
  return(vec_colnames)
}

colnames(Data_teatru)[6:21] <- paste_tocolnames(colnames(Data_teatru)[6:21], "APS pre_")
colnames(Data_teatru)[22:35] <- paste_tocolnames(colnames(Data_teatru)[22:35], "PPS pre_")
colnames(Data_teatru)[36:55] <- paste_tocolnames(colnames(Data_teatru)[36:55], "PANAS pre_")
colnames(Data_teatru)[76:79] <- paste_tocolnames(colnames(Data_teatru)[76:79], "SRS post_")
colnames(Data_teatru)[80:99] <- paste_tocolnames(colnames(Data_teatru)[80:99], "PANAS post_")
colnames(Data_teatru)[100:113] <- paste_tocolnames(colnames(Data_teatru)[100:113], "PPS post_")
colnames(Data_teatru)[114:129] <- paste_tocolnames(colnames(Data_teatru)[114:129], "APS post_")

colnames(Data_psiho)[6:21] <- paste_tocolnames(colnames(Data_psiho)[6:21], "APS pre_")
colnames(Data_psiho)[22:35] <- paste_tocolnames(colnames(Data_psiho)[22:35], "PPS pre_")
colnames(Data_psiho)[36:55] <- paste_tocolnames(colnames(Data_psiho)[36:55], "PANAS pre_")
colnames(Data_psiho)[76:79] <- paste_tocolnames(colnames(Data_psiho)[76:79], "SRS post_")
colnames(Data_psiho)[80:99] <- paste_tocolnames(colnames(Data_psiho)[80:99], "PANAS post_")
colnames(Data_psiho)[100:113] <- paste_tocolnames(colnames(Data_psiho)[100:113], "PPS post_")
colnames(Data_psiho)[114:129] <- paste_tocolnames(colnames(Data_psiho)[114:129], "APS post_")
# as.data.frame(colnames(Data_psiho))
# as.data.frame(colnames(Data_teatru))

colnames(Data_teatru) <- enc2native(colnames(Data_teatru))      # fix encoding
colnames(Data_psiho) <- enc2native(colnames(Data_psiho))


## Recode known missing values
# str(Data_psiho, list.len = ncol(Data_psiho))
# str(Data_psiho, list.len = ncol(Data_psiho))
Data_teatru <-
  Data_teatru %>%
  replace(. == "/", NA) %>%                                     # missing values are coded "/"
  replace(. == "-", NA) %>%                                     # missing values are coded "-"
  replace(. == "NA", NA)                                        # missing values are coded "NA"

Data_psiho <-
  Data_psiho %>%
  replace(.=="/", NA) %>%                                       # missing values are coded "/"
  replace(.=="-", NA) %>%                                       # missing values are coded "-"
  replace(. == "NA", NA)                                        # missing values are coded "NA"

  
## Check for non-numeric elements in data sets
check_numeric1 <- as.data.frame(sapply(Data_teatru, varhandle::check.numeric)) 
check_numeric2 <- as.data.frame(sapply(Data_psiho, varhandle::check.numeric))
# sapply(check_numeric1, function(x) length(which(!x)))     # look at columns with non-numeric and count of non-numeric values
# sapply(check_numeric2, function(x) length(which(!x)))

nonnumeric1 <- sapply(check_numeric1, function(x) which(!x, arr.ind = TRUE))    # find row numbers for non-numeric values
nonnumeric2 <- sapply(check_numeric2, function(x) which(!x, arr.ind = TRUE)) 
nonnumeric1[lapply(nonnumeric1, length) > 0]                                   # return only columns and rown numbers were non-numeric
nonnumeric2[lapply(nonnumeric2, length) > 0]

## Recode to numeric
Data_teatru[, -c(1:5)] <- sapply(Data_teatru[, -c(1:5)], as.numeric)    
Data_psiho[, -c(1:5)] <- sapply(Data_psiho[, -c(1:5)], as.numeric)     # cant do this because of encoding:  mutate_at(-c(1:5), ~as.numeric(.))
  
## Correct typos in IDs
#unique(Data_teatru$`Indicativ subiect`)
#unique(Data_psiho$`Indicativ subiect`)
Data_teatru$`Indicativ subiect`[Data_teatru$`Indicativ subiect` == "18.A.1:3"] <- "18.A.1.3"
Data_teatru$`Indicativ subiect`[Data_teatru$`Indicativ subiect` == "26.A.1.3."] <- "26.A.1.3"
Data_teatru$`Indicativ subiect`[Data_teatru$`Indicativ subiect` == "27.A.1.3."] <- "27.A.1.3"

Data_psiho$`Indicativ subiect` <- gsub(".P", "", Data_psiho$`Indicativ subiect`)               # delete .P from IDs
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "16.A.A.1.3"] <- "16.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "1.A.1.3"] <- "01.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "2.A.1.3"] <- "02.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "4.A.1.3"] <- "04.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "0.4.A.1.3"] <- "04.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "5.A.1.3"] <- "05.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "6.A.1.3"] <- "06.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "7.A.1.3"] <- "07.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "8.A.1.3"] <- "08.A.1.3"
Data_psiho$`Indicativ subiect`[Data_psiho$`Indicativ subiect` == "9.A.1.3"] <- "09.A.1.3"
Data_psiho$`Indicativ subiect` <- paste0(Data_psiho$`Indicativ subiect`, ".P")                # add .P to all IDs

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Scoring Questionnaire and Unite
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Define function that calculates RowSums but only for rows with less than 10% NAs; and return NA if all row values are NA 
SpecialRowSums <- function(df, napercent = .1) {
  ifelse(rowSums(is.na(df)) > ncol(df) * napercent,
         NA,
         rowSums(df, na.rm = TRUE) * NA ^ (rowSums(!is.na(df)) == 0)
  )
}

## APS: simple sum
Data_teatru$`APS pre_Total` <- SpecialRowSums(Data_teatru[ ,sprintf("APS pre_%d", 1:16)], napercent = .13)  # not more than 2 NAs for 16 items
Data_teatru$`APS post_Total` <- SpecialRowSums(Data_teatru[ ,sprintf("APS post_%d", 1:16)], napercent = .13)
Data_psiho$`APS pre_Total` <- SpecialRowSums(Data_psiho[ ,sprintf("APS pre_%d", 1:16)], napercent = .13)
Data_psiho$`APS post_Total` <- SpecialRowSums(Data_psiho[ ,sprintf("APS post_%d", 1:16)], napercent = .13)

## PSS-SF 14: Items 4, 5, 6, 7, 9, 10, and 13 are scored in reverse direction.
keys_PSS <- c(1,1,1,-1,-1,-1,-1,1,-1,-1,1,1,-1,1)

Data_teatru$`PPS pre_Total` <- 
  SpecialRowSums(
  psych::reverse.code(items = Data_teatru[ ,sprintf("PPS pre_%d", 1:14)], keys = keys_PSS,  mini = 0, maxi = 4),
  napercent = .1)  # not more than 1 NAs for 14 items 
Data_teatru$`PPS post_Total` <- 
  SpecialRowSums(
    psych::reverse.code(items = Data_teatru[ ,sprintf("PPS post_%d", 1:14)], keys = keys_PSS,  mini = 0, maxi = 4),
    napercent = .1)
Data_psiho$`PPS pre_Total` <- 
  SpecialRowSums(
    psych::reverse.code(items = Data_psiho[ ,sprintf("PPS pre_%d", 1:14)], keys = keys_PSS,  mini = 0, maxi = 4),
    napercent = .1)  # not more than 1 NAs for 14 items 
Data_psiho$`PPS post_Total` <- 
  SpecialRowSums(
    psych::reverse.code(items = Data_psiho[ ,sprintf("PPS post_%d", 1:14)], keys = keys_PSS,  mini = 0, maxi = 4),
    napercent = .1)

## PANAS: Positive Affect Score = sum items 1, 3, 5, 9, 10, 12, 14, 16, 17, 19. Negative Affect Score = sum items 2, 4, 6, 7, 8, 11, 13, 15, 18, 20.
Data_teatru$`PA pre_Total` <- SpecialRowSums(Data_teatru[ ,35 + c(1,3,5,9,10,12,14,16,17,19)], napercent = .11) # not more than 1 NAs for 10 items
Data_teatru$`NA pre_Total` <- SpecialRowSums(Data_teatru[ ,35 + c(2,4,6,7,8,11,13,15,18,20)], napercent = .11)
Data_psiho$`PA pre_Total` <- SpecialRowSums(Data_psiho[ ,35 + c(1,3,5,9,10,12,14,16,17,19)], napercent = .11) 
Data_psiho$`NA pre_Total` <- SpecialRowSums(Data_psiho[ ,35 + c(2,4,6,7,8,11,13,15,18,20)], napercent = .11)

Data_teatru$`PA post_Total` <- SpecialRowSums(Data_teatru[ ,79 + c(1,3,5,9,10,12,14,16,17,19)], napercent = .11) 
Data_teatru$`NA post_Total` <- SpecialRowSums(Data_teatru[ ,79 + c(2,4,6,7,8,11,13,15,18,20)], napercent = .11)
Data_psiho$`PA post_Total` <- SpecialRowSums(Data_psiho[ ,79 + c(1,3,5,9,10,12,14,16,17,19)], napercent = .11) 
Data_psiho$`NA post_Total` <- SpecialRowSums(Data_psiho[ ,79 + c(2,4,6,7,8,11,13,15,18,20)], napercent = .11)


# Define other grouping varibles
Data_teatru <- 
  Data_teatru %>%
    mutate(Etapa = case_when(`Etapã, zi` %in% c("I.1", "I.2") ~ "I",
                             `Etapã, zi` %in% c("II.1",  "II.2") ~ "II",
                             `Etapã, zi` %in% c("III.1", "III.2") ~ "III",
                             `Etapã, zi` %in% c("IV.1", "IV.2") ~ "IV",
                             TRUE ~ NA_character_),
           Zi = case_when(`Etapã, zi` == "I.1" ~ "zi1", 
                          `Etapã, zi` == "I.2" ~ "zi2",  
                          `Etapã, zi` == "II.1" ~ "zi3", 
                          `Etapã, zi` == "II.2" ~ "zi4",
                          `Etapã, zi` == "III.1" ~ "zi5", 
                          `Etapã, zi` == "III.2" ~ "zi6", 
                          `Etapã, zi` == "IV.1" ~ "zi7", 
                          `Etapã, zi` == "IV.2" ~ "zi8", 
                          TRUE ~ NA_character_)) 

Data_psiho <- 
  Data_psiho %>%
  mutate(Etapa = case_when(`Etapã, zi` %in% c("I.1", "I.2") ~ "I",
                           `Etapã, zi` %in% c("II.1",  "II.2") ~ "II",
                           `Etapã, zi` %in% c("III.1", "III.2") ~ "III",
                           `Etapã, zi` %in% c("IV.1", "IV.2") ~ "IV",
                           TRUE ~ NA_character_),
         Zi = case_when(`Etapã, zi` == "I.1" ~ "zi1", 
                        `Etapã, zi` == "I.2" ~ "zi2",  
                        `Etapã, zi` == "II.1" ~ "zi3", 
                        `Etapã, zi` == "II.2" ~ "zi4",
                        `Etapã, zi` == "III.1" ~ "zi5", 
                        `Etapã, zi` == "III.2" ~ "zi6", 
                        `Etapã, zi` == "IV.1" ~ "zi7", 
                        `Etapã, zi` == "IV.2" ~ "zi8", 
                        TRUE ~ NA_character_)) 

## Unite data sets
Data_teatru$Dataset <- rep("teatru", nrow(Data_teatru))
Data_psiho$Dataset <- rep("psiho", nrow(Data_psiho))

Data_United <- rbind(Data_teatru, Data_psiho)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Analyses
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Data_teatru %>% 
#   dplyr::summarize(mean_pre = mean(`APS pre_Total`, na.rm=TRUE),
#                    mean_post = mean(`APS post_Total`, na.rm=TRUE),
#                    mean_diff = mean(`APS pre_Total`, na.rm=TRUE) - mean(`APS post_Total`, na.rm=TRUE),   # m_pre - m_post
#                    sd_pre = sd(`APS pre_Total`, na.rm=TRUE),                    
#                    sd_post = sd(`APS post_Total`, na.rm=TRUE),
#                    n_pre = sum(!is.na(`APS pre_Total`)),                                           # n 
#                    n_post = sum(!is.na(`APS post_Total`)))                                         # n 
# 
# Data_teatru %>%
#   do(broom::tidy(t.test(.$`APS pre_Total`, 
#                         .$`APS post_Total`, 
#                         mu = 0, 
#                         alt = "two.sided", 
#                         # paired = TRUE, 
#                         conf.level = 0.95)))
# 
# t.test(Data_teatru$`APS pre_Total`, Data_teatru$`APS post_Total`)
# wilcox.test(Data_teatru$`APS pre_Total`, Data_teatru$`APS post_Total`)
# 
# t.test(Data_teatru$`PPS pre_Total`, Data_teatru$`PPS post_Total`)
# wilcox.test(Data_teatru$`PPS pre_Total`, Data_teatru$`PPS post_Total`)
# t(summary(Data_teatru[ ,c("APS pre_Total", "APS post_Total", 
#                           "PPS pre_Total", "PPS post_Total")]))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   OUTCOMES PRE-POST INTERVENTION   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function to run all for teatru, psiho and United
func_prepost_tot <- function(df){
  
  Data_melt <-
    df[, c("Indicativ subiect", "Grupa", "Nume Prenume", "Etapã, zi", 
           "APS pre_Total", "APS post_Total", 
           "PPS pre_Total", "PPS post_Total")] %>% 
    gather("APS pre_Total":"PPS post_Total", key = "variable", value = "value")  %>% 
    mutate_at(vars(c(1:5)), funs(as.factor)) %>% 
    mutate(variable = factor(variable, levels = c("APS pre_Total", "APS post_Total", 
                                                  "PPS pre_Total", "PPS post_Total")))
  
  # APS t test - paired
  aps_ttest <- 
    df %>%
    select(`Indicativ subiect`, `APS pre_Total`, `APS post_Total`) %>%
    group_by(`Indicativ subiect`) %>%
    summarise_all(funs(na.omit(.)[1])) %>%                        # squash rows with NAs per id
    # filter_all(all_vars(!is.na(.))) %>%                         # drop row if any column is NA -- dont use here
    do(broom::tidy(t.test(.$`APS pre_Total`,
                          .$`APS post_Total`,
                          mu = 0,
                          alt = "two.sided",
                          paired = TRUE,
                          conf.level = 0.95))) 
  aps_ttest_out <- knitr::kable(aps_ttest, caption = paste0(deparse(substitute(df)), "APS"))
  
  # APS plot t test - unpaired
  aps_plot <-   
    Data_melt %>%
    filter(variable %in% c("APS pre_Total", "APS post_Total")) %>%
    #group_by(`Etapã, zi`) %>%
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot() +
    stat_summary(fun.data = mean_se,  colour = "darkred") +
    xlab("") +
    ggtitle(deparse(substitute(df))) +
    #facet_wrap(~`Etapã, zi`) +
    ggpubr::stat_compare_means(method = "t.test", 
                               label = "p.signif",                                         # to avoid scientific notation of very small p-values
                               #paired = TRUE, 
                               comparisons = list(c("APS pre_Total", "APS post_Total")))  
  
  
  # PPS t test - paired
  pps_ttest <- 
    df %>%
    select(`Indicativ subiect`, `PPS pre_Total`, `PPS post_Total`) %>%
    group_by(`Indicativ subiect`) %>%
    summarise_all(funs(na.omit(.)[1])) %>%                        # squash rows with NAs per id
    # filter_all(all_vars(!is.na(.))) %>%                         # drop row if any column is NA -- dont use here
    do(broom::tidy(t.test(.$`PPS pre_Total`,
                          .$`PPS post_Total`,
                          mu = 0,
                          alt = "two.sided",
                          paired = TRUE,
                          conf.level = 0.95)))
  pps_ttest_out <- knitr::kable(pps_ttest, caption = paste0(deparse(substitute(df)), "PPS")) 
  
  # PPS plot t test - unpaired
  pps_plot <-
    Data_melt %>%
    filter(variable %in% c("PPS pre_Total", "PPS post_Total")) %>%
    #group_by(`Etapã, zi`) %>%
    ggplot(aes(x = variable, y = value)) +
    geom_boxplot() +
    stat_summary(fun.data = mean_se,  colour = "darkred") +
    xlab("") +
    ggtitle(deparse(substitute(df))) +
    #facet_wrap(~`Etapã, zi`) +
    ggpubr::stat_compare_means(method = "t.test", 
                               label = "p.signif",                                         # to avoid scientific notation of very small p-values
                               #paired = TRUE, 
                               comparisons = list(c("PPS pre_Total", "PPS post_Total"))) 
  
  print(aps_plot)
  print(pps_plot)
  return(list(aps_ttest_out, pps_ttest_out))
}


# Teatru
func_prepost_tot(Data_teatru)
# Psiho
func_prepost_tot(Data_psiho)
# United
func_prepost_tot(Data_United)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   OUTCOMES PRE-POST for PANAS    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# PANAS for zi1, zi3, zi5, zi7 are Pre; zi2, zi4, zi6, zi8 are Post 
## Function plot PANAS, compare by zi
plot_panas_zi <- function(df, pre_var, post_var){
  df_modif <-
    df %>%
    gather(pre_var, post_var, key = "variable", value = "value") 
  
  stat.test <-
    df_modif %>%
    rstatix::pairwise_t_test(value ~ Zi) %>%                # pairwise
    # rstatix::adjust_pvalue() %>%                          # (stat_compare_means doesnt do adjusted), parwise is already adjusted here
    rstatix::add_significance() %>%                                                       
    filter(p.signif != "ns") 
  
  p<-
    ggplot(df_modif, aes(y = value, x = Zi)) +
    ggtitle(paste0(deparse(substitute(df)), " : ", pre_var, " - " ,post_var)) + 
    geom_boxplot() + stat_summary(fun.data = mean_se,  colour = "darkred") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  if(nrow(stat.test) > 0){
    p <- 
      p + stat_pvalue_manual(stat.test, label = "p.signif", 
                             y.position = seq(max(df_modif$value, na.rm = TRUE)+2, max(df_modif$value, na.rm = TRUE)*1.4, 
                                          length.out = nrow(stat.test)))                             # very hacky
  }
  
  return(stat.test) %>% print(n = Inf)
  p
}

# Teatru
plot_panas_zi(Data_teatru, "PA pre_Total", "PA post_Total")
plot_panas_zi(Data_teatru, "NA pre_Total", "NA post_Total")
# Psiho
plot_panas_zi(Data_psiho, "PA pre_Total", "PA post_Total")
plot_panas_zi(Data_psiho, "NA pre_Total", "NA post_Total")
# United
plot_panas_zi(Data_United, "PA pre_Total", "PA post_Total")
plot_panas_zi(Data_United, "NA pre_Total", "NA post_Total")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   IOS - pre on begining of etapa, post on end  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function plot pre post data faceted by zi
plot_prepost_zi <- function(df, pre_var, post_var){
  df %>%
    gather(pre_var, post_var, key = "variable", value = "value") %>%
    mutate(variable = factor(variable, levels = c(pre_var, post_var))) %>%
    ggplot(aes(y = value, x = variable)) +
    facet_wrap(~Zi, nrow = 1) + 
    ggtitle(deparse(substitute(df))) + 
    geom_boxplot() + stat_summary(fun.data = mean_se,  colour = "darkred")  +
    ggpubr::stat_compare_means(method = "t.test",
                               label = "p.signif",                                         # to avoid scientific notation of very small p-values
                               #paired = TRUE,
                               comparisons = list(c(pre_var, post_var)))
}


## Function plot pre post data adjusted comparisons of all 
plot_prepost_zi2 <- function(df, pre_var, post_var){
  df_modif <- 
    df %>%
    gather(pre_var, post_var, key = "variable", value = "value") %>%
    mutate(variable = factor(variable, levels = c(pre_var, post_var)),
           condition = interaction(variable, Zi),
           condition = as.factor(condition)) 
  
  stat.test <-
    df_modif %>%
    #group_by(`Indicativ subiect`) %>%
    rstatix::t_test(value ~ condition) %>%
    rstatix::adjust_pvalue() %>%
    rstatix::add_significance() %>%
    filter(p.signif != "ns") 
  
  ggplot(df_modif, aes(y = value, x = condition)) +
    ggtitle(deparse(substitute(df))) + 
    geom_boxplot() + stat_summary(fun.data = mean_se,  colour = "darkred") +
    stat_pvalue_manual(stat.test, label = "p.signif", 
                       y.position = seq(max(df_modif$value, na.rm = TRUE)+2, max(df_modif$value, na.rm = TRUE)*2, 
                                        length.out = nrow(stat.test)))                                                  # very hacky
}



# Teatru
plot_prepost_zi(Data_teatru, "IOS pre", "IOS post")
plot_prepost_zi2(Data_teatru, "IOS pre", "IOS post")
# Psiho
plot_prepost_zi(Data_psiho, "IOS pre", "IOS post")
plot_prepost_zi2(Data_psiho, "IOS pre", "IOS post")
# United
plot_prepost_zi(Data_United, "IOS pre", "IOS post")
plot_prepost_zi2(Data_United, "IOS pre", "IOS post")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~   Variables multiple measurements in zi    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Function plot pre-ex1-ex2-..-post data faceted by zi
plot_preexpost_zi <- function(df, pre_var, post_x1_var, post_x2_var, post_x3_var){
  df_modif <-
    df %>%
    gather(pre_var, post_x1_var, post_x2_var, post_x3_var, key = "variable", value = "value") %>%
    mutate(variable = factor(variable, levels = c(pre_var, post_x1_var, post_x2_var, post_x3_var)))
    
  stat.test <-
    df_modif %>%
    group_by(Zi) %>%
    tidyr::drop_na(value) %>%                                     # filter so grouping factor levels get droped and we can compare with unevel levels
    rstatix::pairwise_t_test(value ~ variable) %>%                # pairwise
    rstatix::add_significance() %>%                                    
    filter(p.signif != "ns") 
  
  p<-
    ggplot(df_modif, aes(y = value, x = variable)) +
      facet_wrap(~Zi, nrow = 1) + 
      ggtitle(deparse(substitute(df))) + 
      geom_boxplot() + stat_summary(fun.data = mean_se,  colour = "darkred") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      stat_pvalue_manual(stat.test, label = "p.signif", 
                         y.position = seq(max(df_modif$value, na.rm = TRUE)+2, max(df_modif$value, na.rm = TRUE)*2, 
                                          length.out = nrow(stat.test)))                                                  # very hacky
  
  return(stat.test) %>% print(n = Inf)
  p
}


# VAS stres 
plot_preexpost_zi(Data_teatru, "VAS stres pre", "VAS stres post ex1", "VAS stres post ex2", "VAS stres post ex3")
plot_preexpost_zi(Data_psiho, "VAS stres pre", "VAS stres post ex1", "VAS stres post ex2", "VAS stres post ex3")
plot_preexpost_zi(Data_United, "VAS stres pre", "VAS stres post ex1", "VAS stres post ex2", "VAS stres post ex3")


# VAS stare de bine 
plot_preexpost_zi(Data_teatru, "VAS stare de bine pre", "VAS stare de bine post ex1", "VAS stare de bine post ex2", "VAS stare de bine post ex3")
plot_preexpost_zi(Data_psiho, "VAS stare de bine pre", "VAS stare de bine post ex1", "VAS stare de bine post ex2", "VAS stare de bine post ex3")
plot_preexpost_zi(Data_United, "VAS stare de bine pre", "VAS stare de bine post ex1", "VAS stare de bine post ex2", "VAS stare de bine post ex3")


# VAS corp 
plot_preexpost_zi(Data_teatru, "VAS corp pre", "VAS corp post ex1", "VAS corp post ex2", "VAS corp post ex3")
plot_preexpost_zi(Data_psiho, "VAS corp pre", "VAS corp post ex1", "VAS corp post ex2", "VAS corp post ex3")
plot_preexpost_zi(Data_United, "VAS corp pre", "VAS corp post ex1", "VAS corp post ex2", "VAS corp post ex3")



