##############################################################################################################################################
## NYC-QR Scoring 
##############################################################################################################################################
library(psych)
library(tidyverse)

setwd("C:/Users/Mihai/Desktop/O.4 prealabil pt Frontiers/O.4 Scale Scoring")

## Define function copy paste from excel
paste.excel <- function(header = TRUE, ...) {
  read.table("clipboard", sep = "\t", header = header, ...)
}

## Define function copy paste to excel
write.excel <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  write.table(x, "clipboard", sep = "\t", dec = ",", row.names = row.names, col.names = col.names, ...)
}

## Read Pattern Matrix from original article
first_fit <- readRDS("first_fit.RDS")
second_fit <- readRDS("second_fit.RDS") 

## FUNCTION FOR SCORING
score_nycqr <- function(Data_othervars, Data_nycqr){
  first_index <-  c(1:9, 11:23)        # first 23 items but item 10 is omited in scoring!!!!
  second_index <- c(24:31)             # last 8 items
  
  names_first_index <- c("Q01", "Q02", "Q03", "Q04", "Q05", "Q06", "Q07", "Q08", "Q09", "Q11",
                         "Q12", "Q13", "Q14", "Q15", "Q16", "Q17",
                         "Q18", "Q19", "Q20", "Q21", "Q22", "Q23")
  
  names_second_index <- c("Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31")
  
  Data_nycqr <- 
    Data_nycqr %>%
    dplyr::rename_at(first_index, ~ names_first_index) %>%
    dplyr::rename_at(second_index, ~ names_second_index)       # CAREFUL item 10 is still in dataframe
  
  
  ## Check for non-numeric elements in data sets
  check_numeric <- as.data.frame(sapply(Data_nycqr, varhandle::check.numeric)) 
  nonnumeric <- sapply(check_numeric, function(x) which(!x, arr.ind = TRUE))   # find row numbers for non-numeric values
  cat("Non-numeric elements: \n")
  print(nonnumeric[lapply(nonnumeric, length) > 0])                            # return only columns and row numbers were non-numeric
  
  ## Remove rows with non-numeric elements
  if(length(nonnumeric[lapply(nonnumeric, length) > 0])  != 0){
    Data_nycqr <- Data_nycqr[-unique(unlist(nonnumeric)), ]
    Data_othervars <- Data_othervars[-unique(unlist(nonnumeric)), ]              # must delete from all to keep row.number constant
  }
    
  ## Recode to numeric
  Data_nycqr <- 
    Data_nycqr %>% 
    mutate_if(function(x) is.factor(x) | is.character(x), funs(as.numeric(as.character(.))))
  
  ## Derive Factor Scores
  first_factscores <- psych::factor.scores(x = Data_nycqr[, first_index], f = first_fit, method = "tenBerge")
  df_first_factscores <- 
    first_factscores$scores %>% 
    as.data.frame() %>%
    dplyr::rename_at(vars(c("PA3", "PA2", "PA1", "PA4", "PA5")), ~ c("Past", "Positive", "Future", "Negative", "Friends"))
  
  second_factscores <- psych::factor.scores(x = Data_nycqr[, second_index], f = second_fit, method = "tenBerge")
  df_second_factscores <- 
    second_factscores$scores %>% 
    as.data.frame() %>%
    dplyr::rename_at(vars(c("PA2", "PA1", "PA3")), ~ c("Vague", "Words", "Images"))
  
  ## Merge with demographic data and rename after
  # bla <- 
  # dplyr::left_join(rownames_to_column(Data_othervars), rownames_to_column(df_first_factscores), by = "rowname") %>%
  #   dplyr::left_join(., rownames_to_column(df_second_factscores), by = "rowname")
  
  Data <- cbind(Data_othervars, df_first_factscores, df_second_factscores)
  
  return(Data)
}

## Read Data -- Read for Pre and Read for Post
Data_othervars <- paste.excel()   
Data_nycqr <- paste.excel()       # select for pre

Date_pre <- score_nycqr(Data_othervars, Data_nycqr)

Data_nycqr <- paste.excel()       # select for post

Date_post <- score_nycqr(Data_othervars, Data_nycqr)



## Copy paste to excel
write.excel(Date_pre)
write.excel(Date_post)

#######
# -- from here te script works interactively: rename ID&Condition, 
#######































# ## Rerun Author code (doi: 10.1371/journal.pone.0097176)
# library(psych)
# setwd("C:/Users/Mihai/Desktop/O.4 prealabil pt Frontiers/O.4 Scale Scoring")
# data <- read.csv("https://raw.githubusercontent.com/NeuroanatomyAndConnectivity/NYC-Q/master/data/mriq_all_subs_18_60.csv")
# first_ind = c(4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 
#               22, 23, 24, 25, 26)
# second_ind = c(27, 28, 29, 30, 31, 32, 33, 34)
# data$sex = factor(data$sex, levels = c(1, 2), labels = c("male", "female"))
# 
# first_fit = psych::fa(data[first_ind], nfactors=5, scores="tenBerge", fm="pa", rotate="oblimin")
# first_fit    # PA3   PA2   PA1   PA4   PA5 = Past  Positive  Future  Negative  Friends
# first_fit$scores
# # saveRDS(first_fit, file = "first_fit.RDS")
# 
# second_fit = psych::fa(data[second_ind], nfactors = 3, scores = "tenBerge", fm = "pa", rotate = "oblimin") 
# second_fit   # PA2   PA1   PA3  = +Vague/-Specific  Words  Images             
# second_fit$scores
# # saveRDS(second_fit, file = "second_fit.RDS")
# 
# ## Generate random Likert data 1-9 for TEST
# first_gen_raw_data <- as.data.frame(matrix(floor(runif(2200, 1, 9)), ncol = 22)) 
# colnames(first_gen_raw_data) <- c("Q01", "Q02", "Q03", "Q04", "Q05", "Q06", "Q07", "Q08", "Q09", "Q11",
#                             "Q12", "Q13", "Q14", "Q15", "Q16", "Q17",
#                             "Q18", "Q19", "Q20", "Q21", "Q22", "Q23")
#              
#              
# second_gen_raw_data <- as.data.frame(matrix(floor(runif(800, 1, 9)), ncol = 8)) 
# colnames(second_gen_raw_data) <- c("Q24", "Q25", "Q26", "Q27", "Q28", "Q29", "Q30", "Q31")             
#              
# 
# ## Scoring new data for TEST
# first_factscores <- psych::factor.scores(x = first_gen_raw_data, f = first_fit, method = "tenBerge")
# first_factscores$scores
# 
# first_factscores <- psych::factor.scores(x = first_gen_raw_data, f = first_fit, method = "tenBerge")
# first_factscores$scores
