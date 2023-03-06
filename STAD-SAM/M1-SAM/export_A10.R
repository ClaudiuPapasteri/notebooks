# Load packages
if (!require("pacman")) install.packages("pacman")
packages <- c(
  "papaja",
  "tidyverse",       
  "broom", "rstatix",
  "ggplot2", "ggpubr", "scales",        
  "rio",
  "ggstatsplot"
  # , ...
)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = packages)

# Themes for ggplot2 ploting (here used APA style)
theme_set(theme_apa())


# -------------------------------------------------------------------------

## Define function that recodes to numeric, but watches out to coercion to not introduce NAs
colstonumeric <- function(df){
  tryCatch({
    df_num <- as.data.frame(
      lapply(df,
             function(x) { as.numeric(as.character(x))})) 
  },warning = function(stop_on_warning) {
    message("Stoped the execution of numeric conversion: ", conditionMessage(stop_on_warning))
  }) 
}
##
## Define function that reverse codes items
ReverseCode <- function(df, tonumeric = FALSE, min = NULL, max = NULL) {
  if(tonumeric) df <- colstonumeric(df)
  df <- (max + min) - df
}
##
## Define function that scores only rows with less than 10% NAs (returns NA if all or above threshold percentage of rows are NA); can reverse code if vector of column indexes and min, max are provided.
ScoreLikert <- function(df, napercent = .1, tonumeric = FALSE, reversecols = NULL, min = NULL, max = NULL) {
  reverse_list <- list(reversecols = reversecols, min = min, max = max)
  reverse_check <- !sapply(reverse_list, is.null)
  
  # Recode to numeric, but watch out to coercion to not introduce NAs
  colstonumeric <- function(df){
    tryCatch({
      df_num <- as.data.frame(
        lapply(df,
               function(x) { as.numeric(as.character(x))})) 
    },warning = function(stop_on_warning) {
      message("Stoped the execution of numeric conversion: ", conditionMessage(stop_on_warning))
    }) 
  }
  
  if(tonumeric) df <- colstonumeric(df)
  
  if(all(reverse_check)){
    df[ ,reversecols] <- (max + min) - df[ ,reversecols]
  }else if(any(reverse_check)){
    stop("Insuficient info for reversing. Please provide: ", paste(names(reverse_list)[!reverse_check], collapse = ", "))
  }
  
  ifelse(rowSums(is.na(df)) > ncol(df) * napercent,
         NA,
         rowSums(df, na.rm = TRUE) * NA ^ (rowSums(!is.na(df)) == 0)
  )
}


# -------------------------------------------------------------------------

## Read files
folder <- here::here("pilotA10")
file <- "APS A10.xlsx"

# Read data
data <- rio::import(file.path(folder, file),
                    skip = 0, which = "APS")

data <- 
  data %>%
  dplyr::rename_with(.col = 13:29, ~ c(sprintf("APS_%d", 1:16), "id")) %>%
  dplyr::mutate(id = stringr::str_remove(id, "^0+"),                    # remove leading zeros
                id = stringr::str_remove_all(id, "[[:blank:]]"),          # remove any white space
                id =  toupper(id)) %>% 
  dplyr::select(13:29) %>%
  dplyr::filter(across(all_of(c("APS_1", "APS_2", "APS_3")), ~ !is.na(.x)))    # if missing all 3 items then drop row (nonsense info)

# Add PrePost Column
data <- 
  data %>%
  dplyr::group_by(id) %>%                           # can do arrange on Dates column if rows are not in order, but here they are
  dplyr::mutate(numbering = row_number()) %>%
  dplyr::mutate(PrePost = dplyr::case_when(numbering == 1 ~ "Pre",
                                           numbering == 2 ~ "Post",
                                           numbering == 3 ~ "F_up",
                                           TRUE ~ "Other")) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(PrePost = factor(PrePost, levels = c("Pre", "Post", "F_up")))

table(data$id, data$PrePost)   # check --  Sofi: F-up missing for id 6,7,13,18,19,21

# Add Cond
id_df <- rio::import(file.path(folder, file),
                     skip = 0, which = "Iduri")

id_df <- 
  id_df %>%
  dplyr::rename_with(.col = everything(), ~ c("Cond", "id", "email")) %>%
  dplyr::mutate(id = stringr::str_remove(id, "ID")) %>%
  dplyr::mutate(id = stringr::str_remove_all(id, "[[:blank:]]"),          # remove any white space
                id = stringr::str_remove(id, "^0+"),                     # remove leading zeros
                id =  toupper(id))  

data <- dplyr::left_join(data, id_df, by = "id")



# -------------------------------------------------------------------------

# Updated because special characters were not recognized -- replaced as wild cards in regex
data[, sprintf("APS_%d", 1:16)] <-
  data[, sprintf("APS_%d", 1:16)] %>%
  dplyr::mutate_all(~case_when(stringr::str_detect(., "niciodat*") ~ 1,
                               stringr::str_detect(., "ocazional*") ~ 2,
                               stringr::str_detect(., "c?teodat*") ~ 3,
                               stringr::str_detect(., "deseori*") ~ 4,
                               stringr::str_detect(., "ntotdeauna") ~ 5,
                               TRUE ~ NA_real_))

# Score
data$APS_Total <- ScoreLikert(data[, sprintf("APS_%d", 1:16)], napercent = .13)

# -------------------------------------------------------------------------

data_wide <-
  data %>%
  dplyr::select(-c(1:16), -numbering) %>% 
  tidyr::pivot_wider(id_cols = c(id, email), names_from = PrePost, values_from = APS_Total, names_prefix = "APS_")

# rio::export(data, file.path(folder, "ExportA10_longdata.xlsx"))
# rio::export(data_wide, file.path(folder, "ExportA10_widedata.xlsx"))
