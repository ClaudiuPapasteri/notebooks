## O.4 resting state - pt Miralena

library(tidyverse)

wd <- "C:/Users/Mihai/Desktop/O.4 prealabil pt Frontiers/O.4 Scale Scoring/scorate 21.08.2019"
setwd(wd)

## Read
Data_arsq <- readRDS("Data_arsq.RDS")
Data_nycq <- readRDS("Data_nycq.RDS")

## Clean
Data_arsq <-
  Data_arsq %>%
  select(-"Data.x", -"Data.y") %>%
  na_if("NA") %>%                           # make NA chars NA so to not get warning message of NAs introduced by coercion
  mutate_at(vars(- c(1:5)), funs(as.numeric(as.character(.)))) %>%
  select(c(1:15, 41:50))

Data_nycq <-
  Data_nycq %>%
  select(-"Data.x", -"Data.y") %>%
  na_if("NA") %>%                           # make NA chars NA so to not get warning message of NAs introduced by coercion
  mutate_at(vars(- c(1:5)), funs(as.numeric(as.character(.))))


## Automatic Post-Pre Difference Scores
# ARSQ
var_prepost <- colnames(Data_arsq[, grepl(".*pre|.*post", colnames(Data_arsq))])    # find variables that have "pre" and "post"
var_prepost <-  gsub("_pre", "", var_prepost)                                           # delete pre and post from colname
var_prepost <-  gsub("_post", "", var_prepost)
var_prepost <- unique(var_prepost)                                                      # keep unique -- all good         
new_diffvar_name <- vector(mode="character", length = length(var_prepost))              # initialize vector for for()
for(i in seq_along(var_prepost)){
  var_name_pre <- paste0(var_prepost[i], "_pre")
  var_name_post <- paste0(var_prepost[i], "_post")
  new_diffvar_name[i] <- paste0(var_prepost[i], "_Diff")
  Data_arsq[, new_diffvar_name[i]] <- Data_arsq[, var_name_post] - Data_arsq[, var_name_pre]
}

# NYCQ
var_prepost <- colnames(Data_nycq[, grepl(".*pre|.*post", colnames(Data_nycq))])    # find variables that have "pre" and "post"
var_prepost <-  gsub("_pre", "", var_prepost)                                           # delete pre and post from colname
var_prepost <-  gsub("_post", "", var_prepost)
var_prepost <- unique(var_prepost)                                                      # keep unique -- all good         
new_diffvar_name <- vector(mode="character", length = length(var_prepost))              # initialize vector for for()
for(i in seq_along(var_prepost)){
  var_name_pre <- paste0(var_prepost[i], "_pre")
  var_name_post <- paste0(var_prepost[i], "_post")
  new_diffvar_name[i] <- paste0(var_prepost[i], "_Diff")
  Data_nycq[, new_diffvar_name[i]] <- Data_nycq[, var_name_post] - Data_nycq[, var_name_pre]
}


## Sort OGL and ECRAN separatly
Data_arsq_OLG <-
  Data_arsq %>%
  filter(Conditie == "OGL")

Data_arsq_ECRAN <-
  Data_arsq %>%
  filter(Conditie == "ECRAN")

Data_nycq_OLG <-
  Data_nycq %>%
  filter(Conditie == "OGL")

Data_nycq_ECRAN <-
  Data_nycq %>%
  filter(Conditie == "ECRAN")

## Write
# xlsx::write.xlsx2(Data_arsq_OLG, "Data_arsq Diff for Miralena.xlsx", sheetName = "OGL", append = TRUE)
# xlsx::write.xlsx2(Data_arsq_ECRAN, "Data_arsq Diff for Miralena.xlsx", sheetName = "ECRAN", append = TRUE)
# xlsx::write.xlsx2(Data_nycq_OLG, "Data_nycq Diff for Miralena.xlsx", sheetName = "OGL", append = TRUE)
# xlsx::write.xlsx2(Data_nycq_ECRAN, "Data_nycq Diff for Miralena.xlsx", sheetName = "ECRAN", append = TRUE)


