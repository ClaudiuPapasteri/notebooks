## O.4 resting state - pt Miralena

library(tidyverse)

wd <- "C:/Users/Mihai/Desktop/O.4 prealabil pt Frontiers/O.4 Scale Scoring/scorate 21.08.2019"
setwd(wd)

## Read
Data_vas <- xlsx::read.xlsx2("VAS, IOS.xlsx", sheetName = "Toti")

## To numeric
Data_vas <-
  Data_vas %>%
  mutate_at(vars(6:11), function(x) as.numeric(as.character(x)))

## Calc Diff Scores
Data_vas$VaS_Diff <- Data_vas$VaS_post - Data_vas$VaS_pre 
Data_vas$VaB_Diff <- Data_vas$VaB_post - Data_vas$VaB_pre
Data_vas$IOS_Diff <- Data_vas$IOS_post - Data_vas$IOS_pre
		
## Transform
Data_vas_OGL <-
  Data_vas %>%
  filter(ID %in% c(2:8, 10:14, 16:33)) %>%
  filter(Conditie == "OGL")

Data_vas_ECRAN <-
  Data_vas %>%
  filter(ID %in% c(2:8, 10:14, 16:33)) %>%
  filter(Conditie == "ECRAN")

## Write
# xlsx::write.xlsx2(Data_vas_OGL, "Data_VAS&IOS for Miralena.xlsx", sheetName = "OGL", append = TRUE)
# xlsx::write.xlsx2(Data_vas_ECRAN, "Data_VAS&IOS for Miralena.xlsx", sheetName = "ECRAN", append = TRUE)         