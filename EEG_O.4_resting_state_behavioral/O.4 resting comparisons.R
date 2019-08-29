#### O.4 resting comparisons - 30 subjects (OGL/CTRL Pre-Post)

library(tidyverse)

setwd("C:/Users/Mihai/Desktop/O.4 prealabil pt Frontiers/O.4 Scale Scoring/scorate 21.08.2019")

##########################################################################################
## ARSQ
Data_arsq_pre <- xlsx::read.xlsx("ARSQ.xlsx", sheetName = "Pre")
Data_arsq_post <- xlsx::read.xlsx("ARSQ.xlsx", sheetName = "Post")


# Select subjects (IDs are c(2:8, 10:14, 16:33))
Data_arsq_pre <-
  Data_arsq_pre %>%
  dplyr::mutate(ID = stringr::str_remove_all(ID, "[[:blank:]]")) %>%       # remove any white spaces
  dplyr::mutate(ID = stringr::str_remove_all(ID, "O.4")) %>%               # remove "O.4"
  dplyr::mutate(ID = stringr::str_remove_all(ID, "\\.")) %>%               # remove random dots
  dplyr::mutate(ID = as.numeric(ID)) %>%
  filter(ID %in% c(2:8, 10:14, 16:33)) %>%
  dplyr::rename("Conditie" = Conditia.) %>%
  tidyr::separate(Conditie,                                                # separate Conditie into Order and Conditie
                  into = c("Order", "Conditie")) %>%
  dplyr::mutate(Conditie = as.factor(Conditie)) %>%
  dplyr::rename_at(vars(-c(1:6)), list(~paste0(., "_pre")))                # paste "pre" to column names excluding first 6

Data_arsq_post <-
  Data_arsq_post %>%
  dplyr::mutate(ID = stringr::str_remove_all(ID, "[[:blank:]]")) %>%       # remove any white spaces
  dplyr::mutate(ID = stringr::str_remove_all(ID, "O.4")) %>%               # remove "O.4"
  dplyr::mutate(ID = stringr::str_remove_all(ID, "\\.")) %>%               # remove random dots
  dplyr::mutate(ID = as.numeric(ID)) %>%
  filter(ID %in% c(2:8, 10:14, 16:33)) %>%
  dplyr::rename("Conditie" = Conditia.) %>%
  tidyr::separate(Conditie,                                                # separate Conditie into Order and Conditie
                  into = c("Order", "Conditie")) %>%
  dplyr::mutate(Conditie = as.factor(Conditie)) %>%
  dplyr::rename_at(vars(-c(1:6)), list(~paste0(., "_post")))               # paste "post" to column names excluding first 6
  

Data_arsq <- 
  dplyr::full_join(Data_arsq_pre, Data_arsq_post, by = c("ID", "Nume.Prenume", "Varsta", "Order", "Conditie"))

# saveRDS(Data_arsq, "Data_arsq.RDS")
# xlsx::write.xlsx2(Data_arsq, "Data_arsq.xlsx")


##########################################################################################
## NYCQ
Data_nycq_pre <- xlsx::read.xlsx("NYCQ.xlsx", sheetName = "Pre")
Data_nycq_post <- xlsx::read.xlsx("NYCQ.xlsx", sheetName = "Post")

# Select subjects (IDs are c(2:8, 10:14, 16:33))
Data_nycq_pre <-
  Data_nycq_pre %>%
  dplyr::mutate(ID = stringr::str_remove_all(ID, "[[:blank:]]")) %>%       # remove any white spaces
  dplyr::mutate(ID = stringr::str_remove_all(ID, "O.4")) %>%               # remove "O.4"
  dplyr::mutate(ID = stringr::str_remove_all(ID, "\\.")) %>%               # remove random dots
  dplyr::mutate(ID = as.numeric(ID)) %>%
  filter(ID %in% c(2:8, 10:14, 16:33)) %>%
  dplyr::rename("Conditie" = Conditia.) %>%
  tidyr::separate(Conditie,                                                # separate Conditie into Order and Conditie
                  into = c("Order", "Conditie")) %>%
  dplyr::mutate(Conditie = as.factor(Conditie)) %>%
  dplyr::rename_at(vars(-c(1:6)), list(~paste0(., "_pre")))                # paste "pre" to column names excluding first 6

Data_nycq_post <-
  Data_nycq_post %>%
  dplyr::mutate(ID = stringr::str_remove_all(ID, "[[:blank:]]")) %>%       # remove any white spaces
  dplyr::mutate(ID = stringr::str_remove_all(ID, "O.4")) %>%               # remove "O.4"
  dplyr::mutate(ID = stringr::str_remove_all(ID, "\\.")) %>%               # remove random dots
  dplyr::mutate(ID = as.numeric(ID)) %>%
  filter(ID %in% c(2:8, 10:14, 16:33)) %>%
  dplyr::rename("Conditie" = Conditia.) %>%
  tidyr::separate(Conditie,                                                # separate Conditie into Order and Conditie
                  into = c("Order", "Conditie")) %>%
  dplyr::mutate(Conditie = as.factor(Conditie)) %>%
  dplyr::rename_at(vars(-c(1:6)), list(~paste0(., "_post")))               # paste "post" to column names excluding first 6


Data_nycq <- 
  dplyr::full_join(Data_nycq_pre, Data_nycq_post, by = c("ID", "Nume.Prenume", "Varsta", "Order", "Conditie"))

# saveRDS(Data_nycq, "Data_nycq.RDS")
# xlsx::write.xlsx2(Data_nycq, "Data_nycq.xlsx")