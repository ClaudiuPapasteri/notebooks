# Load packages
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
  "rio",
  "GGally"
  # , ...
)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = packages)

# Themes for ggplot2 ploting (here used APA style)
theme_set(theme_apa())



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
  cat("**Merge was succesful**")
  rm("Data_merge1", "Data_merge2", "Data_merge3", "DataBIO", "DataDG", "DataTrust", "DataVAS", "test_names", "merge_names")
}else cat("**Merge unsuccesful**") 


# Gender Dataframe
Data_Gen <- rio::import(file.path(wd, "Gen varsta O03 O02 O2.xlsx"), which = "O.2")

Data_Gen_merged <- 
  Data %>%
  tidyr::separate(Indicativ,  c("ID_tag", "ID", "study_tag"), "\\s+") %>%                               # split on white space
  select(-c("ID_tag", "study_tag")) %>%
  mutate(ID = as.numeric(as.character(ID))) %>%
  dplyr::left_join(., Data_Gen, by = c("ID")) %>%
  select(1:7, Gen, Varsta, everything())

Data_fem <- 
  Data_Gen_merged %>%
  filter(Gen == "F")

Data_masc <- 
  Data_Gen_merged %>%
  filter(Gen == "M")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

names_Neo <- paste0("Neo_", c("Nev", "Ext", "Opn", "Agr", "Con", 
                              sprintf("N%d", 1:6),
                              sprintf("E%d", 1:6),
                              sprintf("O%d", 1:6),
                              sprintf("A%d", 1:6),
                              sprintf("C%d", 1:6)))
old_names <- c("VasS_pre", "VasS_post", "VasB_pre", "VasB_post", "IOS_pre", "IOS_post", "Ox_pre", "Ox_post")
new_names <- c("Vas_Stres_Pre", "Vas_Stres_Post", "Vas_Bine_Pre", "Vas_Bine_Post", "IOS_Pre", "IOS_Post", "OXT_Pre", "OXT_Post")  
new_Neo <- c("N", "E", "O", "A", "C", 
             sprintf("N%d", 1:6),
             sprintf("E%d", 1:6),
             sprintf("O%d", 1:6),
             sprintf("A%d", 1:6),
             sprintf("C%d", 1:6))

Data_fem_O2 <- 
  Data_fem %>%
  select(ID, starts_with("Vas"), starts_with("Ox"), starts_with("IOS"), names_Neo) %>%
  select(-contains("Neg")) %>%
  rename_all(list(~stringr::str_remove_all(., c("_Poz|Poz")))) %>%
  select(ID, starts_with("VasS"), starts_with("VasB"), starts_with("IOS"), starts_with("Ox"), names_Neo) %>%
  mutate(ID = paste0(ID, "_O.2")) %>%
  dplyr::rename_at(vars(old_names), ~new_names) %>%
  dplyr::rename_at(vars(names_Neo), ~new_Neo)

Data_masc_O2 <- 
  Data_masc %>%
  select(ID, starts_with("Vas"), starts_with("Ox"), starts_with("IOS"), names_Neo) %>%
  select(-contains("Neg")) %>%
  rename_all(list(~stringr::str_remove_all(., c("_Poz|Poz")))) %>%
  select(ID, starts_with("VasS"), starts_with("VasB"), starts_with("IOS"), starts_with("Ox"), names_Neo) %>%
  mutate(ID = paste0(ID, "_O.2")) %>%
  dplyr::rename_at(vars(old_names), ~new_names) %>%
  dplyr::rename_at(vars(names_Neo), ~new_Neo)


############################################################################################
#### AICI
############################################################################################

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read and Merge O.4
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wd <- "C:/Users/Mihai/Desktop/O.4 prealabil pt Frontiers/O.4 Date Psiho&OXT"
setwd(wd)

Data_OXT <- rio::import(file.path(wd, "\\OXT", "O.4 OXT Data.xlsx"))

Data_psih <- rio::import(file.path(wd, "\\Psiho", "DATE O4Cl 16.11.2019 procesat.xlsx"), which = "26martie2019")
Data_psih <- Data_psih[, c(1:6, 8:13)]

Data_Neo <- rio::import(file.path(wd, "\\Psiho", "DATE O4Cl 16.11.2019 procesat.xlsx"), which = "Neo scored")
Data_BDI <- rio::import(file.path(wd, "\\Psiho", "DATE O4Cl 16.11.2019 procesat.xlsx"), which = "BDI scored")
Data_STAI <- rio::import(file.path(wd, "\\Psiho", "DATE O4Cl 16.11.2019 procesat.xlsx"), which = "STAI scored")

Data_STAI <- 
  Data_STAI %>% 
  dplyr::filter(!is.na(ID)) %>%                                                                # no ID, no data
  dplyr::filter(rowSums(is.na(.)) < 2) %>%                                                     # filter out rows (no more than 1 NA on row)
  mutate(ID = stringr::str_replace(ID, "\\s", "|")) %>%                                        # replace first whitespace with | and use it to separate
  tidyr::separate(ID, into = c("ID", "Experim"), sep = "\\|") %>%                              # separate on only first whitespace that was replaced
  dplyr::select(-Experim)

Data_BDI <- 
  Data_BDI %>% 
  dplyr::filter(!is.na(ID)) %>%                                                                # no ID, no data
  dplyr::filter(rowSums(is.na(.)) < 1) %>%                                                     # filter out rows (not 1 NA)
  mutate(ID = stringr::str_replace(ID, "\\s", "|")) %>%                                        # replace first whitespace with | and use it to separate
  tidyr::separate(ID, into = c("ID", "Experim"), sep = "\\|") %>%                              # separate on only first whitespace that was replaced
  dplyr::select(-Experim)

Data_Neo <- 
  Data_Neo %>% 
  dplyr::filter(!is.na(ID)) %>%                                                                # no ID, no data
  dplyr::filter(rowSums(is.na(.)) < 6) %>%                                                     # filter out rows (no more than 5 NA on row)
  mutate(ID = stringr::str_replace(ID, "\\s", "|")) %>%                                        # replace first whitespace with | and use it to separate
  tidyr::separate(ID, into = c("ID", "Experim"), sep = "\\|") %>%                              # separate on only first whitespace that was replaced
  dplyr::select(-Experim)

Data_psih <- 
  Data_psih %>% 
  dplyr::filter(!is.na(ID)) %>%                                                                # no ID, no data
  dplyr::filter(rowSums(is.na(.)) < 8) %>%                                                     # filter out rows (no more than 7 NA on row)
  tidyr::separate(Conditia,  c("Nr_zi", "Conditia"), "\\s+") %>%                               # split on white space
  mutate(ID = stringr::str_replace(ID, "\\s", "|")) %>%                                        # replace first whitespace with | and use it to separate
  tidyr::separate(ID, into = c("ID", "Experim"), sep = "\\|") %>%                              # separate on only first whitespace that was replaced
  dplyr::rename("Nume" = Nume_Prenume) %>% 
  dplyr::select(-Experim)

Data_OXT <-   
  Data_OXT %>% 
  tidyr::separate(ID,  c("ID", "Ziua", "Nr_zi", "Proba"), "\\s+") %>%                          # split on white space
  select(ID, Nr_zi, Proba, OXT) %>%
  tidyr::separate(ID, into = c("ID", "Experim"), sep = "\\/") %>%                              # separate on /
  dplyr::select(-Experim) %>% 
  dplyr::rename("PrePost" = Proba) %>% 
  mutate(PrePost = forcats::fct_recode(PrePost, "Pre" = "A", "Post" = "B")) %>%                # Proba A = Pre, B = Post on same day
  spread(key = PrePost, value = OXT) %>%
  dplyr::rename_at(vars(Pre, Post), ~ c("OXT_Pre", "OXT_Post"))


Data_merged <- dplyr::left_join(Data_psih, Data_OXT, by = c("ID", "Nr_zi"))                 # ID 40, 59 are incomplete in OXT -- dplyr::full_join


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Make Wide Dataframe of merged
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check if ids have > 1 row of data (empty ID have only 1 row; thei have only 1 Condition) 
# Careful! This function modfies the datasets in the global envinronment
delete_empty_id <- function(df){
  list_empty_id <- 
    df %>%
    dplyr::group_by(ID) %>%
    dplyr::summarise(row_count = n()) %>%
    dplyr::rename("empty_id" = ID) %>%
    mutate(delete_id = if_else(row_count < 2, TRUE, FALSE)) %>%
    filter(delete_id == TRUE)
  
  df_modif <- 
    df %>%
    filter(!ID %in% list_empty_id$empty_id)
  
  if(!identical(df, df_modif)){
    df <- deparse(substitute(df))
    cat("Deleting from ", print(as.name(df))); print(list_empty_id)                    # print out which ids are deleted from which dataset
    assign(df, df_modif, envir = globalenv())                                          # assign modified df to original dataset from Global
  }else cat("No empty datasets. Nothing to delete")
}
delete_empty_id(Data_merged) 

# Dataframe for regular analyses
Data_merged_wide <-
  Data_merged %>%
  select(-c(Varsta, Gen)) %>%            # exclude Gen, Varsta: merge after because it interferes with spread()
  gather("variable", "value", c(Data, Nr_zi, Vas_Stres_Pre:OXT_Post), -c(1:2)) %>%       # Conditia needs to be outside
  unite("united_var", c(variable, Conditia), sep = "_") %>%
  spread(united_var, value) %>%
  mutate_at(vars(5:22), list(~as.numeric(as.character(.))))

# sum(is.na(Data_merged[, 6:15])) == sum(is.na(Data_merged_wide[, 5:22]))    # check if nr of NA is the same after melt


# Full Wide Dataframe  (HERE We APPLY EXCLUSION CRITERIA) 
Data_merged_wide_all <- 
  Data_merged[, c("ID", "Nume", "Varsta", "Gen")] %>%                        # need to add these back, but just the main unduplicated row
  dplyr::distinct(ID, .keep_all = TRUE) %>%
  dplyr::left_join(., Data_merged_wide, by = "ID") %>%
  dplyr::rename("Nume" = Nume.x) %>%
  dplyr::select(-Nume.y) %>% 
  dplyr::left_join(., Data_BDI, by = "ID") %>%
  dplyr::left_join(., Data_STAI, by = "ID") %>%
  dplyr::left_join(., Data_Neo, by = "ID") %>%
  mutate_at(vars(23:60), list(~as.numeric(as.character(.)))) %>%
  mutate(Varsta = as.numeric(as.character(Varsta))) %>% 
  filter(!ID %in% c(19, 43)) %>%           # 55 subs remain after this
  filter(Varsta < 40) %>%                  # 53 subs remain after this
  filter(ScorBDI < 30) %>%                 # 50 subs remain after this
  dplyr::arrange(ID)


# Full Long Dataframe
vars_OglEcran <- grep("OGL|ECRAN", colnames(Data_merged_wide_all))

Data_merged_long_all <-
  Data_merged_wide_all %>%
  gather(variable, value, vars_OglEcran, -c(ID)) %>%                                      # gather Data and Nr_zi as these are OglEcran level
  tidyr::separate(variable,  c("variable", "OglEcran"), "_(?=[^_]+$)") %>%                # split only on last "_"
  spread(variable, value) %>% 
  rename_at(vars(vars_OglEcran), list(~stringr::str_remove_all(., c("_OGL|_ECRAN")))) %>%          
  arrange(ID)

vars_PrePost <- grep("Pre|Post", colnames(Data_merged_long_all))

Data_merged_long_all <-
  Data_merged_long_all %>%
  pivot_longer(vars_PrePost, names_to = c(".value","PrePost"), names_pattern = "(.*)_(.*)") %>%     # magic     
  mutate_at(vars(48:50), list(~as.numeric(as.character(.)))) %>%
  arrange(ID)


# Females and Males Dataframes
Data_fem <- 
  Data_merged_wide_all %>%
  filter(Gen == "f")
Data_masc <- 
  Data_merged_wide_all %>%
  filter(Gen == "m")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



Data_fem_O4 <-
  Data_fem %>%
  select(ID, starts_with("Vas"), starts_with("OXT"), starts_with("IOS"), new_Neo) %>%
  select(-contains("ECRAN")) %>%
  rename_all(list(~stringr::str_remove_all(., c("_OGL")))) %>%
  select(ID, starts_with("Vas_S"), starts_with("Vas_B"), starts_with("IOS"), starts_with("OXT"), new_Neo) %>%
  select(1, 3,2, 5,4, 7,6, 9,8, everything()) %>%
  mutate(ID = paste0(ID, "_O.4"))

Data_fem_O4O2 <- rbind(Data_fem_O4, Data_fem_O2) 



Data_masc_O4 <-
  Data_masc %>%
  select(ID, starts_with("Vas"), starts_with("OXT"), starts_with("IOS"), new_Neo) %>%
  select(-contains("ECRAN")) %>%
  rename_all(list(~stringr::str_remove_all(., c("_OGL")))) %>%
  select(ID, starts_with("Vas_S"), starts_with("Vas_B"), starts_with("IOS"), starts_with("OXT"), new_Neo) %>%
  select(1, 3,2, 5,4, 7,6, 9,8, everything()) %>%
  mutate(ID = paste0(ID, "_O.4"))

Data_masc_O4O2 <- rbind(Data_masc_O4, Data_masc_O2)


# Toti
Data_toti_O4O2 <- rbind(Data_fem_O4O2, Data_masc_O4O2)





##### 
NormeCombinate <- data.frame(     
  Scale = c("Nev", "Ext", "Opn", "Agr", "Con", sprintf("N%01d", seq(1,6)), sprintf("E%01d", seq(1,6)), sprintf("O%01d", seq(1,6)), sprintf("A%01d", seq(1,6)), sprintf("C%01d", seq(1,6))),
  Medie = c(84.82, 108.86, 107.65, 112.44, 120.25,  15.49, 14.14, 13.83, 15.65, 14.68, 11.04,  21.49, 17.73, 16.27, 18.21, 16.39, 18.77,  16.63, 18.36, 19.48, 15.47, 18.69, 19.03,  18.16, 19.35, 21.22, 16.83, 16.96, 19.92,  20.17, 19.18, 22.28, 20.09, 20.50, 18.02),
  SD = c(20.57, 16.59, 15.18, 17.24, 20.40,  4.89, 4.62, 5.20, 4.37, 4.40, 4.73,  4.14, 4.80, 4.43, 4.09, 4.60, 4.29,  4.31, 5.01, 4.01, 3.53, 4.67, 3.71,  4.66, 5.20, 4.52, 4.24, 4.48, 4.02,  4.16, 4.52, 4.59, 4.31, 4.85, 4.82)      )


BazaScoruriTComb <- data.frame(
  ID = Data_toti_O4O2$ID , 
  N = ( Data_toti_O4O2$N - NormeCombinate[1, 2] ) / NormeCombinate[1, 3] * 10  + 50 ,
  E = ( Data_toti_O4O2$E - NormeCombinate[2, 2] ) / NormeCombinate[2, 3] * 10  + 50 ,
  O = ( Data_toti_O4O2$O - NormeCombinate[3, 2] ) / NormeCombinate[3, 3] * 10  + 50 ,
  A = ( Data_toti_O4O2$A - NormeCombinate[4, 2] ) / NormeCombinate[4, 3] * 10  + 50 ,
  C = ( Data_toti_O4O2$C - NormeCombinate[5, 2] ) / NormeCombinate[5, 3] * 10  + 50 ,
  
  N1 = ( Data_toti_O4O2$N1 - NormeCombinate[6, 2] ) / NormeCombinate[6, 3] * 10  + 50 ,
  N2 = ( Data_toti_O4O2$N2 - NormeCombinate[7, 2] ) / NormeCombinate[7, 3] * 10  + 50 ,
  N3 = ( Data_toti_O4O2$N3 - NormeCombinate[8, 2] ) / NormeCombinate[8, 3] * 10  + 50 ,
  N4 = ( Data_toti_O4O2$N4 - NormeCombinate[9, 2] ) / NormeCombinate[9, 3] * 10  + 50 ,
  N5 = ( Data_toti_O4O2$N5 - NormeCombinate[10, 2] ) / NormeCombinate[10, 3] * 10  + 50 ,
  N6 = ( Data_toti_O4O2$N6 - NormeCombinate[11, 2] ) / NormeCombinate[11, 3] * 10  + 50 ,
  E1 = ( Data_toti_O4O2$E1 - NormeCombinate[12, 2] ) / NormeCombinate[12, 3] * 10  + 50 ,
  E2 = ( Data_toti_O4O2$E2 - NormeCombinate[13, 2] ) / NormeCombinate[13, 3] * 10  + 50 ,
  E3 = ( Data_toti_O4O2$E3 - NormeCombinate[14, 2] ) / NormeCombinate[14, 3] * 10  + 50 ,
  E4 = ( Data_toti_O4O2$E4 - NormeCombinate[15, 2] ) / NormeCombinate[15, 3] * 10  + 50 ,
  E5 = ( Data_toti_O4O2$E5 - NormeCombinate[16, 2] ) / NormeCombinate[16, 3] * 10  + 50 ,
  E6 = ( Data_toti_O4O2$E6 - NormeCombinate[17, 2] ) / NormeCombinate[17, 3] * 10  + 50 ,
  O1 = ( Data_toti_O4O2$O1 - NormeCombinate[18, 2] ) / NormeCombinate[18, 3] * 10  + 50 ,
  O2 = ( Data_toti_O4O2$O2 - NormeCombinate[19, 2] ) / NormeCombinate[19, 3] * 10  + 50 ,
  O3 = ( Data_toti_O4O2$O3 - NormeCombinate[20, 2] ) / NormeCombinate[20, 3] * 10  + 50 ,
  O4 = ( Data_toti_O4O2$O4 - NormeCombinate[21, 2] ) / NormeCombinate[21, 3] * 10  + 50 ,
  O5 = ( Data_toti_O4O2$O5 - NormeCombinate[22, 2] ) / NormeCombinate[22, 3] * 10  + 50 ,
  O6 = ( Data_toti_O4O2$O6 - NormeCombinate[23, 2] ) / NormeCombinate[23, 3] * 10  + 50 ,
  A1 = ( Data_toti_O4O2$A1 - NormeCombinate[24, 2] ) / NormeCombinate[24, 3] * 10  + 50 ,
  A2 = ( Data_toti_O4O2$A2 - NormeCombinate[25, 2] ) / NormeCombinate[25, 3] * 10  + 50 ,
  A3 = ( Data_toti_O4O2$A3 - NormeCombinate[26, 2] ) / NormeCombinate[26, 3] * 10  + 50 ,
  A4 = ( Data_toti_O4O2$A4 - NormeCombinate[27, 2] ) / NormeCombinate[27, 3] * 10  + 50 ,
  A5 = ( Data_toti_O4O2$A5 - NormeCombinate[28, 2] ) / NormeCombinate[28, 3] * 10  + 50 ,
  A6 = ( Data_toti_O4O2$A6 - NormeCombinate[29, 2] ) / NormeCombinate[29, 3] * 10  + 50 ,
  C1 = ( Data_toti_O4O2$C1 - NormeCombinate[30, 2] ) / NormeCombinate[30, 3] * 10  + 50 ,
  C2 = ( Data_toti_O4O2$C2 - NormeCombinate[31, 2] ) / NormeCombinate[31, 3] * 10  + 50 ,
  C3 = ( Data_toti_O4O2$C3 - NormeCombinate[32, 2] ) / NormeCombinate[32, 3] * 10  + 50 ,
  C4 = ( Data_toti_O4O2$C4 - NormeCombinate[33, 2] ) / NormeCombinate[33, 3] * 10  + 50 ,
  C5 = ( Data_toti_O4O2$C5 - NormeCombinate[34, 2] ) / NormeCombinate[34, 3] * 10  + 50 ,
  C6 = ( Data_toti_O4O2$C6 - NormeCombinate[35, 2] ) / NormeCombinate[35, 3] * 10  + 50 
)

BazaScoruriTCombmelt <- melt(BazaScoruriTComb, id='ID')



GridData <- readRDS(file="C:/Users/Mihai/Desktop/R Notebooks/notebooks/O.4-50sub Frontiers OXT Gender/GridData.rds")

GridGraph <- 
  ggplot(aes(y=Val, x=Scale), data=GridData) + geom_point(shape=95) +
  geom_text(aes(label=round(Lab)),hjust=-0.4, vjust=0.4, size=2.3) +
  scale_y_continuous(breaks=seq(20,90,10)) + # coord_cartesian(ylim=c(10,90)) + 
  scale_x_discrete(breaks = levels(GridData$Scale), 
                   limits = c(levels(GridData$Scale)[1:5], "skip", levels(GridData$Scale)[6:11], "skip", 
                              levels(GridData$Scale)[12:17], "skip", levels(GridData$Scale)[18:23], "skip", 
                              levels(GridData$Scale)[24:29], "skip", levels(GridData$Scale)[30:35], "skip" )) +
  annotate("rect", xmin=.01, xmax=42, ymin=35, ymax=65, alpha=0.1, fill="blue") +
  annotate("rect", xmin=.01, xmax=42, ymin=45, ymax=55, alpha=0.15, fill="blue") +
  geom_hline(aes(yintercept=35), colour="blue",  alpha=0.3) +
  geom_hline(aes(yintercept=65), colour="blue",  alpha=0.3) +
  geom_hline(aes(yintercept=45), colour="blue",  alpha=0.3) +
  geom_hline(aes(yintercept=55), colour="blue",  alpha=0.3) +
  theme_classic() + 
  theme(axis.title.x=element_blank(), text=element_text(size=16,  family="serif")) + 
  ylab("T Scores")


GridGraph + 
  geom_violin(data = BazaScoruriTCombmelt, aes(y = value,  x = variable), fill="red", color="red", alpha = 0.1) + 
  stat_summary(data = BazaScoruriTCombmelt, aes(y = value,  x = variable), fun.data=data_summary, color="red") +
  theme(legend.position="none")


## EXPORT HIGH DPI TIFF
# setwd("C:/Users/Mihai/Desktop/R Notebooks/notebooks/O.4-50sub Frontiers OXT Gender")
# tiff("test.tiff", units = "px", width=3200, height=2000, res=300)
# GridGraph + 
#   geom_violin(data = BazaScoruriTCombmelt, aes(y = value,  x = variable), fill="red", color="red", alpha = 0.1) + 
#   stat_summary(data = BazaScoruriTCombmelt, aes(y = value,  x = variable), fun.data=data_summary, color="red") +
#   theme(legend.position="none")
# dev.off()