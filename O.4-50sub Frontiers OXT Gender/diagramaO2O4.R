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

Data_O2 <-
  Data_Gen_merged %>%
  select(4:5, 8:9) %>%
  dplyr::mutate(Study = rep("O2", nrow(.)))


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


Data_O4 <-
  Data_merged_wide_all %>%
  select(ID, Gen, Varsta, Nr_zi_OGL, Nr_zi_ECRAN) %>%
  dplyr::mutate(Study = rep("O4", nrow(.)))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# Data for diagrame
Data_O2_diagram <-
  Data_O2 %>%
  dplyr::mutate(Order_OGL = ifelse(Exercitiul == "Pozitiv - Negativ", 1, 2),
                Order_NEG = ifelse(Exercitiul == "Negativ - Pozitiv", 1, 2)) %>%
  select(-Exercitiul) %>%
  tidyr::unite(col = "ID", ID, Study, remove = TRUE) %>%
  dplyr::mutate(ID = as.character(ID),
                Gen = as.character(Gen)) %>%
  select(ID, Gen, Varsta, Order_OGL, Order_NEG) 

Data_O4_diagram <-
  Data_O4 %>%
  dplyr::mutate(Gen = as.character(toupper(Gen))) %>%
  dplyr::mutate(ID = as.character(ID)) %>%
  dplyr::rename(Order_OGL = Nr_zi_OGL) %>%
  dplyr::rename(Order_ECRAN = Nr_zi_ECRAN) %>%
  tidyr::unite(col = "ID", ID, Study, remove = TRUE) %>%
  select(ID, Gen, Varsta, Order_OGL, Order_ECRAN) 

df_diagram <- dplyr::bind_rows(Data_O2_diagram, Data_O4_diagram)
  


