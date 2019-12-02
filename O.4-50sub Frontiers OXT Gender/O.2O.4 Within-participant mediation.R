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


Data_fem_O2 <- 
  Data_fem %>%
  select(ID, starts_with("Vas"), starts_with("Ox"), starts_with("IOS")) %>%
  select(ID, ends_with("prePoz"), ends_with("postPoz"), ends_with("pre_Poz"), ends_with("post_Poz")) %>%
  rename_all(list(~stringr::str_remove_all(., c("_Poz|Poz")))) %>%
  select(ID, starts_with("VasS"), starts_with("VasB"), starts_with("IOS"), starts_with("Ox")) %>%
  mutate(ID = paste0(ID, "_O.2"))

Data_masc_O2 <- 
  Data_masc %>%
  select(ID, starts_with("Vas"), starts_with("Ox"), starts_with("IOS")) %>%
  select(ID, ends_with("prePoz"), ends_with("postPoz"), ends_with("pre_Poz"), ends_with("post_Poz")) %>%
  rename_all(list(~stringr::str_remove_all(., c("_Poz|Poz")))) %>%
  select(ID, starts_with("VasS"), starts_with("VasB"), starts_with("IOS"), starts_with("Ox")) %>%
  mutate(ID = paste0(ID, "_O.2"))


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
  select(ID, starts_with("Vas"), starts_with("Ox"), starts_with("IOS")) %>%
  select(ID, ends_with("OGL")) %>%
  rename_all(list(~stringr::str_remove_all(., c("_OGL")))) %>%
  select(ID, starts_with("Vas_S"), starts_with("Vas_B"), starts_with("IOS"), starts_with("OXT")) %>%
  select(1, 3,2, 5,4, 7,6, 9,8) %>%
  mutate(ID = paste0(ID, "_O.4"))

colnames(Data_fem_O2) <- colnames(Data_fem_O4)
Data_fem_O4O2 <- rbind(Data_fem_O4, Data_fem_O2) 



Data_masc_O4 <-
  Data_masc %>%
  select(ID, starts_with("Vas"), starts_with("Ox"), starts_with("IOS")) %>%
  select(ID, ends_with("OGL")) %>%
  rename_all(list(~stringr::str_remove_all(., c("_OGL")))) %>%
  select(ID, starts_with("Vas_S"), starts_with("Vas_B"), starts_with("IOS"), starts_with("OXT")) %>%
  select(1, 3,2, 5,4, 7,6, 9,8) %>%
  mutate(ID = paste0(ID, "_O.4"))

colnames(Data_masc_O2) <- colnames(Data_masc_O4)
Data_masc_O4O2 <- rbind(Data_masc_O4, Data_masc_O2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(JSmediation)

# withinmed_OGL <- 
#   mdt_within_wide(data = Data_fem_O4O2, 
#                   DV_A = Vas_Stres_Pre, DV_B = Vas_Stres_Post, 
#                   M_A = OXT_Pre, M_B = OXT_Post)
# add_index(withinmed_OGL)
# 
# 
# withinmed_OGL <- 
#   mdt_within_wide(data = Data_fem_O4O2, 
#                   DV_A = Vas_Bine_Pre, DV_B = Vas_Bine_Post, 
#                   M_A = OXT_Pre, M_B = OXT_Post)
# add_index(withinmed_OGL)


# withinmed_OGL <- 
#   mdt_within_wide(data = Data_fem_O4O2, 
#                   M_A = Vas_Stres_Pre, M_B = Vas_Stres_Post, 
#                   DV_A = OXT_Pre, DV_B = OXT_Post)
# add_index(withinmed_OGL)


withinmed_OGL_fem <- 
  mdt_within_wide(data = Data_fem_O4O2, 
                  DV_A = Vas_Stres_Pre, DV_B = Vas_Stres_Post, 
                  M_A = IOS_Pre, M_B = IOS_Post)
add_index(withinmed_OGL_fem)

withinmed_OGL_masc <- 
  mdt_within_wide(data = Data_masc_O4O2, 
                  DV_A = Vas_Stres_Pre, DV_B = Vas_Stres_Post, 
                  M_A = IOS_Pre, M_B = IOS_Post)
add_index(withinmed_OGL_masc)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Data_dif_fem_O4O2 <-
  Data_fem_O4O2 %>%
  mutate(Diff_OXT = OXT_Post - OXT_Pre,
         Diff_Vas_Stres = Vas_Stres_Post - Vas_Stres_Pre,
         Diff_Vas_Bine = Vas_Bine_Post - Vas_Bine_Pre,
         Diff_IOS = IOS_Post - IOS_Pre
  ) %>%
  select(ID, starts_with("Dif"))

PerformanceAnalytics::chart.Correlation(Data_dif_fem_O4O2[, -1])  # sig fem (49)

Data_dif_masc_O4O2 <-
  Data_masc_O4O2 %>%
  mutate(Diff_OXT = OXT_Post - OXT_Pre,
         Diff_Vas_Stres = Vas_Stres_Post - Vas_Stres_Pre,
         Diff_Vas_Bine = Vas_Bine_Post - Vas_Bine_Pre,
         Diff_IOS = IOS_Post - IOS_Pre
  ) %>%
  select(ID, starts_with("Dif"))

PerformanceAnalytics::chart.Correlation(Data_dif_masc_O4O2[, -1])  # ns masc (31)

















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
    mutate_at(vars(c(1, 2)), list(~as.factor(.))) %>% 
    mutate(Cond = factor(Cond, levels = c(pre_var, post_var))) 
  
  stat_comp <- ggpubr::compare_means(value ~ Cond, data = df_modif, method = "t.test", paired = TRUE)
  
  stat_comp2 <-
    df_modif %>% 
    do(tidy(t.test(.$value ~ .$Cond,
                   paired = TRUE,
                   data=.)))
  
  plot <- 
    ggpubr::ggpaired(df_modif, x = "Cond", y = "value", id = ind, 
                     color = "Cond", line.color = "gray", line.size = 0.4,
                     palette = c("#00AFBB", "#FC4E07"), legend = "none") +
    stat_summary(fun.data = mean_se,  colour = "darkred") +
    ggpubr::stat_compare_means(method = "t.test", paired = TRUE, label.x = as.numeric(df_modif$Cond)-0.4, label.y = max(df_modif$value)+0.5) + 
    ggpubr::stat_compare_means(method = "t.test", paired = TRUE, label = "p.signif", comparisons = list(c(pre_var, post_var)))
  
  cat(paste0("#### ", pre_var, " ", post_var, "\n", "\n"))
  print(stat_comp)
  print(stat_comp2)
  print(plot)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
func_t_box(Data_fem_O4O2, "ID", "OXT_Pre", "OXT_Post")    # sig




