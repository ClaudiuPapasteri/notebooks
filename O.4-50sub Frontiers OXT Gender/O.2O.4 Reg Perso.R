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




# fem
pred_names <- new_Neo
lm.test <- list()
lm.sig.result <- list()
j <- 0
for(i in seq_along(pred_names)){
  lm.test[[i]] <- summary(lm(
                    reformulate(paste("OXT_Pre +" , pred_names[i]), "OXT_Post"),
                    data = Data_fem_O4O2))
  coef <- lm.test[[i]]$coefficients
  if(coef[paste(pred_names[i]),"Pr(>|t|)"] < .05){
    j <- j + 1 
    lm.sig.result[[j]] <- lm.test[[i]]
  }
} 

fem.results <- lm.sig.result  # E3 = Asertivitate; C3 = Simt Datorie; C5 = Autodisciplina

# masc
pred_names <- new_Neo
lm.test <- list()
lm.sig.result <- list()
j <- 0
for(i in seq_along(pred_names)){
  lm.test[[i]] <- summary(lm(
    reformulate(paste("OXT_Pre +" , pred_names[i]), "OXT_Post"),
    data = Data_masc_O4O2))
  coef <- lm.test[[i]]$coefficients
  if(coef[paste(pred_names[i]),"Pr(>|t|)"] < .05){
    j <- j + 1 
    lm.sig.result[[j]] <- lm.test[[i]]
  }
} 

masc.results <- lm.sig.result  # N5 = Impulsivitate


# Toti
Data_toti_O4O2 <- rbind(Data_fem_O4O2, Data_masc_O4O2)

pred_names <- new_Neo
lm.test <- list()
lm.sig.result <- list()
j <- 0
for(i in seq_along(pred_names)){
  lm.test[[i]] <- summary(lm(
    reformulate(paste("OXT_Pre +" , pred_names[i]), "OXT_Post"),
    data = Data_toti_O4O2))
  coef <- lm.test[[i]]$coefficients
  if(coef[paste(pred_names[i]),"Pr(>|t|)"] < .05){
    j <- j + 1 
    lm.sig.result[[j]] <- lm.test[[i]]
  }
} 

toti.results <- lm.sig.result  # E3 = Asertivitate






#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



# CorOXTpre_masc <- 
#   Data_masc_O4O2 %>% 
#   select("OXT_Pre", new_Neo) %>%
#   correlate(use = "pairwise.complete.obs", method = "pearson") %>% 
#   focus(OXT_Pre)
# 
# CorOXTpre_masc %>% 
#   mutate(rowname = factor(rowname, levels = rowname[order(OXT_Pre)])) %>%  # Order by correlation strength
#   ggplot(aes(x = rowname, y = OXT_Pre)) +
#   geom_bar(stat = "identity") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   ylab("Corelatie cu baseline OXT") +
#   xlab("Variable")

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
  cat("#### Correlations with ", variable, "\n")
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



Data_masc_O4O2 %>%
  select("OXT_Pre", new_Neo) %>%
  Correlations_With_One(variable = "OXT_Pre", correlation_abs_threshold = 0.1, pvalue_threshold = 0.1)

Data_fem_O4O2 %>%
  select("OXT_Pre", new_Neo) %>%
  Correlations_With_One(variable = "OXT_Pre", correlation_abs_threshold = 0.1, pvalue_threshold = 0.1)   # altruism
