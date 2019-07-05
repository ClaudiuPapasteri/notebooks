## O.1.A Oxy & Cort (01.07.2019)

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
  "rio"
  # , ...
)
if (!require("pacman")) install.packages("pacman")
pacman::p_load(char = packages)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Set wd
wd <- "C:/Users/Mihai/Desktop/o1a-report-oxy"
setwd(wd)  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read
Data <- rio::import("rezultate_oxitocina_pre_post_O1a_id28corectat.xlsx", sheet = "oxitocina+cortizol")
Data_PsySOP <- readRDS("Date_merged_Psy&SOP.RDS")


## Clean
Data <-
  Data %>%
    dplyr::filter(rowSums(is.na(.)) < 8) %>%               # filter out rows (no more than 7 NA on row)
    dplyr::select(which(colMeans(is.na(.)) < 0.5))         # filter out columns with Catalina's statistics (no more than 1/2 NA on column values)


## Transform in oder to be consistent with past behavioral data 
oldnames = colnames(Data)
newnames = c("ID_pre","Cortizol_pre", "Oxitocina_pre", "ID_post","Cortizol_post", "Oxitocina_post", "Conditie")

Data <-
  Data %>% 
  dplyr::rename_at(vars(oldnames), ~ newnames) %>%
  dplyr::select(-ID_post) %>%
  dplyr::mutate(ID_pre = stringr::str_remove_all(ID_pre, c(" proba A|/proba A"))) %>%     # small inconsistency "/proba"
  tidyr::separate(ID_pre,  c("ID", "Ziua", "Nr_zi"), "\\s+") %>%                          # split on white space
  dplyr::mutate(Ziua = rep("zi", length.out = n())) %>%
  tidyr::unite("Zi", c("Ziua", "Nr_zi"), sep = "", remove = TRUE) %>%
  mutate(ID = as.numeric(str_extract(ID, "[^/]+"))) %>%                                   # [^/]+ matches 1 or more chars other than /
  mutate(ID = as.character(ID))                                                             # ID Psy&SOP are char, not numeric...for merge

## Melt -- not needed here
Data_melt <-
  Data %>% 
  gather(variable, value, -ID, -Zi, -Conditie) %>%
  tidyr::separate(variable,  c("variable", "PrePost"), "_") %>%
  spread(variable, value) %>%
  mutate(PrePost = factor(PrePost, levels = c("pre","post"))) %>%
  dplyr::arrange(ID)


## Merge with Psy&SOP data
Date_merged <- left_join(Data_PsySOP, Data, by = c("ID", "Conditie"))             # all good: ID 1,4,36,42 dont have Oxy&Cort 



## Table Notebook
varnottable <- c("Nume_Prenume", "NA_per_row",
                 "IOS_mama", "IOS_tata", "IOS_iubit", "IOS_prieten", "IOS_personalitate",
                 sprintf("Stais_pre_%01d", seq(1,20)), 
                 sprintf("Stais_post_%01d", seq(1,20)))

Date_merged %>%                              
  select(-varnottable) %>%
  DT::datatable(                                  # excel downloadable  DT table
    extensions = 'Buttons',
    options = list(pageLength = 20,
                   scrollX='500px', 
                   dom = 'Bfrtip', 
                   buttons = c('excel', "csv")))




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Func t test si boxplot simplu
func_t_box <- function(df, ID, Conditie, pre_var, post_var){
  df_modif <-
    df %>%
    select(ID, Conditie, pre_var, post_var) %>% 
    tidyr::drop_na() %>%
    gather(pre_var, post_var, key = "PrePost", value = "value") %>% 
    mutate_at(vars(c(1, 2)), funs(as.factor)) %>% 
    mutate(PrePost = factor(PrePost, levels = c(pre_var, post_var))) 
  
  stat_comp <-
    df_modif %>% 
    group_by(Conditie) %>% 
    do(tidy(t.test(.$value ~ .$PrePost,
                   paired = TRUE,
                   data=.)))
  
  plot <- 
    ggpubr::ggpaired(df_modif, x = "PrePost", y = "value", id = ID, 
                     color = "PrePost", line.color = "gray", line.size = 0.4,
                     palette = c("#00AFBB", "#FC4E07"), legend = "none") +
    facet_wrap(~Conditie) +
    stat_summary(fun.data = mean_se,  colour = "darkred") +
    ggpubr::stat_compare_means(method = "t.test", paired = TRUE, label.x = as.numeric(df_modif$PrePost) * 0.90, label.y = max(df_modif$value) * 1.15) + 
    ggpubr::stat_compare_means(method = "t.test", paired = TRUE, label = "p.signif", comparisons = list(c(pre_var, post_var)))
  
  cat(paste0("#### ", pre_var, " ", post_var, "\n", "\n"))
  print(stat_comp)
  print(plot)
}

func_t_box(Data, "ID", "Conditie", "Cortizol_pre", "Cortizol_post")  
func_t_box(Data, "ID", "Conditie", "Oxitocina_pre", "Oxitocina_post")  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
