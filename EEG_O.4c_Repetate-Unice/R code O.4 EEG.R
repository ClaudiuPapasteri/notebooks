### O.4 R code - Unice and Retatate
### R code for sorting, integrating and analyses 
# Task output files: 
# ID, experimental condition ("oglinda" / "ecran"), condition ("instructor", "solo"), type of task ("unic", "repetat")
###############################################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load("tidyverse", "rio", "psych", "ggplot2", "plyr", "stringr")        

########################################################################################################################
#################### Create folders by conditions, copy files to them, read and merge ##################################
wd <- "E:/CINETIC diverse/O.4c (EEG)/Date 27.04.2019/Date redenumite"
setwd(wd)

##################### Read file names ##################################################################################
file_names <- dir(pattern = "\\.xls$")
## if above isn't good enough try the following:
# file_names <- list.files(wd)
# file_names <- sop_files[!file.info(sop_files)$isdir]   # exclude directories
# file_names <- sop_files[grep(".xls", sop_files, fixed = TRUE)]


##################### Create folders with Condition names ###############################################################
# this part of script may be re-run if files from wd are updated
dir_names <- c("Unic_CTRL_Instr", "Unic_CTRL_Solo", "Unic_OGL_Instr", "Unic_OGL_Solo",
              "Repetat_CTRL_Instr", "Repetat_CTRL_Solo", "Repetat_OGL_Instr", "Repetat_OGL_Solo")
             
for(dir in dir_names){
  if(!dir.exists(file.path(wd, dir)))
  dir.create(file.path(wd, dir), showWarnings = FALSE)
}


##################### Use file names to sort them to folders ############################################################
sort_files_to_dirs <- function(wd, pattern, dir) {
  check_pattern <- outer(file_names, pattern, stringr::str_detect)               # if all TRUE bye row then it has full pattern
  index <- which(apply(check_pattern, 1, function(x) all(x==TRUE)))              # get index of file_names where all are TRUE
  sorted_files <- file_names[index]                                              # get names of files from indexes
  
  for(files in sorted_files) {                                                   # copy the files to corresponding folder
    file.copy(from = file.path(wd, files), to = file.path(wd, dir))
  }  
}

sort_files_to_dirs(wd = wd, pattern = c("unic", "ecran", "instructor"), dir = "Unic_CTRL_Instr")
sort_files_to_dirs(wd = wd, pattern = c("unic", "ecran", "solo"), dir = "Unic_CTRL_Solo")
sort_files_to_dirs(wd = wd, pattern = c("unic", "oglinda", "instructor"), dir = "Unic_OGL_Instr")
sort_files_to_dirs(wd = wd, pattern = c("unic", "oglinda", "solo"), dir = "Unic_OGL_Solo")

sort_files_to_dirs(wd = wd, pattern = c("repetat", "ecran", "instructor"), dir = "Repetat_CTRL_Instr")
sort_files_to_dirs(wd = wd, pattern = c("repetat", "ecran", "solo"), dir = "Repetat_CTRL_Solo")
sort_files_to_dirs(wd = wd, pattern = c("repetat", "oglinda", "instructor"), dir = "Repetat_OGL_Instr")
sort_files_to_dirs(wd = wd, pattern = c("repetat", "oglinda", "solo"), dir = "Repetat_OGL_Solo")


############ Read in all the .xls from folders and merge them in datasets named after corresponding folder ##############
# this part of script may be re-run if files from wd are updated
# RE-RUN FROM HERE IF FOLDERS AND SORTING WAS ALREADY DONE
wd <- "E:/CINETIC diverse/O.4c (EEG)/Date 27.04.2019/Date redenumite"
setwd(wd)
folders <- list.files(wd)
folders <- folders[file.info(folders)$isdir]   # luam doar folderele
datasetnames <- NULL

for (i in 1:length(folders)) {
  datasetname <- folders[i]
  datasetnames <- c(datasetnames, datasetname)
  current_dir <- setwd(file.path(wd, folders[i]))
  print(paste0("current_dir: ", current_dir))
  
  paths <- dir(pattern = "\\.xls$")
  names(paths) <- basename(paths)

  assign( paste(datasetname), plyr::ldply(paths, rio::import) )
}

setwd(wd)
# detach("package:plyr", unload=TRUE)                                                   # detach plyr because of conflicts with dplyr

#################################### Data Cleaning #####################################################################
# Check if ids have > 1 row of data (empty .xls have only 1 row)
# Careful! This function modfies the datasets in the global envinronment
delete_empty_id <- function(df){
  list_empty_id <- 
    df %>%
    group_by(.id) %>%
    dplyr::summarise(row_count = n()) %>%
    dplyr::rename("empty_id" = .id) %>%
    mutate(delete_id = if_else(row_count < 3, TRUE, FALSE)) %>%
    filter(delete_id == TRUE)
  
  df_modif <- 
    df %>%
    filter(!.id %in% list_empty_id$empty_id)
  
  if(!identical(df, df_modif)){
    df <- deparse(substitute(df))
    cat("Deleting from ", print(as.name(df))); print(list_empty_id)                    # print out which ids are deleted from which dataset
    assign(df, df_modif, envir = globalenv())                                          # assign modified df to original dataset from Global
  }else cat("No empty datasets. Nothing to delete")
}

# Apply function to all datasets (tricky to do in for loop because of super assignment)
delete_empty_id(Unic_CTRL_Instr)
delete_empty_id(Unic_CTRL_Solo)
delete_empty_id(Unic_OGL_Instr)
delete_empty_id(Unic_OGL_Solo)

delete_empty_id(Repetat_CTRL_Instr)
delete_empty_id(Repetat_CTRL_Solo)
delete_empty_id(Repetat_OGL_Instr)
delete_empty_id(Repetat_OGL_Solo)

############################### Exclude Outliers based on RT (by subject and stimulus type) #######################################
## DONT RUN (unless it is needed) 
# Exclude RT outliers (=- 2SD) - instead of simple filter, makeing them NA  is better for paired comparison
remove_outliers <- function(df) {
  df_modif <-
    df %>%
    dplyr::group_by(.id, `Stimulus type`) %>%                  # we could have done before:  dplyr::rename("Stim_type" = `Stimulus type`) 
    mutate(SAM_Resp = if_else(abs(SAM_RT - mean(SAM_RT, na.rm=TRUE)) > 2*sd(SAM_RT, na.rm=TRUE), as.numeric(NA), SAM_Resp))
  
  if(!identical(df, df_modif)){
    df <- deparse(substitute(df))
    cat("Deleting outliers from ", print(as.name(df)));                               # print out datasets which have been modified
    assign(df, df_modif, envir = globalenv())                                          # assign modified df to original dataset from Global
  }else cat("No outlier")
}

remove_outliers(Unic_CTRL_Instr)
remove_outliers(Unic_CTRL_Solo)
remove_outliers(Unic_OGL_Instr)
remove_outliers(Unic_OGL_Solo)

remove_outliers(Repetat_CTRL_Instr)
remove_outliers(Repetat_CTRL_Solo)
remove_outliers(Repetat_OGL_Instr)
remove_outliers(Repetat_OGL_Solo)


#################################### Test if datasets have same columns ##################################################
unic_df_obj <- mget(c("Unic_CTRL_Instr", "Unic_CTRL_Solo", "Unic_OGL_Instr", "Unic_OGL_Solo"))
unic_df_obj <-lapply(unic_df_obj, colnames)
outer(unic_df_obj, unic_df_obj, Vectorize(identical))                           # if all are TRUE, all df have same columns

repetat_df_obj <- mget(c("Repetat_CTRL_Instr", "Repetat_CTRL_Solo", "Repetat_OGL_Instr", "Repetat_OGL_Solo"))
repetat_df_obj <-lapply(repetat_df_obj, colnames)
outer(repetat_df_obj, repetat_df_obj, Vectorize(identical))                           # if all are TRUE, all df have same columns


##########################################################################################################################
#################################### Analyses - UNICE ####################################################################
## Descriptives by condition dataset
descriptive_func <- function(df, Stim_type, By_ID = FALSE){
  df_name <- deparse(substitute(df))
  suppressWarnings({                                                                # if all NAs in SAM_Resp, NaNs and Infs will be produced
    df_modif <- 
      df %>%
      dplyr::rename("ID" = .id) %>%
      select_all(~gsub("\\s+|\\.", "_", .)) %>%                                     # replaces blancks with "_" in colnames 
      filter(Stimulus_type == Stim_type)                                            # filter by stimulus type
      
    if(isTRUE(By_ID)){                                                              # if true group by id, if not return descriptives for all ids
      df_modif %>%
      group_by(ID) %>%
      tidystats::describe_data(SAM_Resp, SAM_RT, na.rm = TRUE) %>%
        knitr::kable(caption = as.name(df_name), format = "pandoc", digits = 2)
    }else{ 
      df_modif %>%
      tidystats::describe_data(SAM_Resp, SAM_RT, na.rm = TRUE) %>%
        knitr::kable(caption = as.name(df_name), format = "pandoc", digits = 2)
    }
  })
}  


descriptive_func(df = Unic_CTRL_Instr, Stim_type = "negativ", By_ID = FALSE)           # Negative - General
descriptive_func(df = Unic_CTRL_Solo, Stim_type = "negativ", By_ID = FALSE)
descriptive_func(df = Unic_OGL_Instr, Stim_type = "negativ", By_ID = FALSE)
descriptive_func(df = Unic_OGL_Solo, Stim_type = "negativ", By_ID = FALSE)

descriptive_func(df = Unic_CTRL_Instr, Stim_type = "negativ", By_ID = TRUE)            # Negative - by id
descriptive_func(df = Unic_CTRL_Solo, Stim_type = "negativ", By_ID = TRUE)
descriptive_func(df = Unic_OGL_Instr, Stim_type = "negativ", By_ID = TRUE)
descriptive_func(df = Unic_OGL_Solo, Stim_type = "negativ", By_ID = TRUE)

descriptive_func(df = Unic_CTRL_Instr, Stim_type = "pozitiv", By_ID = FALSE)           # Positive - General
descriptive_func(df = Unic_CTRL_Solo, Stim_type = "pozitiv", By_ID = FALSE)
descriptive_func(df = Unic_OGL_Instr, Stim_type = "pozitiv", By_ID = FALSE)
descriptive_func(df = Unic_OGL_Solo, Stim_type = "pozitiv", By_ID = FALSE)

descriptive_func(df = Unic_CTRL_Instr, Stim_type = "pozitiv", By_ID = TRUE)            # Positive - by id
descriptive_func(df = Unic_CTRL_Solo, Stim_type = "pozitiv", By_ID = TRUE)
descriptive_func(df = Unic_OGL_Instr, Stim_type = "pozitiv", By_ID = TRUE)
descriptive_func(df = Unic_OGL_Solo, Stim_type = "pozitiv", By_ID = TRUE)

descriptive_func(df = Unic_CTRL_Instr, Stim_type = "neutru", By_ID = FALSE)           # Neutral - General
descriptive_func(df = Unic_CTRL_Solo, Stim_type = "neutru", By_ID = FALSE)
descriptive_func(df = Unic_OGL_Instr, Stim_type = "neutru", By_ID = FALSE)
descriptive_func(df = Unic_OGL_Solo, Stim_type = "neutru", By_ID = FALSE)

descriptive_func(df = Unic_CTRL_Instr, Stim_type = "neutru", By_ID = TRUE)            # Neutral - by id
descriptive_func(df = Unic_CTRL_Solo, Stim_type = "neutru", By_ID = TRUE)
descriptive_func(df = Unic_OGL_Instr, Stim_type = "neutru", By_ID = TRUE)
descriptive_func(df = Unic_OGL_Solo, Stim_type = "neutru", By_ID = TRUE)


############################## Merge condition dataset ############################################################
# Must first rename .id to ID in oder to have .id for df names
ID_rename <- function(df){
  if(".id" %in% colnames(df)) {
    df_modif <- 
      df %>%
      dplyr::rename("ID" = .id)
    df <- deparse(substitute(df))
    cat("Changed .id to ID for: ", as.name(df))
    assign(df, df_modif, envir = globalenv())
  }  
}

ID_rename(Unic_CTRL_Instr)
ID_rename(Unic_CTRL_Solo)
ID_rename(Unic_OGL_Instr)
ID_rename(Unic_OGL_Solo)

# Merge into one df
list_df_merge <- list(Unic_CTRL_Instr, Unic_CTRL_Solo, Unic_OGL_Instr, Unic_OGL_Solo)
names(list_df_merge) <- c("Unic_CTRL_Instr", "Unic_CTRL_Solo", "Unic_OGL_Instr", "Unic_OGL_Solo")
Unic_merged <- plyr::ldply(list_df_merge, data.frame)                    # also works for this job bind_rows(list_df_merge, .id = "column_label")


############################## Analyses on Merged ################################################################
## Just a Test 
  # Unic_merged_spread_Neg <- 
  #   Unic_merged %>%
  #   filter(!is.na(SAM_Resp)) %>%                                           # some files had only NA on SAM_Resp and SAM_RT
  #   select(.id, ID, Subj_id, 
  #          Stimuli.order, MarkerStimuli, Stimulus.type, 
  #          SAM_Resp, SAM_RT) %>%
  #   filter(Stimulus.type == "negativ") %>%                                 # dont forget to pick stymulus type
  #   spread(.id, SAM_Resp)
  # 
  # t.test(Unic_merged_spread_Neg$Unic_CTRL_Instr, Unic_merged_spread_Neg$Unic_CTRL_Solo, na.rm = TRUE)
  # t.test(Unic_merged_spread_Neg$Unic_OGL_Instr, Unic_merged_spread_Neg$Unic_OGL_Solo, na.rm = TRUE)
  # t.test(Unic_merged_spread_Neg$Unic_OGL_Instr, Unic_merged_spread_Neg$Unic_CTRL_Instr, na.rm = TRUE)
  # t.test(Unic_merged_spread_Neg$Unic_OGL_Solo, Unic_merged_spread_Neg$Unic_CTRL_Solo, na.rm = TRUE)

## Function prepair data for analyses
prepaire_merged_func <- function(Stim_type){
  Unic_merged %>%
    filter(!is.na(SAM_Resp)) %>%                                           # some files had only NA on SAM_Resp and SAM_RT
    select(.id, ID, Subj_id, 
           Stimuli.order, MarkerStimuli, Stimulus.type, 
           SAM_Resp, SAM_RT) %>%
    dplyr::rename(Cond = .id) %>% 
    filter(Stimulus.type == Stim_type) %>%                                 # dont forget to pick stymulus type
    mutate(Cond = as.factor(Cond))                                         # tunr to factor for aov family functions
}

Unic_merged_Neg <- prepaire_merged_func("negativ")
Unic_merged_Neu <- prepaire_merged_func("neutru")
Unic_merged_Poz <- prepaire_merged_func("pozitiv")

## Anova and Post-Hoc
# Normality 
Unic_merged_Neg %>%
  select(SAM_Resp) %>%                                                     # must select variables outside function 
  tadaatoolbox::tadaa_normtest(method = "shapiro")                         # , print = "markdown"  for Notebook

# Levene Test (p>.05 = homogeneity of variances)
Unic_merged_Neg %>%
  tadaatoolbox::tadaa_levene(data = ., SAM_Resp ~ Cond)                    # , print = "markdown"  for Notebook

# Anova
Unic_merged_Neg %>%
  #do(broom::glance(aov(.$SAM_Resp ~ .$Cond)))                             # regular anova do(broom::tidy(aov(.$SAM_Resp ~ .$Cond)))
  tadaatoolbox::tadaa_aov(data = ., SAM_Resp ~ Cond, type = 1)             # , print = "markdown"  for Notebook

# Post-Hoc 
Unic_merged_Neg %>%
  # Tukey for equal variance 
  tadaatoolbox::tadaa_pairwise_tukey(data = ., SAM_Resp, Cond)             # , print = "markdown"  for Notebook
  # Games Howell does not assume equal variances
  #tadaatoolbox::tadaa_pairwise_gh(data = ., SAM_Resp, Cond)                # , print = "markdown"  for Notebook

## Plots
options(scipen = 999)                                 # positive values bias towards fixed and negative towards scientific notation
theme_set(papaja::theme_apa())                        # theme for plots          

# by dataset
ggplot(Unic_merged, aes(x = Stimulus.type, y = SAM_Resp)) +
  geom_boxplot() +
  stat_summary(fun.data = mean_se,  colour = "darkred") +
  xlab("") +
  facet_wrap(~.id) +
  ggpubr::stat_compare_means(method = "t.test", 
                             label = "p.signif",                                         # to avoid scientific notation of very small p-values
                             #paired = TRUE, 
                             comparisons = list(c("negativ", "neutru"),
                                                c("neutru", "pozitiv"),
                                                c("negativ", "pozitiv")))  

# by Stimulus type
ggplot(Unic_merged, aes(x = .id, y = SAM_Resp)) +
  geom_boxplot() +
  stat_summary(fun.data = mean_se,  colour = "darkred") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Stimulus.type) +
  ggpubr::stat_compare_means(method = "t.test",
                             label = "p.format",                                         # formated p-values
                             #paired = TRUE, 
                             comparisons = list(c("Unic_CTRL_Instr", "Unic_CTRL_Solo"),
                                                c("Unic_CTRL_Instr", "Unic_OGL_Instr"),
                                                c("Unic_CTRL_Solo", "Unic_OGL_Instr"),
                                                c("Unic_CTRL_Solo", "Unic_OGL_Solo"),
                                                c("Unic_OGL_Instr", "Unic_OGL_Solo"),
                                                c("Unic_CTRL_Instr", "Unic_OGL_Solo"))) 


# drop to CTRL vs OGL - by Stimulus type
Unic_merged %>%
  mutate(.id = case_when(.id %in% c("Unic_CTRL_Instr", "Unic_CTRL_Solo") ~ "Unic_CTRL",
                         .id %in% c("Unic_OGL_Instr", "Unic_OGL_Solo") ~ "Unic_OGL",
                         TRUE ~ as.character(.id))) %>%
    ggplot(aes(x = .id, y = SAM_Resp)) +
    geom_boxplot() +
    stat_summary(fun.data = mean_se,  colour = "darkred") +
    xlab("") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~Stimulus.type) +
    ggpubr::stat_compare_means(method = "t.test",
                               label = "p.format",                                         # formated p-values
                               #paired = TRUE, 
                               comparisons = list(c("Unic_CTRL", "Unic_OGL")))          

# drop to Instr vs Solo - by Stimulus type
Unic_merged %>%
  mutate(.id = case_when(.id %in% c("Unic_CTRL_Instr", "Unic_OGL_Instr") ~ "Unic_Instr",
                         .id %in% c("Unic_CTRL_Solo", "Unic_OGL_Solo") ~ "Unic_Solo",
                         TRUE ~ as.character(.id))) %>%
    ggplot(aes(x = .id, y = SAM_Resp)) +
    geom_boxplot() +
    stat_summary(fun.data = mean_se,  colour = "darkred") +
    xlab("") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    facet_wrap(~Stimulus.type) +
    ggpubr::stat_compare_means(method = "t.test",
                               label = "p.format",                                         # formated p-values
                               #paired = TRUE, 
                               comparisons = list(c("Unic_Instr", "Unic_Solo"))) 


##########################################################################################################################
#################################### Analyses - REPETATE #################################################################
## Descriptives by condition dataset
descriptive_func <- function(df, Stim_type, By_ID = FALSE){
  df_name <- deparse(substitute(df))
  suppressWarnings({                                                                # if all NAs in SAM_Resp, NaNs and Infs will be produced
    df_modif <- 
      df %>%
      dplyr::rename("ID" = .id) %>%
      select_all(~gsub("\\s+|\\.", "_", .)) %>%                                     # replaces blancks with "_" in colnames 
      filter(Stimulus_type == Stim_type)                                            # filter by stimulus type
    
    if(isTRUE(By_ID)){                                                              # if true group by id, if not return descriptives for all ids
      df_modif %>%
        group_by(ID) %>%
        tidystats::describe_data(SAM_Resp, SAM_RT, na.rm = TRUE) %>%
        knitr::kable(caption = as.name(df_name), format = "pandoc", digits = 2)
    }else{ 
      df_modif %>%
        tidystats::describe_data(SAM_Resp, SAM_RT, na.rm = TRUE) %>%
        knitr::kable(caption = as.name(df_name), format = "pandoc", digits = 2)
    }
  })
}  


descriptive_func(df = Repetat_CTRL_Instr, Stim_type = "negativ", By_ID = FALSE)           # Negative - General
descriptive_func(df = Repetat_CTRL_Solo, Stim_type = "negativ", By_ID = FALSE)
descriptive_func(df = Repetat_OGL_Instr, Stim_type = "negativ", By_ID = FALSE)
descriptive_func(df = Repetat_OGL_Solo, Stim_type = "negativ", By_ID = FALSE)

descriptive_func(df = Repetat_CTRL_Instr, Stim_type = "negativ", By_ID = TRUE)            # Negative - by id
descriptive_func(df = Repetat_CTRL_Solo, Stim_type = "negativ", By_ID = TRUE)
descriptive_func(df = Repetat_OGL_Instr, Stim_type = "negativ", By_ID = TRUE)
descriptive_func(df = Repetat_OGL_Solo, Stim_type = "negativ", By_ID = TRUE)

descriptive_func(df = Repetat_CTRL_Instr, Stim_type = "pozitiv", By_ID = FALSE)           # Positive - General
descriptive_func(df = Repetat_CTRL_Solo, Stim_type = "pozitiv", By_ID = FALSE)
descriptive_func(df = Repetat_OGL_Instr, Stim_type = "pozitiv", By_ID = FALSE)
descriptive_func(df = Repetat_OGL_Solo, Stim_type = "pozitiv", By_ID = FALSE)

descriptive_func(df = Repetat_CTRL_Instr, Stim_type = "pozitiv", By_ID = TRUE)            # Positive - by id
descriptive_func(df = Repetat_CTRL_Solo, Stim_type = "pozitiv", By_ID = TRUE)
descriptive_func(df = Repetat_OGL_Instr, Stim_type = "pozitiv", By_ID = TRUE)
descriptive_func(df = Repetat_OGL_Solo, Stim_type = "pozitiv", By_ID = TRUE)

descriptive_func(df = Repetat_CTRL_Instr, Stim_type = "neutru", By_ID = FALSE)           # Neutral - General
descriptive_func(df = Repetat_CTRL_Solo, Stim_type = "neutru", By_ID = FALSE)
descriptive_func(df = Repetat_OGL_Instr, Stim_type = "neutru", By_ID = FALSE)
descriptive_func(df = Repetat_OGL_Solo, Stim_type = "neutru", By_ID = FALSE)

descriptive_func(df = Repetat_CTRL_Instr, Stim_type = "neutru", By_ID = TRUE)            # Neutral - by id
descriptive_func(df = Repetat_CTRL_Solo, Stim_type = "neutru", By_ID = TRUE)
descriptive_func(df = Repetat_OGL_Instr, Stim_type = "neutru", By_ID = TRUE)
descriptive_func(df = Repetat_OGL_Solo, Stim_type = "neutru", By_ID = TRUE)


############################## Merge condition dataset ############################################################
# Must first rename .id to ID in oder to have .id for df names
ID_rename <- function(df){
  if(".id" %in% colnames(df)) {
    df_modif <- 
      df %>%
      dplyr::rename("ID" = .id)
    df <- deparse(substitute(df))
    cat("Changed .id to ID for: ", as.name(df))
    assign(df, df_modif, envir = globalenv())
  }  
}

ID_rename(Repetat_CTRL_Instr)
ID_rename(Repetat_CTRL_Solo)
ID_rename(Repetat_OGL_Instr)
ID_rename(Repetat_OGL_Solo)

# Merge into one df
list_df_merge <- list(Repetat_CTRL_Instr, Repetat_CTRL_Solo, Repetat_OGL_Instr, Repetat_OGL_Solo)
names(list_df_merge) <- c("Repetat_CTRL_Instr", "Repetat_CTRL_Solo", "Repetat_OGL_Instr", "Repetat_OGL_Solo")
Repetat_merged <- plyr::ldply(list_df_merge, data.frame)                # also works for this job bind_rows(list_df_merge, .id = "column_label")


############################## Analyses on Merged ################################################################
## Just a Test 
# Repetat_merged_spread_Neg <- 
#   Repetat_merged %>%
#   filter(!is.na(SAM_Resp)) %>%                                           # some files had only NA on SAM_Resp and SAM_RT
#   select(.id, ID, Subj_id, 
#          Stimuli.order, MarkerStimuli, Stimulus.type, 
#          SAM_Resp, SAM_RT) %>%
#   filter(Stimulus.type == "negativ") %>%                                 # dont forget to pick stymulus type
#   spread(.id, SAM_Resp)
# 
# t.test(Repetat_merged_spread_Neg$Repetat_CTRL_Instr, Repetat_merged_spread_Neg$Repetat_CTRL_Solo, na.rm = TRUE)
# t.test(Repetat_merged_spread_Neg$Repetat_OGL_Instr, Repetat_merged_spread_Neg$Repetat_OGL_Solo, na.rm = TRUE)
# t.test(Repetat_merged_spread_Neg$Repetat_OGL_Instr, Repetat_merged_spread_Neg$Repetat_CTRL_Instr, na.rm = TRUE)
# t.test(Repetat_merged_spread_Neg$Repetat_OGL_Solo, Repetat_merged_spread_Neg$Repetat_CTRL_Solo, na.rm = TRUE)

## Function prepair data for analyses
prepaire_merged_func <- function(Stim_type){
  Repetat_merged %>%
    filter(!is.na(SAM_Resp)) %>%                                           # some files had only NA on SAM_Resp and SAM_RT
    select(.id, ID, Subj_id, 
           Stimuli.order, MarkerStimuli, Stimulus.type, 
           SAM_Resp, SAM_RT) %>%
    dplyr::rename(Cond = .id) %>% 
    filter(Stimulus.type == Stim_type) %>%                                 # dont forget to pick stymulus type
    mutate(Cond = as.factor(Cond))                                         # tunr to factor for aov family functions
}

Repetat_merged_Neg <- prepaire_merged_func("negativ")
Repetat_merged_Neu <- prepaire_merged_func("neutru")
Repetat_merged_Poz <- prepaire_merged_func("pozitiv")

## Anova and Post-Hoc
# Normality 
Repetat_merged_Neg %>%
  select(SAM_Resp) %>%                                                     # must select variables outside function 
  tadaatoolbox::tadaa_normtest(method = "shapiro")                         # , print = "markdown"  for Notebook

# Levene Test (p>.05 = homogeneity of variances)
Repetat_merged_Neg %>%
  tadaatoolbox::tadaa_levene(data = ., SAM_Resp ~ Cond)                    # , print = "markdown"  for Notebook

# Anova
Repetat_merged_Neg %>%
  #do(broom::glance(aov(.$SAM_Resp ~ .$Cond)))                             # regular anova do(broom::tidy(aov(.$SAM_Resp ~ .$Cond)))
  tadaatoolbox::tadaa_aov(data = ., SAM_Resp ~ Cond, type = 1)             # , print = "markdown"  for Notebook

# Post-Hoc 
Repetat_merged_Neg %>%
  # Tukey for equal variance 
  tadaatoolbox::tadaa_pairwise_tukey(data = ., SAM_Resp, Cond)             # , print = "markdown"  for Notebook
# Games Howell does not assume equal variances
#tadaatoolbox::tadaa_pairwise_gh(data = ., SAM_Resp, Cond)                # , print = "markdown"  for Notebook

## Plots
options(scipen = 999)                                 # positive values bias towards fixed and negative towards scientific notation
theme_set(papaja::theme_apa())                        # theme for plots          
# by dataset
ggplot(Repetat_merged, aes(x = Stimulus.type, y = SAM_Resp)) +
  geom_boxplot() +
  stat_summary(fun.data = mean_se,  colour = "darkred") +
  xlab("") +
  facet_wrap(~.id) +
  ggpubr::stat_compare_means(method = "t.test", 
                             label = "p.signif",                                         # to avoid scientific notation of very small p-values
                             #paired = TRUE, 
                             comparisons = list(c("negativ", "neutru"),
                                                c("neutru", "pozitiv"),
                                                c("negativ", "pozitiv")))  

# by Stimulus type
ggplot(Repetat_merged, aes(x = .id, y = SAM_Resp)) +
  geom_boxplot() +
  stat_summary(fun.data = mean_se,  colour = "darkred") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Stimulus.type) +
  ggpubr::stat_compare_means(method = "t.test",
                             label = "p.format",                                         # formated p-values
                             #paired = TRUE, 
                             comparisons = list(c("Repetat_CTRL_Instr", "Repetat_CTRL_Solo"),
                                                c("Repetat_CTRL_Instr", "Repetat_OGL_Instr"),
                                                c("Repetat_CTRL_Solo", "Repetat_OGL_Instr"),
                                                c("Repetat_CTRL_Solo", "Repetat_OGL_Solo"),
                                                c("Repetat_OGL_Instr", "Repetat_OGL_Solo"),
                                                c("Repetat_CTRL_Instr", "Repetat_OGL_Solo"))) 


# drop to CTRL vs OGL - by Stimulus type
Repetat_merged %>%
  mutate(.id = case_when(.id %in% c("Repetat_CTRL_Instr", "Repetat_CTRL_Solo") ~ "Repetat_CTRL",
                         .id %in% c("Repetat_OGL_Instr", "Repetat_OGL_Solo") ~ "Repetat_OGL",
                         TRUE ~ as.character(.id))) %>%
  ggplot(aes(x = .id, y = SAM_Resp)) +
  geom_boxplot() +
  stat_summary(fun.data = mean_se,  colour = "darkred") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Stimulus.type) +
  ggpubr::stat_compare_means(method = "t.test",
                             label = "p.format",                                         # formated p-values
                             #paired = TRUE, 
                             comparisons = list(c("Repetat_CTRL", "Repetat_OGL")))          

# drop to Instr vs Solo - by Stimulus type
Repetat_merged %>%
  mutate(.id = case_when(.id %in% c("Repetat_CTRL_Instr", "Repetat_OGL_Instr") ~ "Repetat_Instr",
                         .id %in% c("Repetat_CTRL_Solo", "Repetat_OGL_Solo") ~ "Repetat_Solo",
                         TRUE ~ as.character(.id))) %>%
  ggplot(aes(x = .id, y = SAM_Resp)) +
  geom_boxplot() +
  stat_summary(fun.data = mean_se,  colour = "darkred") +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~Stimulus.type) +
  ggpubr::stat_compare_means(method = "t.test",
                             label = "p.format",                                         # formated p-values
                             #paired = TRUE, 
                             comparisons = list(c("Repetat_Instr", "Repetat_Solo"))) 

##################################################################################################################
##################################################################################################################
