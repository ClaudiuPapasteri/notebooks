library(googledrive)
library(tidyverse)
# library(googlesheets4)

# Settings
folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Comportament STAD"
setwd(folder)

##################################################################################################################
# Get the data from Google Drive
sheet_url <- "https://docs.google.com/spreadsheets/d/1p8pVPv6PiVOtS_qdEY5npaIAQqOzZQyT"
sheet_name_scalp <- "log EEG SSM"
sheet_name_ic <- "log EEGic SSM"
file_name <- "SSM_log_Scalp_Aprilie.xlsx"

###############################
# Call Google Drive authentication forcing interactive login and save in cache 
googledrive::drive_auth(use_oob = TRUE, cache = TRUE)      

# Reuse token to Sheet authentification 
# googlesheets4::gs4_auth(token = googledrive::drive_token())    # the file is .xlsx not google sheet

# Download the .xlsx from google drive
googledrive::drive_download(sheet_url, path = file.path(folder, file_name), overwrite = TRUE)
##################################################################################################################


# Read the .xlsx file
data <- rio::import(file = file.path(folder, file_name), which = sheet_name_scalp)

# 
data <- 
  data %>% 
  janitor::clean_names()

# Clean contents of columns with .botrec file names 
data_clean <- 
  data %>% 
  tidyr::separate(col = "fisier_obtinut1", 
                  into = c("fisier_obtinut1", "fisier_obtinut1_sec"),
                  sep = "\\sSI\\s|\\s\\+\\s") %>%                               # some have multiple file names separated by " SI " or " + "
  tidyr::separate(col = "fisier_obtinut2", 
                  into = c("fisier_obtinut2", "fisier_obtinut2_sec"),
                  sep = "\\sSI\\s|\\s\\+\\s") %>%                               # some have multiple file names separated by " SI " or " + "
  tidyr::separate(col = "fisier_obtinut3", 
                  into = c("fisier_obtinut3", "fisier_obtinut3_sec"),
                  sep = "\\sSI\\s|\\s\\+\\s") %>%                               # some have multiple file names separated by " SI " or " + "
  dplyr::select(where(~!all(is.na(.x)))) %>%                                    # remove created columns that have only NAs
  dplyr::mutate(
    across(.cols = contains("fisier_obtinut"),
           .fns = ~ gsub("\\s", "", .x))                                        # remove white spaces
  ) %>%
  dplyr::mutate(
    across(.cols = contains("fisier_obtinut"),
           .fns = ~ if_else(is.na(.x), "-", .x))                                # replace NAs with "-"
  ) %>%
  dplyr::mutate(
    across(.cols = contains("fisier_obtinut"),                                  # paste ".botrec" extension to names that contains "TestSession" or "config" but do not end with "botrec"
           .fns = ~ if_else(
             grepl(pattern = "^(?=.*TestSession|config)(?!.*botrec)", .x, perl = TRUE), paste0(.x, ".botrec"), .x)
           )   
  )



# Check .botrec files
folder_botrec <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions"

# Make new directory, if necessary
folder_clean_botrec <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_Clean"

if(!dir.exists(folder_clean_botrec)){
  dir.create(folder_clean_botrec)
}

# Get .botrec file names
df_files_botrec <-
  data_clean %>%
  dplyr::select(id, contains("fisier_obtinut"))

colnames_botrec <- grep(pattern = "fisier_obtinut", colnames(df_files_botrec), value = TRUE)   # columns

pattern_files_botrec <- "^(?=.*TestSession|config)(?=.*botrec)"                        # contains "TestSession" or "config", contains ".botrec"

###############################
# Copy .botrec files to new location
for(col in colnames_botrec){
  dir_name <- file.path(folder_clean_botrec, col)
  
  if(!dir.exists(dir_name)){
    dir.create(dir_name)
  }
  
  for(row in seq_len(nrow(df_files_botrec))){
    file_name <- df_files_botrec[row, col]
    full_file_path <- file.path(folder_botrec, file_name)
    new_file_path <- file.path(dir_name, file_name)
    
    if(file.exists(full_file_path) & !file.exists(new_file_path)){
      file.copy(from = full_file_path, to = new_file_path)
    }
  }
}
###############################

#########################################################################################
# Checks
check_table1 <- 
  df_files_botrec %>%
  dplyr::select(-id) %>%
  dplyr::mutate(
    across(.cols = contains("fisier_obtinut"),                                  # paste ".botrec" extension to names that contains "TestSession" but do not end with "botrec"
           .fns = ~ if_else(
             grepl(pattern = pattern_files_botrec, .x, perl = TRUE), .x, NA_character_)
    )     
  ) %>%
  as.list() %>%
  sapply(function(x){x[!is.na(x)]})

check_file_count1 <- 
  df_files_botrec %>%
  dplyr::summarise(
    across(.cols = contains("fisier_obtinut"), 
           .fns = ~ sum(grepl(pattern = pattern_files_botrec, .x, perl = TRUE)))   # count number of valid files
  ) %>%
  unlist()

check_table2 <- list.files(folder_clean_botrec, pattern = ".", all.files = FALSE, recursive = TRUE, full.names = FALSE) %>%
  split(., dirname(.)) %>%
  sapply(basename)

check_file_count2 <- 
  check_table2 %>%
  sapply(length)

identical(setdiff(check_file_count1, check_file_count2), numeric(0))   # check counts

setdiff(unlist(check_table1), unlist(check_table2))                    # check all elements

# Create a list that stores vectors named like dataframe columns, that contain the different elements (the recording names)
diff_list <- list()
for(vec in seq_len(length(check_table1))){                             # get element names and indexes for unmatched
  name_elem <- setdiff(check_table1[[vec]], check_table2[[vec]])
  if(!identical(name_elem, character(0))){
    idx_elem <- match(name_elem, check_table1[[vec]], nomatch = NA_integer_)
    name_vec <- names(check_table1)[vec]
    diff_list[[name_vec]] <- c(diff_list[[name_vec]], name_elem)
    cat("Element ", name_elem, "indexed as element ", idx_elem, "from list ", name_vec, "\n")
  }
}

#########################################
# Save output 
main_folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_Clean_demoimitMectrics"
setwd(main_folder)

data_clean_output <- list(data = data, data_clean = data_clean, 
                          df_files_botrec = df_files_botrec, 
                          check_table1 = check_table1, check_table2 = check_table2, diff_list = diff_list)


saveRDS(data_clean_output, file = file.path(main_folder, "data_clean_output.RDS"))
