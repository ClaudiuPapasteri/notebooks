# Reads the online participant table, organizes botrec files according to table, logs issues, saves final table


# Prerequisites -----------------------------------------------------------

# Packages
packages <- c("tidyverse", "googledrive", "rio")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

########################

# Settings
folder <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions_SSM_Clean"
setwd(folder)

# Check .botrec files
folder_botrec <- "C:/Users/claud/Desktop/Desktop/aaa new Desktop/STAD/Sessions SSM 01.09-24.11.2022"
# dir(folder_botrec, pattern = ".*botrec") 

# Make new directory, if necessary
folder_clean_botrec <- file.path(folder, "Clean_Sessions_01.09-24.11.2022")

##################################################################################################################
# INTERACTIVE - Get the data from Google Drive ----------------------------

# Get the data from Google Drive
sheet_url <- "https://docs.google.com/spreadsheets/d/1OLTjD36e_SWbf_kTKR7H13kPsJPmi-150uJPVo_avig/edit?usp=sharing"
sheet_name_scalp <- "date SSM Claudiu scalp"
sheet_name_ic <- "date SSM Claudiu IC"
file_name <- paste0("STAD-inregistrari_", Sys.Date())

###############################
if(interactive()){
  want_dl <- readline("Want to re-download data? 1-YES 2-NO: ")
  if (want_dl == 1) {
    # Call Google Drive authentication forcing interactive login and save in cache 
    googledrive::drive_auth(use_oob = TRUE, cache = TRUE)      
    
    # Reuse token to Sheet authentification 
    # googlesheets4::gs4_auth(token = googledrive::drive_token())    # the file is .xlsx not google sheet
    
    # Download the .xlsx from google drive
    googledrive::drive_download(sheet_url, path = file.path(folder, file_name), overwrite = TRUE)
  }
}  
##################################################################################################################
# INTERACTIVE - Read the latest .xlsx file for scalp ----------------------

# Read the latest .xlsx file for scalp SSM
last_xlsx_file <- 
  dir(folder, pattern = ".*xlsx", full.names = TRUE) %>% 
  file.info() %>%
  dplyr::arrange(dplyr::desc(ctime)) %>%
  dplyr::slice(1) %>%
  row.names()

# Check if it is the same that was downloaded this session
check_file <- identical(paste0(file_name, ".xlsx"), basename(last_xlsx_file))

# If it is read it, if not decide what to do
if(interactive() & check_file){
  data <- rio::import(file = last_xlsx_file, which = sheet_name_scalp)
} else {
  want_read <- readline("what you are reading is not the lastest data. Want to read any way? 1-YES 2-NO: ")
  if (want_read == 1) {
    data <- rio::import(file = last_xlsx_file, which = sheet_name_scalp)
  }  
}


# INTERACTIVE - Clean data ------------------------------------------------
# Clean contents of columns with .botrec file names 
data_clean <- 
  data %>% 
  tidyr::separate(col = "output_file", 
                  into = c("output_file", "output_file_sec"),
                  sep = "\\sSI\\s|\\s\\+\\s") %>%                               # some have multiple file names separated by " SI " or " + "
  dplyr::select(where(~!all(is.na(.x)))) %>%                                    # remove if created columns that have only NAs
  dplyr::mutate(
    across(.cols = contains("output_file"),
           .fns = ~ gsub("\\s", "", .x))                                        # remove white spaces
  ) %>%
  dplyr::mutate(
    across(.cols = contains("output_file"),
           .fns = ~ if_else(is.na(.x), "-", .x))                                # replace NAs with "-"
  ) %>%
  dplyr::mutate(
    across(.cols = contains("output_file"),                                  # paste ".botrec" extension to names that contains "TestSession" or "config" but do not end with "botrec"
           .fns = ~ if_else(
             grepl(pattern = "^(?=.*TestSession|config)(?!.*botrec)", .x, perl = TRUE), paste0(.x, ".botrec"), .x)
           )   
  ) %>%
  dplyr::mutate(
    across(.cols = contains("bot_recording"),                                  # paste ".botrec" extension to names that contains "TestSession" or "config" but do not end with "botrec"
           .fns = ~ if_else(
             grepl(pattern = "^(?=.*TestSession|config)(?!.*botrec)", .x, perl = TRUE), paste0(.x, ".botrec"), .x)
          )  
  ) %>%  
  dplyr::mutate(base_name = stringr::str_remove(output_file, "\\.botrec"),
                base_name = dplyr::if_else(base_name == "-", NA_character_, base_name),
                output_file_clean = stringr::str_c(base_name, "_clean", ".botrec")
  ) %>%
  dplyr::select(-base_name)


# Remove 
# Remove elderly participants - they will be in another sheet
data_clean <-
  data_clean %>%
  dplyr::filter(!str_detect(id, "VS"))


# Keep only rows with botrec file names in output_file? 
if(interactive()) {
  want_removerow1 <- readline("Want to remove rows with no botrec file names? 1-YES 2-NO: ")
  if (want_removerow1 == 1) {
    data_clean <-
      data_clean %>%
      dplyr::filter(!is.na(output_file) & output_file != "-")
  }  
} 

# Keep only rows that have singular botrec file in output_file (i.e. no output_file_sec) -- more than 10 recordings are split in two 
if(interactive()) {
  want_removerow2 <- readline("Want to remove rows with no botrec file names? 1-YES 2-NO: ")
  if (want_removerow2 == 1) {
    data_clean <-
      data_clean %>%
      dplyr::filter(is.na(output_file_sec) | output_file_sec == "-")
  }  
} 
      

# Check number of participants
# data_clean %>% dplyr::count(id)
# data_clean %>% dplyr::count(id) %>% dplyr::filter(n > 1) 


##################################################################################################################
# INTERACTIVE - Move .botrec files from table in different processing folder -------------


# Make new directory, if necessary
if(!dir.exists(folder_clean_botrec)){
  dir.create(folder_clean_botrec)
}

# Get .botrec file names
df_files_botrec <-
  data_clean %>%
  dplyr::select(id, contains("output_file"))

colnames_botrec <- grep(pattern = "output_file", colnames(df_files_botrec), value = TRUE)   # columns

pattern_files_botrec <- "^(?=.*TestSession|config)(?=.*botrec)"                        # contains "TestSession" or "config", contains ".botrec"
pattern_files_botrec_noclean <- "^(?=.*TestSession|config)(?!.*_clean)(?=.*botrec)"    # only .botrec files that were not cleaned (do not have "_clean")

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
# Check output_file_clean and clean botrec files
sum(data_clean$output_file != "-")
sum(data_clean$output_file_sec != "-")
sum(!is.na(data_clean$output_file_clean))
file.exists(file.path(folder_clean_botrec, "output_file", data_clean$output_file_clean[!is.na(data_clean$output_file_clean)]))  # its the 27 we already know about

# Checks
check_table1 <- 
  df_files_botrec %>%
  dplyr::select(-id, -output_file_clean) %>%
  dplyr::mutate(
    across(.cols = contains("output_file"),    
           .fns = ~ if_else(
             grepl(pattern = pattern_files_botrec_noclean, .x, perl = TRUE), .x, NA_character_)
    )     
  ) %>%
  as.list() %>%
  sapply(function(x){x[!is.na(x)]})

check_file_count1 <- 
  df_files_botrec %>%
  dplyr::select(-id, -output_file_clean) %>%
  dplyr::summarise(
    across(.cols = contains("output_file"),   # not output_file_clean    
           .fns = ~ sum(grepl(pattern = pattern_files_botrec_noclean, .x, perl = TRUE)))   # count number of valid files
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

#########################################################################################
# INTERACTIVE - Log recording problems in participant table ---------------

# Log recordings with problems
# 1. output_file*_sec column store botrec files for sessions that have multiple files and need column to code for these problems --- THIS WILL PROBALY CHANCE IN FUTURE
# 2. if .botrec file is simply missing
data_clean <-    
  data_clean %>% 
  dplyr::mutate(
    problem = dplyr::if_any(.cols = matches("output_file.*\\_sec"),   # if the is nonNA (not "-") in column containing "sec", tag it in problem column
                     .fns = ~ !.x == "-"),
    problem = as.numeric(problem)
  ) %>%
  dplyr::mutate(
    problem = dplyr::if_else(is.na(output_file) | output_file == "-", 1, problem)
  )    

# 3. if botrec file is listed in diff_list, tag it in problem column (probably the file does not exist, but tag it any way) ---- THIS WILL CHANCE
if(exists(diff_df)) {
  diff_df <- as.data.frame(diff_list)
  match(data_clean$output_file, diff_df$output_file) 
  idx <- which(data_clean$output_file %in%  diff_df$output_file)
  
  data_clean$problem[idx] <- 1    # tag as problematic
}




#########################################################################################
#########################################
# Save output 
data_clean_output <- list(data = data, data_clean = data_clean, 
                          df_files_botrec = df_files_botrec, 
                          check_table1 = check_table1, check_table2 = check_table2, diff_list = diff_list)


saveRDS(data_clean_output, file = file.path(folder, "data_clean_output_01.09-24.11.2022.RDS"))
