# library(tidyverse)

main_folder <- "C:/Users/Mihai/Desktop/R Notebooks/notebooks/STAD-demoimitMetrics/Sessions_Clean_demoimitMectrics"
setwd(main_folder)

#################

# Read clean table
data_clean_list <- readRDS("data_clean_output.RDS")
data_clean <- data_clean_list$data_clean

diff_list <- data_clean_list$diff_list

# Fix some stuff in clean table
pattern_files_botrec <- "^(?=.*TestSession|config)(?=.*botrec)"                        # contains "TestSession" or "config", contains ".botrec"

data_clean2 <-    # 1. fisier_obtinut3 need to be coalesced to fisier_obtinut1 or fisier_obtinut2
  data_clean %>%
  dplyr::mutate(
    across(.cols = contains("fisier_obtinut"),                                  # anything not compling to botrec file pattern to NA
           .fns = ~ if_else(
             grepl(pattern = pattern_files_botrec, .x, perl = TRUE), .x, NA_character_) 
    )   
  ) %>%
  dplyr::mutate(fisier_obtinut1 = dplyr::coalesce(fisier_obtinut1, fisier_obtinut3)) %>% # coalesce fisier_obtinut3 recordings in fisier_obtinut1 --- THIS WILL PROBALY CHANCE IN FUTURE
  dplyr::mutate(conditie1 = dplyr::coalesce(conditie1, conditie3))        # do the same for conditie --- THIS WILL PROBABLY CHANGE

data_clean2 <-    # 2. fisier_obtinut*_sec column store botrec files for sessions that have multiple files and need column to code for these problems --- THIS WILL PROBALY CHANCE IN FUTURE
  data_clean2 %>% 
  dplyr::mutate(
    problem = if_any(.cols = matches("fisier_obtinut.*\\_sec"),   # if the is nonNA in colums containing "sec", tag it in problem column
                 .fns = ~ !is.na(.x)),
    problem = as.numeric(problem)
  )

# 3. if botrec file is listed in diff_list, tag it in problme column (probably the file does not exist, but tag it any way)
# ---- THIS WILL CHANCE
diff_df <- as.data.frame(diff_list)
match(data_clean2$fisier_obtinut2, diff_df$fisier_obtinut2) 
idx <- which(data_clean2$fisier_obtinut2 %in%  diff_df$fisier_obtinut2)

data_clean2$problem[idx] <- 1    # tag as problematic 

# Read lists of recordings with demoimitMetrics
fisier_obtinut1 <- readRDS("fisier_obtinut1.RDS")
fisier_obtinut2 <- readRDS("fisier_obtinut2.RDS")
fisier_obtinut3 <- readRDS("fisier_obtinut3.RDS")

# Define function
list_to_nested <- function(list, group_var){
  list %>%
    do.call(dplyr::bind_rows, .) %>%
    dplyr::group_by( {{ group_var }} ) %>%
    tidyr::nest()
}

# Lists of recordings to nested data frames
fisier_obtinut1_df <- list_to_nested(fisier_obtinut1, file)
fisier_obtinut2_df <- list_to_nested(fisier_obtinut2, file)
fisier_obtinut3_df <- list_to_nested(fisier_obtinut3, file)


# Rowbind fisier_obtinut3 to fisier_obtinut1 --- THIS WILL PROBABLY CHANGE
fisier_obtinut1_df <- dplyr::bind_rows(fisier_obtinut1_df, fisier_obtinut3_df)
rm(fisier_obtinut3_df)   # this was incorporated in fisier_obtinut3 in clean table and also in nested dataframe of sessions

# Merge nested data frames into clean table
full_data <- 
  data_clean2 %>%
  dplyr::left_join(fisier_obtinut1_df, by = c("fisier_obtinut1" = "file")) %>%
  dplyr::rename(data_fisier_obtinut1 = data) %>%
  dplyr::left_join(fisier_obtinut2_df, by = c("fisier_obtinut2" = "file")) %>%
  dplyr::rename(data_fisier_obtinut2 = data) 



# Data for export
full_data_output <-
  full_data %>%
  dplyr::mutate(id = toupper(id),
                partener1 = toupper(partener1),
                partener2 = toupper(partener2)) %>%
  dplyr::filter(id != "TEST") %>%
  dplyr::filter(problem != 1) %>%
  dplyr::select(-c("nume", "contact", "subiect_eeg1", "subiect_eeg2", "ora1", "ora2", "ora3", "casca1", "casca2", "casca3",
                   "observatii1", "observatii2", "observatii3", "inreg_folosita3", "fisier_obtinut3", "data3_in_caz_de_erori_la_data1_sau2",
                   "fisier_obtinut2_sec", "conditie3", "asistenti3", "config_folosita3"))

full_data_output <- full_data_output[!unlist(lapply(full_data_output$data_fisier_obtinut1, is.null)),]   # drop rows that have NULL in data columns
full_data_output <- full_data_output[!unlist(lapply(full_data_output$data_fisier_obtinut2, is.null)),]

full_data_output_clean <- 
  full_data_output %>% 
  tidyr::pivot_longer(cols = c(data_fisier_obtinut1, data_fisier_obtinut2), names_to = "order", values_to = "data") %>%
  unnest(cols = data) 


