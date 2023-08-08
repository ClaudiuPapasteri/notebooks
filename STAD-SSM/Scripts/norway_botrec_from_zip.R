# Norway zipped .botrec data

zips_folder <- r"(C:\Users\claud\Desktop\Desktop\aaa new Desktop\STAD\Sessions_SSM_Clean\STAD1_behavior_Norway_full)"
setwd(zips_folder)

zips <- list.files(path = zips_folder, full.names	= TRUE)
# botrecs <- zip::zip_list(zips[4])$filename

botrecs_names_from_zip <- function(zip, only_match_sub = FALSE, full_names	= FALSE) {
  files <- zip::zip_list(zip)$filename   # similar base: unzip(zip, list = TRUE)$Name
  botrecs <- files[grep(".botrec", files, fixed = TRUE)]
  if(only_match_sub) {
    pattern <- stringr::str_extract(basename(zip), "sub-\\d{3}")
    idx <- grep(pattern, basename(botrecs))
    botrecs <- botrecs[idx]
  }
  if (full_names) {
    return(file.path(zip, botrecs))
  } else {
    return(botrecs)
  }
} 
# botrecs_names_from_zip(zips[4])
# botrecs_names_from_zip(zips[4], only_match_sub = TRUE)

# unzip to temporary folder
tmp_folder <- tempdir()
win_tmp_folder <- gsub("\\\\", "/", tmp_folder)
win_tmp_folder

# batch extract only botrecs from zips
for(i in seq_along(zips)) {
  zip::unzip(
    zipfile = zips[i], 
    files = botrecs_names_from_zip(zips[i], only_match_sub = TRUE),
    exdir = tmp_folder
  )
}

# copy all botrecs to another folder
folder_to <- r"(C:\Users\claud\Desktop\Desktop\aaa new Desktop\STAD\Sessions_SSM_Clean\STAD1_behavior_Norway_botrec)"
botrecs_names_tmp <- list.files(path = tmp_folder, full.names	= TRUE, recursive = TRUE)

for(i in seq_along(botrecs_names_tmp)) {
  file.copy(from = botrecs_names_tmp[i], to = folder_to)
}

# check that all are copied 
botrecs_names_to <- list.files(path = folder_to)
check <- setdiff(botrecs_names_to, basename(botrecs_names_tmp))  # duplicated(basename(botrecs_names_tmp))

# delete temp folder if check passed
if(identical(check, character(0))) unlink(tmp_folder, recursive = TRUE, force = TRUE, expand = TRUE)
