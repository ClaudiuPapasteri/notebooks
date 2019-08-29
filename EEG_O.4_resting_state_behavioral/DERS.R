## DERS-36 (Likert 1-5): numbering according to all 41 item initial version but some items are not used: 13,2,11,18,36 -- we gave the 36 final version
# In our study there are only Pre measurments
# Reverse scored: 24, 28, 7, 3, 12, 21, 9, 39, 26, 8, 1
# nonaccept: 29, 25, 15, 14, 33, 27
# goals: 30, 22, 16, 38, 24 
# impulse: 37, 31, 17, 23, 4, 28
# awareness: 7, 3, 12, 21, 9, 39
# strategies:  20, 19, 35, 40, 32, 26, 41, 34 
# clarity: 6, 5, 10, 8, 1
# Ders_Total: nonaccept + goals + impulse + awareness + strategies + clarity

library(tidyverse)

setwd("C:/Users/Mihai/Desktop/O.4 prealabil pt Frontiers/O.4 Scale Scoring")

## Define function copy paste from excel
paste.excel <- function(header = TRUE, ...) {
  read.table("clipboard", sep = "\t", header = header, ...)
}

## Define function copy paste to excel
write.excel <- function(x, row.names = FALSE, col.names = TRUE, ...) {
  write.table(x, "clipboard", sep = "\t", dec = ",", row.names = row.names, col.names = col.names, ...)
}

## Define function that calculates RowSums but only for rows with less than 30% NAs; and return NA if all row values are NA 
SpecialRowSums <- function(df, napercent = .3) {
  ifelse(rowSums(is.na(df)) > ncol(df) * napercent,
         NA,
         rowSums(df, na.rm = TRUE) * NA ^ (rowSums(!is.na(df)) == 0)
  )
}

## Read Data 
Data_othervars <- paste.excel()   
Data_ders <- paste.excel()  

## Check for non-numeric elements in data sets
check_numeric <- as.data.frame(sapply(Data_ders, varhandle::check.numeric)) 
nonnumeric <- sapply(check_numeric, function(x) which(!x, arr.ind = TRUE))   # find row numbers for non-numeric values
cat("Non-numeric elements: \n")
if(length(nonnumeric[lapply(nonnumeric, length) > 0])  != 0){
  print(nonnumeric[lapply(nonnumeric, length) > 0])                            # return only columns and row numbers were non-numeric
}else
  cat("All elements are numberic.")
  
## Remove rows with non-numeric elements
if(length(nonnumeric[lapply(nonnumeric, length) > 0])  != 0){
  deletenonnumeric <- utils::winDialog("yesno", "Delete non-numeric data?")
  if (deletenonnumeric == "YES") {
    Data_ders <- Data_ders[-unique(unlist(nonnumeric)), ]
    Data_othervars <- Data_othervars[-unique(unlist(nonnumeric)), ]              # must delete from all to keep row.number constant
    # row.names(Data_ders) <- 1:nrow(Data_ders)                                  # update row.names ?? - not needed
    # row.names(Data_othervars) <- 1:nrow(Data_othervars) 
    cat("Non-numeric data was deleted.")
  }  
}

## Convert to numeric?
converttonumeric <- utils::winDialog("yesno", "Convert all data to numeric?")
if (converttonumeric == "YES") {
  Data_ders <- 
    Data_ders %>% 
    mutate_if(function(x) is.factor(x) | is.character(x), funs(as.numeric(as.character(.))))
  cat("All data has been converted to numeric.")
}  

## Remove rows with more tha 6 missing (alot of completly missing because the questionnaire is given only 1 time)
missing_index <- which(rowSums(is.na(Data_ders)) > 6, arr.ind=TRUE)
cat("Rows with more than 6 NA:")
print(missing_index)
if(length(missing_index) != 0){
  deleterowsmissing <- utils::winDialog("yesno", "Delete rows with missing values?")
  if (deleterowsmissing == "YES") {
    Data_ders <- Data_ders[-missing_index, ]
    Data_othervars <- Data_othervars[-missing_index, ]
    # row.names(Data_ders) <- 1:nrow(Data_ders)                                 # update row.names ?? - not needed
    # row.names(Data_othervars) <- 1:nrow(Data_othervars)
    cat("Rows with more than 6 NA were deleted")
  }  
}

## Validation (numbers must be 1-5 or NA)
check_valid <- as.data.frame(sapply(Data_ders, function(x) !x %in% c(1:5, NA)))
invalid <- sapply(check_valid, function(x) which(x, arr.ind = TRUE))
invalid <- invalid[lapply(invalid, length) > 0]
cat("Invalid data:")
print(invalid)

if(length(invalid) != 0){
  valid_list_index <- vector()
  modifyinvalid <- utils::winDialog("yesno", "Modify invalid by hand?")
  if (modifyinvalid == "YES") {
    for (name in names(invalid)) {
      var_name <- name
      for (elem in invalid[[name]]){
        elem_name <- elem
        valid_list_index <- c(valid_list_index, paste0("$", var_name, "[", elem_name, "]"))  # cat("$", var_name, "[", elem_name, "]", "\n", sep = "")
      }
    }
    invalid_cells <- paste0("Data_ders", valid_list_index)
    for(cell in invalid_cells){
      edit_code <- paste(cell, " <- ", "utils::edit(", cell, ")")
      eval(parse(text = edit_code))
    }
  }  
}  
  

## Rename items
old <- colnames(Data_ders)
new <- sprintf("ders%d", setdiff(1:41, c(13, 2, 11, 18, 36)))     # items 13,2,11,18,36 are missing but numbering is according to original article
Data_ders <- 
  Data_ders %>%
  rename_at(vars(old), ~ new)

## Reverse scoring
reversed_index <- sprintf("ders%d", c(24, 28, 7, 3, 12, 21, 9, 39, 26, 8, 1))
Data_ders[, reversed_index] <- 6 - Data_ders[, reversed_index]
# we could use: psych::reverse.code(keys, df, mini = rep(1, ncol(df)), maxi = rep(5, ncol(df)))

## Calculate Composite Scores
Data <- Data_othervars
Data$nonaccept <- SpecialRowSums(Data_ders[, sprintf("ders%d", c(29, 25, 15, 14, 33, 27))])
Data$goals <- SpecialRowSums(Data_ders[, sprintf("ders%d", c(30, 22, 16, 38, 24))]) 
Data$impulse <- SpecialRowSums(Data_ders[, sprintf("ders%d", c(37, 31, 17, 23, 4, 28))])
Data$awareness <- SpecialRowSums(Data_ders[, sprintf("ders%d", c(7, 3, 12, 21, 9, 39))])
Data$strategies <-  SpecialRowSums(Data_ders[, sprintf("ders%d", c(20, 19, 35, 40, 32, 26, 41, 34))]) 
Data$clarity <- SpecialRowSums(Data_ders[, sprintf("ders%d", c(6, 5, 10, 8, 1))])
Data$Ders_Total <- SpecialRowSums(Data_ders[, sprintf("ders%d", setdiff(1:41, c(13, 2, 11, 18, 36)))])    # sum all items except 13,2,11,18,36
                                          

## Copy paste to excel
write.excel(Data)