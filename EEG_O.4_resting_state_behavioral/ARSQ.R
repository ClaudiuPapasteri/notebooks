## ARSQ (Likert 1-5) -- no reverse scoring, in total 30 items to score + 25 additional single items
# discont: 1, 2, 3
# tom: 4, 5, 6
# self: 7, 8, 9
# planning: 10, 11, 12
# sleep: 13, 14, 15
# comfort: 16, 17, 18
# somatic: 19, 20, 21
# health: 22, 23, 24
# visual: 25, 26, 27
# verbal: 28, 29, 30
# + 25 individual items

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
Data_arsq <- paste.excel()  

## Check for non-numeric elements in data sets
check_numeric <- as.data.frame(sapply(Data_arsq, varhandle::check.numeric)) 
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
    Data_arsq <- Data_arsq[-unique(unlist(nonnumeric)), ]
    Data_othervars <- Data_othervars[-unique(unlist(nonnumeric)), ]              # must delete from all to keep row.number constant
    # row.names(Data_arsq) <- 1:nrow(Data_arsq)                                  # update row.names ?? - not needed
    # row.names(Data_othervars) <- 1:nrow(Data_othervars) 
    cat("Non-numeric data was deleted.")
  }  
}

## Convert to numeric?
converttonumeric <- utils::winDialog("yesno", "Convert all data to numeric?")
if (converttonumeric == "YES") {
  Data_arsq <- 
    Data_arsq %>% 
    mutate_if(function(x) is.factor(x) | is.character(x), funs(as.numeric(as.character(.))))
  cat("All data has been converted to numeric.")
}  

## Remove rows with more tha 6 missing 
missing_index <- which(rowSums(is.na(Data_arsq)) > 6, arr.ind=TRUE)
cat("Rows with more than 6 NA:")
print(missing_index)
if(length(missing_index) != 0){
  deleterowsmissing <- utils::winDialog("yesno", "Delete rows with missing values?")
  if (deleterowsmissing == "YES") {
    Data_arsq <- Data_arsq[-missing_index, ]
    Data_othervars <- Data_othervars[-missing_index, ]
    # row.names(Data_arsq) <- 1:nrow(Data_arsq)                                 # update row.names ?? - not needed
    # row.names(Data_othervars) <- 1:nrow(Data_othervars)
    cat("Rows with more than 6 NA were deleted")
  }  
}

## Validation (numbers must be 1-5 or NA)
check_valid <- as.data.frame(sapply(Data_arsq, function(x) !x %in% c(1:5, NA)))
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
    invalid_cells <- paste0("Data_arsq", valid_list_index)
    for(cell in invalid_cells){
      edit_code <- paste(cell, " <- ", "utils::edit(", cell, ")")
      eval(parse(text = edit_code))
    }
  }  
}  


## Rename items
old <- colnames(Data_arsq)
new <- sprintf("arsq%d", c(1:55))     # only 30 items are in Composite Scores, 20 are individual 
Data_arsq <- 
  Data_arsq %>%
  rename_at(vars(old), ~ new)


## Calculate Composite Scores
Data <- Data_othervars
Data$discont <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(1, 2, 3))])
Data$tom <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(4, 5, 6))]) 
Data$self <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(7, 8, 9))])
Data$planning <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(10, 11, 12))])
Data$sleep <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(13, 14, 15))]) 
Data$comfort <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(16, 17, 18))])
Data$somatic <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(19, 20, 21))])
Data$health <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(22, 23, 24))])
Data$visual <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(25, 26, 27))])
Data$verbal <- SpecialRowSums(Data_arsq[, sprintf("arsq%d", c(28, 29, 30))])


Data <- cbind(Data, Data_arsq[, sprintf("arsq%d", c(31:55))])


## Copy paste to excel
write.excel(Data)


