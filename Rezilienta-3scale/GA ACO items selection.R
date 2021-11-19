# GA item selection test for Resilience
# tutorial: https://okan.cloud/posts/2021-01-19-how-to-shorten-a-measurement-instrument-automatically-part-ii/

# Load packages
if(!require(GAabbreviate)) install.packages("GAabbreviate")
library(GAabbreviate)
library(tidyverse)

# Read Data
folder <- "C:/Users/Mihai/Desktop/R Notebooks/notebooks/Rezilienta-3scale"
file <- "BAZA DE DATE FINALA VARIANTA EXCEL (1).xlsx"
setwd(folder)

suppressMessages({                                   # some columns are renamed and the warning breaks pandoc
  Data <- rio::import(file.path(folder, file),
                      which = "Sheet1",
                      skip = 0)
})



### GA item select
items <- Data[, c(sprintf("a%d", 1:25), sprintf("b%d", c(1:10, 12:33)), sprintf("c%d", 1:25))]   # exclude b11 with all values = 2.109 (no variance)
items <- as.matrix(sapply(items, as.integer))

scales <- rowSums(Data[, c(sprintf("a%d", 1:25), sprintf("b%d", c(1:10, 12:33)), sprintf("c%d", 1:25))], na.rm = TRUE)  
scales <- as.matrix(sapply(scales, as.integer))

GAA <- GAabbreviate(items = items, scales = scales,
                    itemCost = 0.01, # The cost of each item
                    maxItems = 6, # Max number of items per dimension
                    maxiter = 1000, # Max number of iterations
                    run = 100, # Number of runs
                    crossVal = TRUE, # Cross-validation
                    seed = 2021) # Seed for reproducibility

plot(GAA)
summary(GAA)




#############################

if(!require(ShortForm)) install.packages("ShortForm")
library(ShortForm)

items <- Data[, c(sprintf("a%d", 1:25), sprintf("b%d", c(1:10, 12:33)), sprintf("c%d", 1:25))]   # exclude b11 with all values = 2.109 (no variance)

item_names <- names(items)

model <- paste("Total =~", paste(item_names, collapse = " + "))
model
item_name_list <- list(item_names)    # list(c(), c()) for 2 dimensions


ACO <- antcolony.lavaan(data = items, # Response data set
                        ants = 20, # Number of ants
                        evaporation = 0.9, #  % of the pheromone retained after evaporation
                        antModel = model, # Factor model for ECR
                        list.items = item_name_list, # Items for each dimension
                        full = ncol(items), # The total number of unique items in the full scale
                        i.per.f = 10, # The desired number of items per dimension, e.g. c(6, 6) for 2 dimensions
                        factors = "Total", # Names of dimensions, e.g. c('Avoidance','Anxiety')
                        # lavaan settings - Change estimator to WLSMV
                        lavaan.model.specs = list(model.type = "cfa", auto.var = T, estimator = "WLSMV",
                                                  ordered = NULL, int.ov.free = TRUE, int.lv.free = FALSE, 
                                                  auto.fix.first = TRUE, auto.fix.single = TRUE, 
                                                  auto.cov.lv.x = TRUE, auto.th = TRUE, auto.delta = TRUE,
                                                  auto.cov.y = TRUE, std.lv = F),
                        steps = 50, # The number of ants in a row for which the model does not change
                        fit.indices = c('cfi', 'tli', 'rmsea'), # Fit statistics to use
                        fit.statistics.test = "(cfi > 0.90)&(tli > 0.90)&(rmsea < 0.08)",
                        max.run = 1000) # The maximum number of ants to run before the algorithm stops
