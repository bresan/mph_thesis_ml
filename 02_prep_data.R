#######################################
## Grant Nguyen
## Purpose: Clean input data appropriately

#####################################################
## Set filepaths to code and data
master_dir <- "/Users/Grant/Desktop/Thesis/"
data_dir <- paste0(master_dir,"/data")
code_dir <- paste0(master_dir,"/code")


#####################################################
## Set Packages
require(data.table) # For easy data management
require(ggplot2) # For graphing
require(reshape2) # For potential reshapes 
require(foreign) # For importing dataset
require(rpart) # Decision trees
require(readstata13) # Importing dataset
require(haven)


#####################################################
## Set Packages
data <- data.table(read.dta(paste0(data_dir,"/risk score database for PLoS One_updated 25.9.14.dta")))
data <- data.table(read.dta13(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_3rd Nov 2016.dta")))
data <- data.table(read_dta(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_Version 2013.dta")))

data[,SiteID:=as.character(SiteID)]
sites <- unique(data[,SiteID])

combined_data <- data[include==1,]


impute_missing <- function(data,pred_vars) {
  ## Apply multiple imputation to a dataset and a set of prediction variables, using Andrew Gelman's MI package
  ## We use this to feed imputed data into the Random Forests analysis, as it requires a complete dataset
  ## MI comparisons here: http://thomasleeper.com/Rcourse/Tutorials/mi.html
  require(mi)
  data <- missing_data.frame(data.frame(test[,.SD,.SDcols=c(pred_vars)]))
  show(data)
  # change() # If I want to change the transformation, imputation method,etc
  summary(data)
  imputations <- mi(data)
  Rhats(imputations)
  new <- complete(imputations)
  lapply(new,summary)
  
  ## Here, return the imputations as separate data frames (or do we just run the analysis separately here?)
}

