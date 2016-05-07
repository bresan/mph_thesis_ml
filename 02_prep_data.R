#######################################
## Grant Nguyen
## Purpose: Clean input data appropriately

#####################################################
## Set filepaths to code and data
if(Sys.info()[1] =="Windows") {
  master_dir <- "H:/Thesis"
  code_dir <- paste0(master_dir,"/mph_thesis_ml")
} else {
  master_dir <- "/Users/Grant/Desktop/Thesis"
  code_dir <- paste0(master_dir,"/code")
}

data_dir <- paste0(master_dir,"/data")


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
## Import Data
# data <- data.table(read.dta(paste0(data_dir,"/risk score database for PLoS One_updated 25.9.14.dta")))
# data <- data.table(read.dta13(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_3rd Nov 2016.dta")))
# data <- data.table(read_dta(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_Version 2013.dta")))

  master_data <- data.table(fread(paste0(data_dir,"/01_cleaned_data.csv")))


#####################################################
## Generate a count of all missing values and output it
  count_missing <- function(x) {
    n <- length(x[is.na(x)])
    return(n)
  }
  
  missing_data <- data.table(master_data[,lapply(.SD,count_missing)])
  missing_data <- melt(missing_data)
  setnames(missing_data,"value","num_missing")
  missing_data <- missing_data[num_missing != 0,]
  missing_data <- missing_data[order(-num_missing)]
  write.csv(missing_data,paste0(data_dir,"/02_missing_vals.csv"),row.names=F)
  
#####################################################
## Impute all missing values using Multiple Imputation

## Define MI wrapper to apply different types of MI
impute_missing <- function(data,pred_vars,mi_type="mi_gelman") {

  
  ## Apply multiple imputation to a dataset and a set of prediction variables, using Andrew Gelman's MI package
  ## We use this to feed imputed data into the Random Forests analysis, as it requires a complete dataset
  ## MI comparisons here: http://thomasleeper.com/Rcourse/Tutorials/mi.html
  if(mi_type == "mi_gelman") {
    require(mi)
    data <- missing_data.frame(data.frame(data[,.SD,.SDcols=c(pred_vars)]))
    show(data)
    # change() # If I want to change the transformation, imputation method,etc
    summary(data)
    imputations <- mi(data)
    Rhats(imputations)
    new <- complete(imputations)
    lapply(new,summary)
    
    ## Here, return the imputations as separate data frames (or do we just run the analysis separately here?)
  } else if(mi_type == "amelia") {
    
  } else if(mi_type == "mice") {
    
  }
}

## Apply MI function
pred_vars <- names(master_data)[!names(master_data) %in% c("death","malariadeath")]

## Test out MI on 5,000 random observations, before scaling it up and applying the true function
mi_type = "mi_gelman"
set.seed(9840294)
data <- copy(master_data) 
data[,sort_obs:=rnorm(nrow(data))]
data <- data[order(sort_obs)]
data <- data[1:5000,]
data[,sort_obs:=NULL]

imputed_data <- system.time(impute_missing(data,pred_vars,mi_type="mi_gelman"))

