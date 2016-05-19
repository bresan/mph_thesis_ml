## Grant Nguyen
## Prepare cleaned data from 01_clean_data, and prepare it for analysis using various techniques


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


#####################################################
## Import Data
# data <- data.table(read.dta(paste0(data_dir,"/risk score database for PLoS One_updated 25.9.14.dta")))
# data <- data.table(read.dta13(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_3rd Nov 2016.dta")))
# data <- data.table(read_dta(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_Version 2013.dta")))

master_data <- data.table(fread(paste0(data_dir,"/01_cleaned_data.csv")))


#####################################################
## Classify variables appropriately
death_vars <- c("death","malariadeath")
data_stubs <- substr(names(master_data),1,2)
dx_vars <- names(master_data)[data_stubs == "dx"]
ss_vars <- names(master_data)[data_stubs == "ss"]
cv_vars <- names(master_data)[data_stubs == "cv"]
te_vars <- names(master_data)[data_stubs == "te"]
tr_vars <- names(master_data)[data_stubs == "tr"]

uncat_vars <- names(master_data)[!names(master_data) %in% c(death_vars,dx_vars,ss_vars,cv_vars,te_vars,tr_vars)]
if(length(uncat_vars) > 0) {
  print("The following variables are not categorized as outcomes or one of dx,ss,cv,te,tr") 
  print(uncat_vars)
  stop()
}


#####################################################
## Prepare for random forest and logistic regression
predict_vars <- c(dx_vars,ss_vars,cv_vars,te_vars,tr_vars)
outcome_vars <- "death"

## Generate a test dataset with only signs and symptoms and 5000 random observations, for easier testing of methods
test_predict_vars <- ss_vars
set.seed(9840294)
test_data <- copy(master_data[,.SD,.SDcols=c(test_predict_vars,outcome_vars)]) 
test_data[,sort_obs:=rnorm(nrow(test_data))]
test_data <- test_data[order(sort_obs)]
test_data <- test_data[1:5000,]
test_data[,sort_obs:=NULL]


#####################################################
## Output data objects to feed into random forest and logistic regression
save.image(file=paste0(data_dir,"/02_prepped_data.RData"))


