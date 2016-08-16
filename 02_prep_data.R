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
library(data.table) # For easy data management
library(ggplot2) # For graphing


#####################################################
## Import Data
# data <- data.table(read.dta(paste0(data_dir,"/risk score database for PLoS One_updated 25.9.14.dta")))
# data <- data.table(read.dta13(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_3rd Nov 2016.dta")))
# data <- data.table(read_dta(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_Version 2013.dta")))

master_data <- data.table(fread(paste0(data_dir,"/01_cleaned_data.csv")))

## Remove all data that has missing deaths
master_data <- master_data[death!="Missing",]

## Drop weight for now, until I can figure out how/whether to use it
master_data[,ss_weight:=NULL]

## Drop monthyear for now as well -- it seems to carry undue weight re: Death
master_data[,cv_monthyear:=NULL]

#####################################################
## Classify variables appropriately
death_vars <- c("death")
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
pred_formula <- as.formula(paste("death~",paste(predict_vars,collapse="+")))
outcome_vars <- "death"

## Generate a test dataset with only signs and symptoms and 5000 random observations, for easier testing of methods
test_predict_vars <- predict_vars
test_formula <- as.formula(paste("death~",paste(test_predict_vars,collapse="+")))

set.seed(9840294)
test_data <- copy(master_data[,.SD,.SDcols=c(test_predict_vars,outcome_vars)]) 
test_data[,sort_obs:=rnorm(nrow(test_data))]
test_data <- test_data[order(sort_obs)]
test_data <- test_data[1:5000,]
test_data[,sort_obs:=NULL]

## Convert both datasets' characters to factor variables
## We do this here to avoid the test data from having factor levels that don't actually exist in the test dataset 

# First, define a function that encodes the reference groups appropriately
convert_factors <- function(dt) {
  char_cols <- names(dt[,.SD,.SDcols=sapply(dt,is.character)])
  nonmiss_factors <- c("cv_gender","cv_site_id","ss_airway","ss_pallor","tr_anti_malarial")
  miss_factors <- char_cols[!char_cols %in% c(nonmiss_factors,"cv_age","te_malaria_test","death")]  
  age_levels <- c("48 to 60 months","0 to 1 months","2 to 4 months","4 to 7 months","7 to 12 months",
                  "12 to 24 months","24 to 36 months","36 to 48 months")
  malaria_levels <- c("None","Missing","Complicated","Uncomplicated")
  death_levels <- c("No","Yes")
  
  if(length(char_cols) > 0 & length(char_cols) != length(names(dt))) {
    miss_dt <- dt[,lapply(.SD,factor,levels=c("No","Yes","Missing")),.SDcols=miss_factors]
    nonmiss_dt <- dt[,lapply(.SD,as.factor),.SDcols=nonmiss_factors]
    age_dt <- dt[,lapply(.SD,factor,levels=age_levels),.SDcols="cv_age"]
    mal_dt <- dt[,lapply(.SD,factor,levels=malaria_levels),.SDcols="te_malaria_test"]
    death_dt <- dt[,lapply(.SD,factor,levels=death_levels),.SDcols="death"]
    
    dt[,c(char_cols):=NULL]
    dt <- cbind(dt,miss_dt,nonmiss_dt,age_dt,mal_dt,death_dt)
  } else if(length(char_cols)==length(names(dt))) {
    dt <- dt[,lapply(.SD,as.factor),.SDcols=char_cols]
  }
  return(dt)
}

master_data <- convert_factors(master_data) 
test_data <- convert_factors(test_data)

## Check for missing variables in dataset (random forests do not do well with missing observations)
## Solution: Convert to character and label as informative missing
check_missing <- function(dt) {
  num_cols <- names(dt[,.SD,.SDcols=sapply(dt,is.numeric)])
  if(length(num_cols) > 0) {
    miss_dt <- dt[,lapply(.SD,function(x) sum(is.na(x))),.SDcols=num_cols]
    miss_dt[,id:=1]
    miss_dt <- melt(miss_dt,id.vars="id",measure.vars=num_cols,value.name="num_missing")
    miss_vars <- unique(miss_dt[num_missing>0,as.character(variable)])
    miss_dt[,id:=NULL]
  } else miss_vars <- NULL
  
  if(length(miss_vars) > 0) {
    print(paste0("The following variables have missing values: "))
    print(miss_dt[num_missing>0,])
    stop()
  } else {
    print(paste0("No variables with missing values present in the dataset"))
  }
}

check_missing(master_data)
check_missing(test_data)


#####################################################
## Output data objects to feed into random forest and logistic regression
save.image(file=paste0(data_dir,"/02_prepped_data.RData"))

## Create a table with number of total cases, proportion of dead out of cases, and number of dead out of cases
  dx_cols <- names(master_data)[grepl("dx_admit_",names(master_data)) | grepl("dx_final_",names(master_data))]
  dx_cols <- dx_cols[!dx_cols %in% c("dx_admit_count","dx_final_count")]
  
  total_cases <- master_data[,lapply(.SD,sum),.SDcols=dx_cols]
  total_dead <- master_data[death=="Yes",lapply(.SD,sum),.SDcols=dx_cols]
  
  ## Format variable names
  format_dx_vars <- function(data) {
    data[,id:=1]
    data <- melt(data,id.vars="id")
    
    data[grepl("admit",variable),admit_type:="Admission"] 
    data[grepl("final",variable),admit_type:="Discharge"]
    data[,diag_var:=as.numeric(gsub(".*_","",variable))]
    data[,c("id","variable"):=NULL]
    return(data)
  }
  
  total_cases <- format_dx_vars(total_cases)
  setnames(total_cases,"value","cases")
  total_dead <- format_dx_vars(total_dead)
  setnames(total_dead,"value","case_deaths")
  
  ## Combine datasets and add on diagnosis longnames
  dx_master <- merge(total_cases,total_dead,by=c("diag_var","admit_type"))
  dx_master[,case_death_rt := case_deaths/cases]
  
  diag_map <- fread(paste0(data_dir,"/diagnosis_map.csv"))
  setnames(diag_map,"diag_code","diag_var")
  diag_map[,short_name:=NULL]
  dx_master <- merge(dx_master,diag_map,by=c("diag_var"))
  
  ## Format and output appropriately
  dx_master <- dx_master[order(admit_type,-case_death_rt),]
  dx_master[,case_death_rt:=round(case_death_rt*100,2)]
  
  dx_master <- dx_master[admit_type=="Discharge"]
  dx_master[,c("diag_var","admit_type"):=NULL]
  
  dx_master[,cases:=formatC(cases, format="d", big.mark=',')]
  
  setcolorder(dx_master,c("diag_name","cases","case_deaths","case_death_rt"))
  setnames(dx_master,c("cases","case_deaths","case_death_rt","diag_name"),
           c("Total Cases","Case Deaths","Percent of Cases","Diagnosis"))
  write.csv(dx_master,paste0(data_dir,"/02_case_deaths.csv"),row.names=F)


## Now, output a dataset and test formula only with variables present at admission
dx_admit_vars <- dx_vars[grepl("admit",dx_vars)]
tr_admit_vars <- tr_vars[grepl("admit",tr_vars)]
predict_vars <- c(dx_admit_vars,ss_vars,cv_vars,tr_admit_vars)

pred_formula <- as.formula(paste("death~",paste(predict_vars,collapse="+")))
outcome_vars <- "death"

## Generate a test dataset with only signs and symptoms and 5000 random observations, for easier testing of methods
test_predict_vars <- predict_vars
test_formula <- as.formula(paste("death~",paste(test_predict_vars,collapse="+")))
master_data <- master_data[,.SD,.SDcols=c(test_predict_vars,outcome_vars)]
save.image(file=paste0(data_dir,"/02_prepped_data_admitonly.RData"))


