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
convert_factors <- function(dt) {
  char_cols <- names(dt[,.SD,.SDcols=sapply(dt,is.character)])
  print(length(char_cols))
  if(length(char_cols) > 0 & length(char_cols) != length(names(dt))) {
    fac_dt <- dt[,lapply(.SD,as.factor),.SDcols=char_cols]
    dt[,c(char_cols):=NULL]
    dt <- cbind(dt,fac_dt)
  } else if(length(char_cols)==length(names(dt))) {
    dt <- dt[,lapply(.SD,as.factor),.SDcols=char_cols]
  }
  return(dt)
}

master_data <- convert_factors(master_data) 
test_data <- convert_factors(test_data)

## Check for missing variables in dataset (random forests do not do well with missing observations
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
  } else {
    print(paste0("No variables with missing values present in the dataset"))
  }
}

check_missing(master_data)
check_missing(test_data)

## Create a correlation plot across variables
## Current issue: Most variables are categorical rather than numeric, makes cor function not work

## Currently, this only returns p values for each var, not var-var combinations. Why? Because combinations aren't looped both ways?
# pos_dim <- dim(master_data)[2]
# neg_dim <- (-1 * pos_dim)
#             
# mapply(function(x, y) chisq.test(x, y)$p.value, data.frame(master_data)[,neg_dim], MoreArgs=list(data.frame(master_data)[,pos_dim]))

# This seems to return nulls for everything -- try chisq on 1x1 combos before getting more fancy
# chisq.test(table(master_data[,te_bs_admit],master_data[,dx_admit_50]))
# library(plyr)
# 
# combos <- combn(ncol(Dat),2)
# 
# chisq_results <- adply(combos, 2, function(x) {
#   test <- tryCatch(chisq.test(Dat[, x[1]], Dat[, x[2]]),
#                    error= function(err) {
#                      print(paste0("One of these columns has no variance in obs"))
#                      print(names(Dat[,c(x[1],x[2])))
#                      err_frame <- data.frame(parameter=NA,statistic=NA,p.value=NA)
#                      return(err_frame)
#                     })
#   
#   out <- data.frame("Row" = colnames(Dat)[x[1]]
#                     , "Column" = colnames(Dat[x[2]])
#                     , "Chi.Square" = round(test$statistic,3)
#                     ,  "df"= test$parameter
#                     ,  "p.value" = round(test$p.value, 3)
#   )
#   return(out)
# })  

## Note that this doesn't account for multiple testing (but since this is descriptive, doesn't matter)
## This needs a correlation matrix when what we have is a set of chi squared results
# corrplot.mixed(cor(data.frame(data)), lower="circle", upper="color", 
#                tl.pos="lt", diag="n", order="hclust", hclust.method="complete")


#####################################################
## Output data objects to feed into random forest and logistic regression
save.image(file=paste0(data_dir,"/02_prepped_data.RData"))


