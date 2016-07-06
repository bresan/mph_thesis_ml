#####################################################
## Grant Nguyen
## Purpose: Apply cleaning and data analysis functions to the dataset


#####################################################
## Set filepaths to code and data

rep_num <- 1      # The repetition number used for the cross-validation (10 repetitions of 10-fold CV), used as a unique seed for each rep
fold_num <- 10    # The fold number that we should extract from the 10-fold CV to use as a holdout
tot_folds <- 10   # The total number of folds that we are running (to make sure we specify the correct value for k in createFolds)
death_wt <- 10    # The weights to use on death for the different methods
admit_type <- "admit_only" # Whether to run analyses on "all" variables or "admit_only" -- those only available at admission 

home_dir <- "/homes/gngu/Thesis"
 
ifelse(admit_type=="all",
       load(paste0(home_dir,"/data/02_prepped_data.RData")),
       load(paste0(home_dir,"/data/02_prepped_data_admitonly.RData")))
data_dir <- paste0(home_dir,"/data")
fig_dir <- paste0(data_dir,"/03_figures")
code_dir <- paste0(home_dir,"/mph_thesis_ml")

methods <- c("lr","dt","ct","rf","gb") # The two-letter abbreviations for all of the statistical methods

## Create a common post-fix for standard saving of files with post-fixes by rep/fold/weight combinations
postfix <- paste0(rep_num,"_",fold_num,"_",death_wt,"_",admit_type)

## Add a new R libraries location containing ROCR, xgboost, DiagrammeR, ResourceSelection, Ckmeans.1d.dp, and party packages (not installed on routine cluster)
.libPaths(new = c(.libPaths(),paste0(home_dir,"/../r_libraries")))

#####################################################
## Set Packages
library(data.table) # For easy data management
library(ggplot2) # For graphing
library(caret) # To create folds for each repetition of k-fold cross-validation
library(reshape2) # Standard reshaping requirements

## Import analysis functions
source(paste0(code_dir,"/analysis_functions.R"))

## Set seed for reproducibility, toggled by repetition number
## Keep the same rep/fold splits across death_wt and admission loops
set.seed(paste0(rep_num,"99",fold_num,"99"))

## Create function to easily add id variables for each loop (used to format output datasets)
add_loopvars <- function(data) {
  data[,fold:=fold_num]
  data[,rep:=rep_num]
  data[,d_wt:=death_wt]
  data[,admit:=admit_type]
  return(data)
}

####################################################
## Format data

## Create test and train datasets
data_indices <- master_data[,as.factor(death)]

## Create holdouts, attempting to balance class distribution within splits
## The indices must be factors in order to be balanced appropriately
holdouts <- createFolds(data_indices,k=tot_folds,list=T,returnTrain=F)[[fold_num]]

train_data <- master_data[-holdouts]
test_data <- master_data[holdouts]

## First, resample with replacement to up-weight deaths by the factor specified
if(death_wt != 1) {
  death_data <- train_data[death=="Yes",]
  boot_indic <- sample(1:nrow(death_data), (nrow(death_data) * (death_wt-1)), replace=T)
  boot_data <- death_data[boot_indic,]
  train_data <- rbindlist(list(train_data,boot_data),use.names=T)
}


##########################
## Define functions
run_caret <- function(data,formula,method) {
  library(caret);library(pROC)
  if(method == "ctree") sel_method <- "ctree"
  if(method == "xgboost") sel_method <- "xgbTree"
  if(method == "rpart") sel_method <- "rpart"
  if(method == "rand_forest") sel_method <- "rf"
  
  # Here we use 10-fold cross-validation, repeating twice, and using random search for tuning hyper-parameters.
  fitControl <- trainControl(method = "cv", number = 10, repeats = 2, search = "random",
                             summaryFunction = twoClassSummary,classProbs=T)
  
  ct_fit <- train(formula, data=data,method=sel_method, trControl = fitControl,metric = "ROC")
  ct_imp <- varImp(ct_fit)
  ct_pred <- predict(ct_fit,type="prob",newdata=test_data)
  return(list(ct_fit,ct_pred,ct_imp))
}

############################
## Run analyses
system.time(test_ctree <- run_caret(data=train_data,formula=test_formula,method="ctree"))
system.time(test_rpart <- run_caret(data=train_data,formula=test_formula,method="rpart"))
system.time(test_boost <- run_caret(data=train_data,formula=test_formula,method="xgboost"))
system.time(test_rf <- run_caret(data=train_data,formula=test_formula,method="rand_forest"))

save.image("/homes/gngu/test_results.RData")

