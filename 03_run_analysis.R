#####################################################
## Grant Nguyen
## Purpose: Apply cleaning and data analysis functions to the dataset


#####################################################
## Set filepaths to code and data
if(Sys.info()[1] =="Windows") {
  master_dir <- "H:/Thesis"
  code_dir <- paste0(master_dir,"/mph_thesis_ml")
  
  rep_num <- 1      # The repetition number used for the cross-validation (10 repetitions of 10-fold CV), used as a unique seed for each rep
  fold_num <- 10    # The fold number that we should extract from the 10-fold CV to use as a holdout
  tot_folds <- 10   # The total number of folds that we are running (to make sure we specify the correct value for k in createFolds)
} else if (Sys.info()[1] == "Macintosh") {
  master_dir <- "/Users/Grant/Desktop/Thesis"
  code_dir <- paste0(master_dir,"/code")
  
  rep_num <- 1
  fold_num <- 10
  tot_folds <- 10
} else if (SYs.info()[1] == "Unix") {
  master_dir <- "/Users/Grant/Desktop/Thesis"
  code_dir <- paste0(master_dir,"/code")
  
  rep_num <- commandArgs()[3]
  fold_num <- commandArgs()[4]
  tot_folds <- commandArgs()[5]
}

data_dir <- paste0(master_dir,"/data")


#####################################################
## Set Packages
require(data.table) # For easy data management
require(ggplot2) # For graphing
require(caret) # To create folds for each repetition of k-fold cross-validation

## Import analysis functions
source(paste0(code_dir,"/analysis_functions.R"))

## Set seed for reproducibility, toggled by repetition number
set.seed(rep_num)

####################################################
## Import data
load(paste0(data_dir,"/02_prepped_data.RData"))

## Create test and train datasets
data_indices <- master_data[,as.factor(death)]

## Create holdouts, attempting to balance class distribution within splits
## The indices must be factors in order to be balanced appropriately
holdouts <- createFolds(data_indices,k=tot_folds,list=T,returnTrain=F)[[fold_num]]

train_data <- master_data[-holdouts]
test_data <- master_data[holdouts]

####################################################
## Run analyses, extract pertinent information
## Dealing with unbalanced data http://digitalassets.lib.berkeley.edu/sdtr/ucb/text/666.pdf

## Results: Creates tree, but unclear results at the moment.
  run_dtree <- function(data,formula,death_weight=10) {
    require(rpart)
    ## Create a regression tree using rpart
    data_new <- data
    data_new[death=="No",weight:=1]
    data_new[death=="Yes",weight:=death_weight]
    
    rt_fit <- rpart(formula,data=data.frame(data_new),control=rpart.control(),weights=weight)
#     summary(rt_fit)
#     printcp(rt_fit)
#     plotcp(rt_fit)
#     plot(rt_fit, uniform=T)
#     text(rt_fit,use.n=T,all=T,cex=.8)
    rt_pred <- predict(rt_fit,test_data)
    return(list(rt_fit, rt_pred))
  }
  
  dt_results <- run_dtree(data=train_data,formula=test_formula,death_weight=5)
  dt_fit <- dt_results[1][[1]]
  dt_preds <- dt_results[2][[1]]

## Results: Interesting results on unconsciousness being very bad, deep breathing yes can be bad if no cough, and pallor can be bad if missing and no deep breathing or missing
  run_ctree <- function(data,formula,death_weight=1) {
    ## Use party package
    require(party)
    data_new <- data
    data_new[death=="No",weight:=1]
    data_new[death=="Yes",weight:=death_weight]
    
    ct_fit = ctree(formula,data=data.frame(data_new),controls=ctree_control(maxdepth=3),weights=c(data_new[,weight]))
#     plot(ct_fit,main="Conditional Inference Tree")
    ct_pred = predict(ct_fit,type="prob",newdata=test_data)
    ct_pred <- do.call(rbind,ct_pred) # Convert from a list of lists to a matrix
    return(list(ct_fit,ct_pred))
  }
  ct_results <- run_ctree(data=train_data,formula=test_formula,death_weight=10)

  ct_fit <- ct_results[1][[1]]
  ct_preds <- ct_results[2][[1]]


## Random Forests
  run_rf <- function(data,num_trees,formula,sample_weights) {
    require(randomForest)
#     rfImpute(death~.,data.frame(data)) # Trying to impute data...
    rf_fit <- randomForest(formula,data=data.frame(data),ntree=num_trees,replace=T,keep.forest=T,importance=T,classwt=c(1,sample_weights))
    
    ## Get raw predictive accuracy, and generate predictions for ROC curves
    rf_pred = predict(rf_fit,type="prob",newdata=test_data)
#     print(rf_pred)
#     print(data[,death])
#     table(rf_pred[,2],data[,death])
#     print(rf_fit)
    return(list(rf_fit,rf_pred))
  }

  rf_results <- run_rf(data=train_data,num_trees=500,formula=test_formula,sample_weights=5)
  rf_fit <- rf_results[1][[1]]
  rf_preds <- rf_results[2][[1]]


## Gradient Boosting Machines
  require(xgboost); require(Matrix)

  run_boost <- function(data) {
    sparse_matrix <- sparse.model.matrix(death~.-1, data=data.frame(data))
    y <- data[,as.integer(death)]
    input_data <- xgb.DMatrix(sparse_matrix)
    boost_fit = xgb.train(data=input_data,label=y,nrounds=5,nfold=3)
    print(summary(boost_fit))
    boost_pred = predict(boost_fit,newdata=data[,death],
                        n.trees=5000)
    return(list(boost_fit,boost_pred))
  }
  
  gb_results <- run_boost(data=test_data)
  gb_fit <- gb_results[1][[1]]
  gb_preds <- gb_results[2][[1]]


## 10-fold CV Random Forests and Gradient Boosting Machines
#   require(caret); require(e1071)
#   ctrl = trainControl(method="repeatedcv", number=10, repeats=5)
#   trf = train(test_formula, data=test_data, method="rf", metric="Kappa",
#               trControl=ctrl)
#   
#   system.time(train(test_formula, data=test_data, method="rf", metric="Kappa",
#         trControl=ctrl))
#   tgbm = train(test_formula, data=test_data, method="gbm", metric="Kappa",
#                trControl=ctrl)
# 
#   ## Plot variable importance and RF fit
#   plot(rf_fit,log="y")
#   varImpPlot(rf_fit)

  ## Combine predictions from various methods
#   preds_combined <- cbind(p1 = rf_preds[,2],p2=ct_preds[,2])

  ## Plot ROC curves
  test_data[,death_test:=as.numeric(death)-1] # Factor var is 1 for alive, 2 for dead -- convert to 0 for alive, 1 for dead

  require(ROCR)
  pred_forest = prediction(rf_preds[,2],test_data[,death_test])
  perf_forest = performance(pred_forest,"tpr","fpr")

  pred_ctree = prediction(ct_preds[,2],test_data[,death_test])
  perf_ctree = performance(pred_ctree,"tpr","fpr")

  ## Plot ROC curves of predictions
  plot(perf_forest, main="ROC", col="red")
  plot(perf_ctree, col="blue",add=T)
  abline(a=0,b=1)
  legend("bottomleft", 
         legend = c("Random Forest","Conditional Tree"), 
         lty = 1, cex=.5,
         col = c("red", "blue"))


  ## Calculate AUC
  auc.perf = performance(pred_forest, measure = "auc")
  auc_forest <- auc.perf@y.values
  print(paste0("AUC of Random Forest is ",auc_forest))

  auc.perf = performance(pred_ctree, measure = "auc")
  auc_ctree <- auc.perf@y.values
  print(paste0("AUC of Conditional Tree is ",auc_ctree))


####################################################
## Export data


