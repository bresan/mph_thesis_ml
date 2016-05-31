#####################################################
## Grant Nguyen
## Purpose: Apply cleaning and data analysis functions to the dataset


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

## Import analysis functions
source(paste0(code_dir,"/analysis_functions.R"))


####################################################
## Import data
load(paste0(data_dir,"/02_prepped_data.RData"))


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
    
    fit <- rpart(formula,data=data.frame(data_new),control=rpart.control(),weights=weight)
    # fit <- rpart(death~.,data=data.frame(data_new))
    summary(fit)
    printcp(fit)
    plotcp(fit)
    plot(fit, uniform=T)
    text(fit,use.n=T,all=T,cex=.8)
  }
  
  run_dtree(data=test_data,formula=test_formula,death_weight=5)


## Results: Interesting results on unconsciousness being very bad, deep breathing yes can be bad if no cough, and pallor can be bad if missing and no deep breathing or missing
  run_dtree3 <- function(data,formula,death_weight=1) {
    ## Use party package
    require(party)
    data_new <- data
    data_new[death=="No",weight:=1]
    data_new[death=="Yes",weight:=death_weight]
    ct = ctree(formula,data=data.frame(data_new),controls=ctree_control(maxdepth=3),weights=c(data_new[,weight]))
    plot(ct,main="Conditional Inference Tree")
    table(predict(ct),data.frame(data_new)$death)
    ct_pred = predict(ct,newdata=data,type="prob")
    ct_pred <- do.call(rbind,ct_pred) # Convert from a list of lists to a matrix
    return(ct,ct_pred)
  }
  ct_results <- run_dtree3(data=test_data,formula=test_formula,death_weight=10)
  # system.time(run_dtree3(data=master_data,formula=pred_formula,death_weight=10))

  ct_preds <- ct_results[2]
  ct_fit <- ct_results[1]


## Random Forests
## Results: 
  run_rf <- function(data,num_trees,formula,sample_weights) {
    require(randomForest)
#     rfImpute(death~.,data.frame(data)) # Trying to impute data...
    rf_fit <- randomForest(formula,data=data.frame(data),ntree=num_trees,keep.forest=T,importance=T)
    
    ## Get raw predictive accuracy, and generate predictions for ROC curves
    test_forest = predict(rf_fit,newdata=data,type="prob")
    table(test_forest,data[,death])
    print(rf_fit)
    return(list(rf_fit,rf_pred))
  }

  rf_results <- run_rf(data=test_data,num_trees=1000,formula=test_formula,sample_weights=5)
  rf_fit <- rf_results[1]
  rf_preds <- rf_results[2]

  ## Plot variable importance and RF fit
  plot(rf_fit,log="y")
  varImpPlot(rf_fit)


  ## Combine predictions from various methods
  preds_combined <- cbind(p1 = rf_preds[,2],p2=ct_pred[,2])

  ## Plot ROC curves
  require(ROCR)
  pred_forest = prediction(preds_combined,test_data[,death])
  perf_forest = performance(pred_forest,"tpr","fpr")
  plot(perf_forest, main="ROC", colorize=T)
  plot(perf_forest,col=3,add=T)
  abline(a=0,b=1)

  ## Calculate AUC
  auc.perf = performance(pred_forest, measure = "auc")
  auc_forest <- auc.perf@y.values
  print(paste0("AUC of Random Forest is ",auc_forest))


####################################################
## Export data


