#####################################################
## Grant Nguyen
## Purpose: Apply cleaning and data analysis functions to the dataset


#####################################################
## Set filepaths to code and data
if(Sys.info()[1] =="Windows") {
  home_dir <- "H:/Thesis"
  
  rep_num <- 1      # The repetition number used for the cross-validation (10 repetitions of 10-fold CV), used as a unique seed for each rep
  fold_num <- 10    # The fold number that we should extract from the 10-fold CV to use as a holdout
  tot_folds <- 10   # The total number of folds that we are running (to make sure we specify the correct value for k in createFolds)
  death_wt <- 10    # The weights to use on death for the different methods
  admit_type <- "admit_only" # Whether to run analyses on "all" variables or "admit_only" -- those only available at admission 
} else if (Sys.info()[1] == "Darwin") { # Macintosh
  home_dir <- "/Users/Grant/Desktop/Thesis"
  
  rep_num <- 1
  fold_num <- 10
  tot_folds <- 10
  death_wt <- 10
  admit_type <- "admit_only"
} else if (Sys.info()[1] == "Linux") {
  home_dir <- "/homes/gngu/Thesis"
  
  rep_num <- as.numeric(commandArgs()[4])
  fold_num <- as.numeric(commandArgs()[5])
  tot_folds <- as.numeric(commandArgs()[6])
  death_wt <- as.numeric(commandArgs()[7])
  admit_type <- as.character(commandArgs()[8])
  
  print(commandArgs())
}

ifelse(admit_type=="all",
       load(paste0(home_dir,"/data/02_prepped_data.RData")),
       load(paste0(home_dir,"/data/02_prepped_data_admitonly.RData")))
data_dir <- paste0(home_dir,"/data")
fig_dir <- paste0(data_dir,"/03_figures")
code_dir <- paste0(home_dir,"/mph_thesis_ml")

methods <- c("lr","dt","ct","rf","gb") # The two-letter abbreviations for all of the statistical methods

# test_formula <- as.formula("death~.") # Refactor formula to see if computation goes quicker

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

# save(train_data,file=paste0(data_dir,"/test_split_",postfix,".RData"))

####################################################
## Run analyses, extract pertinent information
## Dealing with unbalanced data http://digitalassets.lib.berkeley.edu/sdtr/ucb/text/666.pdf

## Logistic Regression
  run_logistic <- function(data,formula) {
    lr_fit <- glm(formula,data=data,family = binomial(link = "logit"))
    lr_pred <- predict(lr_fit,test_data)
    return(list(lr_fit, lr_pred))
  }
  system.time(lr_results <- run_logistic(data=train_data,formula=test_formula))
  lr_fit <- lr_results[1][[1]]
  lr_preds <- lr_results[2][[1]]

## Logistic Regression with Backwards Selection
## new <- step(lr_fit)
## However, runtime is indeterminate right now (____ hrs)


## Decision Tree
  run_dtree <- function(data,formula,death_weight=10) {
    library(rpart)
    data_new <- copy(data)
    data_new[death=="No",weight:=1]
    data_new[death=="Yes",weight:=death_weight]
    ## Create a regression tree using rpart    
#     rt_fit <- rpart(formula,data=data.frame(data_new),control=rpart.control(),weights=weight,parms=list(split="information", loss=matrix(c(0,death_weight,1,0), byrow=TRUE, nrow=2)))
    rt_fit <- rpart(formula,data=data.frame(data_new),control=rpart.control(),weights=weight)
    rt_pred <- predict(rt_fit,test_data)
    return(list(rt_fit, rt_pred))
  }
  system.time(dt_results <- run_dtree(data=train_data,formula=test_formula,death_weight=death_wt))
  dt_fit <- dt_results[1][[1]]
  dt_preds <- dt_results[2][[1]][,2]


## Conditional Inference Tree
  run_ctree <- function(data,formula,death_weight=1) {
    ## Use party package
    library(party)
    data_new <- copy(data)
    data_new[death=="No",weight:=1]
    data_new[death=="Yes",weight:=death_weight]
    
    ct_fit = ctree(formula,data=data.frame(data_new),controls=ctree_control(maxdepth=3),weights=c(data_new[,weight]))
    ct_pred = predict(ct_fit,type="prob",newdata=test_data)
    ct_pred <- do.call(rbind,ct_pred) # Convert from a list of lists to a matrix
    return(list(ct_fit,ct_pred))
  }
  system.time(ct_results <- run_ctree(data=train_data,formula=test_formula,death_weight=death_wt))

  ct_fit <- ct_results[1][[1]]
  ct_preds <- ct_results[2][[1]][,2]

## Run a random forest 
## Roughly 35 minutes for 100 trees -> ~3 hours for 500 trees
## http://stats.stackexchange.com/questions/37370/random-forest-computing-time-in-r
  run_rf <- function(data,num_trees,formula,sample_weights) {
    library(randomForest)
    rf_fit <- randomForest(formula,data=data.frame(data),ntree=num_trees,replace=T,keep.forest=T,importance=T,classwt=c(1,sample_weights))
    
    ## Get raw predictive accuracy, and generate predictions for ROC curves
    rf_pred = predict(rf_fit,type="prob",newdata=test_data)
    return(list(rf_fit,rf_pred))
  }

  run_par_rf <- function(data,formula,sample_weights) {
    library(doMC)
    library(randomForest)
    registerDoMC(cores=4)
    
    ## Separate out the outcome variable for speed purposes
    data_new <- copy(data)
    outcome <- data_new[,death]
    data_new[,death:=NULL]
    rf_fit <- foreach(ntree=rep(200,4), .combine=combine, .multicombine=TRUE,
                  .packages='randomForest') %dopar% {
                    randomForest(x=data_new,y=outcome,ntree=ntree,replace=T,keep.forest=T,importance=T,classwt=c(1,sample_weights))
                  }
    save(rf_fit,file=paste0(data_dir,"/03_fits/rf_fit_",postfix,".RData"))
    rf_pred = predict(rf_fit,type="prob",newdata=test_data)
    return(list(rf_fit,rf_pred))
  }

  run_car_rf <- function(data,formula,sample_weights) {
    library(doMC); library(caret)
    registerDoMC(cores=4)
    control <- trainControl(method="repeatedcv",number=10,repeats=3,classProbs=T)
    tunegrid <- expand.grid(ntree=c(50, 100, 200))
    train_model <- train(formula=formula,data=data.frame(data),method=customRF,metric=metric,tunGrid=tunegrid,trControl=control)
    
    save(rf_fit,file=paste0(data_dir,"/03_fits/caret_fit_",postfix,".RData"))
    rf_pred = predict(rf_fit,type="prob",newdata=test_data)
    return(list(rf_fit,rf_pred))
  }

if(Sys.info()[1] =="Linux") {
  system.time(rf_results <- run_par_rf(data=train_data,formula=test_formula,sample_weights=death_wt))
} else if(Sys.info()[1] =="Windows")  {
  system.time(rf_results <- run_rf(data=train_data,formula=test_formula,sample_weights=death_wt,num_trees=50))
}
  rf_fit <- rf_results[1][[1]]
  rf_preds <- rf_results[2][[1]][,2]

## Gradient Boosting Machines
## Roughly 20 seconds for 5 rounds and 3 folds
  run_boost <- function(tr_data,te_data,death_weight) {
    library(xgboost); library(Matrix)
#     library(Ckmeans.1d.dp) ## Needed for xgb.plot.importance
    library(DiagrammeR) ## Needed for xgb.plot.tree
    
    xgb_features <- names(tr_data)[names(tr_data) != "death"]
    sparse_train <- sparse.model.matrix(death~.-1, data=data.frame(tr_data))
    sparse_test <- sparse.model.matrix(death~.-1, data=data.frame(te_data))
    y <- tr_data[,as.numeric(death == "Yes")]
#     input_data <- xgb.DMatrix(sparse_train)
#     input_data <- xgb.DMatrix(sparse_train,label=y,weight=as.numeric(y*death_weight)
    
    # This only works with sparse_matrix but not the DMatrix -- not really sure what's going on here
    # See http://stackoverflow.com/questions/37057326/grid-tuning-xgboost-with-missing-data for another case of this
    boost_fit = xgboost(data=sparse_train,label=y,nrounds=200,nfold=10,scale_pos_weight=death_weight,objective="binary:logistic") 
    
    print(summary(boost_fit))
    importance <- xgb.importance(feature_names = xgb_features, model = boost_fit)
    print(importance)
    
    boost_pred = predict(boost_fit,newdata=sparse_test)
    return(list(boost_fit,boost_pred,importance))
  }
  
  system.time(gb_results <- run_boost(tr_data=train_data,te_data=test_data,death_weight=10))
  gb_fit <- gb_results[1][[1]]
  gb_preds <- gb_results[2][[1]]
  gb_imp <- gb_results[3][[1]]
 

####################################################
## Plot results
  ## Calculate ROC curves
  test_data[,death_test:=as.numeric(death)-1] # Factor var is 1 for alive, 2 for dead -- convert to 0 for alive, 1 for dead
  
  library(ROCR)

  ## First, save the ROC curves for use in plotting compiled 
  extract_roc <- function(pred_type) {
    pred <- prediction(get(paste0(pred_type,"_preds")),test_data[,death_test])
    perf <- performance(pred,"tpr","fpr")
    roc_results <- data.table(fpr=unlist(perf@x.values),tpr=unlist(perf@y.values),model_type=pred_type)
    return(roc_results)
  }
  roc_results <- rbindlist(lapply(methods,extract_roc))
  roc_results <- add_loopvars(roc_results)
  write.csv(roc_results,paste0(data_dir,"/03_perf/roc_",postfix,".csv"),row.names=F) 

  ## Plot ROC curves of predictions
  pdf(paste0(fig_dir,"/results_",postfix,".pdf"))
  ggplot(data=roc_results,aes(x=fpr,y=tpr,color=model_type)) + 
    geom_line() + 
    scale_x_continuous(breaks=seq(0,1,.2)) + 
    scale_y_continuous(breaks=seq(0,1,.2)) +
    ggtitle("ROC Curve for selected methods")

  ## Plot decision tree results
  plotcp(dt_fit)
#   plot(dt_fit, uniform=T)
#   text(dt_fit,use.n=T,all=T,cex=.8)
  
  ## Plot conditional inference tree results
  plot(ct_fit,main="Conditional Inference Tree")
  
  ## Plot random forest results
  varImpPlot(rf_fit)
  
  ## Plot xgboost variable importance (top 20)
  gb_imp <- gb_imp[order(-gb_imp$Gain),]
  
#   print(xgb.plot.importance(gb_imp[1:20,]))
  dev.off()
  
  ## Save xgboost ensemble tree
  ## Note: Output is stored in html format, so it can only be run locally, and exported via RStudio viewer
  library(stringr)
  library(DiagrammeR)

  print(home_dir)
  print(code_dir)

  source(paste0(code_dir,"/xgb_funcs.R")) # Import edited xgboost.multi.tree graphing function

  xg_tree <- xgb.plot.multi.trees(model = gb_fit, features.keep = 3) ## Need to add feature names
  save(xg_tree,file=paste0(fig_dir,"/gb_",postfix,".RData"))
  
  ## Calculate AUC
  calc_auc <- function(pred_method) {
    library(ROCR)
    auc_perf <- performance(get(paste0("pred_",pred_method)),measure="auc")
    auc <- unlist(auc_perf@y.values)
    auc_dt <- data.table(pred_method,auc)
    return(auc_dt)
  }

  auc_results <- rbindlist(lapply(methods,calc_auc))

  ## Calculate Accuracy at various cutoffs
  ## Cutoffs are the probability of event (death) predicted by each method
  calc_accuracy <- function(pred_method) {
    library(ROCR)
    get_accuracy <- function(x) {
      acc_perf@y.values[[1]][max(which(acc_perf@x.values[[1]] >= x))]
    }
    
    acc_perf <- performance(get(paste0("pred_",pred_method)),measure="acc")

    ## This gives the accuracy of the method at different cutoffs of predicted probability
    test_probs <- c(seq(.1,.5,.1),.75,.9)
    results <- unlist(do.call(rbind,lapply(test_probs,get_accuracy)))
    acc_dt <- data.table(cbind(
      pred_type=rep(pred_method,length(test_probs)),
      pred_prob=test_probs,
      results))
    setnames(acc_dt,"V3","accuracy") # For some reason, renaming accuracy within cbind doesn't work
    return(acc_dt)
  }

  acc_results <- rbindlist(lapply(methods,calc_accuracy))

  ## Calculate Hosmer-Lemeshow statistic
  ## Create function that takes in pred_method and returns a data.table with the statistic and p_value
  calc_hl <- function(pred_method) {
    library(ResourceSelection)
    preds <- get(paste0(pred_method,"_preds"))
    if(length(unique(preds)) != 1) {
      hl_results <- hoslem.test(test_data[,death_test],preds,g=15)
      results <- data.table(method=pred_method,stat=hl_results$statistic,p=hl_results$p.value)
    } else {
      results <- data.table(method=pred_method,stat=NA,p=0)
    }
    return(results)  
  }

  hl_compiled <- rbindlist(lapply(methods,calc_hl))

  calc_hl_bins <- function(pred_method) {
    library(ResourceSelection)
    preds <- get(paste0(pred_method,"_preds"))
    if(length(unique(preds)) != 1) {
      hl_results <- hoslem.test(test_data[,death_test],preds,g=15)
      bin_results <- data.frame(cbind(hl_results$observed,hl_results$expected))
      bin_results$method <- paste0(pred_method)
      setDT(bin_results,keep.rownames=T)
      setnames(bin_results,"rn","prob_range")
    } else {
      bin_results <- data.table(prob_range="0,1",method=paste0(pred_method),y0=NA,y1=NA,yhat0=NA,yhat1=NA)
    }
    return(bin_results)
  }
  hl_bins <- rbindlist(lapply(methods,calc_hl_bins))


####################################################
## Export data
## Check size of all objects
  for (thing in ls()) { message(thing); print(object.size(get(thing)), units='auto') }

## Export model fits
#   save(dt_fit,ct_fit,rf_fit,gb_fit,
#        file=paste0(data_dir,"/03_fits/fit_",postfix,".RData")
#   )

## Export variables included in trees and variable importances (RF and GB)
## dt_fit, ct_fit, rf_fit, gb_fit

  var_list <- c()
  traverse <- function(treenode){
    if(treenode$terminal){
      bas=paste("Current node is terminal node with",treenode$nodeID,'prediction',treenode$prediction)
      print(bas)
      return(0)
    } else {
      bas=paste("Current node",treenode$nodeID,"Split var. ID:",treenode$psplit$variableName,"split value:",treenode$psplit$splitpoint,'prediction',treenode$prediction)
      print(bas)
      var_list <<- c(var_list,treenode$psplit$variableName) ## Edit the global var_list variable (can't do it inside and return var_list because of recursion)
    }
    traverse(treenode$left)
    traverse(treenode$right)
  }

  traverse(ct_fit@tree)
  ct_list <- data.table(var_name=var_list,include=1,method="ct")

  lr_list <- coef(summary(lr_fit))[,4]
  lr_list <- data.frame(as.list(lr_list))
  ## Reshape long the lr_list here
  library(reshape2)
  lr_list <- melt(lr_list)
  lr_list <- lr_list[lr_list$value < .05,] 
  lr_list <- data.table(var_name=as.character(lr_list$variable),include=1,method="lr")
  
  include_list <- rbindlist(list(ct_list,lr_list),use.names=T)
  include_list <- add_loopvars(include_list)


  ## Data.table with model_type, imp_type (gini or other), measure
  pull_imp <- function(pred_type) {
    if(grepl("dt",pred_type) & !is.null(get(paste0(pred_type,"_fit"))$variable.importance)) {
      imp <- data.frame(measure=get(paste0(pred_type,"_fit"))$variable.importance)
      setDT(imp,keep.rownames=T)
      setnames(imp,c("rn"),c("var_name"))
      
      imp[,imp_type:="accuracy"]
    } else if(grepl("dt",pred_type) & is.null(get(paste0(pred_type,"_fit"))$variable.importance)) {
      imp <- data.table(var_name=NA,imp_type=NA,measure=NA)
    } else if(grepl("rf",pred_type)) {
      imp <- data.frame(get(paste0(pred_type,"_fit"))$importance)
      setDT(imp, keep.rownames=T)
      setnames(imp,c("rn","MeanDecreaseAccuracy","MeanDecreaseGini"),c("var_name","accuracy","gini"))
      imp[,c("Yes","No"):=NULL]
      imp <- melt(imp,id.vars="var_name",variable.name="imp_type",value.name="measure",variable.factor=F)
    } else if(grepl("gb",pred_type)) {
      imp <- get(paste0(pred_type,"_imp"))[,list(Feature,Gain)]
      setnames(imp,c("Feature","Gain"),c("var_name","measure"))
      imp[,imp_type:="accuracy"]
    }
    imp[,model_type:=pred_type]
    return(imp)
  }
  imp_methods <- methods[!methods %in% c("ct","lr")]
  importances <- rbindlist(lapply(imp_methods,pull_imp),use.names=T)
  importances <- add_loopvars(importances)
  
  write.csv(include_list,paste0(data_dir,"/03_perf/include_vars_",postfix,".csv"),row.names=F)
  write.csv(importances,paste0(data_dir,"/03_perf/imp_",postfix,".csv"),row.names=F)

## Export csv for AUC, accuracy, and hosmer-lemeshow, along with fold# and rep#
  auc_results <- add_loopvars(auc_results)
  write.csv(auc_results,paste0(data_dir,"/03_perf/auc_",postfix,".csv"),row.names=F)

  acc_results <- add_loopvars(acc_results)
  write.csv(acc_results,paste0(data_dir,"/03_perf/acc_",postfix,".csv"),row.names=F)

  hl_compiled <- add_loopvars(hl_results)
  write.csv(hl_compiled,paste0(data_dir,"/03_perf/hl_",postfix,".csv"),row.names=F)
    
  hl_bins <- add_loopvars(hl_bins)
  write.csv(hl_bins,paste0(data_dir,"/03_perf/hl_bins_",postfix,".csv"),row.names=F)


