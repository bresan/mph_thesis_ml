#####################################################
## Grant Nguyen
## Purpose: Create Functions to Analyze data and output results


## Logistic Regression
run_logistic <- function(data,formula) {
  lr_fit <- glm(formula,data=data,family = binomial(link = "logit"))
  lr_pred <- predict(lr_fit,test_data,type="response")
  
  lr_coefs <- data.frame(lr_fit$coefficients)
  setDT(lr_coefs,keep.rownames=T)
  setnames(lr_coefs,c("rn","lr_fit.coefficients"),c("var_name","beta"))
  
  return(list(lr_fit,lr_pred,lr_coefs))
}

## LASSO -- no significant performance improvement over logistic regression
run_lasso <- function(data) {
  library(glmnet)
  data_new <- copy(data)
  data_test <- model.matrix(test_formula,test_data)
  outcome <- as.double(data_new[,death])-1 # Turn death into a 0/1 with 1 being death
  data_new[,death:=NULL]
  data_new <- model.matrix(~.,data_new)
  lasso_cv <- cv.glmnet(data_new,outcome,family="binomial",type.measure="auc")
  lasso_preds <- predict(lasso_cv,data_test,s="lambda.min",type="response")
  lambda_index <- which(lasso_cv$lambda == lasso_cv$lambda.min)
  lasso_coefs <- lasso_cv$glmnet.fit$beta[,lambda_index]
  lasso_coefs[lasso_coefs==0]
  
  lasso_coefs <- data.frame(lasso_coefs)
  setDT(lasso_coefs,keep.rownames=T)
  setnames(lasso_coefs,c("rn","lasso_coefs"),c("var_name","beta"))
}

## Decision Tree
run_dtree <- function(data,formula) {
  library(rpart)
  ## Create a regression tree using rpart    
  rt_fit <- rpart(formula,data=data.frame(data),control=rpart.control())
  rt_pred <- predict(rt_fit,test_data)
  return(list(rt_fit, rt_pred))
}

## Conditional Inference Tree
run_ctree <- function(data,formula) {
  ## Use party package
  library(party)    
  ct_fit = ctree(formula,data=data.frame(data),controls=ctree_control(maxdepth=3))
  ct_pred = predict(ct_fit,type="prob",newdata=test_data)
  ct_pred <- do.call(rbind,ct_pred) # Convert from a list of lists to a matrix
  return(list(ct_fit,ct_pred))
}

## Parameter selection for CI Tree-- aborted, long runtime and minimal performance gain
run_car_ctree <- function(data,formula) {
  library(caret)
  ct_fit <- train(formula, data=data,method='ctree')
  ct_imp <- varImp(ct_fit)
  ct_pred <- predict(ct_fit,type="prob",newdata=test_data)
  return(list(ct_fit,ct_pred,ct_imp))
}

## Random Forest
run_rf <- function(data,num_trees,formula) {
  library(randomForest)
  rf_fit <- randomForest(formula,data=data.frame(data),ntree=num_trees,replace=T,keep.forest=T,importance=T)
  
  ## Get raw predictive accuracy, and generate predictions for ROC curves
  rf_pred = predict(rf_fit,type="prob",newdata=test_data)
  return(list(rf_fit,rf_pred))
}

## Parallel random forests (on the cluster)
run_par_rf <- function(data,formula) {
  library(doMC)
  library(randomForest)
  registerDoMC(cores=4)
  
  ## Separate out the outcome variable for speed purposes
  data_new <- copy(data)
  outcome <- data_new[,death]
  data_new[,death:=NULL]
  rf_fit <- foreach(ntree=rep(200,4), .combine=combine, .multicombine=TRUE,
                    .packages='randomForest') %dopar% {
                      randomForest(x=data_new,y=outcome,ntree=ntree,replace=T,keep.forest=T,importance=T)
                    }
  #     save(rf_fit,file=paste0(data_dir,"/03_fits/rf_fit_",postfix,".RData"))
  rf_pred = predict(rf_fit,type="prob",newdata=test_data)
  return(list(rf_fit,rf_pred))
}

## Parameter selection for RFs-- aborted, long runtime and minimal performance gain
run_car_rf <- function(data,formula,sample_weights) {
  library(doMC); library(caret)
  registerDoMC(cores=4)
  control <- trainControl(method="cv",number=5)
  tunegrid <- expand.grid(ntree=c(2, 4, 10))
  train_model <- train(formula=formula,data=data.frame(data),method="rf",tunGrid=tunegrid,trControl=control)
  
  rf_pred = predict(rf_fit,type="prob",newdata=test_data)
  return(list(rf_fit,rf_pred))
}

## Gradient Boosting Machines
run_boost <- function(tr_data,te_data) {
  library(xgboost); library(Matrix)
  
  xgb_features <- names(tr_data)[names(tr_data) != "death"]
  sparse_train <- sparse.model.matrix(death~.-1, data=data.frame(tr_data))
  sparse_test <- sparse.model.matrix(death~.-1, data=data.frame(te_data))
  y <- tr_data[,as.numeric(death == "Yes")]
  #     input_data <- xgb.DMatrix(sparse_train)
  #     input_data <- xgb.DMatrix(sparse_train,label=y,weight=as.numeric(y*death_weight)
  
  # This only works with sparse_matrix but not the DMatrix -- not really sure what's going on here
  # See http://stackoverflow.com/questions/37057326/grid-tuning-xgboost-with-missing-data for another case of this
  boost_fit = xgboost(data=sparse_train,label=y,nrounds=200,nfold=10,objective="binary:logistic") 
  importance <- xgb.importance(feature_names = xgb_features, model = boost_fit)
  boost_pred = predict(boost_fit,newdata=sparse_test)
  
  return(list(boost_fit,boost_pred,importance))
}

## Parameter Selection on GBMs -- aborted, too much time with little performance gain
run_car_boost <- function(tr_data,te_data) {
  library(xgboost); library(Matrix); library(caret);library(pROC);library(doMC)
  registerDoMC(cores = 1)
  
  xgb_features <- names(tr_data)[names(tr_data) != "death"]
  
  # Here we use 10-fold cross-validation, repeating twice, and using random search for tuning hyper-parameters.
  fitControl <- trainControl(method = "cv", number = 10, repeats = 2, search = "random",
                             summaryFunction = twoClassSummary,classProbs=T)
  
  # train a xgbTree model using caret::train
  boost_fit <- train(test_formula, data = tr_data, method = "xgbTree", trControl = fitControl,metric = "ROC")
  
  car_preds <- predict(boost_fit,newdata=te_data,type="prob")
  car_preds <- car_preds[,2]
  
  car_imp <- varImp(boost_fit)
  car_imp <- car_imp$importance
  setDT(car_imp, keep.rownames=T)
  setnames(car_imp,c("rn","Overall"),c("var_name","measure"))
  
  return(list(boost_fit,car_preds,car_imp))
}
