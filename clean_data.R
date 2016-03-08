#######################################
## Grant Nguyen
## Purpose: Clean input data appropriately

convert_blanks <- function(data,convert_vars) {
  ## Loop over expected-to-be binary prediction variables, convert "blank (9)" to missing, then convert variable to numeric binary
  
}

omit_missing <- function(data,pred_vars,outcome_var) {
  
}

impute_missing <- function(data,pred_vars) {
  ## Apply multiple imputation to a dataset and a set of prediction variables, using Andrew Gelman's MI package
  ## We use this to feed imputed data into the Random Forests analysis, as it requires a complete dataset
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

