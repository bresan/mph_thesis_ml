## Grant Nguyen
## Combine results from parallelized runs of regression/tree/forests
## We want to gather the following metrics for analysis: AUC, Accuracy, Hosmer-Lemeshow, Variable Importance/Inclusion

if(Sys.info()[1] =="Windows") {
  home_dir <- "H:/Thesis"
} else if (Sys.info()[1] == "Linux") {
  home_dir <- "/homes/gngu/Thesis"
}

## Import libraries
library(data.table)

## Setup filepaths
code_dir <- paste0(home_dir,"/mph_thesis_ml")
data_dir <- paste0(home_dir,"/data")
out_dir <- paste0(home_dir,"/results")

## Identify max reps and folds to bring in
max_reps <- 10
max_folds <- 10 ## This must be over 1 otherwise everything will be in the test dataset
death_wts <- c(5,10) # How much to weight the outcome of death in the 
f_vars <- expand.grid(c(1:max_reps),c(1:max_folds),death_wts)

postfixes <- paste0("",f_vars$Var1,"_",f_vars$Var2,"_",f_vars$Var3,".csv")

## Import and analyze all datasets
## ROC Curves
roc_results <- data.table(rbindlist(lapply(postfixes,
                                           function(x) fread(paste0(data_dir,"/03_perf/roc_",x)))))

# AUC
auc_results <- data.table(rbindlist(lapply(postfixes,
                                          function(x) fread(paste0(data_dir,"/03_perf/auc_",x)))))
auc_summary <- auc_results[,list(mean=mean(auc),lower=quantile(auc,.025),upper=quantile(auc,.975)),by=list(pred_method,d_wt)]

# Accuracy
acc_results <- data.table(rbindlist(lapply(postfixes,
                                          function(x) fread(paste0(data_dir,"/03_perf/acc_",x)))))
acc_results[,accuracy:=as.numeric(accuracy)]
acc_summary <- acc_results[,list(mean=mean(accuracy),lower=quantile(accuracy,.025),upper=quantile(accuracy,.975)),
                           by=list(pred_type,pred_prob,d_wt)]

# Hosmer-Lemeshow
hl_results <- data.table(rbindlist(lapply(postfixes,
                                           function(x) fread(paste0(data_dir,"/03_perf/hl_",x)))))
hl_results <- hl_results[!is.na(stat)]
hl_summary <- hl_results[,list(mean_stat=mean(stat),mean_p=mean(p)),by=list(method,d_wt)]

hl_bin_results <- data.table(rbindlist(lapply(postfixes,
                                              function(x) fread(paste0(data_dir,"/03_perf/hl_bins_",x)))))

# Variable Importance/Inclusion
# Need to regenerate due to poor rbinding of GB results
var_imp <- data.table(rbindlist(lapply(postfixes,
                                      function(x) fread(paste0(data_dir,"/03_perf/imp_",x)))))
var_imp <- var_imp[!is.na(var_name),]
var_imp[,measure:=as.numeric(measure)]
imp_summary <- var_imp[,list(measure=sum(measure)),by=list(var_name,imp_type,model_type,d_wt)]

include_vars <- data.table(rbindlist(lapply(postfixes,
                                       function(x) fread(paste0(data_dir,"/03_perf/include_vars_",x)))))
# A value of over 1 for the mean here means that it was included in more than one split on the CTree
ct_summary <- ct_incl[,list(mean=sum(include)/(max_reps*max_folds)),by=list(var_name,method,d_wt)]

## Output results
save(list(auc_summary,acc_summary,hl_summary,imp_summary,ct_summary),paste0(out_dir,"/results.RData"))

# ## Output compiled results and summary metrics
# write.csv(compiled_results,paste0(out_dir,"/compiled_results.csv"),row.names=F)
# write.csv(summary_results,paste0(out_dir,"/summary_results.csv"),row.names=F)
# 
