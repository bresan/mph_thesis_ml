## Grant Nguyen
## Combine results from parallelized runs of regression/tree/forests
## We want to gather the following metrics for analysis: AUC, Accuracy, Hosmer-Lemeshow, Variable Importance/Inclusion

if(Sys.info()[1] =="Windows") {
  home_dir <- "H:/Thesis"
} else if (Sys.info()[1] == "Linux") {
  home_dir <- "/homes/gngu/Thesis"
}

## Import libraries
library(data.table); library(ggplot2)

## Setup filepaths
code_dir <- paste0(home_dir,"/mph_thesis_ml")
data_dir <- paste0(home_dir,"/data")
out_dir <- paste0(home_dir,"/results")
fig_dir <- paste0(home_dir,"/graphs")

## Identify max reps and folds to bring in
max_reps <- 10
max_folds <- 10 ## This must be over 1 otherwise everything will be in the test dataset
death_wts <- c(5,10) # How much to weight the outcome of death
admit_types <- c("all","admit_only")
f_vars <- expand.grid(c(1:max_reps),c(1:max_folds),death_wts,admit_types)

postfixes <- paste0("",f_vars$Var1,"_",f_vars$Var2,"_",f_vars$Var3,"_",f_vars$Var4,".csv")

## Import and analyze all datasets
## ROC Curves
roc_results <- data.table(rbindlist(lapply(postfixes,
                                           function(x) fread(paste0(data_dir,"/03_perf/roc_",x)))))

# AUC
auc_results <- data.table(rbindlist(lapply(postfixes,
                                          function(x) fread(paste0(data_dir,"/03_perf/auc_",x)))))
auc_summary <- auc_results[,list(mean=mean(auc),lower=quantile(auc,.025),upper=quantile(auc,.975)),by=list(pred_method,d_wt,admit)]

# Accuracy
acc_results <- data.table(rbindlist(lapply(postfixes,
                                          function(x) fread(paste0(data_dir,"/03_perf/acc_",x)))))
acc_results[,accuracy:=as.numeric(accuracy)]
acc_summary <- acc_results[,list(mean=mean(accuracy),lower=quantile(accuracy,.025),upper=quantile(accuracy,.975)),
                           by=list(pred_type,pred_prob,d_wt,admit)]

# Hosmer-Lemeshow
hl_results <- data.table(rbindlist(lapply(postfixes,
                                           function(x) fread(paste0(data_dir,"/03_perf/hl_",x)))))
hl_results <- hl_results[!is.na(stat)]
hl_summary <- hl_results[,list(mean_stat=mean(stat),mean_p=mean(p)),by=list(method,d_wt,admit)]

hl_bin_results <- data.table(rbindlist(lapply(postfixes,
                                              function(x) fread(paste0(data_dir,"/03_perf/hl_bins_",x)))))

# Variable Importance/Inclusion
# Need to regenerate due to poor rbinding of GB results
var_imp <- data.table(rbindlist(lapply(postfixes,
                                      function(x) fread(paste0(data_dir,"/03_perf/imp_",x)))))
var_imp <- var_imp[!is.na(var_name),]
var_imp[,measure:=as.numeric(measure)]
imp_summary <- var_imp[,list(measure=sum(measure)),by=list(var_name,imp_type,model_type,d_wt,admit)]

include_vars <- data.table(rbindlist(lapply(postfixes,
                                       function(x) fread(paste0(data_dir,"/03_perf/include_vars_",x)))))
# A value of over 1 for the mean here means that it was included in more than one split on the CTree
include_summary <- include_vars[,list(mean=sum(include)/(max_reps*max_folds)),by=list(var_name,method,d_wt,admit)]

## Output results
save(auc_summary,acc_summary,hl_summary,imp_summary,include_summary,file=paste0(out_dir,"/results.RData"))


## Save ROC graphs with all 100 draws and median draw highlighted in red
auc_results[,median:=quantile(auc,.5,type=3),by=list(pred_method,d_wt,admit)] # This chooses a median by defaulting to nearest even order statistic
auc_meds <- auc_results[auc==median,list(fold,rep,d_wt,admit,pred_method)]
auc_meds <- auc_meds[!duplicated(auc_meds[,list(d_wt,admit,pred_method)]),]
auc_meds[,median:=1]
setnames(auc_meds,"pred_method","model_type")
roc_results <- merge(roc_results,auc_meds,by=c("d_wt","admit","model_type","fold","rep"),all.x=T)
roc_results[is.na(median),median:=0]

for(weight in c(5,10)) {
  png(file=paste0(fig_dir,"/roc_",weight,".png"),width=700,height=500)
  plot <- ggplot(NULL,aes(x=fpr,y=tpr, group=interaction(rep,fold))) +
    geom_line(data=roc_results[d_wt==weight & median == 0,],alpha=.1) +
    geom_line(data=roc_results[d_wt==weight & median == 1,], color="red") +
    facet_wrap(~ admit+model_type, ncol=5) +
    ggtitle(paste0("ROC for death weight ",weight," by dataset (admission-only vs. all) and model, one line per rep/fold combination"))
  print(plot)
  dev.off()
}

## Graph just the median ROC curves
png(file=paste0(fig_dir,"/roc_combined.png"),width=700,height=500)

plot <- ggplot(data=roc_results[median == 1,],aes(x=fpr,y=tpr,color=model_type)) +
geom_line() +
facet_wrap(~admit+d_wt) +
ggtitle("Median (by AUC) ROC curves of all tested methods \n By dataset (admission-data only or all data) and death weight (5 or 10)") 
print(plot)

dev.off()
