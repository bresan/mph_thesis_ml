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

## Bring in method labels (to go from two-letter short labels to table/graph labels)
method_labels <- data.table(fread(paste0(data_dir,"/method_map.csv")))

## Import and analyze all datasets
## ROC Curves
roc_results <- data.table(rbindlist(lapply(postfixes,
                                           function(x) fread(paste0(data_dir,"/03_perf/roc_",x)))))

# AUC
auc_results <- data.table(rbindlist(lapply(postfixes,
                                          function(x) fread(paste0(data_dir,"/03_perf/auc_",x)))))
auc_summary <- auc_results[,list(mean=mean(auc),lower=quantile(auc,.025),upper=quantile(auc,.975)),by=list(pred_method,d_wt,admit)]
auc_summary[,auc_max:=max(mean),by=list(pred_method,admit)]
best_auc <- auc_summary[mean==auc_max]
best_auc <- unique(best_auc,by=c("pred_method","admit"))
best_models <- best_auc[,list(pred_method,d_wt,admit)]

# Accuracy
acc_results <- data.table(rbindlist(lapply(postfixes,
                                          function(x) fread(paste0(data_dir,"/03_perf/acc_",x)))))
acc_results[,accuracy:=as.numeric(accuracy)]
acc_summary <- acc_results[,list(mean=mean(accuracy),lower=quantile(accuracy,.025),upper=quantile(accuracy,.975)),
                           by=list(pred_method,pred_prob,d_wt,admit)]
acc_summary <- merge(acc_summary,best_models,by=c("pred_method","d_wt","admit"))

# Hosmer-Lemeshow
hl_results <- data.table(rbindlist(lapply(postfixes,
                                           function(x) fread(paste0(data_dir,"/03_perf/hl_",x)))))
hl_results <- hl_results[!is.na(stat)]
hl_summary <- hl_results[,list(mean_stat=mean(stat),mean_p=mean(p)),by=list(pred_method,d_wt,admit)]
hl_summary <- merge(hl_summary,best_models,by=c("pred_method","d_wt","admit"))

hl_bin_results <- data.table(rbindlist(lapply(postfixes,
                                              function(x) fread(paste0(data_dir,"/03_perf/hl_bins_",x)))))
hl_bin_results <- merge(hl_bin_results,best_models,by=c("pred_method","d_wt","admit"))

# Variable Importance/Inclusion
# Need to regenerate due to poor rbinding of GB results
var_imp <- data.table(rbindlist(lapply(postfixes,
                                      function(x) fread(paste0(data_dir,"/03_perf/imp_",x)))))
var_imp <- var_imp[!is.na(var_name),]
var_imp[,measure:=as.numeric(measure)]
imp_summary <- var_imp[,list(measure=sum(measure)),by=list(var_name,imp_type,pred_method,d_wt,admit)]
imp_summary <- merge(imp_summary,best_models,by=c("pred_method","d_wt","admit"))

include_vars <- data.table(rbindlist(lapply(postfixes,
                                       function(x) fread(paste0(data_dir,"/03_perf/include_vars_",x)))))
# A value of over 1 for the mean here means that it was included in more than one split on the CTree
include_summary <- include_vars[,list(mean=sum(include)/(max_reps*max_folds)),by=list(var_name,pred_method,d_wt,admit)]
include_summary <- merge(include_summary,best_models,by=c("pred_method","d_wt","admit"))

## Create summary measures of variable importance
imp_summary <- imp_summary[order(pred_method,imp_type,admit,-measure)]
imp_summary <- imp_summary[,rank:=rank(-measure,ties.method="min"),by=list(pred_method,imp_type,admit)]
imp_top_15 <- imp_summary[rank <= 15]

format_vartypes <- function(data) {
  data <- data[substr(var_name,1,2)=="ss",var_type:="ss"]
  data <- data[substr(var_name,1,2)=="dx",var_type:="dx"]
  data <- data[substr(var_name,1,2)=="tr",var_type:="tr"]
  data <- data[substr(var_name,1,2)=="te",var_type:="te"]
  data <- data[substr(var_name,1,2)=="cv",var_type:="cv"]
  return(data)
}

imp_top_15 <- format_vartypes(imp_top_15)
imp_top_15 <- imp_top_15[,combined_varname := sprintf("%.1f %s",rank,var_name)]

## Graph of variables included in the ctree and logistic regression (significance)
incl_test <- copy(include_summary)
incl_test <- incl_test[order(pred_method,admit,d_wt,-mean)]
incl_test <- incl_test[,rank:=rank(-mean),by=list(pred_method,admit)]
incl_test <- incl_test[,min_rank:=min(rank,ties.method="min"),by=list(pred_method,admit)]
incl_top_15 <- incl_test[rank <=15 | rank == min_rank]
incl_top_15 <- incl_top_15[,combined_varname := sprintf("%.1f %s",rank,var_name)]
incl_top_15 <- format_vartypes(incl_top_15)

## For Heatmap of variable importance/inclusion, do some re-coding to make it sensible
## Logistic regression makes dummies, so we have to categorize them appropriately
incl_heat <- copy(incl_top_15)
incl_heat <- incl_heat[grepl("cv_age",var_name),var_name:="cv_age"]
incl_heat <- incl_heat[grepl("ss_airway",var_name),var_name:="ss_airway"]
incl_heat <- incl_heat[grepl("cv_site_id",var_name),var_name:="cv_site_id"]
incl_heat <- incl_heat[grepl("ss_pallor",var_name),var_name:="ss_pallor"]
incl_heat <- incl_heat[grepl("ss_temp_under35p5",var_name),var_name:="ss_temp_under35p5"]
incl_heat <- incl_heat[grepl("te_hb_under7",var_name),var_name:="te_hb_under7"]
incl_heat <- incl_heat[grepl("te_malaria_test",var_name),var_name:="te_malaria_test"]
incl_heat <- incl_heat[grepl("dx_malaria_final",var_name),var_name:="dx_malaria_final"]
incl_heat <- incl_heat[grepl("ss_jaundice",var_name),var_name:="ss_jaundice"]

## Format inclusion and importance datasets similarly
imp_heat <- merge(imp_top_15,method_labels,by="pred_method")
imp_heat[,pred_method:=NULL]
imp_heat <- imp_heat[,model_expanded:=paste0(Method,"_",imp_type)]

incl_heat[pred_method=="lr",rank:=7.5] # All logistic is included in everything
incl_heat <- merge(incl_heat,method_labels,by="pred_method")
incl_heat[,pred_method:=NULL]
setnames(incl_heat,"Method","model_expanded")
heat_data <- rbindlist(list(incl_heat[,list(var_name,model_expanded,admit,rank)],
                            imp_heat[,list(var_name,model_expanded,admit,rank)]),
                       use.names=T)

heat_template <- data.table(expand.grid(var_name=unique(heat_data$var_name),
                                        model_expanded=unique(heat_data$model_expanded),
                                        admit=unique(heat_data$admit)))
heat_template <- merge(heat_template,heat_data,by=c("var_name","model_expanded","admit"),all.x=T)

## Add method labels onto all output datasets
format_methods <- function(data) {
  ## Re-assign variables instead of merging so that the changes will "stick" easily
  for(type in unique(method_labels[,pred_method])) {
    proper_name <- unique(method_labels[pred_method==type,Method])
    data[pred_method==type,pred_method:=proper_name]
  }
  setnames(data,"pred_method","Method")
}

for(d in c("best_auc","auc_summary","acc_summary","auc_results","roc_results",
           "hl_summary","imp_summary","include_summary",
           "imp_top_15","incl_top_15","best_models")) {
  format_methods(get(d))
}

## Output results
save(best_auc,auc_summary,acc_summary,hl_summary,
     imp_summary,include_summary,
     imp_top_15,incl_top_15,heat_template,file=paste0(out_dir,"/results.RData"))


## Save ROC graphs with all 100 draws and median draw highlighted in red
auc_results[,median:=quantile(auc,.5,type=3),by=list(Method,d_wt,admit)] # This chooses a median by defaulting to nearest even order statistic
auc_meds <- auc_results[auc==median,list(fold,rep,d_wt,admit,Method)]
auc_meds <- auc_meds[!duplicated(auc_meds[,list(d_wt,admit,Method)]),]
auc_meds[,median:=1]
roc_results <- merge(roc_results,auc_meds,by=c("d_wt","admit","Method","fold","rep"),all.x=T)
roc_results[is.na(median),median:=0]
roc_results <- merge(roc_results,best_models,by=c("Method","admit","d_wt"))

png(file=paste0(fig_dir,"/roc_admit.png"),width=700,height=500)
plot <- ggplot(NULL,aes(x=fpr,y=tpr, group=interaction(rep,fold))) +
  geom_line(data=roc_results[median == 0,],alpha=.1) +
  geom_line(data=roc_results[median == 1,], color="red") +
  facet_wrap(~ admit+Method, ncol=5) +
  ggtitle(paste0("ROC by dataset (admission-only vs. all) and model, one line per rep/fold combination"))
print(plot)
dev.off()

## Graph just the median ROC curves
png(file=paste0(fig_dir,"/roc_median.png"),width=700,height=500)

plot <- ggplot(data=roc_results[median == 1,],aes(x=fpr,y=tpr,color=Method)) +
geom_line() +
facet_wrap(~admit) +
ggtitle("Median (by AUC) ROC curves of all tested methods \n By dataset (admission-data only or all data) and death weight (5 or 10)") 
print(plot)

dev.off()
