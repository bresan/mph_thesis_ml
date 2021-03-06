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
death_wts <- c(1,5,10,20,30) # How much to weight the outcome of death
admit_types <- c("all","admit_only")
f_vars <- expand.grid(c(1:max_reps),c(1:max_folds),death_wts,admit_types)

postfixes <- paste0("",f_vars$Var1,"_",f_vars$Var2,"_",f_vars$Var3,"_",f_vars$Var4,".csv")

## Bring in method labels (to go from two-letter short labels to table/graph labels)
method_labels <- data.table(fread(paste0(data_dir,"/method_map.csv")))


####################################################################################################
## Import and analyze all datasets
## ROC Curves
roc_results <- data.table(rbindlist(lapply(postfixes,
                                           function(x) fread(paste0(data_dir,"/03_perf/auc/roc_",x)))))

# AUC
auc_results <- data.table(rbindlist(lapply(postfixes,
                                          function(x) fread(paste0(data_dir,"/03_perf/auc/auc_",x)))))
auc_summary <- auc_results[,list(mean=mean(auc),lower=quantile(auc,.025),upper=quantile(auc,.975)),by=list(pred_method,d_wt,admit)]
auc_summary[,auc_max:=max(mean),by=list(pred_method,admit)]
best_auc <- auc_summary[mean==auc_max]
best_auc <- unique(best_auc,by=c("pred_method","admit"))
best_auc[,format_auc := paste0(round(mean,2)," (",paste0(round(lower,2))," - ",paste0(round(upper,2)),")")]

best_models <- best_auc[,list(pred_method,d_wt,admit)] # Save best models map to merge with other datasets

admit_auc <- best_auc[admit=="admit_only",list(pred_method,d_wt,format_auc)]
setnames(admit_auc,c("d_wt","format_auc"),c("Death Wt Admit Only","AUC Admit Only"))
all_auc <- best_auc[admit=="all",list(pred_method,d_wt,format_auc,mean)]
all_auc[,sort_order:=rank(-mean,ties.method="min")]
all_auc[,mean:=NULL]
setnames(all_auc,c("d_wt","format_auc"),c("Death Wt All Vars","AUC All Vars"))
best_auc <- merge(admit_auc,all_auc,by=c("pred_method"))
best_auc <- best_auc[order(sort_order),]
best_auc[,sort_order:=NULL]

# Accuracy
acc_results <- data.table(rbindlist(lapply(postfixes,
                                          function(x) fread(paste0(data_dir,"/03_perf/acc/acc_",x)))))
acc_results[,accuracy:=as.numeric(accuracy)]
acc_summary <- acc_results[,list(mean=mean(accuracy),lower=quantile(accuracy,.025),upper=quantile(accuracy,.975)),
                           by=list(pred_method,pred_prob,d_wt,admit)]
acc_summary <- merge(acc_summary,best_models,by=c("pred_method","d_wt","admit"))

acc_summary[,format_acc := paste0(round(mean,2)," (",paste0(round(lower,2))," - ",paste0(round(upper,2)),")")]
admit_acc <- acc_summary[admit=="admit_only",list(pred_method,pred_prob,format_acc)]
setnames(admit_acc,c("format_acc"),c("Accuracy Admit Only"))
all_acc <- acc_summary[admit=="all",list(pred_method,pred_prob,format_acc)]
setnames(all_acc,c("format_acc"),c("Accuracy All Variables"))
acc_summary <- merge(admit_acc,all_acc,by=c("pred_method","pred_prob"))
acc_summary <- acc_summary[order(pred_method,pred_prob),]

# Hosmer-Lemeshow
hl_results <- data.table(rbindlist(lapply(postfixes,
                                           function(x) fread(paste0(data_dir,"/03_perf/hl/hl_",x)))))
hl_results <- hl_results[!is.na(stat)]
hl_summary <- hl_results[,list(mean_stat=mean(stat),mean_p=mean(p)),by=list(pred_method,d_wt,admit)]
hl_summary <- merge(hl_summary,best_models,by=c("pred_method","d_wt","admit"))
hl_summary[,format_hl := paste0(round(mean_stat,2)," (",paste0(round(mean_p,2)),")")]
admit_hl <- hl_summary[admit=="admit_only",list(pred_method,format_hl)]
setnames(admit_hl,"format_hl","Admit Only Mean Stat (mean p-val)")
all_hl <- hl_summary[admit=="all",list(pred_method,format_hl)]
setnames(all_hl,"format_hl","All Variables Mean Stat (mean p-val)")
hl_summary <- merge(admit_hl,all_hl,by="pred_method")
hl_summary <- hl_summary[order(pred_method),]

hl_bin_results <- data.table(rbindlist(lapply(postfixes,
                                              function(x) fread(paste0(data_dir,"/03_perf/hl/hl_bins_",x)))))
hl_bin_results <- merge(hl_bin_results,best_models,by=c("pred_method","d_wt","admit"))

# Variable Importance
var_imp <- data.table(rbindlist(lapply(postfixes,
                                      function(x) fread(paste0(data_dir,"/03_perf/var_imp/imp_",x)))))
var_imp <- var_imp[!is.na(var_name),]
var_imp[,measure:=as.numeric(measure)]
imp_summary <- var_imp[,list(measure=sum(measure)),by=list(var_name,imp_type,pred_method,d_wt,admit)]
imp_summary <- merge(imp_summary,best_models,by=c("pred_method","d_wt","admit"))

# Logistic Regression betas
lr_betas <- data.table(rbindlist(lapply(postfixes,
                                        function(x) fread(paste0(data_dir,"/03_perf/var_imp/lr_",x)))))

# Logistic regression inclusion
include_vars <- data.table(rbindlist(lapply(postfixes,
                                       function(x) fread(paste0(data_dir,"/03_perf/var_imp/include_vars_",x)))))
# A value of over 1 for the mean here means that it was included in more than one split on the CTree
include_summary <- include_vars[,list(mean=sum(include)/(max_reps*max_folds)),by=list(var_name,pred_method,d_wt,admit)]
include_summary <- merge(include_summary,best_models,by=c("pred_method","d_wt","admit"))
include_summary[,var_name:=gsub("."," ",var_name,fixed=T)]

####################################################################################################
## Create summary measures of variable importance
imp_summary <- imp_summary[order(pred_method,imp_type,admit,-measure)]
imp_summary <- imp_summary[,rank:=rank(-measure,ties.method="min"),by=list(pred_method,imp_type,admit)]
imp_top_15 <- imp_summary[rank <= 15]

format_vartypes <- function(data) {
  data <- data[substr(var_name,1,2)=="ss",var_type:="Symptoms and Signs"]
  data <- data[substr(var_name,1,2)=="dx",var_type:="Diagnosis"]
  data <- data[substr(var_name,1,2)=="tr",var_type:="Treatment"]
  data <- data[substr(var_name,1,2)=="te",var_type:="Testing"]
  data <- data[substr(var_name,1,2)=="cv",var_type:="Covariate"]
  data[,var_name:=substring(var_name,4)]
  data[,var_name:=gsub("Missing","Miss",var_name)]
  return(data)
}

imp_top_15 <- format_vartypes(imp_top_15)

## Graph of variables included in the ctree and logistic regression (significance then betas)
# Collapse lr_test to means of the betas
lr_test <- lr_betas[,list(mean_beta=mean(beta)),by=list(d_wt,admit,var_name)]

# First find the top-15 LR by significance, then by betas if there are >15 100% sig vars
lr_test <- merge(lr_test,include_summary[pred_method=="lr"],by=c("d_wt","var_name","admit"))
lr_test <- lr_test[order(admit,d_wt,-mean,-mean_beta)]
lr_test[,rank:=rank(-mean,ties.method="min"),by=list(admit)]
lr_test <- lr_test[rank<=15,]
lr_test[,abs_beta:=abs(mean_beta)]
lr_test[,rank:=rank(-abs_beta,ties.method="min"),by=list(admit)]
lr_test <- lr_test[rank<=15,list(d_wt,pred_method,var_name,admit,rank,mean_beta)]
setnames(lr_test,"mean_beta","mean")

incl_test <- copy(include_summary[pred_method=="ct",])
incl_test <- incl_test[order(pred_method,admit,d_wt,-mean)]
incl_test[,rank:=rank(-mean,ties.method="min"),by=list(pred_method,admit)]
incl_top_15 <- incl_test[rank <=15]

incl_top_15 <- rbindlist(list(incl_top_15,lr_test),use.names=T)
incl_top_15 <- format_vartypes(incl_top_15)

## Format variable names of predictors
dx_map <- fread(paste0(data_dir,"/diagnosis_map.csv"))
dx_grid <- data.table(expand.grid(diag_code=unique(dx_map$diag_code),prefix=c("dx_",""),admit=c("admit","final","misdiag")))
dx_grid <- merge(dx_grid,dx_map,by="diag_code")
dx_grid[,var_name:=paste0(prefix,admit,"_",diag_code)]
dx_grid[,short_name:=paste0(prefix,admit,"_",short_name)]

tr_matchvars <- copy(dx_map)
tr_matchvars[,var_name:=paste0("match_",diag_code)]
tr_matchvars[,short_name:=paste0("treatment_",short_name)]

format_varnames <- function(data) { 
  for(type in sort(unique(dx_grid[,var_name]),decreasing=T)) {
    proper_name <- unique(dx_grid[var_name==type,short_name])
    data[,var_name:=gsub(type,proper_name,var_name)]
  }
  for(type in sort(unique(tr_matchvars[,var_name]),decreasing=T)) {
    proper_name <- unique(tr_matchvars[var_name==type,short_name])
    data[,var_name:=gsub(type,proper_name,var_name)]
  }
}

for(d in c("imp_summary","include_summary","imp_top_15","incl_top_15")) {
  format_varnames(get(d)) 
}

incl_top_15[,combined_varname := sprintf("%02d %s",rank,var_name)]
imp_top_15[,combined_varname := sprintf("%02d %s",rank,var_name)]


## For Heatmap of variable importance/inclusion, do some re-coding to make it sensible
## Logistic regression makes dummies, so we have to categorize them appropriately
incl_heat <- copy(incl_top_15)
incl_heat <- incl_heat[grepl("age",var_name),var_name:="age"]
incl_heat[,var_name:=gsub("Yes","",var_name)]
incl_heat[,var_name:=gsub("No","",var_name)]
incl_heat[,var_name:=gsub("Miss","",var_name)]

## Format inclusion and importance datasets similarly
imp_heat <- merge(imp_top_15,method_labels,by="pred_method")
imp_heat[,pred_method:=NULL]
imp_heat[imp_type=="accuracy",imp_type:="acc"]
imp_heat <- imp_heat[,model_expanded:=paste0(Method,"_",imp_type)]

# incl_heat[pred_method=="lr",rank:=7] # All logistic is included in everything
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


####################################################################################################
## Format admit lables to be presentation-friendly
heat_template[admit=="admit_only",admit:="Admit Only"]
heat_template[admit=="all",admit:="All Variables"]
setnames(heat_template,"admit","Admit")

## Add a sort order for heat template
sort_order <- unique(heat_template[,list(model_expanded,var_name,Admit,rank)]) 
sort_order <- sort_order[!is.na(rank),list(tot_num=length(rank),
                                           best_rank=min(rank)),
                         by=list(var_name,Admit)]
sort_order <- sort_order[order(tot_num,-best_rank)]
sort_order <- sort_order[,sort:=rank(tot_num,ties.method="first"),by=list(Admit)]

heat_template <- merge(heat_template,sort_order,by=c("var_name","Admit"))


## Add method labels onto all output datasets
format_labels <- function(data) {
  ## Re-assign variables instead of merging so that the changes will "stick" easily
  if("pred_method" %in% colnames(data)) {
    for(type in unique(method_labels[,pred_method])) {
      proper_name <- unique(method_labels[pred_method==type,Method])
      data[pred_method==type,pred_method:=proper_name]
    }
    setnames(data,"pred_method","Method")
  }
  
  ## Format admit lables to be presentation-friendly
  if("admit" %in% colnames(data)) {
    data[admit=="admit_only",admit:="Admit Only"]
    data[admit=="all",admit:="All Variables"]
  }
#   setnames(data,"admit","Admit")
  
  ## Rename d_wt
  if("d_wt" %in% colnames(data)) setnames(data,"d_wt","Death_weight")
}

for(d in c("best_auc","auc_summary","acc_summary","auc_results","roc_results",
           "hl_summary","imp_summary","include_summary",
           "imp_top_15","incl_top_15","best_models")) {
  format_labels(get(d))
}


####################################################################################################
## Output results

## Save ROC graphs with all 100 draws and median draw highlighted in red
auc_results[,median:=quantile(auc,.5,type=3),by=list(Method,Death_weight,admit)] # This chooses a median by defaulting to nearest even order statistic
auc_meds <- auc_results[auc==median,list(fold,rep,Death_weight,admit,Method)]
auc_meds <- auc_meds[!duplicated(auc_meds[,list(Death_weight,admit,Method)]),]
auc_meds[,median:=1]
roc_results <- merge(roc_results,auc_meds,by=c("Death_weight","admit","Method","fold","rep"),all.x=T)
roc_results[is.na(median),median:=0]
roc_results <- merge(roc_results,best_models,by=c("Method","admit","Death_weight"))

## Calculate FPR given TPR cutoffs
setkey(roc_results,Method,admit,Death_weight,fold,rep)

## Calculate the rel_change within group -- each observation's value of rel_change is the rel_change between the current observation and the previous
## Equation is (current_value - previous_value) / previous_value
calc_tpr_fpr <- function(tpr_threshold=.75) {
  dt <- roc_results[shift(tpr,1,type="lag") < tpr_threshold & tpr > tpr_threshold]
  dt <- dt[,list(mean=mean(fpr),lower=quantile(fpr,.025),upper=quantile(fpr,.975)),by=list(Method,admit,Death_weight)]
  dt[,threshold:=paste0("FPR at TPR ",tpr_threshold)]
  return(dt)
}

thresholds <- c(.7,.8,.9)
summary_tpr <- rbindlist(lapply(thresholds,calc_tpr_fpr))
summary_tpr[,format_fpr := paste0(round(mean,2)," (",round(lower,2),"-",round(upper,2),")")]
summary_tpr <- data.table(dcast(summary_tpr[,list(Method,admit,format_fpr,threshold)],Method+admit~threshold,value.var="format_fpr"))
summary_tpr <- summary_tpr[order(admit,Method),]

save(best_auc,auc_summary,acc_summary,hl_summary,
     imp_summary,include_summary,
     imp_top_15,incl_top_15,heat_template,
     roc_results,summary_tpr,
     file=paste0(out_dir,"/results.RData"))


