## Grant Nguyen 
## Launch 10 repetitions of 10-fold cross-validation 

## Setup filepaths
code_dir <- ""
data_dir <- ""
out_dir <- ""

## Identify max number of repetitions and folds
max_reps <- 10
max_folds <- 10 
rep_fold_combos <- expand.grid(c(1:max_reps),c(1:max_folds))


########################################################
## Pull in functions and create repetition-cv map
library(data.table)

## Define qsub function to submit jobs to the cluster
qsub <- function(jobname, code, hold=NULL, pass=NULL, slots=1, submit=F, log=T, proj = "") { 
  user <- Sys.getenv("USER") # Default for linux user grab. "USERNAME" for Windows
  # choose appropriate shell script 
  if(grepl(".r", code, fixed=T) | grepl(".R", code, fixed=T)) shell <- "r_shell.sh" else if(grepl(".py", code, fixed=T)) shell <- "python_shell.sh" else shell <- "stata_shell.sh" 
  # set up number of slots
  if (slots > 1) { 
    slot.string = paste(" -pe multi_slot ", slots, sep="")
  } 
  # set up jobs to hold for 
  if (!is.null(hold)) { 
    hold.string <- paste(" -hold_jid \"", hold, "\"", sep="")
  } 
  # set up arguments to pass in 
  if (!is.null(pass)) { 
    pass.string <- ""
    for (ii in pass) pass.string <- paste(pass.string, " \"", ii, "\"", sep="")
  }  
  # construct the command 
  sub <- paste("qsub",
               if(log==F) " -e /dev/null -o /dev/null ",  # don't log (if there will be many log files)
               if(log==T) paste0(" -e /share/temp/sgeoutput/",user,"/errors -o /share/temp/sgeoutput/",user,"/output "),
               if(proj != "") paste0(" -P ",proj," "),
               if (slots>1) slot.string, 
               if (!is.null(hold)) hold.string, 
               " -N ", jobname, " ",
               shell, " ",
               code, " ",
               if (!is.null(pass)) pass.string, 
               sep="")
  # submit the command to the system
  if (submit) {
    system(sub) 
  } else {
    cat(paste("\n", sub, "\n\n "))
    flush.console()
  } 
} 


########################################################
## Delete existing output from analyses
system(paste0("perl -e 'unlink <",data_dir,"/*.csv>' "))

# Alternative if no perl:
# system(paste0("rm ",data_dir,"/*.csv"))


########################################################
## Launch jobs 
for(rep in 1:max_reps) {
  for(fold in 1:max_folds) {
    qsub(paste0("cv_",rep,"_",fold),code=paste0(code_dir,"/03_run-analysis.R"),pass=list(rep,fold,max_folds),submit=F,proj="")
  }
}


########################################################
## Check for results and combine them
check_results <- function(locations,check_dir,prefix="",postfix,sleep=30) {
  counter <- 0
  time_counter <- 0
  while(counter == 0) {
    inner_counter <- 0
    missing_list <- ""
    for(loc in locations) {
      if(file.exists(paste0(check_dir,"/",prefix,loc,postfix))) {
        inner_counter <- inner_counter + 1
      } else {
        missing_list <- paste0(missing_list," ",loc)
      }
    }
    
    if(length(locations) == inner_counter) {
      print("All results are present")
      counter <- 1
    } else {
      print(paste0("Have ",inner_counter," results: expecting ",length(locations)," ",Sys.time()))
      if(inner_counter > (length(locations) * .75)) {
        print(paste0("Still Missing: ",missing_list)) 
      }
      time_counter <- time_counter + 1
      Sys.sleep(sleep)
      if(time_counter * (sleep/60) > 180) stop("Jobs are taking over 3 hours -- stopping execution") 
    }
  }
}

## Wait 30 minutes, then start checking
Sys.sleep(60*30)

for(rep in 1:max_reps) {
  print(paste0("Checking folds for rep ",rep))
  check_results(c(1:max_folds),data_dir,prefix="results)",postfix=".csv",sleep=60)
}

## Once all files are written, combine them together
## Use check_loc_results, or submit a combine job?
setwd(data_dir)
files <- paste0("results_",rep_fold_combos$Var1,"_",rep_fold_combos$Var2,".csv")
compiled_results <- data.table(rbindlist(lapply(files,function(x) fread(x))))

## Determine the best method based on all of the CV runs
summary_results <- compiled_results[,list(mean_acc = mean(accuracy), sd_acc = sd(accuracy), mean_auc = mean(auc)), by=list(mdoel_type)]

## Output compiled results and summary metrics
write.csv(compiled_results,paste0(out_dir,"/compiled_results.csv"),row.names=F)
write.csv(summary_results,paste0(out_dir,"/summary_results.csv"),row.names=F)

