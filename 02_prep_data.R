#######################################
## Grant Nguyen
## Purpose: Clean input data appropriately

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


#####################################################
## Import Data
# data <- data.table(read.dta(paste0(data_dir,"/risk score database for PLoS One_updated 25.9.14.dta")))
# data <- data.table(read.dta13(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_3rd Nov 2016.dta")))
# data <- data.table(read_dta(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_Version 2013.dta")))

  master_data <- data.table(fread(paste0(data_dir,"/01_cleaned_data.csv")))


#####################################################
## Generate a count of all missing values and output it
  count_missing <- function(x) {
    n <- length(x[is.na(x)])
    return(n)
  }
  
  missing_data <- data.table(master_data[,lapply(.SD,count_missing)])
  missing_data <- melt(missing_data)
  setnames(missing_data,"value","num_missing")
  missing_data <- missing_data[num_missing != 0,]
  missing_data <- missing_data[order(-num_missing)]
  write.csv(missing_data,paste0(data_dir,"/02_missing_vals.csv"),row.names=F)
  
#####################################################
## Impute all missing values using Multiple Imputation

## Define MI wrapper to apply different types of MI
impute_missing <- function(data,id_vars,mi_type="mi_gelman") {
  ## Apply multiple imputation to a dataset and a set of prediction variables, using Andrew Gelman's MI package
  ## We use this to feed imputed data into the Random Forests analysis, as it requires a complete dataset
  ## MI comparisons here: http://thomasleeper.com/Rcourse/Tutorials/mi.html
  
  ## First, prepare variable lists
  get_min_max <- function(x) {
    c(min=min(x,na.rm=T),max=max(x,na.rm=T))
  }
  pred_vars <- data.table(data)[,lapply(.SD,get_min_max)]
  pred_vars[1,type:="min_val"]
  pred_vars[2,type:="max_val"]
  pred_vars <- melt(pred_vars,id.vars=c("type"),variable.name="master_var",value.name="val")
  pred_vars[,master_var:=as.character(master_var)]
  pred_vars <- data.table(dcast(pred_vars,master_var~type,value.var="val"))     
  
  pred_vars[,test_var:=as.numeric(max_val)] # Figure out which are numeric or not, and which of the numeric ones are nominal(binary 1/0)
  same_vars <- unique(pred_vars[min_val==max_val,master_var]) # Get a list of variables where the min and max do not vary
  
  print("Removing the following variables which do not vary in the dataset: ")
  print(same_vars)
  data[,c(same_vars):=NULL]
  
  pred_vars <- pred_vars[min_val != max_val,]
  
  ## Do we want to be imputing our outcome? Or not? 
  # > table(master_data[,death])
  # 
  #       No   Yes 
  # 528 80979  2583
  
  nom_vars <- unique(pred_vars[(test_var==1|is.na(test_var)) & !(master_var %in% id_vars),master_var])
  cont_vars <- unique(pred_vars[!master_var %in% c(nom_vars,id_vars),master_var])
  
  ## Remove variables that only have the same outcomes
  nom_vars <- nom_vars[!nom_vars %in% same_vars]
  cont_vars <- cont_vars[!cont_vars %in% same_vars]
  
  if(mi_type == "mi_gelman") {
    require(mi)
    data <- missing_data.frame(data.frame(data[,.SD,.SDcols=c(nom_vars,cont_vars)]))
    show(data)
    # change() # If I want to change the transformation, imputation method,etc
    summary(data)
    imputations <- mi(data)
    Rhats(imputations)
    new <- complete(imputations)
    lapply(new,summary)
    
    ## Here, return the imputations as separate data frames (or do we just run the analysis separately here?)
  } else if(mi_type == "amelia") {
    # Troubleshooting Amelia: https://lists.gking.harvard.edu/pipermail/amelia/2013-July/001030.html
    require(Amelia)
    imputed_dataset <- amelia(x=data.frame(data),m=3, # where m = number of datasets to construct
                              idvars=id_vars,
                              noms=nom_vars # Nominal/categorical variables
                             ) # cont_vars = continuous variable
  } else if(mi_type == "mice") {
    
  }
}

## Apply MI function
  
## Test out MI on 5,000 random observations, before scaling it up and applying the true function
  test_mi = "amelia"
  set.seed(9840294)
  data <- copy(master_data) 
  data[,sort_obs:=rnorm(nrow(data))]
  data <- data[order(sort_obs)]
  data <- data[1:5000,]
  data[,sort_obs:=NULL]

  id_vars <- c("cv_dateadmit","dx_malaria_final","death","malariadeath")
  convert_vars <- c("dx_malaria_final","death","malariadeath") ## Need to convert to nominal eventually rather than using as an id var
  
  imputed_data <- system.time(impute_missing(data,id_vars,mi_type=test_mi))
  
