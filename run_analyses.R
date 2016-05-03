#####################################################
## Grant Nguyen
## Purpose: Apply cleaning and data analysis functions to the dataset


#####################################################
## Set filepaths to code and data
master_dir <- "/Users/Grant/Desktop/Thesis/"
data_dir <- paste0(master_dir,"/data")
code_dir <- paste0(master_dir,"/code")


#####################################################
## Set Packages
require(data.table) # For easy data management
require(ggplot2) # For graphing
require(reshape2) # For potential reshapes 
require(foreign) # For importing dataset
require(rpart) # Decision trees
require(readstata13) # Importing dataset

## Import analysis and data cleaning functions
source(paste0(code_dir,"/clean_data.R"))
source(paste0(code_dir,"/analyse_functions.R"))

## Declare predictive variables
pred_vars <- c("Mfever","Mdiffbreath","Maltconscious","Munabledrink",
               "Mtemp","Mpallor","Mjaundice","Mdpbreath","Munconscious","Mmenin",
               "Convulsions","Vomiting","Diarrhea","TeaUrine","Airway","Wheezing",
               "Rhonchi","Lethargy","UnableSit")
id_vars <- c("","","")

####################################################
## Import and Separate Data
data <- data.table(read.dta13(paste0(data_dir,"/IP data base Nov 2015_Grant Nguyen_3rd Nov 2016.dta")))

data <- data.table(read.dta(paste0(data_dir,"/risk score database for PLoS One_updated 25.9.14.dta")))
data[,SiteID:=as.character(SiteID)]
sites <- unique(data[,SiteID])

combined_data <- data[include==1,]

jinja_data <- combined_data[grepl("Jinja",SiteID),]
mubende_data <- combined_data[grepl("Mubende",SiteID),]
tororo_data <- combined_data[grepl("Tororo",SiteID),]
apac_data <- combined_data[grepl("Apac",SiteID),]


####################################################
## Clean datasets by site
jinja_data <- impute_missing(jinja_data, pred_vars, outcome_var)

####################################################
## Apply analyses, output results


