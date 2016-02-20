## Exploratory Data Analysis on Thesis Data
## Grant Nguyen
## Feb 19, 2016

#####################################################
## Set filepaths etc.
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


#####################################################
## Take a preliminary look at the data
## Citation: http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0133950#sec017
data <- data.table(read.dta(paste0(data_dir,"/risk score database for PLoS One_updated 25.9.14.dta")))
include <- data[include==1,]

#####################################################
## Output summary graphs

## Number of visits over time 
count_total <- include[,list(visits=.N),by=list(monthyear,SiteID)]
plot <- ggplot(count_total, aes(x=monthyear,y=visits)) + 
  geom_line(aes(color=factor(SiteID))) 
print(plot)

## Death rate per facility, by month
deaths <- include[Disposition=="death (1)",list(deaths=.N),by=list(monthyear,SiteID)]
admissions <- include[,list(admissions=.N),by=list(monthyear,SiteID)]
deaths <- merge(deaths,admissions,by=c("monthyear","SiteID"),all=T)
deaths[,death_rate:=deaths/admissions]
plot <- ggplot(deaths, aes(x=monthyear,y=death_rate)) + 
  geom_line(aes(color=factor(SiteID))) 
print(plot)

## Signs/Symptoms breakdowns

#####################################################
## Try to make a decision tree
pred_vars <- c("Mfever","Mdiffbreath","Maltconscious","Munabledrink",
               "Mtemp","Mpallor","Mjaundice","Mdpbreath","Munconscious","Mmenin",
               "Convulsions","Vomiting","Diarrhea","TeaUrine","Airway","Wheezing",
               "Rhonchi","Lethargy","UnableSit")
formulas <- as.formula(paste("death~",paste(pred_vars,collapse="+")))
fit <- rpart(formulas,data=data.frame(data),control=rpart.control(minsplit=2, minbucket=1, cp=0.0009,maxdepth=5))
fit <- rpart(death~.,data=data.frame(data))
summary(fit)


## Random Forests


## SVM


