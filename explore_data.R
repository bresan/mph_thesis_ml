## Exploratory Data Analysis on Thesis Data
## Grant Nguyen
## Feb 19, 2016

#####################################################
## Set filepaths to code and data
if(c(os)=="Windows") {
  master_dir <- "H:/Thesis/data"
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
require(reshape2) # For potential reshapes 
require(foreign) # For importing dataset
require(rpart) # Decision trees


#####################################################
## Take a preliminary look at the data
## Citation: http://journals.plos.org/plosone/article?id=10.1371/journal.pone.0133950#sec017
data <- data.table(read.dta(paste0(data_dir,"/risk score database for PLoS One_updated 25.9.14.dta")))
include <- data[include==1,]

## What is the average length of stay?
table(data[,LOS]) # Seems like length of stay has lots of negative values and some implausible high numbers -- probably due to poor date coding
hist(data[LOS > 0 & LOS < 50,LOS]) # Do a brief scan of how long kids stay in the hospital

## Investigate different outcomes for patients
table(data[,Disposition])
table(data[,list(Disposition,include)])
addmargins(table(data[,list(death,SiteID)]))

## Investigate number of deaths, by year and facility
for(site in unique(data[,SiteID])) {
  print(paste0("Status by Year for ",site))
  print(addmargins(table(data[SiteID==site,list(Disposition,year)])))
  # new <- addmargins(table(data[SiteID==site,list(Disposition,year)]))
}


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

