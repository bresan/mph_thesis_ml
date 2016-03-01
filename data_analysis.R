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

## What is the average length of stay?
table(data[,LOS]) # Seems like length of stay has lots of negative values and some implausible high numbers -- probably due to poor date coding
hist(data[LOS > 0 & LOS < 50,LOS]) # Do a brief scan of how long kids stay in the hospital

## Investigate different outcomes for patients
table(data[,Disposition])
table(data[,list(Disposition,include)])

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


#####################################################
## Try to make a decision tree

## First, declare formula
pred_vars <- c("Mfever","Mdiffbreath","Maltconscious","Munabledrink",
               "Mtemp","Mpallor","Mjaundice","Mdpbreath","Munconscious","Mmenin",
               "Convulsions","Vomiting","Diarrhea","TeaUrine","Airway","Wheezing",
               "Rhonchi","Lethargy","UnableSit")
formulas <- as.formula(paste("death~",paste(pred_vars,collapse="+")))

## Create a regression tree using rpart
fit <- rpart(formulas,data=data.frame(data),control=rpart.control(minsplit=2, minbucket=1, cp=0.0007,maxdepth=5))
fit <- rpart(death~.,data=data.frame(data))
summary(fit)
printcp(fit)
plotcp(fit)
plot(fit, uniform=T)
text(fit,use.n=T,all=T,cex=.8)

## Another dtree using maptree
library(maptree)
library(cluster)
draw.tree( clip.rpart (rpart ( raw), best=7),
           nodeinfo=TRUE, units="species",
           cases="cells", digits=0)
a = agnes ( raw[2:4], method="ward" )
names(a)
a$diss
b = kgs (a, a$diss, maxclust=20)

plot(names(b), b, xlab="# clusters", ylab="penalty", type="n")
xloc = names(b)[b==min(b)]
yloc = min(b)
ngon(c(xloc,yloc+.75,10, "dark green"), angle=180, n=3)
apply(cbind(names(b), b, 3, 'blue'), 1, ngon, 4) # cbind(x,y,size,color)

## Use party package
library(party)
data_new <- data
data_new[death=="no (0)",weight:=1]
data_new[death=="yes (1)",weight:=20]
(ct = ctree(formulas,data=data.frame(data_new),controls=ctree_control(maxdepth=3),weights=c(data_new[,weight])))
plot(ct,main="Conditional Inference Tree")
table(predict(ct),data.frame(data_new)$death)
tr.pred = predict(ct,newdata=data,type="prob")

## Try bagging
library(ipred)
fit <- bagging(formulas,data=data.frame(data),coob=T,nbagg=30)
print(fit)
summary(fit)

## Random Forests
library(randomForest)
rfImpute(death~.,data.frame(data)) # Trying to impute data...
rf_fit <- randomForest(formulas,data=data.frame(data),ntree=1000,keep.forest=F,importance=T)
plot(rf_fit,log="y")


## SVM


