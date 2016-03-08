#####################################################
## Grant Nguyen
## Purpose: Create Functions to Analyze data and output results

#####################################################
## Try to make a decision tree

## First, declare formula
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

####################################################################
## Cross-Validate
cv_results <- function(data,model_names) {
  require(ModelGood)
  require(stargazer) # For creating pretty tables -- does this actually work with the roc output?
  ## Perform bootstrap cross-validation of RoC and Brier scores with 1000 bootstrapped samples
  roc_results <- Roc(as.list(model_names),data=data,verbose=0,crRatio=1,splitMethod="bootCV",B=1000) 
  print(roc_results)
  plot(roc_results, legend=F,yaxis.las=T, col=c("blue","green","red"))
}


