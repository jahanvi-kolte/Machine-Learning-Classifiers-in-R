#Lab4

#Package Installation
install.packages("ISLR")
install.packages("leaps")
install.packages("glmnet")

#Loading libraries
library(leaps)
library(ISLR)
library(glmnet)

#Loading Data
fix(Hitters)

#Data pre-processing
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary)) 
Hitters=na.omit(Hitters)
dim(Hitters)
#263 20

#subset selection methods

#Best Subset Selection
regfit.full <- regsubsets(Salary~., data=Hitters, nvmax=19)
regfit.full.summary <- summary(regfit.full)
regfit.full.summary

coef(regfit.full,5)

regfit.full.summary$rss
regfit.full.summary$rsq
regfit.full.summary$adjr2
regfit.full.summary$cp
regfit.full.summary$bic

regfit.full.summary$rsq #always inc
regfit.full.summary$rss # always dec with inc in num of var

which.max(regfit.full.summary$rsq)
which.min(regfit.full.summary$rss)


which.min(regfit.full.summary$cp)
which.min(regfit.full.summary$bic)

#Plotting Adjusted R^2
regfit.full.summary$adjr2
plot(regfit.full.summary$adjr2, xlab="Number of Variables",ylab="Adjusted R^2", type="l", main="Adjusted R^2 statistic for different number of variables selected by Best Subset Selection Method")
max.adjr2.varnum=which.max(regfit.full.summary$adjr2)
max.adjr2.varnum
points(max.adjr2.varnum,regfit.full.summary$adjr2[max.adjr2.varnum],col="red",cex=2,pch=20)
#cex - cap expansion how large point is
#pch - circle
#adj r2 max
#cp - lowest
#bic - lowest

#Plotting Cp
regfit.full.summary$cp
plot(regfit.full.summary$cp, xlab="Number of Variables",ylab="Cp", type="l", main="Cp statistic for different number of variables selected by Best Subset Selection Method")
min.cp.varnum=which.min(regfit.full.summary$cp)
min.cp.varnum
points(min.cp.varnum,regfit.full.summary$cp[min.cp.varnum],col="red",cex=2,pch=20)

#Plotting BIC
regfit.full.summary$bic
plot(regfit.full.summary$bic, xlab="Number of Variables",ylab="Cp", type="l", main="BIC statistic for different number of variables selected by Best Subset Selection Method")
min.bic.varnum=which.min(regfit.full.summary$bic)
min.bic.varnum
points(min.bic.varnum,regfit.full.summary$bic[min.bic.varnum],col="red",pch=20, cex=2)

#Forward Selection
regfit.fwd <- regsubsets(Salary~., data=Hitters, nvmax=19,method="forward")
regfit.fwd.summary <- summary(regfit.fwd)
regfit.fwd.summary

#Backward Selection
regfit.bwd <- regsubsets(Salary~., data=Hitters, nvmax=19,method="backward")
regfit.bwd.summary <- summary(regfit.bwd)
regfit.bwd.summary



# Ridge and Lasso Regularization
#use glmnet
#prev we have whole data  with x and y
#here specify them separately

x= model.matrix(Salary~.,Hitters)[,-1]
#[,-1] to remove a dimemnsion of intercept
y=Hitters$Salary

#Ridge Regression (alpha=0)

grid <- 10^seq(10, -2, length=100) # lambda sequence
ridge.mod <- glmnet(x, y, alpha=0, lambda=grid)
dim(coef(ridge.mod))
#20 100
#100 values of lamda
#20 (19 co-effiecients plus intercept)
dim(ridge.mod$beta)

grid
log(grid)
beta.coef=coef(ridge.mod)
dim(beta.coef)
#When including an intercept term in the regression, we usually leave this coefficient unpenalized.
beta.l2.norm<- sapply(1:length(grid), function(i)sqrt(sum(coef(ridge.mod)[-1,i]^2)))
beta.l2.norm
plot(beta.l2.norm~log(grid), xlab="log(lamda)",ylab="L2 norm of beta ridge", type="b", main="Ridge Regression: L2 norm of beta ridge vs log(lamda)")

#larger lamda, lower the l2 norm of coeff

# Predict at lambda = 50
predict(ridge.mod, s = 50,type = "coefficients")

# Split the data into training data set and test data set.
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]
# Model fit on the training set
ridge.mod <- glmnet(x[train,], y[train], alpha=0, thresh=1e-12)
# Use 10-fold cross-validation to choose lambda
set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], nfolds = 10, lambda = ridge.mod$lambda, alpha=0)
cv.out

plot(cv.out,main="Plot of CV Error vs log(lamda) for Ridge regression")

best.lamda<-cv.out$lambda.min
best.lamda
ridge.preds<-predict(ridge.mod,s=best.lamda,newx=x[test,])
ridge.preds
mean((ridge.preds-y.test)^2)


#Lasso Regression
#lasso introduces sparse model some co-eff 0
# alpha=1


#Model fit on the training set
lasso.mod <- glmnet(x[train,], y[train], alpha=1, thresh=1e-12)
# Use 10-fold cross-validation to choose lambda
set.seed(1)
cv.out=cv.glmnet(x[train,], y[train], nfolds = 10, lambda = lasso.mod$lambda, alpha=1)
plot(cv.out,main="Plot of CV Error vs log(lamda) for Lasso regression")
best.lamda<-cv.out$lambda.min
best.lamda
lasso.preds<-predict(lasso.mod,s=best.lamda,newx=x[test,])
lasso.preds
mean((lasso.preds-y.test)^2)


sum(lasso.mod$beta[,which(cv.out$lambda == best.lamda)] == 0)

###Problem 1
#Define predict function 
predict.regsubsets =function (object ,newdata ,id ,...){
  form=as.formula(object$call [[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object ,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi 
}

#Create CV matrix
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.error <- matrix(NA,k,19,dimnames=list(NULL,paste(1:19)))
cv.error

#Calculate CV Error
for(j in 1:k){
  fwd.fit=regsubsets(Salary~., data=Hitters[folds!=j,],nvmax=19,method="forward")
  for(i in 1:19){
    pred=predict(fwd.fit, Hitters[folds==j,],id=i)
    cv.error[j,i]=mean((Hitters$Salary[folds==j]-pred)^2)
  }
}

cv.error

mean.cv.error=apply(cv.error ,2,mean)
mean.cv.error
which.min(mean.cv.error)
par(mfrow=c(1,1))
plot(mean.cv.error ,type='b',main="Problem 1")

#Forward Subset Selection on entire dataset
regfit.fwd=regsubsets (Salary~.,data=Hitters , nvmax=19,method="forward")
coef(regfit.fwd ,9)

###Problem 2



# Split the data into training data set and test data set.
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

start.time <- proc.time()
# Model fit on the training set
ridge.mod <- glmnet(x[train,], y[train], alpha=0, thresh=1e-12)
# Use 10-fold cross-validation to choose lambda
set.seed(1)
cv.out<-cv.glmnet(x[train,], y[train], nfolds = 5, lambda = ridge.mod$lambda, alpha=0)
best.lamda<-cv.out$lambda.min
ridge.preds<-predict(ridge.mod,s=best.lamda,newx=x[test,])
end.time <- proc.time() - start.time

best.lamda
ridge.preds
cv.out
plot(cv.out,main="Problem 2")
mean((ridge.preds-y.test)^2)
end.time 

###Problem 3

# Split the data into training data set and test data set.
set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)
y.test <- y[test]

start.time <-proc.time()
# Model fit on the training set
lasso.mod <- glmnet(x[train,], y[train], alpha=1, thresh=1e-12)
# Use 10-fold cross-validation to choose lambda
set.seed(1)
cv.out<-cv.glmnet(x[train,], y[train], nfolds = 5, lambda = lasso.mod$lambda, alpha=1)
best.lamda<-cv.out$lambda.min
lasso.preds<-predict(lasso.mod,s=best.lamda,newx=x[test,])
end.time <- proc.time() - start.time

best.lamda
lasso.preds
cv.out
plot(cv.out,main="Problem 3")
mean((lasso.preds-y.test)^2)
end.time

