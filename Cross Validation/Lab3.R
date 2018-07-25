#Lab 3

getwd()

#Read the data
corollas <- read.csv("ToyotaCorolla.csv", header=T, na.strings = '')

#Data Cleansing
#Delete unnecessary columns or include only necessary columns
corollas <- corollas[,names(corollas) %in% c("Age_08_04","KM","Price")]

#check data for missing values
sum(is.na(corollas))

#check for factor variables
sapply(corollas1,class)
#Everything is Integer here, so we are good.
#otherwise change it to integer/factor or whatever the data type is expected 


set.seed(1)
#Split training and testing data
train_id = sample(nrow(corollas), nrow(corollas)/2)

#Fit Linear model with degree 1
lm.fit = lm(Price~Age_08_04+KM, data=corollas, subset=train_id)
#you can specify train_id without mentioning subset
#lm.fit = lm(Price~Age_08_04+KM, data=corollas, train_id)

attach(corollas)
#if you want to avoid specifying col using $
#MSE for degree 1 linear model
mean((Price - predict(lm.fit,corollas))[-train_id]^2)

#Fit degree 2 linear model
lm.fit2 = lm(Price~poly(Age_08_04, 2)+poly(KM, 2),data=corollas, subset=train_id)

#Fit degree 3 linear model
lm.fit3 = lm(Price~poly(Age_08_04, 3)+poly(KM, 3),data=corollas, subset=train_id)

#MSE for degree 2 linear model
mean((Price - predict(lm.fit2,corollas))[-train_id]^2)

#MSE for degree 3 linear model
mean((Price - predict(lm.fit3,corollas))[-train_id]^2)

lm.fit = lm(Price~Age_08_04+KM, data=corollas, subset=train_id)


lm.mse=rep(0,5)
ptm_lm=proc.time()
for (i in 1:5) {
  lm.fit = lm(Price~poly(Age_08_04, i)+poly(KM, i),data=corollas, subset=train_id)
  lm.mse[i] = mean((Price - predict(lm.fit,corollas))[-train_id]^2)
}
rtime_lm = proc.time() -ptm_lm
rtime_lm
lm.mse


library(boot)

#LOOCV
cv.error=rep(0,5)
ptm_loocv_lm=proc.time()
for (i in 1:5) {
  glm.fit = glm(Price~poly(Age_08_04, i)+poly(KM,i), data = corollas)
  cv.error[i] = cv.glm(corollas, glm.fit)$delta[1] 
}
rtime_loocv_lm = proc.time() -ptm_loocv_lm
rtime_loocv_lm
cv.error
plot(cv.error, type="b", xlab="Degree of Polynomial",ylab="Cross Validation Error", main="LOOCV Error for Linear model")

summary(lm.fit)
summary(lm.fit2)
summary(lm.fit3)

#k-fold validation
set.seed(1)
cv.error=rep(0,5)
ptm_kfold_lm = proc.time()
for (i in 1:5) {
  glm.fit = glm(Price~poly(Age_08_04, i)+poly(KM,i), data = corollas)
  cv.error[i] = cv.glm(corollas, glm.fit, K=10)$delta[1]
}
rtime_kfold_lm=proc.time() - ptm_kfold_lm
cv.error
plot(cv.error, type="b", xlab="Degree of Polynomial",ylab="Cross Validation Error", main="K-Fold CV Error for Linear model where K=10")

# LOOCV - quadratic is better
#K-fold - it also says quadratic is better

predict(lm.fit2,corollas)[-train_id]



cv.glm(DF, glm.fit,K=10,cost=cost)$delta[1]



#Generate synthetic Data

n = 1000
x1 = runif(n)
x2 = runif(n, -2, 1)
z = (x1-0.2)*(x1-0.5)*(x1-0.9) * 25 - x2*(x2+1.2)*(x2-0.8)+ rnorm(n)/3
y = as.integer(z>0)
plot(x1, x2, col=c("red", "blue")[y+1],main="Synthetic Data Plot")
DF = data.frame(x1,x2,y)

#LOOCV for Synthetic Data after fitting Logistic Regression model

cv.error=rep(0,5)
ptm_looocv_lr=proc.time()
for (i in 1:5) {
  syn_logit = glm(y~poly(x1, i)+poly(x2,i), data = DF,family=binomial)
  cv.error[i] = cv.glm(DF, syn_logit)$delta[1] 
}
rtime_loocv_lr = proc.time() - ptm_looocv_lr
rtime_loocv_lr
cv.error
plot(cv.error, type="b", xlab="Degree of Polynomial",ylab="Cross Validation Error", main="LOOCV Error for Logistic Regression model over synthetic data")

#Split training and testing data 

syn_test_id <- sample(nrow(DF), 100)
syn_test <- DF[syn_test_id,]
syn_train <- DF[-syn_test_id,]




#K-fold 
#Define cost function
cost <- function(r, pi=0){
  mean(abs(r-pi)>0.5)
}

#Cal K-fold CV after fitting logistic regression model

cv.error=rep(0,5)
ptm = proc.time()
for (i in 1:5) {

  glm.fit = glm(y~poly(x1, i)+poly(x2,i), data = syn_train,family=binomial)
  cv.error[i] = cv.glm(syn_train, glm.fit, cost, K=10)$delta[1]
}
proc.time() - ptm
cv.error
plot(cv.error, type="b", xlab="Degree of Polynomial",ylab="Cross Validation Error", main="K-Fold CV Error for Logistic Regression model over synthetic data where K=10")

#Predict using the best model
syn_glm_fit = glm(y~poly(x1, 3)+poly(x2,3), data = syn_train,family=binomial)
syn_pred <- predict(syn_glm_fit,syn_test,type="response")
#syn_pred
syn_bin <-as.numeric(syn_pred >= 0.5)
#syn_bin
output <- table(syn_bin,syn_test$y)
output
accu <- (output[1,1] + output[2,2])/sum(output)
accu
err_rate <- 1 - accu
err_rate
