#Lab7
#jsk362

install.packages("tree")
#Loading Libraries
library(tree)
library(ISLR)

attach(Carseats)
?Carseats

High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

tree.carseats=tree(High~.-Sales ,Carseats )
summary(tree.carseats)
#inc purity or dec mse so only 8 used
plot(tree.carseats,main="Graph 1")
text(tree.carseats,pretty=0)

tree.carseats

set.seed(2)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
output<-table(tree.pred,High.test)
#Test accuracy
(output[1,1] + output[2,2])/sum(output)*100

set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
cv.carseats

prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
output<-table(tree.pred,High.test)
#Test accuracy
(output[1,1] + output[2,2])/sum(output)*100
#0.77 earlier 0.715

#Bagging and Random Forests
install.packages("randomForest")
library(MASS)
library(randomForest)
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,
                          importance=TRUE)
bag.boston


set.seed(1)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)

set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)


set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,
                         importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)

#Random forest better

importance(rf.boston)
#by removing the feature how the metric is affected
#higher value , correcponding feature is imp

#besides making pred, can tell us which features are good
#rm , lstat
varImpPlot(rf.boston)


#Take Home part
#Clean workspace

install.packages("tree")
install.packages("randomForest")

#Loading Libraries
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
attach(Carseats)
?Carseats

High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

set.seed(2)
train=sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.test=Carseats[-train,]
High.test=High[-train]

names(Carseats)
dim(Carseats)

####

set.seed(2)
bag.carseats=randomForest(High~.-Sales,data=Carseats,subset=train,mtry=10,ntree=10,
                        importance=TRUE)
bag.carseats

set.seed(2)
yhat.bag = predict(bag.carseats,newdata=Carseats.test)
sum(yhat.bag!=High.test)/length(High.test)
output<- table(yhat.bag,High.test)
#Test misclassification rate
(output[1,2] + output[2,1])/sum(output)*100
bag.carseats
importance(bag.carseats)


#Bagging 500 trees
set.seed(2)
bag.carseats=randomForest(High~.-Sales,data=Carseats,subset=train,mtry=10,ntree=500,
                          importance=TRUE)
set.seed(2)
yhat.bag = predict(bag.carseats,newdata=Carseats.test)
sum(yhat.bag!=High.test)/length(High.test)


output<- table(yhat.bag,High.test)
output
#Test misclassification rate
(output[1,2] + output[2,1])/sum(output)*100

#Random Forest
set.seed(2)
rf.carseats=randomForest(High~.-Sales,data=Carseats,subset=train,mtry=3,ntree=500,
                          importance=TRUE)
set.seed(2)
yhat.rf = predict(rf.carseats,newdata=Carseats.test)
sum(yhat.rf!=High.test)/length(High.test)


output<- table(yhat.rf,High.test)
output
#Test misclassification rate
(output[1,2] + output[2,1])/sum(output)*100

install.packages("gbm")
library(gbm)

#Boosting
Carseats$High
Carseats$High <- ifelse(Carseats$High=="Yes" , 1, 0)
Carseats$High
set.seed (2)
boost.carseats=gbm(High~.-Sales,data=Carseats[train,],distribution="bernoulli",n.trees=5000, interaction.depth=4)
set.seed (2)
yhat.boost=predict(boost.carseats,newdata=Carseats.test, n.trees=5000,type="response")
yhat.boost
yhat.boost <- as.numeric(yhat.boost >= 0.5)
High.test <-ifelse(High.test=="Yes" , 1, 0)
sum(yhat.boost!=High.test)/length(High.test)
table(yhat.boost,High.test)

#Logistic Regression
carseats.logit <- glm(High~.-Sales, data=Carseats[train,], family=binomial)
summary(carseats.logit)
carseats.logit.pred <- predict(carseats.logit,Carseats.test,type="response")
carseats.logit.pred
#Say threshold is 0.5
carseats.bin <-as.numeric(carseats.logit.pred >= 0.5)
carseats.bin
output <- table(carseats.bin,High.test)
output
#Test misclassification rate
(output[1,2] + output[2,1])/sum(output)*100
