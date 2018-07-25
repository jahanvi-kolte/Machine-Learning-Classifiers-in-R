#Lab2
#Jahanvi (jsk362)

###Read the dataset
titanic <- read.csv("titanic.csv", header=T, na.strings = '')
#View(titanic)

#891 - observations

###Data Cleaning
#1. Delete unuseful columns ’Passenger ID’, ’Name’, ’Ticket’ and ’Cabin’.
titanic <- titanic[,!names(titanic) %in% c("PassengerId","Name","Ticket","Cabin")]

#Converting to factor variables
titanic$Pclass <- as.factor(titanic$Pclass)

#2. Deal with missing data by removing all associate rows. 
#(Alternatively, you may replace missing values with mean/median).

#Find columns with missing data
colnames(titanic)[colSums(is.na(titanic))>0]

#Find rows where column is na
titanic[is.na(titanic$Embarked),]
titanic[!complete.cases(titanic),]

#extra commands
#nrow(titanic)
#ncol(titanic)
#na.omit(titanic)

#If you have 'NA' char value
#titanic[titanic=='NA'] <- NA

#Age (For continuous variable, you can )
titanic$Age <- ifelse (is.na(titanic$Age),mean(titanic$Age,na.rm=TRUE),titanic$Age)
#titanic$Age[is.na(titanic$Age)] <-median(titanic$Age)

#Remove NA records
titanic <- titanic[complete.cases(titanic),]
#titanic <- titanic[rowSums(!is.na(titanic))>0,]

#Cabin has sparse data or lot of missing value records but we already removed it
#In case we wanted to clean it the commands would be


#Embarked - a factor predictor, we can remove it
#titanic <- titanic[complete.cases(titanic),]


###Generating training and test data set
set.seed(100)
train_ind <- sample(1:nrow(titanic),2/3*nrow(titanic))

titanic_train <- titanic[train_ind,]
titanic_test <- titanic[-train_ind,]
titanic_log_reg <- glm(Survived~., data=titanic_train, family=binomial)
summary(titanic_log_reg)
titanic_pred <- predict(titanic_log_reg,titanic_test,type="response")
titanic_pred
titanic_bin <-as.numeric(titanic_pred >= 0.5)
titanic_bin
output <- table(titanic_bin,titanic_test$Survived)
output
accu <- (output[1,1] + output[2,2])/sum(output)
accu
err_rate <- 1 - accu
err_rate
1 - accu
TPR <- output[2,2]/sum(output[ ,2])
TPR
FNR <- output[1,2]/sum(output[ ,2])
FNR
TNR <- output[1,1]/sum(output[ ,1])
TNR
FPR <- output[2,1]/sum(output[ ,1])
FPR

bal_accu <- 0.5 * TPR + 0.5 * TNR

#simple classifier
titanic_logit_Sex <- glm(Survived~Sex, data=titanic_train, family=binomial)
titanic_pred_Sex <- predict(titanic_logit_Sex,titanic_test,type="response")
titanic_pred_Sex
titanic_bin_Sex <-as.numeric(titanic_pred_Sex >= 0.5)
titanic_bin_Sex
output <- table(titanic_bin_Sex,titanic_test$Survived)
output
accu <- (output[1,1] + output[2,2])/sum(output)
accu
err_rate <- 1 - accu
err_rate

#################
#  KNN
################

#3. Check for factor variables and convert them into dummy variables.
#To check class for titanic
sapply(titanic,class)

#Converting to factor variables
titanic$Pclass <- as.factor(titanic$Pclass)

#unique values of factor variable in R
levels(titanic$Pclass)
levels(titanic$Embarked)

#Converting factor variable into dummy variables
titanic$SexMale <- model.matrix(~Sex, data=titanic)[,"Sexmale"]
titanic$Pclass2 <- model.matrix(~Pclass, data=titanic)[,"Pclass2"]
titanic$Pclass3 <- model.matrix(~Pclass, data=titanic)[,"Pclass3"]
titanic$EmbarkedQ <- model.matrix(~Embarked, data=titanic)[,"EmbarkedQ"]
titanic$EmbarkedS <- model.matrix(~Embarked, data=titanic)[,"EmbarkedS"]

#Remove the factor variables
titanic <- titanic[, !colnames(titanic) %in% c("Sex","Pclass","Embarked")]
View(titanic)

set.seed(100)
train_ind <- sample(1:nrow(titanic),2/3*nrow(titanic))

#Normalize 
titanic_normal <- scale(titanic[,!names(titanic) %in% 'Survived'])
titanic.train <- titanic_normal[train_ind, ]
titanic.test <- titanic_normal[-train_ind, ]


train.survive <- titanic$Survived[train_ind]
test.survive <- titanic$Survived[-train_ind]

library(class)
knn.pred <- knn(titanic.train,titanic.test,train.survive,1)
table(knn.pred,test.survive)
mean(knn.pred==test.survive)
knn_err_rate = 1 - mean(knn.pred==test.survive)
knn_err_rate

K=c(1,3,5,10,20,50)
error <- rep(NA,length(K))
for( i in seq(length(K))){
  set.seed(100)
  knn.pred <- knn(titanic.train,titanic.test,train.survive,K[i])
  error[i] = 1 - mean(knn.pred==test.survive)
}
  
plot(K,error,main="KNN: Total Error Rate plot for different K(1,3,5,10,20,50)",xlab="K",ylab="Total Error Rate",type="b")


#with and w/o normalization
K=seq(1,20)


#w/o normalization
titanic.train <- titanic[train_ind, ]
titanic.test <- titanic[-train_ind, ]


### Cancer study
cancer <- read.csv("pros.csv", header=T, na.strings = '')
cancer <- cancer[complete.cases(cancer),]
cancer <- cancer[cancer$VOL!=0,]

cancer <- cancer[,!names(cancer) %in% c("ID")]
nrow(cancer)
sapply(cancer,class)
cancer$RACE = as.factor(cancer$RACE)
cancer$DPROS = as.factor(cancer$DPROS)
cancer$DCAPS = as.factor(cancer$DCAPS)
set.seed(100)
cancer_train_ind <- sample(1:nrow(cancer), 2/3*nrow(cancer))



cancer_train <- cancer[cancer_train_ind,]
cancer_test <- cancer[-cancer_train_ind,]
cancer_log_reg <- glm(CAPSULE~., data=cancer_train, family=binomial)
summary(cancer_log_reg)
cancer_pred <- predict(cancer_log_reg,cancer_test,type="response")
cancer_pred
cancer_bin <-as.numeric(cancer_pred >= 0.5)
cancer_bin
output <- table(cancer_bin,cancer_test$CAPSULE)
output



p <- seq(0,1,0.05)
tpr <- rep(NA,length(p))
fpr <- rep(NA,length(p))
for (i in seq(length(p))){
 cancer_bin <- as.numeric(cancer_pred>=p[i])
 tpr[i] <- sum(cancer_bin==1 & cancer_test$CAPSULE == 1)/sum(cancer_test$CAPSULE==1)
 fpr[i] <- sum(cancer_bin==1 & cancer_test$CAPSULE == 0)/sum(cancer_test$CAPSULE==0)
}

plot(fpr,tpr,main="Cancer: TPR vs FPR with Logistic Regression",xlab="TPR",ylab="FPR",type="l")


