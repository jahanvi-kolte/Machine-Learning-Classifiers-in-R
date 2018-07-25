#Lab6

#Install Packages
install.packages("ISLR")
install.packages("gam")

#Load Libraries
library(ISLR)
library(gam)
library(splines)

#Attach Dataset
attach(Wage)

View(Wage)

#year,4 means 4 degrees of freedom
gam1=lm(wage~ns(year,4)+ns(age,5)+education,data=Wage)
#gam as we cannot use lm with s
gam.m3=gam(wage~s(year,4)+s(age,5)+education,data=Wage)

par(mfrow=c(1,3))
plot(gam.m3, se=TRUE,col="blue",main="Graph 1: GAM with smoothing splines")
plot.Gam(gam1, se=TRUE, col="red",main="Graph 2: GAM with natural splines")
#dotted lines - confidence interval

#Model selection
gam.m1=gam(wage~s(age,5)+education,data=Wage)
gam.m2=gam(wage~year+s(age,5)+education,data=Wage)

anova(gam.m1, gam.m2, gam.m3, test="F")
#F-test between first two pairs and second two pairs, order matters, should be increasing order of flexibility
#overfitting issue if too flexible

#p-value is sig from 1, so it, *** stars 
#m3 mor flexible but not very diff
#so second model has apt degree 
#choose model which is diff and after which model is not so different
#F-statistic

#Make predictions
summary(gam.m3)
preds=predict(gam.m2, newdata=Wage)

###GAM for classification
#use identity function , and specify binomial classification 
gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr,se=T,col="green",main="Graph 3")
#edu - var large - conf interval wide. less for less than HS

table(education,I(wage>250))
#No observation for <HS
#If no single observation, hard to make prediction 
#So, you can remove that level

gam.lr=gam(I(wage>250)~year+s(age,df=5)+education,family=binomial,data=Wage,
           subset=(education!="1. < HS Grad") )

par(mfrow=c(1,3))
plot(gam.lr,se=T,col="orange",main="Graph 4")
#can observe increasing trend
#confirms with regression results

#y-axis: log(p/1-p); p is between 0 and 1

#Take Home Questions 
#Question 1
gam.m4=lm(wage~poly(year,6)+bs(age,degree=5)+education,data=Wage)
par(mfrow=c(1,3))
plot.Gam(gam.m4, se=TRUE,col="blue")

#or

gam.m4=gam(wage~poly(year,6)+bs(age,degree=5)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam.m4, se=TRUE,col="blue",main="Graph 5")
#assumes plot.gam

#Question 2

gam.lr.m1=gam(I(wage>250)~year+education,family=binomial,data=Wage,
           subset=(education!="1. < HS Grad") )
gam.lr.m2=gam(I(wage>250)~year+age+education,family=binomial,data=Wage,
              subset=(education!="1. < HS Grad") )
gam.lr.m3=gam(I(wage>250)~year+s(age,2)+education,family=binomial,data=Wage,
              subset=(education!="1. < HS Grad") )
gam.lr.m4=gam(I(wage>250)~year+s(age,5)+education,family=binomial,data=Wage,
              subset=(education!="1. < HS Grad") )
gam.lr.m5=gam(I(wage>250)~year+s(age,8)+education,family=binomial,data=Wage,
              subset=(education!="1. < HS Grad") )

#anova(gam.lr.m1,gam.lr.m2,gam.lr.m3,gam.lr.m4,gam.lr.m5, test="F")
anova(gam.lr.m1,gam.lr.m2,gam.lr.m3,gam.lr.m4,gam.lr.m5, test="Chisq")
