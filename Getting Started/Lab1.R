getwd();
a<-5
a
b <- c(1,2,3)
b
c <- (1:5)
c
e <- rep(0,10)
e
f <- c("Monday", "Tuesday")
f
b <- as.factor(b)
b
levels(b)
d>5
d > 5
d<-c(1:10)
d > 5
f
f[f=="Monday"]
f[f=="Monday"] <- "Sunday"
f
Fun1 <- function(a) { return (a+2) }
fun2 <- function(a){ print(sum(a) d<-Fun1(a) return(mean(d)) }
fun2 <- function(a){
print(sum(a)
d<-Fun1(a)
fun2 <- function(a){
print(sum(a))
d<-Fun1(a)
return(mean(d))
}
(1:4)
b <- Fun2((1:4))
b <- fun2((1:4))
g<- c(1:4)
sum(g)
Fun1(g)
mean(Fun1(g))
b
fun3 <- function(a){}
fun3 <- function(a){
d<- length(a)
for (i in (1:d)){}
for(i in (1:d)){
j<- d+1-i
print(a[j])
}
}
b=c(3,5,2,7)
Fun3(b)
fun3(b)
help("read.table")
A <- matrix(c(1, 3, 2, 2, 8, 9), ncol = 3)
A
A2 <- matrix(c(1, 3, 2, 2, 8, 9), ncol = 3, byrow = T)
A2
2 * A
B <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 3)
C <- matrix(c(5, 3, 1, 4), ncol = 2)
rbind(B, C)
B
C
lbind(B, C)
help(rbind)
help(lbind)
??lblind
??lbind
help(rbind)
rbind(B,C, deparse.level = 1)
ncol(B)
ncol(C)
rbind(B,C)
rbind(B,C)
D<- as.matrix(1:6, ncol=3)
D
rbind(B,C)
rbind(B,D)
D<- as.matrix(c(1:6),ncol=3)
D<- as.matrix(c(1:6),ncols=3)
D<- matrix(c(1:6),ncols=3)
D<- matrix(c(1:6),ncol=3)
rbind(B,D)
B
rbind(B,D, byrow=T)
lbind(B,D)
seq(from = -10, to = 20, by = 0.01)
y=(0.1*x^3)-(2*x^2)+(0.1*x)
x =seq(from = -10, to = 20, by = 0.01)
y=(0.1*x^3)-(2*x^2)+(0.1*x)
help(plot)
plot(x,y,type="l",main="x-y",xlab = "x_axis",y_lab="y_axis")
plot(x,y,type="l",main="x-y",xlab = "x_axis",y_lab ="y_axis")
use warnings()
warnings()
plot(x,y,type="l",main="x-y",xlab = "x_axis",y_lab ='y_axis')
plot(x,y,type="p",main="x-y",xlab = "x_axis",y_lab ='y_axis')
library(plyr)
library(plyr)
library(plyr)
options("repos")[[1]][1]
options(repos="http://streaming.stat.iastate.edu/CRAN")
install.packages("plyr")
options(repos="http://archive.linux.duke.edu/cran/")
install.packages("plyr")
library("plyr")
rbind(B,C)
lbind(B,C)
help(rbind)
plot(x,y,type="l",main="x-y",xlab = "x_axis",y_lab="y_axis")
plot(x,y,type="l",main="x-y",xlab = "x_axis",ylab="y_axis")
plot(x,y,type="l",main="x-y",xlab = "x_axis",ylab="y_axis")
z=e^x
z=x^2
par(mfrow = c(2, 1))
plot(x,z)
par(mfrow = c(2, 1))
plot(x,y,type="l",main="x-y",xlab = "x_axis",ylab="y_axis")
par(mfrow = c(2, 2))
plot(x,z)
plot(x,z)
help(par)
par(mfrow = c(2, 1))
par("G1"
)
plot(x,y)
plot(x,z)
plot(y,z)
d <- read.table("ToyotaCorolla",header="T",sep=",")
d <- read.table("ToyotaCorolla.csv",header="T",sep=",")
getwd()
setwd("/Users/jahanvi/Documents/Cornell/sem1/sdm/practicals")
d <- read.table("ToyotaCorolla.csv",header="T",sep=",")
d <- read.table("ToyotaCorolla.csv",header=T,sep=",")
d[1,]
nrow(d)
distinct(d$Model)
unique(d$Model)
nrow(unique(d$Model))
count(unique(d$Model))
c <=count(unique(d$Model))
c =count(unique(d$Model))
is.NA(d)
is.na(d)
sum(is.na(d))
c <- d[, c("Price","Age_08_04", "KM", "Fuel_Type", "HP",
"Met_Color", "Doors", "Quarterly_Tax", "Weight") ]
is.factor(corollas2$Fuel_Type)
is.factor(c$Fuel_Type)
is.factor(c$Met_Color)
as.factor(c$Met_Color)
dim(d)
is.factor(c$Met_Color)
as.factor(,c)
as.factor(,c$Met_Color)
as.factor(c$Met_Color)
c[,c("Met_Color")] <- as.factor(,c$Met_Color)
c[,c("Met_Color")] <- as.factor(c$Met_Color)
is.factor(c$Met_Color)
c$Met_Color[1:10,]
c$Met_Color
d$Met_Color
is.factor(c$Met_Color)
corollasLM = lm( formula = Price ~ ., data = c )
summary(corollasLM)
unique(c$Fuel_Type)
unique(trim(c$Fuel_Type))
unique(rtrim(c$Fuel_Type))
trimws(c$Fuel_Type)
unique(trimws(c$Fuel_Type))
unique(trimws(c$Model))
trimws(c$Model)
count(trimws(c$Model))
trim <- function( x ) {
gsub("(^[[:space:]]+|[[:space:]]+$)", "", x)
}
trimws(d$Model)
unique(trimws(d$Model))
trim(d$Model)
unique(trim(d$Model))
is.factor(corollas2$Fuel_Type)
unique(c$Fuel_Type)
levels(c$Fuel_Type)
levels(c$Fuel_Type)[1]
summary(corollasLM)
mean(d$Price)
plot(x,y)
plot(x,y,type="l",main="x-y",xlab = "x_axis",ylab="y_axis")
plot(x,y,type="l",main="x-y",xlab = "x_axis",ylab="y_axis",xlim=c(-10,20))
getwd()
read.csv("titanic.csv")
titanic =read.csv("titanic.csv", header=TRUE)
