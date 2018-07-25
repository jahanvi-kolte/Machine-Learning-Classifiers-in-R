#Lab 5b
#jsk362


#class (p- poisionous, e-edible)

install.packages("e1071")
library(e1071)

mushroom <- read.csv("mushroom.csv", header=T, na.strings = '')
View(mushroom)


#hamming distance for categorical data instead of numerical distance

distm <- as.dist(hamming.distance(as.matrix(mushroom)[, 2:14]))
hc.complete <- hclust(distm, method="complete")

#hc cut 
#col - 8 group 
plot(hc.complete, main="Complete Linkage", cex=.9)
hc.cut <- cutree(hc.complete, 8)

table(mushroom[,1], hc.cut)

#cut 2
hc.cut <- cutree(hc.complete, 2)
table(mushroom[,1], hc.cut)

#Categorical variables to dummy variables, 
#using Euclidean distance based complete linkage 
sapply(mushroom,class)
sapply(mushroom,levels)


mushroom$cap.shapef <- model.matrix( ~ cap.shape, data=mushroom )[, "cap.shapef"]
mushroom$cap.shapes <- model.matrix( ~ cap.shape, data=mushroom )[, "cap.shapes"]
mushroom$cap.shapex <- model.matrix( ~ cap.shape, data=mushroom )[, "cap.shapex"]
levels(mushroom$cap.surface)
mushroom$cap.surfaces <- model.matrix( ~ cap.surface, data=mushroom )[, "cap.surfaces"]
mushroom$cap.surfacey <- model.matrix( ~ cap.surface, data=mushroom )[, "cap.surfacey"]
levels(mushroom$cap.color)
mushroom$cap.colorn <- model.matrix( ~ cap.color, data=mushroom )[, "cap.colorn"]
mushroom$cap.colorw <- model.matrix( ~ cap.color, data=mushroom )[, "cap.colorw"]
mushroom$cap.colory <- model.matrix( ~ cap.color, data=mushroom )[, "cap.colory"]
levels(mushroom$bruise)
mushroom$bruiset <- model.matrix( ~ bruise, data=mushroom )[, "bruiset"]
levels(mushroom$ordor)
mushroom$ordorl <- model.matrix( ~ ordor, data=mushroom )[, "ordorl"]
mushroom$ordorn <- model.matrix( ~ ordor, data=mushroom )[, "ordorn"]
mushroom$ordorp <- model.matrix( ~ ordor, data=mushroom )[, "ordorp"]

mushroom$gill.spacingw <- model.matrix(~gill.spacing, data=mushroom)[,"gill.spacingw"]


mushroom$gill.sizen <- model.matrix(~gill.size, data=mushroom)[,"gill.sizen"]


mushroom$gill.colorh <- model.matrix(~gill.color, data=mushroom)[,"gill.colorh"]
mushroom$gill.colork <- model.matrix(~gill.color, data=mushroom)[,"gill.colork"]
mushroom$gill.colorn <- model.matrix(~gill.color, data=mushroom)[,"gill.colorn"]
mushroom$gill.colorp <- model.matrix(~gill.color, data=mushroom)[,"gill.colorp"]
mushroom$gill.colorw <- model.matrix(~gill.color, data=mushroom)[,"gill.colorw"]


mushroom$stalk.shapet <- model.matrix(~stalk.shape, data=mushroom)[,"stalk.shapet"]


mushroom$stalk.rootc <- model.matrix(~stalk.root, data=mushroom)[,"stalk.rootc"]
mushroom$stalk.roote <- model.matrix(~stalk.root, data=mushroom)[,"stalk.roote"]
mushroom$stalk.rootr <- model.matrix(~stalk.root, data=mushroom)[,"stalk.rootr"]


mushroom$ring.typep <- model.matrix(~ring.type, data=mushroom)[,"ring.typep"]


mushroom$populationn <- model.matrix(~population, data=mushroom)[,"populationn"]
mushroom$populations <- model.matrix(~population, data=mushroom)[,"populations"]
mushroom$populationv <- model.matrix(~population, data=mushroom)[,"populationv"]
mushroom$populationy <- model.matrix(~population, data=mushroom)[,"populationy"]


mushroom$habitatg <- model.matrix(~habitat, data=mushroom)[,"habitatg"]
mushroom$habitatm <- model.matrix(~habitat, data=mushroom)[,"habitatm"]
mushroom$habitatp <- model.matrix(~habitat, data=mushroom)[,"habitatp"]
mushroom$habitatu <- model.matrix(~habitat, data=mushroom)[,"habitatu"]

#remove original variables
mushroom <- mushroom[, !colnames(mushroom) %in% 
                       c("cap.shape","cap.surface","cap.color",
                         "bruise"    ,    "ordor"    ,     "gill.spacing" , "gill.size",
                         "gill.color"  ,  "stalk.shape",   "stalk.root"  ,  "ring.type" ,
                         "population" ,   "habitat" )]

dim(mushroom)
names(mushroom)

#no scaling as all are dummy variables
hc.complete=hclust(dist(mushroom[,-1]), method="complete")
hc.cut <- cutree(hc.complete, 8)

table(mushroom[,1], hc.cut)


#or 
dat = model.matrix(~., data=mushroom[,2:14])[,-1]
distm = as.dist(hamming.distance(dat))
hc.complete=hclust(distm, method="complete")
hc.cut <- cutree(hc.complete, 8)

table(mushroom[,1], hc.cut)
#unlist - you cannot compare ordering to some other sort of unordering


#pca
names(iris)
View(iris)

apply(iris[, -5], 2, mean)
apply(iris[, -5], 2, var)

#if variance is different for columns, pca driven by max variance

#Scale and standardize 
#pca takes care of this , scale=TRUE
pr.out=prcomp(iris[, -5], scale=TRUE)
names(pr.out)


biplot(pr.out,scale=0)
#rank defiecient
#some col can be linearly dependent
#whole space can be degenerated space will be of lower dim
#so number of vectors needed will be less

pr.var=pr.out$sdev^2
pve=pr.var/sum(pr.var)
#73% by pc1
cumsum(pve)
#almost all var can be explained by first 2 pc

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,type='b')

