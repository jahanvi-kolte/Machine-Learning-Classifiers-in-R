#Lab 5a


### K-means

#syntactic data
#to avoid randomness in data generation / to replicate result
set.seed(101)

#random normal dist- rnorm, 100 data points , 2 dim
x=matrix(rnorm(100*2), 100,2)

#translate origin to separate data
xmean=matrix(rnorm(8,sd=4),4,2)

#randomly choose index
which=sample(1:4,100,replace=TRUE)

x=x+xmean[which,]

#color indiactes different classes
plot(x, col=which,pch=19)

#blackdot is closer to blue class ; we can see that algorithm can improve

set.seed(101)
#try 15 different initial configurations
km.out=kmeans(x,4,nstart=15)
km.out
plot(x, col=km.out$cluster, pch=1, cex=2,lwd=2)

table(km.out$cluster)
table(which)
#1,4,3,2 terminology
points(x, col=c(1,4,3,2)[which], pch=19)


#the more trials, the better

set.seed(101)
km.out=kmeans(x,4,nstart=1)
km.out$tot.withinss
set.seed(101)
km.out=kmeans(x,4,nstart=20)
km.out$tot.withinss

#Hierarchical clustering 
#distance based clustering technique
#Here, we need to specify method/linkage

hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

#Kind of balanced
plot(hc.complete,main="Complete Linkage", cex=.9)


#not that balanced
plot(hc.average, main="Average Linkage", cex=.9)
#not very balanced or very imbalanced at all
plot(hc.single, main="Single Linkage", cex=.9)


hc.cut=cutree(hc.complete,4)
hc.cut
#method to cut different tree is same

table(hc.cut)
table(which)
#4 for which is 1 for hc.cut

table(hc.cut,c(3,2,1,4)[which])

#Average linkage clustering results
hc.cut=cutree(hc.average,4)
hc.cut
table(hc.cut)
table(which)
#3214
table(hc.cut,c(3,2,1,4)[which])

plot(x, col=hc.cut, pch=1, cex=2,lwd=2, main="Average Linkage Clustering")
points(x, col=c(3,2,1,4)[which], pch=19)


#Co-relation high - means two vectors are closer - means distance should be smaller
#earlier corralation 1 or -1 in 2d
#we generated data in 2d
#prove matehematically
#if only 2dimensions, what is the correlation/calaculation


#as.dist - convert to distance matrox in R
#we only need lower traingle

#Differences - just use kmeaqns n specify classesToAM(
#  hc - we build tree, depending on t, we cut classes 
#)

#submit code for NCI
x3=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x3)))
plot(hclust(dd, method="complete"))

#NC160 dataset (gene-cancer info)

library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
#View(nci.data)

#standardize
sd.data=scale(nci.data)

#Form distance matrix
sd.data.dist <- dist(sd.data)

#Complete Linkage Hierarchical Clustering
hc.complete=hclust(sd.data.dist, method="complete")
hc.complete$labels <- nci.labs
plot(hc.complete, main="Complete Linkage for NC160",cex=.9)

#Average Linkage Hierarchical Clustering
hc.average=hclust(sd.data.dist, method="average")
hc.average$labels <- nci.labs
plot(hc.average,main="Average Linkage for NC160", cex=.9)

#Single Linkage Hierarchical Clustering
hc.single=hclust(sd.data.dist, method="single")
hc.single$labels <- nci.labs
plot(hc.single,main="Single Linkage for NC160", cex=.9)

#Cut complete linkage
hc.cut=cutree(hc.complete,5)
hc.cut

table(hc.cut,nci.labs)

#Kmeans on NC160
set.seed(3)
#try 15 different initial configurations
km.out=kmeans(sd.data,5,nstart=20)
km.out

table(km.out$cluster,hc.cut)
#change this

#Co-relation based complete linkage
data.dist <- as.dist(1-cor(t(sd.data)))
hc.complete.cor=hclust(data.dist, method="complete")
hc.cut.cor=cutree(hc.complete.cor,5)
table(hc.cut.cor,hc.cut)
table(hc.cut.cor,km.out$cluster)
#change
