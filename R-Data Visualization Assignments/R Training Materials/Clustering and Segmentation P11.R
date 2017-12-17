# This portion deals with the clustering and segmentation portion of the data science specialisation course.
# We are using the data set mtcars here.

#Author: Abhinaba Chakraborty
#last Updated: 17.12.2017

data(mtcars)
df_cars=mtcars

#Hierarchical clustering


# 1. STANDARDISING THE DATASET

#Here we are standardising with median and median absolute deviation instead of (mean and standard deviation)
library(dplyr)

#Calculating the medians of all the columns
medians=apply(df_cars,2,median)

#Calculating the median absolute deviation
mads=apply(df_cars,2,mad) # mad=median(abs(xi-median(x)))

#Re-scaling the dataset
cars.std=data.frame(scale(df_cars,center=medians,scale = mads))
#In case we would have used mean and standard deviation, we would have written the above eqn., in the following
#way, as by default mean and standard deviation will be passed if we are not passing any value
#cars.std=data.frame(scale(df_cars))  

head(cars.std)
#Now, we found out that columns vs and am are having NAN and INF fields in them, so we may choose to remove those 
#columns, however here vs and am are alredy dummy datas, so we can exclude them from the standardisation process
#So we do the exclude them from ur dataset
df_cars2=mtcars[,c(1:7,10,11)]

#Calculating the medians of all the columns again (excluding vs and am)
medians=apply(df_cars2,2,median)

#Calculating the median absolute deviation again (excluding vs and am)
mads=apply(df_cars2,2,mad) # mad=median(abs(xi-median(x)))

#Re-scaling the dataset again (excluding vs and am)
cars.std=data.frame(scale(df_cars2,center=medians,scale = mads))

head(cars.std)

#Since the standardisation is done now, we are adding our removed columns back to our data set
cars.std=cbind.data.frame(cars.std,df_cars[,c(8,9)])

head(cars.std)


# 2. CALCULATING THE EUCLIDEAN DISTANCE
cars.dist=dist(cars.std)

cars.dist


# 3. PASSING THE DISTANCE MATRIX TO HIERARCHICAL CLUSTERING

cars.hclust=hclust(cars.dist)
#By Default Complete linkage is used for clustering our data

#In case we wish to use Single or average linkage, we do the following
#cars.hclust=hclust(cars.dist,method="single") OR cars.hclust=hclust(cars.dist,method="average")

#Plotting the dendogram
plot(cars.hclust,labels=rownames(df_cars),main="Default from hclust",col="black")

#If we wish to cut the dendogram for 2 clusters, we us cutree, it will help us to break our dendogram into two 
#clusters
groups.2=cutree(cars.hclust,2)

#Looking at the cars in diff. clusters
groups.2

#Analysing the clusters
table(groups.2)

#Now we are trying to compare two clusters based on their mean
apply(df_cars,2,function(x) tapply(x,groups.2,mean))

#Validating our cluster (We would need a library named cluster)
library(cluster)

#Validating our cluster with silhoutee plot
diss=daisy(cars.std) #We are passing the standardised data fr our car data set
sk=silhouette(groups.2,diss)
plot(sk)


#If we wish to change the cluster to which any records belongs, we can do the following
cars.std$cluster=groups.2
cars.std$cluster[1]=2
View(cars.std)


#K-means clustering
#Here we are randomly creating a data set
n=100
g=6
set.seed(g)
d = data.frame(
    x=unlist(lapply(1:g,function(i) rnorm(n/g, runif(1)*i^2))),
    y=unlist(lapply(1:g,function(i) rnorm(n/g, runif(1)*i^2)))
)
plot(d,col="mediumturquoise",pch=16,
     xlab="Arrival Time Deviations",
     ylab="Departure Time Deviations",
     main="Scatter plot: Delays from schedule in Minutes"
)


#Demonstrating K-means
mydata=d

#Determine the optimal cluster size based on within sum of square
#within sum of square is the degrees of freedom * variance of the dataset
wss=(nrow(mydata)-1)*sum(apply(mydata,2,var))

#Now we are doing the same thing as we did above, but with k-means
#Here the i in the loop denotes the number of clusters and we iterate through multiple combos (2 to 5 clusters)
#so as to determine the k-means value for 2 to 15 number of clusters (the loop starts at 2 because the minimum 
#number of clusters can be 2) 
for (i in 2:15) wss[i]=sum(kmeans(mydata,centers = i)$withinss)

#The above helps us to determine; what is the optimal number of clusters where the wss value is low and after which
#there will be no change of variance among the values of the cluster
#So, we are plotting an elbow chart/scree plot to determine the optimal no. of cluster
plot(1:15,wss,type="b",xlab="Number of clusters",ylab="within groups sum of squares",col="mediumseagreen",pch=12)

 
#So from the elbow chart we concluded that we can have 4 number of clusters for our dataset
fit=kmeans(mydata,4)

#Adding the calculated cluster to the original data set d
d$cluster=fit$cluster

library(ggplot2)
ggplot(d,aes(x=cluster))+geom_bar(aes(fill=cluster))


#Taking a real time data set to test k-means
