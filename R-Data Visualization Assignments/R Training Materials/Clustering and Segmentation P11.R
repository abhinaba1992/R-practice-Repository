# This portion deals with the clustering and segmentation portion of the data science specialisation course.
# We are using the data set mtcars here.

#Author: Abhinaba Chakraborty
#last Updated: 12.12.2017

data(mtcars)
df_cars=mtcars

# 1. STANDARDISING THE DATASET

#Here we are standardising with median and median absolute deviation instead of (mean/standard deviation)
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
#So we find out that complete linkage is used for clustering our data

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
