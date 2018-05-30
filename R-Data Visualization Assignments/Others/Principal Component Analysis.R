#Setting the Directory
setwd('C:\\Users\\Abhinaba\\Desktop\\Hackathons\\Understanding PCA with R- Big Mart Sales Practice Problem')

#load train and test file
train = read.csv("train_Big.csv")
test = read.csv("test_Big.csv")

#Viewing the same
View(train)
View(test)

#add a column (so that the test data set becomes same as the train data set for this scenario)
test$Item_Outlet_Sales = 1

#combine the train and test data set
combi <- rbind(train, test)
#Viewing the same
View(combi)

#impute missing values with median for the Item_Weight column
combi$Item_Weight[is.na(combi$Item_Weight)] = median(combi$Item_Weight, na.rm = TRUE)


#impute 0 with median for the Item_Visibility column
combi$Item_Visibility = ifelse(combi$Item_Visibility == 0, median(combi$Item_Visibility),combi$Item_Visibility)

#We are trying to find mode and impute the missing/blank values for the column Outlet_Size
#Checking out the missing values
table(combi$Outlet_Size, combi$Outlet_Type)

#Imputing the missing catergory/level for Outlet_Size with other
levels(combi$Outlet_Size)[1]="Other"


#remove the dependent and identifier variables (Since these variables are not necessary to us now)
my_data = subset(combi, select = -c(Item_Outlet_Sales, Item_Identifier,Outlet_Identifier))

#check available variables
colnames(my_data)

#Since PCA works on numeric variables, let's see if we have any variable other than numeric.
str(my_data)
#so, 6 out of 9 variables are categorical in nature, we need to do some sort of transforamtion for or PCA
#algorithm to work. 


#Doing the data preparation for the same
library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c("Item_Fat_Content","Item_Type",
                                                     "Outlet_Establishment_Year","Outlet_Size",
                                                     "Outlet_Location_Type","Outlet_Type"))

#To check, if we now have a data set of integer values, simple write:

#check the data set
str(new_my_data)

#And, we now have all the numerical values. Let's divide the data into test and train.

#divide the new data
pca.train <- new_my_data[1:nrow(train),]
pca.test <- new_my_data[-(1:nrow(train)),]


#We can now go ahead with PCA.

#principal component analysis
#Scaling the values or doing standardisation
prin_comp <- prcomp(pca.train, scale. = T)
names(prin_comp)


#The prcomp() function results in 5 useful measures:

#1. center and scale refers to respective mean and standard deviation of the variables 
#that are used for normalization prior to implementing PCA

#outputs the mean of variables
prin_comp$center

#outputs the standard deviation of variables
prin_comp$scale

#2. The rotation measure provides the principal component loading. Each column of rotation
#matrix contains the principal component loading vector. This is the most important measure we should be interested in.

prin_comp$rotation

#This returns 44 principal components loadings. Is that correct ? Absolutely. In a data set, the maximum number of principal
#component loadings is a minimum of (n-1, p).#44 components are returned because the same is the total number of cols/features 
#after dummification 

#Let's look at first 4 principal components and first 5 rows.

prin_comp$rotation[1:5,1:4]

#3. In order to compute the principal component score vector, we don't need to multiply the loading with data. Rather, the 
#matrix x has the principal component score vectors in a 8523 × 44 dimension.

dim(prin_comp$x)

#Let's plot the resultant principal components.

biplot(prin_comp, scale = 0)

#compute standard deviation of each principal component
std_dev = prin_comp$sdev

#compute variance
pr_var = std_dev^2

#check variance of first 10 components
pr_var[1:10]


#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20] #Seeing for the first 20 components


#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")


#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")


#We are now doing the prediction on the same
#add a training set with principal components
train.data = data.frame(Item_Outlet_Sales = train$Item_Outlet_Sales, prin_comp$x)

#we are interested in first 30 PCAs
train.data = train.data[,1:31]

#run a decision tree
library(rpart)
rpart.model = rpart(Item_Outlet_Sales ~ .,data = train.data, method = "anova")
rpart.model

#transform test into PCA
test.data = predict(prin_comp, newdata = pca.test)
test.data = as.data.frame(test.data)

#select the first 30 components
test.data = test.data[,1:30]

#make prediction on test data
rpart.prediction = predict(rpart.model, test.data)
