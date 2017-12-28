#Retail project (Using random forest for the same)
#Classification problem

# 1. ENVIRONMENT SETUP

#Loading the required libraries for the project
library(randomForest) #For using random forest related functions
library(dplyr) #for data preparation activities


#Setting the working directory
setwd("C:/Users/chakrabortyab/Desktop/R Practice/Data")

#Loading darta from file
retail_data=read.csv("store_train.csv")

#Viewing the data (Optional)
View(retail_data)


# 2. DATA PREPARATION

#Checking the data type of the target variable
class(retail_data$store)
#We need to convert the target variable to factor for the random forest to work
retail_data_Clean=retail_data %>% mutate(store=as.factor(store))
#Checking the data type again to verify
class(retail_data_Clean$store)

#Checking if any columns contain NAs
apply(retail_data,2,function(x) sum(is.na(x)))
#Thus by running the above query we can conclude that there is one NA value for our column 

#removing the NA field from pupulation
retail_data_Clean=retail_data_Clean %>% na.omit()

#Verifying the removal of all NAs fro mour code
apply(retail_data_Clean,2,function(x) sum(is.na(x)))

#Merging all the sales data into one column sales
retail_data_Clean=retail_data_Clean %>% mutate(sales_total=sales0+sales1+sales2+sales3+sales4) %>% select(-sales0,-sales1,-sales2,-sales3,-sales4)
                                    
#Taking a glimpse of our data set
glimpse(retail_data_Clean)


# 3. SPLITTING THE DATA INTO TRAIN AND TEST

set.seed(1)
retail_clnData=sample(1:nrow(retail_data_Clean),0.75*nrow(retail_data_Clean)) 
Retail_train=retail_data_Clean[train,]
Retail_test=retail_data_Clean[-train,]


# 4. TRAINING THE RANDOM FOREST EQN.
#We are removing the location related variables as we do not usually include them as a best practice measure
class_rf=randomForest(store~.-countyname-storecode-Areaname-countytownname-state_alpha-store_Type,data=Retail_train,do.trace=T)


#Visualising the random forest
class_rf

# 5. APPLYING RANDOM FOREST TO GET THE SCORE OR PROBABILITY

Retail_train$score=predict(class_rf,newdata= Retail_train, type="prob")[,1]

cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)#Initialising the data frame with the first row
cutoffs=round(seq(0,1,length=100),3)#Setting up the probability values that the loop will iterate through


#Running the loop and filling up the data frame
for (cutoff in cutoffs){
  predicted=as.numeric(Retail_train$score>cutoff)
  
  TP=sum(predicted==1 & Retail_train$store==1)
  FP=sum(predicted==1 & Retail_train$store==0)
  FN=sum(predicted==0 & Retail_train$store==1)
  TN=sum(predicted==0 & Retail_train$store==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

#Viewing the cutoff data
View(cutoff_data)

#Removing the first row in the data frame as it was only used for initilisation
cutoff_data=cutoff_data[-1,]


#We are now creating all the matrix like sensitivity, specificity, accuracy, distance,
#lift, KS, user defined matrix etc
cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP,Sn=TP/P, Sp=TN/N,
         dist=sqrt((1-Sn)**2+(1-Sp)**2)
         ) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)


View(cutoff_data)


#Now we are converting the wide format into long format so that we can plot those details
library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M) #here gather is used for the conversion from wide to long format



library(ggplot2)
#Plotting the graph
ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()




#Now, since we decided we are going to get the cutoff with max KS, WE do the following

KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]
KS_cutoff




library(pROC)

roccurve=roc(treaddata_train$Revenue.Grid,treaddata_train$score)

#plotting the ROC curver
plot(roccurve)

#Area under the curver
auc(roccurve)


# 6. SEEING IMPORTANCE
abc=importance(class_rf)
abc[order(abc[,1],decreasing=T),]

#Plotting the chart for the same
varImpPlot(class_rf)

