#Retail project (Using random forest for the same)
#Classification problem
#AUTHOR: Abhinaba Chakraborty
#LAST MODIFIED: 29th Dec 2017

# 1. ENVIRONMENT SETUP

#Loading the required libraries for the project
library(randomForest) #For using random forest related functions
library(dplyr) #for data preparation activities
library(tidyr) #for converting data from wide format to long format
library(ggplot2) #for data visualisations
library(pROC) #To draw an area under the curve


#Setting the working directory
#setwd("C:/Users/chakrabortyab/Desktop/R Practice/Data")
setwd("C:/Users/Abhinaba/Desktop/Edvancer Materials/Project/R Projects/Retail-Copy")

#Loading data from file
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


#Removing the location related variables
retail_data_Clean=retail_data_Clean %>% select(-countyname,-storecode,-Areaname,-countytownname,
                                               -state_alpha,-store_Type)

#Taking a glimpse of our data set
glimpse(retail_data_Clean)



# 3. SPLITTING THE DATA INTO TRAIN AND TEST

set.seed(1)
retail_clnData=sample(1:nrow(retail_data_Clean),0.75*nrow(retail_data_Clean)) 
Retail_train=retail_data_Clean[retail_clnData,]
Retail_test=retail_data_Clean[-retail_clnData,]


# 4. TRAINING THE RANDOM FOREST EQN.
#We are removing the location related variables as we do not usually include them as a best practice measure
class_rf=randomForest(store~.,data=Retail_train,do.trace=T)


#Visualising the random forest
class_rf

# 5. APPLYING RANDOM FOREST TO GET THE SCORE OR PROBABILITY

Retail_train$score=predict(class_rf,newdata= Retail_train, type="prob")[,1]

# 6. FINDING THE MATRICES FOR VALIDATION
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)#Initialising the data frame with the first row
cutoffs=round(seq(0,1,length=100),3)#Setting up the probability values that the loop will iterate through

#Running the loop and filling up the data frame
for (cutoff in cutoffs){
  predicted=as.numeric(Retail_train$score>cutoff)
  
  TP=sum(Retail_train$store==1 & predicted==1)
  FP=sum(Retail_train$store==0 & predicted==1)
  FN=sum(Retail_train$store==1 & predicted==0)
  TN=sum(Retail_train$store==0 & predicted==0)
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
  select(-P,-N)

#Viewwing the cutoff data
View(cutoff_data)


#Now we are converting the wide format into long format so that we can plot those details

cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift) %>%
  gather(Criterion,Value,Sn:Lift) #here gather is used for the conversion from wide to long format


#Plotting the graph
ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+geom_line()


#Now, since we decided we are going to get the cutoff with max KS, WE do the following

KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]
KS_cutoff
#From above we get the cutoff is 0.525


# 7. DRAWING THE ROC CURVE
roccurve=roc(Retail_train$store,Retail_train$score)

#plotting the ROC curve
plot(roccurve)

#Area under the curver
auc(roccurve) #We get a 99% AUC value for our data


# 8. SEEING IMPORTANCE TO SEE THE VARIABE IMPORTANCE
Retail_data_imp=importance(class_rf)
Retail_data_imp[order(Retail_data_imp[,1],decreasing=T),]

#Plotting the chart for the same
varImpPlot(class_rf)


# 9. PREDICTING THE VALUE FOR TEST DATA
Retail_test$score=predict(class_rf,newdata= Retail_test, type="prob")[,1]

#Visualising the confusion matrix for the test data
table(Retail_test$store,as.numeric(Retail_test$score>KS_cutoff))

roccurve_test=roc(Retail_test$store,Retail_test$score)

#plotting the ROC curver
plot(roccurve_test)

#Area under the curver
auc(roccurve_test) #We get a 81% AUC value for our sample test data


# 9. TESTING UPON THE ORIGINAL TEST DATA
retail_data_test_org=read.csv("store_test.csv")

#Checking if any NAs are present in our original test dataset
apply(retail_data_test_org,2,function(x) sum(is.na(x)))

#removing the NA field from pupulation
retail_data_test_org_Clean=retail_data_test_org %>% na.omit()

#Verifying the removal of all NAs from our code
apply(retail_data_test_org_Clean,2,function(x) sum(is.na(x)))

#Having an original copy for the same
retail_data_test_org_Final=retail_data_test_org_Clean

#Removing the location oriented variables from the data set
retail_data_test_org_Clean=retail_data_test_org_Clean %>% select(-countyname,-storecode,-Areaname,
                                                                 -countytownname,-state_alpha,-store_Type)

#Checking the score
retail_data_test_org_Clean$score=predict(class_rf,newdata= retail_data_test_org_Clean, type="prob")[,1]

#Now, our cutoff is 0.525, therefore
retail_data_test_org_Clean=retail_data_test_org_Clean %>% mutate(prediction=ifelse(score>0.525,"1","0"))

#Appending the Score and prediction column with the original data set
retail_data_test_org_Clean$score=retail_data_test_org_Clean$score
retail_data_test_org_Clean$prediction=retail_data_test_org_Clean$prediction


#Writing it to the directory
write.csv(retail_data_test_org_Clean,"retail-full_test_with_Predicted_Values.csv")
