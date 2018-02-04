#This is the second attempt at the retail project part 2 problem: We are using Logistic regression here instead
#of random forest
#AUTHOR: Abhinaba Chakraborty
#LAST MODIFIED: 14th January 2018


# 1. ENVIRONMENT SETUP

#Loading the required libraries for the project
library(dplyr) #used for functions like mutate, select, filter, arrange, summarise etc
library(car) #contains the vif function that removes multicollinearity 
library(tidyr) #contains the gather function that is used to convert wide format to long format 
library(ggplot2) #for graphical representation of data
library(pROC) #for drawing the area under the curve


#Setting the working directory
setwd("C:/Users/Abhinaba/Desktop/Edvancer Materials/Project/R Projects/Retail-Copy")

#Loading data from file
retail_data=read.csv("store_train.csv")

#Viewing the data (Optional)
View(retail_data)

#Taking a glimpse at the dataset
glimpse(retail_data)

#Creating a copy of the data set where we would be doing the data pre. activities
retail_data_clean=retail_data

#----------------------------------------------------------------------------------------------------

# 2. DATA PREPARATION

#Making a common variables sales and deleting the 5 sales values
retail_data_clean = retail_data_clean %>% mutate(sales=sales0+sales1+sales2+sales3+sales4) %>% 
  select(-sales0,-sales1,-sales2,-sales3,-sales4)

#Taking a glimpse at the modified data set
glimpse(retail_data_clean)

#Checking the boxplot of the same to check for the outliers
boxplot(retail_data_clean$sales)

#Checking the upper and lower bound values
min(retail_data_clean$sales)
max(retail_data_clean$sales)

summary(retail_data_clean$sales)
#Q1: 3422
#Q3: 4969
#Min: 2173
#Max: 11140

#Finding the Inter quantile range (Q3-Q1)
IQR_Totalsales=IQR(retail_data_clean$sales)

#Deciding the lower bound and upper bound values
low_bound=(3422-(1.5*IQR_Totalsales))
upp_bound=(4969+(1.5*IQR_Totalsales))

#Filters having values within the decided range
retail_data_clean=retail_data_clean %>% filter(sales>=low_bound & sales<=upp_bound)

#Viewing the filter retail data
View(retail_data_clean)

#Count of rows in the filtered data set
nrow(retail_data_clean)

#Total rows in the data set
nrow(retail_data)

#Rows filtered out
nrow(retail_data)-nrow(retail_data_clean)

#lets delete the countyname,storecode, Areaname, countytownname, state_alpha specific colums as 
#location specific details are very difficult to group and work with
retail_data_clean = retail_data_clean %>%  select(-countyname,-storecode,-Areaname,-countytownname,-state_alpha)

#Taking a glimpse
glimpse(retail_data_clean)


#Converting the store type variable
unique(retail_data_clean$store_Type)

retail_data_clean=retail_data_clean %>% mutate(str_typ_1=as.numeric(store_Type=="Supermarket Type1"),
                                             str_typ_2=as.numeric(store_Type=="Supermarket Type2"),
                                             str_typ_3=as.numeric(store_Type=="Supermarket Type3")) %>% select(-store_Type)


glimpse(retail_data_clean)


# Deleting the ID field as its not required for data interpretation
retail_data_clean=retail_data_clean %>% select(-Id)


glimpse(retail_data_clean)


#Checking if any columns contain NAs
apply(retail_data_clean,2,function(x) sum(is.na(x)))

#removing the NA field from column population as it may give an error
retail_data_clean=retail_data_clean %>% na.omit()


#Splitting the data into train and test

set.seed(2)
s=sample(1:nrow(retail_data_clean),0.7*nrow(retail_data_clean))
retail_data_clean_train=retail_data_clean[s,]
retail_data_clean_test=retail_data_clean[-s,]
#Viewing the train data set 
View(retail_data_clean_train)
#Takinga closer look at the summary of the train data set
summary(retail_data_clean_train)

#----------------------------------------------------------------------------------------------------

# 3. REMOVING MULTICOLLINEARITY

for_vif=lm(store~.,data=retail_data_clean_train)
t=vif(for_vif)
#Finding the variables with highest vif values
sort(t,decreasing = T)
#There are no variables with a vif more than 5.0, 

#----------------------------------------------------------------------------------------------------

# 4. RUNNING THE LOGISTIC REGRESSION IN ORDER TO IDENTIFY AND ISOLATE THE INSIGNIFICANT VARIABLES

fit=glm(store~.,family="binomial",data=retail_data_clean_train)

#Checking the summary of fit
summary(fit)

#We are following a step wise approach for removal of the unsignificant variables/variables with higher 
#probability of adhereing to the null hypothesis
fit=step(fit)

# we get the final set of values for Best possible combination
formula(fit)


#Best possible iteration values
# Step:  AIC=2789.95
# eqn.: store ~ population + sales
fit1=glm(store ~ population + sales, data=retail_data_clean_train, family="binomial")

summary(fit1)

#----------------------------------------------------------------------------------------------------

# 5. PREDICTING THE VALUES FOR PROBALITIES 

retail_data_clean_train$score=predict(fit1,newdata=retail_data_clean_train,type = "response")


View(retail_data_clean_train)


#Viewing the probability data along with other factors
View(retail_data_clean_train[,c('store','score')])


#----------------------------------------------------------------------------------------------------

# 6. CREATING THE CONNFUSION MATRIX
#Creating the confusion matrix along with other matrices like sensitivity, specificity, accuracy etc.
#let's just randomly assign a probability cutoff of 0.2
cutoff=0.2

predicted=as.numeric(retail_data_clean_train$score>cutoff)

#Calculating the confusion matrix alues
TP=sum(predicted==1 & retail_data_clean_train$store==1)
FP=sum(predicted==1 & retail_data_clean_train$store==0)
FN=sum(predicted==0 & retail_data_clean_train$store==1)
TN=sum(predicted==0 & retail_data_clean_train$store==0)


#Calculating the total number of real positives and real negatives (P and N)
P=TP+FN
N=TN+FP

# total number of observations
total=P+N


#Now, we run a loop of cutoff values and calculate TP,FP,FN and TN for each iteration
#So, basically the step that we did above for a probability value of 0.2, we are doing
#it again and again for all combinations
cutoff_data=data.frame(cutoff=0,TP=0,FP=0,FN=0,TN=0)#Initialising the data frame with the first row
cutoffs=round(seq(0,1,length=100),3)#Setting up the probability values that the loop will iterate through
#Rounded off to 3 decimal places

#Running the loop and filling up the data frame
for (cutoff in cutoffs){
  predicted=as.numeric(retail_data_clean_train$score>cutoff)
  
  TP=sum(predicted==1 & retail_data_clean_train$store==1)
  FP=sum(predicted==1 & retail_data_clean_train$store==0)
  FN=sum(predicted==0 & retail_data_clean_train$store==1)
  TN=sum(predicted==0 & retail_data_clean_train$store==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

View(cutoff_data)

#Removing the first row in the data frame as it was only used for initilisation
cutoff_data=cutoff_data[-1,]

#We are now creating all the matrix like sensitivity, specificity, accuracy, distance,
#lift, KS, user defined matrix etc
cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,dist=sqrt((1-Sn)**2+(1-Sp)**2),P=FN+TP,N=TN+FP) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  mutate(Accuracy=(TP+TN)/(P+N)) %>%
  mutate(Lift=(TP/P)/((TP+FP)/(P+N))) %>%
  mutate(M=(8*FN+2*FP)/(P+N)) %>%
  select(-P,-N)


View(cutoff_data)


cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M) #here gather is used for the conversion from wide to long format


View(cutoff_viz)


#Plotting the graph
ggplot(filter(cutoff_viz,Criterion!="Lift"),aes(x=cutoff,y=Value,color=Criterion))+
  geom_line()


#We can visualise the lift seperately because of its scale
cutoff_viz %>%
  filter(Criterion=="Lift") %>%
  ggplot(aes(x=cutoff,y=Value,color=Criterion))+geom_line()


#Now, since we decided we are going to get the cutoff with max KS, WE do the following

KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]
KS_cutoff

#So, our cutoff becomes 0.384

#Drawing the ROC or Receiver Operating Characteristics curve for train data

roccurve=roc(retail_data_clean_train$store,retail_data_clean_train$score)

#plotting the ROC curver
plot(roccurve)

#Area under the curver
auc(roccurve)

#Area under the curve for our train data set becomes 0.7096

#----------------------------------------------------------------------------------------------------
# 7. TESTING THE MODEL ON OUR SAMPLE TEST DATA 

retail_data_clean_test$score=predict(fit1,newdata = retail_data_clean_test,type = "response")

#View the data
View(retail_data_clean_test)

#Preparing the Confusion matrix for the test data
table(retail_data_clean_test$store,as.numeric(retail_data_clean_test$score>KS_cutoff))
#KS cutoff is calculated from the model

#From above, We find the fllowing
TP_test=299
FN_test=145
TN_test=382
FP_test=176


#Calculating other matrix
Sensitivity_test=(TP_test/(TP_test+FN_test))
Specificity_test=(TN_test/(FP_test+TN_test))
Precision_test=(TP_test/(TP_test+FP_test))
Accuracy_test=((TP_test+TN_test)/(TP_test+TN_test+FN_test+FP_test))
KS=abs(((TP_test)/(TP_test+FN_test))-((FP_test)/(FP_test+TN_test)))


#Sensitivity: 0.67
#Specificity: 0.68
#Precision: 0.62
#Accuracy: 0.67
#KS: 0.35

#Drawing the ROC or Receiver Operating Characteristics curve for test data

roccurve_test=roc(retail_data_clean_test$store,retail_data_clean_test$score)

#plotting the ROC curver
plot(roccurve_test)

#Area under the curver
auc(roccurve_test)

#Area under the curve for our test data set becomes 0.7285

#----------------------------------------------------------------------------------------------------

# 8. TESTING OUR MODEL ON THE ORIGINAL TEST DATA

retail_data_test_DS=read.csv("store_test.csv")

retail_data_test_DS_Orig=retail_data_test_DS


#performing the basic data prep
retail_data_test_DS=retail_data_test_DS  %>% mutate(sales=sales0+sales1+sales2+sales3+sales4) %>% 
  select(-sales0,-sales1,-sales2,-sales3,-sales4)

retail_data_test_DS = retail_data_test_DS %>%  select(-countyname,-storecode,-Areaname,-countytownname,-state_alpha)

retail_data_test_DS=retail_data_test_DS %>% mutate(str_typ_1=as.numeric(store_Type=="Supermarket Type1"),
                                               str_typ_2=as.numeric(store_Type=="Supermarket Type2"),
                                               str_typ_3=as.numeric(store_Type=="Supermarket Type3")) %>% select(-store_Type)

retail_data_test_DS=retail_data_test_DS %>% select(-Id)


glimpse(retail_data_test_DS)


#Running the logistic regression on the dataset
retail_data_test_DS$score=predict(fit1,newdata = retail_data_test_DS,type = "response")

#Now, our cutoff is 0.101, therefore
retail_data_test_DS=retail_data_test_DS %>% mutate(prediction=ifelse(score>0.384,1,0))

#Viewing the same
View(retail_data_test_DS)


#Appending the Score and prediction column with the original data set
retail_data_test_DS_Orig$score=retail_data_test_DS$score
retail_data_test_DS_Orig$prediction=retail_data_test_DS$prediction


#stroring the result in an object store
store=retail_data_test_DS_Orig$score
#Calculating the average probability score so as to assign the missing values
avg_result=round(mean(store,na.rm = TRUE),3)
#Creating a new data frame to write the resut
dataframe_result_final=data.frame(store)
#Replacing the NA values in the data frame with the average score/probability
dataframe_result_final$store[which(is.na(dataframe_result_final$store))]=avg_result

#Viewing the files
View(retail_data_test_DS_Orig)
#Viewing the data set that has to be written
View(dataframe_result_final)


#Writing it to the directory
write.csv(dataframe_result_final,"Abhinaba_Chakraborty_P2_part2.csv",row.names=FALSE)
