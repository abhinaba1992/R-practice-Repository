#This is the second part of the banking project for the poruguese bank; where we need to prepare a model for the business
#based on past data soas to predict which customers are going togo for a term deposit basedon their charectaristics

#Author: Abhinaba Chakraborty
#Date: 9th December 2017

#Including all the packages we need
library(dplyr) #used for functions like mutate, select, filter, arrange, summarise etc
library(car) #contains the vif function that removes multicollinearity 
library(tidyr) #contains the gather function that is used to convert wide format to long format 
library(ggplot2) #for graphical representation of data
library(pROC) #for drawing the area under the curve

#Setting the workinng directory
setwd("C:/Users/Abhinaba/Desktop/Edvancer Materials/Project/R Projects/Banking-Copy")

#Loading the term deposit  data set
trmdepdata=read.csv("bank-full_train.csv")

#Viewing the dataset
glimpse(trmdepdata)

trmdepdata_clean=trmdepdata



#-----------------------------------------------------------------------------------------------------------
# 1. Data Preparation and variable treatment

#Variable : job

#Creating dummies for the Field Job; there are 12 levels of the variable 
#Viewing the count of categorical variables here
table(trmdepdata_clean$job)

#We have 12 categories; hence we need to do a grouping

#Seeing the percentages of job categories grouped by  yes/No 
round(prop.table(table(trmdepdata_clean$job,trmdepdata_clean$y),1),2)

#Grouping the job categories as following (have considered the yes side of the grouping)
#As per the n-1 rule, we are getting the below no. of categories, ignoring blue-collar
#that contributes to about 7% of yes
# 0.29: student
# 0.23: retired
# 0.15: unemployed
# 0.14: management
# 0.12: admin, self-employed, unknown
# 0.11: Technician
# 0.09: entrepreneur, services
# 0.08: housemaid

trmdepdata_clean=trmdepdata_clean %>% mutate(jb1=as.numeric(job=="student"),
                                             jb2=as.numeric(job=="retired"),
                                             jb3=as.numeric(job=="unemployed"),
                                             jb3=as.numeric(job=="management"),
                                             jb4=as.numeric(job %in% c("admin","self-employed","unknown")),
                                             jb5=as.numeric(job=="technician"),
                                             jb6=as.numeric(job %in% c("entrepreneur","services")),
                                             jb7=as.numeric(job=="housemaid")) %>% select(-job)


glimpse(trmdepdata_clean)

#---------------------------------------------------------------------
#Variable : marital status

table(trmdepdata_clean$marital)


#Ignoring the type divorced by virtue of the n-1 thumb rule and also least value rule
trmdepdata_clean=trmdepdata_clean %>% mutate(ms1=as.numeric(marital=="married"),
                                             ms2=as.numeric(marital=="single")) %>% select(-marital)


glimpse(trmdepdata_clean)

#---------------------------------------------------------------------
#Variable : education

table(trmdepdata_clean$education)

#Ignoring the type unknow by virtue of the n-1 thumb rule and also least value rule
trmdepdata_clean=trmdepdata_clean %>% mutate(ed1=as.numeric(education=="primary"),
                                             ed2=as.numeric(education=="secondary"),
                                             ed3=as.numeric(education=="tertiary")) %>% select(-education)



glimpse(trmdepdata_clean)

#---------------------------------------------------------------------
#Variable : contact

table(trmdepdata_clean$contact)

#Ignoring the type unknow by virtue of the n-1 thumb rule and also least value rule

trmdepdata_clean=trmdepdata_clean %>% mutate(cn1=as.numeric(contact=="cellular"),
                                             cn2=as.numeric(contact=="telephone")) %>% select(-contact)

glimpse(trmdepdata_clean)

#---------------------------------------------------------------------
#Variable : default
table(trmdepdata_clean$default)

#Ignoring the type yes by virtue of the n-1 thumb rule and also least value rule

trmdepdata_clean=trmdepdata_clean %>% mutate(defau_no=as.numeric(default=="no")) %>% select(-default)

glimpse(trmdepdata_clean)

#---------------------------------------------------------------------
#Variable : housing
table(trmdepdata_clean$housing)

#Ignoring the type no by virtue of the n-1 thumb rule and also least value rule

trmdepdata_clean=trmdepdata_clean %>% mutate(hous_yes=as.numeric(housing=="yes")) %>% select(-housing)

glimpse(trmdepdata_clean)

#---------------------------------------------------------------------
#Variable : loan
table(trmdepdata_clean$loan)

#Ignoring the type yes by virtue of the n-1 thumb rule and also least value rule

trmdepdata_clean=trmdepdata_clean %>% mutate(loan_no=as.numeric(loan=="no")) %>% select(-loan)

glimpse(trmdepdata_clean)

#---------------------------------------------------------------------
#Variable : poutcome
table(trmdepdata_clean$poutcome)

#Ignoring the type other by virtue of the n-1 thumb rule and self assumed business scenario

trmdepdata_clean=trmdepdata_clean %>% mutate(pot1=as.numeric(poutcome=="unknown"),
                                             pot2=as.numeric(poutcome=="failure"),
                                             pot3=as.numeric(poutcome=="success")) %>% select(-poutcome)

glimpse(trmdepdata_clean)

#---------------------------------------------------------------------
#Variable : y (Converting the dependent variable to binary)

trmdepdata_clean=trmdepdata_clean %>% mutate(bi_y=as.numeric(y=="yes")) %>% select(-y)

glimpse(trmdepdata_clean)
#---------------------------------------------------------------------
#Dropping the value month as it doesn't seem to have any significance (self-asumed)
trmdepdata_clean=trmdepdata_clean %>% select(-month)

glimpse(trmdepdata_clean)


#-----------------------------------------------------------------------------------------------------------

# 2. Splitting the data into train and test modules

#Sampling 70 % of the data
set.seed(2)
s=sample(1:nrow(trmdepdata_clean),0.7*nrow(trmdepdata_clean))
trmdepdata_clean_train=trmdepdata_clean[s,]
trmdepdata_clean_test=trmdepdata_clean[-s,]
View(trmdepdata_clean_train)
summary(trmdepdata_clean_train)


#-----------------------------------------------------------------------------------------------------------

# 3. Removing multicollinearity


#using the lm function for regression and removing the ID variable as its not necessary

for_vif=lm(bi_y~.-ID,data=trmdepdata_clean_train)
#Running the vif function
t=vif(for_vif)
#Finding the variables with highest vif values
sort(t,decreasing = T)
#Checking the summary of for_vif
summary(for_vif)


#Removing the variable pot1 since it has the highest vif value (which is above the value 5)
for_vif=lm(bi_y~.-ID-pot1,data=trmdepdata_clean_train)
t=vif(for_vif)
sort(t,decreasing = T)

#Based on same logic, removing the variable ed2 since it has the highest vif value
for_vif=lm(bi_y~.-ID-pot1-ed2,data=trmdepdata_clean_train)
t=vif(for_vif)
sort(t,decreasing = T)

#So all the VIF values above 5 are removed now

#-----------------------------------------------------------------------------------------------------------

# 4. Running the logistic regression post removal of all the unecessary variables that we found out during 
# vif resolution

#Removing the columns that have high vif values and/or unecessary
trmdepdata_clean_train_fit=trmdepdata_clean_train %>%
  select(-ID,-pot1,-ed2)
#
glimpse(trmdepdata_clean_train_fit)

#Running the logistic regression
fit=glm(bi_y~.,family="binomial",data=trmdepdata_clean_train_fit)


summary(fit)

#We are following a step wise approach for removal of the unsignificant variables/variables with higher probability
fit=step(fit)

# we get the final set of values for Best possible combination
formula(fit)
#Best possible iteration values
# Step:  AIC=11137.99
# bi_y ~ age + balance + duration + campaign + pdays + jb1 + jb2 + 
#   jb3 + jb7 + ms1 + ms2 + ed1 + ed3 + cn1 + cn2 + hous_yes + 
#   loan_no + pot3

#So now we use glm with the new combination (We are only including the variables we want)
fit1=glm(bi_y ~ age + balance + duration + campaign + pdays + jb1 + jb2 + 
  jb3 + jb7 + ms1 + ms2 + ed1 + ed3 + cn1 + cn2 + hous_yes + 
  loan_no + pot3, data=trmdepdata_clean_train, family="binomial")

summary(fit1)



#Now there might be still some variables with a pr value greater than 5 % or 0.05
#So we need to remove them one by one


#So, first we remove age
fit1=glm(bi_y ~ balance + duration + campaign + pdays + jb1 + jb2 + 
           jb3 + jb7 + ms1 + ms2 + ed1 + ed3 + cn1 + cn2 + hous_yes + 
           loan_no + pot3, data=trmdepdata_clean_train, family="binomial")

summary(fit1)

#With the same principle, we remove ms2
fit1=glm(bi_y ~ balance + duration + campaign + pdays + jb1 + jb2 + 
           jb3 + jb7 + ms1 + ed1 + ed3 + cn1 + cn2 + hous_yes + 
           loan_no + pot3, data=trmdepdata_clean_train, family="binomial")

summary(fit1)

#With the same principle, we remove jb7
fit1=glm(bi_y ~ balance + duration + campaign + pdays + jb1 + jb2 + 
           jb3 + ms1 + ed1 + ed3 + cn1 + cn2 + hous_yes + 
           loan_no + pot3, data=trmdepdata_clean_train, family="binomial")

summary(fit1)

#With the same principle, we remove ed3
fit1=glm(bi_y ~ balance + duration + campaign + pdays + jb1 + jb2 + 
           jb3 + ms1 + ed1 + cn1 + cn2 + hous_yes + 
           loan_no + pot3, data=trmdepdata_clean_train, family="binomial")

summary(fit1)

#Now, all the variables with less probability are removed now and we have a clean model.


#-----------------------------------------------------------------------------------------------------------

#5. Predicting the values for probabilities

trmdepdata_clean_train$score=predict(fit1,newdata=trmdepdata_clean_train,type = "response")

View(trmdepdata_clean_train)


#Viewing the probability data along with other factors
View(trmdepdata_clean_train[,c('bi_y','score')])

#-----------------------------------------------------------------------------------------------------------

#6. Creating the confusion matrix along with other matrices like sensitivity, specificity, accuracy etc.
#let's just randomly assign a probability cutoff of 0.2
cutoff=0.2

predicted=as.numeric(trmdepdata_clean_train$score>cutoff)

#Calculating the confusion matrix alues
TP=sum(predicted==1 & trmdepdata_clean_train$bi_y==1)
FP=sum(predicted==1 & trmdepdata_clean_train$bi_y==0)
FN=sum(predicted==0 & trmdepdata_clean_train$bi_y==1)
TN=sum(predicted==0 & trmdepdata_clean_train$bi_y==0)


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
  predicted=as.numeric(trmdepdata_clean_train$score>cutoff)
  
  TP=sum(predicted==1 & trmdepdata_clean_train$bi_y==1)
  FP=sum(predicted==1 & trmdepdata_clean_train$bi_y==0)
  FN=sum(predicted==0 & trmdepdata_clean_train$bi_y==1)
  TN=sum(predicted==0 & trmdepdata_clean_train$bi_y==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}

View(cutoff_data)

#Removing the first row in the data frame as it was only used for initilisation
cutoff_data=cutoff_data[-1,]


#We are now creating all the matrix like sensitivity, specificity, accuracy, distance,
#lift, KS, user defined matrix etc
cutoff_data=cutoff_data %>%
  mutate(Sn=TP/P, Sp=TN/N,
         dist=sqrt((1-Sn)**2+(1-Sp)**2),
         P=FN+TP,N=TN+FP) %>%
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

#So, our cutoff becomes 0.101

#Drawing the ROC or Receiver Operating Characteristics curve for train data

roccurve=roc(trmdepdata_clean_train$bi_y,trmdepdata_clean_train$score)

#plotting the ROC curver
plot(roccurve)

#Area under the curver
auc(roccurve)

#Area under the curve for our train data set becomes 0.8911


#-----------------------------------------------------------------------------------------------------------

#7. Testing of the train data set with highest KS cutoff

#Applying the cutoff we got for highest test data to our test data set
#WE first make the score column for our predicted proability values
trmdepdata_clean_test$score=predict(fit1,newdata = trmdepdata_clean_test,type = "response")

#View the data
View(trmdepdata_clean_test)

#Preparing the Confusion matrix for the test data
table(trmdepdata_clean_test$bi_y,as.numeric(trmdepdata_clean_test$score>KS_cutoff))
#KS cutoff is calculated from the model

#From above, We find the fllowing
TP_test=944
FN_test=191
TN_test=6590
FP_test=1770

#Calculating other matrix
Sensitivity_test=(TP_test/(TP_test+FN_test))
Specificity_test=(TN_test/(FP_test+TN_test))
Precision_test=(TP_test/(TP_test+FP_test))
Accuracy_test=((TP_test+TN_test)/(TP_test+TN_test+FN_test+FP_test))
KS=abs(((TP_test)/(TP_test+FN_test))-((FP_test)/(FP_test+TN_test)))

#Sensitivity: 0.83
#Specificity: 0.78
#Precision: 0.34
#Accuracy: 0.79
#KS: 0.61

#Drawing the ROC or Receiver Operating Characteristics curve for test data

roccurve_test=roc(trmdepdata_clean_test$bi_y,trmdepdata_clean_test$score)

#plotting the ROC curver
plot(roccurve_test)

#Area under the curver
auc(roccurve_test)

#Area under the curve for our test data set becomes 0.8839

#-------------------------------------------------------------------------------------------------------

#Working upon the fair Banking test data #Setting the workinng directory

#Loading the term deposit  test data set for banks
trmdepdata_Test_DS=read.csv("bank-full_test.csv")
trmdepdata_Test_DS_Orgi=trmdepdata_Test_DS

# Data Cleansing as per the designed model

trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(jb1=as.numeric(job=="student"),
                                             jb2=as.numeric(job=="retired"),
                                             jb3=as.numeric(job=="unemployed"),
                                             jb3=as.numeric(job=="management"),
                                             jb4=as.numeric(job %in% c("admin","self-employed","unknown")),
                                             jb5=as.numeric(job=="technician"),
                                             jb6=as.numeric(job %in% c("entrepreneur","services")),
                                             jb7=as.numeric(job=="housemaid")) %>% select(-job)

trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(ms1=as.numeric(marital=="married"),
                                             ms2=as.numeric(marital=="single")) %>% select(-marital)


trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(ed1=as.numeric(education=="primary"),
                                             ed2=as.numeric(education=="secondary"),
                                             ed3=as.numeric(education=="tertiary")) %>% select(-education)

trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(cn1=as.numeric(contact=="cellular"),
                                             cn2=as.numeric(contact=="telephone")) %>% select(-contact)

trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(defau_no=as.numeric(default=="no")) %>% select(-default)


trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(hous_yes=as.numeric(housing=="yes")) %>% select(-housing)

trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(loan_no=as.numeric(loan=="no")) %>% select(-loan)

trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(pot1=as.numeric(poutcome=="unknown"),
                                             pot2=as.numeric(poutcome=="failure"),
                                             pot3=as.numeric(poutcome=="success")) %>% select(-poutcome)

trmdepdata_Test_DS=trmdepdata_Test_DS %>% select(-month)



glimpse(trmdepdata_Test_DS)

#Running the logistic regression on the dataset
trmdepdata_Test_DS$score=predict(fit1,newdata = trmdepdata_Test_DS,type = "response")

#Now, our cutoff is 0.101, therefore
trmdepdata_Test_DS=trmdepdata_Test_DS %>% mutate(prediction=ifelse(score>0.101,"y","n"))

#Viewing the same
View(trmdepdata_Test_DS)


#Appending the Score and prediction column with the original data set
trmdepdata_Test_DS_Orgi$score=trmdepdata_Test_DS$score
trmdepdata_Test_DS_Orgi$prediction=trmdepdata_Test_DS$prediction

#Viewing the files
View(trmdepdata_Test_DS_Orgi)

#Writing it to the directory
write.csv(trmdepdata_Test_DS_Orgi,"bank-full_test_with_Predicted_Values.csv")
