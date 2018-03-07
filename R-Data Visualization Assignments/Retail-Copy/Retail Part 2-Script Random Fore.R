#This is the second attempt at the retail project part 2 problem: We are using Logistic regression here instead
#of random forest
#AUTHOR: Abhinaba Chakraborty
#LAST MODIFIED: 7th February 2018


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

# Deleting the ID field as its not required for data interpretation
retail_data_clean=retail_data_clean %>% select(-Id)


#Trying to Create dummies for some of the location specific variables
#countyname
head(sort(table(retail_data_clean$countyname),decreasing=T),11)
#Creatig dummies for some of the countnames
retail_data_clean=retail_data_clean %>% mutate(couty_typ1=as.numeric(countyname=="Washington County"),
                                               couty_typ2=as.numeric(countyname=="Franklin County"),
                                               couty_typ3=as.numeric(countyname=="Middlesex County"),
                                               couty_typ4=as.numeric(countyname=="Aroostook County"),
                                               couty_typ5=as.numeric(countyname=="Essex County"),
                                               couty_typ6=as.numeric(countyname=="Penobscot County"),
                                               couty_typ7=as.numeric(countyname=="Worcester County"),
                                               couty_typ8=as.numeric(countyname=="Hancock County"),
                                               couty_typ9=as.numeric(countyname=="Windham County"),
                                               couty_typ10=as.numeric(countyname=="Lincoln County"),
                                               couty_typ11=as.numeric(countyname=="Grafton County")
) %>% select(-countyname)


#Areaname
head(sort(table(retail_data_clean$Areaname),decreasing=T),12)
#Creating dummies for the same
retail_data_clean=retail_data_clean %>% mutate(AreaNam_typ1=as.numeric(Areaname=="Boston-Cambridge-Quincy, MA-NH HUD Metro FMR Area"),
                                               AreaNam_typ2=as.numeric(Areaname=="Hartford-West Hartford-East Hartford, CT HUD Metro FMR Area"),
                                               AreaNam_typ3=as.numeric(Areaname=="Providence-Fall River, RI-MA HUD Metro FMR Area"),
                                               AreaNam_typ4=as.numeric(Areaname=="Penobscot County, ME (part) HUD Metro FMR Area"),
                                               AreaNam_typ5=as.numeric(Areaname=="Springfield, MA HUD Metro FMR Area"),
                                               AreaNam_typ6=as.numeric(Areaname=="Washington County, ME"),
                                               AreaNam_typ8=as.numeric(Areaname=="Oxford County, ME"),
                                               AreaNam_typ9=as.numeric(Areaname=="Coos County, NH"),
                                               AreaNam_typ10=as.numeric(Areaname=="Hancock County, ME"),
                                               AreaNam_typ11=as.numeric(Areaname=="Burlington-South Burlington, VT MSA")
) %>% select(-Areaname)


#countytownname
head(sort(table(retail_data_clean$countytownname),decreasing=T),11)
#Creating dummies for the variable for storecode
retail_data_clean=retail_data_clean %>% mutate(CntyTwn_typ1=as.numeric(countytownname=="Jackson County"),
                                               CntyTwn_typ2=as.numeric(countytownname=="Washington County"),
                                               CntyTwn_typ3=as.numeric(countytownname=="Franklin County"),
                                               CntyTwn_typ4=as.numeric(countytownname=="Jefferson County"),
                                               CntyTwn_typ5=as.numeric(countytownname=="Lincoln County"),
                                               CntyTwn_typ6=as.numeric(countytownname=="Madison County"),
                                               CntyTwn_typ7=as.numeric(countytownname=="Marion County"),
                                               CntyTwn_typ8=as.numeric(countytownname=="Monroe County"),
                                               CntyTwn_typ9=as.numeric(countytownname=="Union County"),
                                               CntyTwn_typ10=as.numeric(countytownname=="Clay County"),
                                               CntyTwn_typ11=as.numeric(countytownname=="Lee County")
) %>% select(-countytownname)


#state_alpha
head(sort(table(retail_data_clean$state_alpha),decreasing=T),11)
#Creating dummies for the variable for state_alpha
retail_data_clean=retail_data_clean %>% mutate(StatAlp_typ1=as.numeric(state_alpha=="ME"),
                                               StatAlp_typ2=as.numeric(state_alpha=="MA"),
                                               StatAlp_typ3=as.numeric(state_alpha=="NH"),
                                               StatAlp_typ4=as.numeric(state_alpha=="VT"),
                                               StatAlp_typ5=as.numeric(state_alpha=="TX"),
                                               StatAlp_typ6=as.numeric(state_alpha=="CT"),
                                               StatAlp_typ7=as.numeric(state_alpha=="GA"),
                                               StatAlp_typ8=as.numeric(state_alpha=="VA"),
                                               StatAlp_typ9=as.numeric(state_alpha=="MO"),
                                               StatAlp_typ10=as.numeric(state_alpha=="KY"),
                                               StatAlp_typ11=as.numeric(state_alpha=="KS")
) %>% select(-state_alpha)


# #State
# head(sort(table(retail_data_clean$State),decreasing=T),10)
# #Creating dummies for the variable for State
# retail_data_clean=retail_data_clean %>% mutate(State_1=as.numeric(State==23),
#                                                State_2=as.numeric(State==25),
#                                                State_3=as.numeric(State==33),
#                                                State_4=as.numeric(State==50),
#                                                State_5=as.numeric(State==48)
# ) %>% select(-State)



#country
head(sort(table(retail_data_clean$country),decreasing=T),11)
#Creating dummies for the variable for Country
retail_data_clean=retail_data_clean %>% mutate(Country_1=as.numeric(country==9),
                                               Country_2=as.numeric(country==3),
                                               Country_3=as.numeric(country==5),
                                               Country_4=as.numeric(country==17),
                                               Country_5=as.numeric(country==7),
                                               Country_6=as.numeric(country==11),
                                               Country_7=as.numeric(country==27),
                                               Country_8=as.numeric(country==15),
                                               Country_9=as.numeric(country==19),
                                               Country_10=as.numeric(country==13),
                                               Country_11=as.numeric(country==1)
) %>% select(-country)


#CouSub
head(sort(table(retail_data_clean$CouSub),decreasing=T),11)
#Creating dummies for CouSub
retail_data_clean=retail_data_clean %>% mutate(CouSub_1=as.numeric(CouSub==99999),
                                               CouSub_2=as.numeric(CouSub==2760)
) %>% select(-CouSub)



#We shouldn't group storecode as because it's an ID related variaable
retail_data_clean = retail_data_clean %>%  select(-storecode)


#Viewing the data set
glimpse(retail_data_clean)
View(retail_data_clean)

#Checking if any columns contain NAs
apply(retail_data_clean,2,function(x) sum(is.na(x)))
#No outliers found

#removing the NA field from column population as it may give an error
retail_data_clean=retail_data_clean %>% na.omit()
#In case you remove any NAs, you can run the following line to reverify
nrow(retail_data_clean)
#------------------------------------------------------------------------------------------------


#3. Splitting the data into train and test
set.seed(2)
s=sample(1:nrow(retail_data_clean),0.7*nrow(retail_data_clean))
retail_data_clean_train=retail_data_clean[s,]
retail_data_clean_test=retail_data_clean[-s,]
#Viewing the train data set 
View(retail_data_clean_train)
#Takinga closer look at the summary of the train data set
summary(retail_data_clean_train)

#----------------------------------------------------------------------------------------------------

#4. RANDOM FOREST FOR CLASSIFICATION
library(randomForest) #Package for using the random forest function

#Applying random forest function, do.trace would help to capture the output on the run, so it will show how many trees
#are getting created and other random forest related metrics

fit1=randomForest(store~.,data=retail_data_clean_train,do.trace=T)
#So from the above code, we can see the trace and conclude that 500 trees has been ran
#So we would get a OOB value or error value, along with the errors of the category value 1 and value 0

fit1
#IF we run the above piece of code, we would get many matrices
#Now, since this is a classification problem, the number of variables that would be used for each split would be 
#Sqrt(11) where 11 is the no. of cols in the data set, which is equal to 3.31, hence rounded off to 3
#We would also see OOB estmate error, which is nothing but the avg. of error or OOB across all the 500 trees
#Created in this scenario. also we can see the confusion matrix along with the misclassification percentage


#----------------------------------------------------------------------------------------------------

#5.PREDICTING THE VALUES FOR PROBALITIES 

#Predicting the values
retail_data_clean_train$score=predict(fit1,newdata = retail_data_clean_train)

#Viewing the probability data along with other factors
View(retail_data_clean_train[,c('store','score')])


#Calculating importance
Var_Imp=importance(fit1)
#OR
Var_Imp[order(Var_Imp[,1],decreasing=T),] #Here we are doing a sort in order to check the variable with highest importance


#We can alos try and plot the same for better visualising the importance
varImpPlot(fit1)



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

#So, our cutoff becomes 0.485

#Drawing the ROC or Receiver Operating Characteristics curve for train data

roccurve=roc(retail_data_clean_train$store,retail_data_clean_train$score)

#plotting the ROC curver
plot(roccurve)

#Area under the curve
auc(roccurve)

#Area under the curve for our train data set becomes 0.9939

#----------------------------------------------------------------------------------------------------
# 7. TESTING THE MODEL ON OUR SAMPLE TEST DATA 

retail_data_clean_test$score=predict(fit1,newdata = retail_data_clean_test)

#View the data
View(retail_data_clean_test)

#Preparing the Confusion matrix for the test data
table(retail_data_clean_test$store,as.numeric(retail_data_clean_test$score>KS_cutoff))
#KS cutoff is calculated from the model

#From above, We find the fllowing
TP_test=314
FN_test=130
TN_test=461
FP_test=97


#Calculating other matrix
Sensitivity_test=(TP_test/(TP_test+FN_test))
Specificity_test=(TN_test/(FP_test+TN_test))
Precision_test=(TP_test/(TP_test+FP_test))
Accuracy_test=((TP_test+TN_test)/(TP_test+TN_test+FN_test+FP_test))
KS=abs(((TP_test)/(TP_test+FN_test))-((FP_test)/(FP_test+TN_test)))


#Sensitivity: 0.70
#Specificity: 0.82
#Precision: 0.76
#Accuracy: 0.77
#KS: 0.53
#Drawing the ROC or Receiver Operating Characteristics curve for test data

roccurve_test=roc(retail_data_clean_test$store,retail_data_clean_test$score)

#plotting the ROC curver
plot(roccurve_test)

#Area under the curver
auc(roccurve_test)

#Area under the curve for our test data set becomes 0.8211

#----------------------------------------------------------------------------------------------------

# 8. TESTING OUR MODEL ON THE ORIGINAL TEST DATA

retail_data_test_DS=read.csv("store_test.csv")

retail_data_test_DS_Orig=retail_data_test_DS


#performing the basic data prep
retail_data_test_DS=retail_data_test_DS %>% select(-Id)


retail_data_test_DS=retail_data_test_DS %>% mutate(couty_typ1=as.numeric(countyname=="Washington County"),
                                                   couty_typ2=as.numeric(countyname=="Franklin County"),
                                                   couty_typ3=as.numeric(countyname=="Middlesex County"),
                                                   couty_typ4=as.numeric(countyname=="Aroostook County"),
                                                   couty_typ5=as.numeric(countyname=="Essex County"),
                                                   couty_typ6=as.numeric(countyname=="Penobscot County"),
                                                   couty_typ7=as.numeric(countyname=="Worcester County"),
                                                   couty_typ8=as.numeric(countyname=="Hancock County"),
                                                   couty_typ9=as.numeric(countyname=="Windham County"),
                                                   couty_typ10=as.numeric(countyname=="Lincoln County"),
                                                   couty_typ11=as.numeric(countyname=="Grafton County")
) %>% select(-countyname)



retail_data_test_DS=retail_data_test_DS %>% mutate(AreaNam_typ1=as.numeric(Areaname=="Boston-Cambridge-Quincy, MA-NH HUD Metro FMR Area"),
                                                   AreaNam_typ2=as.numeric(Areaname=="Hartford-West Hartford-East Hartford, CT HUD Metro FMR Area"),
                                                   AreaNam_typ3=as.numeric(Areaname=="Providence-Fall River, RI-MA HUD Metro FMR Area"),
                                                   AreaNam_typ4=as.numeric(Areaname=="Penobscot County, ME (part) HUD Metro FMR Area"),
                                                   AreaNam_typ5=as.numeric(Areaname=="Springfield, MA HUD Metro FMR Area"),
                                                   AreaNam_typ6=as.numeric(Areaname=="Washington County, ME"),
                                                   AreaNam_typ8=as.numeric(Areaname=="Oxford County, ME"),
                                                   AreaNam_typ9=as.numeric(Areaname=="Coos County, NH"),
                                                   AreaNam_typ10=as.numeric(Areaname=="Hancock County, ME"),
                                                   AreaNam_typ11=as.numeric(Areaname=="Burlington-South Burlington, VT MSA")
) %>% select(-Areaname)



retail_data_test_DS=retail_data_test_DS %>% mutate(CntyTwn_typ1=as.numeric(countytownname=="Jackson County"),
                                                   CntyTwn_typ2=as.numeric(countytownname=="Washington County"),
                                                   CntyTwn_typ3=as.numeric(countytownname=="Franklin County"),
                                                   CntyTwn_typ4=as.numeric(countytownname=="Jefferson County"),
                                                   CntyTwn_typ5=as.numeric(countytownname=="Lincoln County"),
                                                   CntyTwn_typ6=as.numeric(countytownname=="Madison County"),
                                                   CntyTwn_typ7=as.numeric(countytownname=="Marion County"),
                                                   CntyTwn_typ8=as.numeric(countytownname=="Monroe County"),
                                                   CntyTwn_typ9=as.numeric(countytownname=="Union County"),
                                                   CntyTwn_typ10=as.numeric(countytownname=="Clay County"),
                                                   CntyTwn_typ11=as.numeric(countytownname=="Lee County")
) %>% select(-countytownname)



retail_data_test_DS=retail_data_test_DS %>% mutate(StatAlp_typ1=as.numeric(state_alpha=="ME"),
                                                   StatAlp_typ2=as.numeric(state_alpha=="MA"),
                                                   StatAlp_typ3=as.numeric(state_alpha=="NH"),
                                                   StatAlp_typ4=as.numeric(state_alpha=="VT"),
                                                   StatAlp_typ5=as.numeric(state_alpha=="TX"),
                                                   StatAlp_typ6=as.numeric(state_alpha=="CT"),
                                                   StatAlp_typ7=as.numeric(state_alpha=="GA"),
                                                   StatAlp_typ8=as.numeric(state_alpha=="VA"),
                                                   StatAlp_typ9=as.numeric(state_alpha=="MO"),
                                                   StatAlp_typ10=as.numeric(state_alpha=="KY"),
                                                   StatAlp_typ11=as.numeric(state_alpha=="KS")
) %>% select(-state_alpha)



retail_data_test_DS=retail_data_test_DS %>% mutate(Country_1=as.numeric(country==9),
                                                   Country_2=as.numeric(country==3),
                                                   Country_3=as.numeric(country==5),
                                                   Country_4=as.numeric(country==17),
                                                   Country_5=as.numeric(country==7),
                                                   Country_6=as.numeric(country==11),
                                                   Country_7=as.numeric(country==27),
                                                   Country_8=as.numeric(country==15),
                                                   Country_9=as.numeric(country==19),
                                                   Country_10=as.numeric(country==13),
                                                   Country_11=as.numeric(country==1)
) %>% select(-country)



retail_data_test_DS=retail_data_test_DS %>% mutate(CouSub_1=as.numeric(CouSub==99999),
                                                   CouSub_2=as.numeric(CouSub==2760)
) %>% select(-CouSub)


retail_data_test_DS = retail_data_test_DS %>%  select(-storecode)


#Taking a view at the data set
View(retail_data_test_DS)
glimpse(retail_data_test_DS)


#Running the logistic regression on the dataset
retail_data_test_DS$score=predict(fit1,newdata = retail_data_test_DS)

#Now, our cutoff is 0.404, therefore
retail_data_test_DS=retail_data_test_DS %>% mutate(prediction=ifelse(score>0.485,1,0))

#Viewing the same
View(retail_data_test_DS)


#Appending the Score and prediction column with the original data set
retail_data_test_DS_Orig$score=retail_data_test_DS$score
retail_data_test_DS_Orig$prediction=retail_data_test_DS$prediction

#handling NA values in the data set
#stroring the result in an object store
store=retail_data_test_DS_Orig$score
#Calculating the average probability score so as to assign the missing values
avg_result=round(mean(store,na.rm = TRUE),3)
#Creating a new data frame to write the result
dataframe_result_final=data.frame(store)
#Replacing the NA values in the data frame with the average score/probability
dataframe_result_final$store[which(is.na(dataframe_result_final$store))]=avg_result

#Viewing the files
View(retail_data_test_DS_Orig)
#Viewing the data set that has to be written
View(dataframe_result_final)


#Writing it to the directory
write.csv(dataframe_result_final,"Abhinaba_Chakraborty_P2_part2.csv",row.names=FALSE)
