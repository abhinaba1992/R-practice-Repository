#This portion covers the concepts of Logistic regression
#The following assignment primarily focuses on a bank that has come up with share trading services. So, they offer commission
#for each transaction a customer does. So, they have come up with a list of customers and they found out that not all the customers
#who take up this service yield profit to the bank as they might not be using the service. Based on their Pilot; they are able to
#find out who have used this service and yield profits. So, based on the charactisitcs of the customer and past data, we need to
#Come with a model that could predict the right customer.

#Author: Abhinaba Chakraborty
#last Updated: 05.12.2017

#Here our dependent variable is revenue grid since they have asked us to find who is a revenue making customer
#for them.

#To get the current working directory
getwd()

#Setting the work directory
setwd("C:/E drive/Docs/R Practice docs")

#Reading the values
treaddata=read.csv("Existing Base.csv")

#Displaying the same for cross checking
View(treaddata)


library(dplyr)

glimpse(treaddata)


#Step 1 [Data Preparation] (Cleaning the data)

#1. Convert characters to numeric
#2. Remove % signs from variables
#3. Create new variables as per requirements.
#4. Convert categorical variables into dummies

#Converting field children to numeric
treaddata=treaddata %>% mutate(children=ifelse(children=="Zero",0,substr(children,1,1)),
                               children=as.numeric(children))

glimpse(treaddata)


#Creating dummies for the variable age group; since we are doing logistic regression, so beside converting value
#ranges into mean we can also group them categorically in the following way coz this becomes more meaningful
#in case of logistic regression as helps to discover a better relationship.
table(treaddata$age_band) #This helps us to know the count of categories

#So here we are trying to group 13 categorical age ranges into a lower no. of categories.
round(prop.table(table(treaddata$age_band,treaddata$Revenue.Grid),1),2)
#The above will give us the proportion of the revenue grid for each age group.
#This exercise of converting catgories to proportion is helpful when we have a lot of categories
#Like we have 13 categories or types of age range values

#as per above approach, we divide our categorical variable into folllowing dummies
#We are targetting the Category 1 (as we have 1 and 2 as values for the column Revenue Grid)
#We can get six categories as per proportion

# 0.05 : Unknown
# 0.09 : 61 - 65
# 0.10 : 45-50, 51-55, 65-70, 71+
# 0.11 : 22-25, 26-30, 31-35, 41-45, 55-60
# 0.13 : 36-40
# 0.17 : 18-21

#So we are taking n-1 variables here, ignoring the unknown variable
treaddata=treaddata %>% mutate(ag1=as.numeric(age_band=="61-65"),
                               ag2=as.numeric(age_band %in% c("45-50","51-55","65-70","71+")),
                               ag3=as.numeric(age_band %in% c("22-25","26-30","31-35","41-45","55-60")),
                               ag4=as.numeric(age_band=="36-40"),
                               ag5=as.numeric(age_band=="18-21")
                               ) %>% select(-age_band)

glimpse(treaddata)


#Next we group the status variable into the following ways (since we have less number of categories here,
#we may choose to ignore the proportions approach used in the above category)
table(treaddata$status) #This helps us to know the count of categories

#So, we are grouping those categories in the following way (as per the policy for dummy creation; we are ignoring 
# the unknow status type here, also widowed is removed because that may give rise to multicollinearity issue)
treaddata=treaddata %>% mutate(status_div=as.numeric(status=="Divorced/Separated"),
                               status_partner=as.numeric(status=="Partner"),
                               status_single=as.numeric(status=="Single/Never Married")
                               ) %>% select(-status)


glimpse(treaddata)


#Next we target the variable occupation for creating dummies, even here we have a lot of categories
table(treaddata$occupation) # We have 9 categories, so next we find out the proportion with revenue grid


round(prop.table(table(treaddata$occupation,treaddata$Revenue.Grid),1),2)


#As per the above proportion we target the value of category 1 for revenue grid, we get the following 4 categories

# 0.09 = Housewife
# 0.10 = Retired
# 0.11 = Manual Worker, Other, Secretarial/Admin, Student, Unknown
# 0.12 = Business Manager, Professional


#Now as per the rule of the dummies, we are taking n-1 variables, Now, Based on some business req. , we choose
#to include housewife over the group (Manual Worker, Other, Secretarial/Admin, Student, Unknown) as the later
#should have been significant as per the proportion 
treaddata=treaddata %>%
  mutate(occ_BM_prof=as.numeric(occupation %in% c("Business Manager","Professional")),
         occ_Retired=as.numeric(occupation=="Retired"),
         occ_HW=as.numeric(occupation=="Housewife")) %>%
  select(-occupation)


glimpse(treaddata)


#Next we are comparinng for occupation partner for creating dummies
table(treaddata$occupation_partner) # We have 9 categories, so next we find out the proportion with revenue grid

round(prop.table(table(treaddata$occupation_partner,treaddata$Revenue.Grid),1),2)


#As per the above proportion we target the value of category 1 for revenue grid, we get the following 3 categories

# 0.10 = Other, Retired, Unknown
# 0.11 = Business Manager, Housewife, Manual Worker, Professional
# 0.12 = Secretarial/Admin, Student

#So we are taking the following variables based on the n-1 principle and business req.
treaddata=treaddata %>%
  mutate(op_1=as.numeric(occupation_partner %in% c("Other","Retired","Unknown")),
         op_2=as.numeric(occupation_partner %in% c("Student","Secretarial/Admin"))) %>%
  select(-occupation_partner)


glimpse(treaddata)


#Next we check for home status
table(treaddata$home_status) # We have 5 categories, so we do not need to do a proportion here

#We can simply ignore the unclassified category value for home status here
treaddata=treaddata %>%
  mutate(hs_livein=as.numeric(home_status=="Live in Parental Hom"),
         hs_own=as.numeric(home_status=="Own Home"),
         hs_rent_private=as.numeric(home_status=="Rent Privately"),
         hs_rent_council=as.numeric(home_status=="Rent from Council/HA")) %>%
  select(-home_status)


glimpse(treaddata)


#Next we deal with family income
table(treaddata$family_income) #We have 13 categories here, so we are doing a proportion here


round(prop.table(table(treaddata$family_income,treaddata$Revenue.Grid),1),2)

#Run the above function to take a look at the groupings
#based on the above groupings, we are doing the following by creating n-1 dummies

treaddata=treaddata %>%
  mutate(fi_1=as.numeric(family_income %in% 
                           c("< 4,000","< 8,000, >= 4,000")),
         fi_2=as.numeric(family_income %in% 
                           c("<12,500, >=10,000","<25,000, >=22,500","<27,500, >=25,000")),
         fi_3=as.numeric(family_income %in% 
                           c("<10,000, >= 8,000","<15,000, >=12,500","<20,000, >=17,500",">=35,000")),
         fi_4=as.numeric(family_income %in% 
                           c("<17,500, >=15,000","<22,500, >=20,000","<30,000, >=27,500"))
  ) %>%
  select(-family_income)


glimpse(treaddata)



#Next we are converting 3 variables namely self_employed, self_employed_partner and gender into dummies

table(treaddata$self_employed)
table(treaddata$self_employed_partner)
table(treaddata$gender)

#Since these 3 variables have yes/No/unknow as their values, we would be creating 2 dummies for each one 
#of them in the following way

treaddata=treaddata %>%
  mutate(self_emp_yes=as.numeric(self_employed=="Yes"),
         self_emp_part_yes=as.numeric(self_employed_partner=="Yes"),
         gender_f=as.numeric(gender=="Female"),
         gender_m=as.numeric(gender=="Male")) %>%
  select(-self_employed,-self_employed_partner,-gender)


glimpse(treaddata)


#Next, we are dropping off the location or area related variables
treaddata=treaddata %>%
  select(-TVarea,-post_code,-post_area,-region)
  

glimpse(treaddata)


#Next we Targetting the Year_last_moved variable

round(prop.table(table(treaddata$year_last_moved,treaddata$Revenue.Grid),1),2)

#We are not grouping anything here, we are just doing a missing value tratement for the value 0 in Year_last_moved
#column
treaddata=treaddata %>% 
  mutate(year_last_moved=ifelse(year_last_moved==0,1954,year_last_moved))


glimpse(treaddata)



#Step 2 [Train and Test] (Splitting our data into two parts)

set.seed(2)
s=sample(1:nrow(treaddata),0.7*nrow(treaddata))
treaddata_train=treaddata[s,]
treaddata_test=treaddata[-s,]
View(treaddata_train)
summary(treaddata_train)


#Step 3 [Removing multicollearity]
#Loading the library CAR since it has the vif function
library(car)
#using the lm function for regression and removing the REF No variable as its not necessary

for_vif=lm(Revenue.Grid~.-REF_NO,data=treaddata_train)
#Running the vif function
t=vif(for_vif)
#Finding the variables with highest vif values
sort(t,decreasing = T)
#Checking the summary of for_vif
summary(for_vif)


#We need to remove all the variables whose vif value is greater than 5 starting with the highest ones
#Removing Investment.in.Commudity
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity,data=treaddata_train)
t=vif(for_vif)
sort(t,decreasing = T)
summary(for_vif)

#WE remove Investment.in.Derivative
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative,data=treaddata_train)
t=vif(for_vif)
sort(t,decreasing = T)
summary(for_vif)

#We remove Investment.in.Equity
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity,data=treaddata_train)
t=vif(for_vif)
sort(t,decreasing = T)
summary(for_vif)

#We remove ag3
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-ag3,data=treaddata_train)
t=vif(for_vif)
sort(t,decreasing = T)
summary(for_vif)




#Next we remove gender_f
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-ag3-gender_f,data=treaddata_train)
t=vif(for_vif)
sort(t,decreasing = T)
summary(for_vif)


#Next we remove fi_3
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-ag3-gender_f-fi_3,data=treaddata_train)
t=vif(for_vif)
sort(t,decreasing = T)
summary(for_vif)


#Next we remove hs_own
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-ag3-gender_f-fi_3-hs_own,data=treaddata_train)
t=vif(for_vif)
sort(t,decreasing = T)
summary(for_vif)

#Next we remove Portfolio.Balance
for_vif=lm(Revenue.Grid~.-REF_NO-Investment.in.Commudity-Investment.in.Derivative-Investment.in.Equity-ag3-gender_f-fi_3-hs_own-Portfolio.Balance,data=treaddata_train)
t=vif(for_vif)
sort(t,decreasing = T)
summary(for_vif)

#So, all the values of VIF are under 5 now, so we do a  summary of our dataset


#So now we remove all the above variables from our dataset
treaddata_fit=treaddata_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-ag3,-gender_f,-fi_3,-hs_own,-Portfolio.Balance)

#Running the logistic regression
fit=glm(Revenue.Grid~.,family="binomial",data=treaddata_fit)
#We will get an error in the above step as our dependent variable Revenue.Grid has 1 and 2 as their 
#Categorical values instead of 1 and 0

#So we need to convert the same
treaddata$Revenue.Grid=as.numeric(treaddata$Revenue.Grid==1)

#Followingly , we are preparing the train and test again
set.seed(2)
s=sample(1:nrow(treaddata),0.7*nrow(treaddata))
treaddata_train=treaddata[s,]
treaddata_test=treaddata[-s,]


#So now we remove all the unecesssary variables from our modified dataset
treaddata_fit=treaddata_train %>%
  select(-REF_NO,-Investment.in.Commudity,-Investment.in.Derivative,
         -Investment.in.Equity,-ag3,-gender_f,-fi_3,-hs_own,-Portfolio.Balance)



#Running the logistic regression again
fit=glm(Revenue.Grid~.,family="binomial",data=treaddata_fit)


summary(fit)

#The interpretation of summary function and all matrices is a bit diff. for linear and logistic regression
#The higher the value of an estimate is, the better the chance it has of increasing the value of 1 or positive
#probability
#The lower the value of estimate, the lesser it has a chance of increasing the value of the postive probabilty of 1

#Null deviance describes that what would have been the error in the model had there been no variables in the model
#Residual deviance describes what is the error after we have introduced all the variables in the model.

#The larger the difference is between Null deviance and Residual deviance, the better is the model

#Akaike Information Criterion(AIC): AIC helps us in comparing multiple models. Lower the AIC, better is the model.
#It is based on the errors in our model.

#There is a function that helps us to remove variables with higher probabilities
#Now there are three ways of removing variables with higher probability
#1.) Backward elimination: Here all the included variables in the model are removed one at a time
#that are of less significance(based on the probability). It is the technique demonstrated in the
#"Linear regression Part 9"
#2.) Forward addition: Here we add one variable at a time and check if its significant, if its significant
#We keep it and else we remove it. We do this again and again until we stack up all the variables in the model.
#3.) Stepwise: In Stepwise all the variables are added in the beginning,and unsignificant variables are 
#removed one at a time, the only difference is that whatever variables it removes; even those variables are
#checked for significance across various iterations.


#So we  do
fit=step(fit) #So this function will run various iteration to arrive with the variables having the lowest AIC value.
              #if we only  want to see the final iteration and not each one, we can do fit=step(fit,trace=false)

#In order to get the final set of variables used in the step wise iteration, we use the function formula
formula(fit)

# #We get the below combination
# Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
#   Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
#   Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
#   Home.Loan + Online.Purchase.Amount + op_1 + fi_2 + fi_4 + 
#   self_emp_part_yes + gender_m


#So now we use GLM with this new combination

fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
           Personal.Loan + Investment.in.Mutual.Fund + Investment.Tax.Saving.Bond + 
           Home.Loan + Online.Purchase.Amount + op_1 + fi_2 + fi_4 + 
           self_emp_part_yes + gender_m, data=treaddata_train, family="binomial")

summary(fit1)

#Now there might be still some variables with a pr value greater than 5 % or 0.05
#So we need to remove them one by one

#So first we are removing investment in mutual funds
fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
           Personal.Loan  + Investment.Tax.Saving.Bond + 
           Home.Loan + Online.Purchase.Amount + op_1 + fi_2 + fi_4 + 
           self_emp_part_yes + gender_m , data=treaddata_train, family = "binomial")

summary(fit1)

#Based on the same principle, we remove gender_m since it ahs the highest pr value
fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
           Personal.Loan  + Investment.Tax.Saving.Bond + 
           Home.Loan + Online.Purchase.Amount + op_1 + fi_2 + fi_4 + 
           self_emp_part_yes , data=treaddata_train, family = "binomial")

summary(fit1)


#Based on the same principle, we remove op1 since it ahs the highest pr value
fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
           Personal.Loan  + Investment.Tax.Saving.Bond + 
           Home.Loan + Online.Purchase.Amount + fi_2 + fi_4 + 
           self_emp_part_yes , data=treaddata_train, family = "binomial")

summary(fit1)


#Based on the same principle, we remove fi_2 since it ahs the highest pr value
fit1=glm(Revenue.Grid ~ Average.Credit.Card.Transaction + Balance.Transfer + 
           Term.Deposit + Life.Insurance + Medical.Insurance + Average.A.C.Balance + 
           Personal.Loan  + Investment.Tax.Saving.Bond + 
           Home.Loan + Online.Purchase.Amount + fi_4 + 
           self_emp_part_yes , data=treaddata_train, family = "binomial")

summary(fit1)


#Now, all the variables with less probability are removed now and we have a clean model.
#Below we are predicting for probability values and assigning it to our train data set, note that we 
#pass an extra parameter type=response here which ws not the case for linear regression, it is to
#basically depict that we are expecting a probability value here instead of continuous data.

treaddata_train$score=predict(fit1,newdata=treaddata_train,type = "response")

View(treaddata_train) # by running this we would find a new column score with the probabilities 
                      # for our logistic regression


#We want to take a look at only the Revenue Grid and score column
View(treaddata_train[,c('Revenue.Grid','score')])

#We are trying to randomly give a cutoff to check the values of TP,TN,FP and FN respectively.
cutoff=0.2

predicted=as.numeric(treaddata_train$score>cutoff)

#Calculating the confusion matrix alues
TP=sum(predicted==1 & treaddata_train$Revenue.Grid==1)
FP=sum(predicted==1 & treaddata_train$Revenue.Grid==0)
FN=sum(predicted==0 & treaddata_train$Revenue.Grid==1)
TN=sum(predicted==0 & treaddata_train$Revenue.Grid==0)


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

#Running the loop and filling up the data frame
for (cutoff in cutoffs){
  predicted=as.numeric(treaddata_train$score>cutoff)
  
  TP=sum(predicted==1 & treaddata_train$Revenue.Grid==1)
  FP=sum(predicted==1 & treaddata_train$Revenue.Grid==0)
  FN=sum(predicted==0 & treaddata_train$Revenue.Grid==1)
  TN=sum(predicted==0 & treaddata_train$Revenue.Grid==0)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,FN,TN))
}


#Lets view the cutoff data
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


#Now we need to check for highest value for KS and lowest value for ditance to get the best cutoff value
#for our model, we need to find that both the highest KS value and lowest distance value point to the cutoff
#0.091, but accuracy points out to the cutoff 0.374, however since the targetted variable is imbalanced, we 
#cannot go with accuracy here. so best is to go with KS. Also, with the cutoff value of 0.091 the value of
#sensitivity is also very high. so we are considering 0.091 here.



#Now we are converting the wide format into long format so that we can plot those details
library(tidyr)
cutoff_viz=cutoff_data %>%
  select(cutoff,Sn,Sp,dist,KS,Accuracy,Lift,M) %>%
  gather(Criterion,Value,Sn:M) #here gather is used for the conversion from wide to long format

View(cutoff_viz)

library(ggplot2)
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


#Applying the cutoff we got for highest test data to our test data set
#WE first make the score column for our predicted proability values
treaddata_test$score=predict(fit1,newdata = treaddata_test,type = "response")

#Preparing the Confusion matrix for the test data
table(treaddata_test$Revenue.Grid,as.numeric(treaddata_test$score>KS_cutoff))


#So we find out that the sensitivity on the unknow data set is 89%, which is a good measure as our model
#is performing well in an unknown data set as well

#---------------------------------------------------------------------------------------------------------
#Now, in case we go with the cutoff for minimum distance, we do the following
dist_cutoff=cutoff_data$cutoff[which.min(cutoff_data$dist)][1]
dist_cutoff

table(treaddata_test$Revenue.Grid,as.numeric(treaddata_test$score>dist_cutoff))

#So we find out that the sensitivity on the unknow data set is 89% here as well, which is a god measure 
#for our model is performing well in an unknown data set as well

#---------------------------------------------------------------------------------------------------------
#Now, in case we want to go with the cutoff for maximum accuracy
Acc_cutoff=cutoff_data$cutoff[which.max(cutoff_data$Accuracy)][1]
Acc_cutoff
table(treaddata_test$Revenue.Grid,as.numeric(treaddata_test$score>Acc_cutoff))


#We find out that going with the cutoff for maximum accuracy, the sensitivity for unknown data set comes
#down to 70% which was earlier 89% when we considered our cutoff based on maximum KS / minimum distance

#---------------------------------------------------------------------------------------------------------
#Now, in case we want to go with our custom business matrix
# Cutoff with minimum M ( The hypothetical business criterion)
M_cutoff=cutoff_data$cutoff[which.min(cutoff_data$M)][1]
M_cutoff
table(treaddata_test$Revenue.Grid,as.numeric(treaddata_test$score>M_cutoff))


#We find out that going for the cutoff for minimum value for custom matrix, we find out our sensitivity for unknow
#dataset to be 82% which is again lesser than what we got for max KS/min dist.

#---------------------------------------------------------------------------------------------------------
#Drawing the ROC curve

library(pROC)

roccurve=roc(treaddata_train$Revenue.Grid,treaddata_train$score)

#plotting the ROC curver
plot(roccurve)

#Area under the curver
auc(roccurve)


#In case we didn't have the pROC package, we can do the following

roc_data=cutoff_data %>% 
  select(cutoff,Sn,Sp) %>% 
  mutate(TPR=Sn,FPR=1-Sp) %>% 
  select(cutoff,TPR,FPR)

ggplot(roc_data,aes(x=FPR,y=TPR))+geom_line()+ggtitle("My ROC Curve")
