#This portion covers the concepts of Linear regression
#The following assignment primarily focuses on a debt advisory firm that provides advise to individuals based on their 
#characteristics on which loan to choose from which bank based on the past credit history, personal details and other 
#factors of the person seeking the loan. WE NEED TO COME WITH A POC FOR THE SAME.
#Author: Abhinaba Chakraborty
#last Updated: 29.11.2017

#To get the current working directory
getwd()

#Setting the work directory
setwd("C:/E drive/Docs/R Practice docs")

#Reading the values
loandata=read.csv("loans data.csv")

#Displaying the same for cross checking
View(loandata)

#This is a classical example of regression problem, our dependent variable is interest rate here
#and all other variables are dependent variables.

#Checking the data Set, there are 15 variables and 2500 observations and data points
# So we have 2500 customer details and 15 characteristics

#Directly installling dplyr packge from a different repository
#install.packages("dplyr", repos = "http://mran.revolutionanalytics.com")

library(dplyr)

glimpse(loandata)


#Step 1 [Data Preparation] (Cleaning the data)

#1. Convert characters to numeric
#2. Remove % signs from variables
#3. Create new variables as per requirements.
#4. Convert categorical variables into dummies

#Using the mutate function along with filtering to create the new columns
clnloandata=loandata %>% mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)),
       Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)),
       Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines),
       Amount.Requested=as.numeric(Amount.Requested),
       Amount.Funded.By.Investors=as.numeric(Amount.Funded.By.Investors),
       Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance)
       )

glimpse(clnloandata)


#Removing the column Amount Funded By Investors (as because when the user comes to the loan advisory firm, he will
#not have a amount; it is only after the loan advicery firms confirms a bank and the person get's the loan and then we
#know that the variable value)
clnloandata=clnloandata %>% select(-Amount.Funded.By.Investors)

glimpse(clnloandata)


#Now since the FICO range (Equivalent of CIBIL score in UK) is given as a range of numbers, we may want to either take
#the avg. of range or max or min of the range based on our choice.
#Taking the average in this scenario
clnloandata=clnloandata %>% mutate(f1=as.numeric(substr(FICO.Range,1,3)),
                                   f2=as.numeric(substr(FICO.Range,5,7)),
                                   fico=0.5*(f1+f2)) %>% select(-FICO.Range,-f1,-f2)

glimpse(clnloandata)



#Now we are cleaning up the data for employment length and removing all the variations of the string it has

#Seeing the variation of data in employment length
table(clnloandata$Employment.Length)

#Cleaning
clnloandata=clnloandata %>% mutate(el=ifelse(substr(Employment.Length,1,2)=="10",10,Employment.Length),
                                   el=ifelse(substr(Employment.Length,1,1)=="<",0,el),
                                   el=gsub("years","",el),
                                   el=gsub("years","",el),
                                   el=as.numeric(el)
                                   )  %>% select(-Employment.Length) %>% na.omit()

#na.omit would drop records with na

glimpse(clnloandata)


#Now we are creating dummy variables for home.ownership
table(clnloandata$Home.Ownership)

#We have four categories(MORTGAGE,OTHER,OWN,RENT), hence we would create 3 dummies as per the rule for creating dummies
clnloandata = clnloandata %>% mutate(HW_RENT=as.numeric(Home.Ownership=="RENT"),
                                     HW_MORT=as.numeric(Home.Ownership=="MORTGAGE"),
                                     HW_OWN=as.numeric(Home.Ownership=="OWN")) %>% select(-Home.Ownership)


glimpse(clnloandata)


#Now for the loan purpose variable, we have 14 categories, it's not possible to create 13 dummies for the same
table(clnloandata$Loan.Purpose)

#So we have to logically club the variable with some other variable (say for example interest rate)

#So first we check which categories are close and can be merged together
#Sp, we get the mean of interest rate for each categorical variable of loan purpose, i.e; 14 means
round(tapply(clnloandata$Interest.Rate,clnloandata$Loan.Purpose,mean))

#So we get 5 categories after logically merging the aggregated Interest rates
#1: car, educational, major_purchase: all have 11 as an avg interest rate
#2: credit_card, house, small_business, other: all have 13 as an avg interest rate
#3: home_improvement, medical, vacation, wedding: all have 12 as an avg interest rate
#4: debt_consolidation, moving: all have 14 as an avg interest rate
#5: renewable_energy: have an interest rate of 10

#Now clubbing the categories in the data set, we are creating 4 dummies out of 5 variables
clnloandata=clnloandata %>% mutate(LP_1=as.numeric(Loan.Purpose %in% c("car","educational","major_purchase")),
                                   LP_2=as.numeric(Loan.Purpose %in% c("credit_card","house","small_business","other")),
                                   LP_3=as.numeric(Loan.Purpose %in% c("home_improvement","medical","vacation","wedding")),
                                   LP_4=as.numeric(Loan.Purpose %in% c("debt_consolidation","moving"))) %>% select(-Loan.Purpose) 

glimpse(clnloandata)



#We have to create dummy variables for loan length
table(clnloandata$Loan.Length)


#We have two categories for the same, so we would be creating one dummy for it
clnloandata=clnloandata %>% mutate(LL_36=as.numeric(Loan.Length=="36 months")) %>% select(-Loan.Length) 


glimpse(clnloandata)


#It is a good practice not to use any geo location or place in our data, hence dropping the state column
clnloandata = clnloandata %>% select(-State)
glimpse(clnloandata)


# Now we are checking if there are any missing values in our data, we should not have any missing values in our data
apply(clnloandata,2,function(x) sum(is.na(x)))




#Step 2 [Train and Test] (Splitting our data into two parts)

# Breaking the data into two parts: train and test
# Sampling 75% of the data
set.seed(2)
s=sample(1:nrow(clnloandata),0.75*nrow(clnloandata))
clnloandata_train=clnloandata[s,]
clnloandata_test=clnloandata[-s,]

#Train and test is differentiated, belowwe check their count
dim(clnloandata_train)
dim(clnloandata_test)


#Checkingthe correlation betweenvariables(Here, we can use the function Cor for our data set)
View(cor(clnloandata_train))
#Here we can take a sneak  peak at the correlation between variables and  especially on our dependent variable interest rate.
#Wecan also find out any problems of multicollinearity.



#Step 3 [Remove multicollinearity among variables]

#Now in order to implement linear regression in R we  have function named lm or linear model which is used as follows
fit=lm(Interest.Rate~.-ID,data=clnloandata_train)
#here the  dot means all variables excluding  thevariables mentioned  above (Interest.Rate is mentioned here)
#and we remove the column ID from our train dataset

# Now, we are running the summary function to see the T-values, standard errors and R square values etc.
summary(fit)

#Now, in order to run VIF or variance inflation factor in order to remove multicollinearity, we do the following
#Loading the package car which has the function for VIF
library(car)
#Now we are running the VIF here
t=vif(fit)
sort(t,decreasing = T)

#We found out that the values like HW_RENT,HW_MORT etc.are having high values, so we remove HW_RENT first since it has
#the highest VIF value

fit=lm(Interest.Rate~.-ID-HW_RENT,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)


# Next we remove LP_4 because it has high VIF value
fit=lm(Interest.Rate~.-ID-HW_RENT-LP_4,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)

#So all the values are below 5 now and the multicollinearity problem is solved
#We take a look at our model data now
summary(fit)

#We can see the section of residuals and estimate
#WE have to make sure that the 4 assumptions that we made about our data are right

#Based on the pr value of variables in the model, we are removing variables as per significance

#lets remove LP_2 coz it has the highest pr value

fit=lm(Interest.Rate~.-ID-HW_RENT-LP_4-LP_2,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)
summary(fit)

#Based on the same principal, we remove Debt to income ratio

fit=lm(Interest.Rate~.-ID-HW_RENT-LP_4-LP_2-Debt.To.Income.Ratio,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)
summary(fit)

#Based on the same principal, we remove Open Credit Lines

fit=lm(Interest.Rate~.-ID-HW_RENT-LP_4-LP_2-Debt.To.Income.Ratio-Open.CREDIT.Lines,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)
summary(fit)

#Based on the same principal, we remove amount requested
fit=lm(Interest.Rate~.-ID-HW_RENT-LP_4-LP_2-Debt.To.Income.Ratio-Open.CREDIT.Lines-Amount.Requested,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)
summary(fit)


#Based on the same principal, we remove HW_OWN
fit=lm(Interest.Rate~.-ID-HW_RENT-LP_4-LP_2-Debt.To.Income.Ratio-Open.CREDIT.Lines-Amount.Requested-HW_OWN,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)
summary(fit)



#Based on the same principal, we remove LP_3
fit=lm(Interest.Rate~.-ID-HW_RENT-LP_4-LP_2-Debt.To.Income.Ratio-Open.CREDIT.Lines-Amount.Requested-HW_OWN-LP_3,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)
summary(fit)



#Based on the same principal, we remove Resolving credit balance
fit=lm(Interest.Rate~.-ID-HW_RENT-LP_4-LP_2-Debt.To.Income.Ratio-Open.CREDIT.Lines-Amount.Requested-HW_OWN-LP_3-Revolving.CREDIT.Balance,data=clnloandata_train)
t=vif(fit)
sort(t,decreasing = T)
summary(fit)


#So, at this point of time, all the unecessary variables are taken out of the model and this is the final model that we have.


#Step 4 [Using the model to check for our prediction and errors]
#Here the fitted and the residual function helps us to project the predicted values

#In the below code we are creating a data frame having the columns interest rate (Original), fitted/predicted interest rate
#and Errors (Actual value - Fitted/predicted value)


train_res=cbind.data.frame(Actual=clnloandata_train$Interest.Rate,Fitted=fitted(fit),Error=residuals(fit))

View(train_res)


#We take out RMSE or root mean square Error to see the severity of the errors
#RMSE = summation of errors / degrees of freedom or No of records
#Close to 0, better it is. Anything above 5 or 10 would not be a good value for standard error
rmse_train=sqrt(mean(train_res$Error^2))


#MAPE or mean absolute percentage error is another measure that is used to check the %age of errors
#in the model


#We are now verifying the assumptions

library(ggplot2)
#Assumption 1: Error should follow normal distribution
ggplot(train_res,aes(Error))+geom_histogram()


#So , we find out the first assumption is correct


#Assumption 1: Homoscadascicity (x=fitted and y=error) fitted and error should be scattered or randomly distributed
ggplot(train_res,aes(x=Fitted,y=Error))+geom_point()


#So , 2nd assumption is also correct


#Testing our model on our test data set (here we use the function predict for that purpose)
ir_predict=predict(fit,newdata=clnloandata_test)
Testrs=cbind.data.frame(Act=clnloandata_test$Interest.Rate,pred=ir_predict)
View(Testrs)

#Finding RMS or root mean square
res=clnloandata_test$Interest.Rate-ir_predict
rmse_test=sqrt(mean(res^2))

#So, we have a stable model here and the errors in train and test are consistant.

#Sapiro wilk test and anderson darling test helps us in checking normality of a column.
#Sapiro wilk test works when the number of records are less than 5000. if more than 5000, we may use anderson darling test
#In both of these, ew get a p value. here null hypothesis is that there is a normal distribution, where alternate hypothesis
#is there is no normal distrbution. So if P is low, null mst go and if p is high(>0.05), null is true.