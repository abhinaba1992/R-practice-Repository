#This is the part 1 of the banking project

#Total records: 31647

#Author: Abhinaba Chakraborty

#Setting the workinng directory
setwd("C:/Users/Abhinaba/Desktop/Edvancer Materials/Project/R Projects/Banking-Copy")

#Loading the term deposit  data set
trmdepdata=read.csv("bank-full_train.csv")

#Viewing the dataset
View(trmdepdata)

#Question 1:
#Find mean of the variable age. Round off to 2 decimal places.

#Solution
round(mean(trmdepdata$age),2)

#Answer : 40.91



#Question 2:
#Total number of outliers present in the variable balance.Use 'Q1-1.5*IQR' to calculate lower limit 
#and 'Q3 + 1.5×IQR' to calculate upper limit. calculate the count of values in variable balance which
#are beyond these limits

#Solution
#Finding out the values for Q1 and Q2
summary(trmdepdata$balance)
#Q1: 72, Q3: 1414

#Finding the inter quartile range (Q3-Q1)
bal_Iqr=IQR(trmdepdata$balance)
#IQR: 1342

#Calculating Upper and lower bounds (that is the value for upper and lower whiskers)
#Lower bound
low_bound=(72-(1.5*1342))
#Upper bound
Upp_bound=(1414+(1.5*1342))
#Low bound: -1941
#Up bound: 3427

#Outlier Treatment or filtering the rows based upon our values
library(dplyr)
Cln_trmdepdata=trmdepdata %>% filter(balance>=-1941 & balance<=3427)

Clean_Values_Cnt=nrow(Cln_trmdepdata)
#Count of values which are not outliers is 28318

#Count of Outliers
Outl_cnt=nrow(trmdepdata)-Clean_Values_Cnt


#The outlier count is 3329



#Question 3: Find the variance of variable balance.

#Solution
var(trmdepdata$balance)

#Variance is 9273256



#Question 4: which function is used to remove multicollinearity among variables?

#vif() or variance inflation factor


#Question 5: Model with 'lower AIC' value is a better model or the model with 'higher AIC' value?

#Lower AIC is better



#Question 6: Should the variable ID be included in building the model?

#Answer: No



#Question 7: Does validation help in generalising the model?
#Answer: Yes



#Question 8: Whether the data given (bank-full_train) is a balanced or extremely 
#imbalanced data( ratios of class counts even more extreme than 5%:95%)?

#Answer: balanced


#Question 9: How is box plot upper whisker is calculated ? Choose out of these:

#Answer Q3+1.5*IQR



#Question 10: R2 or adjusted R^2, which metric to be used to check goodness of the model?
#Answer adjusted R^2