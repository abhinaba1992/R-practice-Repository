#This portion covers the concepts like two sample t-tests (paired and unpaired T-tests), ANOVA and Chi Square tests
#Author: Abhinaba Chakraborty
#last Updated: 26.11.2017

#Setting the work directory
setwd("C:/Users/chakrabortyab/Desktop/R Practice/Data")
#setwd("C:/E drive/Docs/R Practice docs/Data")

#Reading the values
winedata=read.csv("winequality-red.csv",sep=";")

#Displaying the same for cross checking
View(winedata)


#Performing Single sample T-Tests (Two sided test i.e; hO != ha)
#Testing a value for null hypothesis where the null hypthesis is 6.10
# t value is calculated on the formulae for one sample t test 
#(t = (sample mean) -(mean for null hypothese)/ (sample standard deviation)/ square root (total number of values)  )
# df is degrees of freedom is number of records - 1 (n-1) or (nrow(winedata)-1) 
# P-value is the probability from minus infinity to the Border line value in the symetric chart. (its basically the area
#under the rejection region)
# Thus for eg: with a confidence level of 95 % the broder line value would be 1.96
# so the P - value here would be calculated from minus infinity to 1.96
# if the p-value is greater than the level or significance or error (alpha) we accept the nulll hypothesis.
# Or else we reject the null hypothesis


t.test(winedata$fixed.acidity,mu=6.10) #reject


#By default the level of significance is 95%, if you wish to change it, we do the following
t.test(winedata$fixed.acidity,mu=6.10,conf.level = 0.99) #reject


#If we are going for a lesser than or greater test for alternate hypothesis, we do the following
t.test(winedata$fixed.acidity,mu=6.10,alternative = "less") #accept

t.test(winedata$fixed.acidity,mu=6.10,alternative = "greater") #reject

#---------------------------------------------------------------------------------------------------

# Paired T-tests (It is used to compare two different attributes of a common entity or group or comparing a before and 
# after scenario), we can think of this as comparing two different columns
# By default the Mu value is 0 for paired t-tests
t.test(winedata$free.sulfur.dioxide,winedata$total.sulfur.dioxide, paired= TRUE)
# here Ho = Mu1 - Mu0 = 0 (null hypothesis) and 
#      Ha = Mu1 - Mu0 != 0 (alternate hypothesis) 


# If we want to manually pass a value for null hypothesis or Mu we can do the following
t.test(winedata$free.sulfur.dioxide,winedata$total.sulfur.dioxide, paired= TRUE,mu=0.5)

#---------------------------------------------------------------------------------------------------


#Unpaired T-tests (It is used to compare the same attribute for two different entities or groups)
# you can think that here the comparison happens row wise
# filtering the alcohol quality entity
alcohol_quality3=winedata$alcohol[winedata$quality==3]
alcohol_quality7=winedata$alcohol[winedata$quality==7]
# here Ho = Mu1 - Mu0 = 0 (null hypothesis) and 
#      Ha = Mu1 - Mu0 != 0 (alternate hypothesis)


# When we do a unpaired T-test, we need to check for a variance as well and we do a variance test.
# it is also called a f-test. this is done, because the number of records for two entity group can be different

#We do it in the following way
var.test(alcohol_quality3,alcohol_quality7)


# We accept the null hypothesis if we get a highr P-value from the above f test
# We then do the following
t.test(alcohol_quality3,alcohol_quality7, var.equal = TRUE) # We can specify the value for var.equal to be true or valse
# based on the result of the f- test. i.e; if we accept the null hypothesis for the
# f - test then var.equal = TRUE or else it's FALSE.
# adding var.equal attribute automatically makes sure that
# an unpaired t - test has to be done.

#---------------------------------------------------------------------------------------------------
#ANOVA
# Now unpaired T test would help to compare two different entities r groups, however if we want to compare
# Multiple entities or groups, we go for ANOVA, here we basically check for variance and thus a f-test is
# used in this scenario.
# Here the null hypothese is that the difference between means of all categories is equal to 0
# While alternat hypothese is that the difference between means of all categories is not equal to 0

# Syntax breakdown aov(<numerical data> ~ as.factor(<categorical data>), data = <dataset>)
fit = aov(alcohol ~ as.factor(quality), data=winedata)

#We run the summary function to visualise all the values of our dataset
summary(fit)


#Now, if we want to identify the changes, we can use tapply for finding the differences
tapply(winedata$alcohol,winedata$quality,mean)


#Now if want to check if these are statistically different, we go for the following function
pairwise.t.test(winedata$alcohol,winedata$quality,p.adf="none")


#We can visualise the change in means through a box plot as well
boxplot(winedata$alcohol~winedata$quality)

#---------------------------------------------------------------------------------------------------
#Chi square test
# Chi square test for proportions
#It is used to check and verify for proportions 
# We have size differnt types of qualities of wine in our dataset, for e.g. 3,4,5,6,7 and 8
# Suppose it is claimed that we have equal proportions of qualities for all types where as for 7 and 8 we have half 
# proportions, ie; our proportions for 3,4,5,6,7 and 8 and 20%,20%,20%,20%,10% and 10% respectively.
#  (or 0.20,0.20,0.20,0.20,0.10,0.10)


#First we can check for proportions
table(winedata$quality)


#Then we check for the percentage of the quality values for our assurance
prop.table(table(winedata$quality))


#Here null hypothese is that the proportion for all the qualities are same
# Alternate hypothese is that the proportion for all the qualities are different 

#Now we can go for chi suqre test for proportions
chisq.test(table(winedata$quality),p=c(0.20,0.20,0.20,0.20,0.10,0.10))

#Comparing again with original proportion values
chisq.test(table(winedata$quality),p=c(0.03,0.03,0.42,0.39,0.12,0.01))


#Chi square test for independence (here, two independent values are compared)
# here null hypothesis is that the variables are independent 
# here alternate hypothesis is that the variables are dependent
chisq.test(table(winedata$fixed.acidity,winedata$alcohol))


# Now, for a chi square test for independence to work, there must be a minimum of 5 values in each cell.  
# Else, we get a warning and we need to go for a fisher test as well
# here null hypothesis is that the variables are independent 
# here alternate hypothesis is that the variables are dependent
fisher.test(table(winedata$fixed.acidity,winedata$alcohol))
