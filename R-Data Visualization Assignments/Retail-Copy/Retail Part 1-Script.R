#This is the part 1 of the Retail project

#Total records: 3338

#Author: Abhinaba Chakraborty


#Setting the workinng directory
setwd("C:/Users/Abhinaba/Desktop/Edvancer Materials/Project/R Projects/Retail-Copy")


#Loading the term deposit  data set
retdata=read.csv("store_train.csv")

#Viewing the dataset
View(retdata)


library(dplyr)

#Question 1:
#what is the total sales (sum of all sales) of Supermarket Type1 in area Kennebec County, ME?
#Filtering the sales details
Sales_retdata=retdata %>% mutate(total_sales=sales0+sales1+sales2+sales3+sales4) %>% filter(Areaname=="Kennebec County, ME" & store_Type=="Supermarket Type1")
View(Sales_retdata)
sum(Sales_retdata$total_sales)

#Answer 38680


#Question 2:
#Should storecode be included in building models?
#Answer: No


#Question 3:
# should country be treated as numeric type or character?
#Answer: character


#Question 4:
#Find out number of unique categories of variable Areaname.
length(unique(retdata$Areaname))

#Answer: 1891 unique areas


#Question 5:
#For store type grocery store what is the response rate ? [ what % of obs have response value as 1 ]  
#Round off to two decimal digits. 

Sales_resp_data0=nrow(retdata[retdata$store_Type=="Grocery Store" & retdata$store==0,])
Sales_resp_data1=nrow(retdata[retdata$store_Type=="Grocery Store" & retdata$store==1,])

round((Sales_resp_data1/(Sales_resp_data0+Sales_resp_data1))*100,2)
#or 

ret_data2=retdata[retdata$store_Type=="Grocery Store",]
round(prop.table(table(ret_data2$store)),2)

#42.13 %




#Question 6:
#Do all the sales variable follow normal distribution?
hist(retdata$sales0)
hist(retdata$sales1)
hist(retdata$sales2)
hist(retdata$sales3)
hist(retdata$sales4)

#Answer: No


#Question 7:
#Number of outliers for total sales based on following limits (q1-1.5*IQR, q3+1.5*IQR)?
Sales_retdata2=retdata %>% mutate(total_sales=sales0+sales1+sales2+sales3+sales4)
boxplot(Sales_retdata2$total_sales)
length(Sales_retdata2$total_sales)

min(Sales_retdata2$total_sales)
max(Sales_retdata2$total_sales)

summary(Sales_retdata2$total_sales)
#Q1: 3422
#Q3: 4969

IQR_Totalsales=IQR(Sales_retdata2$total_sales)
low_bound=(3422-(1.5*IQR_Totalsales))
upp_bound=(4969+(1.5*IQR_Totalsales))


CLn_SalesData=Sales_retdata2 %>% filter(total_sales>=-1101.875 & total_sales<=7289.125)


Clean_Values_Cnt=nrow(CLn_SalesData)

Outl_cnt=nrow(Sales_retdata2)-Clean_Values_Cnt

#Answer: 140


#Question 8:
#which store type has maximum variance in total sales?
Sales_retdata3=retdata %>% mutate(total_sales=sales0+sales1+sales2+sales3+sales4)
tapply(Sales_retdata3$total_sales,Sales_retdata3$store_Type,var)

#Answer: Grocery store



#Question 9:
#How many dummies will you create for variable state_alpha?
length(unique(retdata$state_alpha))

#Answer: 53 


#Question 10
#What should be the type of categorical variable when using the function randomForest?

#Answer: factor