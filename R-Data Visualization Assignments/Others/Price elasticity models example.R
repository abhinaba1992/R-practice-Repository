#This piece of code demonstrates the working of a price elasticity model in R with a simple example
#Source: http://www.salemmarafi.com/code/price-elasticity-with-r/

#Data Dictionary
# Sales: the total eggs sold on the given day
# Price Eggs: the price of the eggs on the day
# Ad Type: the type of poster – 0 is the egg poster, 1 is the cookie poster.
# Price Cookies: the price of cookies on the day




#Loading and viewing the files

setwd('C:\\Users\\Abhinaba\\Desktop\\Edvancer Materials\\Data')

sales.data=read.csv('supermarket.csv')

View(sales.data)


#data inspection steps
#Checking  the data types
str(sales.data)

#Lets change the data type of the ad type variable as factor
sales.data$Ad.Type=as.factor(sales.data$Ad.Type)


#We are now checking the different values of the numeric fields for analysis
summary(sales.data)


#Let's assume we want to run a regression model for the prediction

#Splitting of data in train and test (Note taht since in this example, the number of rows is less(30), we may not choose
#to do the data splitting)

set.seed(2)
s=sample(1:nrow(sales.data),0.7*nrow(sales.data))

sales.data_Train=treaddata[s,]
sales.data_Test=treaddata[s,]



#You can check the correlation between various variables in the following way (this is just for your reference and you may
#not choose to do something like this for this example)
View(cor(sales.data$Sales,sales.data$Price.Eggs))



#Running the model
# We can run the entire regression or add each variable to see the impact on the regression model. 
# Since we have few predictors lets choose the latter option for fun.
#We are creating 3 models for our reference

# Create models
m1=lm(Sales~.-Ad.Type-Price.Cookies,data=sales.data) #By only considering the that the sales of eggs are affected by price 
                                                     #of eggs
m2=update(m1,.~.+Ad.Type) #By adding AdType element 
m3=update(m2,.~.+Price.Cookies) #By adding Price of Cookies element

#The below library would help us compare the three different models
library(memisc)

mtable(m1,m2,m3)

View(mtable(m1,m2,m3))


#Verifying assumptions and checking the model efficiency

# Linearity Plots
par(mfrow=c(2,2))
plot(m3) #This would help us to understand the hetroscadastic/homoscadastic nature of our data along with the QQ plot 
         #in order to better understand the Normality of the errors
par(mfrow=c(1,1))

# Multi-collinearity
library(car)
vif(m3) # variance inflation factors (All the values are under 5, so we can relax with that part) 
vif(m3) > 5 # Checking if any VIF values are less than 5.

# Diagnosis: Nonlinearity
crPlots(m3)

#We see that there is definitely some issues with linearity but not to an extent that it is a cause 
#for concern for the purpose of demonstration. So we keep calm, and move on.


#Lastly we want to test independence of the residuals using the Durban Watson Test:

# Diagnosis: Non-independence of Errors 
# We want a D-W Statistic close to 2
durbinWatsonTest(m3)

#The output shows that there is no autocorrelation issues in the model

# Price Elasticity
# We now have our model:
# Sales of Eggs = 137.37 – (16.12)Price.Eggs + 4.15 (Ad.Type) – (8.71)Price.Cookies
# 
# Own Price Elasticity
# To calculate Price Elasticity of Demand we use the formula:
# PE = (ΔQ/ΔP) * (P/Q)
# 
# (ΔQ/ΔP) is determined by the coefficient -16.12 in our regression formula.
# To determine (P/Q) we will use the mean Price (4.43) and mean Sales (30).
# 
# Therefore we have PE = -16.12 * 4.43/30 = -2.38
# 
# This means that an increase in the price of eggs by 1 unit will decrease the sales by 2.38 units.
# 
# Cross Price Elasticity
# To calculate Cross Price Elasticity of Demand we are essentially looking for how the price of cookies 
#impacts the sales of eggs. So we use the formula:
#   
#   CPEcookies = (ΔQ/ΔPcookies) * (Pcookies/Q)
# 
# We know from our regression that (ΔQ/ΔPcookies) is the coefficient of Price of Cookies (-8.71).
# We use the mean price of cookies and mean sales for the rest of the formula giving (4.37/30)
# 
# CPEcookies = -8.71 * (4.37/30) = -1.27
# 
# This means that an increase in the price of cookies by 1 unit will decrease the sales of eggs by 1.27 units.
# 
# Interpretation
# We now know that the price of eggs and price of cookies are complementary to one another in this scenario. 
#Since you only sell too products, one explanation could be that people who come in for cookies and eggs would 
#rather get them elsewhere if the price is too high.
# 
# Also, it means that if you had to choose between a price cut on cookies or eggs, go with cookies!
#   
#   Next steps
# You are now in an ideal situation where you can run an optimization function to set the right price for both 
#cookies and eggs.
# This is out of the scope of this post, but if you’re interested in doing that check out R’s optim() function ~ or 
#leave a comment below

#Running the price elasticity

# Calculate Price Elasticity
PE=as.numeric(m3$coefficients["Price.Eggs"] * mean(sales.data$Price.Eggs)/mean(sales.data$Sales))
CPEcookies=as.numeric(m3$coefficients["Price.Cookies"] * mean(sales.data$Price.Cookies)/mean(sales.data$Sales))

# Print Results 
PE
CPEcookies



#Handling the adds


# Subset the data
sales.adEggs = subset(sales.data,Ad.Type==0)
sales.adCookies = subset(sales.data,Ad.Type==1)

# Diagnostic on subsets' means and if they are different ... they are. 
wilcox.test(x=sales.adCookies$Sales,y=sales.adEggs$Sales,exact=F,paired=T)

# On average, does the advert for eggs generate higher sales for eggs?
mean(sales.adEggs$Sales) >= mean(sales.adCookies$Sales)
