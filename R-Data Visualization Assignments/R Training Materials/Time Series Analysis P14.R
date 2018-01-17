#This is a practice assignment to demonstrate Time series analysis
#AUTHOR: Abhinaba Chakraborty
#LAST MODIFIED: 17th JAN 2018

#PART 1: 12th November 2017

#Setting the working directory
setwd("C:\\Users\\Abhinaba\\Desktop\\Edvancer Materials\\Data")

#Reading the required files upon which we would be doing a time series analysis
rain=read.csv("rain.csv")

#First we need to convert our data into time series format, here we just mention the point 
#where our series is going to start
raints=ts(rain,start=c(1813)) #Since we know our series starts with 1813 before hand

#trying to see that the data for how long data is there and wat are the various matrics associated
#with it
raints

#So we found out that there is 100 years of data
#Also, Frequency is given as 1, so this is a yearly data
#When Frequency is 12, then frquency becomes montly
#When Frequency is 52, then frequency becomes weekly

#We are now trying to plot the rains in the plot
plot(rain)
#We would see that we get a meaningless value here, so we need to do plot(raints)
plot(raints)
#We see proper data in the above scenario, and we conclude that there is only level pattern here

#Running the time series function
#Now, since here we can only see the level pattern, we would ideally run the SES or single exponential
#smoothing, hence
rainforecast=HoltWinters(raints,beta=F,gamma=F) #Since we do not have any other pattern than level
                                                #hence beta and gamma are set to false here

#Running and analyzing the SES object here
rainforecast
#We get an alpha value of around 0.02, which concludes that there is a very less dependency/weightage on 
#the previous value, ideally this value can range between 0 and 1, larger the value, higher the dependency 
#on the previous variable

#We also find out that the coefficient is around 24.67, which is same in context of linear/logistic regression
#In other words, its the starting point from where our eqn. starts. If we see the chart, we would find out that
#this is the value where the chart starts fluctuating or the MA or moving average

#We can also try and pass other combos to find out if there are any other patterns
#for e.g.
rainforecast=HoltWinters(raints,beta=T,gamma=T)
#The above code would give us an error since we are giving gamma as true, which signifies that our data has
#a seasonal pattern, now for that theory to work, there has to be monthy data, so that the algo. can find
#and verify patterns for atleast two years, which is not possible here since we are showing the data points
#in months

#Trying another combination by removing gamma
rainforecast=HoltWinters(raints,gamma=F)
#So, we found out that there are some trend pattern in our data as well

#------------------------------------------------------------------------------------------------------

#Trying to run the forecast with combination 1 first
rainforecast=HoltWinters(raints,beta=F,gamma=F)

#Trying to plot a graph for the same
plot(rainforecast)
#We would see a red line in the graph that shows the general trend or behaviour of the pattern

#We are now about to do the forecasting
#loading library forecast
library(forecast)

rf=forecast:::forecast.HoltWinters(rainforecast,h=3) #Predicting for upcoming 3 years
#forecast.HoltWinters is an unexported function from forecast which means you need three colons to access it.
#But you should never have to do this because it should be automatically found when you run forecast on a 
#the output from HoltWinters(). However, since here the later was not working. We had to use the former

#plotting the predicted values
plot(rf)
#So, we would see a light grey band, in which we would see three blue dots. Which are our predicted values
#and in this light grey band, we would also see a light grey area, which is our confidence interval. 

#Seeing the confidence bands
rf
#So our values can lie between the confidence interval forecasted in the same, so it shows us the values
#which can occur with 80%,90%,100% confidence intervals. The dark grey area denotes the 80% confidence 
#interval. So, our values can range between 19 to 30 considering a confidence interval of 80.

#------------------------------------------------------------------------------------------------------

#Trying this entire process again with combination 2 again (here we are considering that we have a trend
#pattern along with level pattern)
rainforecast=HoltWinters(raints,gamma=F)

plot(rainforecast)

rf=forecast:::forecast.HoltWinters(rainforecast,h=3) #Predicting for upcoming 3 years

plot(rf)

rf
#We find that we have a minimal trend pattern in our data where the values are fluctuating very less

#------------------------------------------------------------------------------------------------------

#Getting back to the original example here
rainforecast=HoltWinters(raints,beta=F,gamma=F)

#Now, we found out that the alpha value or weightage for P or level pattern is 0.02, however we can assign
#custom weightage for our data too in the foloowing ways
#So, we can write the above query as
rainforecast=HoltWinters(raints,alpha=0.5,beta=F,gamma=F) #here we give custom weight to alpha

#now, if we see the object
rainforecast

#Trying to plot a graph for the same
plot(rainforecast)
#So, it would give a fluctuating prediction as the red line for this would be very inconsistant
#This may harm the confidence interval along with the prediction/forecasting

#Predicting the values
rf=forecast:::forecast.HoltWinters(rainforecast,h=5) #Predicting for upcoming 5 years

plot(rf)
#Now, we would find out that the confidence interval is increasing over time, which is not a good sign
#as becasue in that case our model is not reliable. Thus, we may conclude that assigning a custom weightage
#of 0.5 for alpha was a very bad idea in this context as it will result into a lot of variation in my values
#and there is no credibility of our forecasting.

#So we can conclude that it is always the best idea to go with the weightage the algo provides us, however
#We may have the option to provide custom weightage for the same 

rf

#------------------------------------------------------------------------------------------------------

#Verifying assumptions
# 1. Normality of errors/residuals
hist(rf$residuals) #The histogram should follow a normal distribution

# 2. Verifying that there is no auto correlation of errors
acf(rf$residuals[-1],lag.max = 20) #acf is the auto correlation function or the plot that shows 
                                   #auto correlation
#So the above plot would tell us what residuals are crossing the confidence intervals with a lag of 
#different values. So, the blue dotted line is the confidence interval for the residuals and the horizontal
#line passing through the middle is the centre of confidence interval of the residual. the upper vertical
#and lower vertical lines in the horizontal surface represent auto correlation values when the series is 
#dropped by 1 lag, 2 lag, 3 lag and so on and compared to the main series. If any vertical lines 
#(both upward & Downward) crosses the blue horizontal line, then we may conlcude that there is a correlation
#between that particular lag instance and the main series.

#This may be used to verify the first assumption as well
qqnorm(as.numeric(rf$residuals))
#for checking sample qunatile Vs theoretical quantile.
#Alternatively, we can also use a normal histogram and/or Sapiro wilks test and/or anderson darling test for
#checking normality


#PART 2: 18th November 2017
skirts=read.csv("skirts.csv")
skirts=ts(skirts,start=c(1866)) #Since we know our series starts with 1866 before hand
plot(skirts)


#We conclude that there is a level and trend pattern in the above chart
skirtforecast=HoltWinters(skirts,gamma = F) #Ideally we need to set values for alpha, beta and gamma,
                                            #but since we have level and trend component in our charts
                                            #we already knew that alpha and gamma will be true, hence we
                                            #didn't mention it in the function.
plot(skirtforecast) 
skirtforecast


#Now we are forecasting for the next 20 years to look at the pattern
skirtfuture=forecast:::forecast.HoltWinters(skirtforecast,h=20) #Predicting for upcoming 20 years

plot(skirtfuture)
#We would see that the confidence interval is varying a lot over the next 20 years and this makes our data
#less reliable, thus we can try and do the same we did above for a shorter time period
skirtfuture=forecast:::forecast.HoltWinters(skirtforecast,h=5) #Predicting for upcoming 5 years

plot(skirtfuture)

skirtfuture


#Verifying the assumption
#1. Normality of errors
hist(skirtfuture$residuals)
qqnorm(as.numeric(skirtfuture$residuals))


#2. Verifying that there is no auto correlation of errors
acf(skirtfuture$residuals[-c(1,2)],lag.max = 30) #acf is the auto correlation function or the plot that shows 
#auto correlation

#We found out that there is one spike that is going out of the designated limits, now in order to conclude
#that this is statistically significant, we do a box ljung test
#box ljung test tells us if the spikes that are crossing the border lines in the acf plot are statistically
#significant or not
#Null hypothesis for this box ljung test is that there is no auto correlation in betweeb the errors,
#that is the values are independent
#Alternate hypotheses is that there is auto correlation among the errors
Box.test(as.numeric(skirtfuture$residuals),lag=20,type="Ljung-Box")
#SInce the p-value is high, we accept our null hypothesis




#New data set
souvenir=read.csv("souvenir.csv")

souvenirts=ts(souvenir,frequency=12,start=c(1987,1))
#Here the frequency is set to 12, whcih signifies that the data is present in a monthly fashion
#also, since the frequncy of the data is monthly, the start function will allow us to enter the month
#along with the year as well, here 1987 is the year and 1 is the month

souvenirts
#So running the above object would show us the data broken down by Year and monthly fashion

plot(souvenirts)
#So Here we see all the three patterns or components namely level, pattern and cyclic patterns as the 
#data is getting repeated in a yearly fashion, also we see an increase in sales year after year.

#Now, the problem with exponential smoothing is that it can't handle the pattern or trend of increasing
#seasonality over a period of time, so we need to balance it out first i.e.; we need to control that and
#then use our model on top of it.
#So in order to predict or forecast the future values in a scenario like this we need to first control 
#the variation and then do the forecasting


#We are doing some basic transformation to our data to make to simplify the pattern
souvenirts=log(souvenirts) #This is somewhat similar to the concept in linear regression where we do
                           #some basic transformation if our data doesn't follow normal distribution


plot(souvenirts)

# Sincew we concluded that there are all the 3 main patterns in our data, we are now passing the object
#holt winters method (we have not mentioned any values for alpha, beta or gamma as all of them are true)
souvenirforecast=HoltWinters(souvenirts)

#Viewing the data set
souvenirforecast

#plotting the trend
plot(souvenirforecast)

#forecasting the values
souvenirfuture=forecast:::forecast.HoltWinters(souvenirforecast,h=48) #Predicting for upcoming 48 months

#plotting the same
plot(souvenirfuture)
#Now the above plot will show the values in the log scale, we need to convert the same in an exponential
#scale

#Viewing the forecasted values
souvenirfuture

#Verifing assumptions
#acf plot
acf(souvenirfuture$residuals,lag.max = 30,na.action = na.pass) 
#acf is the auto correlation function or the plot that shows auto correlation
#na.action = na.pass signifies how nas will be treated while verifying the assumption

#normality of errors
hist(souvenirfuture$residuals)
qqnorm(as.numeric(souvenirfuture$residuals))


#ARIMA (Auto regressive integrated moving average)
#Identifying patterns in our data
plot(decompose(souvenirts))
#The above line would help us to distinguish and understand each pattern of data


#ARIMA models have 3 parameters and is generally written as ARIMA(p,d,q)
auto.arima(souvenirts,trace = T)
#The above function would help us to get the best model


#WE need to now run the model in our data set for which we use arima
arimafit=arima(souvenirts,order=c(2,0,0),seasonal=c(1,1,0))

#forecasting
arimafuture=forecast:::forecast.Arima(arimafit,h=48)

#Plotting the same
plot(arimafuture)

#Viewing the values for the same
arimafuture

#Verifying assumptions (although this is optional for ARIMA)
#acf plot
acf(arimafuture$residuals,lag.max = 30,na.action = na.pass) 


#normality of errors
hist(arimafuture$residuals)
qqnorm(as.numeric(arimafuture$residuals))


#DYNAMIC REGRESSION (When we have to forecast the value of a variable based on another variable)
library(fpp)
#Loading the library fpp that contains the data set for quarterly changes in  US consumption 
#and personal income

#Plotting the same
plot(usconsumption,xlab="Year",main="Quarterly changes in  US consumption and personal income")
#So we would be able to see the data fr two columns here, the consumption and income, this data is
#represented quarter wise

fit=auto.arima(usconsumption[,1],xreg=usconsumption[,2]) #We are trying to predict consumption based on 
                                                         #income, hence xreg is mentioned as income column
#Viewing the same                                       
fit

#Running the ARIMA 
usc=Arima(usconsumption[,1],xreg=usconsumption[,2],order = c(1,0,2))
usc

#Trying to predict the values for the next two quarters
#Passing the values for income in the next two quarters
new=cbind.data.frame(income=c(0.8,0.5)) #Here we are giving the values  of income for the next 2 quarters

#predicting the values
usfuture=forecast:::forecast.Arima(usc,xreg=new$income)

#Plotting the values for reference
plot(usfuture)

#Visualising the above
usfuture


#lagged variable
plot(insurance,xlab="Year",main="Insurance advertising and quotations")
#Insurance data set here has quotes and tv advertisements column, the above graph shows the pattern
#for Quotes and TV adverts

Advert=cbind(insurance[,2],c(NA,insurance[1:39,2]),
             c(NA,NA,insurance[1:38,2]),
             c(NA,NA,NA,insurance[1:37,2]))
#We are lagging the series by one component one by one
#So lagging the series down by one step, two step and three step respectively would help us find wether
#there is a relation between first days advertisement value with the second day, third day and/or fourth
#day. (It will be clearer while we view the Advert data frame in line 356)

#Creating the column names for the above data
colnames(Advert)=paste("Adlag",0:3,sep="")

#Looking at the data set
View(Advert)


#We are now trying to predict the values of advertisements based on quotes with various lags
#First, we are only considering the column Adlag0 for prediction
fit1=auto.arima(insurance[4:40,1],xreg=Advert[4:40,1],d=0) #here d signifies the drift

#Second, we are now considering the column Adlag0 and Adlag1 for prediction
fit2=auto.arima(insurance[4:40,1],xreg=Advert[4:40,1:2],d=0) #here d signifies the drift

#Third, we are now considering the column Adlag0, Adlag1 and Adlag2 for prediction
fit3=auto.arima(insurance[4:40,1],xreg=Advert[4:40,1:3],d=0) #here d signifies the drift

#Fourth, we are now considering the column Adlag0, Adlag1, Adlag2 and Adlag3 for prediction
fit4=auto.arima(insurance[4:40,1],xreg=Advert[4:40,1:4],d=0) #here d signifies the drift


#Checking the aic of all the above to check which has the lowest aic
fit1$aic
fit2$aic
fit3$aic
fit4$aic
#So, from above we can conclude that fit 2 or the second model has the lowest aic

#So, now we are trying to create an auto.arima and fit the 2nd model on top of it
fit=auto.arima(insurance[,1],xreg=Advert[,1:2],d=0)

#Now we are predicting the future values based on the current values
fc=forecast(fit,xreg=cbind(rep(8,20),c(Advert[40,1],rep(8,19))),h=20)
#Here we are creating the the regressior, so for that we are creating 20 rows with 8 as their values
#We are also taking the last value of the main series that is 8.72860 and repeating it 19 times
#this is done in order to predict the future values, here it would predict the data for next 20 months

#We are now plotting the same
plot(fc,main="forecast quotes with advertising set to 8",ylab="Quotes")

#NOTE: for doing forecasting which is dependent on another variable we either need to know the future values
#of xregs(the regressor variable) or assume the values of the same
