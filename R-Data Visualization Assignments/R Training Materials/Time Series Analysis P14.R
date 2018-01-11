#This is a practice assignment to demonstrate Time series analysis

#Setting the working directory
setwd("C:\\E drive\\Docs\\R Practice docs\\Data")

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

#-------------------------------------------------

#Verifying assumptions
# 1. Normality of errors/residuals
hist(rf$residuals) #The histogram should follow a normal distribution

# 2. Verifying that there is no auto correlation of errors
acf(rf$residuals[-1],lag.max = 20) #acf is the auto correlation function or the plot that shows 
                                   #auto correlation