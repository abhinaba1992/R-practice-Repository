setwd("C:/Users/Abhinaba/Desktop/Edvancer Materials/Others/Practice")

Pv_dataSet = read.table("poverty.txt", sep="\t",header=TRUE)

View(Pv_dataSet)


#Dealing with Brth15to17
table(Pv_dataSet$Brth15to17)

hist(Pv_dataSet$Brth15to17)


library(dplyr)

#Considering n-1 variables by the theory of dummies and leaset value rules
Pv_dataSet = Pv_dataSet %>% mutate(Brth15to17_10=as.numeric(Pv_dataSet$Brth15to17<=10),
                                   Brth15to17_20=as.numeric(Pv_dataSet$Brth15to17>10 & Pv_dataSet$Brth15to17<=20),
                                   Brth15to17_30=as.numeric(Pv_dataSet$Brth15to17>20 & Pv_dataSet$Brth15to17<=30),
                                   Brth15to17_40=as.numeric(Pv_dataSet$Brth15to17>30 & Pv_dataSet$Brth15to17<=40)) %>% select (-Brth15to17)


glimpse(Pv_dataSet)
View(Pv_dataSet)


#Dealing with Brth18to19

table(Pv_dataSet$Brth18to19)

hist(Pv_dataSet$Brth18to19)

#Considering n-1 variables by the theory of dummies and leaset value rules and alos removing the ones above 100
Pv_dataSet = Pv_dataSet %>% mutate(Brth18to19_60=as.numeric(Pv_dataSet$Brth18to19>40 & Pv_dataSet$Brth18to19<=60),
                                   Brth18to19_80=as.numeric(Pv_dataSet$Brth18to19>60 & Pv_dataSet$Brth18to19<=80),
                                   Brth18to19_100=as.numeric(Pv_dataSet$Brth18to19>80 & Pv_dataSet$Brth18to19<=30)) %>% select (-Brth18to19)



glimpse(Pv_dataSet)
View(Pv_dataSet)


#Dealing with ViolCrime
table(Pv_dataSet$ViolCrime)

hist(Pv_dataSet$ViolCrime)

#Considering n-1 variables by the theory of dummies and leaset value rules
Pv_dataSet = Pv_dataSet %>% mutate(ViolCrime_10=as.numeric(Pv_dataSet$ViolCrime<10),
                                   ViolCrime_20=as.numeric(Pv_dataSet$ViolCrime>10 & Pv_dataSet$ViolCrime<=20)) %>% select (-ViolCrime)



glimpse(Pv_dataSet)
View(Pv_dataSet)


#Dealing with Teenbirth
table(Pv_dataSet$TeenBrth)

hist(Pv_dataSet$TeenBrth)

#Considering n-1 variables by the theory of dummies and leaset value rules
Pv_dataSet = Pv_dataSet %>% mutate(TeenBrth_30=as.numeric(Pv_dataSet$TeenBrth<30),
                                   TeenBrth_40=as.numeric(Pv_dataSet$TeenBrth>30 & Pv_dataSet$TeenBrth<=40),
                                   TeenBrth_50=as.numeric(Pv_dataSet$TeenBrth>40 & Pv_dataSet$TeenBrth<=50),
                                   TeenBrth_60=as.numeric(Pv_dataSet$TeenBrth>50 & Pv_dataSet$TeenBrth<=60)) %>% select (-TeenBrth)


glimpse(Pv_dataSet)
View(Pv_dataSet)

#Dropping Location as we do not need it
Pv_dataSet=Pv_dataSet %>% select(-Location)


glimpse(Pv_dataSet)
View(Pv_dataSet)

#Splitting the dat ainto train and test
set.seed(2)
s=sample(1:nrow(Pv_dataSet),0.7*nrow(Pv_dataSet))
Pv_dataSet_Train=Pv_dataSet[s,]
Pv_dataSet_Test=Pv_dataSet[-s,]

summary(Pv_dataSet_Train)
View(Pv_dataSet_Train)

#Doing VIF to remove multicollinearity
library(car)

for_vif=lm(PovPct~.,data=Pv_dataSet_Train)
t=vif(for_vif)
sort(t,decreasing=TRUE)
summary(for_vif)

#All the vif values are under 5

#sO we do a summary and remove variables with higher pr value than 5%
for_vif=lm(PovPct~.-TeenBrth_50,data=Pv_dataSet_Train)
summary(for_vif)

for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40,data=Pv_dataSet_Train)
summary(for_vif)

for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40-Brth18to19_100,data=Pv_dataSet_Train)
summary(for_vif)


for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40-Brth18to19_100-ViolCrime_20,data=Pv_dataSet_Train)
summary(for_vif)


for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40-Brth18to19_100-ViolCrime_20-ViolCrime_10,data=Pv_dataSet_Train)
summary(for_vif)


for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40-Brth18to19_100-ViolCrime_20-ViolCrime_10-Brth15to17_40,data=Pv_dataSet_Train)
summary(for_vif)



for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40-Brth18to19_100-ViolCrime_20-ViolCrime_10-Brth15to17_40-TeenBrth_30,data=Pv_dataSet_Train)
summary(for_vif)



for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40-Brth18to19_100-ViolCrime_20-ViolCrime_10-Brth15to17_40-TeenBrth_30-Brth15to17_20,data=Pv_dataSet_Train)
summary(for_vif)



for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40-Brth18to19_100-ViolCrime_20-ViolCrime_10-Brth15to17_40-TeenBrth_30-Brth15to17_20-Brth15to17_30,data=Pv_dataSet_Train)
summary(for_vif)


for_vif=lm(PovPct~.-TeenBrth_50-TeenBrth_40-Brth18to19_100-ViolCrime_20-ViolCrime_10-Brth15to17_40-TeenBrth_30-Brth15to17_20-Brth15to17_30-TeenBrth_60,data=Pv_dataSet_Train)
summary(for_vif)



#Prediction
Pv_dataSet_Train_Pre=cbind.data.frame(Actual=Pv_dataSet_Train$PovPct,Fitted=fitted(for_vif),Error=residuals(for_vif))


View(Pv_dataSet_Train_Pre)



#RMSE 
rmse_train=sqrt(mean(Pv_dataSet_Train_Pre$Error^2))


#Verifying the assumptions

library(ggplot2)
#Assumption 1: Error should follow normal distribution
ggplot(Pv_dataSet_Train_Pre,aes(Error))+geom_histogram()


#Assumption 2: Homoscadascicity (x=fitted and y=error) fitted and error should be scattered or randomly distributed

ggplot(Pv_dataSet_Train_Pre,aes(x=Fitted,y=Error))+geom_point()



ir_predict=predict(for_vif,newdata=Pv_dataSet_Test)
Testrs=cbind.data.frame(Act=Pv_dataSet_Test$PovPct,pred=ir_predict)
View(Testrs)

#Finding RMS or root mean square
res=Pv_dataSet_Test$PovPct-ir_predict
rmse_test=sqrt(mean(res^2))
