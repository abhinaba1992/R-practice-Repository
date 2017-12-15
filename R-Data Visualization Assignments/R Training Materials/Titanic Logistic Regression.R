# Data Loading

setwd("C:/Users/Abhinaba/Desktop/Edvancer Materials/Others/Practice")

Titanic_data=read.csv("train.csv")

View(Titanic_data)

#Data preparation
library(dplyr)

#Modifying pclass
table(Titanic_data$Pclass)


#omiting pclass type 2 by the n-1 thumb rule for dummies and also by the least  value rule
Titanic_data = Titanic_data %>% mutate(pcl_1=as.numeric(Titanic_data$Pclass==1),
                                       pcl_3=as.numeric(Titanic_data$Pclass==3)) %>% select(-Pclass)

glimpse(Titanic_data)
View(Titanic_data)

#Modifying sex
table(Titanic_data$Sex)
#omiting sex female by the n-1 thumb rule for dummies and also by the least  value rule
Titanic_data = Titanic_data %>% mutate(sex_m=as.numeric(Titanic_data$Sex=="male")) %>% select(-Sex)

glimpse(Titanic_data)
View(Titanic_data)


#Modifying age
#Since the columns have NAs, we can replace them in the following way
age.mean = round(mean(Titanic_data$Age, na.rm=TRUE))
Titanic_data$Age[is.na(Titanic_data$Age)] = age.mean

table(Titanic_data$Age)
#Sicne there is a lot of categoreis for ages, we do a grouping check
round(prop.table(table(Titanic_data$Age,Titanic_data$Survived),1),2)

#We are still having a lot of values, SO we check the max and min of the variables
max(Titanic_data$Age)
min(Titanic_data$Age)
range(Titanic_data$Age)

#Grouping the variable age based on age range and removing the veteran people age based on the n-1 rule for creating dummies
Titanic_data = Titanic_data %>% mutate(age_young=as.numeric(Titanic_data$Age>0 & Titanic_data$Age<=20),
                                       age_Adult=as.numeric(Titanic_data$Age>20 & Titanic_data$Age<=40),
                                       age_MidAged=as.numeric(Titanic_data$Age>40 & Titanic_data$Age<=60)
                                       #,age_Veteran=as.numeric(Titanic_data$Age>60)
) %>% select(-Age)


glimpse(Titanic_data)
View(Titanic_data)


#Modifying SibSp
table(Titanic_data$SibSp)

#omiting SibSp 5 by the n-1 thumb rule for dummies and also by the least  value rule
Titanic_data = Titanic_data %>% mutate(SibSp_0=as.numeric(Titanic_data$SibSp==0),
                                       SibSp_1=as.numeric(Titanic_data$SibSp==1),
                                       SibSp_2=as.numeric(Titanic_data$SibSp==2),
                                       SibSp_3=as.numeric(Titanic_data$SibSp==3),
                                       SibSp_4=as.numeric(Titanic_data$SibSp==4),
                                       SibSp_8=as.numeric(Titanic_data$SibSp==8)) %>% select(-SibSp)

glimpse(Titanic_data)
View(Titanic_data)

#Modifying Parch
table(Titanic_data$Parch)

#omiting parch 6 by the n-1 thumb rule for dummies and also by the least  value rule
Titanic_data = Titanic_data %>% mutate(parch_o=as.numeric(Titanic_data$Parch==0),
                                       parch_1=as.numeric(Titanic_data$Parch==1),
                                       parch_2=as.numeric(Titanic_data$Parch==2),
                                       parch_3=as.numeric(Titanic_data$Parch==3),
                                       parch_4=as.numeric(Titanic_data$Parch==4),
                                       parch_5=as.numeric(Titanic_data$Parch==5)) %>% select(-Parch)


glimpse(Titanic_data)
View(Titanic_data)


#Modifying Fare
table(Titanic_data$Fare)



#Sicne there is a lot of categoreis for fares, we do a grouping check
round(prop.table(table(Titanic_data$Fare,Titanic_data$Survived),1),2)

#We are still having a lot of values, SO we check the max and min of the variables
range(Titanic_data$Fare)
min(Titanic_data$Fare)
max(Titanic_data$Fare)

hist(Titanic_data$Fare)

#omiting the max fare(above 300) by the n-1 thumb rule for dummies and also by the least  value rule
Titanic_data = Titanic_data %>% mutate(Fare_3rdClss=as.numeric(Titanic_data$Fare>0 & Titanic_data$Fare<=100),
                                       Fare_2ndClass=as.numeric(Titanic_data$Fare>100 & Titanic_data$Fare<=200),
                                       Fare_1stClass=as.numeric(Titanic_data$Fare>200 & Titanic_data$Fare<=300)) %>% select(-Fare)

glimpse(Titanic_data)
View(Titanic_data)


#Modifying Embarked
table(Titanic_data$Embarked)

#Removing Q by n-1 thumb rule and least value rule
Titanic_data = Titanic_data %>% mutate(Emb_s=as.numeric(Titanic_data$Embarked=="S"),
                                       Emb_c=as.numeric(Titanic_data$Embarked=="C")) %>% select(-Embarked)


glimpse(Titanic_data)
View(Titanic_data)


#Removing Name, Ticket and Cabin as it didn't amke any sense to include them

Titanic_data=Titanic_data %>% select(-Name,-Ticket,-Cabin)

glimpse(Titanic_data)
View(Titanic_data)

#Splitting the data into train and text
s=sample(1:nrow(Titanic_data),0.7*nrow(Titanic_data))
Titanic_data_train=Titanic_data[s,]
Titanic_data_test=Titanic_data[-s,]


#Removing multicollienarity
library(car)


for_vif=lm(Survived~.-PassengerId,data=Titanic_data_train)

t=vif(for_vif)
sort(t,decreasing=TRUE)


#As per the VIF rule, omitting parch_0
for_vif=lm(Survived~.-PassengerId-parch_o,data=Titanic_data_train)
t=vif(for_vif)
sort(t,decreasing=TRUE)

#As per the VIF rule, omitting SibSp_0
for_vif=lm(Survived~.-PassengerId-parch_o-SibSp_0,data=Titanic_data_train)
t=vif(for_vif)
sort(t,decreasing=TRUE)


#As per the VIF rule, age_Adult
for_vif=lm(Survived~.-PassengerId-parch_o-SibSp_0-age_Adult,data=Titanic_data_train)
t=vif(for_vif)
sort(t,decreasing=TRUE)


#Now we remove all the above variables from our data set
Titanic_data_train_fit=Titanic_data_train %>% select(-PassengerId,-parch_o,-SibSp_0,-age_Adult)


#running logistic regression
fit=glm(Survived~.,family="binomial",data=Titanic_data_train_fit)

summary(fit)

#Removing the unecessary variables one by one through step function
fit=step(fit)


formula(fit)


fit1=glm(Survived ~ pcl_1 + pcl_3 + sex_m + age_young + age_MidAged + 
           SibSp_3 + SibSp_4 + parch_4 + Emb_s,family="binomial",data=Titanic_data_train)

summary(fit1)



fit1=glm(Survived ~ pcl_1 + pcl_3 + sex_m + age_young + age_MidAged + 
           SibSp_3 + SibSp_4 + Emb_s,family="binomial",data=Titanic_data_train)
summary(fit1)




fit1=glm(Survived ~ pcl_1 + pcl_3 + sex_m + age_young + age_MidAged + 
           SibSp_3 + Emb_s,family="binomial",data=Titanic_data_train)
summary(fit1)




fit1=glm(Survived ~ pcl_3 + sex_m + age_MidAged + 
           SibSp_3 + Emb_s,family="binomial",data=Titanic_data_train)
summary(fit1)


#All the variables with a higher pr(greater than 0.05) values are removed
Titanic_data_train$scores=predict(fit1,newdata = Titanic_data_train,type="response")
View(Titanic_data_train)


cutoff_data=data.frame(cutoff=0,TP=0,FN=0,FP=0,TN=0)
cutoffs=round(seq(0,1,length=100),3)

for(cutoff in cutoffs)
{
  predicted=as.numeric(Titanic_data_train$scores>cutoff)
  
  TP=sum(predicted==1 & Titanic_data_train$Survived==1)
  FP=sum(predicted==1 & Titanic_data_train$Survived==0)
  TN=sum(predicted==0 & Titanic_data_train$Survived==0)
  FN=sum(predicted==0 & Titanic_data_train$Survived==1)
  cutoff_data=rbind(cutoff_data,c(cutoff,TP,FP,TN,FN))
}


View(cutoff_data)


cutoff_data=cutoff_data[-1,]


cutoff_data=cutoff_data %>%
  mutate(P=FN+TP,N=TN+FP,Sn=TP/P, Sp=TN/N,
         dist=sqrt((1-Sn)**2+(1-Sp)**2)) %>%
  mutate(KS=abs((TP/P)-(FP/N))) %>%
  select(-P,-N)


View(cutoff_data)
