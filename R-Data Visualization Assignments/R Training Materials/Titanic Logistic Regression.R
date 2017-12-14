# Data Loading

setwd("C:/Users/chakrabortyab/Desktop/R Practice")

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


#Removing Name, Passenger ID, Ticket and Cabin as it didn't amke any sense to include them

Titanic_data=Titanic_data %>% select(-Name,-PassengerId,-Ticket,-Cabin)

glimpse(Titanic_data)
View(Titanic_data)



#Removing multicollienarity





