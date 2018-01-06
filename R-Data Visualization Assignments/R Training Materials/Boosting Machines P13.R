#This part demonstrates the use of boosting machines in order to solve regression or classification problems
#Author: Abhinaba Chakraborty
#Last modified: 06/01/2018

#Including the required packages
library(dplyr)
library(kknn)
library(gbm)
library(randomForest)
library(ggplot2)
library(cvTools)


#Creating a customised function for creating our dummies
CreateDummies = function(data,var,freq_cutoff=0){
  t=table(data[,var])
  t=t[t>freq_cutoff]
  t=sort(t)
  categories=names(t)[-1]
  
  for( cat in categories){
    name=paste(var,cat,sep="_")
    name=gsub(" ","",name)
    name=gsub("-","_",name)
    name=gsub("\\?","Q",name)
    name=gsub("<","LT_",name)
    name=gsub("\\+","",name)
    name=gsub(">","GT_",name)
    name=gsub("=","EQ_",name)
    name=gsub(",","",name)
    data[,name]=as.numeric(data[,var]==cat)
  }
  data[,var]=NULL
  return(data)
}

#Setting the working directory
setwd("C:/Users/Abhinaba/Desktop/Edvancer Materials/Data")


#Loading the data set
bd=read.csv("bike_sharing_hours.csv",stringsAsFactors = F)

#Viewing the data set
View(bd)

#This is a regression problem where we have to predict the count of bikes that can be taken in a day

#Removing the unwanted cols or variables from the data set
bd=bd %>% select(-yr,-instant,-dteday,-temp,-casual,-registered)

#Now we are using the function we created for the creation of dummies
for(var in c("season","mnth","hr","weekday")){
  bd=CreateDummies(bd,var,500)
}

#Viewing the data set with the dummies
View(bd)

#Splitting the data into train and test
set.seed(2)
s=sample(1:nrow(bd),0.8*nrow(bd))
bd_train=bd[s,]
bd_test=bd[-s,]

#Ideally, we follow the steps like removing multicollinearity or variables with high pr values etc.
#Which is also what we need to follow for GBM gradient boosting machine as well, however, for the 
#sake of this example we are directly trying to solev the eqn. since the moto here is to understand 
#GBM or gradient boosting machine


#Following the simple linear model first
fit=lm(cnt~.-weekday_6,data=bd_train) #Removed weekday_6 because its an exact replica of another variable
                                      #weekday, and it can thus contain multicollinearity 

#Predicting the values for the same
test.pred=predict(fit,newdata = bd_test)

#Calculating RMSE for the above eqn.
(test.pred-bd_test$cnt)**2 %>% mean() %>% sqrt()
#We would get a vaalue of 111.455


#Stacking 

#In order to do stacking we first need to do cross validation first
#So, we are doing cross validation here using a custom function
mykfolds=function(nobs,nfold=5){ #nobs is the no. of variables and nfold is the number of parts
                                 #So, the number of variables in each part would be nobs/nfold
t=cvFolds(nobs,K=nfold,type='random') #cvFolds function is used to split the data into train and test 
                                        #Internally, its basically used for cross validation
  
folds=list() #Creating an empty list

#Here we are creating a train and test for our data through an iterating step and storing that data
#in a list for each fold that we have selected
for(i in 1:nfold){
    test=t$subsets[t$which==i]
    train=t$subsets[t$which!=i]
    folds[[i]]=list('train'=train,'test'=test)
  }
return(folds)
}

#running the above function with our choosen records
myfolds=mykfolds(nrow(bd_train),10)

#We are now creating a data set where all the values are 0
bd_train_layer1=data.frame(rf_var=numeric(nrow(bd_train)),gbm_var=numeric(nrow(bd_train)))
#So bascially what we are going to do is to create an empty data set with values set as 0 so as to
#get predicted values from our data set. In this case we have chosen random forest and gbm technique.
#to be stacked in our model. We would applying stacking on the same, so as to get combined values 
#obtained from both the techniques.

#Viewing the above data layer
View(bd_train_layer1)


#So, now we are iterating through the list to do the stacking
for(i in 1:10){
  print(c(i))
  fold=myfolds[[i]]
  
  train_data=bd_train[fold$train,]
  test_data=bd_train[fold$test,]
  
  print('rf')
  
  rf.fit=randomForest(cnt~.-weekday_6,data=train_data,ntree=100,mtry=10)
  
  rf_score=predict(rf.fit,test_data)
  
  print('gbm')
  
  gbm.fit=gbm(cnt~.-weekday_6,data=train_data,distribution="gaussian",n.trees=100,interaction.depth = 3)
  #Distribution is made as gaussian so that it runs our regression problem
  #n.trees are the no. of trees that needs to be sequentially grown for the gbm or generalised boosting
  #regression
  #interaction depth is bascially the number of combination it would try internally
  
  gbm_score=predict(gbm.fit,newdata=test_data,n.trees=100)
  
  bd_train_layer1$rf_var[fold$test]=rf_score
  
  bd_train_layer1$gbm_var[fold$test]=gbm_score
}

#We are now applying our trained model eqn. on the test data
#So, We are first creating a data set where all the values are 0 for our test data
bd_test_layer2=data.frame(rf_var=numeric(nrow(bd_test)),gbm_var=numeric(nrow(bd_test)))

#Now we are running the random forest and gbm on the train data again, so as to get the values for 
#test data set to work
full.rf=randomForest(cnt~.-weekday_6,data=bd_train,ntree=100,mtry=10)

full.gbm=gbm(cnt~.-weekday_6,data=bd_train,distribution="gaussian",n.trees = 100,interaction.depth = 3)

#Predicting the values for rf/random forest and gbm for our test data
#random forest
bd_test_layer2$rf_var=predict(full.rf,newdata=bd_test)

#GBM
bd_test_layer2$gbm_var=predict(full.gbm,newdata=bd_test,n.trees=100)


#Creating the final stacked model
bd_train_layer1$cnt=bd_train$cnt
bd_test_layer2$cnt=bd_test$cnt

lin.model=lm(cnt~.,data=bd_train_layer1)

#predicting the values
test.predict=predict(lin.model,newdata=bd_test_layer2)

#rmse
(test.pred-bd_test_layer2$cnt)**2 %>% mean() %>% sqrt()
#We should see that the error has reduced.
