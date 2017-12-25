#This piece of R-Script helps us to demonstrate decision trees
#Author: Abhinaba Chakraborty


#We would need the following package for decision trees
library(tree) #Contains the function to be used for decision trees
library(ISLR) #Contains the data set named car seats 
library(dplyr) 

#Viewing the data set that we would be working upon
View(Carseats)

#This is a classification problem where our target variable is sales
#However we do not have a benchmar or bound for classification here as sales is a continuous data
#Lets assume that the customer has said that 8 is the benchmark for sales figure, that is anything
#above 8 is "yes" and anything less than or equal to 8 is "No", therefore
#Also, note that unlike logistic regression, classification in decision trees has to be done on a
#variable of type factor, on the flip side if the data type of the target variable is a factor
#so the machine understands that this is going to be in decision trees
mysales=Carseats %>% mutate(High=as.factor(ifelse(Sales<=8,"No","Yes"))) %>% select(-Sales)
View(mysales)


#Using the function tree for getting the decision trees
tree.carseats=tree(High~.,data=mysales)

#Plotting the decision tree
plot(tree.carseats)

#Plotting the texts in the decision tree, the yes will always be on the left while the no will always
#be on the right, pretty is an attribute for beautification and cex means the text size
text(tree.carseats,pretty=0,cex=0.5)


#It may also happen that the variable/column based on which the gini index has to be calculated turns
#out to be a numerical variable; in that case all the variables in that range are compared in a lower
#than and greater than scenario so as to find the best possible index [Decision trees, 5th Nov, 1:20:10]



#If we look at the tree below we would find the following on each node of the tree
# 1. "total number of records"
# 2. "the deviance": which is a value that is high if the group is hetrogeneous and low if the group is 
#homogeneous, so smaller it is, the its for us
# 3. "the target variable value which is more": that is yes/no whichever is having more values at that level 
# 4. "the percentage of yes/no in that level"
#[Decision trees, 5th Nov, 1:40:00-1:43:00]
tree.carseats


#We can also use the function summary to see a summarised view of our data
summary(tree.carseats)


#Trying to get a predicted set of values and creating a confusion matrix for the same
predicted_values=predict(tree.carseats,newdata=mysales,type="class")
Actual_Values=mysales$High
#Trying to find the confusion matrix for the above
table(Actual_Values,predicted_values)
#above eqn. would give us 36/400, 0.09% misclassfication, which is pretty good

#-----------------------------------------------------------------------------------------------------
#Now, We are streamliing/Extending the above piece of code in a much more organised way, as in we would 
#be following all the steps one by one that we did for linear/logistic regression


# 1. Splitting the data into train ad test
set.seed(2)
train=sample(1:nrow(mysales),200) # We could have also written train=sample(1:nrow(mysales),0.50*nrow(mysales))
mysales_train=mysales[train,]
mysales_test=mysales[-train,]

#---------------------------------------------------------------------------------------------------
# 2. We are predicting the values for our train data
tree.pred.train=predict(tree.carseats,newdata=mysales_train,type="class")
tree.orgi.train=mysales_train$High

#---------------------------------------------------------------------------------------------------
# 3. Creating the confusion matrix
table(tree.orgi.train,tree.pred.train)
#So from the above line, we get 20 misclassifications out of 200 variables, thus our misclassification
#rate is 20/200 which is 0.1 or 10 % misclassification, so we can conclude that this is a good model

#---------------------------------------------------------------------------------------------------
# 4. Testing the above in our test data we get
tree.pred.test=predict(tree.carseats,newdata=mysales_test,type="class")
tree.orgi.test=mysales_test$High

#---------------------------------------------------------------------------------------------------

# 5. Visualising the confusion matrix for the test data
table(tree.orgi.test,tree.pred.test)
#So from the above line, we get 16 misclassifications out of 200 variables, thus our misclassification
#rate is 16/200 which is 0.08 or 8 % misclassification, so we can conclude that this is a good model

# 6. plotting the same from the test data
plot(tree.carseats)
text(tree.carseats,pretty=0,cex=0.5)

#---------------------------------------------------------------------------------------------------

#Sometimes, it may also happen that we get a very high value for misclassification for our test data,
#this is because in decision tress, the algorithm  tries to generalise the eqn. too much on the train
#data set.that is when we train our model, then the eqn. would learn perfectly , but however, if we try
#and apply the same on a test data set, it may fail miserably. So, for that we need to do something called
#as pruning, so that the eqn. for decision trees yield a better result on test data or new data as well.


#So pruning helps us to make sure that our model is performing well on the test data or any other sample
#data other than the one in which it is trained, it basically cuts down our tree

#In order to do pruning, we need to first do a cross validation, we use the function cv.tree for doing
#cross validation
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)

#Now, we are plotting the same
plot(cv.carseats$size,cv.carseats$dev,type="b")
#So the point where the plot starts to increase again after falling is where we need to do the pruning
#From the above eqn. we can find that pruning would need to be done at 16 since it starts to increase
#back from 16

#So we are pruning at 16, so basically this means that we would have 16 terminal nodes
prune.carseats=prune.misclass(tree.carseats,best=16)

plot(tree.carseats)

text(tree.carseats,pretty=0,cex=0.5)

summary(prune.carseats)

#So applying the same model on the test data again
tree.pred.test=predict(tree.carseats,newdata=mysales_test,type="class")
tree.orgi.test=mysales_test$High
table(tree.orgi.test,tree.pred.test)

#---------------------------------------------------------------------------------------------------

#DOING REGRESSION ON DECISION TREES

#Like we get Yes/No in a classification problem through decision trees, we would get a average value
# of our target variable in case of performing a regression, where as in the classification problem
#using decision trees, we get the proportion of the highest class 

#First we are splitting the data into train and test
set.seed(3)
train=sample(1:nrow(Carseats),200) # We could have also written train=sample(1:nrow(mysales),0.50*nrow(mysales))
carseats_train=Carseats[train,]
carseats_test=Carseats[-train,]

#Predicting the values (here as the target variable passed to the tree function is a continuous variable
#the machine would automatically interpret as a regression problem)
rt.carseats=tree(Sales~.,data=carseats_train)

plot(rt.carseats)
text(rt.carseats,pretty=0,cex=0.8)

#Predicting the values
sales_pred_train=predict(rt.carseats,newdata=carseats_train)
sales_pred_test=predict(rt.carseats,newdata=carseats_test)
  
#In case of regression using decision trees, when we apply our eqn./model in an unknown data set, the values
#based on which our model is trained remain fixed, so for example if in one of the branches the criteria 
#for decision making is (Price<132.5) as per the train data set, then even in an unknown data set, the 
#Criteria would be same. Which is not in the case of linear regression because in linear regression 
#with even a small change in the value of the independent variable, the dependent variable gets affected.
#So this is a drawback of decision trees.


#Looking out for errors
rmse_train=sqrt(mean((sales_pred_train-carseats_train$Sales)^2))
rmse_test=sqrt(mean((sales_pred_test-carseats_train$Sales)^2))

rmse_train
rmse_test


#Now, we need to do a cross validation followed by a pruning
#So, Doing cross validation to for the same
set.seed(4)
cv.rt.carseats=cv.tree(rt.carseats)

#Now, we are plotting the same for us to find the number bracnhes to prune
plot(cv.rt.carseats$size,cv.rt.carseats$dev,type="b")

#So, from the above plotm we found out that we need prune the tree at 7
prune.rt.carseats=prune.tree(rt.carseats,best=7)

#Plotting the same
plot(prune.rt.carseats)
text(prune.rt.carseats,pretty=0)

#Predicting the values for train and test data
sales_pred_train=predict(prune.rt.carseats,newdata=carseats_train)
sales_pred_test=predict(prune.rt.carseats,newdata=carseats_test)

#Checking for errors
rmse_train=sqrt(mean((sales_pred_train-carseats_train$Sales)^2))
rmse_test=sqrt(mean((sales_pred_test-carseats_train$Sales)^2))

rmse_train
rmse_test