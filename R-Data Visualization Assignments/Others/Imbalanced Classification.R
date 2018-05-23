#This piece of code gives us a generic idea of how to handle imbalanced classes in R intended for solving traditional 
#Classification problem (This is taken from an article at analytics vidhya)

#We need to ainstall the rose package for this
install.packages("ROSE")


#Loading the library ROSE
library(ROSE)

#Loading the inbuilt data hacide for showing this example
data(hacide)

#Setting the training part of the same data set into a new dat frame
df=data.frame(hacide.train)

#Viewing the data set hacide
View(df)

#Viewing the structure of the same
str(df)

#Checking the count of class
table(df$cls)
#0: 980
#1: 20

#Checking the percentage proportion
prop.table(table(df$cls))
#0: 98 %
#1: 2 %


#As we see, this data set contains only 2% of positive cases and 98% of negative cases. 
#This is a severely imbalanced data set. So, the imbalance would badly affect our prediction accuracy. 
#Let's build a model on this data. I'll be using decision tree algorithm for modeling purpose.

library(rpart)
treeimb <- rpart(cls ~ ., data = hacide.train)
pred.treeimb <- predict(treeimb, newdata = hacide.test)



#Let's check the accuracy of this prediction. To check accuracy, ROSE package has a function names accuracy.meas,
#it computes important metrics such as precision, recall & F measure.
accuracy.meas(hacide.test$cls, pred.treeimb[,2])

# These metrics provide an interesting interpretation. With threshold value as 0.5, Precision = 1 says there are no 
# false positives. Recall = 0.20 is very much low and indicates that we have higher number of false negatives. 
# Threshold values can be altered also. F = 0.167 is also low and suggests weak accuracy of this model.

#Viewing the ROC curve for the above prediction
roc.curve(hacide.test$cls, pred.treeimb[,2], plotit = F)


#over sampling
data_balanced_over = ovun.sample(cls ~ ., data = hacide.train, method = "over",N = 1960)$data
table(data_balanced_over$cls)


#In the code above, method over instructs the algorithm to perform over sampling. N refers to number of observations in
#the resulting balanced set. In this case, originally we had 980 negative observations. So, I instructed this line of 
#code to over sample minority class until it reaches 980 and the total data set comprises of 1960 samples


#Under sampling
data_balanced_under = ovun.sample(cls ~ ., data = hacide.train, method = "under", N = 40, seed = 1)$data
table(data_balanced_under$cls)


#Now the data set is balanced. But, you see that we've lost significant information from the sample. Let's do both 
#undersampling and oversampling on this imbalanced data. This can be achieved using method = "both". In this case, 
#the minority class is oversampled with replacement and majority class is undersampled without replacement.

data_balanced_both <- ovun.sample(cls ~ ., data = hacide.train, method = "both", p=0.5,N=1000, seed = 1)$data
table(data_balanced_both$cls)


#p refers to the probability of positive class in newly generated sample.

#The data generated from oversampling have expected amount of repeated observations. Data generated from undersampling
#is deprived of important information from the original data. This leads to inaccuracies in the resulting performance. 
#To encounter these issues, ROSE helps us to generate data synthetically as well. The data generated using ROSE is 
#considered to provide better estimate of original data


data.rose = ROSE(cls ~ ., data = hacide.train, seed = 1)$data
table(data.rose$cls)


#This generated data has size equal to the original data set (1000 observations). Now, we've balanced data sets using 4 
#techniques. Let's compute the model using each data and evaluate its accuracy.

#build decision tree models
tree.rose = rpart(cls ~ ., data = data.rose)
tree.over = rpart(cls ~ ., data = data_balanced_over)
tree.under = rpart(cls ~ ., data = data_balanced_under)
tree.both = rpart(cls ~ ., data = data_balanced_both)



#make predictions on unseen data
pred.tree.rose = predict(tree.rose, newdata = hacide.test)
pred.tree.over = predict(tree.over, newdata = hacide.test)
pred.tree.under = predict(tree.under, newdata = hacide.test)
pred.tree.both = predict(tree.both, newdata = hacide.test)



par(mfrow=c(2,2))



#AUC ROSE
roc.curve(hacide.test$cls, pred.tree.rose[,2])
#Area under the curve (AUC): 0.989

#AUC Oversampling
roc.curve(hacide.test$cls, pred.tree.over[,2])
#Area under the curve (AUC): 0.798

#AUC Undersampling
roc.curve(hacide.test$cls, pred.tree.under[,2])
#Area under the curve (AUC): 0.867

#AUC Both
roc.curve(hacide.test$cls, pred.tree.both[,2])
#Area under the curve (AUC): 0.798


#So, we may conclude that ROSE performs the best for handling imbalanced classes
