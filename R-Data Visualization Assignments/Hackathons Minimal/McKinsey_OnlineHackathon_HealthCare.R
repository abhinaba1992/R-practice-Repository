#Trying out deep learning with Neural Networks

#loading all the libraries
library(dplyr)
library(h2o)
library(data.table)


#Setting up directory
setwd('C:\\Users\\Abhinaba\\Desktop\\Hackathons\\McKinsey Online Hackathon - Health Care Analytics')


#Reading the file
#Loading test data along with campaign data
main_Train_Data=fread("train_ajEneEa.csv")


#Viewing the data set
#View(main_Train_Data)



#NA or missing value imputation

#Checking for NA and missing values
apply(main_Train_Data,2,function(x) sum(is.na(x)))
#only the bmi feature contains NA values (1462)
apply(main_Train_Data,2,function(x) length(which(x=="")))
#only the smoking_status feature contains missing values (13292)



#NA Value imputation(using mean)
#Seeing the avg of the bmi column
avg_result=round(mean(main_Train_Data$bmi,na.rm = TRUE),3)

#replacing NAs with average values
main_Train_Data$bmi[which(is.na(main_Train_Data$bmi))]=avg_result

#Missing value imputation
main_Train_Data$smoking_status[which(main_Train_Data$smoking_status=="")]="Unknown"



#Cross checking the counts
apply(main_Train_Data,2,function(x) sum(is.na(x)))
apply(main_Train_Data,2,function(x) length(which(x=="")))


#Feature Engineering and dummy creation
#View(main_Train_Data)


#Age
# typeof(main_Train_Data$age)
# quantile(main_Train_Data$age)

main_Train_Data=main_Train_Data %>% mutate(age_bracket_1=as.numeric(age>=0.00 & age<=1.00),
                                           age_bracket_2=as.numeric(age>=1.00 & age<=24.00),
                                           age_bracket_3=as.numeric(age>24.00 & age<=44.00),
                                           age_bracket_4=as.numeric(age>44.00 & age<=60.00),
                                           age_bracket_5=as.numeric(age>60.00 & age<82.00),
                                           age_bracket_6=as.numeric(age>82.00)
) %>% select(-age)



#Gender
# unique(main_Train_Data$gender)
# table(main_Train_Data$gender)

main_Train_Data = main_Train_Data %>% mutate(Gender_1=as.numeric(gender=="Female"),
                                             Gender_2=as.numeric(gender=="Male")
) %>% select(-gender)


#Hypertension
#unique(main_Train_Data$hypertension) # No transformation required


#Heart Disease
#unique(main_Train_Data$heart_disease) # No transformation required

#Ever married
# unique(main_Train_Data$ever_married)
# table(main_Train_Data$ever_married)

main_Train_Data = main_Train_Data %>% mutate(Marr_Stat=as.numeric(ever_married=="Yes")) %>% select(-ever_married)


#Work Type
# unique(main_Train_Data$work_type)
# table(main_Train_Data$work_type)

main_Train_Data = main_Train_Data %>% mutate(WT_child=as.numeric(work_type=="children"),
                                             WT_Priv=as.numeric(work_type=="Private"),
                                             WT_NevWor=as.numeric(work_type=="Self-employed"),
                                             WT_OWN=as.numeric(work_type=="Govt_job")
) %>% select(-work_type)


#Residence type
# unique(main_Train_Data$Residence_type)
# table(main_Train_Data$Residence_type)

main_Train_Data = main_Train_Data %>% mutate(Res_Type=as.numeric(Residence_type=="Urban")) %>% select(-Residence_type)


#avg_glucose_level
# typeof(main_Train_Data$avg_glucose_level)
# quantile(main_Train_Data$avg_glucose_level)


main_Train_Data=main_Train_Data %>% mutate(AGL_bracket_1=as.numeric(avg_glucose_level>=0.00 & avg_glucose_level<=55.00),
                                           AGL_bracket_2=as.numeric(avg_glucose_level>55.00 & avg_glucose_level<=78.00),
                                           AGL_bracket_3=as.numeric(avg_glucose_level>78.00 & avg_glucose_level<=92.00),
                                           AGL_bracket_4=as.numeric(avg_glucose_level>92.00 & avg_glucose_level<113.00),
                                           AGL_bracket_4=as.numeric(avg_glucose_level>113.00 & avg_glucose_level<292.00),
                                           AGL_bracket_5=as.numeric(avg_glucose_level>292.00)
) %>% select(-avg_glucose_level)



#bmi
# typeof(main_Train_Data$bmi)
# quantile(main_Train_Data$bmi)

main_Train_Data=main_Train_Data %>% mutate(BMI_bracket_1=as.numeric(bmi>=0.00 & bmi<=11.00),
                                           BMI_bracket_2=as.numeric(bmi>11.00 & bmi<=24.00),
                                           BMI_bracket_3=as.numeric(bmi>24.00 & bmi<=29.00),
                                           BMI_bracket_4=as.numeric(bmi>29.00 & bmi<33.00),
                                           BMI_bracket_4=as.numeric(bmi>33.00 & bmi<98.00),
                                           BMI_bracket_5=as.numeric(bmi>98.00)
) %>% select(-bmi)


#smoking_status
# unique(main_Train_Data$smoking_status)
# table(main_Train_Data$smoking_status)

main_Train_Data = main_Train_Data %>% mutate(SMT_1=as.numeric(smoking_status=="formerly smoked"),
                                             SMT_2=as.numeric(smoking_status=="never smoked"),
                                             SMT_3=as.numeric(smoking_status=="Unknown")
) %>% select(-smoking_status)


#Deleting ID
main_Train_Data = main_Train_Data %>% select(-id)


#Conveting stroke to factor

main_Train_Data$stroke=as.factor(main_Train_Data$stroke)


#Viewing the final Data Set
#View(main_Train_Data)



#Defining the input and output layers of Deep learning
output="stroke"
input=setdiff(names(main_Train_Data),output) #Means all cols except output


#initialising h20 frame
h2o.init()


#Converting to h2o type
main_Train_Data_h2o=as.h2o(main_Train_Data)



#Running the deep learning model
dlearning_model <- h2o.deeplearning(model_id='dp_1',
                                    y = output,
                                    x = input,
                                    training_frame = main_Train_Data_h2o,
                                    epoch = 3,
                                    hidden = c(400,400,400,400),
                                    #activation = "Rectifier",
                                    #seed = 1122
                                    reproducible = TRUE,
                                    variable_importances = TRUE
)


#Validation the model
h2o.performance(dlearning_model)



#Treating the test data set
main_Test_Data=fread("test_v2akXPA.csv")
main_Test_Data_Original=main_Test_Data

#Checking for NA and missing values
#apply(main_Test_Data,2,function(x) sum(is.na(x)))
#only the bmi feature contains NA values (591)
#apply(main_Test_Data,2,function(x) length(which(x=="")))
#only the smoking_status feature contains missing values (5751)



#NA Value imputation(using mean)
#Seeing the avg of the bmi column
avg_result_test=round(mean(main_Test_Data$bmi,na.rm = TRUE),3)

#replacing NAs with average values
main_Test_Data$bmi[which(is.na(main_Test_Data$bmi))]=avg_result_test

#Missing value imputation
main_Test_Data$smoking_status[which(main_Test_Data$smoking_status=="")]="Unknown"



#QCing the same
#apply(main_Test_Data,2,function(x) sum(is.na(x)))
#apply(main_Test_Data,2,function(x) length(which(x=="")))


#Age
main_Test_Data=main_Test_Data %>% mutate(age_bracket_1=as.numeric(age>=0.00 & age<=1.00),
                                           age_bracket_2=as.numeric(age>=1.00 & age<=24.00),
                                           age_bracket_3=as.numeric(age>24.00 & age<=44.00),
                                           age_bracket_4=as.numeric(age>44.00 & age<=60.00),
                                           age_bracket_5=as.numeric(age>60.00 & age<82.00),
                                           age_bracket_6=as.numeric(age>82.00)
) %>% select(-age)


#Gender
main_Test_Data = main_Test_Data %>% mutate(Gender_1=as.numeric(gender=="Female"),
                                           Gender_2=as.numeric(gender=="Male")
) %>% select(-gender)


#Ever married

main_Test_Data = main_Test_Data %>% mutate(Marr_Stat=as.numeric(ever_married=="Yes")) %>% select(-ever_married)


#Work Type

main_Test_Data = main_Test_Data %>% mutate(WT_child=as.numeric(work_type=="children"),
                                           WT_Priv=as.numeric(work_type=="Private"),
                                           WT_NevWor=as.numeric(work_type=="Self-employed"),
                                           WT_OWN=as.numeric(work_type=="Govt_job")
) %>% select(-work_type)


#Residence type
main_Test_Data = main_Test_Data %>% mutate(Res_Type=as.numeric(Residence_type=="Urban")) %>% select(-Residence_type)


#avg_glucose_level
main_Test_Data=main_Test_Data %>% mutate(AGL_bracket_1=as.numeric(avg_glucose_level>=0.00 & avg_glucose_level<=55.00),
                                         AGL_bracket_2=as.numeric(avg_glucose_level>55.00 & avg_glucose_level<=78.00),
                                         AGL_bracket_3=as.numeric(avg_glucose_level>78.00 & avg_glucose_level<=92.00),
                                         AGL_bracket_4=as.numeric(avg_glucose_level>92.00 & avg_glucose_level<113.00),
                                         AGL_bracket_4=as.numeric(avg_glucose_level>113.00 & avg_glucose_level<292.00),
                                         AGL_bracket_5=as.numeric(avg_glucose_level>292.00)
) %>% select(-avg_glucose_level)


#bmi
main_Test_Data=main_Test_Data %>% mutate(BMI_bracket_1=as.numeric(bmi>=0.00 & bmi<=11.00),
                                         BMI_bracket_2=as.numeric(bmi>11.00 & bmi<=24.00),
                                         BMI_bracket_3=as.numeric(bmi>24.00 & bmi<=29.00),
                                         BMI_bracket_4=as.numeric(bmi>29.00 & bmi<33.00),
                                         BMI_bracket_4=as.numeric(bmi>33.00 & bmi<98.00),
                                         BMI_bracket_5=as.numeric(bmi>98.00)
) %>% select(-bmi)


#smoking_status
main_Test_Data = main_Test_Data %>% mutate(SMT_1=as.numeric(smoking_status=="formerly smoked"),
                                           SMT_2=as.numeric(smoking_status=="never smoked"),
                                           SMT_3=as.numeric(smoking_status=="Unknown")
) %>% select(-smoking_status)


#Deleting ID
main_Test_Data = main_Test_Data %>% select(-id)


#Predicting the values
#Converting the data into h2o type
Full_data_test_h2o=as.h2o(main_Test_Data)


#Predicting for test data
predict_Value = as.data.frame(h2o.predict(dlearning_model, Full_data_test_h2o))
stroke=predict_Value$predict


final_submission=cbind.data.frame(main_Test_Data_Original,stroke)
final_submission=final_submission %>% select(-gender,-age,-hypertension,-heart_disease,-ever_married,-work_type,-Residence_type,-avg_glucose_level,-bmi,-smoking_status)   


#View(final_submission)


write.csv(final_submission,"Abhinaba_HealthCareAnalytics_Mckinsey.csv",row.names=FALSE)
