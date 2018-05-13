#Trying out deep learning with Neural Networks

#loading all the libraries
library(data.table)
library(dplyr)
library(h2o)


#Setting up directory
setwd('C:\\Users\\Abhinaba\\Downloads\\Train')


#Loading test data along with campaign data
main_Train_Data=fread("train.csv")
Campaign_Data=fread("campaign_data.csv")


#Joining the data sets
Full_data_train=left_join(main_Train_Data,Campaign_Data,by="campaign_id")

#View(Full_data_train)

#Data Prep
Full_data_train=Full_data_train %>% mutate(Commty_1=as.numeric(communication_type=='Newsletter'),
                                           Commty_2=as.numeric(communication_type=='Conference'),
                                           Commty_3=as.numeric(communication_type=='Upcoming Events'),
                                           Commty_4=as.numeric(communication_type=='Others'),
                                           Commty_5=as.numeric(communication_type=='Hackathon'),
                                           Commty_6=as.numeric(communication_type=='Webinar')
) %>% select(-communication_type)


#sort(table(Full_data_train$subject),decreasing=T)



Full_data_train=Full_data_train %>% mutate(sbj_1=as.numeric(subject=="[November Updates] - Announcing DataFest 2018, Mumbai"),
                                           sbj_2=as.numeric(subject=="[Register Now] Just 2 days to go for India's biggest action in Artificial Intelligence & Machine Learning."),
                                           sbj_3=as.numeric(subject=="[Newsletter] Stage for DataHack Summit 2017 is set! Announcing new hackathons, articles and job opportunities !"),
                                           sbj_4=as.numeric(subject=="Register @ DataHack Summit 2017 - India's Largest Conference On Artificial Intelligence & Machine Learning"),
                                           sbj_5=as.numeric(subject=="[September] Exciting days ahead with DataHack Summit 2017, Hackathons, Meetups and more!"),
                                           sbj_6=as.numeric(subject=="Dr Kirk Borne of Booz Allen Hamilton, to keynote at DataHack Summit 2017"),
                                           sbj_7=as.numeric(subject=="[AV Newsletter] Fireside Chat with DJ Patil, DataHack Summit, and much more"),
                                           sbj_8=as.numeric(subject=="[July] Data Science Expert Meetups & Competitions coming your way"),
                                           sbj_9=as.numeric(subject=="A.I. & Machine Learning: 5 reasons why you should attend DataHack Summit 2017"),
                                           sbj_10=as.numeric(subject=="Sneak Peek: A look at the emerging data science world !")
) %>% select(-subject)



#sort(table(Full_data_train$total_links),decreasing=T)


Full_data_train=Full_data_train %>% mutate(tl_1=as.numeric(total_links==67),
                                           tl_2=as.numeric(total_links==63),
                                           tl_3=as.numeric(total_links==104),
                                           tl_4=as.numeric(total_links==119),
                                           tl_5=as.numeric(total_links==88),
                                           tl_6=as.numeric(total_links==13),
                                           tl_7=as.numeric(total_links==75),
                                           tl_8=as.numeric(total_links==18)
) %>% select(-total_links)



#View(Full_data_train)


Full_data_train$is_click=as.factor(Full_data_train$is_click)


#deleting the id cols
Full_data_train=Full_data_train %>% select(-email_body,-email_url)
Full_data_train=Full_data_train %>% select(-id,-user_id,-campaign_id,-send_date)


#Viewing the data set
#View(Full_data_train)

#Defining the input and output layers of Deep learning
output="is_click"
input=setdiff(names(Full_data_train),output) #Means all cols except output


#initialising h20 frame
h2o.init()


#Converting to h2o type
Full_data_train_h2o=as.h2o(Full_data_train)



#Running the deep learning model
dlearning_model <- h2o.deeplearning(model_id='dp_1',
                                    y = output,
                                    x = input,
                                    training_frame = Full_data_train_h2o,
                                    epoch = 3,
                                    hidden = c(400,400,400,400),
                                    #activation = "Rectifier",
                                    #seed = 1122
                                    reproducible = TRUE,
                                    variable_importances = TRUE
)


#Validation the model
h2o.performance(dlearning_model)


#Loading the test data
#Setting the path for test data
setwd('C:\\Users\\Abhinaba\\Downloads\\Test')


#Loading test data
main_Test_Data=fread("test_BDIfz5B.csv")


#Joining the test data set with camaign data
Full_data_test=left_join(main_Test_Data,Campaign_Data,by="campaign_id")


#Data Prep 
Full_data_test=Full_data_test %>% mutate(Commty_1=as.numeric(communication_type=='Newsletter'),
                                         Commty_2=as.numeric(communication_type=='Conference'),
                                         Commty_3=as.numeric(communication_type=='Upcoming Events'),
                                         Commty_4=as.numeric(communication_type=='Others'),
                                         Commty_5=as.numeric(communication_type=='Hackathon'),
                                         Commty_6=as.numeric(communication_type=='Webinar')
) %>% select(-communication_type)




Full_data_test=Full_data_test %>% mutate(sbj_1=as.numeric(subject=="[November Updates] - Announcing DataFest 2018, Mumbai"),
                                         sbj_2=as.numeric(subject=="[Register Now] Just 2 days to go for India's biggest action in Artificial Intelligence & Machine Learning."),
                                         sbj_3=as.numeric(subject=="[Newsletter] Stage for DataHack Summit 2017 is set! Announcing new hackathons, articles and job opportunities !"),
                                         sbj_4=as.numeric(subject=="Register @ DataHack Summit 2017 - India's Largest Conference On Artificial Intelligence & Machine Learning"),
                                         sbj_5=as.numeric(subject=="[September] Exciting days ahead with DataHack Summit 2017, Hackathons, Meetups and more!"),
                                         sbj_6=as.numeric(subject=="Dr Kirk Borne of Booz Allen Hamilton, to keynote at DataHack Summit 2017"),
                                         sbj_7=as.numeric(subject=="[AV Newsletter] Fireside Chat with DJ Patil, DataHack Summit, and much more"),
                                         sbj_8=as.numeric(subject=="[July] Data Science Expert Meetups & Competitions coming your way"),
                                         sbj_9=as.numeric(subject=="A.I. & Machine Learning: 5 reasons why you should attend DataHack Summit 2017"),
                                         sbj_10=as.numeric(subject=="Sneak Peek: A look at the emerging data science world !")
) %>% select(-subject)



Full_data_test=Full_data_test %>% mutate(tl_1=as.numeric(total_links==67),
                                         tl_2=as.numeric(total_links==63),
                                         tl_3=as.numeric(total_links==104),
                                         tl_4=as.numeric(total_links==119),
                                         tl_5=as.numeric(total_links==88),
                                         tl_6=as.numeric(total_links==13),
                                         tl_7=as.numeric(total_links==75),
                                         tl_8=as.numeric(total_links==18)
) %>% select(-total_links)




Full_data_test=Full_data_test %>% select(-email_body,-email_url)
Full_data_test=Full_data_test %>% select(-id,-user_id,-campaign_id,-send_date)



#Converting the data into h2o type
Full_data_test_h2o=as.h2o(Full_data_test)


#Predicting for test data
predict_Value = as.data.frame(h2o.predict(dlearning_model, Full_data_test_h2o))
is_click=predict_Value$predict


#View(predict_Value)


final_submission=cbind.data.frame(main_Test_Data,is_click)

final_submission=final_submission %>% select(-user_id,-campaign_id,-send_date)


#View(final_submission)

setwd('C:\\Users\\Abhinaba\\Downloads')
write.csv(final_submission,"Abhinaba_EmailAnalytics_LOTM.csv",row.names=FALSE)
