library(dplyr)
#######load application
application_train <- read.csv("application_train.csv",sep=",", 
                              header=T, strip.white = T, na.strings = c("NA","NaN","","?","XNA","NA's"))

summary(application_train)
colnames(application_train)
colnames(previous_application)
application_train<-application_train[-c(97:122)]
application_train<-application_train[-c(23)]

########load test
application_test <- read.csv("application_test.csv",sep=",", 
                             header=T, strip.white = T, na.strings = c("NA","NaN","","?","XNA","NA's"))
application_test<-application_test[-c(96:121)]
application_test<-application_test[-c(22)]

########load previous
previous_application <- read.csv("previous_application.csv",sep=",", 
                                 header=T, strip.white = T, na.strings = c("NA","NaN","","?","XNA","NA's"))


previous_application[, 11] <- as.factor(previous_application[, 11])
previous_application[, 37] <- as.factor(previous_application[, 37])

str(previous_application)

###group by the current id and get the meam for the numeric variables
previous_mean<-previous_application %>% group_by(SK_ID_CURR) %>% 
  summarise_if(is.numeric, funs(n(),mean))
previous_mean<-previous_mean[-c(3:21)]

###merge train and test with previous
merge<-left_join(application_train, previous_mean, by.x = "SK_ID_CURR", by.y = "SK_ID_CURR")
merge_test<-left_join(application_test, previous_mean, by.x = "SK_ID_CURR", by.y = "SK_ID_CURR")
str(merge)
index <- sample(1:nrow(merge), 0.3*307511)
train<-merge[index, ]
write.csv(merge, file = "train.csv")
write.csv(merge_test, file = "merge_test.csv")

########load bureau
bureau <- read.csv("bureau.csv",sep=",", 
                                 header=T, strip.white = T, na.strings = c("NA","NaN","","?","XNA","NA's"))
str(bureau)
bureau_mean<-bureau %>% group_by(SK_ID_CURR) %>% 
  summarise_if(is.numeric, funs(n(),mean))

bureau_mean<-bureau_mean[-c(3:15)]
str(bureau_mean)
bureau_mean$AMT_ANNUITY_bureau_mean<-bureau_mean$AMT_ANNUITY_mean
bureau_mean<-subset(bureau_mean, select=-c(AMT_ANNUITY_mean))

###merge train and test with previous
merge<-left_join(merge, bureau_mean, by = "SK_ID_CURR")
merge_test<-left_join(merge_test, bureau_mean, by = "SK_ID_CURR")
str(merge)
write.csv(merge, file = "train.csv")
write.csv(merge_test, file = "merge_test.csv")

#####h2o
library(h2o)
h2o.init()
train <- h2o.uploadFile(path = "train.csv")
test <- h2o.uploadFile(path = "merge_test.csv")
summary(train)
colnames(train)
train[, 3] <- as.factor(train[, 3])
prostate.split <- h2o.splitFrame(data=train, ratios=0.75)
prostate.train <- prostate.split[[1]]
prostate.test <- prostate.split[[2]]
colnames(prostate.train)

#######Rf
rf <- h2o.randomForest(     	## h2o.randomForest function
  training_frame = prostate.train,    	## the H2O frame for training
  validation_frame = prostate.test,  	## the H2O frame for validation (not required)
  x=4:128,                    	## the predictor columns, by column index
  y=3,                      	## the target index (what we are predicting)
  model_id = "rf_v1",	
  ntrees = 200,              	
  seed = 4)            	## Set the random seed so that this can be
summary(rf)            
###########predict
prediction<-h2o.predict(rf, test)
summary(prediction)
h2o.exportFile(prediction, path = 'prediction2.csv')
#######gbm
gbm <- h2o.gbm(     	## h2o.randomForest function
  training_frame = prostate.train,    	## the H2O frame for training
  validation_frame = prostate.test,  	## the H2O frame for validation (not required)
  x=4:128,                    	## the predictor columns, by column index
  y=3,                      	## the target index (what we are predicting)
  model_id = "gbm_v1",	
  ntrees = 200,              	
  seed = 4)            	## Set the random seed so that this can be
summary(gbm)
###########predict
prediction<-h2o.predict(gbm, test)
summary(prediction)
h2o.exportFile(prediction, path = 'prediction1.csv')
#######xgboost
xgboost <- h2o.xgboost(     	## h2o.randomForest function
  training_frame = prostate.train,    	## the H2O frame for training
  validation_frame = prostate.test,  	## the H2O frame for validation (not required)
  x=4:128,                    	## the predictor columns, by column index
  y=3,                      	## the target index (what we are predicting)
  model_id = "gbm_v1",	
  ntrees = 200,              	
  seed = 4)            	## Set the random seed so that this can be
summary(xgboost)
###########predict
prediction<-h2o.predict(xgboost, test)
summary(prediction)
prediction_xgboost<-h2o.exportFile(prediction, path = 'prediction_boost.csv', force = FALSE, sep = ",",
                                   compression = NULL, parts = 1)