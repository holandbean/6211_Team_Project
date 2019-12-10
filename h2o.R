library(h2o)
h2o.init()
library(tidyverse)
train <- read.csv("train_resample.csv",sep=",", 
                  header=T, strip.white = T, na.strings = c("NA","NaN","","?","XNA","NA's"))
train <- train %>%
  select (-c(FLAG_DOCUMENT_2,FLAG_DOCUMENT_3,FLAG_DOCUMENT_4,FLAG_DOCUMENT_5,FLAG_DOCUMENT_6, FLAG_DOCUMENT_7,
             FLAG_DOCUMENT_8, FLAG_DOCUMENT_9, FLAG_DOCUMENT_10, FLAG_DOCUMENT_11, FLAG_DOCUMENT_12,FLAG_DOCUMENT_13, FLAG_DOCUMENT_14, FLAG_DOCUMENT_15, FLAG_DOCUMENT_16,FLAG_DOCUMENT_17,FLAG_DOCUMENT_18,FLAG_DOCUMENT_19,FLAG_DOCUMENT_20,FLAG_DOCUMENT_21 ))
write.csv(train, file = "train.csv")
model_ready2 <- h2o.uploadFile(path = "train.csv")
#####load validation
# validation <- read.csv("test_resample.csv",sep=",", 
#                   header=T, strip.white = T, na.strings = c("NA","NaN","","?","XNA","NA's"))
# validation <- validation %>%
#   select (-c(FLAG_DOCUMENT_2,FLAG_DOCUMENT_3,FLAG_DOCUMENT_4,FLAG_DOCUMENT_5,FLAG_DOCUMENT_6, FLAG_DOCUMENT_7,
#              FLAG_DOCUMENT_8, FLAG_DOCUMENT_9, FLAG_DOCUMENT_10, FLAG_DOCUMENT_11, FLAG_DOCUMENT_12,FLAG_DOCUMENT_13, FLAG_DOCUMENT_14, FLAG_DOCUMENT_15, FLAG_DOCUMENT_16,FLAG_DOCUMENT_17,FLAG_DOCUMENT_18,FLAG_DOCUMENT_19,FLAG_DOCUMENT_20,FLAG_DOCUMENT_21 ))
# write.csv(validation, file = "validation.csv")
# model_ready2 <- h2o.uploadFile(path = "validation.csv")


#converting to cate
#model_ready2 <- h2o.uploadFile(path = "train_resample.csv")
#model_ready2 <- h2o.uploadFile(path = "test_resample.csv")
#model_ready2 <- h2o.uploadFile(path = "after_mice_kaggle.csv")
model_ready2<-model_ready2[-c(1:3)]
model_ready2[, 1:5] <- as.factor(model_ready2[, 1:5])
model_ready2[, 11:14] <- as.factor(model_ready2[, 11:14])
model_ready2[, 19:24] <- as.factor(model_ready2[, 19:24])
model_ready2[, 30:35] <- as.factor(model_ready2[, 30:35])



summary(model_ready2)

dim(model_ready2)
#validation<-h2o.assign(model_ready2,'validation')
train1<-h2o.assign(model_ready2,'train1')
#test1<-h2o.assign(model_ready2,'test1')



# h2o.clusterInfo()
rf1 <- h2o.randomForest(     	## h2o.randomForest function
  training_frame = train,    	## the H2O frame for training
  validation_frame = validation,  	## the H2O frame for validation (not required)
  x=3:46,                    	## the predictor columns, by column index
  y=2,                      	## the target index (what we are predicting)
  model_id = "rf_v1",	
  ntrees = 200,              	
  stopping_rounds = 2,       
  score_each_iteration = T,  
  seed = 1000000)            	## Set the random seed so that this can be
##  reproduced.
###############################################################################
summary(rf1)                 	## View information about the model.
## Keys to look for are validation performance
##  and variable importance
random_forest<-h2o.saveModelDetails(rf1, path = "rf1", force = FALSE)
###########predict
prediction<-h2o.predict(rf1, model_ready2)
summary(prediction)
kaggle_prediction1<-h2o.exportFile(prediction, path = 'kaggle_prediction1.csv', force = FALSE, sep = ",",
                                   compression = NULL, parts = 1)

#######################nerual network
dl1<-h2o.deeplearning(x=3:46, y=2,activation="RectifierWithDropout",
                      training_frame = model_ready2, hidden=c(50,30,21,2),epochs=50,input_dropout_ratio=0.1)
summary(dl1)
########sample
training <- h2o.splitFrame(model_ready2, ratios = c(0.1), seed = 1)
training1 <- training[[1]]
rf2 <- h2o.randomForest(     	## h2o.randomForest function
  training_frame = training1,    	## the H2O frame for training
  #validation_frame = valid,  	## the H2O frame for validation (not required)
  x=3:46,                    	## the predictor columns, by column index
  y=2,                      	## the target index (what we are predicting)
  model_id = "rf_v1",	## name the model in H2O
  ##   not required, but helps use Flow
  ntrees = 200,              	## use a maximum of 200 trees to create the
  
  stopping_rounds = 2,       	## Stop fitting new trees when the 2-tree
  
  ion for
  score_each_iteration = T,  	## Predict against training and validat ##  each tree. Default will skip several.
  seed = 1000000)            	## Set the random seed so that this can be
##  reproduced.
###############################################################################
summary(rf2)                 
#######################################GBM

gbm1 <- h2o.gbm(
  training_frame = train1,        ## the H2O frame for training
  validation_frame = test1,      ## the H2O frame for validation (not required)
  x=2:35,                        ## the predictor columns, by column index
  y=1,                          ## the target index (what we are predicting)
  model_id = "gbm",     ## name the model in H2O
  ntrees = 200,
  seed = 4) 

summary(gbm1)
gbm<-h2o.saveModelDetails(gbm1, path = "gbm", force = FALSE)
###########predict
prediction<-h2o.predict(gbm1, test)
summary(prediction)
prediction_gbm<-h2o.exportFile(prediction, path = 'prediction_gbm.csv', force = FALSE, sep = ",",
                               compression = NULL, parts = 1)
###############xgboost
xgboost<-h2o.xgboost(training_frame = train1,        ## the H2O frame for training
                     validation_frame = test1,      ## the H2O frame for validation (not required)
                     x=2:35,                        ## the predictor columns, by column index
                     y=1,                          ## the target index (what we are predicting)
                     model_id = "xgboost",     ## name the model in H2O
                     ntrees = 200,
                     seed = 4, nfolds = 5)
summary(xgboost)
xgboost<-h2o.saveModelDetails(xgboost, path = "xgboost", force = FALSE)

###########predict
prediction<-h2o.predict(xgboost, test)
summary(prediction)
prediction_xgboost<-h2o.exportFile(prediction, path = 'prediction_boost.csv', force = FALSE, sep = ",",
                                   compression = NULL, parts = 1)
