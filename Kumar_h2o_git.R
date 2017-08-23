rm(list=ls())
setwd('C:/Bootcamp/projects/ML-project/ML_run')
cat('\014')

library(h2o)
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)
library(stringr)
library(Hmisc)
library(VIM)
library(mice)
library(caret)

 ############# Importing Data #################
properties=readRDS("PropertiesClnedRnd2.rds")
train <- fread('train_2016_v2.csv', na.strings = "")
train <- train %>% rename(
  id_parcel = parcelid)

train <- train %>% 
  mutate(year=year(transactiondate),
         month=month(transactiondate)) %>%
  select(-transactiondate)
 train <- train %>%
   filter(logerror %between% c(quantile(train$logerror, .005),
                               quantile(train$logerror, .995)))
 
 
 properties$latitude = properties$latitude/1e6
 properties$longitude = properties$longitude/1e6
 
 properties_train = merge(properties, train, by="id_parcel",all.y=TRUE)

 vec_month = sample(c(10,10),nrow(properties),replace = TRUE)
 # vec_month = sample(c(1,2,3,4,5,6,7,8,9,10,11,12),nrow(properties),replace = TRUE)
 
 properties=properties %>% mutate(.,month=vec_month)
 
# properties=properties %>% select(., 
#                                  num_bath,
#                                  area_total_calc,
#                                  id_parcel
#                           )
# 
# properties_train=properties_train %>% select(., 
#                                              num_bath,
#                                              area_total_calc,
#                                              id_parcel,
#                                              logerror
# )
 properties=select_if(properties,is.numeric)
 properties_train=select_if(properties_train,is.numeric)

h2o.init(nthreads = -1)

Xnames <- names(properties_train)[which(names(properties_train)!="logerror")]
Y <- "logerror"

dx_train <- as.h2o(properties_train)
dx_predict <- as.h2o(properties)

md <- h2o.automl(x = Xnames, y = Y,
                 stopping_metric="MAE",
                 training_frame = dx_train,
                 leaderboard_frame = dx_train)

properties_target<- h2o.predict(md@leader, dx_predict)
predictions <- round(as.vector(properties_target$predict), 4)

result1 <- data.frame(cbind(properties$id_parcel, predictions, predictions,
                            predictions, predictions, predictions,
                            predictions))

colnames(result1)<-c("parcelid","201610","201611","201612","201710","201711","201712")

options(scipen = 999)
write.csv(result1, file = "h20_numeric10_try.csv", row.names = FALSE )

model_path<-h2o.saveModel(object = md@leader, path = "C:/Bootcamp/projects/ML-project/ML_run", force = TRUE)

