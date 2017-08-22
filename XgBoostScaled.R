#iter train_mae_mean train_mae_std test_mae_mean test_mae_std
#1: 1892     0.05730333  9.683778e-05      0.057471 0.0001741034
#Kaggel gives 0.0648007

#Remove Month
#iter train_mae_mean train_mae_std test_mae_mean test_mae_std
#1: 1851     0.05730267  9.928522e-05    0.05746833 0.0001589766
#Kaggel gives 0.0648003



library(data.table)
library(caret)
library(xgboost)
library(dplyr)

setwd("/Users/wei/Desktop/MLProject")
prop <- fread("/Users/wei/Desktop/MLProject/PROPERTIES.csv", stringsAsFactors = FALSE)
#prop10 <- prop %>%
#mutate(, month =0)%>%
#select(-V1)
prop$num_story <- as.numeric(prop$num_story)
prop$flag_fireplace <- as.numeric(prop$flag_fireplace)
prop <- prop %>% select(-V1)
#prop10 = prop%>% mutate(month=10)
#prop11 = prop%>% mutate(month=11)
#prop12 = prop%>% mutate(month=12)



train <- fread("/Users/wei/Desktop/MLProject/TRAIN.csv", stringsAsFactors = FALSE)


setkey(prop, id_parcel)
setkey(train, id_parcel)


#training <- prop[train] 
training <- prop[train] %>% select(-month)


#training <- training %>%
#mutate(month = i.month) %>%
#select(-i.month)
target <- training$logerror 

dtrain <- training[, !c('logerror', 'id_parcel', 'V1','year'),with=FALSE]
feature_names <- names(dtrain)
cv.train <- training[, !c( 'id_parcel', 'V1','year'),with=FALSE]
dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target)

dtest <- xgb.DMatrix(data=as.matrix( prop[, ..feature_names]))
#dtest10 <- xgb.DMatrix(data=as.matrix( prop10[, feature_names]))
#dtest11 <- xgb.DMatrix(data=as.matrix( prop11[, feature_names]))
#dtest12 <- xgb.DMatrix(data=as.matrix( prop12[, feature_names]))
####################
# Cross-validation
####################

# Set up cross-validation scheme (3-fold)
foldsCV <- createFolds(target, k=3, list=TRUE, returnTrain=FALSE)

# Set xgboost parameters. These are not necessarily the optimal parameters.
# Further grid tuning is needed. 
param <- list(
  objective="reg:linear",
  eval_metric = "mae",
  eta = .005,
  max_depth = 2,
  min_child_weight = 10,
  subsample = 0.7,
  colsample_bytree = 0.5
)

# Perform xgboost cross-validation

xgb_cv <- xgb.cv(data=dtrain,
                 preProcess = c("center", "scale"),
                 params=param,
                 #eta =c(0.005,0.01,0.1),
                 nrounds=3000,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 #early_stopping_rounds = 100,
                 print_every_n = 5
)

ggplot(xgb_cv$evaluation_log,aes(x=xgb_cv$evaluation_log$iter,y=xgb_cv$evaluation_log$test_mae_mean)) + geom_line() +
  labs(x = 'Num_iteration', y = 'test-MAE') + theme_bw()




###Stopping. Best iteration:
####[1922]	train-mae:0.057302+0.000091	test-mae:0.057486+0.000171



# Check best results and get best nrounds
print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
nrounds <- xgb_cv$best_iteration



################
# Final model
################

xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     preProcess = c("center", "scale"),
                     nrounds=nrounds,
                     #nrounds=1500,
                     #verbose=1,
                     print_every_n = 5)
###############
# Results
###############

# Feature Importance
importance_matrix <- as.data.frame(xgb.importance(feature_names,model=xgb_mod))

library(ggplot2)
ggplot(importance_matrix,aes(x=reorder(Feature,Gain),y=Gain,fill=Gain)) +
  geom_bar(stat='identity',fill='#009E73') +coord_flip() + theme_bw() + ggtitle('Variable Importance of XGBoost') +xlab(NULL) +
  theme(plot.title = element_text(hjust = 0.5))
          


#xgb.plot.importance(importance_matrix[1:13,])

# Predict
preds <- predict(xgb_mod,dtest)
#preds10 <- predict(xgb_mod,dtest10)
#preds11 <- predict(xgb_mod,dtest11)
#preds12 <- predict(xgb_mod,dtest12)
# For now, use same predictions for each time period. 
#results <- data.table(parcelid=prop$id_parcel, 
 #                     '201610'=preds10, 
 #                     '201611'=preds11, 
 #                     '201612'=preds12, 
#                      '201710'=preds10,
 #                     '201711'=preds11,
 #                     '201712'=preds12
#)


results <- data.table(parcelid=prop$id_parcel, 
                      '201610'=preds, 
                      '201611'=preds, 
                      '201612'=preds, 
                      '201710'=preds,
                      '201711'=preds,
                      '201712'=preds
)


#Write results to csv
#fwrite(results, file='submissionTryXgBoostScaled.csv3', row.names=FALSE)


fwrite(results, file='submissionTryXgBoostScaledNoMonth.csv3', row.names=FALSE)
