
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
prop10 = prop%>% mutate(month=10)
prop11 = prop%>% mutate(month=11)
prop12 = prop%>% mutate(month=12)



train <- fread("/Users/wei/Desktop/MLProject/TRAIN.csv", stringsAsFactors = FALSE)


setkey(prop, id_parcel)
setkey(train, id_parcel)


training <- prop[train]
#training <- training %>%
  #mutate(month = i.month) %>%
  #select(-i.month)
target <- training$logerror 

dtrain <- training[, !c('logerror', 'id_parcel', 'V1','year','i.V1'),with=FALSE]
feature_names <- names(dtrain)
cv.train <- training[, !c( 'id_parcel', 'V1','year','i.V1'),with=FALSE]
dtrain <- xgb.DMatrix(data=as.matrix(dtrain),label=target)

#dtest <- xgb.DMatrix(data=as.matrix( prop[, ..feature_names]))
dtest10 <- xgb.DMatrix(data=as.matrix( prop10[, ..feature_names]))
dtest11 <- xgb.DMatrix(data=as.matrix( prop11[, ..feature_names]))
dtest12 <- xgb.DMatrix(data=as.matrix( prop12[, ..feature_names]))
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
                 params=param,
                 #eta =c(0.005,0.01,0.1),
                 nrounds=3000,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 early_stopping_rounds = 100,
                 print_every_n = 5
)



###Stopping. Best iteration:
  ####[1922]	train-mae:0.057302+0.000091	test-mae:0.057486+0.000171



# Check best results and get best nrounds
print(xgb_cv$evaluation_log[which.min(xgb_cv$evaluation_log$test_mae_mean)])
nrounds <- xgb_cv$best_iteration


######iter train_mae_mean train_mae_std test_mae_mean test_mae_std
##Kaggle gives 0.648020
#########1: 1922     0.05730233   9.07022e-05      0.057486 0.0001707181
################
# Final model
################

xgb_mod <- xgb.train(data=dtrain,
                     params=param,
                     nrounds=nrounds,
                     #nrounds=1500,
                     #verbose=1,
                     print_every_n = 5)
###############
# Results
###############

# Feature Importance
importance_matrix <- xgb.importance(feature_names,model=xgb_mod)
xgb.plot.importance(importance_matrix[1:20,])

# Predict
preds10 <- predict(xgb_mod,dtest10)
preds11 <- predict(xgb_mod,dtest11)
preds12 <- predict(xgb_mod,dtest12)
# For now, use same predictions for each time period. 
results <- data.table(parcelid=prop$id_parcel, 
                      '201610'=preds10, 
                      '201611'=preds11, 
                      '201612'=preds12, 
                      '201710'=preds10,
                      '201711'=preds11,
                      '201712'=preds12
)

#Write results to csv
fwrite(results, file='submissionTryXgBoost819.csv3', row.names=FALSE)







## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

set.seed(28)

control <- trainControl(
  method="cv",
  number=3,
  #verboseIter=FALSE,
  #savePredictions = "final",
  summaryFunction = maeSummary
  #preProcOptions = list(
    #thresh = 0.90,
    #freqCut = 95/5
  #)
  #allowParallel = TRUE
)

xgb.grid <- expand.grid(
  nrounds = c(2000),
  eta = c(0.005,.01,.025,0.1),       #step size shrinkage used in update to prevent overfitting;range[0,1],default =0.3
  max_depth = seq(2,8),      #maximum depth of a tree,range[1,+inf],default=6
  gamma = c(0,.5,1),           #penalty added on the function, minumum loss reduction required to make a split(penalty of complexity),range[0,+inf],default=0
  colsample_bytree = c(.25,.5),#what faction of the entire set of feature do you want to use for constructing each tree,range[0,1],defalut=1
  min_child_weight = c(1,5,10),#minimum sum of instance weight needed in a child,range[0,+inf],default =1
  subsample = c(0.1,0.7,1)            #subsample ratio of the training instance,range[0,1],default = 1
)
#Recap: Controlling the model complexity: max_depth, min_child_weight, gamma
#Robust to noise: subsample, colsample_bytree

xgbTree <- train(logerror ~ .,
                 data = cv.train, 
                 method = "xgbTree", 
                 trControl=control,
                 tuneGrid = xgb.grid,
                 #preProc=c("conditionalX","nzv"),
                 metric = "MAE"
                 #verbose=1,
                 #maximize = FALSE
)


param.cv <- list(
  objective="reg:linear",
  eval_metric = "mae",
  eta = .005,
  max_depth = 6,
  min_child_weight = 5,
  subsample = 0.7,
  colsample_bytree = 0.5,
  gamma=1
)

xgb_cv.tree <- xgb.cv(data=dtrain,
                 params=param.cv,
                 #eta =c(0.005,0.01,0.1),
                 nrounds=3000,
                 prediction=TRUE,
                 maximize=FALSE,
                 folds=foldsCV,
                 early_stopping_rounds = 100,
                 print_every_n = 5
)

xgb_mod.cv <- xgb.train(data=dtrain,
                     params=param.cv,
                     nrounds=2071,
                     #nrounds=1500,
                     #verbose=1,
                     print_every_n = 5)

# Predict
preds10.cv1 <- predict(xgb_mod,dtest10)
preds11.cv1 <- predict(xgb_mod,dtest11)
preds12.cv1 <- predict(xgb_mod,dtest12)
# For now, use same predictions for each time period. 
results.cv1 <- data.table(parcelid=prop$id_parcel, 
                      '201610'=preds10.cv1, 
                      '201611'=preds11.cv1, 
                      '201612'=preds12.cv1, 
                      '201710'=preds10.cv1,
                      '201711'=preds11.cv1,
                      '201712'=preds12.cv1
)

#Write results to csv
fwrite(results.cv1, file='submissionTryXgBoostCV2.csv', row.names=FALSE)
