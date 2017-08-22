##############load library###############
library(caret)
library(randomForest)
library(dplyr)
train = readRDS("~/Zillow-Price/group_data/TrainClnedRnd2_yadi_mod.rds")
fullset = readRDS("~/Zillow-Price/group_data/PropertiesClnedRnd2_yadi_mod.rds")


############ add feature: price per area ###############
train = train %>% dplyr::mutate(tax_area = tax_total/area_total_calc)


############## split data #############################
## Data splitting based on the outcome
seed = 8

set.seed(seed)
trainIndex <- createDataPartition(train$logerror, 
                                  p = 0.6, 
                                  list = FALSE, 
                                  times = 1)
## training set
subTrain <- train[ trainIndex, ]
## testing set
subTest  <- train[-trainIndex, ]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

################## grid search ##########################
set.seed(8)
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary,
                           verboseIter = TRUE)

rfGrid <-  expand.grid(mtry = 1:3)

rf.logerror <- train(logerror ~ .,
                     data = subTrain, 
                     method = "rf", 
                     metric = "MAE",
                     maximize = FALSE,
                     tuneGrid = rfGrid,
                     trControl = gridSearch,
                     importance = TRUE)



rf.logerror
# mtry  MAE       
# 4    0.05745320
# 5    0.05773327
# 6    0.05789661
# 7    0.05799787
# 8    0.05807495
# 9    0.05813697
# 10    0.05818254
# 11    0.05821278
# 12    0.05827683

# mtry  MAE       
# 2     0.05705596
# 3     0.05717000
# 4     0.05747731
# 5     0.05775022
# 6     0.05790532

# mtry  MAE       
# 1     0.05730122
# 2     0.05705683
# 3     0.05716474

plot(rf.logerror)
rf.logerror$bestTune
#   mtry
# 1    4

#   mtry
# 1    2

#   mtry
# 2    2

gbmImp <- varImp(rf.logerror, scale = FALSE)
plot(gbmImp, top = 20)

results <- data.frame(obs = subTest$logerror, 
                      pred = predict(rf.logerror, newdata = subTest))
maeSummary(results)
#       MAE 
# 0.05766138 

#       MAE 
# 0.05738806 

#       MAE 
# 0.05738716 

cor(results)
#           obs     pred
# obs  1.000000 0.158741
# pred 0.158741 1.000000

#           obs      pred
# obs  1.0000000 0.1496773
# pred 0.1496773 1.0000000

#           obs      pred
# obs  1.0000000 0.1486189
# pred 0.1486189 1.0000000

saveRDS(rf.logerror, "rf_ClnedRnd2_full_mtrycv3.rds")


########### manually tuning number of trees #####################
control <- trainControl(method="cv", 
                        number=3,
                        summaryFunction = maeSummary,
                        verboseIter = TRUE)
tunegrid <- expand.grid(mtry=2)
modellist <- list()
for (ntree in c(50, 100, 250, 500, 1000)) {
  set.seed(8)
  fit <- train(logerror~., 
               data=subTrain, 
               method="rf", 
               metric="MAE", 
               tuneGrid=tunegrid, 
               trControl=control, 
               ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}

# compare results
result_ntree <- resamples(modellist)
summary(result_ntree)

# Models: 50, 100, 250, 500, 1000 
# Number of resamples: 3 
# 
# MAE 
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. NA's
# 50   0.05691383 0.05700822 0.05710261 0.05727748 0.05745930 0.05781598    0
# 100  0.05680990 0.05691369 0.05701748 0.05718132 0.05736702 0.05771656    0
# 250  0.05670383 0.05680419 0.05690454 0.05709242 0.05728671 0.05766888    0
# 500  0.05666001 0.05676873 0.05687746 0.05706124 0.05726186 0.05764626    0
# 1000 0.05665045 0.05675741 0.05686437 0.05704450 0.05724153 0.05761869    0


dotplot(result_ntree)

##### ntree = 500 seems to be good, but choose 800 for best score


################ Fitting all training set with Best parameters #############

rf.logerror$bestTune

fitBestModel <- trainControl(method = "none",
                             summaryFunction = maeSummary)
set.seed(8)
rfFit <- train(logerror ~ .,
                 data = train, 
                 method = "rf", 
                 metric = "MAE",
                 maximize = FALSE,
                 trControl = fitBestModel,
                 tuneGrid = rf.logerror$bestTune,
                 verbose = TRUE,
                 ntree = 800,
                 importance = TRUE)

results1 <- data.frame(obs = train$logerror, 
                       pred = predict(rfFit, newdata = train))
maeSummary(results1)
#       MAE 
# 0.0537406 

rfImp <- varImp(rfFit, scale = FALSE)
rfImp$importance
plot(rfImp, top = 50)

saveRDS(rfFit, "rf_clnedRond2_full_fit_mtry2_ntree800.rds")



########################### Making prediction for submission
test_data <- fullset %>% 
  select(intersect(names(fullset), names(train)))
test_data = test_data %>% dplyr::mutate(tax_area = tax_total/area_total_calc)
#test_data <- test_data %>% rename(parcelid = id_parcel)


makePrediction <- function(model, newdata, months, labels) {
  predictions <- newdata[, "id_parcel", drop=FALSE]
  for(i in 1:length(months)) {
    newdata$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = newdata)
  }
  predictions = predictions %>% rename(parcelid = id_parcel)
  write.csv(x = predictions, file = "submission_clnedRnd2_full_mtry2_ntree800.csv",
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(rfFit, newdata = test_data, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))











