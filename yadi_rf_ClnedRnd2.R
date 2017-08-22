##############load library###############
library(caret)
library(randomForest)
library(dplyr)
train = readRDS("~/Zillow-Price/group_data/TrainClnedRnd2.rds")
fullset = readRDS("~/Zillow-Price/group_data/PropertiesClnedRnd2.rds")


#############remove year column#####################
train = train[, -which(names(train) == "year")]

################ convert zip code to numeric ##############
train$region_zip = as.numeric(as.character(train$region_zip))

############ add feature: price per area ###############
train = train %>% dplyr::mutate(tax_area = tax_total/area_total_calc)


############## split data #############################
## Data splitting based on the outcome
set.seed(123)
trainIndex <- createDataPartition(train$logerror, 
                                  p = 0.6, 
                                  list = FALSE, 
                                  times = 1)
## training set
subTrain <- train[ trainIndex,-1]
## testing set
subTest  <- train[-trainIndex,-1]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}

################## grid search ##########################
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           summaryFunction = maeSummary,
                           verboseIter = TRUE)

rfGrid <-  expand.grid(mtry = 2:7)

set.seed(8)
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
# 2     0.05765618
# 3     0.05756304
# 4     0.05747263
# 5     0.05741567
# 6     0.05743120
# 7     0.05749896


plot(rf.logerror)
rf.logerror$bestTune
#   mtry
# 4    5

gbmImp <- varImp(rf.logerror, scale = FALSE)
plot(gbmImp, top = 20)

results <- data.frame(obs = subTest$logerror, 
                      pred = predict(rf.logerror, newdata = subTest))
maeSummary(results)
#       MAE 
# 0.05731453 
cor(results)
#           obs     pred
# obs  1.000000 0.115449
# pred 0.115449 1.000000

saveRDS(rf.logerror, "rf_ClnedRnd2_sub_mtrycv.rds")


########### manually tuning number of trees #####################
control <- trainControl(method="cv", 
                        number=3,
                        summaryFunction = maeSummary,
                        verboseIter = TRUE)
tunegrid <- expand.grid(mtry=5)
modellist <- list()
for (ntree in c(50, 100, 250, 500, 1000, 1500)) {
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
# Models: 50, 100, 250, 500, 1000, 1500 
# Number of resamples: 3 
# 
# MAE 
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. NA's
# 50   0.05714509 0.05733486 0.05752462 0.05752539 0.05771554 0.05790645    0
# 100  0.05704643 0.05726952 0.05749260 0.05746161 0.05766921 0.05784581    0
# 250  0.05704288 0.05726608 0.05748928 0.05742955 0.05762288 0.05775648    0
# 500  0.05703051 0.05725144 0.05747238 0.05741919 0.05761353 0.05775469    0
# 1000 0.05704156 0.05725353 0.05746549 0.05742167 0.05761172 0.05775796    0
# 1500 0.05702958 0.05724472 0.05745986 0.05741514 0.05760792 0.05775599    0

dotplot(result_ntree)

##### ntree = 500 seems to be good, but choose 800 for best score


################ Fitting all training set with Best parameters #############

rf.logerror$bestTune

fitBestModel <- trainControl(method = "none",
                             summaryFunction = maeSummary)
set.seed(8)
rfFit <- train(logerror ~ .,
                 data = train[,-1], 
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
# 0.05507034 

rfImp <- varImp(rfFit, scale = FALSE)
rfImp$importance
plot(rfImp, top = 50)

saveRDS(rfFit, "rf_clnedRond2_sub_fit_mtry5_ntree800.rds")



########################### Making prediction for submission
test_data <- fullset %>% 
  select(intersect(names(fullset), names(train)))
test_data = test_data %>% dplyr::mutate(tax_area = tax_total/area_total_calc)
test_data <- test_data %>% rename( parcelid = id_parcel)


makePrediction <- function(model, newdata, months, labels) {
  predictions <- newdata[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    newdata$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = newdata)
  }
  write.csv(x = predictions, file = "submission_clnedRnd2_sub_mtry5_ntree800.csv",
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(rfFit, newdata = test_data, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))











