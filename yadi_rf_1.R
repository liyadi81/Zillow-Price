##############load library###############
library(caret)
library(randomForest)
library(dplyr)
train = readRDS("~/Zillow-Price/group_data/TrainClned.rds")
fullset = readRDS("~/Zillow-Price/group_data/PropertiesClned.rds")


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
                                  p = .6, 
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

rfGrid <-  expand.grid(mtry = 2:8)

rf.logerror <- train(logerror ~ .,
                     data = subTrain, 
                     method = "rf", 
                     metric = "MAE",
                     maximize = FALSE,
                     tuneGrid = rfGrid,
                     trControl = gridSearch,
                     importance = TRUE)

rf.logerror
plot(rf.logerror)
rf.logerror$bestTune
#   mtry
# 3    4

gbmImp <- varImp(rf.logerror, scale = FALSE)
plot(gbmImp, top = 20)

results <- data.frame(obs = subTest$logerror, 
                      pred = predict(rf.logerror, newdata = subTest))
maeSummary(results)
#       MAE 
# 0.05730982 

cor(results)
#           obs      pred
# obs  1.0000000 0.1146288
# pred 0.1146288 1.0000000

saveRDS(rf.logerror, "rf_logerror_mtrycv_zip2.rds")

########### manually tuning number of trees #####################
control <- trainControl(method="cv", 
                        number=3,
                        summaryFunction = maeSummary,
                        verboseIter = TRUE)
tunegrid <- expand.grid(mtry=4)
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
results <- resamples(modellist)
summary(results)
# Models: 50, 100, 250, 500, 1000, 1500 
# Number of resamples: 3 
# 
# MAE 
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. NA's
# 50   0.05727449 0.05747349 0.05767249 0.05763324 0.05781261 0.05795273    0
# 100  0.05717956 0.05737325 0.05756694 0.05753458 0.05771209 0.05785725    0
# 250  0.05709706 0.05729229 0.05748752 0.05745035 0.05762699 0.05776646    0
# 500  0.05705892 0.05726512 0.05747133 0.05742726 0.05761143 0.05775154    0
# 1000 0.05704086 0.05726413 0.05748741 0.05742179 0.05761225 0.05773709    0
# 1500 0.05703540 0.05725878 0.05748216 0.05741773 0.05760889 0.05773562    0

dotplot(results)

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
# 0.0533703 

rfImp <- varImp(rfFit, scale = FALSE)
plot(rfImp, top = 50)

saveRDS(rfFit, "rf_fit_zip_mtry4_ntree800.rds")



########################### Making prediction for submission
test_data <- fullset %>% 
  select(intersect(names(fullset), names(train)))
test_data = test_data %>% dplyr::mutate(tax_area = tax_total/area_total_calc)
test_data <- test_data %>% rename( parcelid = id_parcel)
test_data$region_zip = as.numeric(as.character(test_data$region_zip))


makePrediction <- function(model, newdata, months, labels) {
  predictions <- newdata[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    newdata$month <- months[i]
    predictions[, labels[i]] <- predict(model, newdata = newdata)
  }
  write.csv(x = predictions, file = "submission_mtry4_ntree800_zip.csv", 
            quote = FALSE, row.names = FALSE)
  return(predictions)
}

makePrediction(rfFit, newdata = test_data, months = c(10, 11, 12, 22, 23, 24), 
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))






