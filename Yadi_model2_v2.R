


############# load library #################
library(data.table)
library(caret)
library(dplyr)

# Setting Parallel processing
# library(doMC)
# library(parallel)
# number_of_cores <- detectCores()
# registerDoMC(cores = number_of_cores/2)

############## only predicting absolute logerror ###########
train_data2$logerror = abs(train_data2$logerror)

str(train_data2)



############## split data #############################
## Data splitting based on the outcome
set.seed(123)
trainIndex <- createDataPartition(train_data2$logerror, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)
## training set
subTrain <- train_data2[ trainIndex,-1]
## testing set
subTest  <- train_data2[-trainIndex,-1]

## define metric - MAE
maeSummary <- function(data, lev = NULL, model = NULL) {
  mae_score <- sum(abs(data$obs - data$pred)) / nrow(data)
  names(mae_score) <- "MAE"
  mae_score
}



################## random search ##########################
rdmSearch <- trainControl(method = "cv",
                          number = 3,
                          summaryFunction = maeSummary,
                          verboseIter = TRUE)

gbmGrid <-  expand.grid(interaction.depth = c(3,4,5,6), 
                        n.trees = seq(from = 100, to = 3700, by = 150), 
                        shrinkage = c(0.03, .01, 0.003),
                        n.minobsinnode = 10)

abslogerror2 <- train(logerror ~ .,
                     data = subTrain, 
                     method = "gbm", 
                     preProcess = c("center", "scale", "BoxCox"),
                     metric = "MAE",
                     maximize = FALSE,
                     tuneGrid = gbmGrid,
                     trControl = rdmSearch
                     )

abslogerror
plot(abslogerror)
abslogerror$bestTune
#     n.trees interaction.depth shrinkage n.minobsinnode
# 49    1000                 5      0.01             20

gbmImp <- varImp(abslogerror, scale = FALSE)
plot(gbmImp, top = 20)

results <- data.frame(obs = subTest$logerror, 
                      pred = predict(abslogerror, newdata = subTest))
maeSummary(results)
#       MAE 
# 0.06294201 

cor(results)
#           obs      pred
# obs  1.0000000 0.1993451
# pred 0.1993451 1.0000000

saveRDS(abslogerror, "model2_v2.rds")



