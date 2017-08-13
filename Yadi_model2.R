


############# load library #################
library(data.table)
library(caret)
library(dplyr)

# Setting Parallel processing
# library(doMC)
# library(parallel)
# number_of_cores <- detectCores()
# registerDoMC(cores = number_of_cores/2)

############### load file #################
train_data1 = fread("train_data_v1.csv", drop = 1)


############## only predicting absolute logerror ###########
train_data1$logerror = abs(train_data1$logerror)

############## change columns to factors #############
factor_cols <- c('aircon',
                 'heating',
                 'num_pool',
                 'pooltypeid7',
                 'zoning_landuse',
                 'region_county',
                 'num_75_bath',
                 'num_story',
                 'flag_fireplace'
                 )

train_data1 <- train_data1 %>% 
  mutate_at(.funs = as.factor, 
            .vars = intersect(names(train_data1), factor_cols))

str(train_data1)



############## split data #############################
## Data splitting based on the outcome
set.seed(123)
trainIndex <- createDataPartition(train_data1$logerror, 
                                  p = .75, 
                                  list = FALSE, 
                                  times = 1)
## training set
subTrain <- train_data1[ trainIndex,-1]
## testing set
subTest  <- train_data1[-trainIndex,-1]

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

gbmGrid <-  expand.grid(interaction.depth = c(1,3,5,7), 
                        n.trees = seq(from = 50, to = 350, by = 100), 
                        shrinkage = c(0.3, .1, 0.03, .01, 0.003),
                        n.minobsinnode = 20)

abslogerror <- train(logerror ~ .,
                     data = subTrain, 
                     method = "gbm", 
                     preProcess = c("center", "scale"),
                     metric = "MAE",
                     maximize = FALSE,
                     tuneGrid = gbmGrid,
                     trControl = rdmSearch
                     )

abslogerror
plot(abslogerror)
abslogerror$bestTune
#     n.trees interaction.depth shrinkage n.minobsinnode
# 44     350                 5      0.03             20

gbmImp <- varImp(abslogerror, scale = FALSE)
plot(gbmImp, top = 20)

results <- data.frame(obs = subTest$logerror, 
                      pred = predict(abslogerror, newdata = subTest))
maeSummary(results)
#       MAE 
# 0.06297067

cor(results)
#           obs      pred
# obs  1.0000000 0.2057272
# pred 0.2057272 1.0000000

saveRDS(abslogerror, "model2_v1.rds")



