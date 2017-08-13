


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


############## only predicting whether overestimate or underestimate ###########
train_data1$logerror = ifelse(train_data1$logerror <= 0, 0, 1)

############## change columns to factors #############
factor_cols <- c('aircon',
                 'heating',
                 'num_pool',
                 'pooltypeid7',
                 'zoning_landuse',
                 'region_county',
                 'num_75_bath',
                 'num_story',
                 'flag_fireplace',
                 'logerror'
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

################## grid search ##########################
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE
                           )

gbmGrid <-  expand.grid(mtry = 2:13
                        )

under_or_over <- train(logerror ~ .,
                       data = subTrain, 
                       method = "rf", 
                       maximize = TRUE,
                       tuneGrid = gbmGrid,
                       trControl = gridSearch
                       )

under_or_over
plot(under_or_over)
under_or_over$bestTune
#   mtry
# 5    6

gbmImp <- varImp(under_or_over, scale = FALSE)
plot(gbmImp, top = 20)

results <- data.frame(obs = subTest$logerror, 
                      pred = predict(under_or_over, newdata = subTest))
table(results)
#     pred
# obs    0    1
#   0 2924 7204
#   1 2448 9992

# accuracy 57.2%

saveRDS(under_or_over, "model1_v1.rds")

#######################

gridSearch2 <- trainControl(method = "boot",
                           number = 5,
                           verboseIter = TRUE
)

under_or_over2 <- train(logerror ~ .,
                       data = train_data1[,-1], 
                       method = "rf",  
                       maximize = TRUE,
                       tuneGrid = under_or_over$bestTune,
                       trControl = gridSearch2
)

under_or_over2
# Accuracy   Kappa     
# 0.5707707  0.09247681



###################### using gbm #######################
gridSearch <- trainControl(method = "cv",
                           number = 3,
                           verboseIter = TRUE)

gbmGrid <-  expand.grid(interaction.depth = c(3,5,7), 
                        n.trees = c(100,200,500), 
                        shrinkage = c(0.3, .1, 0.03, 0.01),
                        n.minobsinnode = 20)

gbmFit2 <- train(logerror ~ .,
                 data = subTrain, 
                 method = "gbm", 
                 metric = "Accuracy",
                 maximize = TRUE,
                 tuneGrid = gbmGrid,
                 trControl = gridSearch
                 )

gbmFit2
plot(gbmFit2)
gbmFit2$bestTune
#     n.trees interaction.depth shrinkage n.minobsinnode
# 18     500                 7      0.03             20

gbmImp2 <- varImp(gbmFit2, scale = FALSE)
plot(gbmImp2, 30)

results2 <- data.frame(obs = subTest$logerror, 
                      pred = predict(gbmFit2, newdata = subTest))
table(results2)
#   pred
# obs     0     1
#   0  2852  7276
#   1  2364 10076

# accuracy 57.3%

saveRDS(gbmFit2, "model1_v2.rds")
