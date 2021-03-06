rm(list=ls())
setwd('C:/Bootcamp/projects/ML-project/ML_run')
cat('\014')

library(glmnet)
library(dplyr)
library(ISLR)

properties_train=readRDS("TrainClned.rds") %>% select(.,-year)
properties=readRDS("PropertiesClned.rds") %>% mutate(.,month=10)
setnames(properties, "id_parcel", "parcelid")
setnames(properties_train, "id_parcel", "parcelid")

properties=select_if(properties,is.numeric)
properties_train=select_if(properties_train,is.numeric)

x = model.matrix(logerror ~ ., properties_train)[,c(-1)]  #remove intercept
y = properties_train$logerror

#Creating training and test sets with an 75-25 split, respectively.
set.seed(0)
train = sample(1:nrow(x), 7.5*nrow(x)/10)
test = (-train)
y.test = y[test]

length(train)/nrow(x)
length(y.test)/nrow(x)


#Values of lambda over which to check.
grid = 10^seq(-7, 2, length = 100)


#Fitting the Lasso regression. Alpha = 1 for Lasso regression.
lasso.models = glmnet(x, y, alpha = 1, lambda=grid, standardize=TRUE, intercept=FALSE)

dim(coef(lasso.models))
coef_lasso=coef(lasso.models)
coef_lasso

#plotting the lasso model
plot(lasso.models, xvar = "lambda",  label = TRUE, main = "Lasso Regression")



#Perform 10-fold cross-validation
set.seed(0)
lasso_cv = cv.glmnet(x[train, ], y[train], alpha = 1, nfolds = 10, lambda = grid, intercept=FALSE)
plot(lasso_cv, main = "Lasso Regression\n")
bestlambda.lasso = lasso_cv$lambda.min
bestlambda.lasso
log(bestlambda.lasso)
coeff_lasso=coef(lasso_cv,s='lambda.min',exact=TRUE)
min(lasso_cv$cvm)

# predict on test data
set.seed(0)
lasso_fit = glmnet(x[train, ],y[train],alpha = 1,lambda = lasso_cv$lambda.min, intercept=FALSE)
y_pred = predict(lasso_fit,x[test, ])
mse_lasso = sum((y_pred-y.test)^2)/length(y.test)
print(mse_lasso)
mae_lasso = sum(abs(y_pred-y.test))/length(y.test)
print(mae_lasso)
# > mse_lasso
# [1] 0.05723179
 
# kaggle
# 0.0649128

plot(y.test,y_pred,xlab="Test logerror",ylab="Predicted logerror",
     main="Prediction using Lasso regression")
text(-1,3,substitute(r^2 == r2,list(r2=cor(y.test,y_pred))),adj=0)
text(-1,2.7,substitute(MSE == r2,list(r2=mse_lasso)),adj=0)
abline(0,1)


xn = model.matrix(~.,properties)[,c(-1)] #remove intercept
library(data.table)
makePrediction <- function(model, newdata, months, labels) {
  predictions <- newdata[, "parcelid", drop=FALSE]
  for(i in 1:length(months)) {
    newdata$month <- months[i]
    predictions[, labels[i]] <- NULL
    predictions[, labels[i]] <- round(predict(model, newdata = newdata, newx=xn),4)
  }
  write.csv(x = predictions, file = "submission1.csv",
            quote = FALSE, row.names = FALSE)
  return(predictions)
}
makePrediction(lasso_fit, newdata = properties, months = c(10, 11, 12, 22, 23, 24),
               labels = c("201610", "201611", "201612", "201710", "201711", "201712"))
