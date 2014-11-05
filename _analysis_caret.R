require(mgcv)
require(boot)
require(gbm)
require(caret)

# setwd("D:/R_direct/DMLab2") 

splitdata_caret <- function(dfr, seed = NULL){
  # Splitting data using Caret package with seed inside
  if (!is.null(seed)) set.seed(seed)
  trainIndex <- createDataPartition(dfr$Outcome, 
                                    p = 0.9, 
                                    list = FALSE,
                                    times = 1)
  dfrTrain <- dfr[trainIndex, ]
  dfrValid <- dfr[-trainIndex, ]
}

# Rewrite function with Caret requirements
rmsle <- function(data.frame(obs, pred), lev = NULL, model = "gbm"){
  # calculate the root mean square logarithmic error
  # see: http://www.kaggle.com/c/online-sales/details/Evaluation
  # Caret requires names like "obs" and "pred"
  dfr_test <- data.frame(obs, pred)
  return(sqrt(sum((log(dfr_test$pred+1) - log(dfr_testobs+1))^2)/length(obs)))
}

rmse <- function(y, y_hat){
  # calculate the root mean square error
  return(sqrt(mean((y_hat - y)^2)))
}

fitControl <- trainControl(
  # Built-in K-fold cross validation
  method = "repeatedcv",
  number = 10,
  # 10 different 10-fold resamples are used
  repeats = 10,
  # Custom metric function for assessing output
  summaryFunction = rmsle)

gbmCaret <- train(Outcome ~ ., data = dfrTrain,
                 method = "gbm",
                 n.trees = 500,
                 shrinkage = 0.01,
                 interaction.depth = 4,
                 trControl = fitControl,
                 # Does not print progress of iterations
                 verbose = FALSE,
                 metric = "rmse")
gbmCaret

prediction1 <- predict.gbm(gbmCaret, test.data, n.trees=453)