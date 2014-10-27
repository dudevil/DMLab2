require(mgcv)
require(boot)

dfr <- read.csv('data//TrainingDataset.csv')

rmsle <- function(y, y_hat){
  # calculate the root mean square logarithmic error
  # see: http://www.kaggle.com/c/online-sales/details/Evaluation
  return(sqrt(sum((log(y_hat+1) - log(y+1))^2)/length(y)))
}

rmse <- function(y, y_hat){
  # calculate the root mean square error
  return(sqrt(mean((y_hat - y)^2)))
}

quantitativeFeatures <- function(dfr){
  # exctracts quantitative features from the dataframe by regexp
  return(dfr[,grepl("^Quan_", names(dfr))])
}

categoricalFeatures <- function(dfr){
  # exctracts quantitative features from the dataframe by regexp
  return(dfr[,grepl("^Cat_", names(dfr))])
}

outcomes <- function(dfr){
  return(dfr[,grepl("^Outcome_M", names(dfr))])
}

quantitativeMetrics <- function(dfr){
  # performs a quick look at the quntitative features:
  # the number of NA's, unique values and the overall sum 
  # the sum is here becouse some of the features are always 0
  quan.metrics <- data.frame(apply(dfr,2,function(col) sum(is.na(col))))
  quan.metrics <- cbind(quan.metrics, apply(dfr, 2, function(col) length(unique(col))))
  quan.metrics <- cbind(quan.metrics, apply(dfr, 2, function(col) sum(col, na.rm=T)))
  names(quan.metrics) <- c('NumNAs', 'NumUnique','Sum')
  quan.metrics[with(quan.metrics,order(NumNAs,-NumUnique,Sum)),]
}

#pick the "best" features as a start
quants.best <- quantitativeFeatures(dfr)[,c('Quan_1', 
                                            'Quan_2',
                                            'Quan_3',
                                            'Quan_4',
                                            'Quan_15',
                                            'Quan_27',
                                            'Quan_28')]

imputeMissing <- function(dfr){
  # imputes missing values to quantitative features
  # dummy now, use distribution imputing later?
  
  #Quan_2 is close to normal but right-skewed, use median
  dfr[is.na(dfr[,'Quan_2']),'Quan_2'] <- median(dfr[,'Quan_2'], na.rm=T)
  #Quan_3 looks Gamma (?) use median
  dfr[is.na(dfr[,'Quan_3']),'Quan_3'] <- median(dfr[,'Quan_3'], na.rm=T)
  #Quan_3 looks Exponential, use median
  dfr[is.na(dfr[,'Quan_4']),'Quan_4'] <- median(dfr[,'Quan_4'], na.rm=T)
  #Quan_15 is even more  Exponential, use median
  dfr[is.na(dfr[,'Quan_15']),'Quan_15'] <- median(dfr[,'Quan_15'], na.rm=T)
  # Quan_27 and Quan_28 have two levels, impute 0, which is also median
  dfr[is.na(dfr[,'Quan_27']),'Quan_27'] <- median(dfr[,'Quan_27'], na.rm=T)
  dfr[is.na(dfr[,'Quan_28']),'Quan_28'] <- median(dfr[,'Quan_28'], na.rm=T)
  return(dfr)
}

cv <- function(train_fun, predict_fun, data, response_col, K=5, cost=rmse){
  # Performs a K-fold cross-validation
  # Args:
  #    train_fun: a function used to train a model. Must accept a single agrument
  #      with training data, and return the trained model
  #    predict_fun: a function used for prediction. Must accept two arguments:
  #      first is a trained model, second is the test fold. Must return predicted
  #      output for the test data
  #    data: a data.frame to split into train/test sets
  #    response_col: response column name
  #    K: number of folds, default = 5
  #    cost: a cost function. The first argument is the true values, the second is
  #      the predicted values
  #
  # Value:
  #    returns an averaged output of the cost function across folds.
  # Example:
  #    cv(function(x) glm(formula = Outcome ~ ., data = x), predict, data, 'Outcome',K=10)

  # prepare errors metric
  err <- rep(NA, K)
  # randomly shuffle the data
  data <- data[sample(nrow(data)),]
  # Create K equally size folds
  folds <- cut(seq(1,nrow(data)),breaks=K,labels=FALSE)
  #Perform K fold cross validation
  for(i in 1:K){
    #Segement data by fold
    testIndexes <- which(folds==i,arr.ind=TRUE)
    testData <- data[testIndexes, ]
    trainData <- data[-testIndexes, ]
    # train model
    model <- train_fun(trainData)
    # predict test set
    prediction <- predict_fun(model, testData)
    err[i] <- cost(testData[,response_col], prediction)
  }
  mean(err)
}
