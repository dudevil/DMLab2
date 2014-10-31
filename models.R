require(mgcv)
require(boot)
require(gbm)

source(paste0(getwd(), '/preprocess.R'))


splitdata <- function(dfr, proportion=0.9, seed=NULL) {
  # split the data into training and validation set with proportion
  if (!is.null(seed)) set.seed(seed)
  index <- 1:nrow(dfr)
  trainindex <- sample(index, trunc(length(index)*proportion))
  trainset <- dfr[trainindex, ]
  validset <- dfr[-trainindex, ]
  list(trainset=trainset,validset=validset)
}

rmsle <- function(y, y_hat){
  # calculate the root mean square logarithmic error
  # see: http://www.kaggle.com/c/online-sales/details/Evaluation
  return(sqrt(sum((log(y_hat+1) - log(y+1))^2)/length(y)))
}

rmse <- function(y, y_hat){
  # calculate the root mean square error
  return(sqrt(mean((y_hat - y)^2)))
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

sets <- splitdata(test.data)
trainset = sets[['trainset']]
validset = sets[['validset']]

test.gbm <- function(x) {
  gbm.fit(
    x = x[,names(x) != 'Outcome'],
    y = x[,c('Outcome')],
    distribution = 'gaussian',
    n.trees = 5000,
    nTrain = round(0.9 * nrow(trainset)),
    shrinkage = 0.01,
    interaction.depth = 4)
  
}

#cv(test.gbm, predict, data=test.data, K=5, cost=rmsle)
