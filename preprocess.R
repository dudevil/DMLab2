require(plyr)
require(reshape2)

source(paste0(getwd(), '/read_data.R'), echo=F)


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


dates <- function(dfr){
  return(dfr[,grepl("^Date_", names(dfr))])
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


imputeMedians <- function(dfr){
  # imputes missing values to quantitative features
  # dummy now, use distribution imputing later?
  data.frame(
    apply(
      dfr,
      2, 
      function(x) replace(x, is.na(x), median(x, na.rm=T))))
  #Quan_2 is close to normal but right-skewed, use median
  #dfr[is.na(dfr[,'Quan_2']),'Quan_2'] <- median(dfr[,'Quan_2'], na.rm=T)
  #Quan_3 looks Gamma (?) use median
  #dfr[is.na(dfr[,'Quan_3']),'Quan_3'] <- median(dfr[,'Quan_3'], na.rm=T)
  #Quan_3 looks Exponential, use median
  #dfr[is.na(dfr[,'Quan_4']),'Quan_4'] <- median(dfr[,'Quan_4'], na.rm=T)
  #Quan_15 is even more  Exponential, use median
  #dfr[is.na(dfr[,'Quan_15']),'Quan_15'] <- median(dfr[,'Quan_15'], na.rm=T)
  # Quan_27 and Quan_28 have two levels, impute 0, which is also median
  #dfr[is.na(dfr[,'Quan_27']),'Quan_27'] <- median(dfr[,'Quan_27'], na.rm=T)
  #dfr[is.na(dfr[,'Quan_28']),'Quan_28'] <- median(dfr[,'Quan_28'], na.rm=T)
  #return(dfr)
}

# select best quantitative features:
quants.best <- quantitativeFeatures(train.data)[,
                                                c('Quan_1',
                                                  'Quan_2',
                                                  'Quan_3',
                                                  'Quan_4',
                                                  'Quan_15')]
quants.best.imputed <- imputeMedians(quants.best)

#select categorical features and cast them to factors
cats <- categoricalFeatures(train.data)
cats <- data.frame(apply(cats, 2, as.factor))

# merge data together
train.data <- cbind(
  outcomes(train.data), 
  quants.best, 
  dates(train.data),
  cats
  )

#extract a "month" feature from the outcomes variables

train.data <- melt(
  data = train.data,
  measure.vars = names(train.data)[grepl('^Outcome_M', names(train.data))],
  variable.name = 'Month',
  value.name = 'Outcome',
  na.rm=T,
  )

train.data <- mutate(train.data, Month=sub('^Outcome_M','', Month))

train.data$Month <- as.factor(train.data$Month)


# prepare test.data in the same manner

quants.test <- quantitativeFeatures(test.data)[,
                                                c('Quan_1',
                                                  'Quan_2',
                                                  'Quan_3',
                                                  'Quan_4',
                                                  'Quan_15')]
quants.test <- imputeMedians(quants.test)


cats.test <- categoricalFeatures(test.data)
cats.test <- data.frame(apply(cats.test, 2, as.factor))

# merge data together
test.data <- cbind(
  quants.test, 
  dates(test.data),
  cats.test
)

#simulate the Month variable

test.data <- merge(data.frame(Month=1:12), test.data)