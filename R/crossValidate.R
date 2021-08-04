#' Cross Validate Predictive Models
#'
#' Perform "manual" cross validation. This function automatically creates folds (groups) in your data to cross-validate predictive models. It executes provided code, repeatedly on each training/testing data split, using each fold as the testing data one time. Returns a data.frame with original data (in original order) and the following additional variables: predicted values of the dependent variable, new ID number (according to the reordered data), original order number, and fold number.
#'
#' @param df data.frame containing all observations, independent variables, and dependent variable
#' @param codeString string containing code to run your ML model, which this function will run for you on each fold. Separate lines with a semicolon. Anywhere you write `traindata`, this function will replace it with the training portion of your data for each fold. Anywhere you write `testdata`, this function will replace it with the current testing fold in the cross validation. Variables you add to `testdata` in your code will be included in the returned data.frame.
#' @param numFolds number of validation folds (groups). Use `nrow(df)` for leave-one-out cross validation (LOOCV).
#' @param randomSort boolean (default TRUE). When TRUE, this function will randomly reorder the observations in your data.frame `df` prior to performing cross validation. When FALSE, `df` will be left in the original order.
#'
#' @return data.frame object
#' @export
#'
#' @examples
#' if (!require(randomForest)) install.packages('randomForest')
#' library(randomForest)
#' mtcars.cv5.rf <- crossValidate(df = mtcars, codeString =  "rf <- randomForest(mpg ~ ., data=traindata, proximity=TRUE,ntree=1000); testdata$rf.pred.mpg <- predict(rf, newdata = testdata)", numFolds = 5, randomSort = TRUE)
#' if (!require(Metrics)) install.packages('Metrics')
#' library(Metrics)
#' Metrics::rmse(mtcars.cv5.rf$rf.pred.mpg, mtcars.cv5.rf$mpg)
#' plot(mtcars.cv5.rf$rf.pred.mpg, mtcars.cv5.rf$mpg, main = "mpg predictions, 5-fold cross validation, random forest", xlab = "predicted", ylab = "actual")
#'
#' if (!require(FNN)) install.packages('FNN'
#' library(FNN)
#' mtcars.loocv.knn4 <- crossValidate(df = mtcars, codeString = 'dtrain.x <- subset(traindata, select = -mpg); dtrain.y <- subset(traindata, select = mpg); dtest.x <- subset(testdata, select = -mpg); dtest.y <- subset(testdata, select = -mpg); pred1 <- FNN::knn.reg(dtrain.x, dtest.x, dtrain.y, k = 4); testdata$knn4.pred.mpg <- pred1$pred', numFolds = nrow(mtcars), randomSort = TRUE)
#' if (!require(Metrics)) install.packages('Metrics')
#' library(Metrics)
#' Metrics::rmse(mtcars.loocv.knn4$knn4.pred.mpg, dtest.y$mpg)
#' plot(mtcars.loocv.knn4$knn4.pred.mpg, dtest.y$mpg, main = "mpg predictions, LOOCV, KNN (k = 4)", xlab = "predicted", ylab = "actual")
#'
crossValidate <- function(df, codeString, numFolds, randomSort = TRUE){

  # Record original order of observations
  df$origOrder <- seq(1:nrow(df))

  # Randomly reorder data, if user wants
  if(randomSort){
    df <- df[sample(1:nrow(df)), ]
  }

  # RandomlySortedDataCopy <- df

  groupsize <- nrow(df)/numFolds # determine average fold (group) size
  df$idnum <- seq(1:nrow(df)) # create ID number for each observation
  df$group <- ceiling(df$idnum/groupsize) # assign group number to observations

  dfinal <- df[-c(1:nrow(df)),] # make empty dataset with all variables

  # Run codeString once for each fold
  for(i in 1:numFolds) {
    cat("\n=== Repetition",i,"of",numFolds,"===\n\n")

    # Prepare data
    traindata <- df[df$group!=i,]
    testdata <- df[df$group==i,]

    traindata$origOrder <- NULL
    traindata$group <- NULL
    traindata$idnum <- NULL

    origOrderTemp <- testdata$origOrder
    testdata$origOrder <- NULL
    testdata$group <- NULL
    testdata$idnum <- NULL


    # Run model
    eval(parse(text=paste(codeString, sep = "")))

    testdata$origOrder <- origOrderTemp

    dfinal <- rbind(dfinal,testdata)
  }


  dfinal <- dfinal[order(dfinal$origOrder),]

  dfinal$origOrder <- NULL

  return(dfinal)
}
