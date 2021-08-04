#' "Adaptive Minimum Match" KNN regression (AMMKNN)
#'
#' Runs an "adaptive minimum matching" KNN (AMMKNN) regression procedure and returns the predictions along with the initially provided new/testing data. This returned data frame can also be used to apply a different rule for choosing the value of K, if the user desires. The predicted values of the dependent variable are stored in the returned data.frame in a variable called `MatchDVMeanMin`. This method was developed and proposed by Anshul Kumar, Lisa Walker, and Roger Edwards in the 2021 article "The application of predictive analytics to identify at-risk students in health professions education."
#'
#' @param trainX a data frame containing training independent variables only
#' @param trainY a data frame containing training dependent variable only
#' @param newX a data frame containing independent variables for new observations
#   on which predictions will be made. These must be the same independent variables as those in `trainX`
#' @param maxK an integer, the maximum number of neighbors to consider
#'
#' @return data.frame object
#' @export
#'
#' @examples
#' trainingRowIndex <- sample(1:nrow(mtcars), 0.75*nrow(mtcars))
#' dtrain <- mtcars[trainingRowIndex, ] # model training data
#' dtest <- mtcars[-trainingRowIndex, ] # model test data
#' dtestWithMatches <- adaptiveMinMatchKNNregression(trainX = subset(dtrain, select = -mpg), trainY = dtrain[c("mpg")], newX = subset(dtest, select = -mpg), maxK = 5)
#' if (!require(Metrics)) install.packages('Metrics')
#' library(Metrics)
#' Metrics::rmse(dtestWithMatches$MatchDVMeanMin, dtest$mpg)
#' plot(dtestWithMatches$MatchDVMeanMin, dtest$mpg, main = "mpg predictions, adaptive minimum match KNN", xlab = "predicted", ylab = "actual")
#'
adaptiveMinMatchKNNregression <- function(trainX, trainY, newX, maxK){

  if(ncol(trainY)!=1) stop("trainY must contain only one column")
  # ssma need to test^


  combinedX <- rbind(trainX, newX)

  if (!require(factoextra)) install.packages('factoextra')
  library(factoextra)
  distance <- get_dist(combinedX)
  distance <- as.matrix(distance)

  # make new distance matrix newdist, which is a subset of the initial
  # distance matrix called distance, in which each column is a
  # newX observation and each row is that
  # newX observation's distance to a training observation.
  # newdist has the same number of rows as trainX and the same number of
  # columns as newX.
  # this will be the top-right quadrant of the initial distance matrix.
  # Our goal is to find matches within trainX for each
  # observation in newX.
  newdist <- distance[1:nrow(trainX),(nrow(trainX)+1):ncol(distance)]
  newdist <- as.data.frame(newdist) #ssma20210801
  rownames(newdist) <- seq(1:nrow(newdist))
  colnames(newdist) <- seq(1:ncol(newdist))
  newdist <- cbind(as.data.frame(newdist),trainY)
  newdist$IDnum <- seq(1:nrow(newdist))

  dfinal2 <- newX[-c(1:nrow(newX)),] # make empty dataset

  for(r in 1:nrow(newX)){ # iterate through newX rows/observations

    # cat("newX has",nrow(newX),"rows")
    # cat("\n\nr =",r,"\n")

    currentNewX <- newX[r,] # extract current row

    # make currentMatches dataframe (which has nrow(trainX) rows and 2 columns)
    # containing the following columns related
    # to the current newX observation: distance to all trainX observations,
    # dependent variable (from trainY) value for each trainX observation.
    # Note that colnames(trainY)[1] gets the name of the dependent variable
    # from trainY.
    eval(parse(text=paste(
      "currentMatches <- subset(newdist, select = c(`",
      r,"`, ",colnames(trainY)[1],", IDnum))",
      sep = "")))

    # order the matches from closest to farthest match
    eval(parse(text=paste(
      "currentMatchesSorted <- currentMatches[order(currentMatches$`",r,"`),]",
      sep = "")))
    # SSMA need to figure out how to name the r variable, since r is a number
    # currentMatchesSorted <- currentMatches[order(subset(DATASETNAME, select = r)),]

    currentMatchesSortedK <- currentMatchesSorted[1:maxK,]

    for(n in 1:nrow(currentMatchesSortedK)){ # iterate through distance matches

      # cat(n,"\n")

      # add nth match's ID to current observation as new column
      eval(parse(text=paste("currentNewX$EucMatchID.", n, " <- ",
                            currentMatchesSortedK[n,"IDnum"],
                            sep = "")))

      # add nth match's distance from current observation
      # to current observation as new column
      eval(parse(text=paste("currentNewX$EucMatchDist.", n, " <- ",
                            currentMatchesSortedK[n,1], sep = "")))

      # add nth match's DV value to current observation, as new column
      eval(parse(text=paste("currentNewX$EucMatchDV.", n, " <- ",
                            currentMatchesSortedK[n,colnames(trainY)[1]], sep = "")))

      # calculate mean of DV of first n matches
      eval(parse(text=paste("currentNewX$EucMatchDVMean.", n, " <- ",
                            apply(currentNewX[grep("EucMatchDV\\.",
                                                   names(currentNewX))],1,mean),
                            sep = "")))

      # calculate SD of DV of first n matches
      eval(parse(text=paste("currentNewX$EucMatchDVSD.", n, " <- ",
                            apply(currentNewX[grep("EucMatchDV\\.",
                                                   names(currentNewX))],1,sd),
                            sep = "")))

    }


    # calculate minimum DV value of all k matches
    eval(parse(text=paste("currentNewX$MatchDVMin <-",
                          apply(currentNewX[grep("EucMatchDV\\.",
                                                 names(currentNewX))],1,min),
                          sep = "")))

    # calculate minimum value of all k matches
    eval(parse(text=paste("currentNewX$MatchDVMax <-",
                          apply(currentNewX[grep("EucMatchDV\\.",
                                                 names(currentNewX))],1,max),
                          sep = "")))

    # calculate minimum value of mean of all k matches
    eval(parse(text=paste("currentNewX$MatchDVMeanMin <-",
                          apply(currentNewX[grep("EucMatchDVMean\\.",
                                                 names(currentNewX))],1,min),
                          sep = "")))

    # calculate minimum value of mean of all k matches
    eval(parse(text=paste("currentNewX$MatchDVMeanMax <-",
                          apply(currentNewX[grep("EucMatchDVMean\\.",
                                                 names(currentNewX))],1,max),
                          sep = "")))

    # add the current newX observation to dfinal2
    dfinal2 <- rbind(dfinal2,currentNewX)

  }


  return(dfinal2)
}
