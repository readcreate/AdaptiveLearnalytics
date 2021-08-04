#' Correlation Variable Selection
#'
#' Identifies which variables in a data.frame (`df`) are correlated with a single selected variable (`mainVarName`) above or at a designated threshold (`threshold`). Returns a new data.frame with only these variables. Returns a data.frame that contains `mainVarName` and all variables in `df` that are correlated above or equal to threshold with `mainVarName`.
#'
#' @param mainVarName string with name of variable to correlate with all others
#' @param df data.frame from which variables under `threshold` will be removed
#' @param threshold correlation threshold to use for variable selection
#'
#' @return data.frame object
#' @export
#'
#' @examples
#' mtcars.mpg.cor.50 <- corVarSelect(mainVarName = "mpg", df = mtcars, threshold = .5)
#'
#'
corVarSelect <- function(mainVarName, df, threshold){


  # Output summary of starting situation
  cat("\n\nStarting process. Input dataframe is called ", deparse(substitute(df)),
      " and contains ",nrow(df)," observations and ",ncol(df),
      " variables, including ",mainVarName,".\n\n", sep = '')

  # Extract main variable and remove it from df
  # MIGHT NOT BE NEEDED
  # eval(parse(text=paste(mainVarVector, " <- df$", mainVarName, sep = "")))
  # eval(parse(text=paste("df$",mainVarName, " <- NULL", sep = "")))

  # Create empty dataframe with one variable (so that it's not empty)
  dftemp <- data.frame(temp=c(1:nrow(df)))

  # Identify sufficiently correlated variables
  for(i in 1:length(names(df))){ # iterate through variables

    # calculate correlation between current variable and mainVarName
    eval(parse(text=paste("c <- cor(df[i],df$",mainVarName,")", sep = "")))

    # output correlation for user reference
    cat("\n\n",i,". Correlation between ",names(df[i]),
        " (SD = ",sd(df[[i]]),") and ",mainVarName," = ",c,".", sep = '')

    # include sufficiently correlated variables in final dataframe
    if(c>=threshold) {
      dftemp <- cbind(dftemp,df[i])
    }
  }

  # Remove temporary variable that was initially in dftemp
  dftemp$temp <- NULL

  cat("\n\nVariable selection process is complete. Returned dataframe contains ",
      nrow(dftemp)," observations and ",ncol(dftemp),
      " variables, including ",mainVarName,".\n\n", sep = '')

  # Give the final version of dataframe back to the user
  return(dftemp)
}
