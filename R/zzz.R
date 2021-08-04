## Startup functions ------------------------------------

#' .onAttach start message
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return invisible()
.onAttach <- function(libname, pkgname) {
  start_message <- c( "\nThanks for using the AdaptiveLearnalytics package.\n\n"
                      , "Please cite this package when you use it.\n\n"
                      , "We welcome your questions or feedback: Anshul Kumar <akumar@mghihp.edu>\n\n"
                      , "Suggested citation:\nAnshul Kumar and Roger Edwards. 2021. AdaptiveLearnalytics: Adaptive Predictive Learning Analytic Tools. https://github.com/readcreate/AdaptiveLearnalytics \n\n"
                      , "Run the following code for more information:\nhelp(package = 'AdaptiveLearnalytics')\n\n"
                      , "This package was initially developed to assist with the following publication: Anshul Kumar, Lisa Walker, and Roger Edwards (2021), *The application of predictive analytics to identify at-risk students in health professions education.*\n\n"
                      , "We used the following resources to make this package:\n* Erik Erhardt. 2018. R Package Development. https://statacumen.com/teach/ShortCourses/R_Packages/R_Package_Development_20180817.html \n* Hadley Wickham. R Packages. https://r-pkgs.org/ \n* Sharon Machlis. 2019. How to write your own R package. IDG TECHtalk. https://www.youtube.com/watch?v=qxRSzDejea4\n\n"
  )
  packageStartupMessage(start_message)
  invisible()
}


#' .onLoad getOption package settings
#'
#' @param libname defunct
#' @param pkgname defunct
#'
#' @return
#'
#' @examples
#' getOption("AKmisc.name")
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.AKmisc <- list(
    #AKmisc.path = "~/R-dev",
    AKmisc.install.args  = "",
    AKmisc.name          = "Anshul Kumar, Roger Edwards",
    AKmisc.desc.author   = "Anshul Kumar <akumar@mghihp.edu>",
    AKmisc.desc.license  = "GNU GPLv3",
    AKmisc.desc.suggests = NULL,
    AKmisc.desc          = list()
  )
  toset <- !(names(op.AKmisc) %in% names(op))
  if (any(toset)) options(op.AKmisc[toset])

  invisible()
}
