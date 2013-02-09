#' Dump measured data
#'
#' Dump the scale_fit  model data from nls environment
#'
#' @param object a sfit object
#' @param drop.norm logical indicating whether to print Norm column
#' @param ... additional arguments
#' @export
#' @examples
#' data(USLcalc)
#' sl1<-sflist(USLcalc)
#' measured(sl1)
#'
#' @rdname measured
#' @export measured
measured <- function(object,...) UseMethod("measured")

#' @return data.frame
#'
#' @rdname measured
#' @method measured sfit
#' @S3method measured sfit
measured.sfit <- function(object,drop.norm=TRUE,...)
  {
    df<-get(as.character(object$data),envir=object$m$getEnv())

    if (drop.norm)
      {
        return(df[,c("N","X_N")])
      }
    df
  }

#' @rdname measured
#' @method measured sflist
#' @S3method measured sflist
measured.sflist <- function(object,drop.norm=TRUE,...)
  {
    measured.sfit(object[[1]],drop.norm,...)
  }
