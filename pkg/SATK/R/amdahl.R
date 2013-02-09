#' Create a amdalh object from a dataframe or matrix object
#'
#' Extends the native \code{nls} object to specialize it calculating Gunther's
#'nls models for Universal Scaling Law. Allows multiple models to be
#'stored in one workspace.
#'
#' @param x a data.frame or matrix object containing data for model.
#' A data.frame should the data in columns named \code{N},for "scaled objects"
#' (e.g. users, cpus), and \code{X_N}, for measured throughput. The default for
#' a matrix object is to put scaled objects in the matrix's column 1
#' (i.e. \code{x[,1]}) and throughput in column 2 \code{x[,2]}
#' @param start  a named list with start values for nls
#' @param ... additional arguments to pass to function
#' @return sfit object
#' @export
#' @examples
#' data(USLcalc)
#' AMD1<-amdahl(USLcalc)
#'
amdahl <- function(x,start=c(alpha=.1),...)
  {

    
    amdsfit <- sfit(
                    formula=Norm ~ N/(1 + alpha * (N-1)),
                    x=x,
                    stype="amdahl",
                    start=start,
                    ...
                    )

    if (is.null(amdsfit) ) {
      return(NULL)
    }
    amdsfit$Nmax <- Inf
    amdsfit$Xmax <- Inf
    amdsfitval<-structure(amdsfit,class=c("amdahl",class(amdsfit)))
    amdsfitval
  }

#' xroof method for an amdahl object 
#'
#' A xroof specialized for amdahl objects 
#'
#' @rdname xroof.amdahl
#' @param object  an amdhal object
#' @method xroof amdahl
#' @S3method xroof amdahl
#'
xroof.amdahl <- function(object,...)
{
  return(measured(object)$X_N[1]/coef(object)['alpha']) 
}
