#' Create a gustafson object from a dataframe or matrix object
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
#' @param start a named list with a guess for alpha
#' @param ... additional arguments to pass to function
#' @return sfit object
#' @export
#' @examples
#' data(USLcalc)
#' gf1<-gustafson(USLcalc)
#'
gustafson <-
function (x, start=c(alpha = 0.1), ...) 
{
    gufsfit<- sfit(Norm ~ N - alpha * N + alpha,
                   x,
                   stype="gustafson",
                   start = start,
                   ...)
    
    if ( is.null(gufsfit) )
      {
        return(NULL)
      }
    gufsfit$Nmax <- Inf
    gufsfit$Xmax <- Inf
    gufsfitval <- structure(gufsfit,class=c("gustafson",class(gufsfit)))
    gufsfitval 
}

