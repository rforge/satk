#' Create a usl object from a dataframe or matrix object
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
#' @param start a named list with initial guess two usl parameters
#' @param ... additional arguments to pass to function
#' @return sfit object
#' @export
#' @examples
#' data(USLcalc)
#' USL1<-usl(USLcalc)
#'
#' data(SSUSLcalc)
#' USL2<-usl(SSUSLcalc,data.cols=c("V1","V2"),start=c(alpha=.1,beta=.001))
usl <- function(x,start=c(alpha=.1,beta=.01),...)
  {

    
    uslsfit <- sfit(
                    Norm ~ N/(1 + alpha * (N-1) + beta * N * (N-1)),
                    x,
                    stype="usl",
                    start=start,
                    ...
                    )
      
    if ( is.null(uslsfit) ) {
      return(NULL)
    }
    
#     if ( coef(uslsfit)['beta'] < 0 ) {
#           warning("usl: fit produced unphysical beta coefficient")
#           print(coef(uslsfit))
#           return(NULL)
#     }
      
    uslsfit$Nmax <- as.numeric(sqrt((1-coef(uslsfit)['alpha'])/coef(uslsfit)['beta']))
    uslsfit$Xmax <- as.numeric(measured(uslsfit)$X_N[1] *
                                   uslsfit$Nmax /(1  + coef(uslsfit)['alpha'] * (uslsfit$Nmax-1) +
                                                      coef(uslsfit)['beta'] * uslsfit$Nmax * (uslsfit$Nmax-1)))
    uslsfitval <- structure(uslsfit,class=c("usl",class(uslsfit)))
    uslsfitval
  }

