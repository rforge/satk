#' Create a aloha object from a dataframe or matrix object
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
#' @param start named list with the start guess for alpha
#' @param ... additional arguments to pass to function
#' @return sfit object
#' @export
#' @examples
#' data(USLcalc)
#' AL1<-aloha(USLcalc)
#'
aloha<- function(x,start=c(alpha=.1),...)
  {

    
    alohasfit <- sfit(
                       Norm ~ N*exp(-alpha*(N-1)),
                        x,
                        stype="aloha",
                        start=start,
                        ...
                        )

    if (is.null(alohasfit) ) {
      return(NULL)
    }
    #wrong but I'm too tired to figure this out now
    alohasfit$Nmax <- NA
    alohasfit$Xmax <- NA
    alohasval <- structure(alohasfit,class=c("aloha",class(alohasfit)))
    alohasval
  }
 
#' @rdname format_legend
#' @method format_legend aloha
#' @S3method format_legend aloha

format_legend.aloha <- function(object) {
           legend_text=sprintf("expression(alpha == %.4f, R^2 == %.4f,Nmax==%.2f,Xmax==%.2f,Xroof==%.2f,Z(sec)==%.2f)",
                               coef(object)['alpha'],
                               1-sse(object)/sst(object),
                               Nmax(object),
                               Xmax(object),
                               measured(object)$X_N[1]/coef(object)['alpha'],
                               0.0)

           eval(parse(text=legend_text))

         }

