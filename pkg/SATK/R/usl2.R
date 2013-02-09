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
#' USL2<-usl2(USLcalc)
#'
#' data(SSUSLcalc)
#' USL2<-usl(SSUSLcalc,data.cols=c("V1","V2"),start=c(alpha=.1,beta=.001))
usl2 <- function(x,start=c(alpha=.1,beta=.01),...)
  {

    
    uslsfit <- sfit(
                    Norm ~ N/(1 + alpha * (N-1) + beta * N * (N-1)),
                    x,
                    stype="usl2",
                    start=start,
                    ...
                    )
      
    if ( is.null(uslsfit) ) {
      return(NULL)
    }
    uslsfit$Nmax <- as.numeric(sqrt((1-coef(uslsfit)['alpha'])/coef(uslsfit)['beta']))
    uslsfit$Xmax <- measured(uslsfit)$X_N[[1]] *
      predict(uslsfit,data.frame(N=c(uslsfit$Nmax)))
    uslsfitval <- structure(uslsfit,class=c("usl2",class(uslsfit)))
    uslsfitval
  }

plot.usl2 <- function(x,...)
  {

    if(!inherits(x,"sfit"))  stop("'plot.sfit' not called x of sfit class")

    sfit_type<-attr(x,"sfit_type")
    
    input <- measured(x)
    fitvec<-as.vector(fitted(x))
    plot(input$N, input$X_N[1] * fitvec, type="l",
         lty="dashed",
         lwd=1,
         axes=T,
         ylab="Throughput X(N)", xlab="Virtual Users (N)",
         ...)
    title(paste("Fitted Scalability",sfit_type))
    points(input$N, input$X_N)
    Xroof <- (predict(x)[1]*measured(x)$X_N[1])/as.numeric(coef(x)['alpha'])
   legend("bottom",
           legend=eval(parse(text=sprintf("expression(alpha == %.4f,beta == %.4e, R^2 == %.4f, Nmax==%.2f, Xmax==%.2f,Xroof==%.2f,Z(sec)==%.2f)",
                               coef(x)['alpha'],
                               coef(x)['beta'],
                               1-sse(x)/sst(x),
                               Nmax(x),
                               Xmax(x),
                               Xroof,
                               0.0
                               ))),
           ncol=2)

  }

