#' Create a scale_fit object from a dataframe or matrix object
#'
#' Extends the native \code{nls} object to specialize it for fitting scaling models 
#'Allows multiple models to be
#'stored in one workspace.
#'
#' @param formula a formula for to fit
#' @param x a data.frame or matrix object containing data for model.
#' A data.frame should the data in columns named \code{N},for "scaled objects"
#' (e.g. users, cpus), and \code{X_N}, for measured throughput. The default for
#' a matrix object is to put scaled objects in the matrix's column 1
#' (i.e. \code{x[,1]}) and throughput in column 2 \code{x[,2]}
#' @param data.cols a vector indicating which columns in x has scaled object
#' number and throughput data. \code{data.cols[[1]]} is scaled objects,
#' and \code{data.col[[2]]} is measured throughput. It can be a numeric vector
#' containing column numbers or a character vector containing the column names.
#' @param start vector with initial guess for parameters
#' @param stype character string to set the sfit_type attribute to.
#' @param eff.check toggle check for over-acheivers
#' @param ... additional arguments to pass to function
#' @return sfit object
#' @export
sfit <- function(formula,x,stype=NA,data.cols,start,eff.check=FALSE,...)
  {

    if ( ! inherits(formula,"formula") )
      {

        print("formula must be a formula")
        stop()
      }

      
    if (! is.data.frame(x) && ! is.matrix(x))
      {
         print("x needs to be a data.frame or matrix object")
        stop()
        
      }
      
    if ( missing(data.cols) ) {
      if ( is.matrix(x) ) {
        cols <- c(1,2)
      } else {
        cols <- c("N","X_N")
      }
    } else {
      cols <- data.cols
    }
    
    if (length(cols) != 2 ) {
      print("data.cols must be a vector of length 2")
      stop()
    }

    measured.df<-as.data.frame(subset(x,TRUE,cols))

    
    if( nrow(measured.df) < 1 ) {
      print("Could not locate scaling data in x")
      stop()
    }
      
    names(measured.df)<-c("N","X_N")
    full_measured.df <- measured.df

    if ( full_measured.df[1,1] != 1) {

      fill_val <- fillx1(formula,full_measured.df,stype,start=start,...)
      full_measured.df <- data.frame(N=c(1,full_measured.df[,1]),X_N = c(fill_val,full_measured.df[,2]))

    }
    
    buildsfitval(formula,full_measured.df,stype=stype,start=start,eff.check=eff.check,...) 
  }

buildsfitval <- function(formula,x,stype=NA,data.cols,start,eff.check=FALSE,only.phys=TRUE,...) {
     measured.df<-transform(x,Norm = x$X_N/x$X_N[1])
    efficiencies <- measured.df$Norm/measured.df$N

    if ( eff.check ) {
      
      if (any(efficiencies > 1) ) {
        print(efficiencies)
        print("Over achievers: Some efficiencies > 100%")
        stop()
      }

    }
    
                                    
    fm_char <- as.character(formula)
    fm_char <- paste(fm_char[[2]],fm_char[[1]],fm_char[[3]])
    sfitnls <- nls(
                        as.formula(fm_char),
                        measured.df,
                        start=start,
                        ...
                        )

    sfitnls$efficiencies<-efficiencies
    sfitnls$sst <- sum((measured.df$Norm - mean(measured.df$Norm))^2)
    sfitnls$Nmax <- NA
    sfitnls$XMax <- NA

    
    sfitval<-structure(sfitnls,class=c("sfit",class(sfitnls)),sfit_type=stype)
    sfitval

   }

    fillx1 <- function(formula,x,stype,start=c(alpha=.1,beta=.01),...) {
       start_value <- x[1,2]
       new_x <- data.frame(N=c(1,x[,1]),X_N=c(0,x[,2]))
       f.x <- function(x) {
         new_x[1,2] <<-x
         x.sfit <- buildsfitval(formula,new_x,start=start,eff.check=F,...)
         sse(x.sfit) / sst(x.sfit)
       }
       value <- optimize(f.x,c(0,start_value))
       value$minimum

    }
#' Get sfit sfit_type attribute from an sfit object
#'
#' S3 method to access sfit object's sfit_type attribute
#'
#' @rdname sfit_type
#' @param object  a sfit object
#' @export sfit_type
#' 
 
sfit_type <- function(object) UseMethod("sfit_type")

#' @rdname sfit_type
#' @method sfit_type sfit
#' @S3method sfit_type sfit
sfit_type.sfit <- function(object) {
  return(attr(object,"sfit_type"))
}

#' Get sst slot from a scale_fit object
#'
#' S3 method to access scale_fit object's sst slot 
#'
#' @rdname sst
#' @param object  a scale object
#' @export sst
#' 
sst <- function(object) UseMethod("sst")

#' @rdname sst
#' @method sst sfit
#' @S3method sst sfit
sst.sfit<-function(object) object$sst

#' Get sse slot from a sse object
#'
#' S3 method for sfit object
#' @rdname sse
#' @param object a scale_fit object
#' @export sse
#' 
sse  <-function(object) UseMethod("sse")

#' @rdname sse
#' @method sse sfit
#' @S3method sse sfit

sse.sfit<-function(object) deviance(object)

#' Dump the efficiencies vector
#'
#' Access and print a scale object's efficiencies vector
#' @rdname effic
#' @param object a sfit
#' @param ... additional arguments
#' @export effic

effic <- function(object) UseMethod("effic")

#' @rdname effic
#' @method effic sfit
#' @S3method effic sfit
effic.sfit <- function(object) object$efficiencies


#' print sfit object
#'
#' S3method to print scale_fit object
#'
#' @param x a sfit object
#' @param ... additional arguments to pass to function
#' @method print sfit
#' @export

print.sfit<- function(x, ...)
  {
    print.data.frame(cbind(measured(x,drop.norm=FALSE),Efficiencies=effic(x)),row.names=F)
    cat("\n")
#    cat("sse:  ", sse(x),"\n")
    cat("sst:  ", sst(x),"\n")
    NextMethod()
                
  }

#' Plot sfit object 
#'
#' @param x an sfit object
#' @param lty line type see plot for all options
#' @param type what type of plot should be drawn (see plot)
#' @param lwd the line width
#' @param xlab a label for the x axis, defaults to a description of x.
#' @param ylab a label for the x axis, defaults to a description of y.
#' @param plot.legend toggle drawing the legend (default TRUE)
#' @param legend.params supply your own parameters to the legend function
#' @param plot.measured toggle adding measured points to the graph (default TRUE)
#' @param points.params supply your own parameters to the points function
#' @param plot.lin_scale toggle adding linear scaling line (default TRUE)
#' @param lin_scale.params supply your own parameters to the abline function that draws the linear scale line
#' @param plot.title toggle plot the stand title (default TRUE)
#' @param title.params supply your own parameters to the title function
#' @param ... additional arguments to pass to function
#' @method plot sfit
#' @export

plot.sfit <- function(x,
                      lty="solid",
                      type="l",
                      lwd=1,
                      ylab="Throughput X(N)",
                      xlab="Virtual users",
                      plot.legend=TRUE,
                      legend.params=NULL,
                      plot.measured=TRUE,
                      points.params=NULL,
                      plot.title=TRUE,
                      title.params=NULL,
                      plot.lin_scale=FALSE,
                      lin_scale.params=NULL,
                      ...)
  {

    if(!inherits(x,"sfit"))  stop("'plot.sfit' not called with an x of sfit class")

    sfit_type<-attr(x,"sfit_type")
    
    input <- measured(x)
    fitvec<-as.vector(fitted(x))
    plot(input$N, input$X_N[1] * fitvec,
         type=type,
         lty=lty,
         lwd=lwd,
         ylab=ylab,
         xlab=xlab,
         ...)

    if ( plot.title ) {
      title.args <- list()
      if ( is.null(title.params) ) {
        title.args$main<-paste("Fitted Scalability",sfit_type)
      } else {
        title.args <- title.params
      }
      do.call("title",title.args)
    }
    
    if ( plot.measured ) {
      points.args<-list()
      if ( is.null(points.params)) {
        points.args$pch <- 16
      } else {
        points.args <- points.params
      }
        points.args$x <- input$N
        points.args$y <- input$X_N
        do.call("points",points.args)
    }
 

    if ( plot.legend ) {
      legend.args <- list()
      if ( is.null(legend.params)) {
        legend.args$x <- "bottom"
        legend.args$legend <- format_legend(x)
        legend.args$ncol <- 2
      } else {
        legend.args <- legend.params
      }
        do.call("legend",legend.args)
    }

    
    if ( plot.lin_scale ) {
      lin_scale.args <- list()
      if ( is.null(lin_scale.params)) {
        lin_scale.args$lty <- "dashed"
      } else {
        lin_scale.args <- lin_scale.params
      }
       lin_scale.args$a <- 1
       lin_scale.args$b <- measured(x)$X_N[1]
       do.call("abline",lin_scale.args)
    }
}


#' Get Max # of users for fit attribute from an sfit object
#'
#' S3 method to access sfit object's sfit_type attribute
#'
#' @rdname Nmax
#' @param object  a sfit object
#' @export Nmax
#'

Nmax <- function(object) UseMethod("Nmax")


#' @rdname Nmax
#' @method Nmax sfit
#' @S3method Nmax sfit
Nmax.sfit <- function(object)   object$Nmax
  

#' Get Max Throughput of users for fit attribute from an sfit object
#'
#' S3 method to access sfit object's sfit_type attribute
#'
#' @rdname Xmax
#' @param object  a sfit object
#' @export Xmax
#'

Xmax <- function(object) UseMethod("Xmax")


#' @rdname Xmax
#' @method Xmax sfit
#' @S3method Xmax sfit

Xmax.sfit <- function(object) object$Xmax



 
#' @rdname Nmax
#' @method Nmax sfit
#' @S3method Nmax sfit
Nmax.sfit <- function(object)   object$Nmax


#' predict method for sfit an object 
#'
#' A predict generic specialized for sfit objects 
#'
#' @param object  a sfit object
#' @method predict sfit
#' @export
predict.sfit <- function(object,...){
  x1 <- measured(object)$X_N[[1]]
 return (x1 * stats:::predict.nls(object,...))
  
}

#' xroof method 
#'
#' 
#' @rdname xroof
#' @param object sfit object
#' @export xroof

xroof <- function(object,...)  UseMethod("xroof")

