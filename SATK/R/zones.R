#' Constructor for zones object
#'
#' method to build zone object
#'
#' @rdname zones
#' @param object  a sfit object
#' @export 
#'  
zones <- function(x,...)
  {
    zonestmp <- list()
    zonestmp$usl <- usl(x,...)
    if ( ! is.null(zonestmp$usl)) {
      dp.usl <- measured(zonestmp$usl)
      max.ind <- which.max(dp.usl$X_N)-1
      zonestmp$amdahl <- amdahl(dp.usl[1:max.ind,],data.cols=c("N","X_N"))
    } else {
      zonestmp$amdahl <- amdahl(x,...)
    }
    
    amdahl_alfa <- coef(zonestmp$amdahl)['alpha']
    zonestmp$AsyncA <- as.numeric((1-amdahl_alfa)/amdahl_alfa)
    zonesval<-structure(zonestmp,class=c("zones",class(zonestmp)))
    zonesval
  }
#' Convert zones object to data.frame
#'
#' 
#' @rdname zones_to_frame
#' @param object a zones
#' @export zones_to_frame

zones_to_frame <- function(object,...)  UseMethod("zones_to_frame")

#' @rdname zones_to_frame
#' @method zones_to_frame
#' @S3method zones_to_frame zones
zones_to_frame.zones<- function(object,predict_vector=NULL) {
  outframe <- data.frame(N=vector(),X_N=vector(),label=vector())
  predict_frame <- NULL
  if (!is.null(predict_vector)) predict_frame<-data.frame(N=predict_vector)
  
  if (!is.null(object$usl)) {
    measuredframe <- measured(object$usl)
    if(is.null(predict_frame)) {
      predict_frame<-measuredframe["N"]
    } 
    uslframe <- data.frame(N=measuredframe$N,X_N=predict(object$usl,predict_frame))
    uslframe <- transform(uslframe,label="USL")
    outframe <- rbind(outframe,uslframe)
  } else {
    measuredframe <- measured(object$amdahl)
    if(is.null(predict_frame)) {
      predict_frame<-measuredframe["N"]
    }
  }
  measuredframe <- transform(measuredframe,label="measured")
  outframe <- rbind(outframe,measuredframe)
  amdahlframe <- data.frame(N=predict_frame$N,X_N=predict(object$amdahl,predict_frame))
  amdahlframe <- transform(amdahlframe,label="Sync")
  outframe <- rbind(outframe,amdahlframe)
  amdsigma <- as.numeric(coef(object$amdahl)['alpha'])
  erlangvec<-sapply(predict_frame$N,function(x) ErlangB(object$AsyncA,x))
  erlangvec <- (1/amdsigma)*(1 - erlangvec)
  erlangframe <- data.frame(N=predict_frame$N,X_N=measuredframe$X_N[[1]]*erlangvec)
  erlangframe <- transform(erlangframe,label="ASync")
  rbind(outframe,erlangframe)
}

#' plot method for zones object
#'
#' method to plot zones object
#'
#' @method plot zones
#' @export
#' 
plot.zones <- function(x,xlab="N",ylab="Throughput X(N)",...,plot.lin_scale=F,predict_vector=NULL)
  {
    fits_data.df <- zones_to_frame(x,predict_vector=predict_vector)
    fits <- levels(fits_data.df$label)
    fits <- fits[fits != "measured"]
    lattypes <- rep("l",length(fits))
    latltys  <- rep(1,length(fits))
    lattypes <- c("p",lattypes)
    if (length(fits) == 3) {
      latcols <- c("red","blue","green")
    } else {
      latcols <- c("blue","green")
    }
    latcols <- c("black",latcols)

    if ( plot.lin_scale ) {

      sflist.panel <- function(...) {
        panel.xyplot(...)
        panel.abline(1,measured(x$usl)$X_N[1],lty="dashed")
        
      }
                 key_list <- list(
                                  x=.6,y=.2,
                                  text=list(lab=c("linear",fits,"measured")),
                                  lines=list(
                                    pch=c(NA,16),
                                    col=c("black",latcols,"black"),
                                    lty =c(2,latltys,2),
                                    type=c("l",lattypes,"p"),
                                    cex=.8),
                                  transparent=T,border=F,rep=F)


    } else {
      sflist.panel <- function(...) {
        panel.xyplot(...)
      }
                      key_list <- list(
                                       x=.6,y=.2,
                                       text=list(lab=c("measured",fits)),
                                       lines=list(
                                         pch=c(16,NA),
                                         col=c("black",latcols),
                                         type=c("p",lattypes),
                                         cex=.8),
                                       transparent=T,border=F,rep=F)

    }

    
    xyplot(X_N~N,
           fits_data.df,
           groups=fits_data.df$label,
           type=lattypes,
           col=latcols,
           pch=16,
           distribute.type=TRUE,
           panel=sflist.panel,
           key=key_list,
           xlab=xlab,
           ylab=ylab
           )

  }

#' xroof method for an amdahl object 
#'
#' A xroof specialized for amdahl objects 
#'
#' @rdname xroof.zones
#' @param object  an amdhal object
#' @method xroof zones
#' @S3method xroof zones
#'
xroof.zones <- function(object,...)
{
  
  return(as.numeric(xroof(object$amdahl))) 
}
