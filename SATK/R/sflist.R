#' sflist_list constructors
#'
#' Details of sflist
#' 
#' @title sflist: sflist_list constructor
#' @rdname sflist
#' @export sflist
sflist <- function(x,...) {
  UseMethod("sflist")
}

#' Create a sflit_list object from a dataframe or matrix object
#'
#' Extends the R \code{list} object to specializeit to groups of sfit objects
#' that have all be fitted from the same data source
#'
#' @param x a data.frame or matrix object containing data for model.
#' A data.frame should the data in columns named \code{N},for "scaled objects"
#' (e.g. users, cpus), and \code{X_N}, for measured throughput. The default for
#' a matrix object is to put scaled objects in the matrix's column 1
#' (i.e. \code{x[,1]}) and throughput in column 2 \code{x[,2]}
#' @param fits a list that specifics which sfit objects to include in the list
#' @param ... additional arguments to pass to each of the sfit objects
#' @param starts a named list of start parameter for the nls in the sfit object
#' @param algorithms a named list of alogirthms to use in the nls in the sfit object
#' @param fits.params a named list of named lists of fit parameters
#' @param fit.names names to give the fit list
#' @return sflist object
#' @rdname sflist
#' @method sflist data.frame
#' @S3method sflist data.frame
#' @examples
#' data(USLcalc)
#' sl1<-sflist(USLcalc)
#'
#' data(SSUSLcalc)
#' sl2<-sflist(SSUSLcalc,data.cols=c("V1","V2"),starts=c(usl=c(alpha=.1,beta=.001)))

sflist.data.frame  <- function(x,fits=.fits(),...,starts,algorithms,fits.params,fit.names=NULL)
  {
#Robbed this idea from write.csv
    Call<-match.call(expand.dots=TRUE)
    fit_names <- eval(Call$fit.names)
    Call$fit.names <- NULL
    
    
    sfitlist<-vector("list",length(fits))
    actualfits<-vector()


    for( i in 1:length(fits) )
      {
         fit_call<-Call
         fit_call$fits <- NULL
         if ( ! is.null(fit_call$starts) )
           {
             starts <- fit_call$starts
             fit_call$starts <- NULL
             start <- starts[[fits[[i]]]]
             if ( ! is.null(start)) {
               fit_call$start <- start
             }
           }
         if ( ! is.null(fit_call$algorithms) )
           {
             algorithms <- fit_call$algorithms
             fit_call$algorithms <- NULL
             algorithm <- algorithms[[fits[[i]]]]
             if ( ! is.null(algorithm)) {
               fit_call$algorithm <- algorithm
             }
           }

         if ( ! is.null(fit_call$fits.params))
             {
               call.fits.params <- fit_call$fits.params
               fit_call$fit.params <- NULL
               fit.params <- call.fits.params[[fits[[i]]]]
               if ( ! is.null(fit.params)) {
                 param_list<-as.list(fit.params)[-1]
                 for ( j in 1:length(param_list))
                   {
                     param <- names(param_list)[[j]]
                     value <- param_list[[j]]
                     fit_call[[param]]<-value
                   }
               }

             }
         fit_call[[1L]] = as.name(fits[[i]])
         fit_return <- eval.parent(fit_call)
         sfitlist[[i]] <- fit_return
         if ( ! is.null(fit_return)) {
           actualfits<-c(actualfits,fits[[i]])
         }
       }
      
#    names(sfitlist)<-fits[! is.null(sfitlist) ]

     if ( is.null(fit_names) ) {
       fit_names <-actualfits
     }

     if ( length(unique(fit_names)) != length(sfitlist) ) {
       stop("names of fits in sflist  must be unique and equal to the number of fits")
     }
    
    names(sfitlist) <- fit_names
    sfitlistval<-structure(sfitlist,class=c("sflist",class(sfitlist)))
    sfitlistval
  }

#' @rdname sflist
#' @method sflist sfit
#' @S3method sflist sfit
sflist.sfit <- function(...,fit.names=NULL)
  {
    #robbed from Table.R. has to be an easier way to do this
    list.names <- function(...,deparse.level=1) {
      l <- as.list(substitute(list(...)))[-1L]
      nm <- names(l)
      fixup <- if (is.null(nm)) seq_along(l) else nm == ""
      dep <- vapply(l[fixup], function(x)
                    switch(deparse.level + 1,
                           "", ## 0
                           if (is.symbol(x)) as.character(x) else "", ## 1
                           deparse(x, nlines=1)[1L] ## 2
                           ),
                    "")
      if (is.null(nm))
        dep
      else {
        nm[fixup] <- dep
        nm
      }
    }

    sf_list <-list(...)
#    rest <- list(...)
#    sf_list<-vector("list",length(rest) + 1)
#    sf_list[[1]]<-x
#    ffit_name <- substitute(x)
#    sf_list[2:length(sf_list)] <- rest
#    lnames <- list.names(...)
#    fit_names <- c(ffit_name,lnames)
    
    fit_names <- list.names(...)

    if ( ! is.null(fit.names) ) {
      fit_names <- fit.names
    }

    if ( length(unique(fit_names)) != length(sf_list) ) {
      stop("fit.names must be unique and equal to the number of fits")
    }

    names(sf_list) <- fit_names
        
    for ( cnt in 1:length(sf_list) ) {
       if(! inherits(sf_list[[cnt]],"sfit")) {
         stop("'sflist' must be called with sfit object")
       }
       
       if ( cnt == 1 ) {
         model.df<-measured(sf_list[[1]])
         next
       }
       
       measured.df <- measured(sf_list[[cnt]])
       if( ! (identical(model.df,measured.df))) {
         stop("All sfit objects in sflist must be fit from the same data")
      }
    }
    
    sfitlistval<-structure(sf_list,class=c("sflist",class(sf_list)))
    sfitlistval
  }


#' Create data frame from sflit_list object 
#'
#' builds a data frame containing fitted data from each of the sfit objects
#' in the sfit_list plus the original measured data. 
#' 
#'
#' @param object a sfit_list
#' @param keep.measured a logical indicate to include measured
#' @return data.frame
#' @export
#' @examples
#' data(USLcalc)
#' sl1<-sflist(USLcalc)
#' build_fits_frame(sl1)
#



build_fits_frame<-function(object,keep.measured=TRUE)
  {
   return_frame<-data.frame()
   flistname = deparse(substitute(object))
   for ( i in 1:length(object) )
     {
       N <- measured(object[[i]])$N
       X_N <- measured(object[[i]])$X_N
       measured_X_N<-measured(object[[1]])$X_N
       tput<-measured_X_N[[1]]*as.vector(fitted(object[[i]]))
#       return_frame<-rbind(return_frame,data.frame(N=N,X_N=tput,type=names(object)[[i]],slname=flistname))
       return_frame<-rbind(return_frame,data.frame(N=N,X_N=tput,type=sfit_type(object[[i]]),slname=flistname))
     }

   if ( keep.measured ) {
     measured_N<-measured(object[[1]])$N
     measured_X_N<-measured(object[[1]])$X_N
     return_frame<-rbind(return_frame,data.frame(N=measured_N,X_N=measured_X_N,type="measured",slname=flistname))
   }
   return_frame
 }

#' Draw a graph of all the objects plus measured data in sfit_list
#'
#' Draws a graph of all the objects plus measured data in x
#' in the sfit_list plus the original measured data. 
#' 
#'
#' @param x a sfit_list
#' @param xlab a label for the x-axis
#' @param ylab a label for the y-axis
#' @param plot.lin_scale add a line showing linear scaling
#' @param ... additional arguments to pass to each of the sfit objects
#' @return plot
#' @method plot sflist
#' @S3method plot sflist
#' @import lattice
#' @export
#' @examples
#' library(lattice)
#' data(USLcalc)
#' sl1<-sflist(USLcalc)
#' plot(sl1)
#

plot.sflist <- function(x,xlab="N",ylab="Throughput X(N)",...,plot.lin_scale=F)
  {
    fits_data.df <- build_fits_frame(x)
    fits <- levels(fits_data.df$type)
    fits <- fits[fits != "measured"]
    lattypes <- rep("l",length(fits))
    latltys  <- rep(1,length(fits))
    lattypes <- c(lattypes,"p")
    latcols <- c("blue","red","green","purple","orange")[1:length(fits)]
    latcols <- c(latcols,"black")

    if ( plot.lin_scale ) {

      sflist.panel <- function(...) {
        panel.xyplot(...)
        panel.abline(1,measured(x)$X_N[1],lty="dashed")
        
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
                                       text=list(lab=c(fits,"measured")),
                                       lines=list(
                                         pch=c(16,NA),
                                         col=c(latcols,"black"),
                                         type=c(lattypes,"p"),
                                         cex=.8),
                                       transparent=T,border=F,rep=F)

    }

    
    xyplot(X_N~N,
           fits_data.df,
           groups=fits_data.df$type,
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


#' @S3method [ sflist
`[.sflist` <- function(x,...,drop=TRUE)
  {

    cl <- oldClass(x)
    val<-NextMethod("[")
    class(val)<-cl
    val
  }


#' Build a sflist from sfit objects
#'
#' S3 method to build an sflist from sfit objects
#'
#' @rdname as.sflist
#' @param ...  sfit objects
#' @export as.sflist
#' @import compare
#' @return sflist

