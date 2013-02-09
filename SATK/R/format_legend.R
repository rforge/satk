#' Format a legend for a specific object
#'
#' S3 method format a legend in plot.sfit based on the sfit_type
#'
#' @rdname format_legend
#' @param object  a sfit object
#' @export format_legend
#'
format_legend <- function(object) 
{
  UseMethod("format_legend")
}


#' @rdname format_legend
#' @method format_legend gustafson
#' @S3method format_legend gustafson

format_legend.gustafson <- function(object) {
  legend_text=sprintf("expression(alpha == %.4f, R^2 == %.4f,Nmax==%.2f,Xmax==%.2f,Xroof==%.2f,Z(sec)==%.2f)",
                      coef(object)['alpha'],
                      1-sse(object)/sst(object),
                      Nmax(object),
                      Xmax(object),
                      measured(object)$X_N[1]/coef(object)['alpha'],
                      0.0)
  
  eval(parse(text=legend_text))
  
}

#' @rdname format_legend
#' @method format_legend amdahl
#' @S3method format_legend amdahl
format_legend.amdahl <- function(object)
{
  
  legend_text <- sprintf("expression(alpha == %.4f, R^2 == %.4f,Nmax==%.2f,Xmax==%.2f, Z(sec)==%.2f)",
                         coef(object)['alpha'],
                         1-sse(object)/sst(object),
                         Nmax(object),
                         Xmax(object),
                         0.0)
  
  eval(parse(text=legend_text))
  
  
}



#' @rdname format_legend
#' @method format_legend sfit
#' @S3method format_legend sfit

format_legend.sfit <- function(object)
{
  
  legend_text=sprintf("expression(alpha == %.4f, beta == %.6f, R^2 == %.4f, Nmax==%.2f, Xmax==%.2f,Xroof==%.2f,Z(sec)==%.2f)",
                      coef(object)['alpha'],
                      coef(object)['beta'],
                      1-sse(object)/sst(object),
                      Nmax(object),
                      Xmax(object),
                      measured(object)$X_N[1]/coef(object)['alpha'],
                      0.0
  )
  
  eval(parse(text=legend_text))
  
}