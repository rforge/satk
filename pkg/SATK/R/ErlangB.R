# cite Neil's paper
#ErlangB <- function(A,P)
#  {
#
#    erlangB <- A/(1+A)
#    for( k in (2:P) ){
#      erlangB <- erlangB * (A/(A*erlangB + k))
#
#    }
#        erlangB
#
#  }

#
#' @export
ErlangB <- function(A,P)
  {

    InvB <- 1.0
    for (j in (1:P)){
      InvB <- 1.0 + j / A * InvB
    }
    erlangB <- 1.0 / InvB
    erlangB
  }
