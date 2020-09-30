#' Autoregressive Prewhitening
#'
#' @param x univariate or multivariate time series
#' @param ...
#' @param return character string, determines whether to return model estimates ("est"), the ar model ("model"), or both ("both")
#'
#' @details Function modified from ar.prewhiten in the dplR package by Andy Bunn.
#'
#' @return  model estimates, ar model object, or a list containing both
#'
#' @examples
#' # simulate autocorrelated timeseries
#' ar.sim<-arima.sim(model=list(ar=c(.9,-.2)),n=100)
#'
#' #prewhiten
#' white <- ar_prewhiten(ar.sim, return = "both" )
#'
#' plot(ar.sim)
#' lines(white[[1]], col = "red")
#'
ar_prewhiten <- function (x, return = "both", ...)
{
  idx.goody <- !is.na(x)
  ar <- ar(x[idx.goody])
  x[idx.goody] <- ar$resid + ar$x.mean

  if(return == "est"){
    ret <- x
  }
  if(return == "model"){
    ret <- ar
  }
  if(return == "both"){
    ret <- list(x, ar)
  }
  return (ret)
}

