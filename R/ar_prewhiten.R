#' Autoregressive Prewhitening
#'
#' @param x univariate or multivariate time series
#' @param ...
#' @param return character string, determines whether to return model residuals ("resid"), the ar model ("model"), or both ("both")
#'
#' @details Function modified from ar.prewhiten in the dplR package
#'
#' @return
#'
#' @examples
ar_prewhiten <- function (x, return = "both", ...)
{
  idx.goody <- !is.na(x)
  ar <- ar(x[idx.goody])
  x[idx.goody] <- ar$resid + ar$x.mean

  if(return == "resid"){
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

