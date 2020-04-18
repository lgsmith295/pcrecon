#' Autoregressive Prewhitening
#'
#' @param x
#' @param model
#' @param ...
#'
#' @return
#'
#' @examples
ar_prewhiten <- function (x, return = "both", ...)
{
  y <- x
  idx.goody <- !is.na(y)
  ar <- ar(y[idx.goody])
  y[idx.goody] <- ar$resid + ar$x.mean
  if(return == "resid"){
    ret <- y
  }
  if(return == "model"){
    ret <- ar
  }
  if(return == "both"){
    ret <- list(y, ar)
  }
  return (ret)
}

