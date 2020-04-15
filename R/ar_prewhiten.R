#' Autoregressive Prewhitening
#'
#' @param x
#' @param model
#' @param ...
#'
#' @return
#'
#' @examples
ar_prewhiten <- function (x, model = FALSE, ...)
{
  y <- x
  idx.goody <- !is.na(y)
  ar <- ar(y[idx.goody])
  y[idx.goody] <- ar$resid
  if (model == TRUE){
    ret <- list(y, ar)
  } else {
  ret <- y
  }
  return (ret)
}

