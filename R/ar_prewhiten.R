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
  ar1 <- ar(y[idx.goody])
  y[idx.goody] <- ar1$resid
  if (isTRUE(model)) {
    structure(y, model = ar1)
  } else {
    y
  }
}
