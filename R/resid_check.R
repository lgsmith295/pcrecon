#' PCReg_recon object residual check
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
resid_check <- function(data) {
  if(class(data) != "PCReg_recon") stop("Data must an object of class PCReg_recon. See documentation for details")

  fits <- list()

  for(i in 1:length(data$LM)) {
    fits[[i]] <- tibble::tibble(nest = i, fit = fitted(data$LM[[i]]), res = resid(data$LM[[i]]))
  }
  df_fits <- dplyr::bind_rows(fits)


  print(ggplot2::ggplot(df_fits, aes(fit+res, fit, color = as.factor(nest)), alpha = 0.1) + geom_point() + geom_abline(intercept = 0, slope = 1) + theme_bw()) # + geom_smooth(alpha = 0.01)

  print(ggplot2::ggplot(df_fits, aes(fit, res)) + geom_point() + geom_hline(yintercept = 0) + facet_wrap(~nest) + theme_bw())

  # ggplot2::ggplot(df_fits, aes(res, fit, color = nest)) + geom_point() + geom_hline(yintercept = 0)


}



