#' select PCs
#'
#' @param data list object from calc_PCs
#' @param type eigenvalue1, firstm, cum.perc, mean, and all
#' @param m
#' @param cum.perc
#'
#' @return
#'
#' @examples
select_PCs <- function(data = PCA, type = select_pc, cum_perc = NULL, m = NULL ) {
  if(!type %in% c("eigenvalue1", "cum.perc", "mean")) {
    stop("select_pc must be one of: eigenvalue1, cum.perc, or mean")
  }
  #extract PC values
  PCs <- as.data.frame(data$PCA$x)
  PC_df <- cbind(year = data$chrons_period$year, PCs)

  eigval <- as.data.frame(factoextra::get_eig(data$PCA))
  PC <- c(1:ncol(data$PCA$x))
  PC <- paste0("PC", PC)

  eigval <- cbind(eigval, PC)

  eigval_small <- switch(type,
                         eigenvalue1 = dplyr::filter(eigval, eigval$eigenvalue >= 0.999),
                         cum.perc = dplyr::filter(eigval, eigval$cumulative.variance.percent >= cum.perc),
                         mean = dplyr::filter(eigval, eigval$eigenvalue >= mean(eigval$eigenvalue)))

  list <- list(eigval_small = eigval_small, PC_vals = PC_df)
}
