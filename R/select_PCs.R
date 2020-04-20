#' select PCs
#'
#' @param data list object from calc_PCs
#' @param type eigval1, firstm, cum.perc, mean, and all
#' @param cum.perc
#' @param m
#'
#' @return
#'
#' @examples
select_PCs <- function(data = PCA, type = select.pc, cum.perc = NULL, m = NULL ) {
  #extract PC values
  PCs <- as.data.frame(data$PCA$x)
  PC_df <- cbind(year = data$chrons_period$year, PCs)

  eigval <- as.data.frame(factoextra::get_eig(data$PCA))
  PC <- c(1:ncol(data$PCA$x))
  PC <- paste0("PC", PC)

  eigval <- cbind(eigval, PC)

  eigval_small <- switch(type,
                         eigval1 = dplyr::filter(eigval, eigval$eigenvalue >= 0.999),
                         firstm = eigval[complete.cases(eigval[1:m, ]), ],
                         cum.perc = dplyr::filter(eigval, eigval$cumulative.variance.percent >= cum.perc),
                         mean = dplyr::filter(eigval, eigval$eigenvalue >= mean(eigval$eigenvalue)),
                         all = eigval)

  list <- list(eigval_small = eigval_small, PC_vals = PC_df)
}
