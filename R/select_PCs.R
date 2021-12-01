#' select PCs
#'
#' @param data list object from calc_PCs
#' @param type how eigenvectors are selected for inclusion in model, should be one of: "eigenvalue1", "firstn", "percent", "mean"
#' @param n number of eigenvectors to retain
#' @param percent value of cumulative percent variance explained
#'
#' @return
#'
#' @examples
select_PCs <- function(data = PCA, type = eigenvalue1, percent = NULL, n = NULL ) {
  if(!select_pc %in% c("eigenvalue1", "percent", "firstn", "mean")) {
    stop("select_pc must be one of: eigenvalue1, percent, firstn, or mean")
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
                         percent = dplyr::filter(eigval,eigval$cumulative.variance.percent >= percent),
                         mean = dplyr::filter(eigval, eigval$eigenvalue >= mean(eigval$eigenvalue)),
                         firstn = dplyr::slice_head(eigval, n = n ))

  selected_pcs <- list(eigval_small = eigval_small, PC_vals = PC_df)
}
