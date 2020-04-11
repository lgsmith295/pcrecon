#' Performance statistics of calibration and validation date from reconstruction
#'
# Need to fix these and the associated tests to indicate if dataframes and what the column names should be
#' @param valid_est dataframe of years and fit estimates from validation period
#' @param observed dataframe of years and observed values
#' @param valid_yrs integer vector of years for validation
#' @param cal_yrs integer vector of years used in calibration
#' @param mod_id character string identifying the model
#'
#' @return R2, Pearson_R2, RE, CE for validation data
#' @details coming soon
#' @export
#'
#' @examples
#' \dontrun{
#' "coming soon"
#' }
perf_stats <- function(valid_est, observed, valid_yrs, calib_yrs, mod_id = "m") {
  if(!all(colnames(observed) == c("year", "means"))) {
    stop("observed must be a dataframe that includes the columns year and means")
  }
  x_valid <- dplyr::filter(observed, observed$year %in% valid_yrs)
  x_calib <- dplyr::filter(observed, observed$year %in% calib_yrs)
  R2 <- 1 - ((sum((x_valid$means - valid_est$fit)^2)) / (sum((x_valid$means - mean(x_valid$means))^2)))
  Pearson_R2 <- (sum((x_valid$means - mean(x_valid$means)) * (valid_est$fit - mean(valid_est$fit))))^2 / (sum((x_valid$means - mean(x_valid$means))^2) * sum((valid_est$fit - mean(valid_est$fit))^2))
  RE <- 1 - sum((x_valid$means - valid_est$fit) ^ 2) / sum((x_valid$means - mean(x_calib$means, na.rm = TRUE))^2)
  CE <- 1 - sum((x_valid$means - valid_est$fit) ^ 2) / sum((x_valid$means - mean(x_valid$means)) ^ 2)
  df <- data.frame(model = mod_id, R2, Pearson_R2, RE, CE, stringsAsFactors = FALSE)
  return(df)
}