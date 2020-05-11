#' Performance statistics of calibration and validation date from reconstruction
#'
# Need to fix these and the associated tests to indicate if dataframes and what the column names should be
#' @param valid_est dataframe of years and fit estimates from validation period
#' @param observed dataframe of years and observed values
#' @param valid_yrs integer vector of years for validation
#' @param calib_yrs integer vector of years used in calibration
#' @param mod_id character string identifying the model
#'
#' @return R2, Pearson_R2, RE, CE for validation data
#' @details coming soon
#' @export
#'
#' @examples
#' set.seed(92831)
#' df <- data.frame(year = 1:20, values = rnorm(20), fit = rnorm(20))
#' perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)
#'
#' df <- data.frame(year = 1:20, values = rnorm(20))
#' df$fit <- df$values * 0.75 + rnorm(20, 0, 0.5)
#' perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)
#'
#' df$fit <- df$values * 0.75 # Rsq should = 1, RE and CE should be very high
#' perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)
#'
#' df$fit <- df$values * 0.75 + 2 # Rsq should = 1 but RE and CE very low
#' perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)
#'
#' df[19, "values"] <- NA_real_
#' perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)
perf_stats <- function(valid_est, calib_est, observed, valid_yrs = valid, calib_yrs = calib, mod_id = "m") {
  if(!all(colnames(observed) == c("year", "values"))) {
    stop("observed must be a dataframe that includes the columns year and values")
  }
  # add warning if removing values

  x_valid <- dplyr::filter(observed, observed$year %in% valid_yrs)
  x_calib <- dplyr::filter(observed, observed$year %in% calib_yrs)

  # Validation
  R2 <- cor(valid_est$fit, x_valid$values, use = "pairwise.complete.obs")^2
  # R2 <- (sum((valid_est$fit - mean(x_valid$values, na.rm = TRUE))^2, na.rm = TRUE)) / (sum((x_valid$values - mean(x_valid$values, na.rm = TRUE))^2, na.rm = TRUE))

  # Pearson_R2 <- (sum((x_valid$values - mean(x_valid$values)) * (valid_est$fit - mean(valid_est$fit))))^2 / (sum((x_valid$values - mean(x_valid$values))^2) * sum((valid_est$fit - mean(valid_est$fit))^2))

  RE <- 1 - sum((x_valid$values - valid_est$fit) ^ 2, na.rm = TRUE) / sum((x_valid$values - mean(x_calib$values, na.rm = TRUE))^2, na.rm = TRUE)

  CE <- 1 - sum((x_valid$values - valid_est$fit) ^ 2, na.rm = TRUE) / sum((x_valid$values - mean(x_valid$values, na.rm = TRUE)) ^ 2, na.rm = TRUE)

  df_valid <- data.frame(model = mod_id, R2, RE, CE, stringsAsFactors = FALSE)

  # Calib
  R2 <- cor(calib_est$fit, x_calib$values, use = "pairwise.complete.obs")^2
  # R2 <- (sum((x_calib$fit - mean(x_calib$values))^2)) / (sum((x_calib$values - mean(x_calib$values))^2))
  #
  # Pearson_R2 <- (sum((x_calib$values - mean(x_calib$values)) * (calib_est$fit - mean(calib_est$fit))))^2 / (sum((x_calib$values - mean(x_calib$values))^2) * sum((calib_est$fit - mean(calib_est$fit))^2))

  RE <- 1 - sum((x_calib$values - calib_est$fit) ^ 2, na.rm = TRUE) / sum((x_calib$values - mean(x_valid$values, na.rm = TRUE))^2, na.rm = TRUE)

  CE <- 1 - sum((x_calib$values - calib_est$fit) ^ 2, na.rm = TRUE) / sum((x_calib$values - mean(x_calib$values, na.rm = TRUE)) ^ 2, na.rm = TRUE)

  df_calib <- data.frame(model = mod_id, R2, RE, CE, stringsAsFactors = FALSE)


  df <- list(validation_stats = df_valid, calibration_stats = df_calib)
  return(df)
}


