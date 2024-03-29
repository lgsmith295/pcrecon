% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/perf_stats.R
\name{perf_stats}
\alias{perf_stats}
\title{Performance statistics of calibration and validation date from reconstruction}
\usage{
perf_stats(
  valid_est,
  calib_est,
  observed,
  valid_yrs = valid,
  calib_yrs = calib,
  mod_id = "m"
)
}
\arguments{
\item{valid_est}{dataframe of years and fit estimates from validation period}

\item{observed}{dataframe of years and observed values}

\item{valid_yrs}{integer vector of years for validation}

\item{calib_yrs}{integer vector of years used in calibration}

\item{mod_id}{character string identifying the model}
}
\value{
R2, Pearson_R2, RE, CE for validation data
}
\description{
Performance statistics of calibration and validation date from reconstruction
}
\details{
coming soon
}
\examples{
set.seed(92831)
df <- data.frame(year = 1:20, values = rnorm(20), fit = rnorm(20))
perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)

df <- data.frame(year = 1:20, values = rnorm(20))
df$fit <- df$values * 0.75 + rnorm(20, 0, 0.5)
perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)

df$fit <- df$values * 0.75 # Rsq should = 1, RE and CE should be very high
perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)

df$fit <- df$values * 0.75 + 2 # Rsq should = 1 but RE and CE very low
perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)

df[19, "values"] <- NA_real_
perf_stats(valid_est = df[1:10, c("year", "fit")], calib_est = df[11:20, c("year", "fit")], observed = df[1:20 , c("year", "values")], valid_yrs = 1:10, calib_yrs = 11:20)
}
