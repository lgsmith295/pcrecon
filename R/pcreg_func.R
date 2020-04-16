#' PCreg
#'
#' @param clim
#' @param crns
#' @param nests
#' @param calib
#' @param valid
#' @param pc.calc
#' @param scale.var
#' @param weight
#'
#' @return
#' @export
#'
#' @examples
pcreg <- function(PCR_crns, clim, calib, valid, pc.calc = calib, select.pc, scale.var, weight = NULL){

  if (class(PCR_crns) != "PCR_crns") {
  stop("PCR_crns must be of class PCR_crns, as returned by function filter_cor()")
  }
periods_df <- PCR_crns$nests
PCA_chrons <- PCR_crns$select_crns
full <- min(c(valid, calib)): max(c(valid,calib))

number_nests <- nrow(periods_df)


for (i in 1 : number_nests) {
  nest_yrs <- c(periods_df$startYR[i] : min(periods_df$endYR))
## make check for start year < end year
  PCA <- calc_PCs(periods_df, PCA_chrons, pc.calc, nest_yrs)

  select_PC <- select_PCs(data = PCA)

  df <- mod_df(data = select_PC$PC_vals, eig = select_PC$eigval_small, nest_yrs, calib)


  full_mod <- lm(formula = clim~., data = df)


  step_mod <- stepAICc(full_mod, trace = TRUE, k = 3)

  model_stats <- sum_mod(data = step_mod, i, nest_yrs)

  if(i == 1) {
    model_table <- model_stats
  } else {
    model_table <- dplyr::bind_rows(model_table, model_stats)

  }

  #### PCs for rest of nest years
  PCA_predict <- as.data.frame(predict(PCA$PCA , PCA$nest))

  #### reconstruction
  predict_nest <- predict(step_mod, PCA_predict, se.fit=TRUE, interval="confidence", level=0.95)
  recon_nest <- as.data.frame(cbind(year = nest_yrs, predict_nest[["fit"]]+ mean(clim$mean)))

  #### validation
  valid_est <- dplyr::filter(recon_nest, recon_nest$year %in% valid) %>%
    dplyr::select(fit)

  clim_full <- as.data.frame(dplyr::filter(clim, clim$year %in% nest_yrs))

  observed <- dplyr::filter(clim_full, clim_full$year %in% full)

  obs_cal <- dplyr::filter(clim_full, clim_full$year %in% calib)

  stats <- perf_stats(valid_est, observed, valid_yrs = valid, calib_yrs = calib, mod_id = i)

  if(i == 1) {
    stats_table <- stats
  } else {
    stats_table <- dplyr::bind_rows(stats_table, stats)

  }

  #### filter recon to new nest years

  if(i == 1) {
    recon <- recon_nest
  } else {
    new_years <- periods_df[i, "startYR"]:(periods_df[i-1, "startYR"] - 1)
    recon_nest <- recon_nest %>%
      dplyr::filter(year %in% new_years)
  }

  #### scale to variance of calibration
  mn <- mean(recon_nest$fit)

  sd_recon <- sd(recon_nest$fit)
  sd_clim <- sd(clim$mean)
  recon_nest$fit <- mn + (recon_nest$fit - mn) * sd_clim/sd_recon

  #### Add reconstruction to dataframe

  recon <- dplyr::arrange(dplyr::bind_rows(recon, recon_nest), year)


  #### drop out shortest chronology

  PCA_chrons <- PCA_chrons %>%
    dplyr::select(-!!periods_df$ID[i])

}

return(recon)
}



#' calculate PCs
#'
#' @param periods_df dataframe of nests
#' @param PCA_chrons dataframe containing chronologies
#' @param period period of time over which to calculate PCs, calibration or nest_yrs
#'
#' @return
#'
#' @examples
calc_PCs <- function(periods_df, PCA_chrons, pc.calc, nest_yrs) {
nest <- PCA_chrons %>%
  dplyr::filter(PCA_chrons$year %in% nest_yrs) %>%
  dplyr::select(-year)
chrons_period <- PCA_chrons %>%
  dplyr::filter(PCA_chrons$year %in% pc.calc)

PCA_chrons_calib <- chrons_period %>%
  dplyr::select(-year)

PCA <- prcomp(PCA_chrons_calib, scale = TRUE)

list(PCA = PCA, chrons_period = chrons_period, nest = nest)
}

#' select PCs
#'
#' @param data list object from calc_PCs
#'
#' @return
#'
#' @examples
select_PCs <- function(data = PCA, type = NULL) {
#extract PC values
PC_values <- as.data.frame(data$PCA$x)
PC_vals <- cbind(year = data$chrons_period$year, PC_values)

# get eigenvalues and select those with cumulative variance explained <80%
eigval <- as.data.frame(factoextra::get_eig(data$PCA))
PC <- c(1:ncol(data$PCA$x))
PC <- paste0("PC", PC)

eigval <- cbind(eigval, PC)
eigval_small <- dplyr::filter(eigval, eigval$eigenvalue >= 0.9999)

list(eigval_small = eigval_small, PC_vals = PC_vals)
}


#' Create dataframe containing clim and select PCs for linear model
#'
#' @param data PCA values
#' @param eig selected eigenvalues and PCs
#'
#' @return
#'
#' @examples
mod_df <- function(data = PC_vals, eig = eigval_small, nest_yrs, calib) {
#### linear model and Step AICc
clim_full <- as.data.frame(dplyr::filter(clim, clim$year %in% nest_yrs))
clim <- dplyr::filter(clim_full, clim_full$year %in% calib)

PC_common <- dplyr::filter(data, data$year %in% clim$year)

# create DF to set up linear model

MOD_PCs <- as.character(eig$PC)

PC_common_small <- dplyr:: select(PC_common, MOD_PCs)

df <- cbind(clim = clim[ ,2] - mean(clim[ ,2]), PC_common_small)

}

#' model stats for table
#'
#' @param data AIC selected linear model object
#'
#' @return
#'
#' @examples
sum_mod <- function(data = step_mod, i, nest_yrs){

model_summary <- summary(data)
formula <- as.character(model_summary$terms)
sigma <- model_summary$sigma
r.sq <- model_summary$r.squared
adj.r.sq <- model_summary$adj.r.squared
nest_number <- i

model_table <- data.frame(model = formula[3], sigma, r.sq, adj.r.sq, stringsAsFactors = FALSE, nest_lwr = min(nest_yrs))
}
