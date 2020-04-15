#' Title
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
pcreg <- function(clim, crns, nests, calib, valid, pc.calc, scale.var, weight){

number_nests <- nrow(periods_df)

for (i in 1 : number_nests) {



  #### new nest years
  nest_yrs <- c(periods_df$startYR[i] : min(periods_df$endYR))

  #### calculate PCs for calibration period

  nest <- PCA_chrons %>%
    dplyr::filter(PCA_chrons$year %in% nest_yrs) %>%
    dplyr::select(-year)


  #Principal component anaylsis for calibration period

  chrons_calib <- PCA_chrons %>%
    dplyr::filter(PCA_chrons$year %in% calib)

  PCA_chrons_calib <- chrons_calib %>%
    dplyr::select(-year)

  PCA <- prcomp(PCA_chrons_calib, scale = TRUE)

  #extract PC values
  PC_values <- as.data.frame(PCA$x)
  PC_vals <- cbind(year = chrons_calib$year,PC_values)

  # get eigenvalues and select those with cumulative variance explained <80%
  eigval <- as.data.frame(get_eig(PCA))
  PC <- c(1:ncol(PCA$x))
  PC <- paste0("PC", PC)

  eigval <- cbind(eigval, PC)

  #### linear model and Step AICc
  eigval_small <- dplyr::filter(eigval, eigval$cumulative.variance.percent <= 80.0)

  clim_full <- as.data.frame(filter(clim_seas, clim_seas$year %in% nest_yrs))
  clim <- filter(clim_full, clim_full$year %in% calib)

  PC_common <- filter(PC_vals, PC_vals$year %in% clim$year)

  # create DF to set up linear model

  MOD_PCs <- as.character(eigval_small$PC)

  PC_common_small <- PC_common %>% dplyr:: select(MOD_PCs)

  df <- cbind(clim = clim[ ,2] - mean(clim[ ,2]), PC_common_small)

  full_mod <- lm(formula = clim~., data = df)


  step_mod <- stepAICc(full_mod, trace = TRUE, k = 3)
  model_summary <- summary(step_mod)

  formula <- as.character(model_summary$terms)
  sigma <- model_summary$sigma
  r.sq <- model_summary$r.squared
  adj.r.sq <- model_summary$adj.r.squared
  nest_number <- i

  model_table <- data.frame(model = formula[3], sigma, r.sq, adj.r.sq, stringsAsFactors = FALSE, nest_lwr = min(nest_yrs))
  if(i == 1) {
    model_stats <- model_table
  } else {
    model_stats <- bind_rows(model_stats, model_table)

  }



  #### PCs for rest of nest years
  PCA_predict <- as.data.frame(predict(PCA, nest))

  #### reconstruction
  predict_nest <- predict(step_mod, PCA_predict, se.fit=TRUE, interval="confidence", level=0.95)
  recon_nest <- as.data.frame(cbind(year = nest_yrs, predict_nest[["fit"]]+ mean(clim$means)))

  #### validation
  est_valid <- filter(recon_nest, recon_nest$year %in% verif) %>%
    dplyr::select(fit)

  observed <- filter(clim_full, clim_full$year %in% full)

  obs_cal <- filter(clim_full, clim_full$year %in% calib)

  valid_yrs <- verif
  calib_yrs <- calib


  stats <- perf_stats(est_valid, observed, valid_yrs, calib_yrs, mod_id = nest_number)

  if(i == 1) {
    stats_table <- stats
  } else {
    stats_table <- bind_rows(stats_table, stats)

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
  sd_clim <- sd(clim$means)
  recon_nest$fit <- mn + (recon_nest$fit - mn) * sd_clim/sd_recon

  #### Add reconstruction to dataframe

  recon <- bind_rows(recon, recon_nest)


  #### drop out shortest chronology

  PCA_chrons <- PCA_chrons %>%
    dplyr::select(-!!periods_df$ID[i])

}
}
