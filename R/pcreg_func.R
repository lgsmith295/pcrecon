#' PCreg
#'
#' @param data
#' @param plot
#' @param m
#' @param weight
#' @param pc_calc
#' @param select_pc
#' @param cum_perc
#' @param scale_var
#' @param out_fmt
#' @param out_dir
#'
#' @return
#' @export
#'
#' @examples
#'
pcreg <- function(data, pc_calc = "calib", select_pc = "eigenvalue1", cum_perc = NULL, m = NULL, scale_var = "calib", weight = NULL, plot = TRUE, out_fmt = "csv", out_dir = "PCregOutput/"){

  if(class(data) != "PCreg_data"){ stop( "data must be object class PCreg_data, as is returned from the eval_clim function. See documentation for details")}

  calib <- data$calib
  valid <- data$valid
  full <- data$full
  prewhiten_crn <- data$prewhiten_crn
  prewhiten_clim <-data$prewhiten_clim
  dir <- data$dir


  periods_df <- data$nests
  PCA_chrons <- data$select_crns
  clim <- data$clim


  number_nests <- nrow(periods_df)


  for (i in 1 : number_nests) {
    nest_yrs <- c(periods_df$startYR[i] : min(periods_df$endYR))

    # temp hack - skip if nest years already all in recon - future change construction of nests and how chronologies dropped out - based on start years - do in git branch
    if(i > 1) {
      if(all(nest_yrs %in% recon$year)) {
        #### drop out shortest chronology
        PCA_chrons <- PCA_chrons %>%
          dplyr::select(-!!periods_df$ID[i])
        next
      }
    } # end hack

    ## make check for start year < end year
    PCA <- calc_PCs(periods_df, PCA_chrons, pc_calc, nest_yrs, calib, full)

    if(i ==1){
    PCA_list <- list(PCA$PCA)
    } else {
    PCA_list[[i]] <- PCA$PCA
    }

    select_PC <- select_PCs(data = PCA, type = select_pc, m = m, cum_perc = cum_perc)

    df <- mod_df(clim = clim, data = select_PC$PC_vals, eig = select_PC$eigval_small, nest_yrs = nest_yrs, calib = calib)


    full_mod <- lm(formula = clim~., data = df, na.action = na.omit)

    step_mod <- stepAICc(full_mod) ## check and document the k = 3 option. During test runs this is what made it select the same model as PCreg.

    if(isTRUE(length(step_mod$coefficients) == 1)) {
      warning(paste0("For nest ", i, ", years " , min(nest_yrs), " to ", min(recon_nest$year), " , the AIC selected linear regression is the intercept only model."))
    }

    model_stats <- sum_mod(data = step_mod, i, nest_yrs)

    if(i == 1) {
      model_table <- model_stats
    } else {
      model_table <- dplyr::bind_rows(model_table, model_stats)

    }

    if(i ==1){
      LM_list <- list(step_mod)
    } else {
      LM_list[[i]] <- step_mod
    }

    #### PCs for rest of nest years
    PCA_predict <- as.data.frame(predict(PCA$PCA , PCA$nest))

    #### reconstruction

    clim_full <- as.data.frame(dplyr::filter(clim, clim$year %in% nest_yrs))

    observed <- dplyr::filter(clim_full, clim_full$year %in% full)

    predict_nest <- predict(step_mod, PCA_predict, se.fit=TRUE, interval="confidence", level=0.95)
    recon_nest <- as.data.frame(cbind(year = nest_yrs, values = predict_nest[["fit"]]))

    #### validation
    valid_est <- dplyr::filter(recon_nest, recon_nest$year %in% valid) %>%
      dplyr::select(fit)

    calib_est <- dplyr::filter(recon_nest, recon_nest$year %in% calib) %>%
      dplyr::select(fit)

    obs_cal <- dplyr::filter(clim_full, clim_full$year %in% calib)

    stats <- perf_stats(valid_est, calib_est, observed, valid_yrs = valid, calib_yrs = calib, mod_id = i)

    if(i == 1) {
      val_stats_table <- stats$validation_stats
    } else {
      val_stats_table <- dplyr::bind_rows(val_stats_table, stats$validation_stats)

    }

    if(i == 1) {
      cal_stats_table <- stats$calibration_stats
    } else {
      cal_stats_table <- dplyr::bind_rows(cal_stats_table, stats$calibration_stats)

    }


    #### scale to variance of calibration

    full_est <-dplyr::filter(recon_nest, year %in% full)
    lower_diff <- recon_nest$fit - recon_nest$lwr
    upper_diff <- recon_nest$upr - recon_nest$fit

    mn <- mean(calib_est$fit, na.rm = TRUE)

    sd_recon <- sd(calib_est$fit, na.rm = TRUE)
    sd_clim <- sd(obs_cal$values, na.rm = TRUE)


    recon_nest$fit <- mn + (recon_nest$fit - mn) * sd_clim/sd_recon
    recon_nest$lwr <- recon_nest$fit - lower_diff
    recon_nest$upr <- recon_nest$fit + upper_diff

    #### redden nest values

    if(prewhiten_clim == TRUE){
      recon_nest <- redden_recon(recon = recon_nest, ar_model = data$clim_ar)
}

    ################### SAVE - THIS IS HOW SCALING FOR NPW WORKS IN PCREG########## dunno why it's different for reddened recons... :/
    # mn <- mean(calib_est$fit)
    #
    # sd_recon <- sd(calib_est$fit)
    # sd_clim <- sd(obs_cal$values)
    # recon_nest$fit <- mn + (recon_nest$fit - mn) * sd_clim/sd_recon


    #### filter recon to new nest years

    if(i == 1) {
      recon <- recon_nest
    } else {
      new_years <- periods_df[i, "startYR"]:(periods_df[i-1, "startYR"] - 1)
      recon_nest <- recon_nest %>%
        dplyr::filter(year %in% new_years)
    }


    #### Add reconstruction to dataframe

    if(i == 1){
      recon <- recon_nest
    }else{
      recon <- dplyr::arrange(dplyr::bind_rows(recon, recon_nest), year)
    }

    #### drop out shortest chronology
# drop all with same start year
    PCA_chrons <- PCA_chrons %>%
      dplyr::select(-!!periods_df$ID[i])

  }


    recon_list <- list(clim = clim, recon = recon, validation_stats = val_stats_table, model_stats = model_table,                         calibration_stats = cal_stats_table, PCA = PCA_list, LM = LM_list)

    if(isTRUE(prewhiten_clim)){recon_list <- list(clim = clim, recon = recon, validation_stats = val_stats_table,
                       model_stats = model_table, calibration_stats = cal_stats_table, clim_ar = data$clim_ar,                            crn_ar = data$crn_ar, PCA = PCA_list, LM = LM_list)}

  class(recon_list) <- "PCReg_recon"

  if(!is.null(out_fmt)){
    to_save <- list(reconstruction = recon_list$recon, model_statistics = cbind(recon_list$model_stats, recon_list$calibration_stats[ ,-c(1:3)]), validation_statistics = recon_list$validation_stats)
    save_data(data = to_save, save_out, dir)
  }

  if(plot == TRUE){
    df <- dplyr::full_join(recon_list$recon, data$clim)

    print(ggplot2::ggplot(df) +
            ggplot2::geom_line(ggplot2::aes(y = fit, x = year, colour = "reconstructed")) +
            ggplot2::geom_line(ggplot2::aes(y = rollingmean(x = df$fit, 10), x = year, colour = "10 year rolling avg"),                                             size = 0.75) +
            ggplot2::geom_line(ggplot2::aes(y = values, x = year, colour = "observed"))) +
      ggplot2::theme_bw()
  }

  return(recon_list)
}



#' Create dataframe containing clim and select PCs for linear model
#'
#' @param data PCA values
#' @param eig selected eigenvalues and PCs
#'
#' @return
#'
#' @examples
mod_df <- function(data = PC_vals, clim, eig = eigval_small, nest_yrs, calib) {
  #### linear model and Step AICc
  clim_full <- as.data.frame(dplyr::filter(clim, clim$year %in% nest_yrs))
  clim <- dplyr::filter(clim_full, clim_full$year %in% calib)

  PC_common <- dplyr::filter(data, data$year %in% clim$year)

  # create DF to set up linear model

  MOD_PCs <- as.character(eig$PC)

  PC_common_small <- dplyr:: select(PC_common, MOD_PCs)

  clim_calib <- dplyr::filter(clim, year %in% calib)

  df <- cbind(clim = clim[ ,2], PC_common_small)

}


#' linear regression model stats
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


#' Rolling Mean
#'
#' @param x
#' @param n
#'
#' @return
#'
#' @examples
rollingmean <- function(x, n = 10){stats::filter(x, rep(1 / n, n), sides = 2)}
