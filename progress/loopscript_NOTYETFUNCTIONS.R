ptm <- proc.time()
source("functions.R")

#Load chronologies and climate data folders

file_names_chron <- list.files("sigfree_chrons")
file_names_clim <- list.files("clim")

# establish common , calibration, and verification periods
full <- c(1900:1979)
verif <- c(1900:1939)
calib <- c(1940:1979)

#season/months of interest
for (i in 1: length(file_names_clim)) {
  clim <- read.csv(paste0("clim/", file_names_clim[i])) #pull climate datsset
  mos <- c("year","may", "jun","jul", "aug") ##select months to average into season
  clim_seas <- dplyr::select(clim, mos) #select those months from the clim_seas dataframe
  means <- rowMeans(clim_seas[ ,-1]) #average across rows, excluding row 1(year)
  clim_seas <- as.data.frame(cbind(year = clim_seas[ ,1], means))
  #Prewhiten (if TRUE) using dplR ar.func
  #clim_seas$means <- dplR:::ar.func(clim_seas$means) # dplr function for AR prewhitening
  clim_seas <- clim_seas[complete.cases(clim_seas), ] ##omit any rows that contain NAs
}


##### fill table with all chronology and lead 1 chronologies

for (i in 1:length(file_names_chron)) {
  chron <- read.crn(paste0("sigfree_chrons/", file_names_chron[i]))
  colnames(chron)[1] <- file_names_chron[i]
  lag_name <- paste0(file_names_chron[i], "_lag")
  chron <- rownames_to_column(chron, "year") %>%
    dplyr::select(-samp.depth) %>%
    mutate(year = as.integer(as.numeric(year))) %>%
    mutate(!!lag_name := lead(chron[ ,1], 1))
  if(i == 1) {
    all_chronologies <- chron
  } else {
    all_chronologies <- full_join(all_chronologies, chron, by = "year")
  }
}

remove(chron, mos, lag_name, means)

#make tables to fill with correlation output for each chronology. This will be used to make a list of
#chronologies that meet your threshhold (pearsons r, p-value, or both) and will be loaded in PCA
rows <- ncol(all_chronologies)-1 # set # of rows to however many chronologies there are
cors_table <- as.data.frame(matrix(NA, nrow = rows, ncol = 3))
# make data frame with n rows and 4 columns (chronology ID, lag, correlation, and -value)
names <-  c("chronology", "correlation", "p_value") #column names for tables
colnames(cors_table) <- names #Add column names to each table

# clip everything to calibration period for correlation

####try with PCreg prewhitened climate
#
#clim_wht <- read.csv("pcreg_prewhite_clim.csv")

cor_clim <- clim_seas %>%
  dplyr::filter(year %in% calib)

cor_crns <- all_chronologies %>%
  dplyr::filter(year %in% calib) %>%
  dplyr::select(-year)


names <- colnames(cor_crns)

for (i in 1:length(names)) {
  crn <- cor_crns[ ,i]
  clim<- cor_clim[ ,2]
  cor <- cor.test(clim, crn, conf.level = 0.1)
  cors_table[i,] <- cbind(names[i], cor$estimate, cor$p.value)
}


#### select ones that meet thresholds

cors_table_small <- filter(cors_table, cors_table$correlation > 0.25) %>%
  filter(p_value <= 0.1)

keepers <- c("year", cors_table_small$chronology)

PCA_chrons <- subset(all_chronologies, select=keepers)

PCA_chrons <- PCA_chrons %>%
  arrange(year)

rows <- ncol(PCA_chrons)-1
periods_df <- as.data.frame(matrix(ncol = 3, nrow = rows))
colnames(periods_df) <- c("ID", "startYR", "endYR")

##Loop through filling in common periods dataframe - this is for automating the nests
for (i in 2:ncol(PCA_chrons)) {
  logical <- !is.na(PCA_chrons[ ,i])
  years <- PCA_chrons$year[logical]
  start <- as.numeric(years[1])
  end <- as.numeric(tail(years, n=1))
  periods_df[i-1, 1] <- colnames(PCA_chrons[i])
  periods_df[i-1, 2] <- start
  periods_df[i-1, 3] <- end
}

periods_df <- periods_df %>%
  arrange(desc(startYR))

###### Loop for nests ######


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

} # end large nest loop

ggplot2::ggplot(recon_nest) +
  ggplot2::geom_line(ggplot2::aes(y = exp(fit), x = year), color = "red") +
  ggplot2::geom_line(climate, ggplot2::aes(y = exp(value), x = year), color = "black")



 model_stats$nest_upr <- c(1980, model_stats$nest_lwr[1:(nrow(model_stats)-1)])
 model_stats$nest <- 1:nrow(model_stats)

 scale_df <- recon %>%
   filter(recon$year %in% calib)

 mn <- mean(scale_df$fit)

 sd_recon <- sd(scale_df$fit)
 sd_clim <- sd(clim$means)
 recon_rescale <- mn + (recon$fit - mn) * sd_clim/sd_recon

 recon$rescale <- recon_rescale^2


 resid_up <- recon$upr - recon$fit
 resid_low <- recon$fit - recon$lwr

 new_upr <- (recon$rescale + resid_up)
 new_lwr <- (recon$rescale - resid_low)

 recon$scale_upr <- new_upr
 recon$scale_lwr <- new_lwr
 recon <- full_join(recon, clim_full)

 # #
 ####### Add variance scaling for final fit
 # mn <- mean(recon$fit[c])
 # sd_recon <- sd(recon_nest$fit[58:98])
 # sd_clim <- sd(clim$means)
 # recon_rescale <- mn + (recon_nest$fit - mn) * sd_clim/sd_recon

 ggplot2::ggplot(recon1$recon$fit) +
   #geom_ribbon(aes(ymin = new_lwr, ymax = new_upr, x = year), fill = "gray50", alpha = 0.5) +
   geom_rect(data = model_stats, aes(xmin = nest_lwr, xmax = nest_upr, ymin = 2, ymax = 7,
                                    fill = as.factor(nest), color = as.factor(nest)), alpha = 0.4) +
   geom_line(aes(y = rescale, x = year), color = "red") +
   geom_line(aes(y = means^2, x = year), color = "black")

  #geom_line(aes(y = means^2, x = year), color = "black")


 Stop the clock
proc.time() - ptm
