## reload package
devtools::load_all()

## parse json files from metadata folder
metadata <- parse_json(dir = system.file("extdata/metadata", package = "pcreg", mustWork = TRUE), center = TRUE)

## filter chronologies based on radius from a point
select_crns_rad <- filter_rad(data = metadata, cent_lat = 36.21, cent_lon = -105.9, radius = 250)

## filter based on climate footprint
## first upload climate footprint, which is a netCDF file downloaded from KNMI climate explorer
footprint <- raster::raster("inst/extdata/gallinas_cf.nc")
select_crns_fp <- filter_foot(data = metadata, footprint = footprint, r = 0.5)

## build large dataframe of chronologies selected from the spatial filter
crns_df <- load_crns(dir = system.file("extdata/crns", package = "pcreg", mustWork = TRUE), crns = select_crns_fp)
crns_df <- dplyr::arrange(crns_df, year)

## load in climate and rename columns
climate <- read.csv("inst/extdata/gallinas_flow.csv")
colnames(climate) <- c("year", c(1:12))

clim_long <- climate %>%
  tidyr::pivot_longer(cols = -year, names_to = "month") %>%
  dplyr::mutate(log_clim = log(.$value)) %>%
  dplyr::select(-value)

names <- c("year", "month", "value")
colnames(clim_long) <- names

## eval clim

data <- eval_clim(crns = crns_df, lag = 1, prewhiten.crn = TRUE, climate = clim_long, mos = 5:8, method = "mean", prewhiten.clim = TRUE, calib = 1927:1945, valid = 1946:1968, cor.window = "valid", type = "pearson", alternative = "two.sided", r = 0.3, alpha = 0.9, print.out = TRUE, save.out = "csv", dir = "test/")

## run PCreg function!
recon <- pcreg(data = data, pc.calc = "calib", select.pc = "mean", scale.var = "calib", plot = TRUE, weight = NULL, cum.perc = NULL, save.out = "csv", dir = dir)

resid_check(data = recon)

recon_filter <- recon$recon %>%
  dplyr::filter(year %in% c(1580:1968))

ggplot2::ggplot(recon_filter) +
  #geom_ribbon(aes(ymin = new_lwr, ymax = new_upr, x = year), fill = "gray50", alpha = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = exp(fit), x = year), color = "red") +
  ggplot2::geom_line(data = data$clim, ggplot2::aes(y = exp(values), x = year), color = "black")

# arguments for clim_eval function check
crns <- crns_df
lead <- 1
prewhiten.crn <- FALSE
climate <- clim_long
mos <- 5:8
method <- "mean"
prewhiten.clim <- FALSE
cor.window <- "calib"
type <- "pearson"
alternative <- "two.sided"
r <- 0.25
alpha <- 0.90
calib <- 1927:1957
valid <- 1957:1967
print.out <- TRUE
save.out <- "table"
dir <- "PCregOutput/"

pc.calc <- "calib"
select.pc <- "eigenvalue1"
scale.var <- 'calib'
weight <- NULL
i <- 1


read_in <- colnames(crns_df[ ,-1])

didnt_load <- select_crns_fp[!stringr::str_detect(select_crns_fp, regex(paste(read_in, collapse = "|"), ignore_case = TRUE))]
