## reload package

devtools::load_all()

## parse json files from metadata folder
metadata <- parse_json(dir = system.file("extdata/metadata", package = "pcreg", mustWork = TRUE), center = TRUE)
metadata <- metadata[-58, ]

## filter chronologies based on radius from a point
select_crns_rad <- filter_rad(data = metadata, cent_lat = 36.21, cent_lon = -105.9, radius = 150)

## filter based on climate footprint
## first upload climate footprint, which is a netCDF file downloaded from KNMI climate explorer
footprint <- raster::raster("inst/extdata/gallinas_cf.nc")
select_crns_fp <- filter_foot(data = metadata, footprint = footprint, r = 0.5)

## build large dataframe of chronologies selected from the spatial filter
crns_df <- load_crns(dir = system.file("extdata/crns", package = "pcreg", mustWork = TRUE), crns = select_crns_fp)
crns_df <- dplyr::arrange(crns_df, year)

climate <- read.csv("inst/extdata/gallinas_flow.csv")
names(climate) <- c("year", c(1:12))
clim_long <- climate %>%
  tidyr::pivot_longer(cols = -year, names_to = "month") %>%
  dplyr::mutate(log_clim = log(.$value)) %>%
  dplyr::select(-value)

clim_long$value <- round(clim_long$log_clim, 4)

clim_long <- clim_long[ ,-3]

data <- eval_clim(crns = crns_df, lead = 1, lag = 1, prewhiten.crn = TRUE, climate = clim_long, mos = 5:8, method = "mean", prewhiten.clim = TRUE, calib = c(1927:1945, 1958:1969), valid = 1946:1957, cor.window = "calib", type = "pearson", alternative = "two.sided", r = -1, alpha = 0.9, print.out = TRUE, save.out = "csv", dir = "test/")


## run PCreg function!
recon <- pcreg(data = data, pc.calc = "calib", select.pc = "eigenvalue1", scale.var = "calib", plot = TRUE, weight = NULL, cum.perc = NULL, save.out = "csv", dir = dir)

#resid_check(data = recon)

recon_filter <- recon$recon %>%
  dplyr::filter(year %in% c(1700:1972))

pcreg_comp <- read.csv("inst/extdata/pcreg_comp.csv")
pcreg_npw <- read.csv("inst/extdata/pcreg_npw1.csv")
pcreg_red <- read.csv("inst/extdata/pcreg_red_recon.csv")

lm_chck <- pcreg_red %>%
  dplyr::filter(year %in% c(1700:1972))
summary(lm(lm_chck$X1 ~ recon_filter$reds))


#### sd 0.79 mn

ggplot2::ggplot(recon_filter) +
  #geom_ribbon(aes(ymin = lwr, ymax = upr, x = year), fill = "gray50", alpha = 0.5) +
  ggplot2::geom_line(ggplot2::aes(y = reds, x = year), color = "blue") +
  ggplot2::geom_line(data = lm_chck, ggplot2::aes(y = X1, x = year), color = "red")
#ggplot2::geom_line(ggplot2::aes(y = reds, x = year), color = "black")

npw <- ar(pcreg_red)
red <- ar(pcreg_npw)

# arguments for clim_eval function check
crns <- crns_df
lead <- 1
prewhiten.crn <- FALSE
clim <- clim_long
mos <- 5:8
method <- "mean"
prewhiten.clim <- FALSE
cor.window <- "calib"
type <- "pearson"
alternative <- "two.sided"
r <- 0.25
alpha <- 0.90
calib <- 1927:1945
valid <- 1946:1972
print.out <- TRUE
save.out <- "table"
dir <- "PCregOutput/"
i <- 1
j <- 1
lag <- 1


pc.calc <- "calib"
select.pc <- "eigenvalue1"
scale.var <- 'calib'
weight <- NULL


read_in <- colnames(crns_df[ ,-1])

didnt_load <- select_crns_rad[!stringr::str_detect(select_crns_rad, stringr::regex(paste(read_in, collapse = "|"), ignore_case = TRUE))]

