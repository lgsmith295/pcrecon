## reload package
devtools::load_all()

## parse json files from metadata folder
metadata <- parse_json(dir = system.file("extdata/metadata", package = "pcreg", mustWork = TRUE), center = TRUE)

## filter chronologies based on radius from a point
select_crns_rad <- filter_rad(data = metadata, cent_lat = 35.65, cent_lon = -105.32, radius = 150)

## filter based on climate footprint
## first upload climate footprint, which is a netCDF file downloaded from KNMI climate explorer
footprint <- raster::raster("inst/extdata/gallinas_cf.nc")
select_crns_fp <- filter_foot(data = metadata, footprint = footprint, r = 0.5)

## build large dataframe of chronologies selected from the spatial filter
crns_df <- load_crns(dir = system.file("extdata/crns", package = "pcreg", mustWork = TRUE), crns = select_crns_rad)
crns_df <- dplyr::arrange(crns_df, year)

## load in climate and rename columns
climate <- read.csv("inst/extdata/gallinas_flow.csv")
names <- c("year", 1:12)
colnames(climate) <- names

## run PCreg function!
recon1 <- pcreg(crns = crns_df, lead = 1, prewhiten.crn = TRUE, climate = climate, mos = 5:8, method = "mean", prewhiten.clim = TRUE,  cor.window = "calib", type = "pearson", alternative = "two.sided", r = 0.25, alpha = 0.90, calib = 1929:1935, valid = 1935:1950, pc.calc = "calib", select.pc = "eigenvalue1", scale.var = "calib", weight = NULL)


# arguments for pcreg function check
crns <- crns_df
lead <- 1
prewhiten.crn <- TRUE
climate <- climate
mos <- 5:8
method <- "mean"
prewhiten.clim <- TRUE
cor.window <- "calib"
type <- "pearson"
alternative <- "two.sided"
r <- 0.25
alpha <- 0.90
calib <- 1927:1957
valid <- 1957:1967
pc.calc <- "calib"
select.pc <- "eigenvalue1"
scale.var <- 'calib'
weight <- NULL
i <- 1


read_in <- colnames(crns_df[ ,-1])

didnt_load <- select_crns_rad[!stringr::str_detect(select_crns_rad, regex(paste(read_in, collapse = "|"), ignore_case = TRUE))]
