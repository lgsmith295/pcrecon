
system.file("extdata/crns", "nm014.crn", package = "pcreg", mustWork = TRUE)
dir <- system.file("extdata/crns", package = "pcreg", mustWork = TRUE)

df <- load_crns(dir = system.file("extdata/crns", package = "pcreg", mustWork = TRUE), crns = list.files(system.file("extdata/crns", package = "pcreg", mustWork = TRUE)))

df <- load_crns(dir = system.file("extdata/crns", package = "pcreg", mustWork = TRUE),
                crns = list.files(system.file("extdata/crns", package = "pcreg", mustWork = TRUE)),
                type = "noaa")


foo <- read.table(file = system.file("extdata/crns", "nm014-noaa.crn", package = "pcreg", mustWork = TRUE), header = TRUE, sep = "", comment.char = "#")

str(foo)

all_crn <- stringr::str_remove(list.files(system.file("extdata/crns", package = "pcreg")), "\\.crn")

df <- load_crns(dir = system.file("extdata/crns",
                                  package = "pcreg"),
                crns = all_crn,
                type_crn = "S",
                type_measure = "R")
