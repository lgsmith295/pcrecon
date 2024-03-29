
system.file("extdata/crns", "nm014.crn", package = "pcreg", mustWork = TRUE)
dir <- system.file("extdata/crns", package = "pcreg", mustWork = TRUE)

df <- load_crns(dir = system.file("extdata/crns", package = "pcreg", mustWork = TRUE), crns = list.files(system.file("extdata/crns", package = "pcreg", mustWork = TRUE)))


foo <- read.table(file = system.file("extdata/crns", "nm014-noaa.crn", package = "pcreg", mustWork = TRUE), header = TRUE, sep = "", comment.char = "#")

str(foo)

all_crn <- stringr::str_remove(list.files(system.file("extdata/crns", package = "pcreg")), "\\.crn")

df <- load_crns(dir = system.file("extdata/crns",
                                  package = "pcreg"),
                crns = all_crn,
                type_crn = "S",
                type_measure = "R")

# 014 should read in one standard but not -noaa
# 537 has -noaa, d, d-noaa, e, e-noaa, i, i-noaa, l, l-noaa, n, n-noaa, t, t-noaa, x, x-noaa
#560 has -noaa, a, r, r-noaa
crns_test <- c("nm014", "nm537", "nm560", "nm569")
df <- load_crns(dir = system.file("extdata/crns",
                                  package = "pcreg"),
                crns = crns_test,
                type_crn = "S",
                type_measure = "R")
colnames(df) # expect "year"  "nm014" "nm537" "nm560" "nm569"

# foo <- read_crn(system.file("extdata/crns/nm569.crn", package = "pcreg"))
# str(foo)

crns_test <- c("nm014", "nm537", "nm560")
df <- load_crns(dir = system.file("extdata/crns",
                                  package = "pcreg"),
                crns = crns_test,
                type_crn = "A",
                type_measure = "R")
colnames(df) # expect "year"   "nm560a"

crns_test <- c("nm014", "nm537", "nm560")
df <- load_crns(dir = system.file("extdata/crns",
                                  package = "pcreg"),
                crns = crns_test,
                type_crn = "R",
                type_measure = "R")
colnames(df)

crns_test <- c("nm014", "nm537", "nm560", "nm569")
df <- load_crns(dir = system.file("extdata/crns",
                                  package = "pcreg"),
                crns = crns_test,
                type_crn = "S",
                type_measure = "E")
colnames(df) # expect "year"   "nm537e"

# test with crn files with < 4 header rows (nm588, nm602) - nm588r but no standard, 602 = standard
crns_test <- c("nm014", "nm588", "nm602")
df <- load_crns(dir = system.file("extdata/crns",
                                  package = "pcreg"),
                crns = crns_test,
                type_crn = "S",
                type_measure = "R")
colnames(df) # expect "year"  "nm014" "nm602"


crns_test <- c("nm038", "nm605", "nm606", "nm607")
df <- load_crns(dir = system.file("extdata/crns",
                                  package = "pcreg"),
                crns = crns_test,
                type_crn = "S",
                type_measure = "R")
colnames(df)

