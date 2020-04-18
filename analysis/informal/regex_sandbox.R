library(stringr)

alphanumeric <- c("abc", "123", "AbcD123", "a1", "ab12c")


str_split(alphanumeric, "[:alpha:]")
str_split_fixed(alphanumeric, "[:alpha:]", n = 5)

str_match(alphanumeric, "[:alpha:]+")

str_match(alphanumeric, "\\d+")
str_match(alphanumeric, "\\d+")
str_match(alphanumeric, "\\w+")

str_extract(alphanumeric, "[:alpha:]+")
str_extract_all(alphanumeric, "[:alpha:]+", simplify = TRUE)

str_extract(alphanumeric, "^[:alpha:]+")
str_extract(alphanumeric, "[:alpha:]+$")
str_extract(alphanumeric, "(\\d)([:alpha:]+)")
str_split(alphanumeric, "([:alpha:]+)(\\d+)", simplify = TRUE)

str_split(alphanumeric, "([:alpha:]+)(\\d+)([:alpha:])", simplify = TRUE)

str_extract_all(alphanumeric, "([:alpha:]+)|(\\d+)|([:alpha:])", simplify = TRUE)
str_extract_all(alphanumeric, "([:alpha:]+)|(?<=[:alpha:]{10}\\d+)|([:alpha:])", simplify = TRUE)

str_split(alphanumeric, "\\b", simplify = TRUE)

str_extract_all(alphanumeric, "([:alpha:]+)(\\d+)([:alpha:])", simplify = TRUE)

str_sub(alphanumeric, "([:alpha:]+)(\\d+)([:alpha:])", simplify = TRUE)

grep("[a-z]", letters)

