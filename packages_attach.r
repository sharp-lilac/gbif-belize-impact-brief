# packages_attach.r

## Check required packages ------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"))
required_packages <- c("httr2", "jsonlite", "rcrossref", "dplyr", "readr", "stringr")
install_if_missing <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package)
    }
}
invisible(lapply(required_packages, install_if_missing))

## Attach packages ------------------------
library(httr2)
library(jsonlite)
library(rcrossref)
library(dplyr)
library(readr)
library(stringr)
