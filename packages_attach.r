# packages_attach.r

## Check required packages ------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"), pkgType = "binary")
required_packages <- c("jsonlite", "rcrossref", "dplyr", "readr", "stringr", "openalexR", "purrr")
install_if_missing <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package)
    }
}
invisible(lapply(required_packages, install_if_missing))

## Attach packages ------------------------
library(jsonlite)
library(rcrossref)
library(dplyr)
library(readr)
library(stringr)
library(openalexR)
library(purrr)
