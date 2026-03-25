# packages_attach.r

## Check required packages ------------------------
options(repos = c(CRAN = "https://cran.rstudio.com/"), pkgType = "binary")
required_packages <- c("jsonlite", "rcrossref", "tidyverse", "readr", "stringr", "openalexR", "purrr", "ggplot2", "ggpubr", "rgbif", "sf", "ggspatial", "scales", "patchwork", "packcircles", "countrycode")
install_if_missing <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
        install.packages(package)
    }
}
invisible(lapply(required_packages, install_if_missing))

## Attach packages ------------------------
library(jsonlite)
library(rcrossref)
library(tidyverse)
library(readr)
library(stringr)
library(openalexR)
library(purrr)
library(ggplot2)
library(ggpubr)
library(rgbif)
library(sf)
library(ggspatial)
library(scales)
library(patchwork)
library(packcircles)
library(countrycode)
