options(repos = c(CRAN = "https://positpmlz.phsa.ca/prod-cran/latest")) 

install.packages("renv", type = "binary")

renv::init()

renv::install("dplyr")

renv::install(c("readr",
                "ggplot2",
                "lubridate",
                "openxlsx",
                "purrr"))

renv::install(c("DBI",
                "odbc"))

renv::install("dbplyr")