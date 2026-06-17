# Libraries ---------------------------------------------------------------
library(dplyr)
library(readr)
library(ggplot2)
library(lubridate)
library(purrr)
library(openxlsx)
library(DBI)
library(odbc)
library(dbplyr)
library(tidyr)


# HSCT Cohort Extraction --------------------------------------------------
source(file.path(getwd(), "script", "hsct_cohort.R"))

# Analysis ----------------------------------------------------------------
source(file.path(getwd(), "script", "analysis.R"))
