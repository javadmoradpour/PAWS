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


# Load Data ---------------------------------------------------------------
source(file.path(getwd(), "script", "load.R"))

# Source Scripts ----------------------------------------------------------

source(file.path(getwd(), "script", "explore_JT.R"))
