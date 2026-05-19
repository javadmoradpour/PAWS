# =============================================================================
# load.R
# Purpose:  Establish database connection, define reusable query functions,
#           and load core datasets into R memory for downstream analysis.
#
# Structure:
#   1. Create Connection
#   2. Helper Functions
#      - parse_age_filter()
#   3. Query Functions
#      - get_dad_data()
#      - get_msp_data()
#   4. Close Connection
# =============================================================================


# 1. Create Connection --------------------------------------------------------
# Connects to the SPEDW data warehouse using the DSN (Data Source Name) method.
# The DSN must be configured in your ODBC driver settings beforehand.

con <- dbConnect(odbc(), "SPEDW")


# 2. Helper Functions ---------------------------------------------------------

#' Parse an age filter input into a named list of lower/upper bounds.
#'
#' Accepts two input formats:
#'   - Numeric vector of length 2: e.g. c(18, 65)  → age BETWEEN 18 AND 65
#'   - "X+" string:                e.g. "65+"       → age >= 65 (no upper bound)
#'
#' @param x  A length-2 numeric vector or a "X+" character string.
#' @return   A named list with elements `lower` (numeric) and `upper` (numeric or Inf).
#' @examples
#'   parse_age_filter(c(18, 65))  # list(lower = 18, upper = 65)
#'   parse_age_filter("65+")      # list(lower = 65, upper = Inf)

parse_age_filter <- function(x) {
  if (is.numeric(x) && length(x) == 2) {
    list(lower = x[1], upper = x[2])
    
  } else if (is.character(x) && endsWith(x, "+")) {
    list(lower = as.numeric(sub("\\+", "", x)), upper = Inf)
    
  } else {
    stop("'age_filter' must be a 2-element numeric vector (e.g. c(18, 65)) ",
         "or an open-ended string (e.g. '65+').")
  }
}


# 3. Query Functions ----------------------------------------------------------
# Each function builds a lazy dplyr query, optionally applies an age filter,
# then calls collect() to pull the result into R memory.
# Using dplyr + dbplyr means the filtering happens on the server side —
# only the rows you need are transferred across the network.
#
# Column lists are defined just before the Load Data section (section 5) and
# passed in via the `columns` argument. This separates configuration from logic:
# you edit column lists in one place without touching the function bodies.
# Passing NULL selects all columns (SELECT *).

#' Retrieve DAD (Discharge Abstract Database) records.
#'
#' Filters by separation_date range and optionally by age.
#' patient_master_key is bounded to exclude test/invalid records.
#'
#' @param con         A DBI database connection object.
#' @param start_date  Start of date range (character "YYYY-MM-DD" or Date).
#' @param end_date    End of date range   (character "YYYY-MM-DD" or Date).
#' @param columns     Character vector of column names to select. NULL = all columns.
#' @param age_filter  Optional. c(min, max) or "X+" string. Default NULL (no filter).
#' @return            A tibble of DAD records collected into R memory.
#' @examples
#'   get_dad_data(con, "2021-01-01", "2021-12-31", columns = dad_cols)
#'   get_dad_data(con, "2021-01-01", "2021-12-31", columns = dad_cols, age_filter = c(18, 65))
#'   get_dad_data(con, "2021-01-01", "2021-12-31", columns = NULL)  # all columns

get_dad_data <- function(con, start_date, end_date, columns = NULL, age_filter = NULL) {
  
  query <- tbl(con, in_schema("nb0077aa", "vw_dad"))
  
  # Select specific columns if provided, otherwise keep all (SELECT *)
  if (!is.null(columns)) {
    query <- query %>% select(all_of(columns))
  }
  
  query <- query %>%
    filter(
      separation_date >= as_date(start_date),
      separation_date <= as_date(end_date),
      patient_master_key > 0,           # exclude invalid/test keys
      patient_master_key < 2000000000
    )
  
  # Optionally layer on an age filter using bounds from parse_age_filter()
  if (!is.null(age_filter)) {
    age <- parse_age_filter(age_filter)
    
    if (is.finite(age$upper)) {
      query <- query %>% filter(age_year >= !!age$lower, age_year <= !!age$upper)
    } else {
      query <- query %>% filter(age_year >= !!age$lower)  # open-ended, e.g. "65+"
    }
  }
  
  message("Collecting DAD data: ", start_date, " to ", end_date,
          if (!is.null(age_filter)) paste0(" | age: ", paste(age_filter, collapse = "-")) else "")
  
  collect(query)
}


#' Retrieve MSP (Medical Services Plan) records.
#'
#' Filters by serv_dt (service date) range and optionally by age group.
#'
#' Note: clnt_a_grp is used as the age field. If this column stores age-group
#' labels (e.g. "18-34") rather than numeric ages, age_filter will not apply
#' correctly — verify the column type before using age_filter with MSP data.
#'
#' @param con         A DBI database connection object.
#' @param start_date  Start of date range (character "YYYY-MM-DD" or Date).
#' @param end_date    End of date range   (character "YYYY-MM-DD" or Date).
#' @param columns     Character vector of column names to select. NULL = all columns.
#' @param age_filter  Optional. c(min, max) or "X+" string. Default NULL (no filter).
#' @return            A tibble of MSP records collected into R memory.
#' @examples
#'   get_msp_data(con, "2020-12-01", "2020-12-15", columns = msp_cols)
#'   get_msp_data(con, "2020-12-01", "2020-12-15", columns = msp_cols, age_filter = c(18, 65))
#'   get_msp_data(con, "2020-12-01", "2020-12-15", columns = NULL)  # all columns

get_msp_data <- function(con, start_date, end_date, columns = NULL, age_filter = NULL) {
  
  query <- tbl(con, in_schema("nb0077aa", "vw_msp"))
  
  # Select specific columns if provided, otherwise keep all (SELECT *)
  if (!is.null(columns)) {
    query <- query %>% select(all_of(columns))
  }
  
  query <- query %>%
    filter(
      serv_dt >= as_date(start_date),
      serv_dt <= as_date(end_date),
      patient_master_key > 0,           # exclude invalid/test keys
      patient_master_key < 2000000000
    )
  
  # Optionally layer on an age filter using bounds from parse_age_filter()
  
  if (!is.null(age_filter)) {
    age <- parse_age_filter(age_filter)
    current_year <- as.integer(format(Sys.Date(), "%Y"))
    
    if (is.finite(age$upper)) {
      query <- query %>% filter(
        clnt_birth_year <= current_year - !!age$lower,  # old enough
        clnt_birth_year >= current_year - !!age$upper   # young enough
      )
    } else {
      query <- query %>% filter(
        clnt_birth_year <= current_year - !!age$lower   # open-ended, e.g. "65+"
      )
    }
  }
  
  message("Collecting MSP data: ", start_date, " to ", end_date,
          if (!is.null(age_filter)) paste0(" | age: ", paste(age_filter, collapse = "-")) else "")
  
  collect(query)
}


# 4. Load Data ----------------------------------------------------------------
# Define column lists here. Edit these vectors to add/remove fields without
# touching the function bodies above.
# Set a vector to NULL to retrieve all columns from that view (SELECT *).

dad_cols <- c(
  "patient_master_key", "dad_id", "gender", "age_year",
  "admission_date", "separation_date",
  "file_year", "hospital",
  "hosp_from", "hosp_to", "care_level",
  "alc_days", "grpr_mthd_label", "cmg_mcc_label", "cmg_cmg_label", "cmg_expct_stay_days",
  "grpr_riw", "cmgp_riw_atpcl_label", "cacs_mac_label", "cacs_cd_label"
)

msp_cols <- c(
  "patient_master_key", "msp_id", "paye_num",
  "clnt_gndr", "clnt_a_grp","clnt_birth_year",
  "serv_dt",
  "paye_stat", "fitm", "paid_serv",
  "clm_tp", "clm_spec", "first_paid_date", "last_paid_date",
  "pmepd_amt", "pmeni_amt", "pmerl_amt", "expd_amt", "billed_service",
  "int_amt", "bcp_amt", "billed_service_clfn_code", "msp_claim_payment_category"
)

# Adjust date ranges and age_filter as needed for your analysis.
# Remove age_filter arguments entirely if no age restriction is required.

df_dad <- get_dad_data(
  con,
  start_date = "2020-12-01",
  end_date   = "2020-12-15",
  columns    = dad_cols
)

df_msp <- get_msp_data(
  con,
  start_date = "2020-12-01",
  end_date   = "2020-12-15",
  columns    = msp_cols
)


# 5. Close Connection ---------------------------------------------------------
# Always disconnect when done to free server resources.

dbDisconnect(con)
rm(con)
