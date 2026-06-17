# =============================================================================
# analysis.R
# Project:  HSCT Cohort
# Purpose:  Summarise and explore the HSCT cohort produced by hsct_cohort.R.
#           Run after main.R (or hsct_cohort.R directly).
#
# Filtering applied at the top of this script:
#   1. Retain only patients who have at least one HSCT-relevant CCI code
#      (patients identified by ICD-10 only are excluded).
#   2. For each patient, keep only hospitalizations within 5 years of their
#      first CCI-coded admission.
#
# Downstream objects used throughout:
#   df_cci_index  — CCI-confirmed HSCT index admissions
#   df_followup   — all hospitalizations within the 5-year follow-up window
# =============================================================================


# 0. Cohort filtering ---------------------------------------------------------

# CCI prefixes that define an HSCT procedure (dots/hyphens already stripped
# when stored in the database — matches hsct_cohort.R HSCT_CCI definition)
HSCT_CCI_PREFIXES <- c("1WZ19", "1LZ19HHU7", "1LZ19HHU8")

# Regex: code must START WITH one of the prefixes
cci_pattern <- paste0("^(", paste(HSCT_CCI_PREFIXES, collapse = "|"), ")")

# Keep index admissions that have at least one matching CCI intervention code
df_cci_index <- df_hsct_index %>%
  filter(if_any(starts_with("interv_code_"), ~ !is.na(.) & grepl(cci_pattern, .)))

# First CCI-coded admission date per patient
first_cci <- df_cci_index %>%
  group_by(patient_master_key) %>%
  summarise(first_cci_date = min(admission_date), .groups = "drop")

# All hospitalizations for CCI patients, within 5 years of first CCI date
df_followup <- df_hsct_all %>%
  filter(patient_master_key %in% first_cci$patient_master_key) %>%
  left_join(first_cci, by = "patient_master_key") %>%
  filter(admission_date <= first_cci_date + years(5)) %>%
  select(-first_cci_date)

message(sprintf(
  "Cohort after CCI filter + 5-year follow-up: %d patients, %d admissions (%d HSCT index, %d other)",
  n_distinct(df_followup$patient_master_key),
  nrow(df_followup),
  sum(df_followup$is_hsct_index),
  sum(!df_followup$is_hsct_index)
))


# 1. Quick data check ---------------------------------------------------------

message("=== CCI-confirmed HSCT Index Admissions ===")
glimpse(df_cci_index)

message("=== 5-Year Follow-up Hospitalizations ===")
glimpse(df_followup)


# 2. Patient-level summary ----------------------------------------------------

patient_summary <- df_followup %>%
  group_by(patient_master_key, gender) %>%
  summarise(
    n_admissions       = n(),
    n_hsct_admissions  = sum(is_hsct_index),
    n_other_admissions = sum(!is_hsct_index),
    first_admission    = min(admission_date),
    last_admission     = max(separation_date),
    followup_years     = as.numeric(difftime(max(separation_date),
                                             min(admission_date),
                                             units = "days")) / 365.25,
    .groups = "drop"
  )

message(sprintf("\nPatients: %d", nrow(patient_summary)))
message(sprintf("  Admissions per patient — median: %.1f, range: %d–%d",
                median(patient_summary$n_admissions),
                min(patient_summary$n_admissions),
                max(patient_summary$n_admissions)))


# 3. Demographics of HSCT index admissions ------------------------------------

message("\n=== Demographics (CCI index admissions) ===")

# Gender
print(df_cci_index %>% count(gender) %>% mutate(pct = round(100 * n / sum(n), 1)))

# Age at index admission
message(sprintf("Age — median: %.1f  range: %d–%d  mean: %.1f",
                median(df_cci_index$age_year),
                min(df_cci_index$age_year),
                max(df_cci_index$age_year),
                mean(df_cci_index$age_year)))

# Age distribution (histogram)
ggplot(df_cci_index, aes(x = age_year)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "white") +
  labs(title = "Age distribution at HSCT index admission (CCI-confirmed)",
       x = "Age (years)", y = "Admissions") +
  theme_minimal()


# 4. Admissions over time -----------------------------------------------------

admissions_by_year <- df_cci_index %>%
  count(file_year) %>%
  arrange(file_year)

print(admissions_by_year)

ggplot(admissions_by_year, aes(x = file_year, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "HSCT index admissions by year (CCI-confirmed)",
       x = "Year", y = "Admissions") +
  theme_minimal()


# 5. Top ICD-10 diagnoses in index admissions ---------------------------------

top_icd <- df_cci_index %>%
  count(diag_code_1, sort = TRUE) %>%
  slice_head(n = 15)

message("\n=== Top 15 primary ICD-10 codes (CCI index admissions) ===")
print(top_icd)


# 6. Top ICD-10 diagnoses in non-HSCT hospitalizations -----------------------

top_icd_other <- df_followup %>%
  filter(!is_hsct_index) %>%
  count(diag_code_1, sort = TRUE) %>%
  slice_head(n = 15)

message("\n=== Top 15 primary ICD-10 codes (other hospitalizations, 5-yr follow-up) ===")
print(top_icd_other)


# 7. Top CCI intervention codes in index admissions --------------------------

top_cci <- df_cci_index %>%
  select(starts_with("interv_code_")) %>%
  pivot_longer(everything(), values_to = "code") %>%
  filter(!is.na(code)) %>%
  count(code, sort = TRUE) %>%
  slice_head(n = 15)

message("\n=== Top 15 CCI intervention codes (CCI index admissions) ===")
print(top_cci)


# 8. Length of stay -----------------------------------------------------------

los_summary <- df_followup %>%
  mutate(los = as.integer(separation_date - admission_date)) %>%
  group_by(is_hsct_index) %>%
  summarise(
    n        = n(),
    median   = median(los, na.rm = TRUE),
    mean     = round(mean(los, na.rm = TRUE), 1),
    p25      = quantile(los, 0.25, na.rm = TRUE),
    p75      = quantile(los, 0.75, na.rm = TRUE),
    max      = max(los, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(type = if_else(is_hsct_index, "HSCT index", "Other")) %>%
  select(type, n, median, mean, p25, p75, max)

message("\n=== Length of stay (days) ===")
print(los_summary)

ggplot(df_followup %>%
         mutate(los  = as.integer(separation_date - admission_date),
                type = if_else(is_hsct_index, "HSCT index", "Other")),
       aes(x = los, fill = type)) +
  geom_histogram(binwidth = 5, position = "dodge", colour = "white") +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Length of stay — HSCT index vs. other admissions (5-yr follow-up)",
       x = "Days", y = "Admissions", fill = NULL) +
  theme_minimal()


# 9. Hospital distribution ----------------------------------------------------

message("\n=== Hospitals (CCI index admissions) ===")
print(df_cci_index %>% count(hospital, sort = TRUE))


# 10. Export ------------------------------------------------------------------
# Uncomment to save results to Excel:
#
# wb <- createWorkbook()
# addWorksheet(wb, "cci_index");       writeData(wb, "cci_index",       df_cci_index)
# addWorksheet(wb, "followup");        writeData(wb, "followup",        df_followup)
# addWorksheet(wb, "patient_summary"); writeData(wb, "patient_summary", patient_summary)
# saveWorkbook(wb, "output/hsct_cohort.xlsx", overwrite = TRUE)
