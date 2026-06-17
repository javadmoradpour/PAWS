# =============================================================================
# analysis.R
# Project:  HSCT Cohort
# Purpose:  Summarise and explore df_hsct_index and df_hsct_all produced by
#           hsct_cohort.R.  Run after main.R (or hsct_cohort.R directly).
# =============================================================================


# 1. Quick data check ---------------------------------------------------------

message("=== HSCT Index Admissions ===")
glimpse(df_hsct_index)

message("=== All Hospitalizations (HSCT patients) ===")
glimpse(df_hsct_all)


# 2. Patient-level summary ----------------------------------------------------

patient_summary <- df_hsct_all %>%
  group_by(patient_master_key, gender) %>%
  summarise(
    n_admissions       = n(),
    n_hsct_admissions  = sum(is_hsct_index),
    n_other_admissions = sum(!is_hsct_index),
    first_admission    = min(admission_date),
    last_admission     = max(separation_date),
    study_years        = as.numeric(difftime(max(separation_date),
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

message("\n=== Demographics (index admissions) ===")

# Gender
print(df_hsct_index %>% count(gender) %>% mutate(pct = round(100 * n / sum(n), 1)))

# Age at index admission
message(sprintf("Age — median: %.1f  range: %d–%d  mean: %.1f",
                median(df_hsct_index$age_year),
                min(df_hsct_index$age_year),
                max(df_hsct_index$age_year),
                mean(df_hsct_index$age_year)))

# Age distribution (histogram)
ggplot(df_hsct_index, aes(x = age_year)) +
  geom_histogram(binwidth = 5, fill = "steelblue", colour = "white") +
  labs(title = "Age distribution at HSCT index admission",
       x = "Age (years)", y = "Admissions") +
  theme_minimal()


# 4. Admissions over time -----------------------------------------------------

# Index admissions by year
admissions_by_year <- df_hsct_index %>%
  count(file_year) %>%
  arrange(file_year)

print(admissions_by_year)

ggplot(admissions_by_year, aes(x = file_year, y = n)) +
  geom_col(fill = "steelblue") +
  labs(title = "HSCT index admissions by year",
       x = "Year", y = "Admissions") +
  theme_minimal()


# 5. Top ICD-10 diagnoses in index admissions ---------------------------------

# Primary diagnoses across all index admissions
top_icd <- df_hsct_index %>%
  count(diag_code_1, sort = TRUE) %>%
  slice_head(n = 15)

message("\n=== Top 15 primary ICD-10 codes (index admissions) ===")
print(top_icd)


# 6. Top ICD-10 diagnoses in non-HSCT hospitalizations -----------------------

top_icd_other <- df_hsct_all %>%
  filter(!is_hsct_index) %>%
  count(diag_code_1, sort = TRUE) %>%
  slice_head(n = 15)

message("\n=== Top 15 primary ICD-10 codes (other hospitalizations) ===")
print(top_icd_other)


# 7. Top CCI intervention codes in index admissions --------------------------

top_cci <- df_hsct_index %>%
  select(starts_with("interv_code_")) %>%
  pivot_longer(everything(), values_to = "code") %>%
  filter(!is.na(code)) %>%
  count(code, sort = TRUE) %>%
  slice_head(n = 15)

message("\n=== Top 15 CCI intervention codes (index admissions) ===")
print(top_cci)


# 8. Length of stay -----------------------------------------------------------

los_summary <- df_hsct_all %>%
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

ggplot(df_hsct_all %>%
         mutate(los  = as.integer(separation_date - admission_date),
                type = if_else(is_hsct_index, "HSCT index", "Other")),
       aes(x = los, fill = type)) +
  geom_histogram(binwidth = 5, position = "dodge", colour = "white") +
  coord_cartesian(xlim = c(0, 100)) +
  labs(title = "Length of stay — HSCT index vs. other admissions",
       x = "Days", y = "Admissions", fill = NULL) +
  theme_minimal()


# 9. Hospital distribution ----------------------------------------------------

message("\n=== Hospitals (index admissions) ===")
print(df_hsct_index %>% count(hospital, sort = TRUE))


# 10. Export ------------------------------------------------------------------
# Uncomment to save results to Excel:
#
# wb <- createWorkbook()
# addWorksheet(wb, "hsct_index");    writeData(wb, "hsct_index",    df_hsct_index)
# addWorksheet(wb, "hsct_all");      writeData(wb, "hsct_all",      df_hsct_all)
# addWorksheet(wb, "patient_summary"); writeData(wb, "patient_summary", patient_summary)
# saveWorkbook(wb, "output/hsct_cohort.xlsx", overwrite = TRUE)
