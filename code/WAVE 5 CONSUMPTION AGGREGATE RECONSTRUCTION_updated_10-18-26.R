# DATE: 4/26/26
# UPDATED: 10/18/26
# Author: Francis Phillip

#
# WAVE 5 CONSUMPTION AGGREGATE RECONSTRUCTION
#
# Based on GHS-Panel Wave 5 Basic Information Document
# Components: Food + Meals + Non-food + Education + Health + Housing
#


# NB: Updated version includes relative paths for easier replication.


library(tidyverse)

cat("=============================================================================\n")
cat("RECONSTRUCTING WAVE 5 CONSUMPTION AGGREGATE\n")
cat("=============================================================================\n\n")


## COMPONENT 1: FOOD CONSUMPTION (7-day recall, annualized) ----

cat("Loading food consumption data...\n")

# Post-planting food (sect6b_plantingw5)
sect6b_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/sect6b_plantingw5.csv")

# Post-harvest food (sect5b_harvestw5)
sect5b_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect5b_harvestw5.csv")

# Food expenditure variable names (check these match your data)
# Post-planting: s6bq8 (total value of food consumed in last 7 days)
# Post-harvest: s5bq8 (total value of food consumed in last 7 days)

# =============================================================================
# QUICK FIX: FOOD WITH OWN-PRODUCTION USING MEDIAN UNIT VALUES
# =============================================================================

cat("Recalculating food with own-production...\n")

# =============================================================================
# CORRECT: CALCULATE UNIT VALUES FROM PURCHASE DATA
# =============================================================================

# STEP 1: Calculate median unit values from purchases (post-planting)
uv_planting <- sect6b_plantingw5 |>
  filter(s6bq6 == "1. YES") |>  # Household purchased this item
  mutate(
    # Quantity purchased × conversion factor = standardized quantity (kg)
    qty_purchased_kg = as.numeric(s6bq7a) * as.numeric(s6bq7_cvn),
    
    # Amount spent on purchase
    value_purchased = as.numeric(s6bq8),
    
    # Calculate unit value = value / standardized quantity (Naira per kg)
    uv = value_purchased / qty_purchased_kg
  ) |>
  # Remove invalid unit values
  filter(!is.na(uv), is.finite(uv), uv > 0, !is.na(qty_purchased_kg), qty_purchased_kg > 0) |>
  group_by(item_cd) |>
  summarise(
    uv_median = median(uv, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

cat("✓ Post-planting unit values calculated\n")
cat("  Items with unit values:", nrow(uv_planting), "\n")
cat("  Total purchase observations:", sum(uv_planting$n_obs), "\n\n")

# STEP 2: Same for post-harvest
uv_harvest <- sect5b_harvestw5 |>
  filter(s5bq6 == "1. YES") |>  # Household purchased
  mutate(
    # Quantity purchased × conversion factor = standardized quantity (kg)
    qty_purchased_kg = as.numeric(s5bq7a) * as.numeric(s5bq7_cvn),
    
    # Amount spent on purchase
    value_purchased = as.numeric(s5bq8),
    
    # Calculate unit value = value / standardized quantity (Naira per kg)
    uv = value_purchased / qty_purchased_kg
  ) |>
  filter(!is.na(uv), is.finite(uv), uv > 0, !is.na(qty_purchased_kg), qty_purchased_kg > 0) |>
  group_by(item_cd) |>
  summarise(
    uv_median = median(uv, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  )

cat("✓ Post-harvest unit values calculated\n")
cat("  Items with unit values:", nrow(uv_harvest), "\n")
cat("  Total purchase observations:", sum(uv_harvest$n_obs), "\n\n")

# STEP 3: Apply to TOTAL consumption (purchase + own-production + gifts)
food_planting <- sect6b_plantingw5 |>
  left_join(uv_planting, by = "item_cd") |>
  mutate(
    # Total quantity consumed × conversion factor = standardized quantity (kg)
    qty_consumed_kg = as.numeric(s6bq2a) * as.numeric(s6bq2_cvn),
    
    # Impute value = total quantity in kg × median unit value (Naira/kg)
    food_value = qty_consumed_kg * uv_median
  ) |>
  group_by(hhid) |>
  summarise(
    food_7day = sum(food_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    food_annual_planting = food_7day * 52.14
  )

food_harvest <- sect5b_harvestw5 |>
  left_join(uv_harvest, by = "item_cd") |>
  mutate(
    # Total quantity consumed × conversion factor = standardized quantity (kg)
    qty_consumed_kg = as.numeric(s5bq2a) * as.numeric(s5bq2_cvn),
    
    # Impute value = total quantity in kg × median unit value (Naira/kg)
    food_value = qty_consumed_kg * uv_median
  ) |>
  group_by(hhid) |>
  summarise(
    food_7day = sum(food_value, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    food_annual_harvest = food_7day * 52.14
  )

# STEP 4: Average across both visits
food_total <- food_planting |>
  full_join(food_harvest, by = "hhid") |>
  mutate(
    food_exp_annual = (food_annual_planting + food_annual_harvest) / 2
  ) |>
  select(hhid, food_exp_annual)

cat("\n✓ Food with own-production calculated\n")
cat("  Mean annual food exp:", scales::comma(round(mean(food_total$food_exp_annual, na.rm = TRUE))), "\n")
cat("  Median annual food exp:", scales::comma(round(median(food_total$food_exp_annual, na.rm = TRUE))), "\n")
cat("  N households:", nrow(food_total), "\n\n")

# =============================================================================
# DIAGNOSTICS
# =============================================================================

# Diagnostic 1: Check conversion factors
cat("\n=== DIAGNOSTIC: Conversion Factors ===\n")
cat("\nPost-planting conversion factors:\n")
sect6b_plantingw5 |>
  filter(!is.na(s6bq7_cvn)) |>
  summarise(
    mean = mean(as.numeric(s6bq7_cvn), na.rm = TRUE),
    median = median(as.numeric(s6bq7_cvn), na.rm = TRUE),
    min = min(as.numeric(s6bq7_cvn), na.rm = TRUE),
    max = max(as.numeric(s6bq7_cvn), na.rm = TRUE),
    n_non_missing = n()
  ) |>
  print()

# Diagnostic 2: Look at unit values
cat("\n\n=== DIAGNOSTIC: Unit Values ===\n")
cat("Post-planting unit values (first 20 items):\n")
uv_planting |>
  arrange(item_cd) |>
  head(20) |>
  mutate(uv_median = scales::comma(uv_median, accuracy = 0.01)) |>
  print()

# Diagnostic 3: Compare to purchase-only version
food_purchase_only <- sect6b_plantingw5 |>
  group_by(hhid) |>
  summarise(food_7day_purchase = sum(as.numeric(s6bq8), na.rm = TRUE)) |>
  mutate(food_annual_purchase = food_7day_purchase * 52.14)

comparison <- tibble(
  measure = c("Purchase only (original)", "With own-production (new)", "Increase"),
  mean_food = c(
    mean(food_purchase_only$food_annual_purchase, na.rm = TRUE),
    mean(food_total$food_exp_annual, na.rm = TRUE),
    NA
  )
) |>
  mutate(
    mean_food = ifelse(is.na(mean_food), 
                       mean_food[2] / mean_food[1] - 1,
                       mean_food)
  )

cat("\n\n=== COMPARISON ===\n")
print(comparison |>
        mutate(mean_food = ifelse(measure == "Increase",
                                  scales::percent(mean_food, accuracy = 0.1),
                                  scales::comma(round(mean_food)))))

cat("\n✓ Food consumption with own-production complete\n")

# =============================================================================
# COMPONENT 2: MEALS AWAY FROM HOME (7-day recall, annualized)
# =============================================================================

cat("Loading meals away from home data...\n")

# Post-planting meals (sect6a_plantingw5)
sect6a_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/sect6a_plantingw5.csv")

# Post-harvest meals (sect5a_harvestw5)
sect5a_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect5a_harvestw5.csv")

# Variable: s6aq2 (planting), s5aq2 (harvest) - value of meals away from home in last 7 days

meals_planting <- sect6a_plantingw5 |>
  group_by(hhid) |>
  summarise(
    meals_exp_7day_planting = sum(s6aq2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    meals_exp_annual_planting = meals_exp_7day_planting * (365 / 7)
  )

meals_harvest <- sect5a_harvestw5 |>
  group_by(hhid) |>
  summarise(
    meals_exp_7day_harvest = sum(s5aq2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    meals_exp_annual_harvest = meals_exp_7day_harvest * (365 / 7)
  )

# Average across both visits
meals_total <- meals_planting |>
  full_join(meals_harvest, by = "hhid") |>
  mutate(
    meals_exp_annual = (meals_exp_annual_planting + meals_exp_annual_harvest) / 2
  ) |>
  select(hhid, meals_exp_annual)

cat("✓ Meals away from home calculated\n")

# =============================================================================
# COMPONENT 3: NON-FOOD EXPENDITURE (mixed recall periods)
# =============================================================================

cat("Loading non-food expenditure data...\n")

# Post-planting non-food (sect7a, sect7b, sect7c_plantingw5)
sect7a_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/sect7a_plantingw5.csv")
sect7b_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/sect7b_plantingw5.csv")
sect7c_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/sect7c_plantingw5.csv")

# Post-harvest non-food (sect6a, sect6b, sect6c_harvestw5)
sect6a_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect6a_harvestw5.csv")
sect6b_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect6b_harvestw5.csv")
sect6c_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect6c_harvestw5.csv")

###
# Convert all expenditure columns to numeric upfront
sect7c_plantingw5 <- sect7c_plantingw5 |>
  mutate(across(starts_with("s7q"), ~as.numeric(.)))

sect6c_harvestw5 <- sect6c_harvestw5 |>
  mutate(across(starts_with("s6q"), ~as.numeric(.)))

# Then proceed with your original code
nonfood_annual_planting <- sect7c_plantingw5 |>
  group_by(hhid) |>
  summarise(
    nonfood_annual_exp_planting = sum(s7q5, na.rm = TRUE) + sum(s7q6, na.rm = TRUE),
    .groups = "drop"
  )

nonfood_annual_harvest <- sect6c_harvestw5 |>
  group_by(hhid) |>
  summarise(
    nonfood_annual_exp_harvest = sum(s6q5, na.rm = TRUE) + sum(s6q6, na.rm = TRUE),
    .groups = "drop"
  )
###

# Sect7a/6a: Weekly items (recall = 7 days)
# Variable: s7q2 (planting), s6q2 (harvest) - expenditure in last 7 days
nonfood_weekly_planting <- sect7a_plantingw5 |>
  group_by(hhid) |>
  summarise(
    nonfood_weekly_exp = sum(s7q2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(nonfood_weekly_annual = nonfood_weekly_exp * (365 / 7))

nonfood_weekly_harvest <- sect6a_harvestw5 |>
  group_by(hhid) |>
  summarise(
    nonfood_weekly_exp = sum(s6q2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(nonfood_weekly_annual = nonfood_weekly_exp * (365 / 7))

# Sect7b/6b: Monthly items (recall = 30 days)
# Variable: s7bq2 (planting), s6bq2 (harvest) - expenditure in last 30 days
nonfood_monthly_planting <- sect7b_plantingw5 |>
  group_by(hhid) |>
  summarise(
    nonfood_monthly_exp = sum(s7q4, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(nonfood_monthly_annual = nonfood_monthly_exp * 12)

nonfood_monthly_harvest <- sect6b_harvestw5 |>
  group_by(hhid) |>
  summarise(
    nonfood_monthly_exp = sum(s6q4, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(nonfood_monthly_annual = nonfood_monthly_exp * 12)

# Sect7c/6c: Annual items (recall = 12 months)
# Variable: s7cq2 (planting), s6cq2 (harvest) - expenditure in last 12 months
# NOTE: Wave 5 asked Q5 and Q6 for last 12 months (see basic info doc)
nonfood_annual_planting <- sect7c_plantingw5 |>
  group_by(hhid) |>
  summarise(
    nonfood_annual_exp_planting = sum(s7q5, na.rm = TRUE) + sum(s7q6, na.rm = TRUE),
    .groups = "drop"
  )

nonfood_annual_harvest <- sect6c_harvestw5 |>
  group_by(hhid) |>
  summarise(
    nonfood_annual_exp_harvest = sum(s6q5, na.rm = TRUE) + sum(s6q6, na.rm = TRUE),
    .groups = "drop"
  )

# Combine all non-food
nonfood_total <- nonfood_weekly_planting |>
  full_join(nonfood_weekly_harvest, by = "hhid", suffix = c("_plant", "_harv")) |>
  full_join(nonfood_monthly_planting, by = "hhid") |>
  full_join(nonfood_monthly_harvest, by = "hhid", suffix = c("_plant", "_harv")) |>
  full_join(nonfood_annual_planting, by = "hhid") |>
  full_join(nonfood_annual_harvest, by = "hhid") |>
  mutate(
    # Average weekly and monthly across visits, use annual as-is (already 12 months)
    nonfood_weekly_avg = (nonfood_weekly_annual_plant + nonfood_weekly_annual_harv) / 2,
    nonfood_monthly_avg = (nonfood_monthly_annual_plant + nonfood_monthly_annual_harv) / 2,
    nonfood_annual_avg = (nonfood_annual_exp_planting + nonfood_annual_exp_harvest) / 2,
    
    # Total non-food
    nonfood_exp_annual = nonfood_weekly_avg + nonfood_monthly_avg + nonfood_annual_avg
  ) |>
  select(hhid, nonfood_exp_annual)

cat("✓ Non-food expenditure calculated\n")

# =============================================================================
# COMPONENT 4: EDUCATION EXPENDITURE (POST-HARVEST ONLY)
# =============================================================================

cat("Loading education expenditure data...\n")

# Education is only in post-harvest (sect2_harvestw5)
sect2_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect2_harvestw5.csv")

# Education expenditure variables (check questionnaire for exact variable names)
# Typical variables: tuition, uniforms, books, transport, etc.
# You'll need to identify the correct column names - likely s2q14*, s2q22*, etc.

###
# Diagnostic:

# Find which columns contain education expenditure values
sect2_harvestw5 |>
  select(hhid, matches("s2q14|s2q22")) |>
  head(10) |>
  print(width = Inf)

# Check data types
sect2_harvestw5 |>
  select(matches("s2q14|s2q22")) |>
  sapply(class) |>
  print()


###

education_total <- sect2_harvestw5 |>
  group_by(hhid) |>
  summarise(
    education_exp_annual = sum(s2q22a, na.rm = TRUE),
    .groups = "drop"
  )

cat("✓ Education expenditure calculated\n")


## COMPONENT 5: HEALTH EXPENDITURE (POST-PLANTING ONLY - sect3) ----

# =============================================================================
# HEALTH EXPENDITURE - WAVE 5
# =============================================================================

cat("\n=== CALCULATING WAVE 5 HEALTH EXPENDITURE ===\n")

# This section asks about health expenditure in the last 12 months
# So it's already annualized!

sect3_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/sect3_plantingw5.csv")
health_w5 <- sect3_plantingw5 |>
  group_by(hhid) |>
  summarise(
    # Sum all health-related expenses
    # s3q12 = outpatient consultation fees (last 12 months)
    # s3q17 = medicine/medical supplies (last 12 months)
    # s3q20 = hospitalization (last 12 months)
    # s3q16 = other health expenses (last 12 months)
    
    health_exp_annual = sum(
      as.numeric(s3q12),  # Consultation
      as.numeric(s3q17,s3q17a),  # Medicine (prescription and non-prescription)
      as.numeric(s3q20),  # Hospitalization
      as.numeric(s3q32),  # Other (bednet)
      na.rm = TRUE
    ),
    .groups = "drop"
  )

health_total <- health_w5

cat("✓ Wave 5 health expenditure calculated\n")
cat("  Mean annual health exp:", scales::comma(round(mean(health_w5$health_exp_annual, na.rm = TRUE))), "\n")
cat("  Median annual health exp:", scales::comma(round(median(health_w5$health_exp_annual, na.rm = TRUE))), "\n")
cat("  N households:", nrow(health_w5), "\n\n")

# Distribution check
cat("Health expenditure distribution:\n")
health_w5 |>
  summarise(
    Mean = mean(health_exp_annual, na.rm = TRUE),
    Median = median(health_exp_annual, na.rm = TRUE),
    P25 = quantile(health_exp_annual, 0.25, na.rm = TRUE),
    P75 = quantile(health_exp_annual, 0.75, na.rm = TRUE),
    P90 = quantile(health_exp_annual, 0.90, na.rm = TRUE),
    `% with zero` = mean(health_exp_annual == 0, na.rm = TRUE) * 100
  ) |>
  mutate(across(Mean:P90, ~scales::comma(round(.))),
         `% with zero` = paste0(round(`% with zero`, 1), "%")) |>
  pivot_longer(everything(), names_to = "Statistic", values_to = "Value") |>
  print()

cat("\n")


# =============================================================================
# COMPONENT 6: HOUSING / IMPUTED RENT (POST-HARVEST ONLY)
# =============================================================================

cat("Loading housing/rent data...\n")

# Housing is in post-harvest (sect9_harvestw5)
sect9_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect9_harvestw5.csv")

housing_total <- sect9_harvestw5 |>
  mutate(
    rent_amount = as.numeric(s9q5a),
    
    # If frequency is text like "MONTHLY", "ANNUAL", etc.
    housing_exp_annual = case_when(
      str_detect(tolower(s9q5b), "PER YEAR") ~ rent_amount * 1,
      str_detect(tolower(s9q5b), "PER MONTH") ~ rent_amount * 12,
      is.na(rent_amount) ~ 0,
      TRUE ~ rent_amount * 12  # Default to monthly
    )
  ) |>
  group_by(hhid) |>
  summarise(
    housing_exp_annual = sum(housing_exp_annual, na.rm = TRUE),
    .groups = "drop"
  )

cat("✓ Housing/rent expenditure calculated\n")

# =============================================================================
# COMBINE ALL COMPONENTS
# =============================================================================

cat("\nCombining all consumption components...\n")

cons_agg_wave5 <- food_total |>
  full_join(meals_total, by = "hhid") |>
  full_join(nonfood_total, by = "hhid") |>
  full_join(education_total, by = "hhid") |>
  full_join(health_total, by = "hhid") |>
  full_join(housing_total, by = "hhid") |>
  mutate(
    across(where(is.numeric), ~replace_na(., 0))
  ) |>
  mutate(
    # Total consumption aggregate
    totcons = food_exp_annual + meals_exp_annual + nonfood_exp_annual + 
      education_exp_annual + health_exp_annual + housing_exp_annual
  )

# Add household size for per-capita calculation
secta_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/secta_harvestw5.csv")

sect1_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect1_harvestw5.csv")

hhsize_w5 <- sect1_harvestw5 |>
  group_by(hhid) |>
  summarise(hhsize = n(), .groups = "drop")

cons_agg_wave5 <- cons_agg_wave5 |>
  left_join(hhsize_w5, by = "hhid") |>
  mutate(
    totcons_pc = totcons / hhsize
  )

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================

cat("\n=============================================================================\n")
cat("WAVE 5 CONSUMPTION AGGREGATE SUMMARY\n")
cat("=============================================================================\n\n")

summary_stats <- cons_agg_wave5 |>
  summarise(
    n_households = n(),
    mean_totcons = mean(totcons, na.rm = TRUE),
    median_totcons = median(totcons, na.rm = TRUE),
    mean_totcons_pc = mean(totcons_pc, na.rm = TRUE),
    median_totcons_pc = median(totcons_pc, na.rm = TRUE),
    mean_food_share = mean(food_exp_annual / totcons, na.rm = TRUE),
    mean_nonfood_share = mean(nonfood_exp_annual / totcons, na.rm = TRUE)
  )

print(summary_stats)

cat("\nComponent breakdown:\n")
cons_agg_wave5 |>
  summarise(
    across(ends_with("_annual"), ~mean(., na.rm = TRUE), .names = "mean_{.col}")
  ) |>
  pivot_longer(everything(), names_to = "component", values_to = "mean_value") |>
  mutate(
    component = str_remove(component, "mean_"),
    mean_value = scales::comma(round(mean_value, 0))
  ) |>
  print()

# =============================================================================
# SAVE OUTPUT
# =============================================================================

write_csv(cons_agg_wave5, "cons_agg_wave5_reconstructed_5-5-26.csv")
cat("\n✓ Consumption aggregate saved to: cons_agg_wave5_reconstructed_5-5-26.csv\n")

cat("\n=============================================================================\n")
cat("WAVE 5 CONSUMPTION AGGREGATE RECONSTRUCTION COMPLETE\n")
cat("=============================================================================\n\n")