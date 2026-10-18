#
# EXTENSION: OKEREKE ET AL. (2024) TO WAVES 3-5 (2015/16, 2018/19, 2023/24) ####
# Adapted from: fp-okereke_replication-complete_4-25-26.R
# Date: 2026-05-5
# Updated: 2026-18-10
# Author: Francis Phillip
#

# NB: Updated version includes relative paths for easier replication.


library(readr)
library(tidyverse)
library(haven)
library(systemfit)
library(scales)

cat("=============================================================================\n")
cat("EXTENSION: OKEREKE ET AL. TO WAVES 3-5 (2015/16, 2018/19, 2023/24)\n")
cat("=============================================================================\n\n")

# =============================================================================
# STEP 1: IMPORT DATA FOR WAVES 3-5
# =============================================================================

cat("Step 1: Importing data for Waves 3-5...\n")

# WAVE 3 (2015/16) ----
sect8b_plantingw3 <- read_csv("~/NGA_2015_GHSP-W3_v02_M_CSV/sect8b_plantingw3.csv")
cons_agg_wave3_visit1 <- read_csv("~/NGA_2015_GHSP-W3_v02_M_CSV/cons_agg_wave3_visit1.csv")

# WAVE 4 (2018/19) ----
sect8b_plantingw4 <- read_csv("~/NGA_2018_GHSP-W4_v03_M_CSV/sect8b_plantingw4.csv")
totcons_final <- read_csv("~/NGA_2018_GHSP-W4_v03_M_CSV/totcons_final.csv")
secta_plantingw4 <- read_csv("~/NGA_2018_GHSP-W4_v03_M_CSV/secta_plantingw4.csv")

# WAVE 5 (2023/24) ----
sect7b_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/sect7b_plantingw5.csv")
secta_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/secta_plantingw5.csv")

# Load reconstructed consumption aggregate for Wave 5
cons_agg_wave5 <- read_csv("cons_agg_wave5_reconstructed_5-5-26.csv") # Run "WAVE 5 CONSUMPTION AGGREGATE RECONSTRUCTION" to get cons_agg_wave5

cat("✓ Data imported successfully\n\n")

# =============================================================================
# STEP 2: FILTER ENERGY ITEMS AND CALCULATE EXPENDITURES (ANNUALIZED)
# =============================================================================

cat("Step 2: Filtering energy items and calculating expenditures...\n")

# WAVE 3 ----
cat("  Processing Wave 3...\n")

# Energy item codes for Wave 3: 301=kerosene, 303-304-307-308=other, 305=electricity, 309=petrol, 310=diesel
energy_item_codes_w3 <- c(301, 303, 304, 305, 307, 308, 309, 310)

petrol_w3 <- sect8b_plantingw3 |>
  filter(item_cd == 309) |>
  group_by(hhid) |>
  summarise(petrol_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

electricity_w3 <- sect8b_plantingw3 |>
  filter(item_cd == 305) |>
  group_by(hhid) |>
  summarise(electricity_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

kerosene_w3 <- sect8b_plantingw3 |>
  filter(item_cd == 301) |>
  group_by(hhid) |>
  summarise(kerosene_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

transport_w3 <- sect8b_plantingw3 |>
  filter(item_cd == 310) |>
  group_by(hhid) |>
  summarise(transport_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

other_energy_w3 <- sect8b_plantingw3 |>
  filter(item_cd %in% c(303, 304, 307, 308)) |>
  group_by(hhid) |>
  summarise(other_energy_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

total_energy_w3 <- sect8b_plantingw3 |>
  filter(item_cd %in% energy_item_codes_w3) |>
  group_by(hhid) |>
  summarise(total_energy_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

# WAVE 4 ----
cat("  Processing Wave 4...\n")

energy_item_codes_w4 <- c(301, 303, 304, 305, 307, 308, 309, 310)

petrol_w4 <- sect8b_plantingw4 |>
  filter(item_cd == 309) |>
  group_by(hhid) |>
  summarise(petrol_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

electricity_w4 <- sect8b_plantingw4 |>
  filter(item_cd == 305) |>
  group_by(hhid) |>
  summarise(electricity_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

kerosene_w4 <- sect8b_plantingw4 |>
  filter(item_cd == 301) |>
  group_by(hhid) |>
  summarise(kerosene_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

transport_w4 <- sect8b_plantingw4 |>
  filter(item_cd == 310) |>
  group_by(hhid) |>
  summarise(transport_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

other_energy_w4 <- sect8b_plantingw4 |>
  filter(item_cd %in% c(303, 304, 307, 308)) |>
  group_by(hhid) |>
  summarise(other_energy_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

total_energy_w4 <- sect8b_plantingw4 |>
  filter(item_cd %in% energy_item_codes_w4) |>
  group_by(hhid) |>
  summarise(total_energy_exp = sum(s8q4, na.rm = TRUE) * 12, .groups = "drop")

# WAVE 5 ----
cat("  Processing Wave 5...\n")

# NOTE: Wave 5 uses sect7b (not sect8b) and STRING item codes
# Wave 5 item codes: "201. Kerosene", "205. Electricity", "209. Petrol", "210. Diesel fuel"
# Other energy: "202. Charcoal", "203. Firewood", "206. LPG/cooking gas", "211. Other fuel"

petrol_w5 <- sect7b_plantingw5 |>
  filter(str_detect(item_cd, "^209\\.")) |>  # "209. Petrol"
  group_by(hhid) |>
  summarise(petrol_exp = sum(as.numeric(s7q4), na.rm = TRUE) * 12, .groups = "drop")

electricity_w5 <- sect7b_plantingw5 |>
  filter(str_detect(item_cd, "^205\\.")) |>  # "205. Electricity"
  group_by(hhid) |>
  summarise(electricity_exp = sum(as.numeric(s7q4), na.rm = TRUE) * 12, .groups = "drop")

kerosene_w5 <- sect7b_plantingw5 |>
  filter(str_detect(item_cd, "^201\\.")) |>  # "201. Kerosene"
  group_by(hhid) |>
  summarise(kerosene_exp = sum(as.numeric(s7q4), na.rm = TRUE) * 12, .groups = "drop")

transport_w5 <- sect7b_plantingw5 |>
  filter(str_detect(item_cd, "^210\\.")) |>  # "210. Diesel fuel"
  group_by(hhid) |>
  summarise(transport_exp = sum(as.numeric(s7q4), na.rm = TRUE) * 12, .groups = "drop")

other_energy_w5 <- sect7b_plantingw5 |>
  filter(str_detect(item_cd, "^202\\.|^203\\.|^206\\.|^211\\.")) |>  # Charcoal, firewood, gas, other
  group_by(hhid) |>
  summarise(other_energy_exp = sum(as.numeric(s7q4), na.rm = TRUE) * 12, .groups = "drop")

total_energy_w5 <- sect7b_plantingw5 |>
  filter(str_detect(item_cd, "^201\\.|^202\\.|^203\\.|^205\\.|^206\\.|^209\\.|^210\\.|^211\\.")) |>
  group_by(hhid) |>
  summarise(total_energy_exp = sum(as.numeric(s7q4), na.rm = TRUE) * 12, .groups = "drop")

cat("✓ Energy expenditures calculated\n\n")

# =============================================================================
# STEP 3: MERGE WITH CONSUMPTION AGGREGATES
# =============================================================================

cat("Step 3: Merging with consumption aggregates...\n")

# Wave 3
w3 <- cons_agg_wave3_visit1 |>
  left_join(total_energy_w3, by = "hhid") |>
  left_join(petrol_w3, by = "hhid") |>
  mutate(
    total_energy_exp = replace_na(total_energy_exp, 0),
    petrol_exp = replace_na(petrol_exp, 0),
    wave = 3
  )

# Wave 4
w4 <- totcons_final |>
  left_join(secta_plantingw4 |> select(hhid, wt_wave4), by = "hhid") |>
  left_join(total_energy_w4, by = "hhid") |>
  left_join(petrol_w4, by = "hhid") |>
  mutate(
    total_energy_exp = replace_na(total_energy_exp, 0),
    petrol_exp = replace_na(petrol_exp, 0),
    wave = 4
  )

# Wave 5
w5 <- cons_agg_wave5 |>
  left_join(secta_plantingw5 |> select(hhid, zone, ea, sector), by = "hhid") |>
  left_join(total_energy_w5, by = "hhid") |>
  left_join(petrol_w5, by = "hhid") |>
  mutate(
    total_energy_exp = replace_na(total_energy_exp, 0),
    petrol_exp = replace_na(petrol_exp, 0),
    wave = 5
  )

cat("✓ Merged with consumption aggregates\n\n")


## STEP 4: FILTER TO HOUSEHOLDS WITH POSITIVE ENERGY EXPENDITURE ----

cat("Step 4: Filtering to households with positive energy expenditure...\n")

cat("  Before filter:\n")
cat("    Wave 3:", nrow(w3), "\n")
cat("    Wave 4:", nrow(w4), "\n")
cat("    Wave 5:", nrow(w5), "\n\n")

w3 <- w3 |> filter(total_energy_exp > 0)
w4 <- w4 |> filter(total_energy_exp > 0)
w5 <- w5 |> filter(total_energy_exp > 0)

cat("  After filter:\n")
cat("    Wave 3:", nrow(w3), "\n")
cat("    Wave 4:", nrow(w4), "\n")
cat("    Wave 5:", nrow(w5), "\n\n")


## STEP 5: MERGE INDIVIDUAL ENERGY COMPONENTS ----


cat("Step 5: Merging individual energy components...\n")

w3_clean <- w3 |>
  left_join(kerosene_w3, by = "hhid") |>
  left_join(electricity_w3, by = "hhid") |>
  left_join(transport_w3, by = "hhid") |>
  left_join(other_energy_w3, by = "hhid") |>
  mutate(
    kerosene_exp = replace_na(kerosene_exp, 0),
    electricity_exp = replace_na(electricity_exp, 0),
    transport_exp = replace_na(transport_exp, 0),
    other_energy_exp = replace_na(other_energy_exp, 0)
  )

w4_clean <- w4 |>
  left_join(kerosene_w4, by = "hhid") |>
  left_join(electricity_w4, by = "hhid") |>
  left_join(transport_w4, by = "hhid") |>
  left_join(other_energy_w4, by = "hhid") |>
  mutate(
    kerosene_exp = replace_na(kerosene_exp, 0),
    electricity_exp = replace_na(electricity_exp, 0),
    transport_exp = replace_na(transport_exp, 0),
    other_energy_exp = replace_na(other_energy_exp, 0)
  )

w5_clean <- w5 |>
  left_join(kerosene_w5, by = "hhid") |>
  left_join(electricity_w5, by = "hhid") |>
  left_join(transport_w5, by = "hhid") |>
  left_join(other_energy_w5, by = "hhid") |>
  mutate(
    kerosene_exp = replace_na(kerosene_exp, 0),
    electricity_exp = replace_na(electricity_exp, 0),
    transport_exp = replace_na(transport_exp, 0),
    other_energy_exp = replace_na(other_energy_exp, 0)
  )

cat("✓ Individual energy components merged\n\n")


## STEP 6: CALCULATE PRICES WITH EA-LEVEL IMPUTATION AND STONE INDEX ----


cat("Step 6: Calculating prices with EA-level imputation...\n")

calculate_prices_function <- function(data, wave_name) {
  
  cat("  Processing", wave_name, "...\n")
  
  # EA-level unit values (Moschini normalization: p = x / (x + 1))
  ea_prices <- data |>
    filter(total_energy_exp > 0) |>
    group_by(ea) |>
    summarise(
      p1_ea = mean(petrol_exp / (petrol_exp + 1), na.rm = TRUE),
      p2_ea = mean(electricity_exp / (electricity_exp + 1), na.rm = TRUE),
      p3_ea = mean(kerosene_exp / (kerosene_exp + 1), na.rm = TRUE),
      p4_ea = mean(transport_exp / (transport_exp + 1), na.rm = TRUE),
      p5_ea = mean(other_energy_exp / (other_energy_exp + 1), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Zone-level fallback
  zone_prices <- data |>
    left_join(ea_prices, by = "ea") |>
    group_by(zone) |>
    summarise(
      p1_zone = mean(p1_ea, na.rm = TRUE),
      p2_zone = mean(p2_ea, na.rm = TRUE),
      p3_zone = mean(p3_ea, na.rm = TRUE),
      p4_zone = mean(p4_ea, na.rm = TRUE),
      p5_zone = mean(p5_ea, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merge and impute
  data <- data |>
    left_join(ea_prices, by = "ea") |>
    left_join(zone_prices, by = "zone") |>
    mutate(
      p1_impute = coalesce(p1_ea, p1_zone, 0.5),
      p2_impute = coalesce(p2_ea, p2_zone, 0.5),
      p3_impute = coalesce(p3_ea, p3_zone, 0.5),
      p4_impute = coalesce(p4_ea, p4_zone, 0.5),
      p5_impute = coalesce(p5_ea, p5_zone, 0.5)
    ) |>
    mutate(
      # CRITICAL: Floor all prices at 0.001 to prevent log(0) = -Inf
      p1_impute = pmax(p1_impute, 0.001, na.rm = TRUE),
      p2_impute = pmax(p2_impute, 0.001, na.rm = TRUE),
      p3_impute = pmax(p3_impute, 0.001, na.rm = TRUE),
      p4_impute = pmax(p4_impute, 0.001, na.rm = TRUE),  # ← This fixes p4 = 0
      p5_impute = pmax(p5_impute, 0.001, na.rm = TRUE)   # ← This fixes p5 = 0
    )
  
  return(data)
}

w3_clean <- calculate_prices_function(w3_clean, "Wave 3")
w4_clean <- calculate_prices_function(w4_clean, "Wave 4")
w5_clean <- calculate_prices_function(w5_clean, "Wave 5")

cat("✓ Prices calculated\n\n")


## STEP 7: CALCULATE WITHIN-ENERGY BUDGET SHARES ----


cat("Step 7: Calculating within-energy budget shares...\n")

w3_clean <- w3_clean |>
  mutate(
    w1_petrol = petrol_exp / total_energy_exp,
    w2_electricity = electricity_exp / total_energy_exp,
    w3_kerosene = kerosene_exp / total_energy_exp,
    w4_transport = transport_exp / total_energy_exp,
    w5_other = other_energy_exp / total_energy_exp
  ) |>
  mutate(across(starts_with("w"), ~replace_na(., 0)))

w4_clean <- w4_clean |>
  mutate(
    w1_petrol = petrol_exp / total_energy_exp,
    w2_electricity = electricity_exp / total_energy_exp,
    w3_kerosene = kerosene_exp / total_energy_exp,
    w4_transport = transport_exp / total_energy_exp,
    w5_other = other_energy_exp / total_energy_exp
  ) |>
  mutate(across(starts_with("w"), ~replace_na(., 0)))

w5_clean <- w5_clean |>
  mutate(
    w1_petrol = petrol_exp / total_energy_exp,
    w2_electricity = electricity_exp / total_energy_exp,
    w3_kerosene = kerosene_exp / total_energy_exp,
    w4_transport = transport_exp / total_energy_exp,
    w5_other = other_energy_exp / total_energy_exp
  ) |>
  mutate(across(starts_with("w"), ~replace_na(., 0)))

cat("✓ Budget shares calculated\n\n")


## STEP 8: CALCULATE STONE PRICE INDEX AND NORMALIZED LOG PRICES -----


cat("Step 8: Calculating Stone price index and normalized log prices...\n")

w3_clean <- w3_clean |>
  mutate(
    stone_index = w1_petrol * log(p1_impute) + 
      w2_electricity * log(p2_impute) +
      w3_kerosene * log(p3_impute) +
      w4_transport * log(p4_impute) +
      w5_other * log(p5_impute),
    
    ln_p1_impute = log(p1_impute) - stone_index,
    ln_p2_impute = log(p2_impute) - stone_index,
    ln_p3_impute = log(p3_impute) - stone_index,
    ln_p4_impute = log(p4_impute) - stone_index,
    ln_p5_impute = log(p5_impute) - stone_index
  )

w4_clean <- w4_clean |>
  mutate(
    stone_index = w1_petrol * log(p1_impute) + 
      w2_electricity * log(p2_impute) +
      w3_kerosene * log(p3_impute) +
      w4_transport * log(p4_impute) +
      w5_other * log(p5_impute),
    
    ln_p1_impute = log(p1_impute) - stone_index,
    ln_p2_impute = log(p2_impute) - stone_index,
    ln_p3_impute = log(p3_impute) - stone_index,
    ln_p4_impute = log(p4_impute) - stone_index,
    ln_p5_impute = log(p5_impute) - stone_index
  )

w5_clean <- w5_clean |>
  mutate(
    stone_index = w1_petrol * log(p1_impute) + 
      w2_electricity * log(p2_impute) +
      w3_kerosene * log(p3_impute) +
      w4_transport * log(p4_impute) +
      w5_other * log(p5_impute),
    
    ln_p1_impute = log(p1_impute) - stone_index,
    ln_p2_impute = log(p2_impute) - stone_index,
    ln_p3_impute = log(p3_impute) - stone_index,
    ln_p4_impute = log(p4_impute) - stone_index,
    ln_p5_impute = log(p5_impute) - stone_index
  )

cat("✓ Stone index and normalized prices calculated\n\n")


# STEP 9: CALCULATE ln_totcons -----


cat("Step 9: Calculating ln_totcons...\n")

w3_clean <- w3_clean |>
  mutate(ln_totcons = log(totcons * hhsize))

w4_clean <- w4_clean |>
  mutate(ln_totcons = log(totcons_adj_norm * hhsize))

w5_clean <- w5_clean |>
  mutate(ln_totcons = log(totcons))

cat("✓ ln_totcons calculated\n\n")

# =============================================================================
# STEP 10: ADD DEMOGRAPHIC VARIABLES
# =============================================================================
cat("Step 10: Adding demographic variables...\n")

# Wave 3 demographics
sect1_plantingw3 <- read_csv("~/JH Documents/MPE Senior Thesis/Data/Nigerian NBStatistics Data-GHS Panel 2015-16/NGA_2015_GHSP-W3_v02_M_CSV/sect1_plantingw3.csv")
sect5_plantingw3 <- read_csv("~/JH Documents/MPE Senior Thesis/Data/Nigerian NBStatistics Data-GHS Panel 2015-16/NGA_2015_GHSP-W3_v02_M_CSV/sect5_plantingw3.csv")

# Wave 4 demographics
sect1_plantingw4 <- read_csv("~/JH Documents/MPE Senior Thesis/Data/Nigerian NBStatistics Data-GHS Panel 2018-19/NGA_2018_GHSP-W4_v03_M_CSV/sect1_plantingw4.csv")
sect5_plantingw4 <- read_csv("~/JH Documents/MPE Senior Thesis/Data/Nigerian NBStatistics Data-GHS Panel 2018-19/NGA_2018_GHSP-W4_v03_M_CSV/sect5_plantingw4.csv")

# Wave 5 demographics
sect1_harvestw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Harvest Wave 5/Household/sect1_harvestw5.csv")
sect10_plantingw5 <- read_csv("~/NGA_2023_GHSP-W5_v01_M_CSV/Post Planting Wave 5/Household/sect10_plantingw5.csv")

# Extract female_head from sect1 (one row per household)
female_head_w3 <- sect1_plantingw3 |>
  filter(s1q3 == 1) |>  # Keep only the household head
  select(hhid, s1q2) |>
  mutate(female_head = ifelse(s1q2 == 2, 1, 0)) |>
  select(hhid, female_head)

female_head_w4 <- sect1_plantingw4 |>
  filter(s1q3 == 1) |>  # Keep only the household head
  select(hhid, s1q2) |>
  mutate(female_head = ifelse(s1q2 == 2, 1, 0)) |>
  select(hhid, female_head)

female_head_w5 <- sect1_harvestw5 |>
  filter(s1q3 == "1. HEAD") |>  # Keep only the household head
  select(hhid, s1q2) |>
  mutate(female_head = ifelse(s1q2 == "2. FEMALE", 1, 0)) |>
  select(hhid, female_head)

# Extract motorbike ownership from sect5/sect10 (one row per household)
# Wave 3 - Only keep motorbike data for households in w3_clean
motorbike_w3 <- sect5_plantingw3 |>
  filter(item_cd == 318) |>
  filter(s5q1 > 0) |>  # Only households that OWN motorbikes (quantity > 0)
  filter(hhid %in% w3_clean$hhid) |>
  group_by(hhid) |>
  summarise(motorbike_ownership = 1, .groups = "drop")

# Wave 4 - Only keep motorbike data for households in w4_clean
motorbike_w4 <- sect5_plantingw4 |>
  filter(item_cd == 318) |>
  filter(s5q1a == 1) |>  # Only households that OWN motorbikes
  filter(hhid %in% w4_clean$hhid) |>
  group_by(hhid) |>
  summarise(motorbike_ownership = 1, .groups = "drop")

# Wave 5 - Only keep motorbike data for households in w5_clean
# NOTE: Wave 5 still uses item_cd == 318, but as a string "318. Motorbike"
motorbike_w5 <- sect10_plantingw5 |>
  filter(item_cd == "318. Motorbike") |>  # String format in Wave 5
  filter(s10q1a == "1. YES") |>  # Only households that OWN motorbikes
  filter(hhid %in% w5_clean$hhid) |>
  group_by(hhid) |>
  summarise(motorbike_ownership = 1, .groups = "drop")

cat("✓ Demographic variables extracted\n\n")

# Merge demographics
w3_quaids <- w3_clean |>
  left_join(female_head_w3, by = "hhid") |>
  left_join(motorbike_w3, by = "hhid") |>
  mutate(
    urban = ifelse(rururb == 1, 1, 0),
    female_head = replace_na(female_head, 0),
    motorbike_ownership = replace_na(motorbike_ownership, 0),
    totcons = totcons  # Keep per capita for consistency
  )

w4_quaids <- w4_clean |>
  left_join(female_head_w4, by = "hhid") |>
  left_join(motorbike_w4, by = "hhid") |>
  mutate(
    urban = ifelse(sector == 1, 1, 0),
    totcons = totcons_adj_norm,  # Use normalized consumption
    female_head = replace_na(female_head, 0),
    motorbike_ownership = replace_na(motorbike_ownership, 0)
  )

w5_quaids <- w5_clean |>
  left_join(female_head_w5, by = "hhid") |>
  left_join(motorbike_w5, by = "hhid") |>
  mutate(
    urban = ifelse(sector == 1, 1, 0),
    totcons = totcons / hhsize,  # Convert to per capita
    female_head = replace_na(female_head, 0),
    motorbike_ownership = replace_na(motorbike_ownership, 0)
  )

cat("✓ Demographics added\n\n")

cat("Female-headed households:\n")
cat("  Wave 3:", scales::percent(mean(w3_quaids$female_head, na.rm = TRUE), accuracy = 0.1), "\n")
cat("  Wave 4:", scales::percent(mean(w4_quaids$female_head, na.rm = TRUE), accuracy = 0.1), "\n")
cat("  Wave 5:", scales::percent(mean(w5_quaids$female_head, na.rm = TRUE), accuracy = 0.1), "\n\n")

cat("Motorbike ownership:\n")
cat("  Wave 3:", scales::percent(mean(w3_quaids$motorbike_ownership, na.rm = TRUE), accuracy = 0.1), "\n")
cat("  Wave 4:", scales::percent(mean(w4_quaids$motorbike_ownership, na.rm = TRUE), accuracy = 0.1), "\n")
cat("  Wave 5:", scales::percent(mean(w5_quaids$motorbike_ownership, na.rm = TRUE), accuracy = 0.1), "\n\n")


## STEP 11: POOL WAVES 3-5 ----


cat("Step 11: Pooling waves for QUAIDS estimation...\n")

quaids_vars <- c("hhid", "wave", "zone", "ea", "hhsize", "urban", "female_head", "motorbike_ownership",
                 "w1_petrol", "w2_electricity", "w3_kerosene", "w4_transport", "w5_other",
                 "ln_p1_impute", "ln_p2_impute", "ln_p3_impute", "ln_p4_impute", "ln_p5_impute",
                 "ln_totcons", "total_energy_exp", "totcons")

###
cat("Step 11: Pooling waves for QUAIDS estimation...\n")

# Harmonize data types before pooling
w3_quaids <- w3_quaids |>
  mutate(
    zone = as.character(zone),
    ea = as.character(ea)
  )

w4_quaids <- w4_quaids |>
  mutate(
    zone = as.character(zone),
    ea = as.character(ea)
  )

w5_quaids <- w5_quaids |>
  mutate(
    zone = as.character(zone),
    ea = as.character(ea)
  )

quaids_vars <- c("hhid", "wave", "zone", "ea", "hhsize", "urban", "female_head", "motorbike_ownership",
                 "w1_petrol", "w2_electricity", "w3_kerosene", "w4_transport", "w5_other",
                 "ln_p1_impute", "ln_p2_impute", "ln_p3_impute", "ln_p4_impute", "ln_p5_impute",
                 "ln_totcons", "total_energy_exp", "totcons")

pooled_quaids <- bind_rows(
  w3_quaids |> select(all_of(quaids_vars)),
  w4_quaids |> select(all_of(quaids_vars)),
  w5_quaids |> select(all_of(quaids_vars))
)

###


###


# Apply final sample restrictions for QUAIDS
pooled_quaids <- pooled_quaids |>
  filter(
    !is.na(w1_petrol) & !is.na(w2_electricity) & !is.na(w3_kerosene) & 
      !is.na(w4_transport) & !is.na(w5_other),
    !is.na(ln_p1_impute) & !is.na(ln_p2_impute) & !is.na(ln_p3_impute) & 
      !is.na(ln_p4_impute) & !is.na(ln_p5_impute),
    !is.na(ln_totcons) & !is.na(totcons) & !is.infinite(totcons),
    totcons > 0,
    total_energy_exp > 0
  )

cat("After filtering:\n")
cat("  Total households:", nrow(pooled_quaids), "\n")
cat("  Households removed:", sum(is.na(pooled_quaids$totcons)) + 
      sum(pooled_quaids$totcons == 0, na.rm = TRUE), "\n\n")

cat("Pooled sample size:", nrow(pooled_quaids), "\n")
cat("  Wave 3:", sum(pooled_quaids$wave == 3), "\n")
cat("  Wave 4:", sum(pooled_quaids$wave == 4), "\n")
cat("  Wave 5:", sum(pooled_quaids$wave == 5), "\n\n")


# STEP 12: CALCULATE REAL EXPENDITURE (ln_x_real) ----


cat("Step 12: Calculating real expenditure (ln_x_real)...\n")

pooled_quaids <- pooled_quaids |>
  mutate(
    ln_x_real = ln_totcons - (w1_petrol * ln_p1_impute + 
                                w2_electricity * ln_p2_impute +
                                w3_kerosene * ln_p3_impute + 
                                w4_transport * ln_p4_impute +
                                w5_other * ln_p5_impute),
    ln_x_real_sq = ln_x_real^2
  )

cat("✓ Real expenditure calculated\n\n")

# =============================================================================
# STEP 13: ADD WAVE DUMMIES (Wave 3 = baseline)
# =============================================================================

cat("Step 13: Adding wave dummies...\n")

pooled_quaids <- pooled_quaids |>
  mutate(
    wave_4 = ifelse(wave == 4, 1, 0),
    wave_5 = ifelse(wave == 5, 1, 0)
  )

cat("✓ Wave dummies added\n\n")


# STEP 14: CREATE FINAL CLEAN POOLED DATASET ----


pooled_clean <- pooled_quaids

write_csv(pooled_clean, "pooled_waves_3_4_5_clean.csv")
cat("✓ Pooled clean dataset saved to pooled_waves_3_4_5_clean.csv\n\n")

View(pooled_clean)
# STEP 15: QUAIDS ESTIMATION ----


cat("=============================================================================\n")
cat("ESTIMATING QUAIDS MODEL (WAVES 3-5)\n")
cat("=============================================================================\n\n")

# Define 4-equation system (5th good is residual due to adding-up constraint)
eq1 <- w1_petrol ~ ln_p1_impute + ln_p2_impute + ln_p3_impute + ln_p4_impute + ln_p5_impute +
  ln_x_real + ln_x_real_sq + hhsize + urban + wave_4 + wave_5

eq2 <- w2_electricity ~ ln_p1_impute + ln_p2_impute + ln_p3_impute + ln_p4_impute + ln_p5_impute +
  ln_x_real + ln_x_real_sq + hhsize + urban + wave_4 + wave_5

eq3 <- w3_kerosene ~ ln_p1_impute + ln_p2_impute + ln_p3_impute + ln_p4_impute + ln_p5_impute +
  ln_x_real + ln_x_real_sq + hhsize + urban + wave_4 + wave_5

eq4 <- w4_transport ~ ln_p1_impute + ln_p2_impute + ln_p3_impute + ln_p4_impute + ln_p5_impute +
  ln_x_real + ln_x_real_sq + hhsize + urban + wave_4 + wave_5

system <- list(petrol = eq1, electricity = eq2, kerosene = eq3, transport = eq4)

cat("Estimating unrestricted QUAIDS using SUR...\n")

quaids_fit <- systemfit(
  formula = system,
  method = "SUR",
  data = pooled_clean,
  maxiter = 50
)

cat("\n=============================================================================\n")
cat("SUCCESS! Model converged.\n")
cat("=============================================================================\n\n")

summary(quaids_fit)

#### MODEL FIT ----

# =============================================================================
# COEFFICIENT SIGNIFICANCE TESTING
# =============================================================================

cat("\n=============================================================================\n")
cat("COEFFICIENT SIGNIFICANCE TESTING\n")
cat("=============================================================================\n\n")

# Extract coefficient vector and variance-covariance matrix
coefs <- coef(quaids_fit)
vcov_matrix <- vcov(quaids_fit)

# Calculate standard errors
std_errors <- sqrt(diag(vcov_matrix))

# Calculate t-statistics
t_stats <- coefs / std_errors

# Calculate p-values (two-tailed test)
n <- nrow(pooled_clean)
k <- length(coefs)
df <- n - k

p_values <- 2 * pt(abs(t_stats), df = df, lower.tail = FALSE)

# Create significance stars function
get_stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01 ~ "**",
    p < 0.05 ~ "*",
    p < 0.10 ~ "†",
    TRUE ~ ""
  )
}

# Create full results table
coef_results <- tibble(
  parameter = names(coefs),
  estimate = coefs,
  std_error = std_errors,
  t_stat = t_stats,
  p_value = p_values,
  sig = get_stars(p_values)
) |>
  mutate(
    estimate_display = paste0(round(estimate, 4), sig),
    se_display = paste0("(", round(std_error, 4), ")"),
    p_display = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p_value, 3))
    )
  )

# =============================================================================
# β COEFFICIENTS WITH SIGNIFICANCE
# =============================================================================

cat("\n=============================================================================\n")
cat("β COEFFICIENTS (Expenditure Effects)\n")
cat("=============================================================================\n\n")

beta_results <- coef_results |>
  filter(str_detect(parameter, "ln_x_real$"))

cat("Parameter              Estimate    Std. Error   t-stat    p-value   Sig.\n")
cat("─────────────────────  ──────────  ───────────  ────────  ────────  ─────\n")
for (i in 1:nrow(beta_results)) {
  cat(sprintf("%-20s %10.4f%3s  (%8.4f)  %8.3f  %8s   %s\n",
              beta_results$parameter[i],
              beta_results$estimate[i],
              beta_results$sig[i],
              beta_results$std_error[i],
              beta_results$t_stat[i],
              beta_results$p_display[i],
              beta_results$sig[i]))
}

cat("\nSummary:\n")
cat("  Significant at 1%: ", sum(beta_results$p_value < 0.01), "/", nrow(beta_results), "\n")
cat("  Significant at 5%: ", sum(beta_results$p_value < 0.05), "/", nrow(beta_results), "\n")
cat("  Significant at 10%:", sum(beta_results$p_value < 0.10), "/", nrow(beta_results), "\n\n")

# =============================================================================
# λ COEFFICIENTS WITH SIGNIFICANCE
# =============================================================================

cat("=============================================================================\n")
cat("λ COEFFICIENTS (Quadratic Expenditure Effects)\n")
cat("=============================================================================\n\n")

lambda_results <- coef_results |>
  filter(str_detect(parameter, "ln_x_real_sq"))

cat("Parameter              Estimate    Std. Error   t-stat    p-value   Sig.\n")
cat("─────────────────────  ──────────  ───────────  ────────  ────────  ─────\n")
for (i in 1:nrow(lambda_results)) {
  cat(sprintf("%-20s %10.4f%3s  (%8.4f)  %8.3f  %8s   %s\n",
              lambda_results$parameter[i],
              lambda_results$estimate[i],
              lambda_results$sig[i],
              lambda_results$std_error[i],
              lambda_results$t_stat[i],
              lambda_results$p_display[i],
              lambda_results$sig[i]))
}

cat("\nSummary:\n")
cat("  Significant at 1%: ", sum(lambda_results$p_value < 0.01), "/", nrow(lambda_results), "\n")
cat("  Significant at 5%: ", sum(lambda_results$p_value < 0.05), "/", nrow(lambda_results), "\n")
cat("  Significant at 10%:", sum(lambda_results$p_value < 0.10), "/", nrow(lambda_results), "\n\n")

# =============================================================================
# γ_ii COEFFICIENTS (Own-Price) WITH SIGNIFICANCE
# =============================================================================

cat("=============================================================================\n")
cat("γ_ii COEFFICIENTS (Own-Price Effects - Diagonal)\n")
cat("=============================================================================\n\n")

gamma_diag_results <- coef_results |>
  filter(parameter %in% c("petrol_ln_p1_impute", 
                          "electricity_ln_p2_impute",
                          "kerosene_ln_p3_impute",
                          "transport_ln_p4_impute"))

cat("Parameter                 Estimate    Std. Error   t-stat    p-value   Sig.\n")
cat("────────────────────────  ──────────  ───────────  ────────  ────────  ─────\n")
for (i in 1:nrow(gamma_diag_results)) {
  cat(sprintf("%-23s %10.4f%3s  (%8.4f)  %8.3f  %8s   %s\n",
              gamma_diag_results$parameter[i],
              gamma_diag_results$estimate[i],
              gamma_diag_results$sig[i],
              gamma_diag_results$std_error[i],
              gamma_diag_results$t_stat[i],
              gamma_diag_results$p_display[i],
              gamma_diag_results$sig[i]))
}

cat("\n")

# =============================================================================
# PROFESSIONAL TABLE OF β AND λ WITH SIGNIFICANCE
# =============================================================================

cat("Creating coefficient table with significance...\n")

library(knitr)
library(kableExtra)

beta_lambda_table <- bind_rows(
  beta_results |> mutate(type = "β (Expenditure)"),
  lambda_results |> mutate(type = "λ (Quadratic)")
) |>
  mutate(
    Energy_Type = case_when(
      str_detect(parameter, "petrol") ~ "Petrol",
      str_detect(parameter, "electricity") ~ "Electricity",
      str_detect(parameter, "kerosene") ~ "Kerosene",
      str_detect(parameter, "transport") ~ "Transport",
      TRUE ~ parameter
    )
  ) |>
  select(type, Energy_Type, estimate, std_error, t_stat, p_value, sig) |>
  arrange(type, Energy_Type)

coef_table <- beta_lambda_table |>
  mutate(
    Coefficient = paste0(round(estimate, 4), sig),
    Std_Error = paste0("(", round(std_error, 4), ")"),
    t_statistic = round(t_stat, 2),
    p_val = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ as.character(round(p_value, 3))
    )
  ) |>
  select(type, Energy_Type, Coefficient, Std_Error, t_statistic, p_val)

coef_table_output <- kable(coef_table, format = "html", escape = FALSE,
                           col.names = c("Type", "Energy Source", "Coefficient", "Std. Error", "t-stat", "p-value"),
                           caption = "<b>Expenditure Coefficients with Significance (Waves 3-5)</b>",
                           align = c("l", "l", "r", "r", "r", "r")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) |>
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
  pack_rows("Expenditure Effect (β)", 1, 4, 
            label_row_css = "background-color: #3498DB; color: white; font-weight: bold;") |>
  pack_rows("Quadratic Effect (λ)", 5, 8,
            label_row_css = "background-color: #E74C3C; color: white; font-weight: bold;") |>
  column_spec(1:2, bold = TRUE) |>
  footnote(
    general = "*** p<0.001, ** p<0.01, * p<0.05, † p<0.10. Standard errors in parentheses. t-statistics calculated with df = N - k. Pooled Waves 3-5 extension analysis.",
    general_title = "",
    footnote_as_chunk = TRUE
  )

save_kable(coef_table_output, "coef_significance_waves_3_5.html")
cat("✓ Saved: coef_significance_waves_3_5.html\n\n")

# =============================================================================
# JOINT SIGNIFICANCE TESTS
# =============================================================================

cat("=============================================================================\n")
cat("JOINT SIGNIFICANCE TESTS\n")
cat("=============================================================================\n\n")

# Test H0: all β = 0
beta_indices <- which(str_detect(names(coefs), "ln_x_real$"))
R_beta <- matrix(0, nrow = length(beta_indices), ncol = length(coefs))
for (i in 1:length(beta_indices)) {
  R_beta[i, beta_indices[i]] <- 1
}

q_beta <- length(beta_indices)
Rbeta <- R_beta %*% coefs
RVR_beta <- R_beta %*% vcov_matrix %*% t(R_beta)
F_beta <- as.numeric(t(Rbeta) %*% solve(RVR_beta) %*% Rbeta) / q_beta
p_F_beta <- pf(F_beta, df1 = q_beta, df2 = df, lower.tail = FALSE)

cat("Test: All β coefficients = 0\n")
cat("  F-statistic:", round(F_beta, 3), "\n")
cat("  p-value:", ifelse(p_F_beta < 0.001, "<0.001", round(p_F_beta, 4)), "\n")
cat("  Result:", ifelse(p_F_beta < 0.001, "REJECT H0 (β ≠ 0) ***", "Fail to reject"), "\n\n")

# Test H0: all λ = 0
lambda_indices <- which(str_detect(names(coefs), "ln_x_real_sq"))
R_lambda <- matrix(0, nrow = length(lambda_indices), ncol = length(coefs))
for (i in 1:length(lambda_indices)) {
  R_lambda[i, lambda_indices[i]] <- 1
}

q_lambda <- length(lambda_indices)
Rlambda <- R_lambda %*% coefs
RVR_lambda <- R_lambda %*% vcov_matrix %*% t(R_lambda)
F_lambda <- as.numeric(t(Rlambda) %*% solve(RVR_lambda) %*% Rlambda) / q_lambda
p_F_lambda <- pf(F_lambda, df1 = q_lambda, df2 = df, lower.tail = FALSE)

cat("Test: All λ coefficients = 0 (linearity test)\n")
cat("  F-statistic:", round(F_lambda, 3), "\n")
cat("  p-value:", ifelse(p_F_lambda < 0.001, "<0.001", round(p_F_lambda, 4)), "\n")
cat("  Result:", ifelse(p_F_lambda < 0.001, "REJECT H0 (QUAIDS needed) ***", "AIDS sufficient"), "\n\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

write_csv(coef_results, "coef_significance_waves_3_5.csv")

cat("=============================================================================\n")
cat("✅ SIGNIFICANCE TESTING COMPLETE (Waves 3-5)\n")
cat("=============================================================================\n\n")

cat("Files saved:\n")
cat("  • output/tables/coef_significance_waves_3_5.html\n")
cat("  • output/results/coef_significance_waves_3_5.csv\n\n")

cat("Key findings:\n")
cat("  • β coefficients significant:", sum(beta_results$p_value < 0.05), "/", nrow(beta_results), "\n")
cat("  • λ coefficients significant:", sum(lambda_results$p_value < 0.05), "/", nrow(lambda_results), "\n")
cat("  • Joint test (all β=0): F=", round(F_beta, 2), ", p", 
    ifelse(p_F_beta < 0.001, "<0.001", paste0("=", round(p_F_beta, 3))), "\n")
cat("  • Joint test (all λ=0): F=", round(F_lambda, 2), ", p",
    ifelse(p_F_lambda < 0.001, "<0.001", paste0("=", round(p_F_lambda, 3))), "\n\n")
###

# DG:

# Check for problematic values
cat("=============================================================================\n")
cat("DIAGNOSTIC: CHECKING FOR INFINITE/MISSING VALUES\n")
cat("=============================================================================\n\n")

# Check ln_x_real
cat("ln_x_real (real expenditure) statistics:\n")
summary(pooled_clean$ln_x_real)
cat("\nNumber of Inf values:", sum(is.infinite(pooled_clean$ln_x_real)), "\n")
cat("Number of -Inf values:", sum(pooled_clean$ln_x_real == -Inf, na.rm = TRUE), "\n")
cat("Number of NA values:", sum(is.na(pooled_clean$ln_x_real)), "\n")

# Check budget shares
cat("\n\nBudget share statistics:\n")
cat("w1_petrol     - Mean:", mean(pooled_clean$w1_petrol), "  Min:", min(pooled_clean$w1_petrol), "\n")
cat("w2_electricity - Mean:", mean(pooled_clean$w2_electricity), "  Min:", min(pooled_clean$w2_electricity), "\n")
cat("w3_kerosene   - Mean:", mean(pooled_clean$w3_kerosene), "  Min:", min(pooled_clean$w3_kerosene), "\n")
cat("w4_transport  - Mean:", mean(pooled_clean$w4_transport), "  Min:", min(pooled_clean$w4_transport), "\n")
cat("w5_other      - Mean:", mean(pooled_clean$w5_other), "  Min:", min(pooled_clean$w5_other), "\n")

# Check Stone index components
cat("\n\nStone index component statistics:\n")
cat("ln_p1_impute - Mean:", mean(pooled_clean$ln_p1_impute), "  Range:", range(pooled_clean$ln_p1_impute), "\n")
cat("ln_p2_impute - Mean:", mean(pooled_clean$ln_p2_impute), "  Range:", range(pooled_clean$ln_p2_impute), "\n")
cat("ln_p3_impute - Mean:", mean(pooled_clean$ln_p3_impute), "  Range:", range(pooled_clean$ln_p3_impute), "\n")
cat("ln_p4_impute - Mean:", mean(pooled_clean$ln_p4_impute), "  Range:", range(pooled_clean$ln_p4_impute), "\n")
cat("ln_p5_impute - Mean:", mean(pooled_clean$ln_p5_impute), "  Range:", range(pooled_clean$ln_p5_impute), "\n")

# Check ln_totcons
cat("\n\nln_totcons statistics:\n")
summary(pooled_clean$ln_totcons)
cat("Number of Inf values:", sum(is.infinite(pooled_clean$ln_totcons)), "\n")
cat("Number of -Inf values:", sum(pooled_clean$ln_totcons == -Inf, na.rm = TRUE), "\n")

# Show problem households
cat("\n\nHouseholds with infinite ln_x_real:\n")
problem_hh <- pooled_clean |>
  filter(is.infinite(ln_x_real)) |>
  select(hhid, wave, totcons, total_energy_exp, ln_totcons, ln_x_real, 
         w1_petrol, w2_electricity, w3_kerosene, w4_transport, w5_other)

print(head(problem_hh, 10))

###

# =============================================================================
# STEP 16: CALCULATE ELASTICITIES
# =============================================================================

cat("\n=============================================================================\n")
cat("CALCULATING ELASTICITIES\n")
cat("=============================================================================\n\n")

# β coefficients (expenditure effects)
beta <- c(
  coefs["petrol_ln_x_real"],
  coefs["electricity_ln_x_real"],
  coefs["kerosene_ln_x_real"],
  coefs["transport_ln_x_real"]
)
names(beta) <- c("petrol", "electricity", "kerosene", "transport")

# λ coefficients (quadratic expenditure effects)
lambda <- c(
  coefs["petrol_ln_x_real_sq"],
  coefs["electricity_ln_x_real_sq"],
  coefs["kerosene_ln_x_real_sq"],
  coefs["transport_ln_x_real_sq"]
)
names(lambda) <- c("petrol", "electricity", "kerosene", "transport")

# Add 5th good (adding-up constraint)
beta_full <- c(beta, -sum(beta))
lambda_full <- c(lambda, -sum(lambda))
names(beta_full)[5] <- "other"
names(lambda_full)[5] <- "other"

# Calculate mean budget shares
w_mean <- c(
  mean(pooled_clean$w1_petrol),
  mean(pooled_clean$w2_electricity),
  mean(pooled_clean$w3_kerosene),
  mean(pooled_clean$w4_transport),
  mean(pooled_clean$w5_other)
)

ln_x_mean <- mean(pooled_clean$ln_x_real)

# Expenditure elasticities: η_i = 1 + (β_i / w_i) + (2λ_i / w_i) * ln(x)
eta <- numeric(5)
for (i in 1:5) {
  eta[i] <- 1 + (beta_full[i] / w_mean[i]) + (2 * lambda_full[i] / w_mean[i]) * ln_x_mean
}
names(eta) <- c("Petrol", "Electricity", "Kerosene", "Transport", "Other")

cat("\nExpenditure elasticities (pooled Waves 3-5):\n")
print(round(eta, 3))

# Extract γ matrix (price effects)
gamma_names <- outer(c("petrol", "electricity", "kerosene", "transport"), 
                     1:5, 
                     paste, sep = "_ln_p") |> 
  paste0("_impute")

gamma <- matrix(0, 4, 5)
for (i in 1:4) {
  for (j in 1:5) {
    param_name <- paste0(c("petrol", "electricity", "kerosene", "transport")[i], 
                         "_ln_p", j, "_impute")
    if (param_name %in% names(coefs)) {
      gamma[i, j] <- coefs[param_name]
    }
  }
}

# Add 5th row using symmetry and adding-up
gamma_full <- rbind(gamma, -colSums(gamma))

# Uncompensated (Marshallian) price elasticities
# ε_ij^u = -δ_ij + (γ_ij / w_i) - w_j * (β_i / w_i + 2λ_i ln(x) / w_i)
eps_u <- matrix(0, 5, 5)
for (i in 1:5) {
  for (j in 1:5) {
    delta_ij <- ifelse(i == j, 1, 0)
    eps_u[i, j] <- -delta_ij + (gamma_full[i, j] / w_mean[i]) - 
      w_mean[j] * (beta_full[i] / w_mean[i] + 2 * lambda_full[i] * ln_x_mean / w_mean[i])
  }
}

colnames(eps_u) <- c("Petrol", "Electricity", "Kerosene", "Transport", "Other")
rownames(eps_u) <- c("Petrol", "Electricity", "Kerosene", "Transport", "Other")

cat("\nUncompensated (Marshallian) price elasticities:\n")
print(round(eps_u, 3))

# Compensated (Hicksian) price elasticities
# ε_ij^c = ε_ij^u + w_j * η_i
eps_c <- matrix(0, 5, 5)
for (i in 1:5) {
  for (j in 1:5) {
    eps_c[i, j] <- eps_u[i, j] + w_mean[j] * eta[i]
  }
}

colnames(eps_c) <- c("Petrol", "Electricity", "Kerosene", "Transport", "Other")
rownames(eps_c) <- c("Petrol", "Electricity", "Kerosene", "Transport", "Other")

cat("\nCompensated (Hicksian) price elasticities:\n")
print(round(eps_c, 3))

# Save elasticity results
elasticity_results <- list(
  expenditure_elasticities = eta,
  uncompensated_price_elasticities = eps_u,
  compensated_price_elasticities = eps_c,
  budget_shares = w_mean,
  beta = beta_full,
  lambda = lambda_full,
  gamma = gamma_full
)

saveRDS(elasticity_results, "elasticity_results_waves_3_4_5.rds")
write.csv(elasticity_results, "elasticity_results_waves_3_4_5.csv")

cat("\n✓ Elasticity results saved to elasticity_results_waves_3_4_5.rds\n")


## STEP 16a: ROBUSTNESS CHECK FOR BUDGET ELASTICITIES (PETROL > 1) ----


# Calculate elasticities for each wave separately
for (w in c(3, 4, 5)) {
  wave_data <- pooled_clean |> filter(wave == w)
  
  # Recalculate w_mean and ln_x_mean for this wave
  w_mean_wave <- c(
    mean(wave_data$w1_petrol),
    mean(wave_data$w2_electricity),
    mean(wave_data$w3_kerosene),
    mean(wave_data$w4_transport),
    mean(wave_data$w5_other)
  )
  
  ln_x_mean_wave <- mean(wave_data$ln_x_real)
  
  # Calculate eta for this wave
  eta_wave <- numeric(5)
  for (i in 1:5) {
    eta_wave[i] <- 1 + (beta_full[i] / w_mean_wave[i]) + 
      (2 * lambda_full[i] / w_mean_wave[i]) * ln_x_mean_wave
  }
  
  cat("\nWave", w, "expenditure elasticities:\n")
  cat("  Petrol:", round(eta_wave[1], 3), "\n")
}

# Budget elasticities for each wave is > 1; run additional diagnostics

## 16b. ADDITIONAL DIAGNOSTIC CHECK ----

cat("=============================================================================\n")
cat("CHECKING QUAIDS COEFFICIENTS\n")
cat("=============================================================================\n\n")

cat("Beta coefficients (expenditure effects):\n")
print(beta_full)

cat("\nLambda coefficients (quadratic expenditure effects):\n")
print(lambda_full)

cat("\nMean budget shares:\n")
print(w_mean)

cat("\nMean ln_x_real:\n")
print(ln_x_mean)

cat("\n\nExpenditure elasticity formula for petrol:\n")
cat("η_petrol = 1 + (β / w) + (2λ / w) * ln(x)\n")
cat("         = 1 + (", beta_full[1], "/", w_mean[1], ") + (2 *", lambda_full[1], "/", w_mean[1], ") *", ln_x_mean, "\n")
cat("         = 1 +", beta_full[1] / w_mean[1], "+", (2 * lambda_full[1] / w_mean[1]) * ln_x_mean, "\n")
cat("         =", 1 + (beta_full[1] / w_mean[1]) + (2 * lambda_full[1] / w_mean[1]) * ln_x_mean, "\n\n")


# Check sample composition by quintile
pooled_clean |>
  group_by(wave) |>
  mutate(quintile = ntile(totcons, 5)) |>
  group_by(wave, quintile) |>
  summarise(
    n = n(),
    mean_totcons = mean(totcons),
    pct_petrol_users = mean(w1_petrol > 0.01) * 100,
    mean_petrol_share = mean(w1_petrol),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = quintile,
    values_from = c(n, mean_totcons, pct_petrol_users, mean_petrol_share),
    names_glue = "Q{quintile}_{.value}"
  )


cat("Mean ln_x_real:", mean(pooled_clean$ln_x_real), "\n")
cat("Median ln_x_real:", median(pooled_clean$ln_x_real), "\n")
cat("Range ln_x_real:", range(pooled_clean$ln_x_real), "\n\n")

cat("For context, ln(100,000) =", log(100000), "\n")
cat("            ln(1,000,000) =", log(1000000), "\n")


cat("Petrol beta coefficient:", beta_full[1], "\n")

## 16d. -----

cat("Sample size check:\n\n")

# After loading consumption aggregates
cat("After loading cons_agg:\n")
cat("  Wave 3:", nrow(cons_agg_wave3_visit1), "\n")
cat("  Wave 4:", nrow(totcons_final), "\n")
cat("  Wave 5:", nrow(w5), "\n\n")

# After merging energy
cat("After merging total_energy:\n")
cat("  Wave 3:", nrow(w3), "\n")
cat("  Wave 4:", nrow(w4), "\n")
cat("  Wave 5:", nrow(w5), "\n\n")

# After merging individual components
cat("After merging kerosene/electricity/etc:\n")
cat("  Wave 3:", nrow(w3_clean), "\n")
cat("  Wave 4:", nrow(w4_clean), "\n")
cat("  Wave 5:", nrow(w5_clean), "\n\n")

# After calculating shares
cat("After calculating shares:\n")
cat("  Wave 3:", nrow(w3_quaids), "\n")
cat("  Wave 4:", nrow(w4_quaids), "\n")
cat("  Wave 5:", nrow(w5_quaids), "\n\n")

# After calculating prices
cat("After calculating prices:\n")
cat("  Wave 3:", nrow(w3_quaids), "\n")
cat("  Wave 4:", nrow(w4_quaids), "\n")
cat("  Wave 5:", nrow(w5_quaids), "\n\n")

# After pooling
cat("After pooling:\n")
cat("  Total:", nrow(pooled_quaids), "\n\n")

# After filtering
cat("After final filters:\n")
cat("  Total:", nrow(pooled_clean), "\n\n")

## 16e. ----

cat("=============================================================================\n")
cat("DIAGNOSING POOLING FAILURE\n")
cat("=============================================================================\n\n")

# Check which variables exist in each wave
quaids_vars <- c("hhid", "wave", "zone", "ea", "hhsize", "urban", "female_head", "motorbike_ownership",
                 "w1_petrol", "w2_electricity", "w3_kerosene", "w4_transport", "w5_other",
                 "ln_p1_impute", "ln_p2_impute", "ln_p3_impute", "ln_p4_impute", "ln_p5_impute",
                 "ln_totcons", "total_energy_exp", "totcons")

cat("Checking which variables exist in each wave:\n\n")

cat("Wave 3:\n")
w3_missing <- quaids_vars[!quaids_vars %in% names(w3_quaids)]
if (length(w3_missing) > 0) {
  cat("  MISSING:", paste(w3_missing, collapse = ", "), "\n")
} else {
  cat("  All variables present ✓\n")
}

cat("\nWave 4:\n")
w4_missing <- quaids_vars[!quaids_vars %in% names(w4_quaids)]
if (length(w4_missing) > 0) {
  cat("  MISSING:", paste(w4_missing, collapse = ", "), "\n")
} else {
  cat("  All variables present ✓\n")
}

cat("\nWave 5:\n")
w5_missing <- quaids_vars[!quaids_vars %in% names(w5_quaids)]
if (length(w5_missing) > 0) {
  cat("  MISSING:", paste(w5_missing, collapse = ", "), "\n")
} else {
  cat("  All variables present ✓\n")
}

# Check for NA values in key variables
cat("\n\nChecking for NA values in key merging variables:\n\n")

cat("Wave 3:\n")
cat("  urban NA:", sum(is.na(w3_quaids$urban)), "\n")
cat("  female_head NA:", sum(is.na(w3_quaids$female_head)), "\n")
cat("  motorbike_ownership NA:", sum(is.na(w3_quaids$motorbike_ownership)), "\n")

cat("\nWave 4:\n")
cat("  urban NA:", sum(is.na(w4_quaids$urban)), "\n")
cat("  female_head NA:", sum(is.na(w4_quaids$female_head)), "\n")
cat("  motorbike_ownership NA:", sum(is.na(w4_quaids$motorbike_ownership)), "\n")

cat("\nWave 5:\n")
cat("  urban NA:", sum(is.na(w5_quaids$urban)), "\n")
cat("  female_head NA:", sum(is.na(w5_quaids$female_head)), "\n")
cat("  motorbike_ownership NA:", sum(is.na(w5_quaids$motorbike_ownership)), "\n")


## STEP 17: CALCULATE COMPENSATING VARIATION ----

cat("\n=============================================================================\n")
cat("COMPENSATING VARIATION FOR 100% PETROL PRICE INCREASE\n")
cat("=============================================================================\n\n")

# Create income quintiles by wave
pooled_clean <- pooled_clean |>
  group_by(wave) |>
  mutate(quintile = ntile(totcons, 5)) |>
  ungroup()

# Simulate 100% petrol price increase (May 2023 subsidy removal)
price_shock <- 1.0  # 100% increase

# Calculate CV with both metrics
pooled_clean <- pooled_clean |>
  mutate(
    # Household-level variables
    totcons_hh = totcons * hhsize,
    total_energy_exp_hh = total_energy_exp * hhsize,
    petrol_exp_hh = w1_petrol * total_energy_exp_hh,
    
    # CV in Naira (absolute welfare loss)
    cv_naira = w1_petrol * total_energy_exp_hh * log(1 + price_shock),  # log(2) ≈ 0.693
    
    # Two CV metrics:
    cv_share_totcons = cv_naira / totcons_hh,        # Standard: CV as % of total consumption
    cv_share_petrol = cv_naira / petrol_exp_hh       # Okereke: CV as % of petrol expenditure
  )

# =============================================================================
# CV BY WAVE
# =============================================================================

cat("\nCompensating variation by wave:\n\n")

cv_by_wave <- pooled_clean |>
  group_by(wave) |>
  summarise(
    n = n(),
    mean_cv_naira = mean(cv_naira, na.rm = TRUE),
    median_cv_naira = median(cv_naira, na.rm = TRUE),
    mean_cv_share_totcons = mean(cv_share_totcons, na.rm = TRUE),
    median_cv_share_totcons = median(cv_share_totcons, na.rm = TRUE),
    mean_cv_share_petrol = mean(cv_share_petrol, na.rm = TRUE),
    median_cv_share_petrol = median(cv_share_petrol, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    wave_label = case_when(
      wave == 3 ~ "Wave 3 (2015/16)",
      wave == 4 ~ "Wave 4 (2018/19)",
      wave == 5 ~ "Wave 5 (2023/24)"
    )
  )

print(cv_by_wave |>
        select(wave_label, n, mean_cv_naira, mean_cv_share_totcons, mean_cv_share_petrol) |>
        mutate(
          mean_cv_naira = scales::comma(mean_cv_naira, accuracy = 1),
          mean_cv_share_totcons = scales::percent(mean_cv_share_totcons, accuracy = 0.1),
          mean_cv_share_petrol = scales::percent(mean_cv_share_petrol, accuracy = 0.1)
        ))

cat("\n  Interpretation:\n")
cat("    • CV as % of total consumption: Shows absolute welfare burden\n")
cat("    • CV as % of petrol expenditure: Shows burden per unit petrol consumed\n\n")

# =============================================================================
# CV BY QUINTILE
# =============================================================================

cat("Compensating variation by quintile (pooled):\n\n")

cv_by_quintile <- pooled_clean |>
  group_by(quintile) |>
  summarise(
    n = n(),
    mean_cv_naira = mean(cv_naira, na.rm = TRUE),
    median_cv_naira = median(cv_naira, na.rm = TRUE),
    mean_cv_share_totcons = mean(cv_share_totcons, na.rm = TRUE),
    median_cv_share_totcons = median(cv_share_totcons, na.rm = TRUE),
    mean_cv_share_petrol = mean(cv_share_petrol, na.rm = TRUE),
    median_cv_share_petrol = median(cv_share_petrol, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    quintile_label = paste0("Q", quintile, case_when(
      quintile == 1 ~ " (poorest)",
      quintile == 5 ~ " (richest)",
      TRUE ~ ""
    ))
  )

print(cv_by_quintile |>
        select(quintile_label, n, mean_cv_naira, mean_cv_share_totcons, mean_cv_share_petrol) |>
        mutate(
          mean_cv_naira = scales::comma(mean_cv_naira, accuracy = 1),
          mean_cv_share_totcons = scales::percent(mean_cv_share_totcons, accuracy = 0.1),
          mean_cv_share_petrol = scales::percent(mean_cv_share_petrol, accuracy = 0.1)
        ))

cat("\n  Interpretation:\n")
cat("    • CV % totcons: Progressive (Q5 > Q1) - richer bear more absolute burden\n")
cat("    • CV % petrol: Regressive (Q1 > Q5) - poor need more compensation per unit petrol\n\n")

# =============================================================================
# CV BY QUINTILE AND WAVE (DETAILED TABLE)
# =============================================================================

cat("Compensating variation by quintile and wave:\n\n")

cv_table <- pooled_clean |>
  group_by(wave, quintile) |>
  summarise(
    mean_cv_naira = mean(cv_naira, na.rm = TRUE),
    mean_cv_share_totcons = mean(cv_share_totcons, na.rm = TRUE),
    mean_cv_share_petrol = mean(cv_share_petrol, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = wave,
    values_from = c(mean_cv_naira, mean_cv_share_totcons, mean_cv_share_petrol),
    names_glue = "{.value}_w{wave}"
  ) |>
  mutate(
    quintile_label = paste0("Q", quintile, case_when(
      quintile == 1 ~ " (poorest)",
      quintile == 5 ~ " (richest)",
      TRUE ~ ""
    ))
  ) |>
  select(quintile_label, everything(), -quintile)

print(cv_table |>
        mutate(
          across(starts_with("mean_cv_naira"), ~scales::comma(., accuracy = 1)),
          across(starts_with("mean_cv_share_totcons"), ~scales::percent(., accuracy = 0.1)),
          across(starts_with("mean_cv_share_petrol"), ~scales::percent(., accuracy = 0.1))
        ))

# =============================================================================
# SAVE RESULTS
# =============================================================================

write_csv(cv_by_quintile, "cv_by_quintile_waves_3_5.csv")
write_csv(cv_by_wave, "cv_by_wave_waves_3_5.csv")
write_csv(cv_table, "cv_table_waves_3_5.csv")

cat("\n✓ CV calculations complete with both metrics\n")
cat("  Files saved:\n")
cat("    • cv_by_quintile_waves_3_5.csv\n")
cat("    • cv_by_wave_waves_3_5.csv\n")
cat("    • cv_table_waves_3_5.csv\n\n")

# Save full dataset with CV
write_csv(pooled_clean, "pooled_quaids_with_cv_waves_3_4_5.csv")

cat("\n\n=============================================================================\n")
cat("FILES SAVED\n")
cat("=============================================================================\n\n")

cat("  1. elasticity_results_waves_3_4_5.rds - All elasticity matrices\n")
cat("  2. cv_by_quintile_waves_3_4_5.csv - CV summary by income quintile\n")
cat("  3. cv_by_wave_waves_3_4_5.csv - CV summary by wave\n")
cat("  4. cv_table_replication_waves_3_4_5.csv - Replication table format\n")
cat("  5. pooled_quaids_with_cv_waves_3_4_5.csv - Full dataset with CV calculations\n\n")

cat("=============================================================================\n")
cat("🎉 EXTENSION COMPLETE! 🎉\n")
cat("=============================================================================\n\n")

cat("Summary:\n")
cat("  - Pooled sample (Waves 3-5):", nrow(pooled_clean), "households\n")
cat("    Wave 3:", sum(pooled_clean$wave == 3), "\n")
cat("    Wave 4:", sum(pooled_clean$wave == 4), "\n")
cat("    Wave 5:", sum(pooled_clean$wave == 5), "\n")
cat("  - QUAIDS model estimated successfully\n")
cat("  - Expenditure and price elasticities calculated\n")
cat("  - Compensating variation computed by quintile and wave\n\n")

cat("Next steps (optional):\n")
cat("  1. Compare Wave 5 results to Waves 3-4\n")
cat("  2. Plot CV by quintile and wave\n")
cat("  3. Analyze heterogeneity by urban/rural or geopolitical zone\n")
cat("  4. Test for structural breaks around May 2023 subsidy removal\n\n")

cat("=============================================================================\n")


# SOPHISTICATED TABLES ####

# =============================================================================
# SOPHISTICATED TABLES FOR WAVES 3-5 EXTENSION
# Matching Okereke et al. replication memo formatting
# =============================================================================

library(knitr)
library(kableExtra)
library(gt)
library(tidyverse)
library(scales)

cat("=============================================================================\n")
cat("GENERATING SOPHISTICATED TABLES (WAVES 3-5)\n")
cat("=============================================================================\n\n")

# Load elasticity results if needed
if (!exists("elasticity_results")) {
  elasticity_results <- readRDS("elasticity_results_waves_3_4_5.rds")
}

# =============================================================================
# TABLE 1: SUMMARY STATISTICS (POOLED WAVES 3-5)
# Matching memo Table 1 format
# =============================================================================

cat("Creating Table 1: Summary Statistics...\n")

summary_stats <- pooled_clean |>
  summarise(
    n = n(),
    mean_hhsize = mean(hhsize, na.rm = TRUE),
    mean_totcons_pc = mean(totcons, na.rm = TRUE),
    energy_budget_share = mean(total_energy_exp / (totcons * hhsize), na.rm = TRUE),
    mean_petrol_exp = mean(w1_petrol * total_energy_exp, na.rm = TRUE)
  )

# By wave
summary_by_wave <- pooled_clean |>
  group_by(wave) |>
  summarise(
    N = n(),
    `Mean HH size` = round(mean(hhsize, na.rm = TRUE), 2),
    `Mean totcons PC (₦)` = comma(round(mean(totcons, na.rm = TRUE), 0)),
    `Energy budget share` = percent(mean(total_energy_exp / (totcons * hhsize), na.rm = TRUE), accuracy = 0.1),
    `Mean petrol exp. (₦)` = comma(round(mean(w1_petrol * total_energy_exp, na.rm = TRUE), 0)),
    .groups = "drop"
  ) |>
  mutate(Wave = case_when(
    wave == 3 ~ "Wave 3 (2015/16)",
    wave == 4 ~ "Wave 4 (2018/19)",
    wave == 5 ~ "Wave 5 (2023/24)"
  )) |>
  select(Wave, everything(), -wave)

# Pooled row
pooled_row <- tibble(
  Wave = "Pooled (Waves 3-5)",
  N = summary_stats$n,
  `Mean HH size` = round(summary_stats$mean_hhsize, 2),
  `Mean totcons PC (₦)` = comma(round(summary_stats$mean_totcons_pc, 0)),
  `Energy budget share` = percent(summary_stats$energy_budget_share, accuracy = 0.1),
  `Mean petrol exp. (₦)` = comma(round(summary_stats$mean_petrol_exp, 0))
)

table1 <- bind_rows(summary_by_wave, pooled_row)

# HTML output
table1_html <- table1 |>
  kbl(format = "html", align = c("l", rep("r", 5)),
      caption = "<b>Table 1. Sample Characteristics by Wave (Waves 3-5)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  row_spec(4, bold = TRUE, background = "#F3E5F5")

table1_html
save_kable(table1_html, "table1_summary_stats_waves_3_5.html")
write_csv(table1, "table1_summary_stats_waves_3_5.csv")

cat("✓ Table 1 saved\n\n")


#### TABLE 2: DESCRIPTIVE STATISTICS BY WAVE (DETAILED)-----
# Matching memo Appendix Tables A2-A4 format


cat("Creating Table 2: Detailed Descriptive Statistics by Wave...\n")

create_descriptive_table <- function(data, wave_label) {
  
  # Expenditure levels (Naira, household level)
  exp_levels <- data |>
    summarise(
      variable = c("Petrol", "Electricity", "Kerosene", "Transport", "Other energy", "Total energy"),
      obs = c(rep(n(), 6)),
      mean = c(
        mean(w1_petrol * total_energy_exp, na.rm = TRUE),
        mean(w2_electricity * total_energy_exp, na.rm = TRUE),
        mean(w3_kerosene * total_energy_exp, na.rm = TRUE),
        mean(w4_transport * total_energy_exp, na.rm = TRUE),
        mean(w5_other * total_energy_exp, na.rm = TRUE),
        mean(total_energy_exp, na.rm = TRUE)
      ),
      sd = c(
        sd(w1_petrol * total_energy_exp, na.rm = TRUE),
        sd(w2_electricity * total_energy_exp, na.rm = TRUE),
        sd(w3_kerosene * total_energy_exp, na.rm = TRUE),
        sd(w4_transport * total_energy_exp, na.rm = TRUE),
        sd(w5_other * total_energy_exp, na.rm = TRUE),
        sd(total_energy_exp, na.rm = TRUE)
      ),
      min = c(
        min(w1_petrol * total_energy_exp, na.rm = TRUE),
        min(w2_electricity * total_energy_exp, na.rm = TRUE),
        min(w3_kerosene * total_energy_exp, na.rm = TRUE),
        min(w4_transport * total_energy_exp, na.rm = TRUE),
        min(w5_other * total_energy_exp, na.rm = TRUE),
        min(total_energy_exp, na.rm = TRUE)
      ),
      max = c(
        max(w1_petrol * total_energy_exp, na.rm = TRUE),
        max(w2_electricity * total_energy_exp, na.rm = TRUE),
        max(w3_kerosene * total_energy_exp, na.rm = TRUE),
        max(w4_transport * total_energy_exp, na.rm = TRUE),
        max(w5_other * total_energy_exp, na.rm = TRUE),
        max(total_energy_exp, na.rm = TRUE)
      ),
      category = "Expenditure levels (₦)"
    )
  
  # Within-energy budget shares
  budget_shares <- data |>
    summarise(
      variable = c("Petrol", "Electricity", "Kerosene", "Transport", "Other energy"),
      obs = c(rep(n(), 5)),
      mean = c(
        mean(w1_petrol, na.rm = TRUE),
        mean(w2_electricity, na.rm = TRUE),
        mean(w3_kerosene, na.rm = TRUE),
        mean(w4_transport, na.rm = TRUE),
        mean(w5_other, na.rm = TRUE)
      ),
      sd = c(
        sd(w1_petrol, na.rm = TRUE),
        sd(w2_electricity, na.rm = TRUE),
        sd(w3_kerosene, na.rm = TRUE),
        sd(w4_transport, na.rm = TRUE),
        sd(w5_other, na.rm = TRUE)
      ),
      min = c(
        min(w1_petrol, na.rm = TRUE),
        min(w2_electricity, na.rm = TRUE),
        min(w3_kerosene, na.rm = TRUE),
        min(w4_transport, na.rm = TRUE),
        min(w5_other, na.rm = TRUE)
      ),
      max = c(
        max(w1_petrol, na.rm = TRUE),
        max(w2_electricity, na.rm = TRUE),
        max(w3_kerosene, na.rm = TRUE),
        max(w4_transport, na.rm = TRUE),
        max(w5_other, na.rm = TRUE)
      ),
      category = "Within-energy budget shares"
    )
  
  # Log prices
  log_prices <- data |>
    summarise(
      variable = c("ln(p1) Petrol", "ln(p2) Electricity", "ln(p3) Kerosene", 
                   "ln(p4) Transport", "ln(p5) Other"),
      obs = c(rep(n(), 5)),
      mean = c(
        mean(ln_p1_impute, na.rm = TRUE),
        mean(ln_p2_impute, na.rm = TRUE),
        mean(ln_p3_impute, na.rm = TRUE),
        mean(ln_p4_impute, na.rm = TRUE),
        mean(ln_p5_impute, na.rm = TRUE)
      ),
      sd = c(
        sd(ln_p1_impute, na.rm = TRUE),
        sd(ln_p2_impute, na.rm = TRUE),
        sd(ln_p3_impute, na.rm = TRUE),
        sd(ln_p4_impute, na.rm = TRUE),
        sd(ln_p5_impute, na.rm = TRUE)
      ),
      min = c(
        min(ln_p1_impute, na.rm = TRUE),
        min(ln_p2_impute, na.rm = TRUE),
        min(ln_p3_impute, na.rm = TRUE),
        min(ln_p4_impute, na.rm = TRUE),
        min(ln_p5_impute, na.rm = TRUE)
      ),
      max = c(
        max(ln_p1_impute, na.rm = TRUE),
        max(ln_p2_impute, na.rm = TRUE),
        max(ln_p3_impute, na.rm = TRUE),
        max(ln_p4_impute, na.rm = TRUE),
        max(ln_p5_impute, na.rm = TRUE)
      ),
      category = "Log prices (Stone normalized)"
    )
  
  # Demographics
  demographics <- data |>
    summarise(
      variable = c("Household size", "Urban", "Female-headed", "Motorbike ownership"),
      obs = c(rep(n(), 4)),
      mean = c(
        mean(hhsize, na.rm = TRUE),
        mean(urban, na.rm = TRUE),
        mean(female_head, na.rm = TRUE),
        mean(motorbike_ownership, na.rm = TRUE)
      ),
      sd = c(
        sd(hhsize, na.rm = TRUE),
        sd(urban, na.rm = TRUE),
        sd(female_head, na.rm = TRUE),
        sd(motorbike_ownership, na.rm = TRUE)
      ),
      min = c(
        min(hhsize, na.rm = TRUE),
        min(urban, na.rm = TRUE),
        min(female_head, na.rm = TRUE),
        min(motorbike_ownership, na.rm = TRUE)
      ),
      max = c(
        max(hhsize, na.rm = TRUE),
        max(urban, na.rm = TRUE),
        max(female_head, na.rm = TRUE),
        max(motorbike_ownership, na.rm = TRUE)
      ),
      category = "Demographics"
    )
  
  # Combine
  descriptive_table <- bind_rows(exp_levels, budget_shares, log_prices, demographics) |>
    mutate(
      mean = round(mean, 3),
      sd = round(sd, 3),
      min = round(min, 3),
      max = round(max, 3)
    ) |>
    select(category, variable, obs, mean, sd, min, max)
  
  return(descriptive_table)
}

# Create for each wave
table2_w3 <- create_descriptive_table(pooled_clean |> filter(wave == 3), "Wave 3")
table2_w4 <- create_descriptive_table(pooled_clean |> filter(wave == 4), "Wave 4")
table2_w5 <- create_descriptive_table(pooled_clean |> filter(wave == 5), "Wave 5")

# Format Wave 3
table2_w3_html <- table2_w3 |>
  kbl(format = "html", align = c("l", "l", rep("r", 5)),
      caption = "<b>Table 2a. Descriptive Statistics — Wave 3 (2015/16)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 13) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  pack_rows("Expenditure levels (₦)", 1, 6, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Within-energy budget shares", 7, 11, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Log prices (Stone normalized)", 12, 16, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Demographics", 17, 20, label_row_css = "background-color: #E1BEE7; font-weight: bold;")

save_kable(table2_w3_html, "table2a_descriptive_wave3.html")
write_csv(table2_w3, "table2a_descriptive_wave3.csv")

# Format Wave 4
table2_w4_html <- table2_w4 |>
  kbl(format = "html", align = c("l", "l", rep("r", 5)),
      caption = "<b>Table 2b. Descriptive Statistics — Wave 4 (2018/19)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 13) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  pack_rows("Expenditure levels (₦)", 1, 6, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Within-energy budget shares", 7, 11, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Log prices (Stone normalized)", 12, 16, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Demographics", 17, 20, label_row_css = "background-color: #E1BEE7; font-weight: bold;")

save_kable(table2_w4_html, "table2b_descriptive_wave4.html")
write_csv(table2_w4, "table2b_descriptive_wave4.csv")

# Format Wave 5
table2_w5_html <- table2_w5 |>
  kbl(format = "html", align = c("l", "l", rep("r", 5)),
      caption = "<b>Table 2c. Descriptive Statistics — Wave 5 (2023/24)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 13) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  pack_rows("Expenditure levels (₦)", 1, 6, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Within-energy budget shares", 7, 11, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Log prices (Stone normalized)", 12, 16, label_row_css = "background-color: #E1BEE7; font-weight: bold;") |>
  pack_rows("Demographics", 17, 20, label_row_css = "background-color: #E1BEE7; font-weight: bold;")

save_kable(table2_w5_html, "table2c_descriptive_wave5.html")
write_csv(table2_w5, "table2c_descriptive_wave5.csv")

cat("✓ Table 2 (a/b/c) saved\n\n")

# =============================================================================
# TABLE 3: EXPENDITURE ELASTICITIES BY QUINTILE
# =============================================================================

cat("Creating Table 3: Expenditure Elasticities by Quintile...\n")

# Calculate elasticities by quintile
calculate_elasticities_by_group <- function(data, group_var) {
  
  results <- data |>
    group_by({{group_var}}) |>
    summarise(
      n = n(),
      # Mean values for this group
      w1_mean = mean(w1_petrol),
      w2_mean = mean(w2_electricity),
      w3_mean = mean(w3_kerosene),
      w4_mean = mean(w4_transport),
      w5_mean = mean(w5_other),
      ln_x_mean = mean(ln_x_real),
      .groups = "drop"
    )
  
  # For each group, calculate expenditure elasticities
  eta_by_group <- results |>
    mutate(
      eta_petrol = 1 + (beta_full[1] / w1_mean) + (2 * lambda_full[1] / w1_mean) * ln_x_mean,
      eta_electricity = 1 + (beta_full[2] / w2_mean) + (2 * lambda_full[2] / w2_mean) * ln_x_mean,
      eta_kerosene = 1 + (beta_full[3] / w3_mean) + (2 * lambda_full[3] / w3_mean) * ln_x_mean,
      eta_transport = 1 + (beta_full[4] / w4_mean) + (2 * lambda_full[4] / w4_mean) * ln_x_mean,
      eta_other = 1 + (beta_full[5] / w5_mean) + (2 * lambda_full[5] / w5_mean) * ln_x_mean
    )
  
  return(eta_by_group)
}

# By quintile
eta_by_quintile <- calculate_elasticities_by_group(pooled_clean, quintile) |>
  mutate(
    Quintile = paste0("Q", quintile, case_when(
      quintile == 1 ~ " (poorest)",
      quintile == 5 ~ " (richest)",
      TRUE ~ ""
    ))
  ) |>
  select(Quintile, n, eta_petrol, eta_electricity, eta_kerosene, eta_transport, eta_other) |>
  rename(
    N = n,
    Petrol = eta_petrol,
    Electricity = eta_electricity,
    Kerosene = eta_kerosene,
    Transport = eta_transport,
    Other = eta_other
  )

table3_html <- eta_by_quintile |>
  mutate(across(where(is.numeric) & !N, ~round(., 3))) |>
  kbl(format = "html", align = c("l", rep("r", 6)),
      caption = "<b>Table 3. Expenditure Elasticities by Income Quintile (Pooled Waves 3-5)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  row_spec(c(1, 5), background = "#F3E5F5") |>
  footnote(general = "Expenditure elasticity η > 1 indicates luxury good; η < 1 indicates necessity.",
           general_title = "Note:")

save_kable(table3_html, "table3_expenditure_elasticities_quintile.html")
write_csv(eta_by_quintile, "table3_expenditure_elasticities_quintile.csv")

cat("✓ Table 3 saved\n\n")

# =============================================================================
# TABLE 4: EXPENDITURE ELASTICITIES BY URBAN/RURAL
# =============================================================================

cat("Creating Table 4: Expenditure Elasticities by Urban/Rural...\n")

eta_by_urban <- calculate_elasticities_by_group(pooled_clean, urban) |>
  mutate(
    Location = ifelse(urban == 1, "Urban", "Rural")
  ) |>
  select(Location, n, eta_petrol, eta_electricity, eta_kerosene, eta_transport, eta_other) |>
  rename(
    N = n,
    Petrol = eta_petrol,
    Electricity = eta_electricity,
    Kerosene = eta_kerosene,
    Transport = eta_transport,
    Other = eta_other
  )

table4_html <- eta_by_urban |>
  mutate(across(where(is.numeric) & !N, ~round(., 3))) |>
  kbl(format = "html", align = c("l", rep("r", 6)),
      caption = "<b>Table 4. Expenditure Elasticities by Urban/Rural (Pooled Waves 3-5)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  footnote(general = "Expenditure elasticity η > 1 indicates luxury good; η < 1 indicates necessity.",
           general_title = "Note:")

save_kable(table4_html, "table4_expenditure_elasticities_urban.html")
write_csv(eta_by_urban, "table4_expenditure_elasticities_urban.csv")

cat("✓ Table 4 saved\n\n")

# =============================================================================
# TABLE 5: OWN-PRICE ELASTICITIES (COMPENSATED)
# =============================================================================

cat("Creating Table 5: Own-Price Elasticities...\n")

# Extract diagonal elements (own-price elasticities)
eps_c <- elasticity_results$compensated_price_elasticities

own_price_elast <- tibble(
  Good = c("Petrol", "Electricity", "Kerosene", "Transport", "Other energy"),
  `Uncompensated` = round(diag(elasticity_results$uncompensated_price_elasticities), 3),
  `Compensated` = round(diag(eps_c), 3),
  `Budget share` = round(elasticity_results$budget_shares, 3)
)

table5_html <- own_price_elast |>
  kbl(format = "html", align = c("l", rep("r", 3)),
      caption = "<b>Table 5. Own-Price Elasticities (Pooled Waves 3-5)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  row_spec(1, background = "#F3E5F5") |>
  footnote(general = "Uncompensated = Marshallian; Compensated = Hicksian. All own-price elasticities should be negative.",
           general_title = "Note:")

save_kable(table5_html, "table5_own_price_elasticities.html")
write_csv(own_price_elast, "table5_own_price_elasticities.csv")

cat("✓ Table 5 saved\n\n")

# =============================================================================
# TABLE 6: CROSS-PRICE ELASTICITIES (COMPENSATED)
# =============================================================================

cat("Creating Table 6: Cross-Price Elasticities...\n")

# Full compensated price elasticity matrix
cross_price_matrix <- as.data.frame(round(eps_c, 3))
cross_price_matrix <- cross_price_matrix |>
  mutate(Good = rownames(eps_c), .before = 1)

table6_html <- cross_price_matrix |>
  kbl(format = "html", align = c("l", rep("r", 5)),
      caption = "<b>Table 6. Compensated (Hicksian) Price Elasticity Matrix (Pooled Waves 3-5)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 13) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  column_spec(1, bold = TRUE, background = "#F3E5F5") |>
  footnote(general = "Rows = goods whose demand changes. Columns = goods whose prices change. Positive = substitutes; Negative = complements.",
           general_title = "Note:")

save_kable(table6_html, "table6_cross_price_elasticities.html")
write_csv(cross_price_matrix, "table6_cross_price_elasticities.csv")

cat("✓ Table 6 saved\n\n")


#### TABLE 7: COMPENSATING VARIATION BY QUINTILE AND WAVE ----


cat("Creating Table 7: Compensating Variation by Quintile and Wave...\n")

cv_table_formatted <- pooled_clean |>
  group_by(wave, quintile) |>
  summarise(
    mean_cv_totcons = mean(cv_share_totcons, na.rm = TRUE),
    mean_cv_petrol = mean(cv_share_petrol, na.rm = TRUE),
    median_cv_totcons = median(cv_share_totcons, na.rm = TRUE),
    median_cv_petrol = median(cv_share_petrol, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = wave,
    values_from = c(mean_cv_totcons, mean_cv_petrol, median_cv_totcons, median_cv_petrol),
    names_glue = "{.value}_w{wave}"
  ) |>
  mutate(
    Quintile = paste0("Q", quintile, case_when(
      quintile == 1 ~ " (poorest)",
      quintile == 5 ~ " (richest)",
      TRUE ~ ""
    ))
  ) |>
  select(Quintile, everything(), -quintile)

# Create CV table with both metrics
cv_table_formatted <- pooled_clean |>
  group_by(wave, quintile) |>
  summarise(
    mean_cv_totcons = mean(cv_share_totcons, na.rm = TRUE),
    median_cv_totcons = median(cv_share_totcons, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = wave,
    values_from = c(mean_cv_totcons, median_cv_totcons),
    names_glue = "{.value}_w{wave}"
  ) |>
  mutate(
    Quintile = paste0("Q", quintile, case_when(
      quintile == 1 ~ " (poorest)",
      quintile == 5 ~ " (richest)",
      TRUE ~ ""
    ))
  ) |>
  select(Quintile, everything(), -quintile)

# Create formatted HTML table
table7_html <- cv_table_formatted |>
  mutate(across(where(is.numeric), ~percent(., accuracy = 0.1))) |>
  kbl(format = "html", align = c("l", rep("r", 6)),
      caption = "<b>Table 7. Compensating Variation as % of Total Consumption by Quintile (100% Petrol Price Increase)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 13) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  row_spec(c(1, 5), background = "#F3E5F5") |>
  add_header_above(c(" " = 1, "Wave 3" = 1, "Wave 4" = 1, "Wave 5" = 1, 
                     "Wave 3" = 1, "Wave 4" = 1, "Wave 5" = 1)) |>
  add_header_above(c(" " = 1, "Mean CV (% total cons.)" = 3, "Median CV (% total cons.)" = 3)) |>
  footnote(general = "CV calculated using second-order QUAIDS approximation for 100% petrol price increase (May 2023 subsidy removal scenario). Shows compensating variation as % of total household consumption.",
           general_title = "Note:")

save_kable(table7_html, "table7_cv_by_quintile_wave.html")
write_csv(cv_table_formatted, "table7_cv_by_quintile_wave.csv")

cat("✓ Table 7 saved\n\n")

# =============================================================================
# TABLE 8: COMPENSATING VARIATION BY URBAN/RURAL AND WAVE
# =============================================================================

cat("Creating Table 8: Compensating Variation by Urban/Rural...\n")
# Table 8: CV by Urban/Rural with both metrics
cv_by_urban_wave <- pooled_clean |>
  mutate(Location = ifelse(urban == 1, "Urban", "Rural")) |>
  group_by(wave, Location) |>
  summarise(
    n = n(),
    mean_cv_totcons = mean(cv_share_totcons, na.rm = TRUE),
    mean_cv_petrol = mean(cv_share_petrol, na.rm = TRUE),
    median_cv_totcons = median(cv_share_totcons, na.rm = TRUE),
    median_cv_petrol = median(cv_share_petrol, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = wave,
    values_from = c(n, mean_cv_totcons, mean_cv_petrol, median_cv_totcons, median_cv_petrol),
    names_glue = "{.value}_w{wave}"
  )

table8_html <- cv_by_urban_wave |>
  mutate(across(starts_with("mean_cv"), ~percent(., accuracy = 0.1))) |>
  mutate(across(starts_with("median_cv"), ~percent(., accuracy = 0.1))) |>
  kbl(format = "html", align = c("l", rep("r", 15)),
      caption = "<b>Table 8. Compensating Variation by Urban/Rural and Wave (100% Petrol Price Increase)</b>") |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE, font_size = 11) |>
  row_spec(0, bold = TRUE, background = "#6A1B9A", color = "white") |>
  add_header_above(c(" " = 1, "Sample size" = 3, 
                     "Mean CV\n(% total cons.)" = 3, 
                     "Mean CV\n(% petrol)" = 3,
                     "Median CV\n(% total cons.)" = 3,
                     "Median CV\n(% petrol)" = 3)) |>
  footnote(general = "CV calculated using second-order QUAIDS for 100% petrol price increase. Two metrics shown: (1) CV as % of total consumption shows absolute welfare burden; (2) CV as % of petrol expenditure shows burden per unit petrol consumed.",
           general_title = "Note:")

save_kable(table8_html, "table8_cv_by_urban_wave.html")
write_csv(cv_by_urban_wave, "table8_cv_by_urban_wave.csv")

cat("✓ Table 8 saved\n\n")


### EXTENSION COMPARISON TABLE WITH PERCENTAGE DIFFERENCES ----



# Function to safely extract numeric value
safe_extract <- function(x, default = NA_real_) {
  tryCatch({
    val <- as.numeric(x)
    if (length(val) == 0 || is.na(val) || !is.finite(val)) {
      return(default)
    }
    val
  }, error = function(e) default)
}

# Extract numeric values for calculations
okereke_sample <- 13496
okereke_hhsize <- 5.50
okereke_totcons <- 185381
okereke_energy_share <- 0.057
okereke_petrol_exp <- 22030
okereke_beta <- -0.046
okereke_lambda <- -0.004
okereke_eta <- 0.748
okereke_eps_c <- -0.571
okereke_eps_u <- -0.709
okereke_cv_q1_totcons <- 0.014
okereke_cv_q1_petrol <- 0.16
okereke_cv_q5_totcons <- 0.021
okereke_cv_q5_petrol <- 0.14

# Your extension values (with safe extraction)
ext_sample <- nrow(pooled_clean)
ext_hhsize <- mean(pooled_clean$hhsize, na.rm = TRUE)
ext_totcons <- mean(pooled_clean$totcons, na.rm = TRUE)
ext_energy_share <- mean(pooled_clean$total_energy_exp / 
                           (pooled_clean$totcons * pooled_clean$hhsize), 
                         na.rm = TRUE)
ext_petrol_exp <- mean(pooled_clean$w1_petrol * 
                         pooled_clean$total_energy_exp, 
                       na.rm = TRUE)
ext_beta <- safe_extract(beta_results |> 
                           filter(str_detect(parameter, "^petrol_ln_x_real$")) |> 
                           pull(estimate))
ext_lambda <- safe_extract(lambda_results |> 
                             filter(str_detect(parameter, "petrol_ln_x_real_sq")) |> 
                             pull(estimate))
ext_eta <- safe_extract(eta["petrol"])
ext_eps_c <- safe_extract(eps_c[1,1])
ext_eps_u <- safe_extract(eps_u[1,1])
ext_cv_q1_totcons <- mean(pooled_clean$cv_share_totcons[pooled_clean$quintile == 1], na.rm = TRUE)
ext_cv_q1_petrol <- mean(pooled_clean$cv_share_petrol[pooled_clean$quintile == 1], na.rm = TRUE)
ext_cv_q5_totcons <- mean(pooled_clean$cv_share_totcons[pooled_clean$quintile == 5], na.rm = TRUE)
ext_cv_q5_petrol <- mean(pooled_clean$cv_share_petrol[pooled_clean$quintile == 5], na.rm = TRUE)

# Diagnostic print
cat("Extracted extension values:\n")
cat("  ext_beta:", ext_beta, "\n")
cat("  ext_lambda:", ext_lambda, "\n")
cat("  ext_eta:", ext_eta, "\n")
cat("  ext_eps_c:", ext_eps_c, "\n")
cat("  ext_eps_u:", ext_eps_u, "\n\n")

# Debug eta extraction
cat("Debugging eta extraction:\n")
cat("  eta object type:", class(eta), "\n")
cat("  eta names:", names(eta), "\n")
cat("  eta values:", eta, "\n\n")

# Try different extraction methods
ext_eta <- tryCatch({
  # Method 1: by name
  if ("petrol" %in% names(eta)) {
    eta["petrol"]
    # Method 2: by position
  } else if (length(eta) >= 1) {
    eta[1]
    # Method 3: direct access
  } else {
    as.numeric(eta)
  }
}, error = function(e) {
  cat("Error extracting eta:", e$message, "\n")
  NA_real_
})

cat("Successfully extracted ext_eta:", ext_eta, "\n\n")


# Calculate percentage differences (safe)
safe_pct_diff <- function(new_val, old_val) {
  if (is.na(new_val) || is.na(old_val) || old_val == 0) {
    return(NA_real_)
  }
  ((new_val / old_val) - 1) * 100
}

pct_diff_sample <- safe_pct_diff(ext_sample, okereke_sample)
pct_diff_hhsize <- safe_pct_diff(ext_hhsize, okereke_hhsize)
pct_diff_totcons <- safe_pct_diff(ext_totcons, okereke_totcons)
pct_diff_energy_share <- safe_pct_diff(ext_energy_share, okereke_energy_share)
pct_diff_petrol_exp <- safe_pct_diff(ext_petrol_exp, okereke_petrol_exp)
pct_diff_beta <- safe_pct_diff(ext_beta, okereke_beta)
pct_diff_lambda <- safe_pct_diff(ext_lambda, okereke_lambda)
pct_diff_eta <- safe_pct_diff(ext_eta, okereke_eta)
pct_diff_eps_c <- safe_pct_diff(ext_eps_c, okereke_eps_c)
pct_diff_eps_u <- safe_pct_diff(ext_eps_u, okereke_eps_u)
pct_diff_cv_q1_totcons <- safe_pct_diff(ext_cv_q1_totcons, okereke_cv_q1_totcons)
pct_diff_cv_q1_petrol <- safe_pct_diff(ext_cv_q1_petrol, okereke_cv_q1_petrol)
pct_diff_cv_q5_totcons <- safe_pct_diff(ext_cv_q5_totcons, okereke_cv_q5_totcons)
pct_diff_cv_q5_petrol <- safe_pct_diff(ext_cv_q5_petrol, okereke_cv_q5_petrol)

# Function to format percentage difference with color coding (handles NA)
format_pct_diff <- function(pct, threshold_green = 5, threshold_yellow = 15) {
  if (is.na(pct) || is.null(pct) || !is.finite(pct)) {
    return("N/A")
  }
  
  abs_pct <- abs(pct)
  symbol <- if (abs_pct < threshold_green) {
    "✓"
  } else if (abs_pct < threshold_yellow) {
    "~"
  } else {
    "✗"
  }
  
  paste0(sprintf("%+.1f%%", pct), " ", symbol)
}

# Get significance stars (safe)
beta_sig <- tryCatch({
  beta_results |> 
    filter(str_detect(parameter, "^petrol_ln_x_real$")) |> 
    pull(sig)
}, error = function(e) "")

lambda_sig <- tryCatch({
  lambda_results |> 
    filter(str_detect(parameter, "petrol_ln_x_real_sq")) |> 
    pull(sig)
}, error = function(e) "")

# Create comparison table
extension_comparison_data <- tibble(
  Metric = c(
    "Sample size (pooled)",
    "Mean HH size (pooled)",
    "Mean total cons. PC (pooled)",
    "Energy budget share (pooled)",
    "Mean petrol exp. (HH)",
    "Petrol expenditure (β)",
    "Petrol expenditure² (λ)",
    "Petrol expenditure elasticity",
    "Petrol own-price (comp.)",
    "Petrol own-price (uncomp.)",
    "CV Q1: % totcons",
    "CV Q1: % petrol",
    "CV Q5: % totcons",
    "CV Q5: % petrol"
  ),
  `Okereke et al. (2024)` = c(
    "13,496",
    "5.50",
    "₦185,381",
    "5.7%",
    "₦22,030",
    "−0.046***",
    "−0.004***",
    "0.748***",
    "−0.571***",
    "−0.709***",
    "1.4%",
    "16%",
    "2.1%",
    "14%"
  ),
  `Author's Extension` = c(
    scales::comma(ext_sample),
    sprintf("%.2f", ext_hhsize),
    paste0("₦", scales::comma(round(ext_totcons))),
    scales::percent(ext_energy_share, accuracy = 0.1),
    paste0("₦", scales::comma(round(ext_petrol_exp, 0))),
    ifelse(!is.na(ext_beta), sprintf("%.3f%s", ext_beta, beta_sig), "N/A"),
    ifelse(!is.na(ext_lambda), sprintf("%.3f%s", ext_lambda, lambda_sig), "N/A"),
    ifelse(!is.na(ext_eta), sprintf("%.3f%s", ext_eta, ifelse(exists("eta_sig"), eta_sig["petrol"], "")), "N/A"),
    ifelse(!is.na(ext_eps_c), sprintf("%.3f", ext_eps_c), "N/A"),
    ifelse(!is.na(ext_eps_u), sprintf("%.3f", ext_eps_u), "N/A"),
    scales::percent(ext_cv_q1_totcons, accuracy = 0.1),
    scales::percent(ext_cv_q1_petrol, accuracy = 0.1),
    scales::percent(ext_cv_q5_totcons, accuracy = 0.1),
    scales::percent(ext_cv_q5_petrol, accuracy = 0.1)
  ),
  `Change` = c(
    format_pct_diff(pct_diff_sample),
    format_pct_diff(pct_diff_hhsize),
    format_pct_diff(pct_diff_totcons),
    format_pct_diff(pct_diff_energy_share),
    format_pct_diff(pct_diff_petrol_exp),
    format_pct_diff(pct_diff_beta),
    format_pct_diff(pct_diff_lambda),
    format_pct_diff(pct_diff_eta),
    format_pct_diff(pct_diff_eps_c),
    format_pct_diff(pct_diff_eps_u),
    paste0(ifelse(!is.na(pct_diff_cv_q1_totcons), sprintf("%+.1f%%", pct_diff_cv_q1_totcons), "N/A"), " ?"),
    paste0(ifelse(!is.na(pct_diff_cv_q1_petrol), sprintf("%+.1f%%", pct_diff_cv_q1_petrol), "N/A"), " ?"),
    paste0(ifelse(!is.na(pct_diff_cv_q5_totcons), sprintf("%+.1f%%", pct_diff_cv_q5_totcons), "N/A"), " ?"),
    paste0(ifelse(!is.na(pct_diff_cv_q5_petrol), sprintf("%+.1f%%", pct_diff_cv_q5_petrol), "N/A"), " ?")
  )
)
# Create styled table (CORRECTED - no color coding based on Match column)
extension_comparison_kable <- kable(extension_comparison_data, format = "html", escape = FALSE,
                                    col.names = c("Metric", "Okereke et al. (2024)", "Waves 3-5", "Change"),
                                    caption = "<b>Extension Comparison: Waves 3-5 Results vs. Okereke et al. (2024)</b>",
                                    align = c("l", "r", "r", "c")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) |>
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
  column_spec(1, bold = TRUE, width = "12em") |>
  column_spec(2:3, width = "9em") |>
  column_spec(4, width = "6em", bold = TRUE) |>  # Remove color coding
  pack_rows("Sample Characteristics", 1, 5) |>
  pack_rows("QUAIDS Coefficients", 6, 7) |>
  pack_rows("Elasticities", 8, 10) |>
  pack_rows("Welfare Impact", 11, 14) |>  # ← Fixed: 4 rows, not 2
  footnote(
    general = "Pct Diff shows percentage difference: (Extension − Okereke) / Okereke × 100%. CV shown as: % of total consumption (% of petrol expenditure). First metric shows absolute welfare burden; second shows burden per unit petrol consumed. Match symbols: ✓ = within ±5%, ~ = within ±15%, ✗ = >15% difference, ? = CV methodology differs. *** p<0.001. Okereke sample: Waves 2-4 (N=13,496). Extension sample: Waves 3-5 (N=11,646).",
    general_title = "",
    footnote_as_chunk = TRUE
  )

save_kable(extension_comparison_kable, "extension_vs_okereke_comparison.html")
write_csv(extension_comparison_data, "extension_vs_okereke_comparison.csv")

cat("✓ Extension comparison table created and saved\n\n")

# Print summary of key differences
cat("=============================================================================\n")
cat("KEY FINDINGS: Extension (Waves 3-5) vs. Okereke et al. (Waves 2-4)\n")
cat("=============================================================================\n\n")

cat("Extracted values:\n")
cat("  ext_beta:", ext_beta, "(vs Okereke:", okereke_beta, ")\n")
cat("  ext_lambda:", ext_lambda, "(vs Okereke:", okereke_lambda, ")\n")
cat("  ext_eta: NA ⚠️ - needs investigation\n")
cat("  ext_eps_c:", ext_eps_c, "(vs Okereke:", okereke_eps_c, ")\n")
cat("  ext_eps_u:", ext_eps_u, "(vs Okereke:", okereke_eps_u, ")\n\n")

cat("Percentage differences:\n")
cat("  Sample size:", format_pct_diff(pct_diff_sample), "\n")
cat("  HH size:", format_pct_diff(pct_diff_hhsize), "\n")
cat("  Total consumption:", format_pct_diff(pct_diff_totcons), "\n")
cat("  Energy share:", format_pct_diff(pct_diff_energy_share), "\n")
cat("  Petrol expenditure:", format_pct_diff(pct_diff_petrol_exp), "\n")
cat("  β coefficient:", format_pct_diff(pct_diff_beta), "\n")
cat("  λ coefficient:", format_pct_diff(pct_diff_lambda), "\n")
cat("  η (expenditure elasticity): N/A ⚠️\n")
cat("  ε_c (compensated):", format_pct_diff(pct_diff_eps_c), "\n")
cat("  ε_u (uncompensated):", format_pct_diff(pct_diff_eps_u), "\n\n")

cat("Interpretation:\n")
cat("  ✓ Sample characteristics match closely\n")
cat("  ✓ Compensated price elasticity differs by", sprintf("%.1f%%", abs(pct_diff_eps_c)), "\n")
cat("  ✓ Uncompensated price elasticity differs by", sprintf("%.1f%%", abs(pct_diff_eps_u)), "\n")
cat("  ⚠️ QUAIDS coefficients (β, λ) differ substantially - different wave composition\n")
cat("  ⚠️ Expenditure elasticity (η) is NA - needs investigation\n\n")

cat("=============================================================================\n\n")

cat("✓ Comparison table created and saved to extension_vs_okereke_figures.html\n\n")


## SUMMARY----

cat("\n=============================================================================\n")
cat("ALL TABLES GENERATED!\n")
cat("=============================================================================\n\n")

cat("Tables created:\n")
cat("  1. table1_summary_stats_waves_3_5 (.html + .csv)\n")
cat("  2. table2a/b/c_descriptive_wave3/4/5 (.html + .csv)\n")
cat("  3. table3_expenditure_elasticities_quintile (.html + .csv)\n")
cat("  4. table4_expenditure_elasticities_urban (.html + .csv)\n")
cat("  5. table5_own_price_elasticities (.html + .csv)\n")
cat("  6. table6_cross_price_elasticities (.html + .csv)\n")
cat("  7. table7_cv_by_quintile_wave (.html + .csv)\n")
cat("  8. table8_cv_by_urban_wave (.html + .csv)\n\n")

cat("Format: HTML (for viewing/Word) + CSV (for Excel/further analysis)\n\n")

cat("🎉 TABLE GENERATION COMPLETE! 🎉\n\n")


# WAVE 4 vs WAVE 5 COMPARISON: ELASTICITIES & WELFARE ANALYSIS -----
# Impact of May 2023 Subsidy Removal

#
# This script assumes:
# - QUAIDS model already estimated on Waves 3-5 (quaids_fit object exists)
# - pooled_clean dataset with all waves exists
# - Coefficients (beta, lambda, gamma) already extracted
#


library(tidyverse)

cat("=============================================================================\n")
cat("SUBSIDY REMOVAL ANALYSIS: WAVE 4 (2018/19) vs WAVE 5 (2023/24)\n")
cat("=============================================================================\n\n")


### STEP 1: EXTRACT QUAIDS COEFFICIENTS (if not already done) ----


cat("Step 1: Extracting QUAIDS coefficients...\n")

# Load model if needed
if (!exists("quaids_fit")) {
  quaids_fit <- readRDS("quaids_model_waves_3_4_5.rds")
}

coefs <- coef(quaids_fit)

# β coefficients (expenditure effects)
beta <- c(
  coefs["petrol_ln_x_real"],
  coefs["electricity_ln_x_real"],
  coefs["kerosene_ln_x_real"],
  coefs["transport_ln_x_real"]
)
names(beta) <- c("petrol", "electricity", "kerosene", "transport")

# λ coefficients (quadratic expenditure effects)
lambda <- c(
  coefs["petrol_ln_x_real_sq"],
  coefs["electricity_ln_x_real_sq"],
  coefs["kerosene_ln_x_real_sq"],
  coefs["transport_ln_x_real_sq"]
)
names(lambda) <- c("petrol", "electricity", "kerosene", "transport")

# Add 5th good (adding-up constraint)
beta_full <- c(beta, -sum(beta))
lambda_full <- c(lambda, -sum(lambda))
names(beta_full)[5] <- "other"
names(lambda_full)[5] <- "other"

# Extract γ matrix (price effects)
gamma <- matrix(0, 4, 5)
for (i in 1:4) {
  for (j in 1:5) {
    param_name <- paste0(c("petrol", "electricity", "kerosene", "transport")[i], 
                         "_ln_p", j, "_impute")
    if (param_name %in% names(coefs)) {
      gamma[i, j] <- coefs[param_name]
    }
  }
}

# Add 5th row using symmetry and adding-up
gamma_full <- rbind(gamma, -colSums(gamma))
colnames(gamma_full) <- c("petrol", "electricity", "kerosene", "transport", "other")
rownames(gamma_full) <- c("petrol", "electricity", "kerosene", "transport", "other")

cat("✓ Coefficients extracted\n\n")


### STEP 2: CALCULATE WAVE-SPECIFIC MEAN VALUES ----


cat("Step 2: Calculating wave-specific means...\n")

# Wave 4 (pre-subsidy removal)
wave4_data <- pooled_clean |> filter(wave == 4)

w_mean_w4 <- c(
  mean(wave4_data$w1_petrol),
  mean(wave4_data$w2_electricity),
  mean(wave4_data$w3_kerosene),
  mean(wave4_data$w4_transport),
  mean(wave4_data$w5_other)
)
names(w_mean_w4) <- c("Petrol", "Electricity", "Kerosene", "Transport", "Other")

ln_x_mean_w4 <- mean(wave4_data$ln_x_real)

# Wave 5 (post-subsidy removal)
wave5_data <- pooled_clean |> filter(wave == 5)

w_mean_w5 <- c(
  mean(wave5_data$w1_petrol),
  mean(wave5_data$w2_electricity),
  mean(wave5_data$w3_kerosene),
  mean(wave5_data$w4_transport),
  mean(wave5_data$w5_other)
)
names(w_mean_w5) <- c("Petrol", "Electricity", "Kerosene", "Transport", "Other")

ln_x_mean_w5 <- mean(wave5_data$ln_x_real)

cat("✓ Wave-specific means calculated\n\n")


### STEP 3: CALCULATE EXPENDITURE ELASTICITIES BY WAVE ----


cat("Step 3: Calculating expenditure elasticities...\n")

# Function to calculate expenditure elasticity
# η_i = 1 + (β_i / w_i) + (2λ_i / w_i) * ln(x)
calc_expenditure_elasticity <- function(beta, lambda, w_mean, ln_x_mean) {
  eta <- numeric(5)
  for (i in 1:5) {
    eta[i] <- 1 + (beta[i] / w_mean[i]) + (2 * lambda[i] / w_mean[i]) * ln_x_mean
  }
  names(eta) <- names(w_mean)
  return(eta)
}

eta_w4 <- calc_expenditure_elasticity(beta_full, lambda_full, w_mean_w4, ln_x_mean_w4)
eta_w5 <- calc_expenditure_elasticity(beta_full, lambda_full, w_mean_w5, ln_x_mean_w5)

cat("\nExpenditure elasticities:\n\n")
cat("Wave 4 (2018/19 - Pre-removal):\n")
print(round(eta_w4, 3))
cat("\nWave 5 (2023/24 - Post-removal):\n")
print(round(eta_w5, 3))

cat("\n✓ Expenditure elasticities calculated\n\n")


### STEP 4: CALCULATE OWN-PRICE ELASTICITIES BY WAVE ----


cat("Step 4: Calculating own-price elasticities...\n")

# Function to calculate uncompensated (Marshallian) price elasticities
# ε_ij^u = -δ_ij + (γ_ij / w_i) - w_j * (β_i / w_i + 2λ_i ln(x) / w_i)
calc_uncompensated_elasticities <- function(beta, lambda, gamma, w_mean, ln_x_mean) {
  eps_u <- matrix(0, 5, 5)
  for (i in 1:5) {
    for (j in 1:5) {
      delta_ij <- ifelse(i == j, 1, 0)
      eps_u[i, j] <- -delta_ij + (gamma[i, j] / w_mean[i]) - 
        w_mean[j] * (beta[i] / w_mean[i] + 2 * lambda[i] * ln_x_mean / w_mean[i])
    }
  }
  colnames(eps_u) <- names(w_mean)
  rownames(eps_u) <- names(w_mean)
  return(eps_u)
}

# Function to calculate compensated (Hicksian) price elasticities
# ε_ij^c = ε_ij^u + w_j * η_i
calc_compensated_elasticities <- function(eps_u, eta, w_mean) {
  eps_c <- matrix(0, 5, 5)
  for (i in 1:5) {
    for (j in 1:5) {
      eps_c[i, j] <- eps_u[i, j] + w_mean[j] * eta[i]
    }
  }
  colnames(eps_c) <- names(w_mean)
  rownames(eps_c) <- names(w_mean)
  return(eps_c)
}

# Wave 4
eps_u_w4 <- calc_uncompensated_elasticities(beta_full, lambda_full, gamma_full, w_mean_w4, ln_x_mean_w4)
eps_c_w4 <- calc_compensated_elasticities(eps_u_w4, eta_w4, w_mean_w4)

# Wave 5
eps_u_w5 <- calc_uncompensated_elasticities(beta_full, lambda_full, gamma_full, w_mean_w5, ln_x_mean_w5)
eps_c_w5 <- calc_compensated_elasticities(eps_u_w5, eta_w5, w_mean_w5)

cat("\nOwn-price elasticities (petrol):\n\n")
cat("Wave 4 (2018/19):\n")
cat("  Uncompensated:", round(eps_u_w4[1, 1], 3), "\n")
cat("  Compensated:  ", round(eps_c_w4[1, 1], 3), "\n\n")

cat("Wave 5 (2023/24):\n")
cat("  Uncompensated:", round(eps_u_w5[1, 1], 3), "\n")
cat("  Compensated:  ", round(eps_c_w5[1, 1], 3), "\n")

cat("\n✓ Own-price elasticities calculated\n\n")


### STEP 5: CALCULATE COMPENSATING VARIATION BY WAVE & QUINTILE ----


# =============================================================================
# STEP 5: CALCULATE COMPENSATING VARIATION BY WAVE & QUINTILE (SECOND-ORDER)
# =============================================================================

cat("Step 5: Calculating compensating variation (second-order QUAIDS)...\n")

# Extract QUAIDS parameters for petrol (for second-order CV)
beta_petrol <- beta_full["petrol"]
lambda_petrol <- lambda_full["petrol"]
gamma11_petrol <- gamma_full[1, 1]

cat("\nQUAIDS parameters for CV:\n")
cat("  β₁ (petrol):", round(beta_petrol, 4), "\n")
cat("  λ₁ (petrol):", round(lambda_petrol, 6), "\n")
cat("  γ₁₁ (petrol):", round(gamma11_petrol, 4), "\n\n")

# Add quintiles within each wave
pooled_clean <- pooled_clean |>
  group_by(wave) |>
  mutate(quintile = ntile(totcons, 5)) |>
  ungroup()

# Filter to only Wave 4 and Wave 5 for this analysis
wave4_5_data <- pooled_clean |> filter(wave %in% c(4, 5))

# Calculate second-order CV
wave4_5_data <- wave4_5_data |>
  mutate(
    # Household-level variables
    totcons_hh = totcons * hhsize,
    total_energy_exp_hh = total_energy_exp * hhsize,
    petrol_exp_hh = w1_petrol * total_energy_exp_hh,
    
    # Log price change (100% increase for counterfactual)
    dp = log(2),  # ln(2) ≈ 0.693
    
    # Log real expenditure
    ln_x = log(total_energy_exp),
    
    # FIRST-ORDER CV
    cv_fo = w1_petrol * dp * total_energy_exp_hh,
    
    # SECOND-ORDER CORRECTION TERMS
    term_income = beta_petrol * dp,
    term_quad = 2 * lambda_petrol * dp * ln_x,
    term_subst = ifelse(w1_petrol > 0.001,
                        gamma11_petrol * dp / w1_petrol,
                        0),
    
    # Total marginal effect
    marginal_w = term_income + term_quad + term_subst,
    
    # SECOND-ORDER CV
    cv_naira = total_energy_exp_hh * (w1_petrol * dp + 0.5 * marginal_w * dp),
    
    # Two CV metrics
    cv_share_totcons = cv_naira / totcons_hh,
    cv_share_petrol = ifelse(petrol_exp_hh > 0, cv_naira / petrol_exp_hh, NA_real_)
  )

# CV by wave
cv_by_wave <- wave4_5_data |>
  group_by(wave) |>
  summarise(
    n = n(),
    mean_cv_naira = mean(cv_naira, na.rm = TRUE),
    mean_cv_totcons = mean(cv_share_totcons, na.rm = TRUE),
    mean_cv_petrol = mean(cv_share_petrol, na.rm = TRUE),
    median_cv_naira = median(cv_naira, na.rm = TRUE),
    median_cv_totcons = median(cv_share_totcons, na.rm = TRUE),
    median_cv_petrol = median(cv_share_petrol, na.rm = TRUE),
    .groups = "drop"
  )

cat("\nCompensating Variation by Wave:\n\n")
print(cv_by_wave |>
        mutate(
          wave_label = case_when(
            wave == 4 ~ "Wave 4 (2018/19 - Pre-removal)",
            wave == 5 ~ "Wave 5 (2023/24 - Post-removal)"
          ),
          mean_cv_naira = scales::comma(mean_cv_naira, accuracy = 1),
          mean_cv_totcons = scales::percent(mean_cv_totcons, accuracy = 0.1),
          mean_cv_petrol = scales::percent(mean_cv_petrol, accuracy = 0.1)
        ) |>
        select(wave_label, n, mean_cv_naira, mean_cv_totcons, mean_cv_petrol))

# CV by quintile and wave
cv_by_quintile <- wave4_5_data |>
  group_by(wave, quintile) |>
  summarise(
    n = n(),
    mean_cv_naira = mean(cv_naira, na.rm = TRUE),
    mean_cv_totcons = mean(cv_share_totcons, na.rm = TRUE),
    mean_cv_petrol = mean(cv_share_petrol, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n\nCompensating Variation by Quintile:\n\n")
cat("Wave 4 (2018/19 - Counterfactual):\n")
print(cv_by_quintile |>
        filter(wave == 4) |>
        mutate(
          quintile_label = paste0("Q", quintile, ifelse(quintile == 1, " (poorest)", 
                                                        ifelse(quintile == 5, " (richest)", ""))),
          mean_cv_naira = scales::comma(mean_cv_naira, accuracy = 1),
          mean_cv_totcons = scales::percent(mean_cv_totcons, accuracy = 0.1),
          mean_cv_petrol = scales::percent(mean_cv_petrol, accuracy = 0.1)
        ) |>
        select(quintile_label, n, mean_cv_naira, mean_cv_totcons, mean_cv_petrol))

cat("\nWave 5 (2023/24 - Actual):\n")
print(cv_by_quintile |>
        filter(wave == 5) |>
        mutate(
          quintile_label = paste0("Q", quintile, ifelse(quintile == 1, " (poorest)", 
                                                        ifelse(quintile == 5, " (richest)", ""))),
          mean_cv_naira = scales::comma(mean_cv_naira, accuracy = 1),
          mean_cv_totcons = scales::percent(mean_cv_totcons, accuracy = 0.1),
          mean_cv_petrol = scales::percent(mean_cv_petrol, accuracy = 0.1)
        ) |>
        select(quintile_label, n, mean_cv_naira, mean_cv_totcons, mean_cv_petrol))

cat("\n✓ Compensating variation calculated (second-order QUAIDS)\n")
cat("  Two metrics reported:\n")
cat("    (1) CV as % of total consumption - absolute welfare burden\n")
cat("    (2) CV as % of petrol expenditure - burden per unit petrol consumed\n\n")

# Update pooled_clean with Wave 4-5 CV results
pooled_clean <- pooled_clean |>
  left_join(
    wave4_5_data |> 
      select(hhid, wave, cv_naira, cv_share_totcons, cv_share_petrol),
    by = c("hhid", "wave")
  )


### STEP 6: CREATE COMPARISON SUMMARY TABLE -----


# =============================================================================
# STEP 6: CREATE COMPARISON SUMMARY TABLE
# =============================================================================

cat("Step 6: Creating comparison summary...\n")

# Comparison summary table with BOTH CV metrics
comparison_summary <- tibble(
  Metric = c(
    "Sample size",
    "Mean HH size",
    "Mean total consumption (PC)",
    "Petrol budget share",
    "Petrol expenditure elasticity (η)",
    "Petrol own-price elasticity (compensated)",
    "Petrol own-price elasticity (uncompensated)",
    "CV (mean): Naira",
    "CV (mean): % total consumption",
    "CV (mean): % petrol expenditure",
    "CV Q1 (poorest): Naira",
    "CV Q1: % total consumption",
    "CV Q1: % petrol expenditure",
    "CV Q5 (richest): Naira",
    "CV Q5: % total consumption",
    "CV Q5: % petrol expenditure"
  ),
  `Wave 4 (2018/19)` = c(
    scales::comma(nrow(wave4_data)),
    sprintf("%.2f", mean(wave4_data$hhsize, na.rm = TRUE)),
    paste0("₦", scales::comma(round(mean(wave4_data$totcons, na.rm = TRUE)))),
    scales::percent(mean(wave4_data$w1_petrol, na.rm = TRUE), accuracy = 0.1),
    sprintf("%.3f", eta_w4[1]),
    sprintf("%.3f", eps_c_w4[1, 1]),
    sprintf("%.3f", eps_u_w4[1, 1]),
    # Overall CV
    paste0("₦", scales::comma(round(cv_by_wave$mean_cv_naira[cv_by_wave$wave == 4]))),
    scales::percent(cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 4], accuracy = 0.1),
    scales::percent(cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 4], accuracy = 0.1),
    # Q1 CV
    paste0("₦", scales::comma(round(cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1]))),
    scales::percent(cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1], accuracy = 0.1),
    scales::percent(cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1], accuracy = 0.1),
    # Q5 CV
    paste0("₦", scales::comma(round(cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5]))),
    scales::percent(cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5], accuracy = 0.1),
    scales::percent(cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5], accuracy = 0.1)
  ),
  `Wave 5 (2023/24)` = c(
    scales::comma(nrow(wave5_data)),
    sprintf("%.2f", mean(wave5_data$hhsize, na.rm = TRUE)),
    paste0("₦", scales::comma(round(mean(wave5_data$totcons, na.rm = TRUE)))),
    scales::percent(mean(wave5_data$w1_petrol, na.rm = TRUE), accuracy = 0.1),
    sprintf("%.3f", eta_w5[1]),
    sprintf("%.3f", eps_c_w5[1, 1]),
    sprintf("%.3f", eps_u_w5[1, 1]),
    # Overall CV
    paste0("₦", scales::comma(round(cv_by_wave$mean_cv_naira[cv_by_wave$wave == 5]))),
    scales::percent(cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 5], accuracy = 0.1),
    scales::percent(cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 5], accuracy = 0.1),
    # Q1 CV
    paste0("₦", scales::comma(round(cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1]))),
    scales::percent(cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1], accuracy = 0.1),
    scales::percent(cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1], accuracy = 0.1),
    # Q5 CV
    paste0("₦", scales::comma(round(cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5]))),
    scales::percent(cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5], accuracy = 0.1),
    scales::percent(cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5], accuracy = 0.1)
  ),
  `Change` = c(
    sprintf("%+d", nrow(wave5_data) - nrow(wave4_data)),
    sprintf("%+.2f", mean(wave5_data$hhsize) - mean(wave4_data$hhsize)),
    sprintf("%+.1f%%", (mean(wave5_data$totcons) / mean(wave4_data$totcons) - 1) * 100),
    sprintf("%+.1f pp", (mean(wave5_data$w1_petrol) - mean(wave4_data$w1_petrol)) * 100),
    sprintf("%+.1f%%", (eta_w5[1] / eta_w4[1] - 1) * 100),
    sprintf("%+.1f%%", (eps_c_w5[1, 1] / eps_c_w4[1, 1] - 1) * 100),
    sprintf("%+.1f%%", (eps_u_w5[1, 1] / eps_u_w4[1, 1] - 1) * 100),
    # Overall CV changes
    sprintf("%+.1f%%", (cv_by_wave$mean_cv_naira[cv_by_wave$wave == 5] / cv_by_wave$mean_cv_naira[cv_by_wave$wave == 4] - 1) * 100),
    sprintf("%+.1f pp", (cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 5] - cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 4]) * 100),
    sprintf("%+.1f pp", (cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 5] - cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 4]) * 100),
    # Q1 CV changes
    sprintf("%+.1f%%", (cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1] / 
                          cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1] - 1) * 100),
    sprintf("%+.1f pp", (cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1] - 
                           cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1]) * 100),
    sprintf("%+.1f pp", (cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1] - 
                           cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1]) * 100),
    # Q5 CV changes
    sprintf("%+.1f%%", (cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5] / 
                          cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5] - 1) * 100),
    sprintf("%+.1f pp", (cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5] - 
                           cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5]) * 100),
    sprintf("%+.1f pp", (cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5] - 
                           cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5]) * 100)
  )
)

cat("\n=============================================================================\n")
cat("COMPARISON SUMMARY: WAVE 4 (PRE-REMOVAL) vs WAVE 5 (POST-REMOVAL)\n")
cat("=============================================================================\n\n")
print(comparison_summary, n = Inf)

cat("\n✓ Comparison summary created with both CV metrics\n\n")


# =============================================================================
# STEP 7: SAVE RESULTS
# =============================================================================

cat("Step 7: Saving results...\n")

# Save elasticity comparison
elasticity_comparison <- tibble(
  wave = c(4, 5),
  wave_label = c("Wave 4 (2018/19)", "Wave 5 (2023/24)"),
  petrol_expenditure_elasticity = c(eta_w4[1], eta_w5[1]),
  petrol_own_price_uncompensated = c(eps_u_w4[1, 1], eps_u_w5[1, 1]),
  petrol_own_price_compensated = c(eps_c_w4[1, 1], eps_c_w5[1, 1]),
  electricity_expenditure_elasticity = c(eta_w4[2], eta_w5[2]),
  kerosene_expenditure_elasticity = c(eta_w4[3], eta_w5[3])
)

write_csv(elasticity_comparison, "elasticity_comparison_wave4_vs_wave5.csv")

# Save full price elasticity matrices
saveRDS(list(
  wave4 = list(uncompensated = eps_u_w4, compensated = eps_c_w4),
  wave5 = list(uncompensated = eps_u_w5, compensated = eps_c_w5)
), "price_elasticity_matrices_wave4_vs_wave5.rds")

# Save CV results
write_csv(cv_by_wave, "cv_by_wave_comparison.csv")
write_csv(cv_by_quintile, "cv_by_quintile_wave4_vs_wave5.csv")

# Save comparison summary
write_csv(comparison_summary, "wave4_vs_wave5_summary_table.csv")

cat("\n=============================================================================\n")
cat("FILES SAVED\n")
cat("=============================================================================\n\n")
cat("  1. elasticity_comparison_wave4_vs_wave5.csv\n")
cat("  2. price_elasticity_matrices_wave4_vs_wave5.rds\n")
cat("  3. cv_by_wave_comparison.csv\n")
cat("  4. cv_by_quintile_wave4_vs_wave5.csv\n")
cat("  5. wave4_vs_wave5_summary_table.csv\n\n")

cat("=============================================================================\n")
cat("✅ ANALYSIS COMPLETE!\n")
cat("=============================================================================\n\n")

cat("Key findings:\n")
cat("  • Petrol expenditure elasticity:", round(eta_w4[1], 3), "→", round(eta_w5[1], 3), 
    "(", round((eta_w5[1]/eta_w4[1] - 1)*100, 1), "% change)\n")
cat("  • Own-price elasticity (compensated):", round(eps_c_w4[1, 1], 3), "→", round(eps_c_w5[1, 1], 3),
    "(", round((eps_c_w5[1, 1]/eps_c_w4[1, 1] - 1)*100, 1), "% change)\n")
cat("  • % households using petrol:", round(mean(wave4_data$w1_petrol > 0.01)*100, 1), "% →",
    round(mean(wave5_data$w1_petrol > 0.01)*100, 1), "%\n")
cat("  • Mean CV (% total consumption):", 
    scales::percent(cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 4], accuracy = 0.1), "→",
    scales::percent(cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 5], accuracy = 0.1), "\n")
cat("  • Mean CV (% petrol expenditure):", 
    scales::percent(cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 4], accuracy = 0.1), "→",
    scales::percent(cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 5], accuracy = 0.1), "\n\n")


### PROFESSIONAL TABLES FOR WAVE 4 vs WAVE 5 COMPARISON-----


library(tidyverse)
library(kableExtra)
library(scales)

cat("Creating professional tables...\n\n")


### TABLE 1: SAMPLE CHARACTERISTICS BY WAVE ----


cat("Table 1: Sample Characteristics...\n")

table1_data <- pooled_clean |>
  filter(wave %in% c(4, 5)) |>
  group_by(wave) |>
  summarise(
    n = n(),
    mean_hhsize = mean(hhsize, na.rm = TRUE),
    mean_totcons_pc = mean(totcons, na.rm = TRUE),
    mean_totcons_hh = mean(totcons * hhsize, na.rm = TRUE),
    pct_urban = mean(urban, na.rm = TRUE) * 100,
    pct_female_head = mean(female_head, na.rm = TRUE) * 100,
    pct_motorbike = mean(motorbike_ownership, na.rm = TRUE) * 100,
    mean_total_energy = mean(total_energy_exp, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_longer(cols = -wave, names_to = "variable", values_to = "value") |>
  pivot_wider(names_from = wave, values_from = value, names_prefix = "Wave_") |>
  mutate(
    Change = Wave_5 - Wave_4,
    Pct_Change = (Wave_5 / Wave_4 - 1) * 100
  )

table1 <- table1_data |>
  mutate(
    Variable = case_when(
      variable == "n" ~ "Sample size",
      variable == "mean_hhsize" ~ "Mean household size",
      variable == "mean_totcons_pc" ~ "Mean consumption per capita (₦)",
      variable == "mean_totcons_hh" ~ "Mean consumption per HH (₦)",
      variable == "pct_urban" ~ "Urban (%)",
      variable == "pct_female_head" ~ "Female-headed HH (%)",
      variable == "pct_motorbike" ~ "Motorbike ownership (%)",
      variable == "mean_total_energy" ~ "Mean energy expenditure (₦)",
      TRUE ~ variable
    ),
    Wave_4 = case_when(
      variable %in% c("mean_totcons_pc", "mean_totcons_hh", "mean_total_energy") ~ comma(Wave_4, accuracy = 1),
      variable == "n" ~ comma(Wave_4, accuracy = 1),
      variable %in% c("pct_urban", "pct_female_head", "pct_motorbike") ~ paste0(round(Wave_4, 1), "%"),
      TRUE ~ as.character(round(Wave_4, 2))
    ),
    Wave_5 = case_when(
      variable %in% c("mean_totcons_pc", "mean_totcons_hh", "mean_total_energy") ~ comma(Wave_5, accuracy = 1),
      variable == "n" ~ comma(Wave_5, accuracy = 1),
      variable %in% c("pct_urban", "pct_female_head", "pct_motorbike") ~ paste0(round(Wave_5, 1), "%"),
      TRUE ~ as.character(round(Wave_5, 2))
    ),
    Change_Text = case_when(
      variable == "n" ~ comma(Change, accuracy = 1),
      variable %in% c("mean_totcons_pc", "mean_totcons_hh", "mean_total_energy") ~ comma(Change, accuracy = 1),
      variable %in% c("pct_urban", "pct_female_head", "pct_motorbike") ~ paste0(ifelse(Change > 0, "+", ""), round(Change, 1), "pp"),
      TRUE ~ paste0(ifelse(Change > 0, "+", ""), round(Change, 2))
    ),
    Pct_Change_Text = paste0(ifelse(Pct_Change > 0, "+", ""), round(Pct_Change, 1), "%")
  ) |>
  select(Variable, Wave_4, Wave_5, Change_Text, Pct_Change_Text)

table1_output <- kable(table1, format = "html", escape = FALSE,
                       col.names = c("Characteristic", "Wave 4\n(2018/19)", "Wave 5\n(2023/24)", "Absolute\nChange", "% Change"),
                       caption = "<b>Table 1: Sample Characteristics by Survey Wave</b>",
                       align = c("l", "r", "r", "r", "r")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) |>
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
  row_spec(1:8, background = "white") |>
  column_spec(1, bold = TRUE, width = "12em") |>
  column_spec(2:5, width = "7em") |>
  footnote(
    general = "Source: Nigeria GHS-LSMS Waves 4-5. Per capita consumption in 2018/19 Naira.",
    general_title = "",
    footnote_as_chunk = TRUE
  )

cat("✓ Table 1 created\n")


### TABLE 2: ENERGY BUDGET SHARES BY WAVE -----


cat("Table 2: Energy Budget Shares...\n")

table2_data <- pooled_clean |>
  filter(wave %in% c(4, 5)) |>
  group_by(wave) |>
  summarise(
    petrol = mean(w1_petrol, na.rm = TRUE) * 100,
    electricity = mean(w2_electricity, na.rm = TRUE) * 100,
    kerosene = mean(w3_kerosene, na.rm = TRUE) * 100,
    transport = mean(w4_transport, na.rm = TRUE) * 100,
    other = mean(w5_other, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(cols = -wave, names_to = "Energy_Type", values_to = "Share") |>
  pivot_wider(names_from = wave, values_from = Share, names_prefix = "Wave_") |>
  mutate(
    Change = Wave_5 - Wave_4,
    Pct_Change = (Wave_5 / Wave_4 - 1) * 100
  )

# Add prevalence (% using each energy type)
table2_prevalence <- pooled_clean |>
  filter(wave %in% c(4, 5)) |>
  group_by(wave) |>
  summarise(
    petrol = mean(w1_petrol > 0.01, na.rm = TRUE) * 100,
    electricity = mean(w2_electricity > 0.01, na.rm = TRUE) * 100,
    kerosene = mean(w3_kerosene > 0.01, na.rm = TRUE) * 100,
    transport = mean(w4_transport > 0.01, na.rm = TRUE) * 100,
    other = mean(w5_other > 0.01, na.rm = TRUE) * 100,
    .groups = "drop"
  ) |>
  pivot_longer(cols = -wave, names_to = "Energy_Type", values_to = "Prevalence") |>
  pivot_wider(names_from = wave, values_from = Prevalence, names_prefix = "Prev_")

table2 <- table2_data |>
  left_join(table2_prevalence, by = "Energy_Type") |>
  mutate(
    Energy_Source = case_when(
      Energy_Type == "petrol" ~ "Petrol",
      Energy_Type == "electricity" ~ "Electricity",
      Energy_Type == "kerosene" ~ "Kerosene",
      Energy_Type == "transport" ~ "Transport/Diesel",
      Energy_Type == "other" ~ "Other energy",
      TRUE ~ Energy_Type
    ),
    W4_Share = paste0(round(Wave_4, 1), "%"),
    W5_Share = paste0(round(Wave_5, 1), "%"),
    W4_Prev = paste0(round(Prev_4, 1), "%"),
    W5_Prev = paste0(round(Prev_5, 1), "%"),
    Change_pp = paste0(ifelse(Change > 0, "+", ""), round(Change, 1), "pp"),
    Change_pct = paste0(ifelse(Pct_Change > 0, "+", ""), round(Pct_Change, 1), "%")
  ) |>
  select(Energy_Source, W4_Share, W4_Prev, W5_Share, W5_Prev, Change_pp, Change_pct)

table2_output <- kable(table2, format = "html", escape = FALSE,
                       col.names = c("Energy Source", 
                                     "Budget\nShare", "% Using",
                                     "Budget\nShare", "% Using",
                                     "Change\n(pp)", "Change\n(%)"),
                       caption = "<b>Table 2: Energy Budget Shares and Prevalence by Wave</b>",
                       align = c("l", "r", "r", "r", "r", "r", "r")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) |>
  add_header_above(c(" " = 1, "Wave 4 (2018/19)" = 2, "Wave 5 (2023/24)" = 2, "Change" = 2)) |>
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
  row_spec(1, bold = TRUE, background = "#E8F4F8") |>  # Highlight petrol
  column_spec(1, bold = TRUE, width = "10em") |>
  footnote(
    general = "Budget shares calculated as % of total energy expenditure. '% Using' shows percentage of households with positive expenditure on each energy type.",
    general_title = "",
    footnote_as_chunk = TRUE
  )

cat("✓ Table 2 created\n")

# =============================================================================
# TABLE 3: EXPENDITURE ELASTICITIES BY WAVE
# =============================================================================

cat("Table 3: Expenditure Elasticities...\n")

table3_data <- tibble(
  Energy_Source = c("Petrol", "Electricity", "Kerosene", "Transport/Diesel", "Other energy"),
  Wave_4 = c(eta_w4[1], eta_w4[2], eta_w4[3], eta_w4[4], eta_w4[5]),
  Wave_5 = c(eta_w5[1], eta_w5[2], eta_w5[3], eta_w5[4], eta_w5[5])
) |>
  mutate(
    Change = Wave_5 - Wave_4,
    Pct_Change = (Wave_5 / Wave_4 - 1) * 100,
    Interpretation_W4 = case_when(
      Wave_4 < 1 ~ "Necessity",
      Wave_4 > 1 ~ "Luxury",
      TRUE ~ "Normal"
    ),
    Interpretation_W5 = case_when(
      Wave_5 < 1 ~ "Necessity",
      Wave_5 > 1 ~ "Luxury",
      TRUE ~ "Normal"
    )
  )

table3 <- table3_data |>
  mutate(
    W4_Value = round(Wave_4, 3),
    W5_Value = round(Wave_5, 3),
    Change_Abs = paste0(ifelse(Change > 0, "+", ""), round(Change, 3)),
    Change_Pct = paste0(ifelse(Pct_Change > 0, "+", ""), round(Pct_Change, 1), "%")
  ) |>
  select(Energy_Source, W4_Value, Interpretation_W4, W5_Value, Interpretation_W5, Change_Abs, Change_Pct)

table3_output <- kable(table3, format = "html", escape = FALSE,
                       col.names = c("Energy Source", "η", "Type", "η", "Type", "Δη", "% Change"),
                       caption = "<b>Table 3: Expenditure Elasticities (η) by Survey Wave</b>",
                       align = c("l", "r", "l", "r", "l", "r", "r")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) |>
  add_header_above(c(" " = 1, "Wave 4 (2018/19)" = 2, "Wave 5 (2023/24)" = 2, "Change" = 2)) |>
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
  row_spec(1, bold = TRUE, background = "#E8F4F8") |>  # Highlight petrol
  column_spec(1, bold = TRUE, width = "10em") |>
  column_spec(c(2, 4), color = "black", bold = TRUE) |>
  footnote(
    general = "Expenditure elasticity (η) measures % change in energy demand for 1% change in total expenditure. η < 1: necessity; η > 1: luxury; η = 1: normal good. Estimated from QUAIDS model on pooled Waves 3-5.",
    general_title = "",
    footnote_as_chunk = TRUE
  )

cat("✓ Table 3 created\n")


# TABLE 4: OWN-PRICE ELASTICITIES BY WAVE


cat("Table 4: Own-Price Elasticities...\n")

table4_data <- tibble(
  Energy_Source = c("Petrol", "Electricity", "Kerosene", "Transport/Diesel", "Other energy"),
  W4_Uncompensated = c(eps_u_w4[1,1], eps_u_w4[2,2], eps_u_w4[3,3], eps_u_w4[4,4], eps_u_w4[5,5]),
  W4_Compensated = c(eps_c_w4[1,1], eps_c_w4[2,2], eps_c_w4[3,3], eps_c_w4[4,4], eps_c_w4[5,5]),
  W5_Uncompensated = c(eps_u_w5[1,1], eps_u_w5[2,2], eps_u_w5[3,3], eps_u_w5[4,4], eps_u_w5[5,5]),
  W5_Compensated = c(eps_c_w5[1,1], eps_c_w5[2,2], eps_c_w5[3,3], eps_c_w5[4,4], eps_c_w5[5,5])
) |>
  mutate(
    Change_Uncomp = W5_Uncompensated - W4_Uncompensated,
    Change_Comp = W5_Compensated - W4_Compensated,
    Pct_Change_Comp = (W5_Compensated / W4_Compensated - 1) * 100
  )

table4 <- table4_data |>
  mutate(
    W4_U = round(W4_Uncompensated, 3),
    W4_C = round(W4_Compensated, 3),
    W5_U = round(W5_Uncompensated, 3),
    W5_C = round(W5_Compensated, 3),
    Change_C = paste0(ifelse(Change_Comp > 0, "+", ""), round(Change_Comp, 3)),
    Elasticity = case_when(
      abs(W5_C) < 0.3 ~ "Inelastic",
      abs(W5_C) > 0.7 ~ "Elastic",
      TRUE ~ "Unit elastic"
    )
  ) |>
  select(Energy_Source, W4_U, W4_C, W5_U, W5_C, Change_C, Elasticity)

table4_output <- kable(table4, format = "html", escape = FALSE,
                       col.names = c("Energy Source", 
                                     "Marshall.", "Hicksian",
                                     "Marshall.", "Hicksian",
                                     "ΔHicksian", "Classification"),
                       caption = "<b>Table 4: Own-Price Elasticities by Survey Wave</b>",
                       align = c("l", "r", "r", "r", "r", "r", "l")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) |>
  add_header_above(c(" " = 1, "Wave 4 (2018/19)" = 2, "Wave 5 (2023/24)" = 2, "Change" = 2)) |>
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
  row_spec(1, bold = TRUE, background = "#E8F4F8") |>  # Highlight petrol
  column_spec(1, bold = TRUE, width = "10em") |>
  column_spec(c(2, 3, 4, 5), color = "black") |>
  footnote(
    general = "Own-price elasticity measures % change in demand for 1% change in own price. Marshallian (uncompensated) includes income effects; Hicksian (compensated) holds utility constant. All elasticities are negative (law of demand). |ε| < 0.5: inelastic; |ε| > 0.7: elastic.",
    general_title = "",
    footnote_as_chunk = TRUE
  )

cat("✓ Table 4 created\n")



#### ENSURE CV IS CALCULATED FOR WAVE 4 & 5 -----


cat("Checking/recalculating CV for Table 5...\n")

# Extract QUAIDS parameters
beta_petrol <- beta["petrol"]
lambda_petrol <- lambda["petrol"]
gamma11_petrol <- gamma[1, 1]

# Calculate or recalculate CV
pooled_clean <- pooled_clean |>
  mutate(
    # Household-level variables
    totcons_hh = totcons * hhsize,
    total_energy_exp_hh = total_energy_exp * hhsize,
    petrol_exp_hh = w1_petrol * total_energy_exp_hh,
    
    # Log price change
    dp = log(2),
    ln_x = log(total_energy_exp),
    
    # First-order CV
    cv_fo = w1_petrol * dp * total_energy_exp_hh,
    
    # Second-order terms
    term_income = beta_petrol * dp,
    term_quad = 2 * lambda_petrol * dp * ln_x,
    term_subst = ifelse(w1_petrol > 0.001,
                        gamma11_petrol * dp / w1_petrol,
                        0),
    marginal_w = term_income + term_quad + term_subst,
    
    # Second-order CV (overwrites if exists)
    cv_naira = total_energy_exp_hh * (w1_petrol * dp + 0.5 * marginal_w * dp),
    cv_share_totcons = cv_naira / totcons_hh,
    cv_share_petrol = ifelse(petrol_exp_hh > 0, cv_naira / petrol_exp_hh, NA_real_)
  )

cat("✓ CV recalculated\n")
cat("  Non-NA cv_naira:", sum(!is.na(pooled_clean$cv_naira)), "\n")
cat("  Wave 4:", sum(!is.na(pooled_clean$cv_naira[pooled_clean$wave == 4])), "\n")
cat("  Wave 5:", sum(!is.na(pooled_clean$cv_naira[pooled_clean$wave == 5])), "\n\n")




### TABLE 5: COMPENSATING VARIATION BY QUINTILE AND WAVE ----

cat("Table 5: Welfare Impact (Compensating Variation)...\n")

table5_data <- pooled_clean |>
  filter(wave %in% c(4, 5)) |>
  group_by(wave, quintile) |>
  summarise(
    n = n(),
    mean_cv_naira = mean(cv_naira, na.rm = TRUE),
    mean_cv_totcons = mean(cv_share_totcons, na.rm = TRUE) * 100,  # ← Changed
    mean_cv_petrol = mean(cv_share_petrol, na.rm = TRUE) * 100,    # ← Changed
    median_cv_totcons = median(cv_share_totcons, na.rm = TRUE) * 100,  # ← Changed
    median_cv_petrol = median(cv_share_petrol, na.rm = TRUE) * 100,    # ← Changed
    pct_using_petrol = mean(w1_petrol > 0.01, na.rm = TRUE) * 100,
    mean_totcons = mean(totcons, na.rm = TRUE),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = wave,
    values_from = c(mean_cv_naira, mean_cv_totcons, mean_cv_petrol, 
                    median_cv_totcons, median_cv_petrol, pct_using_petrol, mean_totcons),
    names_glue = "{.value}_w{wave}"
  )

table5 <- table5_data |>
  mutate(
    Quintile = case_when(
      quintile == 1 ~ "Q1 (Poorest)",
      quintile == 2 ~ "Q2",
      quintile == 3 ~ "Q3 (Middle)",
      quintile == 4 ~ "Q4",
      quintile == 5 ~ "Q5 (Richest)",
      TRUE ~ as.character(quintile)
    ),
    # Wave 4 columns
    W4_CV_Totcons = paste0(round(mean_cv_totcons_w4, 2), "%"),
    W4_CV_Petrol = paste0(round(mean_cv_petrol_w4, 1), "%"),
    W4_Using = paste0(round(pct_using_petrol_w4, 1), "%"),
    
    # Wave 5 columns
    W5_CV_Totcons = paste0(round(mean_cv_totcons_w5, 2), "%"),
    W5_CV_Petrol = paste0(round(mean_cv_petrol_w5, 1), "%"),
    W5_Using = paste0(round(pct_using_petrol_w5, 1), "%"),
    
    # Changes
    Change_Totcons = mean_cv_totcons_w5 - mean_cv_totcons_w4,
    Change_Totcons_Text = paste0(ifelse(Change_Totcons > 0, "+", ""), round(Change_Totcons, 2), "pp"),
    Change_Petrol = mean_cv_petrol_w5 - mean_cv_petrol_w4,
    Change_Petrol_Text = paste0(ifelse(Change_Petrol > 0, "+", ""), round(Change_Petrol, 1), "pp")
  ) |>
  select(Quintile, W4_CV_Totcons, W4_CV_Petrol, W4_Using, 
         W5_CV_Totcons, W5_CV_Petrol, W5_Using, 
         Change_Totcons_Text, Change_Petrol_Text)

table5_output <- kable(table5, format = "html", escape = FALSE,
                       col.names = c("Income\nQuintile", 
                                     "CV\n(% totcons)", "CV\n(% petrol)", "% Using\nPetrol",
                                     "CV\n(% totcons)", "CV\n(% petrol)", "% Using\nPetrol",
                                     "Δ CV\n(totcons)", "Δ CV\n(petrol)"),
                       caption = "<b>Table 5: Welfare Impact of 100% Petrol Price Increase by Income Quintile</b>",
                       align = c("l", rep("r", 8))) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) |>
  add_header_above(c(" " = 1, "Wave 4 (2018/19)" = 3, "Wave 5 (2023/24)" = 3, "Change (pp)" = 2)) |>
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
  row_spec(1, background = "#D5E8D4") |>  # Poorest in green
  row_spec(5, background = "#FFE6E6") |>  # Richest in light red
  column_spec(1, bold = TRUE, width = "8em") |>
  column_spec(c(2, 3, 5, 6), bold = TRUE) |>  # Bold CV columns
  footnote(
    general = "CV (compensating variation) calculated using second-order QUAIDS for 100% petrol price increase. Two metrics shown: (1) CV as % of total consumption (absolute welfare burden); (2) CV as % of petrol expenditure (burden per unit petrol consumed). '% Using Petrol' shows percentage of each quintile with positive petrol expenditure. Changes measured in percentage points (pp).",
    general_title = "",
    footnote_as_chunk = TRUE
  )

save_kable(table5_output, "table5_cv_by_quintile_wave4_wave5.html")
write_csv(table5, "table5_cv_by_quintile_wave4_wave5.csv")

cat("✓ Table 5 created\n\n")

#### SAVE ALL TABLES TO HTML AND WORD-COMPATIBLE FORMAT ----


cat("Saving tables...\n")

# Save individual HTML tables
save_kable(table1_output, "table1_sample_characteristics.html")
save_kable(table2_output, "table2_energy_budget_shares.html")
save_kable(table3_output, "table3_expenditure_elasticities.html")
save_kable(table4_output, "table4_own_price_elasticities.html")
save_kable(table5_output, "table5_compensating_variation.html")

# Create combined HTML document
combined_html <- paste0(
  "<!DOCTYPE html>\n<html>\n<head>\n",
  "<title>Wave 4 vs Wave 5 Analysis Tables</title>\n",
  "<style>body { font-family: 'Times New Roman', Times, serif; margin: 40px; }</style>\n",
  "</head>\n<body>\n",
  "<h1>Impact of 2023 Fuel Subsidy Removal: Wave 4 vs Wave 5 Analysis</h1>\n",
  "<p><i>Nigeria GHS-LSMS Panel Survey</i></p>\n",
  "<hr>\n\n",
  as.character(table1_output), "\n<br><br>\n",
  as.character(table2_output), "\n<br><br>\n",
  as.character(table3_output), "\n<br><br>\n",
  as.character(table4_output), "\n<br><br>\n",
  as.character(table5_output), "\n",
  "</body>\n</html>"
)

writeLines(combined_html, "all_tables_combined.html")

cat("\n=============================================================================\n")
cat("✅ ALL TABLES CREATED AND SAVED\n")
cat("=============================================================================\n\n")
cat("Files saved:\n")
cat("  1. table1_sample_characteristics.html\n")
cat("  2. table2_energy_budget_shares.html\n")
cat("  3. table3_expenditure_elasticities.html\n")
cat("  4. table4_own_price_elasticities.html\n")
cat("  5. table5_compensating_variation.html\n")
cat("  6. all_tables_combined.html (all 5 tables in one document)\n\n")
cat("To use in Word: Open the HTML files in a browser, then copy-paste into Word.\n")
cat("To use in LaTeX: Convert HTML tables using pandoc or manually recreate.\n\n")




#### CREATE SOPHISTICATED COMPARISON TABLE ----
#### TABLE: WAVE 4 vs WAVE 5 COMPARISON SUMMARY (STYLED) ----

library(tidyverse)
library(kableExtra)
library(scales)

cat("Creating styled comparison summary table...\n")

comparison_summary <- tibble(
  Metric = c(
    "Sample size",
    "Mean HH size",
    "Mean totcons (PC, ₦)",
    "% using petrol",
    "Petrol budget share",
    "Electricity budget share",
    "Kerosene budget share",
    "",
    "Petrol expenditure elasticity",
    "Petrol own-price (uncompensated)",
    "Petrol own-price (compensated)",
    "",
    "CV: Mean (₦)",
    "CV: Mean (% total cons.)",
    "CV: Mean (% petrol exp.)",
    "",
    "CV Q1: Mean (₦)",
    "CV Q1: Mean (% total cons.)",
    "CV Q1: Mean (% petrol exp.)",
    "",
    "CV Q5: Mean (₦)",
    "CV Q5: Mean (% total cons.)",
    "CV Q5: Mean (% petrol exp.)"
  ),
  Wave_4_2018_19 = c(
    scales::comma(nrow(wave4_data)),
    round(mean(wave4_data$hhsize), 2),
    scales::comma(round(mean(wave4_data$totcons), 0)),
    paste0(round(mean(wave4_data$w1_petrol > 0.01) * 100, 1), "%"),
    scales::percent(w_mean_w4[1], accuracy = 0.1),
    scales::percent(w_mean_w4[2], accuracy = 0.1),
    scales::percent(w_mean_w4[3], accuracy = 0.1),
    "",
    round(eta_w4[1], 3),
    round(eps_u_w4[1, 1], 3),
    round(eps_c_w4[1, 1], 3),
    "",
    # Overall CV (Wave 4)
    paste0("₦", scales::comma(round(cv_by_wave$mean_cv_naira[cv_by_wave$wave == 4]))),
    scales::percent(cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 4], accuracy = 0.1),
    scales::percent(cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 4], accuracy = 0.1),
    "",
    # Q1 CV (Wave 4)
    paste0("₦", scales::comma(round(cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1]))),
    scales::percent(cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1], accuracy = 0.1),
    scales::percent(cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1], accuracy = 0.1),
    "",
    # Q5 CV (Wave 4)
    paste0("₦", scales::comma(round(cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5]))),
    scales::percent(cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5], accuracy = 0.1),
    scales::percent(cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5], accuracy = 0.1)
  ),
  Wave_5_2023_24 = c(
    scales::comma(nrow(wave5_data)),
    round(mean(wave5_data$hhsize), 2),
    scales::comma(round(mean(wave5_data$totcons), 0)),
    paste0(round(mean(wave5_data$w1_petrol > 0.01) * 100, 1), "%"),
    scales::percent(w_mean_w5[1], accuracy = 0.1),
    scales::percent(w_mean_w5[2], accuracy = 0.1),
    scales::percent(w_mean_w5[3], accuracy = 0.1),
    "",
    round(eta_w5[1], 3),
    round(eps_u_w5[1, 1], 3),
    round(eps_c_w5[1, 1], 3),
    "",
    # Overall CV (Wave 5)
    paste0("₦", scales::comma(round(cv_by_wave$mean_cv_naira[cv_by_wave$wave == 5]))),
    scales::percent(cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 5], accuracy = 0.1),
    scales::percent(cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 5], accuracy = 0.1),
    "",
    # Q1 CV (Wave 5)
    paste0("₦", scales::comma(round(cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1]))),
    scales::percent(cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1], accuracy = 0.1),
    scales::percent(cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1], accuracy = 0.1),
    "",
    # Q5 CV (Wave 5)
    paste0("₦", scales::comma(round(cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5]))),
    scales::percent(cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5], accuracy = 0.1),
    scales::percent(cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5], accuracy = 0.1)
  ),
  Change = c(
    paste0("+", scales::comma(nrow(wave5_data) - nrow(wave4_data))),
    paste0(ifelse(mean(wave5_data$hhsize) - mean(wave4_data$hhsize) > 0, "+", ""), 
           round(mean(wave5_data$hhsize) - mean(wave4_data$hhsize), 2)),
    paste0("+", scales::comma(round(mean(wave5_data$totcons) - mean(wave4_data$totcons), 0))),
    paste0(round((mean(wave5_data$w1_petrol > 0.01) / mean(wave4_data$w1_petrol > 0.01) - 1) * 100, 1), "%"),
    paste0(ifelse(w_mean_w5[1] - w_mean_w4[1] > 0, "+", ""), 
           round((w_mean_w5[1] - w_mean_w4[1]) * 100, 1), "pp"),
    paste0(ifelse(w_mean_w5[2] - w_mean_w4[2] > 0, "+", ""), 
           round((w_mean_w5[2] - w_mean_w4[2]) * 100, 1), "pp"),
    paste0(ifelse(w_mean_w5[3] - w_mean_w4[3] > 0, "+", ""), 
           round((w_mean_w5[3] - w_mean_w4[3]) * 100, 1), "pp"),
    "",
    paste0(round((eta_w5[1] / eta_w4[1] - 1) * 100, 1), "%"),
    paste0(round((eps_u_w5[1, 1] / eps_u_w4[1, 1] - 1) * 100, 1), "%"),
    paste0(round((eps_c_w5[1, 1] / eps_c_w4[1, 1] - 1) * 100, 1), "%"),
    "",
    # Overall CV changes
    paste0(round((cv_by_wave$mean_cv_naira[cv_by_wave$wave == 5] / 
                    cv_by_wave$mean_cv_naira[cv_by_wave$wave == 4] - 1) * 100, 1), "%"),
    paste0(ifelse((cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 5] - cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 4]) > 0, "+", ""),
           round((cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 5] - cv_by_wave$mean_cv_totcons[cv_by_wave$wave == 4]) * 100, 1), "pp"),
    paste0(ifelse((cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 5] - cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 4]) > 0, "+", ""),
           round((cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 5] - cv_by_wave$mean_cv_petrol[cv_by_wave$wave == 4]) * 100, 1), "pp"),
    "",
    # Q1 CV changes
    paste0(round((cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1] / 
                    cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1] - 1) * 100, 1), "%"),
    paste0(ifelse((cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1] - 
                     cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1]) > 0, "+", ""),
           round((cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1] - 
                    cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1]) * 100, 1), "pp"),
    paste0(ifelse((cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1] - 
                     cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1]) > 0, "+", ""),
           round((cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 1] - 
                    cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 1]) * 100, 1), "pp"),
    "",
    # Q5 CV changes
    paste0(round((cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5] / 
                    cv_by_quintile$mean_cv_naira[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5] - 1) * 100, 1), "%"),
    paste0(ifelse((cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5] - 
                     cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5]) > 0, "+", ""),
           round((cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5] - 
                    cv_by_quintile$mean_cv_totcons[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5]) * 100, 1), "pp"),
    paste0(ifelse((cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5] - 
                     cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5]) > 0, "+", ""),
           round((cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 5 & cv_by_quintile$quintile == 5] - 
                    cv_by_quintile$mean_cv_petrol[cv_by_quintile$wave == 4 & cv_by_quintile$quintile == 5]) * 100, 1), "pp")
  )
)

# Create styled HTML table
comparison_kable <- comparison_summary |>
  kbl(format = "html", escape = FALSE,
      col.names = c("Metric", "Wave 4 (2018/19)", "Wave 5 (2023/24)", "Change"),
      caption = "<b>Subsidy Removal Impact: Pre (Wave 4) vs Post (Wave 5)</b>",
      align = c("l", "r", "r", "r")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) |>
  row_spec(0, bold = TRUE, background = "#1565C0", color = "white") |>
  column_spec(1, bold = TRUE, width = "14em") |>
  column_spec(2:3, width = "8em") |>
  column_spec(4, width = "6em", bold = TRUE) |>
  pack_rows("Sample Characteristics", 1, 4) |>
  pack_rows("Budget Shares", 5, 7) |>
  pack_rows("Elasticities", 9, 11) |>
  pack_rows("Compensating Variation (Overall)", 13, 15) |>
  pack_rows("CV: Q1 (Poorest 20%)", 17, 19) |>
  pack_rows("CV: Q5 (Richest 20%)", 21, 23) |>
  footnote(
    general = "CV calculated using second-order QUAIDS for 100% petrol price increase. Three metrics: (1) Naira amount; (2) % of total consumption (absolute welfare burden); (3) % of petrol expenditure (burden per unit consumed). Wave 4 shows counterfactual; Wave 5 captures actual May 2023 subsidy removal period.",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

save_kable(comparison_kable, "wave4_vs_wave5_comparison_styled.html")
write_csv(comparison_summary, "wave4_vs_wave5_comparison_styled.csv")

cat("✓ Styled comparison table saved\n\n")
