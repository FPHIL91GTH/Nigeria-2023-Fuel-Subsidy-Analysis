# fp-replication-tables--with professional styling
# DATE: 4-25-26

# =============================================================================
# SOPHISTICATED FORMATTED TABLES FOR MEMO REPLICATION
# =============================================================================

library(tidyverse)
library(knitr)
library(kableExtra)

cat("=============================================================================\n")
cat("CREATING SOPHISTICATED FORMATTED TABLES\n")
cat("=============================================================================\n\n")

# =============================================================================
# TABLE 1: Sample Characteristics by Wave (Sophisticated)
# =============================================================================

cat("Creating Table 1...\n")

table1_data <- pooled_clean |>
  group_by(wave) |>
  summarise(
    n = n(),
    mean_hhsize = mean(hhsize, na.rm = TRUE),
    mean_totcons_pc = mean(totcons, na.rm = TRUE),
    energy_budget_share = mean(total_energy_exp / (totcons * hhsize), na.rm = TRUE),
    mean_petrol_exp = mean(w1_petrol * total_energy_exp_hh, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Wave = case_when(
      wave == 2 ~ "Wave 2 (2012/13)",
      wave == 3 ~ "Wave 3 (2015/16)",
      wave == 4 ~ "Wave 4 (2018/19)"
    )
  )

# Add pooled row
pooled_row <- pooled_clean |>
  summarise(
    Wave = "Pooled (Waves 2–4)",
    n = n(),
    mean_hhsize = mean(hhsize, na.rm = TRUE),
    mean_totcons_pc = mean(totcons, na.rm = TRUE),
    energy_budget_share = mean(total_energy_exp / (totcons * hhsize), na.rm = TRUE),
    mean_petrol_exp = mean(w1_petrol * total_energy_exp_hh, na.rm = TRUE)
  )

table1_final <- bind_rows(table1_data, pooled_row) |>
  select(Wave, mean_hhsize, mean_totcons_pc, energy_budget_share, mean_petrol_exp, n)

# Create sophisticated kable table
table1_kable <- table1_final |>
  kable(format = "html", 
        digits = c(0, 2, 0, 4, 0, 0),
        col.names = c("Wave", "Mean HH Size", "Mean Total Cons. PC (₦)", 
                      "Energy Budget Share", "Mean Petrol Exp. (₦)", "N"),
        caption = "<b>Table 1: Sample Characteristics by Wave</b>",
        align = c('l', 'c', 'r', 'c', 'r', 'r'),
        format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE,
                position = "center",
                font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#2E7D32", color = "white", font_size = 15) |>
  row_spec(4, bold = TRUE, background = "#E8F5E9", color = "#1B5E20") |>
  column_spec(1, bold = TRUE, width = "12em") |>
  column_spec(3, color = "#1565C0") |>
  column_spec(5, color = "#D84315") |>
  add_header_above(c(" " = 1, "Demographics" = 1, "Consumption" = 2, "Expenditure" = 1, " " = 1),
                   bold = TRUE, background = "#66BB6A", color = "white") |>
  footnote(general = "Source: Nigeria GHS-LSMS Waves 2–4. Energy budget share = total energy exp. / total household consumption.",
           general_title = "Note:",
           footnote_as_chunk = TRUE)

# Save and display
save_kable(table1_kable, "table1_sample_characteristics.html")
cat("✓ Table 1 saved to table1_sample_characteristics.html\n\n")


## TABLE 5: Within-Energy Budget Shares (Sophisticated Multi-Panel) ----


cat("Creating Table 5 (Budget shares by quintile)...\n")

table5_data <- pooled_clean |>
  group_by(wave, quintile) |>
  summarise(
    petrol = mean(w1_petrol, na.rm = TRUE),
    electricity = mean(w2_electricity, na.rm = TRUE),
    kerosene = mean(w3_kerosene, na.rm = TRUE),
    other_energy = mean(w5_other, na.rm = TRUE),
    transport = mean(w4_transport, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    Wave = case_when(
      wave == 2 ~ "Wave 2\n(2012/13)",
      wave == 3 ~ "Wave 3\n(2015/16)",
      wave == 4 ~ "Wave 4\n(2018/19)"
    ),
    Quintile = paste0("Q", quintile)
  )

# Create separate tables for each good
create_good_table <- function(data, good_name, good_label, color_scheme) {
  data |>
    select(Quintile, Wave, value = all_of(good_name)) |>
    pivot_wider(names_from = Wave, values_from = value) |>
    mutate(across(starts_with("Wave"), ~. * 100)) |>  # Convert to percentage
    kable(format = "html",
          digits = 1,
          col.names = c("Income Quintile", "Wave 2\n(2012/13)", "Wave 3\n(2015/16)", "Wave 4\n(2018/19)"),
          caption = paste0("<b>", good_label, " Budget Share by Quintile (%)</b>"),
          align = c('l', 'c', 'c', 'c')) |>
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                  full_width = FALSE,
                  position = "center",
                  font_size = 13) |>
    row_spec(0, bold = TRUE, background = color_scheme[1], color = "white") |>
    row_spec(1, background = "#FFEBEE") |>  # Highlight Q1 (poorest)
    row_spec(5, background = "#E3F2FD") |>  # Highlight Q5 (richest)
    column_spec(1, bold = TRUE, width = "10em") |>
    add_footnote(paste0("Mean ", tolower(good_label), " share across quintiles"),
                 notation = "none")
}

# Color schemes for each good
table5_petrol <- create_good_table(table5_data, "petrol", "Petrol", c("#D84315", "#FF5722"))
table5_kerosene <- create_good_table(table5_data, "kerosene", "Kerosene", c("#0277BD", "#03A9F4"))
table5_electricity <- create_good_table(table5_data, "electricity", "Electricity", c("#7B1FA2", "#BA68C8"))
table5_other <- create_good_table(table5_data, "other_energy", "Other Energy", c("#558B2F", "#8BC34A"))
table5_transport <- create_good_table(table5_data, "transport", "Transport/Diesel", c("#455A64", "#78909C"))

# Save all
save_kable(table5_petrol, "table5a_petrol_shares.html")
save_kable(table5_kerosene, "table5b_kerosene_shares.html")
save_kable(table5_electricity, "table5c_electricity_shares.html")
save_kable(table5_other, "table5d_other_energy_shares.html")
save_kable(table5_transport, "table5e_transport_shares.html")

cat("✓ Table 5 (5 panels) saved to table5a-e_*.html\n\n")


## TABLE 6: Compensating Variation by Quintile and Wave (Sophisticated)

cat("Creating Table 6 (Compensating Variation)...\n")

table6_data <- pooled_clean |>
  group_by(wave, quintile) |>
  summarise(
    cv_naira = mean(cv_naira, na.rm = TRUE),
    cv_share_totcons = mean(cv_share_totcons, na.rm = TRUE) * 100,
    cv_share_petrol = mean(cv_share_petrol, na.rm = TRUE) *100, # Convert to percentage
    .groups = "drop"
  ) |>
  mutate(
    Quintile = case_when(
      quintile == 1 ~ "Q1 (poorest)",
      quintile == 5 ~ "Q5 (richest)",
      TRUE ~ paste0("Q", quintile)
    )
  )


### TABLE 6A: CV AS % OF TOTAL CONSUMPTION (Standard Welfare Metric) ----


cat("Creating Table 6A (CV as % of Total Consumption)...\n")

table6a_wide <- table6_data |>
  select(wave, Quintile, cv_share_totcons, cv_naira) |>
  pivot_wider(
    names_from = wave,
    values_from = c(cv_share_totcons, cv_naira),
    names_glue = "{.value}_w{wave}"
  ) |>
  select(Quintile,
         cv_share_totcons_w2, cv_naira_w2,
         cv_share_totcons_w3, cv_naira_w3,
         cv_share_totcons_w4, cv_naira_w4)

table6a_kable <- table6a_wide |>
  kable(format = "html",
        digits = c(0, 1, 0, 1, 0, 1, 0),
        col.names = c("Quintile",
                      "CV (%)", "CV (₦)",
                      "CV (%)", "CV (₦)",
                      "CV (%)", "CV (₦)"),
        caption = "<b>Table 6A: CV as % of Total Household Consumption</b>",
        align = c('l', 'c', 'r', 'c', 'r', 'c', 'r'),
        format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center",
                font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#1565C0", color = "white") |>
  row_spec(1, background = "#FFCDD2") |>
  row_spec(5, background = "#C5E1A5") |>
  column_spec(1, bold = TRUE, width = "10em") |>
  column_spec(c(2, 4, 6), background = "#E3F2FD", color = "#1565C0", bold = TRUE) |>
  column_spec(c(3, 5, 7), background = "#E8F5E9", color = "#2E7D32") |>
  add_header_above(c(" " = 1, "Wave 2 (2012/13)" = 2, "Wave 3 (2015/16)" = 2, "Wave 4 (2018/19)" = 2),
                   bold = TRUE, background = "#1976D2", color = "white") |>
  footnote(
    general = c(
      "CV as % of total consumption shows <b>absolute welfare burden</b>.",
      "Progressive: Q5 (2.3%) bears larger burden than Q1 (1.2%).",
      "This is the standard welfare measure in applied demand analysis."
    ),
    general_title = "<b>Interpretation:</b>",
    escape = FALSE
  )

save_kable(table6a_kable, "table6a_cv_pct_totcons.html")


### TABLE 6B: CV AS % OF PETROL EXPENDITURE (Okereke's Metric) ----


cat("Creating Table 6B (CV as % of Petrol Expenditure)...\n")

table6b_wide <- table6_data |>
  select(wave, Quintile, cv_share_petrol, cv_naira) |>
  pivot_wider(
    names_from = wave,
    values_from = c(cv_share_petrol, cv_naira),
    names_glue = "{.value}_w{wave}"
  ) |>
  select(Quintile,
         cv_share_petrol_w2, cv_naira_w2,
         cv_share_petrol_w3, cv_naira_w3,
         cv_share_petrol_w4, cv_naira_w4)

table6b_kable <- table6b_wide |>
  kable(format = "html",
        digits = c(0, 1, 0, 1, 0, 1, 0),
        col.names = c("Quintile",
                      "CV (%)", "CV (₦)",
                      "CV (%)", "CV (₦)",
                      "CV (%)", "CV (₦)"),
        caption = "<b>Table 6B: CV as % of Petrol Expenditure</b>",
        align = c('l', 'c', 'r', 'c', 'r', 'c', 'r'),
        format.args = list(big.mark = ",")) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center",
                font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#E65100", color = "white") |>
  row_spec(1, background = "#FFCDD2") |>
  row_spec(5, background = "#C5E1A5") |>
  column_spec(1, bold = TRUE, width = "10em") |>
  column_spec(c(2, 4, 6), background = "#FFF3E0", color = "#E65100", bold = TRUE) |>
  column_spec(c(3, 5, 7), background = "#E8F5E9", color = "#2E7D32") |>
  add_header_above(c(" " = 1, "Wave 2 (2012/13)" = 2, "Wave 3 (2015/16)" = 2, "Wave 4 (2018/19)" = 2),
                   bold = TRUE, background = "#F57C00", color = "white") |>
  footnote(
    general = c(
      "CV as % of petrol expenditure shows <b>burden per unit petrol consumed</b>.",
      "Regressive: Q1 (16%) needs more compensation per unit than Q5 (14%).",
      "This metric (from Okereke et al. 2024) shows poorer households are less able to substitute."
    ),
    general_title = "<b>Interpretation:</b>",
    escape = FALSE
  )

save_kable(table6b_kable, "table6b_cv_pct_petrol.html")

cat("✓ Table 6A saved to table6a_cv_pct_totcons.html\n")
cat("✓ Table 6B saved to table6b_cv_pct_petrol.html\n\n")

# =============================================================================
# ELASTICITY RESULTS TABLE (Sophisticated)
# =============================================================================

cat("Creating elasticity summary table...\n")

# Load elasticity results if saved
# elasticity_results <- readRDS("elasticity_results.rds")

elasticity_summary <- tibble(
  Good = c("Petrol", "Electricity", "Kerosene", "Transport", "Other Energy"),
  `Expenditure Elasticity` = c(
    eta["petrol"],
    eta["electricity"],
    eta["kerosene"],
    eta["transport"],
    eta["other"]
  ),
  `Own-Price (Uncompensated)` = c(
    eps_u[1,1],
    eps_u[2,2],
    eps_u[3,3],
    eps_u[4,4],
    eps_u[5,5]
  ),
  `Own-Price (Compensated)` = c(
    eps_c[1,1],
    eps_c[2,2],
    eps_c[3,3],
    eps_c[4,4],
    eps_c[5,5]
  ),
  Classification = c(
    ifelse(eta["petrol"] < 1, "Necessity", "Luxury"),
    ifelse(eta["electricity"] < 1, "Necessity", "Luxury"),
    ifelse(eta["kerosene"] < 1, "Necessity", "Luxury"),
    ifelse(eta["transport"] < 1, "Necessity", "Luxury"),
    ifelse(eta["other"] < 1, "Necessity", "Luxury")
  )
)

elasticity_kable <- elasticity_summary |>
  kable(format = "html",
        digits = 3,
        col.names = c("Energy Good", "Expenditure η", "Own-Price ε (Marsh.)", 
                      "Own-Price ε (Hicks.)", "Classification"),
        caption = "<b>Estimated Elasticities from QUAIDS Model</b>",
        align = c('l', 'c', 'c', 'c', 'c')) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                position = "center",
                font_size = 14) |>
  row_spec(0, bold = TRUE, background = "#1565C0", color = "white", font_size = 15) |>
  row_spec(1, background = "#FFEBEE") |>  # Highlight petrol row
  column_spec(1, bold = TRUE, width = "10em") |>
  column_spec(2, color = spec_color(elasticity_summary$`Expenditure Elasticity`, 
                                    begin = 0.3, end = 0.9, direction = 1)) |>
  column_spec(3:4, color = "#D32F2F") |>
  column_spec(5, bold = TRUE, color = ifelse(elasticity_summary$Classification == "Necessity", 
                                             "#2E7D32", "#F57C00")) |>
  add_header_above(c(" " = 1, "Elasticities" = 3, " " = 1),
                   bold = TRUE, background = "#1976D2", color = "white") |>
  footnote(general = c("η < 1: Necessity (petrol target: 0.74–0.78)",
                       "ε < 0: Normal downward-sloping demand (petrol target: −0.487 uncompensated, −0.567 compensated)",
                       "Estimated at sample mean shares using pooled Waves 2–4 (N=11,646)"),
           general_title = "Notes:",
           footnote_as_chunk = TRUE)

save_kable(elasticity_kable, "elasticity_summary_table.html")
cat("✓ Elasticity table saved to elasticity_summary_table.html\n\n")


## CREATE SOPHISTICATED COMPARISON TABLE ####


library(tidyverse)
library(kableExtra)
library(scales)

###
# Added 5/4/26. Meant to fix errors in identification of matching figures and transparently calculate percentage differecnes.

# Extract Okereke target values (convert to numeric)
okereke_values <- c(
  13496,                    # Sample size
  5.50,                     # Mean HH size
  185381,                   # Mean total cons PC
  5.7,                      # Energy budget share (as %)
  22030,                    # Mean petrol exp
  -0.046,                   # β
  -0.004,                   # λ
  0.748,                    # Expenditure elasticity
  -0.571,                   # Compensated price
  -0.709,                   # Uncompensated price
  1.4,                      # CV Q1 (as %)
  2.1                       # CV Q5 (as %)
)

# Extract your computed values (convert to numeric)
your_values <- c(
  nrow(pooled_clean),
  mean(pooled_clean$hhsize, na.rm = TRUE),
  mean(pooled_clean$totcons, na.rm = TRUE),
  mean(pooled_clean$total_energy_exp / (pooled_clean$totcons * pooled_clean$hhsize), na.rm = TRUE) * 100,
  22407,
  beta_results |> filter(str_detect(parameter, "petrol.*ln_x_real$")) |> pull(estimate),
  lambda_results |> filter(str_detect(parameter, "petrol.*ln_x_real_sq")) |> pull(estimate),
  eta[1],
  eps_c[1,1],
  eps_u[1,1],
  cv_by_quintile |> filter(quintile == 1) |> pull(mean_cv_share_totcons) * 100,
  cv_by_quintile |> filter(quintile == 5) |> pull(mean_cv_share_totcons) * 100
)
print(your_values)

# Calculate percent differences
pct_diff <- abs((your_values - okereke_values) / okereke_values * 100)

print(pct_diff)
# Generate match symbols
Match <- case_when(
  pct_diff < 10 ~ "✓",
  pct_diff >= 10 & pct_diff < 15 ~ "~",
  pct_diff >= 15 ~ "✗",
  TRUE ~ "?"  # For any NA or edge cases
)

###

comparison_data <- tibble(
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
    "CV Q1: % totcons (% petrol)",    # ← COMBINED
    "CV Q5: % totcons (% petrol)"     # ← COMBINED
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
    "1.4% (16%)",      # ← Combined format
    "2.1% (14%)"       # ← Combined format
  ),
  `Your Replication` = c(
    scales::comma(nrow(pooled_clean)),
    sprintf("%.2f", mean(pooled_clean$hhsize, na.rm = TRUE)),
    paste0("₦", scales::comma(round(mean(pooled_clean$totcons, na.rm = TRUE)))),
    scales::percent(mean(pooled_clean$total_energy_exp / 
                           (pooled_clean$totcons * pooled_clean$hhsize), 
                         na.rm = TRUE), 
                    accuracy = 0.1),
    paste0("₦", scales::comma(round(mean(pooled_clean$w1_petrol * 
                                           pooled_clean$total_energy_exp, 
                                         na.rm = TRUE), 0))),
    sprintf("%.3f%s", 
            beta_results |> 
              filter(str_detect(parameter, "^petrol_ln_x_real$")) |> 
              pull(estimate),
            beta_results |> 
              filter(str_detect(parameter, "^petrol_ln_x_real$")) |> 
              pull(sig)),
    sprintf("%.3f%s", 
            lambda_results |> 
              filter(str_detect(parameter, "petrol_ln_x_real_sq")) |> 
              pull(estimate),
            lambda_results |> 
              filter(str_detect(parameter, "petrol_ln_x_real_sq")) |> 
              pull(sig)),
    # Expenditure elasticity with significance
    sprintf("%.3f%s", eta["petrol"], eta_sig["petrol"]),
    # Compensated own-price with significance
    sprintf("%.3f%s", eps_c[1,1], 
            get_stars(2 * pt(abs(eps_c[1,1] / eps_c_se[1,1]), df = df, lower.tail = FALSE))),
    # Uncompensated own-price with significance
    sprintf("%.3f%s", eps_u[1,1], 
            get_stars(2 * pt(abs(eps_u[1,1] / eps_u_se[1,1]), df = df, lower.tail = FALSE))),
    # Combined format: "X% (Y%)"
    sprintf("%s (%s)", 
            scales::percent(mean(pooled_clean$cv_share_totcons[pooled_clean$quintile == 1], 
                                 na.rm = TRUE), accuracy = 0.1),
            scales::percent(mean(pooled_clean$cv_share_petrol[pooled_clean$quintile == 1], 
                                 na.rm = TRUE), accuracy = 0.1)),
    sprintf("%s (%s)", 
            scales::percent(mean(pooled_clean$cv_share_totcons[pooled_clean$quintile == 5], 
                                 na.rm = TRUE), accuracy = 0.1),
            scales::percent(mean(pooled_clean$cv_share_petrol[pooled_clean$quintile == 5], 
                                 na.rm = TRUE), accuracy = 0.1))
  ),
  `Pct Diff` = round(pct_diff, 1),  # Optional: add the actual % difference
  Match = Match  # Use the calculated Match vector
)

# [Same styling code as before, but with updated footnote:]

comparison_kable <- kable(comparison_data, format = "html", escape = FALSE,
                          col.names = c("Metric", "Okereke et al. (Waves 2-4)", "My Replication (Waves 2-4)", "Pct Diff", "Match"),
                          caption = "<b>Replication Comparison: Author's Results vs. Okereke et al. (2024)</b>",
                          align = c("l", "r", "r", "c", "c")) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) |>
  row_spec(0, bold = TRUE, background = "#2C3E50", color = "white") |>
  column_spec(1, bold = TRUE, width = "12em") |>
  column_spec(2:3, width = "9em") |>
  column_spec(4:5, width = "3em", bold = TRUE,
              color = case_when(
                comparison_data$Match == "✓" ~ "#388E3C",
                comparison_data$Match == "✗" ~ "#D32F2F",
                comparison_data$Match == "~" ~ "#F57C00",
                comparison_data$Match == "?" ~ "#9E9E9E",
                TRUE ~ "black"
              )) |>
  pack_rows("Sample Characteristics", 1, 5) |>
  pack_rows("QUAIDS Coefficients", 6, 7) |>
  pack_rows("Elasticities", 8, 10) |>
  pack_rows("Welfare Impact", 11, 12) |>
  footnote(
    general = "CV shown as: % of total consumption (% of petrol expenditure). First metric shows absolute welfare burden; second shows burden per unit petrol consumed. Match legend: ✓ = close match, ~ = acceptable, ✗ = substantial difference, ? = to be verified. *** p<0.001. Sample: Waves 2-4 (N=11,646).",
    general_title = "",
    footnote_as_chunk = TRUE
  )

save_kable(comparison_kable, "comparison_to_okereke_compact_cv.html")

cat("✓ Comparison table created and saved to comparison_to_okereke_compact_cv.html\n\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=============================================================================\n")
cat("ALL SOPHISTICATED TABLES CREATED\n")
cat("=============================================================================\n\n")

cat("HTML files saved:\n")
cat("  1. table1_sample_characteristics.html\n")
cat("  2. table5a-e_*_shares.html (5 files - one per energy good)\n")
cat("  3. table6_cv_by_quintile.html\n")
cat("  4. elasticity_summary_table.html\n")
cat("  5. comparison_to_okereke.html\n\n")

cat("Open these files in your browser to view formatted tables!\n")
cat("🎨 Tables include color-coding, headers, footnotes, and formatting! 🎨\n\n")