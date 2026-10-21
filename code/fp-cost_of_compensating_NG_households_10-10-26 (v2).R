####
# fp-cost_of_compensating_NG_households_10-10-26 ####
# Date: 2026-10-10
# Author: Francis Phillip
####


cat("Previous calculation of how much Nigerians are living on per day and per year")

cat("In 2019, Nigeria’s National Bureau of Statistics 
reported that roughly 40% of the population (almost 83 million people) lived below the 
national poverty line of 137,430 naira ($381.75) per year, or 376.52 naira ($1.05) per 
day.")

137430/376.52
cat("Ok, so just take the number of dollars per day you think is the poverty rate, multiply it by 365 days, then convert it to naira to get those figures.")



cat("How much does it cost to compensate Nigerian households for welfare losses incurred by the 2023 fuel subsidy removal?")

# Mean household size is 5.31
# Nigeria's population is roughly 230 million
ng_population <- 224000000
ng_hh_size <- 5.31
ng_households <- ng_population/ng_hh_size

print(ng_population)
print(round(ng_population), 2)
cat("This is roughly how many million households there are in Nigeria.")

# Exchange rate (avg 2026 NGN/USD)
exchange_rate_26 <- 1365.58

cat("In the World Bank's 2026 Poverty Equity Brief Report for Nigeria released on Oct 8, 2026, they estimated that 64% of Nigerians lived at or under the poverty line of $4.20/day for lower-middle income countries. I will use this figure to calculate the annual consumption for the first 3 income quintiles.")

lmi_pov_line_usd <- 4.2
umi_pov_line_usd <- 8.3
annual_cons_person_ngn_lmi <- lmi_pov_line_usd * exchange_rate_26 * 365
annual_cons_hh_ngn_lmi <- annual_cons_person_ngn_lmi * ng_hh_size

print(annual_cons_hh_ngn_lmi)
cat("This is my baseline estimate for the annual consumption of the bottom 60% of Nigerian households. Note that this is solely for calculation purposes and not an accurate represenation of Nigerian household consumption.")

annual_cons_person_ngn_umi <- umi_pov_line_usd * exchange_rate_26 * 365
annual_cons_hh_ngn_umi <- annual_cons_person_ngn_umi * ng_hh_size

print(annual_cons_hh_ngn_umi)
cat("This is my baseline estimate for the annual consumption of the top 40% of Nigerian households. Note that this is solely for calculation purposes and not an accurate represenation of Nigerian household consumption.")



hh_per_quintile <- ng_households/5
hh_poverty <- hh_per_quintile*3
print(hh_per_quintile)
cat("This is the number of households per income quintile.")
print(hh_poverty)
cat("This is the number of households I assume are in poverty for calculation purposes. Note that this is an underestimate.")



cat("We can break down that figure into 3 income deciles, with varying magnitudes of compensating variation (CV).")


# Q1 Compensation
cat("My QUAIDS analysis reported a median CV of 28.52% for the bottom 20% of households:")
q1_w5_cv <- 0.2852
1.05*q1_w5_cv*365
q1_compensation_perhh <- annual_cons_hh_ngn_lmi*q1_w5_cv
q1_compensation <- q1_compensation_perhh*hh_per_quintile
print(q1_compensation_perhh)
cat("This is how much each household in Q1 needs to be compensated.")
print(q1_compensation)
cat("This is how much it would cost to compensate all households in Q1.")

# Interpretation: cat("Each of the first 5.52 million households--the bottom 20% of households--needs to be paid an additional annual salary of roughly N2,361,917, which would cost almost N12 trillon (11,997,764,137,119.6).")


# Q2 Compensation
cat("My QUAIDS analysis reported a median CV of 17.94% for income quintile 2:")
q2_w5_cv <- 0.1794 	
q2_compensation_perhh <- annual_cons_hh_ngn_lmi * q2_w5_cv
q2_compensation <- q2_compensation_perhh * hh_per_quintile
print(q2_compensation_perhh)
cat("This is how much each household in Q2 needs to be compensated.")
print(q2_compensation)
cat("This is how much it would cost to compensate all households in Q2.")

# Interpretation: cat("The next 5.52 million households each needs to be paid an additional annual salary roughly N1,019,502, which would cost more than N562 billon (N562,765,339,000)")

# Q3 Compensation
cat("My QUAIDS analysis reported a CV of 14.73% for income quintile 3:")
q3_w5_cv <- 0.1473
q3_compensation_perhh <- annual_cons_hh_ngn_lmi * q3_w5_cv
q3_compensation <- q3_compensation_perhh * hh_per_quintile
print(q3_compensation_perhh)
cat("This is how much each household in Q3 needs to be compensated.")
print(q3_compensation)
cat("This is how much it would cost to compensate all households in Q3.")

# Compensating the bottom 40% of households
bottom40_compensation <- q1_compensation + q2_compensation
print(bottom40_compensation)

# Q4 Compensation

q4_w5_cv <- 0.0825
q4_compensation_perhh <- annual_cons_hh_ngn_umi * q4_w5_cv
q4_compensation <- q4_compensation_perhh * hh_per_quintile

print(q4_compensation_perhh)
cat("This is how much each household in Q4 needs to be compensated.")

print(q4_compensation)
cat("This is how much it would cost to compensate all households in Q4.")

# Q5 Compensation

q5_w5_cv <- 0.0114
q5_compensation_perhh <- annual_cons_hh_ngn_umi * q5_w5_cv
q5_compensation <- q5_compensation_perhh * hh_per_quintile

print(q5_compensation_perhh)
cat("This is how much each household in Q5 needs to be compensated.")

print(q5_compensation)
cat("This is how much it would cost to compensate all households in Q5.")


# Interpretation: cat("The last 5.52 million households--among the 27.6 million living in poverty-- each needs to be paid an additional annual salary of roughly N805,344, which would cost more than N562 billon (N4445,500,000,000)")

tot_comp_perhh <- sum(q1_compensation_perhh, q2_compensation_perhh, q3_compensation_perhh, q4_compensation_perhh, q5_compensation_perhh)
print(tot_comp_perhh)

tot_comp <- sum(q1_compensation, q2_compensation, q3_compensation, q4_compensation, q5_compensation)
print(tot_comp)

col_quintile <- c("Q1", "Q2", "Q3", "Q4", "Q5", "All")
col_annual_cons_hh_ngn <- c(annual_cons_hh_ngn_lmi, annual_cons_hh_ngn_lmi, annual_cons_hh_ngn_lmi, annual_cons_hh_ngn_umi, annual_cons_hh_ngn_umi, sum(annual_cons_hh_ngn_lmi*3,annual_cons_hh_ngn_umi*2))
col_compensation_perhh <- c(q1_compensation_perhh, q2_compensation_perhh, q3_compensation_perhh, q4_compensation_perhh, q5_compensation_perhh, tot_consperhh)
col_totcompensation <- c(q1_compensation, q2_compensation, q3_compensation, q4_compensation, q5_compensation, tot_comp)

compensation_table <- tibble(col_quintile, col_annual_cons_hh_ngn, col_compensation_perhh, col_totcompensation)
View(compensation_table)




compensation_table <- tibble(
  Quintile = c("Q1 (Poorest)", "Q2", "Q3 (Middle)", "Q4", "Q5 (Richest)", "All Households"),
  Annual_Consumption = c(annual_cons_hh_ngn_lmi, annual_cons_hh_ngn_lmi, annual_cons_hh_ngn_lmi, 
                         annual_cons_hh_ngn_umi, annual_cons_hh_ngn_umi, 
                         sum(annual_cons_hh_ngn_lmi*3, annual_cons_hh_ngn_umi*2)),
  Compensation_PerHH = c(q1_compensation_perhh, q2_compensation_perhh, q3_compensation_perhh, 
                         q4_compensation_perhh, q5_compensation_perhh, tot_consperhh),
  Total_Compensation_NGN = c(q1_compensation, q2_compensation, q3_compensation, 
                             q4_compensation, q5_compensation, tot_comp),
  Total_Compensation_USD = c(q1_compensation, q2_compensation, q3_compensation, 
                             q4_compensation, q5_compensation, tot_comp) / exchange_rate_26
)

table_compensation <- compensation_table |>
  mutate(
    Annual_Consumption = scales::comma(Annual_Consumption, accuracy = 1),
    Compensation_PerHH = scales::comma(Compensation_PerHH, accuracy = 1),
    Total_Compensation_NGN = scales::comma(Total_Compensation_NGN / 1e9, accuracy = 0.01),
    Total_Compensation_USD = scales::comma(Total_Compensation_USD / 1e9, accuracy = 0.01)
  ) |>
  kbl(
    col.names = c("Income\nQuintile", 
                  "Annual HH\nConsumption (₦)", 
                  "Compensation\nper HH (₦)",
                  "Total\n(₦ Billion)",
                  "Total\n($ Billion)"),
    align = c("l", "r", "r", "r", "r"),
    caption = "<b>Table 6: Estimated Annual Compensation Requirements by Quintile</b>"
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 12
  ) |>
  row_spec(0, bold = TRUE, color = "white", background = "#2C3E50") |>
  row_spec(6, bold = TRUE, background = "#f0f0f0") |>
  column_spec(1, width = "10em", bold = TRUE) |>
  column_spec(2:5, width = "8em") |>
  footnote(
    general = "Compensation = CV × annual consumption. Exchange rate: ₦1,365.58/USD (2026 projection). 
    Q1-Q3 annualizes the lower-middle income poverty benchmark ($4.20/day); Q4-Q5 annualizes the upper-middle income benchmark ($8.30/day).
    SOURCE:  World Bank. 2026. Nigeria Poverty and Equity Brief : October 2026 (English). Poverty and Equity Brief Washington, D.C. : World Bank Group. http://documents.worldbank.org/curated/en/099253204222517873",
    general_title = "",
    footnote_as_chunk = TRUE
  )

save_kable(table_compensation, "Compensation Table_10-10-26.html")
write_csv(table5, "Compensation Table_10-10-26.csv")
