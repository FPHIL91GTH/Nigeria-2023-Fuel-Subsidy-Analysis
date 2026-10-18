# Fuel Subsidy Incidence in Nigeria: A QUAIDS Analysis

## Overview

This repository contains replication code for:
1. **Phillip (2026)** - "Fuel Subsidy Incidence Before Nigeria's 2023 Removal: A QUAIDS Analysis of 2023/24 Household Data." [JScholarship](https://jscholarship.library.jhu.edu/handle/1774.2/71692)
The data used in this analysis is proprietary to the World Bank/NBS and must be obtained separately from their official sources.

## Key Findings
Methodological: 
- Successful replication of Okereke et al. (2024)’s QUAIDS framework in R 
(N=11,646 households; own-price elasticities match within 5%)

Empirical:
- Fuel subsidies were highly regressive (pro-rich): the richest quintile captured 
60% of petrol spending within energy budgets, whereas the bottom 50% 
consumed only 25% of total petrol expenditure
- Subsidy removal triggered asymmetric energy substitution, with kerosene 
access collapsing 59% (from 55.2% to 22.5%), forcing many poor households 
to reduce modern fuel consumption

## Data
This analysis uses Nigeria's General Household Survey Panel (GHS-Panel), Wave 2-5 (2012-2024), available from the World Bank Microdata Library: [https://microdata.worldbank.org/](link).

**Note:** Raw microdata cannot be redistributed. See `data/raw/README.md` for instructions on obtaining the data.

## Steps for Replication
1. Clone this repository
2. Download GHS data following instructions in `data/raw/README.md`
3. Run scripts in order:
4. Output table and figures will appear in `output/`

## Author
Francis Phillip

Johns Hopkins University

fphilli8@jh.edu

## License
MIT license
