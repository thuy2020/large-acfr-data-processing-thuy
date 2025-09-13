library(tidyverse)
library(jsonlite)
# Read all data files

county_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_counties_2023_20250909_2111.csv") %>% 
  select(-1)

# filter for only 2023 data, standardize names
county_data <- county_data |>
  mutate(year = 2023)|>
  mutate(name = ifelse(is.na(name), name_census, name)) |>
  rename(
    state_abbr = state.abb,
    state_name = state.name,
    entity_id = id,
    document_url = url,
    entity_name = name,
    entity_type = category,
    total_revenues = revenues,
    total_expenses = expenses,
    urban_population = urban_pop,
    pct_urban_population = pct_urban_pop,
    # median_household_income = median_hh_income_21
  )|>
  mutate(
    non_net_pension_liability = net_pension_liability,
    non_net_opeb_liability = net_opeb_liability
  ) |>
  # create net net pension and opeb liabilities
  mutate(
    net_pension_assets = ifelse(is.na(net_pension_assets), 0, net_pension_assets),
    net_opeb_assets = ifelse(is.na(net_opeb_assets), 0, net_opeb_assets),
    net_pension_liability = ifelse(is.na(net_pension_liability), 0, net_pension_liability),
    net_opeb_liability = ifelse(is.na(net_opeb_liability), 0, net_opeb_liability),
    net_net_pension_liability = net_pension_liability - net_pension_assets,
    net_net_opeb_liability = net_opeb_liability - net_opeb_assets
  ) |>
  mutate(
    pension_liability = net_net_pension_liability,
    opeb_liability = net_net_opeb_liability
  ) |>
  # protection against division by zero
  mutate(
    total_assets = ifelse(is.na(total_assets), 0, total_assets),
    total_liabilities = ifelse(is.na(total_liabilities), 0, total_liabilities),
    current_assets = ifelse(is.na(current_assets), 0, current_assets),
    current_liabilities = ifelse(is.na(current_liabilities), 0, current_liabilities),
    
    # net position
    net_position = total_assets - total_liabilities,
    
    # Ratios 
    debt_ratio = ifelse(total_assets == 0, NA, total_liabilities / total_assets),
    current_ratio = ifelse(current_liabilities == 0, NA, current_assets / current_liabilities)
  ) |>
  
  # free cash flow
  mutate(
    free_cash_flow = total_revenues - (total_expenses + current_liabilities)
  ) |>
  # non_current_liabilities
  mutate(
    non_current_liabilities = total_liabilities - current_liabilities
  ) |>
  # Add sum of bonds, loans, and notes
  mutate(
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                     ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                     ifelse(is.na(notes_outstanding), 0, notes_outstanding))
county_data <- county_data |>
  select(
    entity_id,
    entity_name,
    entity_type,
    geo_id,
    year,
    state_name,
    state_abbr,
    total_assets,
    current_assets,
    total_liabilities,
    current_liabilities,
    pension_liability,
    non_net_pension_liability,
    opeb_liability,
    non_net_opeb_liability,
    bonds_outstanding,
    loans_outstanding,
    notes_outstanding,
    compensated_absences,
    bond_loans_notes,
    total_revenues,
    total_expenses,
    net_position,
    debt_ratio,
    current_ratio,
    non_current_liabilities,
    free_cash_flow,
    population,
    urban_population,
    pct_urban_population,
    # median_household_income,
    document_url,
    flg_acfr,
    flg_muni,
    flg_backfilled
  )


# write to json
county_json <- toJSON(county_data, pretty = TRUE)
county_json <- paste0("export default ", county_json)
write(county_json, "output/county_data.js")
# save RDS
saveRDS(county_data, "output/county_data.rds")


