library(tidyverse)
library(jsonlite)

# read csv
county_data <- read_csv("input/all_counties_2023.csv")
county_data <- county_data[, -1]

# filter for only 2023 data, standardize names
county_data <- county_data |>
  mutate(
    year = 2023,
    source_year = 2023,
  ) |>
  mutate(
    name = ifelse(
      is.na(name), name_census, name
    )
  ) |>
  filter(year == 2023) |>
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
  # net position
  mutate(
    net_position = total_assets - total_liabilities
  ) |>
  # debt ratio
  mutate(
    debt_ratio = total_liabilities / total_assets
  ) |>
  # free cash flow
  mutate(
    free_cash_flow = total_revenues - (total_expenses + current_liabilities)
  ) |>
  # current ratio
  mutate(
    current_ratio = current_assets / current_liabilities
  ) |>
  # non_current_liabilities
  mutate(
    non_current_liabilities = total_liabilities - current_liabilities
  ) |>
  # Add sum of bonds, loans, and notes
  mutate(
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                     ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                     ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )


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
    opeb_liability,
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
    flg_county,
    flg_muni
  )


# write to json
county_json <- toJSON(county_data, pretty = TRUE)
county_json <- paste0("export default ", county_json)
write(county_json, "output/county_data.js")


