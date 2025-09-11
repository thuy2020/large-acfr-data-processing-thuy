library(tidyverse)
library(jsonlite)

# Read all data files
state_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_states_2023_20250908_1333.csv") %>% 
  select(-1)

county_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_counties_2023_20250909_2111.csv") %>% 
  select(-1)

municipal_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_municipalities_2023_20250910_1241.csv") %>% 
select(-1)

school_district_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_schooldistricts_2023_20250911_1849.csv") %>% 
  select(-1)

# Process state data
state_data <- state_data |>
  #filter(flg_acfr == 1) |>
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
    pct_urban_population = pct_urban_pop
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
  )

# Process county data
county_data <- county_data |>
  
  filter(flg_acfr == 1) |>
  filter(year == 2023) |>
  filter(
    flg_muni != 1  # Exclude municipalities
  ) |>
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
    pct_urban_population = pct_urban_pop
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
  )

# Process municipal data
municipal_data <- municipal_data |>
  
  filter(flg_acfr == 1) |>
  filter(year == 2023) |>
  filter(
    flg_county != 1  # Exclude counties
  ) |>
  rename(
    state_abbr = state.abb,
    state_name = state.name,
    entity_id = id,
    document_url = url,
    entity_name = name,
    entity_type = category,
    total_revenues = revenues,
    total_expenses = expenses
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
  )

# Process school district data
school_district_data <- school_district_data |>
  
  filter(flg_acfr == 1) |>
  filter(year == 2023) |>
  rename(
    state_abbr = state.abb,
    state_name = state.name,
    entity_id = id,
    document_url = url,
    entity_name = name,
    entity_type = category,
    total_revenues = revenues,
    total_expenses = expenses
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
  )

# Extract demographic data from state data to use later
state_demographics <- state_data |>
  select(
    state_name,
    state_abbr,
    population,
    urban_population,
    pct_urban_population
  ) |>
  add_row(state_name = "District of Columbia",
          state_abbr = "DC",
          population = 689546,
          urban_population = 689546,
          pct_urban_population = 100) %>% 
  mutate(urban_population = ifelse(state_abbr == "NV", 2920000, urban_population)) %>% 
  mutate(pct_urban_population = ifelse(state_abbr == "NV", 94.1, pct_urban_population)) %>% 
  distinct()

# Prepare state data for aggregation
state_financial <- state_data |>
  select(
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
    total_revenues,
    total_expenses
  )

# Prepare county data for aggregation
county_financial <- county_data |>
  select(
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
    total_revenues,
    total_expenses
  )

# Prepare municipal data for aggregation
municipal_financial <- municipal_data |>
  select(
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
    total_revenues,
    total_expenses
  )

# Prepare school district data for aggregation
school_district_financial <- school_district_data |>
  select(
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
    total_revenues,
    total_expenses
  )

# Combine all datasets
all_financial_data <- bind_rows(
  state_financial,
  county_financial,
  municipal_financial,
  school_district_financial
)

# Aggregate by state - using sum() without na.rm to match behavior of other scripts
state_aggregate_data <- all_financial_data |>
  group_by(state_name, state_abbr) |>
  summarize(
    total_assets = sum(total_assets, na.rm = TRUE),
    current_assets = sum(current_assets, na.rm = TRUE),
    total_liabilities = sum(total_liabilities, na.rm = TRUE),
    current_liabilities = sum(current_liabilities, na.rm = TRUE),
    pension_liability = sum(pension_liability, na.rm = TRUE),
    opeb_liability = sum(opeb_liability, na.rm = TRUE),
    bonds_outstanding = sum(bonds_outstanding, na.rm = TRUE),
    loans_outstanding = sum(loans_outstanding, na.rm = TRUE),
    notes_outstanding = sum(notes_outstanding, na.rm = TRUE),
    compensated_absences = sum(compensated_absences, na.rm = TRUE),
    total_revenues = sum(total_revenues, na.rm = TRUE),
    total_expenses = sum(total_expenses, na.rm = TRUE),
    .groups = "drop"
  )

# Calculate derived financial metrics
state_aggregate_data <- state_aggregate_data |>
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
  mutate(
    non_current_liabilities = total_liabilities - current_liabilities
  ) |>
  # Add sum of bonds, loans, and notes
  mutate(
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                     ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                     ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Join with demographic data from state entities
state_aggregate_data <- state_aggregate_data |>
  left_join(state_demographics, by = c("state_name", "state_abbr")) |>
  
  # Add year and entity_type
  mutate(
    year = 2023,
    entity_type = "state_aggregate"
  )

# Add entity_id (using state abbreviation as base)
state_aggregate_data <- state_aggregate_data |>
  mutate(
    entity_id = paste0("agg_", tolower(state_abbr)),
    entity_name = paste(state_name, "Aggregate")
  )

# Select and order final columns
state_aggregate_data <- state_aggregate_data |>
  select(
    entity_id,
    entity_name,
    entity_type,
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
    free_cash_flow,
    non_current_liabilities,
    population,
    urban_population,
    pct_urban_population
  )

# Write to JSON
state_aggregate_json <- toJSON(state_aggregate_data, pretty = TRUE)
state_aggregate_json <- paste0("export default ", state_aggregate_json)
write(state_aggregate_json, "output/state_aggregated.js")
# save RDS copy
saveRDS(state_aggregate_data, "output/state_aggregated.rds")

# Print summary to console
cat("State aggregate data created with", nrow(state_aggregate_data), "states\n")
