library(tidyverse)
library(jsonlite)

# read csv
municipal_data <- read_csv("input/all_municipalities_2023.csv")
municipal_data <- municipal_data[, -1]

# read coordinates data
# coordinates_data <- readRDS("coordinates_data/geocoded_final_20250315_182544.rds")

# filter for only 2023 data, standardize names
municipal_data <- municipal_data |>
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

# Prepare coordinates data for merging
# coordinates_data <- coordinates_data |>
#   rename(
#     entity_name = clean_name,
#     state_name = state.name
#   ) |>
#   # Create a merged key for joining
#   mutate(
#     entity_state_key = tolower(paste(entity_name, state_name, sep = "_"))
#   ) |>
#   select(entity_name, state_name, entity_state_key, latitude, longitude)

# Add the same merged key to municipal data
municipal_data <- municipal_data |>
  mutate(
    entity_state_key = tolower(paste(entity_name, state_name, sep = "_"))
  )

# Merge municipal data with coordinates data using the merged key
# municipal_data <- municipal_data |>
  # left_join(coordinates_data, by = "entity_state_key") |>
  # Use original entity_name and state_name from municipal_data
  # and only keep latitude and longitude from coordinates_data
  # select(-entity_name.y, -state_name.y) |>
  # rename(entity_name = entity_name.x, state_name = state_name.x,
  #        latitude = latitude.y, longitude = longitude.y)

# Select final columns including new latitude and longitude
municipal_data <- municipal_data |>
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
    total_revenues,
    total_expenses,
    net_position,
    pension_liability,
    non_net_pension_liability,
    opeb_liability,
    non_net_opeb_liability,
    bonds_outstanding,
    loans_outstanding,
    notes_outstanding,
    compensated_absences,
    bond_loans_notes,
    debt_ratio,
    free_cash_flow,
    current_ratio,
    non_current_liabilities,
    # median_household_income,
    population,
    latitude,
    longitude,
    document_url,
    flg_acfr,
    flg_county
  )

# write to json
municipal_json <- toJSON(municipal_data, pretty = TRUE)
municipal_json <- paste0("export default ", municipal_json)
write(municipal_json, "output/municipal_data.js")
# save RDS copy
saveRDS(municipal_data, "output/municipal_data.rds")