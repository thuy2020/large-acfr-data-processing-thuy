library(tidyverse)
library(jsonlite)

# Read all data files
state_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_states_2023_20250912_1109.csv") %>% 
  select(-1)

county_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_counties_2023_20250909_2111.csv") %>% 
  select(-1)

municipal_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_municipalities_2023_20250910_1241.csv") %>% 
  select(-1)

school_district_data <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_schooldistricts_2023_20250911_1849.csv") %>% 
  select(-1)

# Process state data
state_data <- state_data |>
  mutate(
    flg_acfr = ifelse(is.na(flg_acfr), 1, flg_acfr),
  ) |>
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
    total_expenses = expenses,
    urban_population = urban_pop,
    pct_urban_population = pct_urban_pop
  ) |>
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
  # Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                     ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                     ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process county data
county_data <- county_data |>
  mutate(
    flg_acfr = ifelse(is.na(flg_acfr), 1, flg_acfr),
  ) |>
  filter(flg_acfr == 1) |>  filter(year == 2023) |>
  filter(flg_muni != 1) |>
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
  # Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                     ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                     ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process municipal data
municipal_data <- municipal_data |>
  filter(year == 2023) |>
  mutate(
    flg_acfr = ifelse(is.na(flg_acfr), 1, flg_acfr),
  ) |>
  filter(flg_acfr == 1) |>
  filter(flg_county != 1) |>
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
  # Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                     ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                     ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process school district data
school_district_data <- school_district_data |>
  filter(year == 2023) |>
  mutate(
    flg_acfr = ifelse(is.na(flg_acfr), 1, flg_acfr),
  ) |>
  filter(flg_acfr == 1) |>
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
  # Use enrollment_22 as population for school districts
  mutate(
    population = enrollment_23
  ) |>
  # Calculate derived financial metrics
  mutate(
    net_position = total_assets - total_liabilities,
    debt_ratio = total_liabilities / total_assets,
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = current_assets / current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                     ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                     ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Function to safely sum numeric columns
safe_sum <- function(data, column_name) {
  if (column_name %in% names(data)) {
    return(sum(data[[column_name]], na.rm = TRUE))
  } else {
    return(NA)
  }
}

# Calculate entity counts
state_count <- nrow(state_data)
county_count <- nrow(county_data)
municipality_count <- nrow(municipal_data)
school_district_count <- nrow(school_district_data)
total_count <- state_count + county_count + municipality_count + school_district_count

# Calculate population totals
general_population <- safe_sum(state_data, "population") 
student_enrollment <- safe_sum(school_district_data, "population") # This is enrollment_22

# Calculate financial totals
total_assets <- safe_sum(state_data, "total_assets") + 
                safe_sum(county_data, "total_assets") + 
                safe_sum(municipal_data, "total_assets") + 
                safe_sum(school_district_data, "total_assets")

current_assets <- safe_sum(state_data, "current_assets") + 
                        safe_sum(county_data, "current_assets") + 
                        safe_sum(municipal_data, "current_assets") + 
                        safe_sum(school_district_data, "current_assets")

total_liabilities <- safe_sum(state_data, "total_liabilities") + 
                     safe_sum(county_data, "total_liabilities") + 
                     safe_sum(municipal_data, "total_liabilities") + 
                     safe_sum(school_district_data, "total_liabilities")

current_liabilities <- safe_sum(state_data, "current_liabilities") + 
                             safe_sum(county_data, "current_liabilities") + 
                             safe_sum(municipal_data, "current_liabilities") + 
                             safe_sum(school_district_data, "current_liabilities")

pension_liability <- safe_sum(state_data, "pension_liability") + 
                           safe_sum(county_data, "pension_liability") + 
                           safe_sum(municipal_data, "pension_liability") + 
                           safe_sum(school_district_data, "pension_liability")

non_net_pension_liability <- safe_sum(state_data, "non_net_pension_liability") + 
                                 safe_sum(county_data, "non_net_pension_liability") + 
                                 safe_sum(municipal_data, "non_net_pension_liability") + 
                                 safe_sum(school_district_data, "non_net_pension_liability")

opeb_liability <- safe_sum(state_data, "opeb_liability") + 
                         safe_sum(county_data, "opeb_liability") + 
                         safe_sum(municipal_data, "opeb_liability") + 
                         safe_sum(school_district_data, "opeb_liability")

non_net_opeb_liability <- safe_sum(state_data, "non_net_opeb_liability") + 
                                 safe_sum(county_data, "non_net_opeb_liability") + 
                                 safe_sum(municipal_data, "non_net_opeb_liability") + 
                                 safe_sum(school_district_data, "non_net_opeb_liability")

bonds_outstanding <- safe_sum(state_data, "bonds_outstanding") + 
                           safe_sum(county_data, "bonds_outstanding") + 
                           safe_sum(municipal_data, "bonds_outstanding") + 
                           safe_sum(school_district_data, "bonds_outstanding")

loans_outstanding <- safe_sum(state_data, "loans_outstanding") + 
                           safe_sum(county_data, "loans_outstanding") + 
                           safe_sum(municipal_data, "loans_outstanding") + 
                           safe_sum(school_district_data, "loans_outstanding")

notes_outstanding <- safe_sum(state_data, "notes_outstanding") + 
                           safe_sum(county_data, "notes_outstanding") + 
                           safe_sum(municipal_data, "notes_outstanding") + 
                           safe_sum(school_district_data, "notes_outstanding")

compensated_absences <- safe_sum(state_data, "compensated_absences") + 
                          safe_sum(county_data, "compensated_absences") + 
                          safe_sum(municipal_data, "compensated_absences") + 
                          safe_sum(school_district_data, "compensated_absences")

bond_loans_notes <- safe_sum(state_data, "bond_loans_notes") + 
                          safe_sum(county_data, "bond_loans_notes") + 
                          safe_sum(municipal_data, "bond_loans_notes") + 
                          safe_sum(school_district_data, "bond_loans_notes")

total_revenues <- safe_sum(state_data, "total_revenues") + 
                  safe_sum(county_data, "total_revenues") + 
                  safe_sum(municipal_data, "total_revenues") + 
                  safe_sum(school_district_data, "total_revenues")

total_expenses <- safe_sum(state_data, "total_expenses") + 
                  safe_sum(county_data, "total_expenses") + 
                  safe_sum(municipal_data, "total_expenses") + 
                  safe_sum(school_district_data, "total_expenses")

net_position <- total_assets - total_liabilities
debt_ratio <- round(total_liabilities / total_assets, 4)
free_cash_flow <- total_revenues - (total_expenses + current_liabilities)
current_ratio <- round(current_assets / current_liabilities, 4)
non_current_liabilities <- total_liabilities - current_liabilities

# Create the overall totals summary object
overall_totals <- list(
  entity_counts = list(
    states = state_count,
    counties = county_count,
    municipalities = municipality_count,
    school_districts = school_district_count,
    total = total_count
  ),
  population = list(
    general_population = general_population,
    student_enrollment = student_enrollment
  ),
  total_assets = total_assets,
  current_assets = current_assets,
  total_liabilities = total_liabilities,
  current_liabilities = current_liabilities,
  pension_liability = pension_liability,
  non_net_pension_liability = non_net_pension_liability,
  opeb_liability = opeb_liability,
  non_net_opeb_liability = non_net_opeb_liability,
  bonds_outstanding = bonds_outstanding,
  loans_outstanding = loans_outstanding,
  notes_outstanding = notes_outstanding,
  compensated_absences = compensated_absences,
  bond_loans_notes = bond_loans_notes,
  total_revenues = total_revenues,
  total_expenses = total_expenses,
  net_position = net_position,
  debt_ratio = debt_ratio,
  free_cash_flow = free_cash_flow,
  current_ratio = current_ratio,
  non_current_liabilities = non_current_liabilities
)

# Convert to JSON
overall_totals_json <- toJSON(overall_totals, pretty = TRUE, auto_unbox = TRUE)
overall_totals_json <- paste0("export default ", overall_totals_json)
write(overall_totals_json, "output/simplified_summary.js")
saveRDS(overall_totals, "output/overall_totals.RDS")
