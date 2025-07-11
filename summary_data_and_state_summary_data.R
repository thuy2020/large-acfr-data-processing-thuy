library(tidyverse)
library(jsonlite)

# Read all data files
state_data <- read_csv("input/all_states_2023.csv")
state_data <- state_data[, -1]

county_data <- read_csv("input/all_counties_2023.csv")
county_data <- county_data[, -1]

municipal_data <- read_csv("input/all_municipalities_2023.csv")
municipal_data <- municipal_data[, -1]

school_district_data <- read_csv("input/all_schooldistricts_2023.csv")
school_district_data <- school_district_data[, -1]

# Process state data
state_data <- state_data |>
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
    total_expenses = expenses,
    urban_population = urban_pop,
    pct_urban_population = pct_urban_pop
  ) |>
  mutate(
    non_net_pension_liability = net_pension_liability,
    non_net_opeb_liability = net_opeb_liability
  ) |>
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
    # Add sum of bonds, loans, and notes
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process county data
county_data <- county_data |>
  filter(year == 2023) |>
  mutate(
    flg_acfr = ifelse(is.na(flg_acfr), 1, flg_acfr),
  ) |>
  filter(flg_acfr == 1) |>
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
    # Add sum of bonds, loans, and notes
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
                       ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
                       ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process municipal data
municipal_data <- municipal_data |>
  mutate(
    flg_acfr = ifelse(is.na(flg_acfr), 1, flg_acfr),
  ) |>
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
  )  |>
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
    # Add sum of bonds, loans, and notes
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# Process school district data
school_district_data <- school_district_data |>
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
    # Add sum of bonds, loans, and notes
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  ) 

# Create entity type summary - using a safer approach with explicit checks
# Function to safely summarize numeric columns
safe_sum <- function(data, column_name) {
  if (column_name %in% names(data)) {
    return(sum(data[[column_name]], na.rm = TRUE))
  } else {
    return(NA)
  }
}

# Function to safely calculate ratios
safe_ratio <- function(data, numerator, denominator) {
  num_sum <- safe_sum(data, numerator)
  denom_sum <- safe_sum(data, denominator)
  
  if (!is.na(num_sum) && !is.na(denom_sum) && denom_sum != 0) {
    return(round(num_sum / denom_sum, 3))
  } else {
    return(NA)
  }
}

# Create summaries for each entity type
state_summary <- state_data |>
  summarize(
    entity_type = "State",
    count = n(),
    population = safe_sum(state_data, "population"),
    total_assets = safe_sum(state_data, "total_assets"),
    current_assets = safe_sum(state_data, "current_assets"),
    total_liabilities = safe_sum(state_data, "total_liabilities"),
    current_liabilities = safe_sum(state_data, "current_liabilities"),
    total_revenues = safe_sum(state_data, "total_revenues"),
    total_expenses = safe_sum(state_data, "total_expenses"),
    pension_liability = safe_sum(state_data, "pension_liability"),
    non_net_pension_liability = safe_sum(state_data, "non_net_pension_liability"),
    opeb_liability = safe_sum(state_data, "opeb_liability"),
    non_net_opeb_liability = safe_sum(state_data, "non_net_opeb_liability"),
    bonds_outstanding = safe_sum(state_data, "bonds_outstanding"),
    loans_outstanding = safe_sum(state_data, "loans_outstanding"),
    notes_outstanding = safe_sum(state_data, "notes_outstanding"),
    compensated_absences = safe_sum(state_data, "compensated_absences"),
    bond_loans_notes = safe_sum(state_data, "bond_loans_notes"),
    student_enrollment = NA_real_,
    net_position = safe_sum(state_data, "total_assets") - safe_sum(state_data, "total_liabilities"),
    debt_ratio = safe_ratio(state_data, "total_liabilities", "total_assets"),
    current_ratio = safe_ratio(state_data, "current_assets", "current_liabilities"),
    free_cash_flow = safe_sum(state_data, "total_revenues") - (safe_sum(state_data, "total_expenses") + safe_sum(state_data, "current_liabilities"))
  )

county_summary <- county_data |>
  summarize(
    entity_type = "County",
    count = n(),
    population = safe_sum(county_data, "population"),
    total_assets = safe_sum(county_data, "total_assets"),
    current_assets = safe_sum(county_data, "current_assets"),
    total_liabilities = safe_sum(county_data, "total_liabilities"),
    current_liabilities = safe_sum(county_data, "current_liabilities"),
    total_revenues = safe_sum(county_data, "total_revenues"),
    total_expenses = safe_sum(county_data, "total_expenses"),
    pension_liability = safe_sum(county_data, "pension_liability"),
    non_net_pension_liability = safe_sum(county_data, "non_net_pension_liability"),
    opeb_liability = safe_sum(county_data, "opeb_liability"),
    non_net_opeb_liability = safe_sum(county_data, "non_net_opeb_liability"),
    bonds_outstanding = safe_sum(county_data, "bonds_outstanding"),
    loans_outstanding = safe_sum(county_data, "loans_outstanding"),
    notes_outstanding = safe_sum(county_data, "notes_outstanding"),
    compensated_absences = safe_sum(county_data, "compensated_absences"),
    bond_loans_notes = safe_sum(county_data, "bond_loans_notes"),
    student_enrollment = NA_real_,
    net_position = safe_sum(county_data, "total_assets") - safe_sum(county_data, "total_liabilities"),
    debt_ratio = safe_ratio(county_data, "total_liabilities", "total_assets"),
    current_ratio = safe_ratio(county_data, "current_assets", "current_liabilities"),
    free_cash_flow = safe_sum(county_data, "total_revenues") - (safe_sum(county_data, "total_expenses") + safe_sum(county_data, "current_liabilities"))
  )

municipal_summary <- municipal_data |>
  summarize(
    entity_type = "Municipality",
    count = n(),
    population = safe_sum(municipal_data, "population"),
    total_assets = safe_sum(municipal_data, "total_assets"),
    current_assets = safe_sum(municipal_data, "current_assets"),
    total_liabilities = safe_sum(municipal_data, "total_liabilities"),
    current_liabilities = safe_sum(municipal_data, "current_liabilities"),
    total_revenues = safe_sum(municipal_data, "total_revenues"),
    total_expenses = safe_sum(municipal_data, "total_expenses"),
    pension_liability = safe_sum(municipal_data, "pension_liability"),
    non_net_pension_liability = safe_sum(municipal_data, "non_net_pension_liability"),
    opeb_liability = safe_sum(municipal_data, "opeb_liability"),
    non_net_opeb_liability = safe_sum(municipal_data, "non_net_opeb_liability"),
    bonds_outstanding = safe_sum(municipal_data, "bonds_outstanding"),
    loans_outstanding = safe_sum(municipal_data, "loans_outstanding"),
    notes_outstanding = safe_sum(municipal_data, "notes_outstanding"),
    compensated_absences = safe_sum(municipal_data, "compensated_absences"),
    bond_loans_notes = safe_sum(municipal_data, "bond_loans_notes"),
    student_enrollment = NA_real_,
    net_position = safe_sum(municipal_data, "total_assets") - safe_sum(municipal_data, "total_liabilities"),
    debt_ratio = safe_ratio(municipal_data, "total_liabilities", "total_assets"),
    current_ratio = safe_ratio(municipal_data, "current_assets", "current_liabilities"),
    free_cash_flow = safe_sum(municipal_data, "total_revenues") - (safe_sum(municipal_data, "total_expenses") + safe_sum(municipal_data, "current_liabilities"))
  )

school_district_summary <- school_district_data |>
  summarize(
    entity_type = "School District",
    count = n(),
    population = safe_sum(school_district_data, "population"),
    total_assets = safe_sum(school_district_data, "total_assets"),
    current_assets = safe_sum(school_district_data, "current_assets"),
    total_liabilities = safe_sum(school_district_data, "total_liabilities"),
    current_liabilities = safe_sum(school_district_data, "current_liabilities"),
    total_revenues = safe_sum(school_district_data, "total_revenues"),
    total_expenses = safe_sum(school_district_data, "total_expenses"),
    pension_liability = safe_sum(school_district_data, "pension_liability"),
    non_net_pension_liability = safe_sum(school_district_data, "non_net_pension_liability"),
    opeb_liability = safe_sum(school_district_data, "opeb_liability"),
    non_net_opeb_liability = safe_sum(school_district_data, "non_net_opeb_liability"),
    bonds_outstanding = safe_sum(school_district_data, "bonds_outstanding"),
    loans_outstanding = safe_sum(school_district_data, "loans_outstanding"),
    notes_outstanding = safe_sum(school_district_data, "notes_outstanding"),
    compensated_absences = safe_sum(school_district_data, "compensated_absences"),
    bond_loans_notes = safe_sum(school_district_data, "bond_loans_notes"),
    student_enrollment = safe_sum(school_district_data, "student_enrollment"),
    net_position = safe_sum(school_district_data, "total_assets") - safe_sum(school_district_data, "total_liabilities"),
    debt_ratio = safe_ratio(school_district_data, "total_liabilities", "total_assets"),
    current_ratio = safe_ratio(school_district_data, "current_assets", "current_liabilities"),
    free_cash_flow = safe_sum(school_district_data, "total_revenues") - (safe_sum(school_district_data, "total_expenses") + safe_sum(school_district_data, "current_liabilities"))
  )

# Combine all summaries
entity_type_summary <- bind_rows(
  state_summary,
  county_summary,
  municipal_summary,
  school_district_summary
)

# Create the overall summary object
# Calculate Overall National Summary
# entity_type_summary here is the combination of state_summary, county_summary, etc.
# state_summary itself contains the correct national total population.
overall_national_summary <- entity_type_summary |>
  summarise(
    # Sum all relevant financial and count columns
    count = sum(count, na.rm = TRUE),
    total_assets = sum(total_assets, na.rm = TRUE),
    current_assets = sum(current_assets, na.rm = TRUE),
    total_liabilities = sum(total_liabilities, na.rm = TRUE),
    current_liabilities = sum(current_liabilities, na.rm = TRUE),
    total_revenues = sum(total_revenues, na.rm = TRUE),
    total_expenses = sum(total_expenses, na.rm = TRUE),
    pension_liability = sum(pension_liability, na.rm = TRUE),
    non_net_pension_liability = sum(non_net_pension_liability, na.rm = TRUE),
    opeb_liability = sum(opeb_liability, na.rm = TRUE),
    non_net_opeb_liability = sum(non_net_opeb_liability, na.rm = TRUE),
    bonds_outstanding = sum(bonds_outstanding, na.rm = TRUE),
    loans_outstanding = sum(loans_outstanding, na.rm = TRUE),
    notes_outstanding = sum(notes_outstanding, na.rm = TRUE),
    compensated_absences = sum(compensated_absences, na.rm = TRUE)
    # Population is handled in the mutate step below
  ) |>
  mutate(
    entity_type = "Overall",
    population = state_summary$population, # Use population from the national state_summary
    student_enrollment = school_district_summary$student_enrollment, # Use enrollment from national school_district_summary
    net_position = total_assets - total_liabilities,
    debt_ratio = ifelse(total_assets == 0 | is.na(total_assets), NA, round(total_liabilities / total_assets, 3)),
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = ifelse(current_liabilities == 0 | is.na(current_liabilities), NA, round(current_assets / current_liabilities, 3)),
    non_current_liabilities = total_liabilities - current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  ) |>
  # Ensure column order matches entity_type_summary for bind_rows
  select(
    entity_type, count, population, student_enrollment, total_assets, current_assets,
    total_liabilities, current_liabilities, total_revenues, total_expenses,
    pension_liability, non_net_pension_liability, opeb_liability, non_net_opeb_liability, bonds_outstanding,
    loans_outstanding, notes_outstanding, compensated_absences, bond_loans_notes,
    net_position, debt_ratio, free_cash_flow, current_ratio, non_current_liabilities
  )

# First build the 4 entity rows
entity_type_summary <- bind_rows(
  state_summary,
  county_summary,
  municipal_summary,
  school_district_summary
)

# Then prepend the overall row
entity_type_summary <- bind_rows(
  overall_national_summary,
  entity_type_summary               # <- keep the 4 rows
) |>
  mutate(
    non_current_liabilities = total_liabilities - current_liabilities
  ) |>
  arrange(factor(entity_type,
                 levels = c("Overall","State","County","Municipality","School District")))


overall_summary <- list(
  overall_summary = entity_type_summary
)

# Convert to JSON
entity_type_summary_json <- toJSON(overall_summary, pretty = TRUE, auto_unbox = TRUE)
entity_type_summary_json <- paste0("export default ", entity_type_summary_json)
write(entity_type_summary_json, "output/summary_data.js")



# ─────────────────────────────  PER‑STATE / PER‑ENTITY SUMMARY  ─────────────────────────────
#  (append this block after the code that writes output/summary_data.js)

# ── STATE entities only ───────────────────────────────────────────────
state_state_summary <- state_data |>
  group_by(state_abbr, state_name) |>
  summarize(
    entity_type               = "State",
    count                     = n(),
    population          = sum(population,            na.rm = TRUE),
    total_assets              = sum(total_assets,          na.rm = TRUE),
    current_assets      = sum(current_assets,        na.rm = TRUE),
    total_liabilities         = sum(total_liabilities,     na.rm = TRUE),
    current_liabilities = sum(current_liabilities,   na.rm = TRUE),
    total_revenues            = sum(total_revenues,        na.rm = TRUE),
    total_expenses            = sum(total_expenses,        na.rm = TRUE),
    pension_liability   = sum(pension_liability,     na.rm = TRUE),
    non_net_pension_liability = sum(non_net_pension_liability, na.rm = TRUE),
    opeb_liability      = sum(opeb_liability,        na.rm = TRUE),
    non_net_opeb_liability = sum(non_net_opeb_liability, na.rm = TRUE),
    bonds_outstanding   = sum(bonds_outstanding,     na.rm = TRUE),
    loans_outstanding   = sum(loans_outstanding,     na.rm = TRUE),
    notes_outstanding   = sum(notes_outstanding,     na.rm = TRUE),
    compensated_absences = sum(compensated_absences, na.rm = TRUE),
    student_enrollment        = NA_real_,
    .groups = "drop"
  ) |>
  mutate(
    net_position   = total_assets - total_liabilities,
    debt_ratio     = ifelse(total_assets == 0, NA, round(total_liabilities / total_assets, 3)),
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = ifelse(current_liabilities == 0, NA, round(current_assets / current_liabilities, 3)),
    non_current_liabilities = total_liabilities - current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# ── COUNTY entities by state ──────────────────────────────────────────
county_state_summary <- county_data |>
  group_by(state_abbr, state_name) |>
  summarize(
    entity_type               = "County",
    count                     = n(),
    population          = sum(population,            na.rm = TRUE),
    total_assets              = sum(total_assets,          na.rm = TRUE),
    current_assets      = sum(current_assets,        na.rm = TRUE),
    total_liabilities         = sum(total_liabilities,     na.rm = TRUE),
    current_liabilities = sum(current_liabilities,   na.rm = TRUE),
    total_revenues            = sum(total_revenues,        na.rm = TRUE),
    total_expenses            = sum(total_expenses,        na.rm = TRUE),
    pension_liability   = sum(pension_liability,     na.rm = TRUE),
    non_net_pension_liability = sum(non_net_pension_liability, na.rm = TRUE),
    opeb_liability      = sum(opeb_liability,        na.rm = TRUE),
    non_net_opeb_liability = sum(non_net_opeb_liability, na.rm = TRUE),
    bonds_outstanding   = sum(bonds_outstanding,     na.rm = TRUE),
    loans_outstanding   = sum(loans_outstanding,     na.rm = TRUE),
    notes_outstanding   = sum(notes_outstanding,     na.rm = TRUE),
    compensated_absences = sum(compensated_absences, na.rm = TRUE),
    student_enrollment        = NA_real_,
    .groups = "drop"
  ) |>
  mutate(
    net_position   = total_assets - total_liabilities,
    debt_ratio     = ifelse(total_assets == 0, NA, round(total_liabilities / total_assets, 3)),
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = ifelse(current_liabilities == 0, NA, round(current_assets / current_liabilities, 3)),
    non_current_liabilities = total_liabilities - current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# ── MUNICIPAL entities by state ───────────────────────────────────────
municipal_state_summary <- municipal_data |>
  group_by(state_abbr, state_name) |>
  summarize(
    entity_type               = "Municipality",
    count                     = n(),
    population          = sum(population,            na.rm = TRUE),
    total_assets              = sum(total_assets,          na.rm = TRUE),
    current_assets      = sum(current_assets,        na.rm = TRUE),
    total_liabilities         = sum(total_liabilities,     na.rm = TRUE),
    current_liabilities = sum(current_liabilities,   na.rm = TRUE),
    total_revenues            = sum(total_revenues,        na.rm = TRUE),
    total_expenses            = sum(total_expenses,        na.rm = TRUE),
    pension_liability   = sum(pension_liability,     na.rm = TRUE),
    non_net_pension_liability = sum(non_net_pension_liability, na.rm = TRUE),
    opeb_liability      = sum(opeb_liability,        na.rm = TRUE),
    non_net_opeb_liability = sum(non_net_opeb_liability, na.rm = TRUE),
    bonds_outstanding   = sum(bonds_outstanding,     na.rm = TRUE),
    loans_outstanding   = sum(loans_outstanding,     na.rm = TRUE),
    notes_outstanding   = sum(notes_outstanding,     na.rm = TRUE),
    compensated_absences = sum(compensated_absences, na.rm = TRUE),
    student_enrollment        = NA_real_,
    .groups = "drop"
  ) |>
  mutate(
    net_position   = total_assets - total_liabilities,
    debt_ratio     = ifelse(total_assets == 0, NA, round(total_liabilities / total_assets, 3)),
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = ifelse(current_liabilities == 0, NA, round(current_assets / current_liabilities, 3)),
    non_current_liabilities = total_liabilities - current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# ── SCHOOL‑DISTRICT entities by state ─────────────────────────────────
school_state_summary <- school_district_data |>
  group_by(state_abbr, state_name) |>
  summarize(
    entity_type               = "School District",
    count                     = n(),
    population          = sum(population,            na.rm = TRUE),
    total_assets              = sum(total_assets,          na.rm = TRUE),
    current_assets      = sum(current_assets,        na.rm = TRUE),
    total_liabilities         = sum(total_liabilities,     na.rm = TRUE),
    current_liabilities = sum(current_liabilities,   na.rm = TRUE),
    total_revenues            = sum(total_revenues,        na.rm = TRUE),
    total_expenses            = sum(total_expenses,        na.rm = TRUE),
    pension_liability   = sum(pension_liability,     na.rm = TRUE),
    non_net_pension_liability = sum(non_net_pension_liability, na.rm = TRUE),
    opeb_liability      = sum(opeb_liability,        na.rm = TRUE),
    non_net_opeb_liability = sum(non_net_opeb_liability, na.rm = TRUE),
    bonds_outstanding   = sum(bonds_outstanding,     na.rm = TRUE),
    loans_outstanding   = sum(loans_outstanding,     na.rm = TRUE),
    notes_outstanding   = sum(notes_outstanding,     na.rm = TRUE),
    compensated_absences = sum(compensated_absences, na.rm = TRUE),
    student_enrollment        = sum(population, na.rm = TRUE), # This is enrollment for school districts
    .groups = "drop"
  ) |>
  mutate(
    net_position   = total_assets - total_liabilities,
    debt_ratio     = ifelse(total_assets == 0, NA, round(total_liabilities / total_assets, 3)),
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = ifelse(current_liabilities == 0, NA, round(current_assets / current_liabilities, 3)),
    non_current_liabilities = total_liabilities - current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  )

# ── Combine all four entity‑type summaries for every state ────────────
state_entity_type_summary <- bind_rows(
  state_state_summary,
  county_state_summary,
  municipal_state_summary,
  school_state_summary
)

# ── Write state‑level entity‑type summary to JS (no extra index object) ──
# Calculate Overall Summary for each State
# state_entity_type_summary is bind_rows(state_state_summary, county_state_summary, ...)
overall_state_summary <- state_entity_type_summary |>
  group_by(state_abbr, state_name) |>
  summarise(
    # Sum all relevant financial and count columns
    count = sum(count, na.rm = TRUE),
    total_assets = sum(total_assets, na.rm = TRUE),
    current_assets = sum(current_assets, na.rm = TRUE),
    total_liabilities = sum(total_liabilities, na.rm = TRUE),
    current_liabilities = sum(current_liabilities, na.rm = TRUE),
    total_revenues = sum(total_revenues, na.rm = TRUE),
    total_expenses = sum(total_expenses, na.rm = TRUE),
    pension_liability = sum(pension_liability, na.rm = TRUE),
    non_net_pension_liability = sum(non_net_pension_liability, na.rm = TRUE),
    opeb_liability = sum(opeb_liability, na.rm = TRUE),
    non_net_opeb_liability = sum(non_net_opeb_liability, na.rm = TRUE),
    bonds_outstanding = sum(bonds_outstanding, na.rm = TRUE),
    loans_outstanding = sum(loans_outstanding, na.rm = TRUE),
    notes_outstanding = sum(notes_outstanding, na.rm = TRUE),
    compensated_absences = sum(compensated_absences, na.rm = TRUE),
    # For population, take the value where entity_type is "State" for the current group.
    # This assumes there's exactly one "State" row per state_abbr/state_name group after binding.
    population = sum(population[entity_type == "State"], na.rm = TRUE),
    student_enrollment = sum(student_enrollment[entity_type == "School District"], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    entity_type = "Overall", # Set entity_type after summarise
    net_position = total_assets - total_liabilities,
    debt_ratio = ifelse(total_assets == 0 | is.na(total_assets), NA, round(total_liabilities / total_assets, 3)),
    free_cash_flow = total_revenues - (total_expenses + current_liabilities),
    current_ratio = ifelse(current_liabilities == 0, NA, round(current_assets / current_liabilities, 3)),
    non_current_liabilities = total_liabilities - current_liabilities,
    bond_loans_notes = ifelse(is.na(bonds_outstanding), 0, bonds_outstanding) + 
      ifelse(is.na(loans_outstanding), 0, loans_outstanding) + 
      ifelse(is.na(notes_outstanding), 0, notes_outstanding)
  ) |>
  # Ensure column order matches for bind_rows
  select(
    state_abbr, state_name, entity_type, count, population, student_enrollment,
    total_assets, current_assets, total_liabilities, current_liabilities,
    total_revenues, total_expenses, pension_liability, non_net_pension_liability, opeb_liability, non_net_opeb_liability,
    bonds_outstanding, loans_outstanding, notes_outstanding, compensated_absences,
    bond_loans_notes, net_position, debt_ratio, free_cash_flow, 
    current_ratio, non_current_liabilities
  )

state_entity_type_summary <- bind_rows(state_entity_type_summary, overall_state_summary) |>
  arrange(state_abbr, state_name, factor(entity_type, levels = c("Overall", "State", "County", "Municipality", "School District"))) # Arrange to group by state and then by entity type, ensuring Overall is last

state_entity_type_json <- toJSON(state_entity_type_summary,
                                 pretty = TRUE, auto_unbox = TRUE)
state_entity_type_json <- paste0("export default ", state_entity_type_json)
write(state_entity_type_json, "output/state_summary_data.js")