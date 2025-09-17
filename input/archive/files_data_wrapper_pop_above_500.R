library(tidyverse)
library(jsonlite)
source("functions.R")
state_aggregated <- readRDS("output/state_aggregated.rds")
overall_totals <- readRDS("output/overall_totals.RDS")
state_entity_type_summary <- readRDS("output/state_entity_type_summary.RDS")
state_data <- readRDS("output/state_data.rds")
county_data <- readRDS("output/county_data.rds")
municipal_data <- readRDS("output/municipal_data.rds")
school_district_data <- readRDS("output/school_district_data.rds")

# Limit to 50 states (exclude District of Columbia)
states_exclude <- c("District of Columbia")
state_aggregated <- state_aggregated %>% filter(!state_name %in% states_exclude)
state_entity_type_summary <- state_entity_type_summary %>% filter(!state_name %in% states_exclude)
state_data <- state_data %>% filter(!state_name %in% states_exclude)
# School districts: keep only those with >= 500 students (population field)
school_district_data <- school_district_data %>% filter(population >= 500)
# Counties: keep only those with population >= 10,000
county_data <- county_data %>% filter(population >= 10000)
# Municipalities: keep only those with population >= 10,000
municipal_data <- municipal_data %>% filter(population >= 10000)
# Standardize DC naming: 'District of Columbia' -> 'Washington' for entity_name in counties and munis
county_data <- county_data %>% mutate(
  entity_name = if_else(state_name == "District of Columbia", "Washington", entity_name)
)
municipal_data <- municipal_data %>% mutate(
  entity_name = if_else(state_name == "District of Columbia", "Washington", entity_name)
)

#####
if (interactive()) {
  school_district_data %>% filter(is.na(population) | population == 0) %>% 
    filter(total_liabilities >0) %>% 
    filter(!str_detect(entity_name, "boces|(service center)|
                         (educational cooperative)|
                         (county board of cooperative educational services)")) %>% 
    select(state_abbr, entity_name, entity_id, total_liabilities) %>% 
    View()
}
######


####Chart 1: Heatmap for per capita Total Liabilities####
# (Aggregate State and Local by state)

state_aggregated %>% 
  select(state_abbr, state_name, total_liabilities, population) %>% 
  mutate(percap_total_liabilities = round(total_liabilities/population)) %>% 
  arrange(percap_total_liabilities) %>% write.csv("output/data_wrapper/chart1_percap_total_liabilities.csv")

####Chart 2: Composition of State and Local Government Debt####

# Extract values
total <- overall_totals$non_current_liabilities
pension <- overall_totals$pension_liability
opeb <- overall_totals$opeb_liability
bond_loans_notes <- overall_totals$bond_loans_notes
comp_abs <- overall_totals$compensated_absences
other <- total - (pension + opeb + bond_loans_notes + comp_abs)

# dataframe
pie_df <- tibble(
  component = c("Net Pension Liability", "Net OPEB Liability", 
                "Bonds, Loans, Notes", "Compensated Absences", "Other"),
  value = c(pension, opeb, bond_loans_notes, comp_abs, other)
) %>%
  mutate(
    percentage = round(100 * value / total, 1),
    label = paste0(component, " (", percentage, "%)"),
    formatted = case_when(
      value >= 1e12 ~ paste0(round(value/1e12, 1), "T"),
      value >= 1e9  ~ paste0(round(value/1e9, 1), "B"),
      value >= 1e6  ~ paste0(round(value/1e6, 1), "M"),
      TRUE ~ as.character(value)
    )
  )

pie_df %>% write.csv("output/data_wrapper/chart2_Composition_State_Local_Gov_Debt.csv")

####Table 1.1: Aggregate Total State and Local Debt####
#Variable: Total Liabilities 
#State Name,Aggregate Total Debt,Aggregate Debt per Capita   

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, total_liabilities, population) %>% 
  arrange(desc(total_liabilities)) %>% 
  slice(1:10) %>% 
  mutate(
    # Convert to billions and format with commas + "B"
    Aggregate_Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
    
    # Per capita (keep as number with commas)
    Aggregate_Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
  ) %>%
  select(state_name, Aggregate_Total_Debt, Aggregate_Debt_per_Capita) %>% 
  rename(
    `State Name` = state_name,
    `Aggregate Total Debt` = Aggregate_Total_Debt,
    `Aggregate Debt per Capita` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/table1.1_Aggregate_State_Local_total_liabilities.csv", row.names = FALSE)

####Table 1.2: Aggregate State and Local Long-Term Debt####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, non_current_liabilities, population) %>% 
  arrange(desc(non_current_liabilities)) %>% 
  slice(1:10) %>% 
  mutate(
    Aggregate_Total_Debt = paste0(
      "$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"
    ),
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(non_current_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(state_name, Aggregate_Total_Debt, Aggregate_Debt_per_Capita) %>% 
  rename(
    `State Name` = state_name,
    `Aggregate Total Debt` = Aggregate_Total_Debt,
    `Aggregate Debt per Capita` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/table1.2_Aggregate_State_Local_non_current_liabilities.csv", row.names = FALSE)

####Table 1.3: State Government Debt####

state_entity_type_summary %>% 
  filter(entity_type == "State") %>% 
  select(state_name, total_liabilities, population) %>% 
  arrange(desc(total_liabilities)) %>% 
  mutate(
    state_gov_Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
    
    state_gov_Total_Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
  ) %>%
  
  select(state_name, state_gov_Total_Debt, state_gov_Total_Debt_per_Capita) %>% 
rename(
  `State Name` = state_name,
  `State Gov Total Debt` = state_gov_Total_Debt, 
  `State Gov Debt per Capita` = state_gov_Total_Debt_per_Capita
) %>% 
  write.csv("output/data_wrapper/table1.3_state_gov_debt.csv", row.names = FALSE)

####Table 1.4: State Government Long-Term Debt####

state_entity_type_summary %>% 
  filter(entity_type == "State") %>% 
  select(state_name, non_current_liabilities, population) %>% 
  arrange(desc(non_current_liabilities)) %>% 
  mutate(
    state_gov_Total_Debt = paste0("$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"),
    
    state_gov_Total_Debt_per_Capita = paste0("$", format(round(non_current_liabilities / population), big.mark = ","))
  ) %>%
  
  select(state_name, state_gov_Total_Debt, state_gov_Total_Debt_per_Capita) %>% 
  rename(
    `State Name` = state_name,
    `State Gov Long Term Debt` = state_gov_Total_Debt, 
    `State Gov Long Term Debt per Capita` = state_gov_Total_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/table1.4_state_gov_longterm_debt.csv", row.names = FALSE)

####Table 1.5: County Government Debt####
#top 50 counties with the most total liabilities
county_data %>% select(state_name, entity_name, population, total_liabilities) %>% 
  arrange(desc(total_liabilities)) %>% 
  slice(1:50) %>% 
  mutate(
    entity_name = str_to_title(entity_name),

    Aggregate_Total_Debt = paste0(
      "$", format(round(total_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(total_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(
    `County Name` = entity_name,
    State = state_name,
    `County Gov Total Debt` = Aggregate_Total_Debt,
    `County Gov Debt per Capita ` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_1.5_County_Government_Debt.csv", row.names = FALSE)

####Table 1.6: County Government Long-Term Debt####

county_data %>% select(state_name, entity_name, population, non_current_liabilities) %>% 
  arrange(desc(non_current_liabilities)) %>% 
  slice(1:50) %>% 
  mutate(
    entity_name = str_to_title(entity_name),

    Aggregate_Total_Debt = paste0(
      "$", format(round(non_current_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(non_current_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(
    `County Name` = entity_name,
     State = state_name,
    `County Gov Long Term Debt` = Aggregate_Total_Debt,
    `County Gov Long Term Debt per Capita ` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_1.6_County_Government_longterm_Debt.csv", row.names = FALSE)
  
####Table 5.1: City Government Debt####
#top 50 cities with the most total liabilities
municipal_data %>% select(state_abbr, entity_name, population, total_liabilities) %>% 
  arrange(desc(total_liabilities)) %>% 
  slice(1:50) %>% 
  mutate(
    entity_name = str_to_title(entity_name),
    Aggregate_Total_Debt = paste0(
      "$", format(round(total_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(total_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(
    `City Name` = entity_name,
    State = state_abbr,
    `City Gov Total Debt` = Aggregate_Total_Debt,
    `City Gov Debt per Capita ` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_5.1_City_Government_Debt.csv", row.names = FALSE)

####Table 5.2: City Government Long-Term Debt####

municipal_data %>% select(state_abbr, entity_name, population, non_current_liabilities) %>% 
  arrange(desc(non_current_liabilities)) %>% 
  slice(1:50) %>% 
  mutate(
    entity_name = str_to_title(entity_name),
    Aggregate_Total_Debt = paste0(
      "$", format(round(non_current_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(non_current_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(
    `City Name` = entity_name,
    State = state_abbr,
    `City Gov Long Term Debt` = Aggregate_Total_Debt,
    `City Gov Long Term Debt per Capita ` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_5.2_City_Government_Longterm_Debt.csv", row.names = FALSE)

####Table 5.3: School District Debt####
school_district_data %>% select(state_name, entity_name, population, total_liabilities) %>% 
  arrange(desc(total_liabilities)) %>% 
  slice(1:50) %>% 
  mutate(
    entity_name = str_to_title(entity_name),
    Aggregate_Total_Debt = paste0(
      "$", format(round(total_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(total_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(
    `School District Name` = entity_name,
    State = state_name,
    `School District Total Debt` = Aggregate_Total_Debt,
    `School District Debt per Capita ` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_5.3_School_District_Debt.csv", row.names = FALSE)

####Table 5.2: City Government Long-Term Debt####

school_district_data %>% select(state_name, entity_name, population, non_current_liabilities) %>% 
  arrange(desc(non_current_liabilities)) %>% 
  slice(1:50) %>% 
  mutate(
    entity_name = str_to_title(entity_name),
    Aggregate_Total_Debt = paste0(
      "$", format(round(non_current_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(non_current_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(
    `School District Name` = entity_name,
     State = state_name,
    `School District Gov Long Term Debt` = Aggregate_Total_Debt,
    `School District Gov Long Term Debt per Capita ` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_5.4_School_District_Longterm_Debt.csv", row.names = FALSE)

####Table 2.1: Ranking of State and Local Debt####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, total_liabilities, population) %>% 
  arrange(desc(total_liabilities)) %>% 
  mutate(Rank = row_number()) %>% 
  
  mutate(
    Aggregate_Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
    Aggregate_Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
  ) %>%
  
  select(Rank, state_name, Aggregate_Total_Debt, Aggregate_Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Aggregate Total Debt` = Aggregate_Total_Debt,
    `Aggregate Debt per Capita` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_2.1_Ranking_state_local_Debt.csv", row.names = FALSE)

####Table 2.2: Ranking of State and Local Debt per Capita####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, total_liabilities, population) %>% 
  arrange(desc(total_liabilities / population)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Aggregate_Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
    Aggregate_Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Aggregate_Debt_per_Capita, Aggregate_Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Aggregate Debt per Capita` = Aggregate_Debt_per_Capita,
    `Aggregate Total Debt` = Aggregate_Total_Debt
  ) %>%
  write.csv("output/data_wrapper/Table_2.2_Ranking_state_local_Debt_perCap.csv", row.names = FALSE)

####Table 2.3: Ranking of State and Local Long-Term Debt####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, non_current_liabilities, population) %>% 
  arrange(desc(non_current_liabilities)) %>% 
  mutate(Rank = row_number()) %>% 
  
  mutate(
    Aggregate_Total_Debt = paste0("$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"),
    Aggregate_Debt_per_Capita = paste0("$", format(round(non_current_liabilities / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Aggregate_Total_Debt, Aggregate_Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Aggregate Long Term Debt` = Aggregate_Total_Debt,
    `Aggregate Long Term Debt per Capita` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_2.3_Ranking_state_local_Longterm_Debt.csv", row.names = FALSE)

####Table 2.4: Ranking of State and Local Long Term Debt per Capita####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, non_current_liabilities, population) %>% 
  arrange(desc(non_current_liabilities / population)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Aggregate_Total_Debt = paste0("$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"),
    Aggregate_Debt_per_Capita = paste0("$", format(round(non_current_liabilities / population), big.mark = ","))
  ) %>%
  
  select(Rank, state_name, Aggregate_Debt_per_Capita, Aggregate_Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Aggregate Long Term Debt per Capita` = Aggregate_Debt_per_Capita,
    `Aggregate Long Term Debt` = Aggregate_Total_Debt
  ) %>% 
  write.csv("output/data_wrapper/Table_2.4_Ranking_state_local_Longterm_Debt_perCap.csv", row.names = FALSE)

####Table 2.5: Ranking of State and Local Pension Debt####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, pension_liability, population) %>% 
  arrange(desc(pension_liability)) %>% 
  mutate(Rank = row_number()) %>% 
  
  mutate(
    Aggregate_Total_Debt = paste0("$", format(round(pension_liability / 1e9, 2), big.mark = ","), "B"),
    Aggregate_Debt_per_Capita = paste0("$", format(round(pension_liability / population), big.mark = ","))
  ) %>%
  
  select(Rank, state_name, Aggregate_Total_Debt, Aggregate_Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Aggregate Pension Debt per Capita` = Aggregate_Debt_per_Capita,
    `Aggregate Total Pension Debt` = Aggregate_Total_Debt
  ) %>% 
  write.csv("output/data_wrapper/Table_2.5_Ranking_state_local_Pension_Debt.csv", row.names = FALSE)

####Table 2.6: Ranking of State and Local Pension Debt per Capita####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, pension_liability, population) %>% 
  arrange(desc(pension_liability / population)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Aggregate_Total_Debt = paste0("$", format(round(pension_liability / 1e9, 2), big.mark = ","), "B"),
    Aggregate_Debt_per_Capita = paste0("$", format(round(pension_liability / population), big.mark = ","))
  ) %>%
  
  select(Rank, state_name, Aggregate_Debt_per_Capita, Aggregate_Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Aggregate Pension Debt per Capita` = Aggregate_Debt_per_Capita,
    `Aggregate Total Pension Debt` = Aggregate_Total_Debt
  ) %>% 
  write.csv("output/data_wrapper/Table_2.6_Ranking_state_local_Pension_Debt_perCap.csv", row.names = FALSE)

####Table 2.7: Ranking of State and Local Other-Post Employment Benefit Debt####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, opeb_liability, population) %>% 
  arrange(desc(opeb_liability)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Aggregate_Total_Debt = paste0("$", format(round(opeb_liability / 1e9, 2), big.mark = ","), "B"),
    Aggregate_Debt_per_Capita = paste0("$", format(round(opeb_liability / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Aggregate_Total_Debt, Aggregate_Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Aggregate OPEB per Capita` = Aggregate_Debt_per_Capita,
    `Aggregate Total OPEB` = Aggregate_Total_Debt
  ) %>% 
  write.csv("output/data_wrapper/Table_2.7_Ranking_state_local_OPEB_Debt.csv", row.names = FALSE)

####Table 2.8: Ranking of State and Local Other-Post Employment Benefit Debt per cap####

state_entity_type_summary %>% 
  filter(entity_type == "Overall") %>% 
  select(state_name, opeb_liability, population) %>%
    arrange(desc(opeb_liability / population)) %>% 
    mutate(Rank = row_number()) %>% 
  mutate(
    Aggregate_Total_Debt = paste0("$", format(round(opeb_liability / 1e9, 2), big.mark = ","), "B"),
    Aggregate_Debt_per_Capita = paste0("$", format(round(opeb_liability / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Aggregate_Debt_per_Capita, Aggregate_Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Aggregate OPEB per Capita` = Aggregate_Debt_per_Capita,
    `Aggregate Total OPEB` = Aggregate_Total_Debt
  ) %>% 
  write.csv("output/data_wrapper/Table_2.8_Ranking_state_local_OPEB_Debt_perCap.csv", row.names = FALSE)

####Table 2.9: Ranking of State and Local Outstanding Bonds, Loans, & Notes ####
  
  state_entity_type_summary %>% 
    filter(entity_type == "Overall") %>% 
    select(state_name, bond_loans_notes, population) %>% 
    arrange(desc(bond_loans_notes)) %>% 
    mutate(Rank = row_number()) %>% 
    mutate(
      Aggregate_Total_Debt = paste0("$", format(round(bond_loans_notes / 1e9, 2), big.mark = ","), "B"),
      Aggregate_Debt_per_Capita = paste0("$", format(round(bond_loans_notes / population), big.mark = ","))
    ) %>%
    select(Rank, state_name, Aggregate_Total_Debt, Aggregate_Debt_per_Capita) %>% 
    rename(
      `State` = state_name,
      `Aggregate Bonds, Loans, & Notes per capita` = Aggregate_Debt_per_Capita,
      `Aggregate Bonds, Loans, & Notes` = Aggregate_Total_Debt
    ) %>% 
  write.csv("output/data_wrapper/Table_2.9_Ranking_state_local_Aggregate Bonds_loans_Notes.csv", row.names = FALSE)
  
  ####Table 2.10: Ranking of State and Local Other-Post Employment Benefit Debt per cap####
  
  state_entity_type_summary %>% 
    filter(entity_type == "Overall") %>% 
    select(state_name, bond_loans_notes, population) %>%
    arrange(desc(bond_loans_notes / population)) %>% 
    mutate(Rank = row_number()) %>% 
    mutate(
      Aggregate_Total_Debt = paste0("$", format(round(bond_loans_notes / 1e9, 2), big.mark = ","), "B"),
      Aggregate_Debt_per_Capita = paste0("$", format(round(bond_loans_notes / population), big.mark = ","))
    ) %>%
    select(Rank, state_name, Aggregate_Debt_per_Capita, Aggregate_Total_Debt) %>% 
    rename(
      `State` = state_name,
      `Aggregate Bonds, Loans, & Notes per capita` = Aggregate_Debt_per_Capita,
      `Aggregate Bonds, Loans, & Notes` = Aggregate_Total_Debt
    ) %>% 
  write.csv("output/data_wrapper/Table_2.10_Ranking_state_local_bonds_loans_notes_perCap.csv", row.names = FALSE)

####Table 3.1: Ranking of State Government Total Debt####
state_data %>% 
  select(state_name, total_liabilities, population) %>% 
  arrange(desc(total_liabilities)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Total_Debt, Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Total Debt` = Total_Debt,
    `Debt per Capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.1_Ranking_state_total_debt.csv", row.names = FALSE)

####Table 3.2: Ranking of State Government Total Debt per Capita####

state_data %>% 
  select(state_name, total_liabilities, population) %>% 
  arrange(desc(total_liabilities / population)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Debt_per_Capita, Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Total Debt` = Total_Debt,
    `Debt per Capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.2_Ranking_state_total_debt_perCap.csv", row.names = FALSE)


####Table 3.3: Ranking of State Government Long-Term Debt####
state_data %>% 
  select(state_name, non_current_liabilities, population) %>% 
  arrange(desc(non_current_liabilities)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(non_current_liabilities / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Total_Debt, Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Total Long Term Debt` = Total_Debt,
    `Long Term Debt per Capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.3_Ranking_state_longterm_debt.csv", row.names = FALSE)

####Table 3.4: Ranking of State Government Long-Term Debt per Capita####

state_data %>% 
  select(state_name, non_current_liabilities, population) %>% 
  arrange(desc(non_current_liabilities / population)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(non_current_liabilities / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Debt_per_Capita, Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Total Long Term Debt` = Total_Debt,
    `Long Term Debt per Capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.4_Ranking_state_longterm_debt_perCap.csv", row.names = FALSE)

####Table 3.5: Ranking of State Government Pension Debt####
state_data %>% 
  select(state_name, pension_liability, population) %>% 
  arrange(desc(pension_liability)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(pension_liability / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(pension_liability / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Total_Debt, Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Total Pension Debt` = Total_Debt,
    `Pension Debt per Capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.5_Ranking_state_pension_debt.csv", row.names = FALSE)

####Table 3.6: Ranking of State Government Pension Debt per Capita####

state_data %>% 
  select(state_name, pension_liability, population) %>% 
  arrange(desc(pension_liability / population)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(pension_liability / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(pension_liability / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Debt_per_Capita, Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Total Pension Debt` = Total_Debt,
    `Pension Debt per Capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.6_Ranking_state_pension_debt_perCap.csv", row.names = FALSE)

####Table 3.7: Ranking of State Government OPEB Debt####
state_data %>% 
  select(state_name, opeb_liability, population) %>% 
  arrange(desc(opeb_liability)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(opeb_liability / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(opeb_liability / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Total_Debt, Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Total OPEB` = Total_Debt,
    `OPEB per Capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.7_Ranking_state_OPEB_debt.csv", row.names = FALSE)

####Table 3.8: Ranking of State Government OPEB Debt per Capita####

state_data %>% 
  select(state_name, opeb_liability, population) %>% 
  arrange(desc(opeb_liability / population)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(opeb_liability / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(opeb_liability / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Debt_per_Capita, Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Total OPEB` = Total_Debt,
    `OPEB per Capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.8_Ranking_state_OPEB_debt_perCap.csv", row.names = FALSE)

####Table 3.9: Ranking of State Government Outstanding Bonds, Loans, & Notes####
state_data %>% 
  select(state_name, bond_loans_notes, population) %>% 
  arrange(desc(bond_loans_notes)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(bond_loans_notes / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(bond_loans_notes / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Total_Debt, Debt_per_Capita) %>% 
  rename(
    `State` = state_name,
    `Bonds, Loans, & Notes` = Total_Debt,
    `Bonds, Loans, & Notes  per capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.9_Ranking_state_bondsloansnotes_debt.csv", row.names = FALSE)

####Table 3.10: Ranking of State Government Outstanding Bonds, Loans, & Notes per Capita####

state_data %>% 
  select(state_name, bond_loans_notes, population) %>% 
  arrange(desc(bond_loans_notes / population)) %>% 
  mutate(Rank = row_number()) %>% 
  mutate(
    Total_Debt = paste0("$", format(round(bond_loans_notes / 1e9, 2), big.mark = ","), "B"),
    Debt_per_Capita = paste0("$", format(round(bond_loans_notes / population), big.mark = ","))
  ) %>%
  select(Rank, state_name, Debt_per_Capita, Total_Debt) %>% 
  rename(
    `State` = state_name,
    `Bonds, Loans, & Notes` = Total_Debt,
    `Bonds, Loans, & Notes  per capita` = Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_3.10_Ranking_state_bondsloansnotes_debt_perCap.csv", row.names = FALSE)

#### County → Tables 4.1–4.10####
make_top50_tables(county_data, "county", "4")

#### Municipalities → Tables 5.1–5.10####
make_top50_tables(municipal_data, "municipal", "5")

#### Schools → Tables 6.1–6.10####
make_top50_tables(school_district_data, "school_districts", "6")
