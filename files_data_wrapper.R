library(tidyverse)
library(jsonlite)

state_aggregated <- readRDS("output/state_aggregated.rds")
overall_totals <- readRDS("output/overall_totals.RDS")
state_entity_type_summary <- readRDS("output/state_entity_type_summary.RDS")
county_data <- readRDS("output/county_data.rds")
municipal_data <- readRDS("output/municipal_data.rds")
school_district_data <- readRDS("output/school_district_data.rds")

state_data <- readRDS("output/state_data.rds")
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
  mutate(
    # Convert to billions and format with commas + "B"
    Aggregate_Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
    
    # Per capita (keep as number with commas)
    Aggregate_Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
  ) %>%
  arrange(desc(total_liabilities)) %>% 
  slice(1:10) %>% 
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
  mutate(
    # Convert to billions and format with commas + "B"
    Aggregate_Total_Debt = paste0(
      "$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"
    ),
    
    # Per capita (keep as number with commas)
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(non_current_liabilities / population), big.mark = ",")
    )
  ) %>%
  arrange(desc(non_current_liabilities)) %>% 
  slice(1:10) %>% 
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
  mutate(
    # Convert to billions and format with commas + "B"
    state_gov_Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
    
    # Per capita (keep as number with commas)
    state_gov_Total_Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
  ) %>%
  arrange(desc(total_liabilities)) %>% 
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
  mutate(
    # Convert to billions and format with commas + "B"
    state_gov_Total_Debt = paste0("$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"),
    
    # Per capita (keep as number with commas)
    state_gov_Total_Debt_per_Capita = paste0("$", format(round(non_current_liabilities / population), big.mark = ","))
  ) %>%
  arrange(desc(non_current_liabilities)) %>% 
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
    # Convert liabilities to billions with formatting
    Aggregate_Total_Debt = paste0(
      "$", format(round(total_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    
    # Per capita liabilities
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
    # Convert liabilities to billions with formatting
    Aggregate_Total_Debt = paste0(
      "$", format(round(non_current_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    
    # Per capita liabilities
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
municipal_data %>% select(state_name, entity_name, population, total_liabilities) %>% 
  arrange(desc(total_liabilities)) %>% 
  slice(1:50) %>% 
  mutate(
    entity_name = str_to_title(entity_name),
    # Convert liabilities to billions with formatting
    Aggregate_Total_Debt = paste0(
      "$", format(round(total_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    # Per capita liabilities
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(total_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(
    `City Name` = entity_name,
    State = state_name,
    `City Gov Total Debt` = Aggregate_Total_Debt,
    `City Gov Debt per Capita ` = Aggregate_Debt_per_Capita
  ) %>% 
  write.csv("output/data_wrapper/Table_5.1_City_Government_Debt.csv", row.names = FALSE)

####Table 5.2: City Government Long-Term Debt####

municipal_data %>% select(state_name, entity_name, population, non_current_liabilities) %>% 
  arrange(desc(non_current_liabilities)) %>% 
  slice(1:50) %>% 
  mutate(
    entity_name = str_to_title(entity_name),
    # Convert liabilities to billions with formatting
    Aggregate_Total_Debt = paste0(
      "$", format(round(non_current_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    # Per capita liabilities
    Aggregate_Debt_per_Capita = paste0(
      "$", format(round(non_current_liabilities / population), big.mark = ",")
    )
  ) %>%
  select(
    `City Name` = entity_name,
    State = state_name,
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
    # Convert liabilities to billions with formatting
    Aggregate_Total_Debt = paste0(
      "$", format(round(total_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    # Per capita liabilities
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
    # Convert liabilities to billions with formatting
    Aggregate_Total_Debt = paste0(
      "$", format(round(non_current_liabilities / 1e9, 1), big.mark = ","), "B"
    ),
    # Per capita liabilities
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

state_data





