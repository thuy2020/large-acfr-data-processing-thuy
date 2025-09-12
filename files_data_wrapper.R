library(tidyverse)
library(jsonlite)

state_aggregated <- readRDS("output/state_aggregated.rds")
overall_totals <- readRDS("output/overall_totals.RDS")
state_entity_type_summary <- readRDS("output/state_entity_type_summary.RDS")
# <- readRDS("output/state_aggregated.rds")
# <- readRDS("output/state_aggregated.rds")
# <- readRDS("output/state_aggregated.rds")

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

