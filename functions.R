library(tidyverse)
library(jsonlite)
county_data <- readRDS("output/county_data.rds")
municipal_data <- readRDS("output/municipal_data.rds")
school_district_data <- readRDS("output/school_district_data.rds")

####Function####

make_top50_tables <- function(data, category, table_prefix) {
  # mapping of metric names to columns and labels
  metrics <- list(
    list(col = "total_liabilities", label_total = "Total Debt", label_percap = "Debt per Capita"),
    list(col = "non_current_liabilities", label_total = "Total Long Term Debt", label_percap = "Long Term Debt per Capita"),
    list(col = "pension_liability", label_total = "Total Pension Debt", label_percap = "Pension Debt per Capita"),
    list(col = "opeb_liability", label_total = "Total OPEB", label_percap = "OPEB per Capita"),
    list(col = "bond_loans_notes", label_total = "Bonds, Loans, & Notes", label_percap = "Bonds, Loans, & Notes per Capita")
  )
  
  # loop over metrics
  for (i in seq_along(metrics)) {
    m <- metrics[[i]]
    
    # --- TOTAL version ---
    df_total <- data %>% 
      select(entity_name, state_abbr, population, !!sym(m$col)) %>%
      arrange(desc(.data[[m$col]])) %>%
      slice(1:50) %>%
      mutate(Rank = row_number()) %>%
      mutate(
        entity_name = str_to_title(entity_name),
        Total_Debt = paste0("$", format(round(.data[[m$col]] / 1e9, 2), big.mark = ","), "B"),
        Debt_per_Capita = paste0("$", format(round(.data[[m$col]] / population), big.mark = ","))
      ) %>%
      select(Rank, entity_name, state_abbr, Total_Debt, Debt_per_Capita) %>%
      rename(
        `Name` = entity_name,
        `State abb` = state_abbr,
        !!m$label_total := Total_Debt,
        !!m$label_percap := Debt_per_Capita
      )
    
    write.csv(
      df_total, 
      sprintf("output/data_wrapper/Table_%s.%d_Ranking_%s_%s.csv", 
              table_prefix, (i - 1) * 2 + 1, category, gsub(" ", "", tolower(m$label_total))), 
      row.names = FALSE
    )
    
    # --- PER CAPITA version ---
    df_percap <- data %>%
      select(entity_name, state_abbr, population, !!sym(m$col)) %>%
      arrange(desc(.data[[m$col]] / population)) %>%
      slice(1:50) %>%
      mutate(Rank = row_number()) %>%
      mutate(
        entity_name = str_to_title(entity_name),
        Total_Debt = paste0("$", format(round(.data[[m$col]] / 1e9, 2), big.mark = ","), "B"),
        Debt_per_Capita = paste0("$", format(round(.data[[m$col]] / population), big.mark = ","))
      ) %>%
      select(Rank, entity_name, state_abbr, Debt_per_Capita, Total_Debt) %>%
      rename(
        `Name` = entity_name,
        `State abb` = state_abbr,
        !!m$label_total := Total_Debt,
        !!m$label_percap := Debt_per_Capita
      )
    
    write.csv(
      df_percap, 
      sprintf("output/data_wrapper/Table_%s.%d_Ranking_%s_%s_perCap.csv", 
              table_prefix, (i - 1) * 2 + 2, category, gsub(" ", "", tolower(m$label_total))), 
      row.names = FALSE
    )
  }
}


# ####total debt####
# county_data %>% 
#   select(entity_name, state_abbr, total_liabilities, population) %>% 
#   arrange(desc(total_liabilities)) %>% 
#   slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Total_Debt, Debt_per_Capita) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Total Debt` = Total_Debt,
#     `Debt per Capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.1_Ranking_county_debt.csv", row.names = FALSE)
# 
# ####debt per cap####
# county_data %>% 
#   select(entity_name, state_abbr, total_liabilities, population) %>%  
#   arrange(desc(total_liabilities / population)) %>% 
#   slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(total_liabilities / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(total_liabilities / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Debt_per_Capita, Total_Debt) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Total Debt` = Total_Debt,
#     `Debt per Capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.2_Ranking_county_debt_perCap.csv", row.names = FALSE)
# 
# ####long term debt####
# county_data %>% 
#   select(entity_name, state_abbr, non_current_liabilities, population) %>%   
#   arrange(desc(non_current_liabilities)) %>% 
#   slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(non_current_liabilities / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Total_Debt, Debt_per_Capita) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Total Long Term Debt` = Total_Debt,
#     `Long Term Debt per Capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.3_Ranking_county_longterm_debt.csv", row.names = FALSE)
# 
# ####long term debt per cap####
#   county_data %>% 
#   select(entity_name, state_abbr, non_current_liabilities, population) %>% 
#   arrange(desc(non_current_liabilities / population)) %>% 
#   slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(non_current_liabilities / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(non_current_liabilities / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Debt_per_Capita, Total_Debt) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Total Long Term Debt` = Total_Debt,
#     `Long Term Debt per Capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.4_Ranking_county_longterm_debt_perCap.csv", row.names = FALSE)
# 
# ####pension_liability####
#   county_data %>% 
#   select(state_abbr, entity_name, pension_liability, population) %>% 
#   arrange(desc(pension_liability)) %>% 
#     slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(pension_liability / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(pension_liability / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Total_Debt, Debt_per_Capita) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Total Pension Debt` = Total_Debt,
#     `Pension Debt per Capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.5_Ranking_county_pension_debt.csv", row.names = FALSE)
# 
# ####pension_liability per cap####
# 
#   county_data %>% 
#   select(state_abbr, entity_name, pension_liability, population) %>% 
#   arrange(desc(pension_liability / population)) %>% 
#     slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(pension_liability / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(pension_liability / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Debt_per_Capita, Total_Debt) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Total Pension Debt` = Total_Debt,
#     `Pension Debt per Capita` = Debt_per_Capita
#   ) %>%
#   write.csv("output/data_wrapper/Table_4.6_Ranking_county_pension_debt_perCap.csv", row.names = FALSE)
# 
# ####OPEP####
#   county_data %>% 
#   select(state_abbr, entity_name, opeb_liability, population) %>% 
#   arrange(desc(opeb_liability)) %>% 
#     slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(opeb_liability / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(opeb_liability / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Total_Debt, Debt_per_Capita) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Total OPEB` = Total_Debt,
#     `OPEB per Capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.7_Ranking_county_OPEB_debt.csv", row.names = FALSE)
# 
# ####OPEP per cap####
#   county_data %>% 
#   select(state_abbr, entity_name, opeb_liability, population) %>% 
#   arrange(desc(opeb_liability / population)) %>% 
#     slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(opeb_liability / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(opeb_liability / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Debt_per_Capita, Total_Debt) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Total OPEB` = Total_Debt,
#     `OPEB per Capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.8_Ranking_county_OPEB_debt_perCap.csv", row.names = FALSE)
# 
# ####Bonds loans notes####
#   county_data %>% 
#   select(state_abbr, entity_name, bond_loans_notes, population) %>% 
#   arrange(desc(bond_loans_notes)) %>% 
#     slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(bond_loans_notes / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(bond_loans_notes / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Total_Debt, Debt_per_Capita) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Bonds, Loans, & Notes` = Total_Debt,
#     `Bonds, Loans, & Notes per capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.9_Ranking_county_bondsloansnotes_debt.csv", row.names = FALSE)
#   
# ####Bonds loans notes per cap####
#   county_data %>% 
#   select(state_abbr, entity_name, bond_loans_notes, population) %>% 
#   arrange(desc(bond_loans_notes / population)) %>% 
#     slice(1:50) %>% 
#   mutate(Rank = row_number()) %>% 
#   
#   mutate(
#     entity_name = str_to_title(entity_name),
#     Total_Debt = paste0("$", format(round(bond_loans_notes / 1e9, 2), big.mark = ","), "B"),
#     Debt_per_Capita = paste0("$", format(round(bond_loans_notes / population), big.mark = ","))
#   ) %>%
#   select(Rank, entity_name, state_abbr, Debt_per_Capita, Total_Debt) %>% 
#   rename(
#     `Name` = entity_name,
#     `State abb` = state_abbr,
#     `Bonds, Loans, & Notes` = Total_Debt,
#     `Bonds, Loans, & Notes per capita` = Debt_per_Capita
#   ) %>% 
#   write.csv("output/data_wrapper/Table_4.10_Ranking_county_bondsloansnotes_debt_perCap.csv", row.names = FALSE)