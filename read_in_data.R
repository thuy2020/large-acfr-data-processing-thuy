library(dplyr)
# Read all data files
state_data_input <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_states_2023_20250912_1109.csv") %>% 
  select(-1)

county_data_input <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_counties_2023_20250914_1051.csv") %>% 
  select(-1)

municipal_data_input <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_municipalities_2023_20250914_1058.csv") %>% 
  select(-1)

school_district_data_input <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_schooldistricts_2023_20250913_1325.csv") %>% 
  select(-1)
