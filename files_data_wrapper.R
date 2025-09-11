library(tidyverse)
library(jsonlite)

#compare files with Thuy's github

state_tn <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_states_2023_20250721_1315.csv") %>% 
  arrange(state.name) %>% 
  select(-1)

state_jc <- read_csv("input/all_states_2023.csv") %>% 
  arrange(state.name) %>% 
  select(-1)

identical(state_tn, state_jc)  

df_remote <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_counties_2023_20250721_1045.csv") %>% 
  arrange(state.name, name) %>% 
  select(-1)

df_local <- read_csv("input/all_counties_2023.csv") %>% 
  arrange(state.name, name) %>% 
  select(-1)

library(dplyr)

# Sort them by a stable key (optional, but keeps results consistent)
df_local_sorted <- df_local %>% arrange(state.name, state)
df_remote_sorted <- df_remote %>% arrange(state.name, state)

df_local %>%
  filter(
    duplicated(across(c(state.name, name, id))) |
      duplicated(across(c(state.name, name, id)), fromLast = TRUE)
  ) %>% View()

#check dup
df_remote %>%
  filter(
    duplicated(across(c(state.name, name))) |
      duplicated(across(c(state.name, name)), fromLast = TRUE)
  ) %>% View()


#muni
muni_tn <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_municipalities_2023_20250721_1133.csv")

muni_jc <- read_csv("input/all_municipalities_2023.csv") %>% 
  filter(name %in% c("indiantown", "jea", "westfield township"))

#school 
school_tn <- read_csv("https://raw.githubusercontent.com/thuy2020/acfrs_data/refs/heads/main/output/all_schooldistricts_2023_20250722_2023.csv") %>% 
  select(-1)
school_jc <- read_csv("input/all_schooldistricts_2023.csv") %>% 
  select(-1)
  
  
setdiff(df_local$id, df_remote$id)
setdiff(df_remote$id, df_local$id)

df_local %>% 
  filter(duplicated(id))

sum(df_local$total_liabilities, na.rm = TRUE)
sum(df_remote$total_liabilities, na.rm = TRUE)
# Rows in remote but not in local
extra_row <- anti_join(df_remote_sorted, df_local_sorted)

# Optionally: check if there's anything in local but not remote
missing_row <- anti_join(df_local_sorted, df_remote_sorted)

identical(county_tn, county_jc)

# Show differences if not identical
if (!identical(df_local, df_remote)) {
  # Show differences in structure
  print(paste("Shape local:", paste(dim(df_local), collapse = " x ")))
  print(paste("Shape remote:", paste(dim(df_remote), collapse = " x ")))
  
  # Compare column names
  print(setdiff(names(df_local), names(df_remote)))
  print(setdiff(names(df_remote), names(df_local)))
  
  # Compare actual content
  diffs <- anti_join(df_local, df_remote)
  print(head(diffs))
}
