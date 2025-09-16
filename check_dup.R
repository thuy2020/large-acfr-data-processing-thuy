library(dplyr)
source("state_aggregated.R")

####Check in processed data ####
### Helper function to check duplicates within a dataset
check_within_duplicates <- function(df, df_name) {
  dupes <- df %>%
    count(entity_id, sort = TRUE) %>%
    filter(n > 1)
  
  if (nrow(dupes) > 0) {
    message("⚠️ Duplicates found in ", df_name, ":")
    print(dupes)
  } else {
    message("✅ No duplicates in ", df_name)
  }
}

### 1. Check within each dataset
check_within_duplicates(state_data, "state_data")
check_within_duplicates(county_data, "county_data")
check_within_duplicates(municipal_data, "municipal_data")
check_within_duplicates(school_district_data, "school_district_data")

### 2. Check duplicates across datasets
all_ids <- bind_rows(
  state_data %>% select(entity_id) %>% mutate(source = "state"),
  county_data %>% select(entity_id) %>% mutate(source = "county"),
  municipal_data %>% select(entity_id) %>% mutate(source = "municipal"),
  school_district_data %>% select(entity_id) %>% mutate(source = "school_district")
)

dupes_across <- all_ids %>%
  group_by(entity_id) %>%
  summarise(sources = paste(unique(source), collapse = ", "), n = n()) %>%
  filter(n > 1)

if (nrow(dupes_across) > 0) {
  message("⚠️ Duplicate IDs found across categories:")
  print(dupes_across)
} else {
  message("✅ No duplicate IDs across categories")
}


####Check dub in in _input data ####

check_within_duplicates_inputdata <- function(df, df_name) {
  dupes <- df %>%
    count(id, sort = TRUE) %>%
    filter(n > 1)
  
  if (nrow(dupes) > 0) {
    message("⚠️ Duplicates found in ", df_name, ":")
    print(dupes)
  } else {
    message("✅ No duplicates in ", df_name)
  }
}


### 1. Check within each dataset
check_within_duplicates_inputdata(state_data_input, "state_data_input")
check_within_duplicates_inputdata(county_data_input, "county_data_input")
check_within_duplicates_inputdata(municipal_data_input, "municipal_data_input")
check_within_duplicates_inputdata(school_district_data_input, "school_district_data_input")

### 2. Check duplicates across datasets
all_ids_inputdata <- bind_rows(
  state_data_input %>% select(id) %>% mutate(source = "state"),
  county_data_input %>% select(id) %>% mutate(source = "county"),
  municipal_data_input %>% select(id) %>% mutate(source = "municipal"),
  school_district_data_input %>% select(id) %>% mutate(source = "school_district")
)

dupes_across_inputdata <- all_ids_inputdata %>%
  group_by(id) %>%
  summarise(sources = paste(unique(source), collapse = ", "), n = n()) %>%
  filter(n > 1)

if (nrow(dupes_across_inputdata) > 0) {
  message("⚠️ Duplicate IDs found across categories:")
  print(dupes_across_inputdata)
} else {
  message("✅ No duplicate IDs across categories")
}

#check those is.na(id)

#These are counties NOT collected yet
county_data_input %>% filter(is.na(id)) %>% View()

#These are big muni not collected yet
municipal_data_input %>% filter(is.na(id)) %>% View()

#These are sd that we have data, but no id: NYC, boston, delaware + 2 big sd NOT collected yet
school_district_data_input %>% filter(is.na(id)) %>% View()
