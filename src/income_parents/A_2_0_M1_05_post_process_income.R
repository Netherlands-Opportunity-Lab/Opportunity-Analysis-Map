# post-process results 
library(tidyverse)
library(progress)
library(writexl)
library(stringr)

data_dir <- "input/income"
output_dir <- "output/income"
export_dir <- "export/income/"

# create paths
dr <- file.path(export_dir)
if (!dir.exists(dr)) dir.create(dr)

expectation_grid <- read_rds(file.path(output_dir, "expectation_grid.rds"))


#### CREATE NEW CATEGORICAL OUTCOME FOR NAs ####

expectation_grid <- 
  expectation_grid %>% 
  mutate(reason_of_na = NA)


# step 1: regressions with fewer than 11/12 observations

# for averages, degrees of freedom - 1, so n < 11
expectation_grid <-
  expectation_grid %>%
  mutate(
    not_disclose = ifelse((income_group == 'all' & n < 11 & is.na(reason_of_na)), TRUE, FALSE),
    reason_of_na = ifelse(not_disclose, '1. fewer than 11 obs', reason_of_na), 
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr),
    n = ifelse(not_disclose, NA, n)
  ) %>% select(-not_disclose)

# for low/middle/high parent, we use degrees of freedom - 1 so n < 12
expectation_grid <-
  expectation_grid %>%
  mutate(
    not_disclose = ifelse((income_group != 'all' & n < 12 & is.na(reason_of_na)), TRUE, FALSE),
    reason_of_na = ifelse(not_disclose, '1. fewer than 12 obs', reason_of_na), 
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr),
    n = ifelse(not_disclose, NA, n)
  ) %>% select(-not_disclose)


# step 2: regressions with est*n < 10 
expectation_grid <-
  expectation_grid %>%
  mutate(
    not_disclose = ifelse((binary == "TRUE" & est*n < 10 & is.na(reason_of_na)), TRUE, FALSE),
    reason_of_na = ifelse(not_disclose, '2. est*n < 10', reason_of_na),
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr),
    n = ifelse(not_disclose, NA, n)
  ) %>% select(-not_disclose)


# step 3: regressions with (1-est)*n < 10
expectation_grid <-
  expectation_grid %>%
  mutate(
    not_disclose = ifelse((binary == "TRUE" & (1-est)*n < 10 & is.na(reason_of_na)), TRUE, FALSE),
    reason_of_na = ifelse(not_disclose, '3. (1-est)*n < 10', reason_of_na),
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr),
    n = ifelse(not_disclose, NA, n)
  ) %>% select(-not_disclose)


# step 4: regressions with est == 0 
expectation_grid <-
  expectation_grid %>%
  mutate(
    not_disclose = ifelse((
      !str_detect(outcome, '_class_') & !str_detect(outcome, '_neighborhood_') &
        est == as.integer(0) & binary == FALSE &  !is.na(est) & is.na(reason_of_na)), 
      TRUE, FALSE),
    reason_of_na = ifelse(not_disclose, '4. est == 0', reason_of_na),
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr),
    n = ifelse(not_disclose, NA, n)
  ) %>% select(-not_disclose)


# step 5: remove estimates with the mentioned postcodes
postcodes <- c("9021", "8843", "4529", "9163")

expectation_grid <-
  expectation_grid %>%
  mutate(
    not_disclose = ifelse((
      str_detect(outcome, '_class_') &  region_type == 'postcode4' & 
        region_id %in% postcodes), 
      TRUE, FALSE),
    reason_of_na = ifelse(not_disclose, '5. n of school < 3', reason_of_na),
    est = ifelse(not_disclose, NA, est),
    lwr = ifelse(not_disclose, NA, lwr),
    upr = ifelse(not_disclose, NA, upr),
    n = ifelse(not_disclose, NA, n)
  ) %>% select(-not_disclose)
    
    

# rounding number of observations to the nearest ten numbers
expectation_grid <-
  expectation_grid %>%
  mutate(n = round(n, -1))


# Separate (outcome files per region) ----
pb <- progress_bar$new(total = length(unique(expectation_grid$outcome))*nlevels(expectation_grid$region_type))
for (oc in unique(expectation_grid$outcome)) {
  
  if (oc %in% c("c11_youth_protection", "c16_youth_protection")) {
    region_levels <- c('all', 'municipality_code_birth', 'postcode3_birth', 
                       'postcode4_birth', 'neighborhood_code_birth')
    
  } else if (grepl('^c35_', oc)) {
    region_levels <- c('all', 'corop_code', 'municipality_code', 'postcode3', 'postcode4', 'neighborhood_code')
    
    
  } else {
    region_levels <- c('all', 'municipality_code', 'postcode3', 'postcode4', 'neighborhood_code')
    
  }
  
  fp <- file.path(dr, paste0(oc, ".csv"))
  df <- expectation_grid %>% 
    filter(outcome == oc) %>% 
    select(outcome, binary, income_group, gender_group, migration_group, 
           household_group, region_type, region_id, n, est, lwr, upr, reason_of_na)
  write_excel_csv(df, fp, na = "")
  pb$tick()
}


# # Separate (outcome files per region) ----
# pb <- progress_bar$new(total = length(unique(expectation_grid$outcome))*nlevels(expectation_grid$region_type))
# for (oc in unique(expectation_grid$outcome)) {
#   dr <- file.path(export_dir, oc)
#   if (!dir.exists(dr)) dir.create(dr)
#   
#   
#   if (oc %in% c("c11_youth_protection", "c16_youth_protection")) {
#     region_levels <- c('all', 'municipality_code_birth', 'postcode3_birth', 
#                        'postcode4_birth', 'neighborhood_code_birth')
#     
#   } else if (grepl('^c35_', oc)) {
#     region_levels <- c('all', 'corop_code', 'municipality_code', 'postcode3', 'postcode4', 'neighborhood_code')
#     
#     
#   } else {
#     region_levels <- c('all', 'municipality_code', 'postcode3', 'postcode4', 'neighborhood_code')
#     
#   }
#   
#   for (rt in region_levels) {
#     fp <- file.path(dr, paste0(oc, "_", rt, ".csv"))
#     df <- expectation_grid %>% 
#       filter(outcome == oc, region_type == rt) %>% 
#       select(outcome, income_group, gender_group, migration_group, household_group, 
#              region_type, region_id, n, est, lwr, upr, reason_of_na)
#     write_excel_csv(df, fp, na = "")
#     pb$tick()
#   }
# }

