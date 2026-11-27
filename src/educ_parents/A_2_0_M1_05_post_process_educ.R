# post-process results 
library(tidyverse)
library(progress)
library(writexl)
library(stringr)


data_dir <- "input/educ"
output_dir <- "output/educ"
export_dir <- "export/educ/"

# create paths
dr <- file.path(export_dir)
if (!dir.exists(dr)) dir.create(dr)


expectation_grid <- read_rds(file.path(output_dir, "expectation_grid.rds"))



#### CBS OUTPUT RULES ####
# The rules and reason codes are in the shared file src/lib/suppression_functions.R
# (the same in Opportunity-Analysis-Gap and -Intragenerational). reason_of_na
# holds ALL the reasons of a cell, separated by "; " (27-11-2026; before, only
# the first rule that applied). A cell with one reason has the same text as before.
#   0. no estimate (the regression gave none)
#   1. fewer than 11 obs (every education group is a mean)
#   2. est*n < 10 and 3. (1-est)*n < 10 (binary outcomes)
#   4. est == 0 (continuous outcomes, not the _class_ and _neighborhood_ ones)
#   5. n of school < 3 (_class_ outcomes in the PC4 areas below)
# Suppressed cells: est, lwr, upr and n NA. n is rounded to tens.
source(file.path("src", "lib", "suppression_functions.R"))

# PC4 areas with fewer than 3 schools
postcodes <- c("9021", "8843", "4529", "9163")

expectation_grid <- kaart_suppress(expectation_grid,
                                   regression = FALSE,
                                   min_n_mean = 11, min_n_regression = 12, min_count = 10,
                                   school_postcodes = postcodes, round_n = 10)

print(table(expectation_grid$reason_of_na, useNA = "ifany"))



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
    select(outcome, binary, education_group, gender_group, migration_group, household_group, 
           region_type, region_id, n, est, lwr, upr, reason_of_na)
  write_excel_csv(df, fp, na = "")
  pb$tick()
}



