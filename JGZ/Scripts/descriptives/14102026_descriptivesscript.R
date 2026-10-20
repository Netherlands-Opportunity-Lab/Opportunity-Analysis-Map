library(haven)
library(writexl)

data <- read_dta("H:/JGZ-Pilot-Data/Data_CatchmentArea_3pooled_morevariables.dta")

data <- data %>%
  mutate(
  d_never = ifelse(d_obs_cat == 1, 1, 0), 
  d_always = ifelse(d_obs_cat == 2, 1, 0), 
  d_least_once_not_alw = ifelse(d_obs_cat != 1 & d_obs_cat != 2, 1, 0)
  )

variables <- c("c_birth_order", "ETNGRP", "cm_inc_decile", "c_educ_hbo_wo", "c_address_parents", "age_mom_3groups",  "c_lms5_pa",  "c_lms5_ma", "gbageslacht", 
               "pa_d_mental_care", "ma_d_mental_care", "health_exp_decile", "d_parents_unknown", "d_health_center_3y", "d_christian_3y" )
all <-data.frame()

for (var in variables) {
  
  new <- data %>%
    mutate(
      !!var := as_factor(get(var)) 
    ) %>%
    group_by(get(var)) %>%
    summarize(
      N = n(),
      total_pr = N / nrow(data),
      d_never_N = sum(d_never == 1, na.rm = TRUE), 
      d_never_pr = d_never_N/N, 
      d_always_N = sum(d_always == 1, na.rm = TRUE), 
      d_always_pr = d_always_N/N,
      d_least_once_not_alw_N = sum(d_least_once_not_alw == 1, na.rm = TRUE), 
      d_least_once_not_alw_pr = d_least_once_not_alw_N/N,
    ) %>%
    mutate(
      sup_total = N < 10 | (nrow(data) - N) < 10,
      sup_never = d_never_N < 10 | (N - d_never_N) < 10,
      sup_always = d_always_N < 10 | (N - d_always_N) < 10,
      sup_least = d_least_once_not_alw_N < 10 | (N- d_least_once_not_alw_N) < 10,
      
      N = ifelse(sup_total, NA, N), 
      total_pr = ifelse(sup_total, NA, total_pr),
      d_never_N = ifelse(sup_never, NA, d_never_N), 
      d_never_pr = ifelse(sup_never, NA, d_never_pr), 
      d_always_N = ifelse(sup_always, NA, d_always_N), 
      d_always_pr = ifelse(sup_always, NA, d_always_pr), 
      d_least_once_not_alw_N = ifelse(sup_least, NA, d_least_once_not_alw_N), 
      d_least_once_not_alw_pr = ifelse(sup_least, NA, d_least_once_not_alw_N), 
      variable = paste0(var)
    ) %>%
    select(-starts_with("sup_"))
  
  all <- bind_rows(all, new)
}

write_xlsx(all, paste0("H:/JGZ-Pilot-Data/Descriptives Results/descriptivestotal.xlsx"))

#For org separately
variables <- c("c_birth_order", "ETNGRP", "cm_inc_decile", "c_educ_hbo_wo", "c_address_parents", "age_mom_3groups",  "c_lms5_pa",  "c_lms5_ma", "gbageslacht", 
               "pa_d_mental_care", "ma_d_mental_care", "health_exp_decile", "d_parents_unknown", "d_health_center_3y", "d_christian_3y" )
orgs <- c("6", "7", "10", "12")


secondary_supression <- function(df) {
  df <- as.data.frame(df)
  cnt <- c("N", "d_never_N", "d_always_N", "d_least_once_not_alw_N")
  sup <- c("sup_total", "sup_never", "sup_always", "sup_least")
  repeat {
    before <- df[sup]
    # row N hidden --> hide all outcomes
    df[df$sup_total, sup[-1]] <- TRUE
    #row exactly 1 outcome hidden -> al;so hide the smallest visible one
    for (i in seq_len(nrow(df))) {
      s <- unlist(df[i, sup[-1]])
      if (sum(s) == 1) {
        vis <- which(!s)
        df[i, sup[-1][vis[which.min(unlist(df[i, cnt[-1]])[vis])]]] <- TRUE
      }
    }
    #column exactly 1 level hidden -> also hide the smalles visible level
    for (j in 1:4) {
      s <- df[[sup[j]]]
      if (sum(s) == 1 & any(!s)) {
        vis <- which(!s)
        df[[sup[j]]][vis[which.min(df[[cnt[j]]][vis])]] <- TRUE
      }
    }
    if (identical(before, df[sup])) break
  }
  df
}

sheets <- list()

for (o in orgs) {

data_o <- data %>% filter(org_catchment == o)
n_o <- nrow(data_o)

all <- data.frame()

for (var in variables) {
  
  new <- data_o %>%
    mutate(
      !!var := as_factor(get(var)) 
    ) %>%
    group_by(level = get(var), .drop = FALSE) %>%
    summarize(
      N = n(),
      total_pr = N / n_o,
      d_never_N = sum(d_never == 1, na.rm = TRUE), 
      d_never_pr = d_never_N/N, 
      d_always_N = sum(d_always == 1, na.rm = TRUE), 
      d_always_pr = d_always_N/N,
      d_least_once_not_alw_N = sum(d_least_once_not_alw == 1, na.rm = TRUE), 
      d_least_once_not_alw_pr = d_least_once_not_alw_N/N,
    ) %>%
    mutate(
      sup_total = N < 10 | (n_o - N) < 10,
      sup_never = d_never_N < 10 | (N - d_never_N) < 10,
      sup_always = d_always_N < 10 | (N - d_always_N) < 10,
      sup_least = d_least_once_not_alw_N < 10 | (N- d_least_once_not_alw_N) < 10,
    ) %>%
    secondary_supression() %>%
    mutate(
      N = ifelse(sup_total, NA, N), 
      total_pr = ifelse(sup_total, NA, total_pr),
      d_never_N = ifelse(sup_never, NA, d_never_N), 
      d_never_pr = ifelse(sup_never, NA, d_never_pr), 
      d_always_N = ifelse(sup_always, NA, d_always_N), 
      d_always_pr = ifelse(sup_always, NA, d_always_pr), 
      d_least_once_not_alw_N = ifelse(sup_least, NA, d_least_once_not_alw_N), 
      d_least_once_not_alw_pr = ifelse(sup_least, NA, d_least_once_not_alw_pr), 
      variable = paste0(var)
    ) %>%
    select(-starts_with("sup_"))
  all <- bind_rows(all, new)
}

sheets[[paste0("org_", o)]] <- all
}


write_xlsx(c(sheets),
           "H:/JGZ-Pilot-Data/Descriptives Results/descriptives_perorg.xlsx")
