# /stat_analyses/02b.supp_extract_events_fa_strat.R
# Code to extract the number of exposed / matched unexposed from the matched cohorts
# created in /control_matching/Matching_controls_FA_strata.r
# Number of events are extracted for
# each cohort (pregnancy outcome, and early childhood
# developmental concerns) and for each ASM.
# The number of events is then checked against /data/stats/n_events_lookup.xlsx
# to determine which model can be used for each cohort/ASM group. 

# 1.1) Housekeeping -------------------------------------------------------

rm(list = ls())
gc()

library(dplyr)
library(tidyverse)

#source filepaths
source("05-stat_analysis/00.stat_setup.r")

# 2.1) Pregnancy cohort  --------------------------------------------

# Pregnancy outcome 
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_preg_outcomes_folic_cohort.rds") )

# check the outcome format (should be binary 0/1)
table(data$pregnancy_loss)

# Convert from No/Yes to 0/1
data <- data %>% 
  mutate(pregnancy_loss = as.numeric(pregnancy_loss == "Yes"))

# Total
preg_any_exposed <- data %>% 
  filter(exposed_any_asm == 1) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_any_unexposed <- data %>% 
  filter(exposed_any_asm == 0) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_any <- bind_rows(preg_any_exposed, preg_any_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Any ASM')



# Carbamazepine
car_cohort <- data %>% 
  filter(exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

preg_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 1) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepinePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 0) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepinePregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_car <- bind_rows(preg_car_exposed, preg_car_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Carbamazepine')


# Lamotrigine
lam_cohort <- data %>% 
  filter(exposed_lamotrigine_mono == 1) %>% 
  distinct(groupID)

preg_lam_exposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 1) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotriginePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_lam_unexposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 0) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotriginePregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_lam <- bind_rows(preg_lam_exposed, preg_lam_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Lamotrigine')

# Levetiracetam
lev_cohort <- data %>% 
  filter(exposed_levetiracetam_mono == 1) %>% 
  distinct(groupID)

preg_lev_exposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 1) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_lev_unexposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 0) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_lev <- bind_rows(preg_lev_exposed, preg_lev_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Levetiracetam')


# Gabapentin
gab_cohort <- data %>% 
  filter(exposed_gabapentin_mono == 1) %>% 
  distinct(groupID)

preg_gab_exposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 1) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_gab_unexposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 0) %>% 
  group_by(pregnancy_loss, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_gab <- bind_rows(preg_gab_exposed, preg_gab_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Gabapentin')


# Bind together
preg_full <- bind_rows(preg_any, preg_car, preg_lam, preg_lev, preg_gab)


# 2.3) Developmental cohort  --------------------------------------------

# Developmental outcome 
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_developmental_folic_cohort.rds") )

# check the outcome format (should be binary 0/1)
table(data$any_dev_excl_v_h)

# clogit() expects a 0/1 outcome
# Convert from No/Yes to 0/1
data <- data %>% 
  mutate(any_dev_concern = as.numeric(any_dev_excl_v_h == "Y"))

# Total
dev_any_exposed <- data %>% 
  filter(exposed_any_asm == 1) %>% 
  group_by(any_dev_concern, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_any_unexposed <- data %>% 
  filter(exposed_any_asm == 0) %>% 
  group_by(any_dev_concern, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_any <- bind_rows(dev_any_exposed, dev_any_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Any ASM')

# Carbamazepine
car_cohort <- data %>% 
  filter(exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

dev_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 1) %>% 
  group_by(any_dev_concern, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 0) %>% 
  group_by(any_dev_concern, high_dose_folic_acid) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_car <- bind_rows(dev_car_exposed, dev_car_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Carbamazepine')


# Bind together
dev_full <- bind_rows(dev_any, dev_car)

# 2.4) Combine full cohorts and save out ----------------------------------

full <- bind_rows(preg_full, dev_full)

write_csv(full, paste0(folder_data_path, "stats/n_exp_unexp_fa_strat.csv"))
