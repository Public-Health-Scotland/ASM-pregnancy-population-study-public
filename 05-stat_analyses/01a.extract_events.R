# /stat_analyses/01a.extract_events.R
# Code to extract the number of exposed / matched unexposed from the matched cohorts
# created in /control_matching/Matching_controls_main.r and 
# /control_matching/Matching_controls_2nd_sens.r. Number of events are extracted for
# each cohort (pregnancy outcome, congenital conditions, and early childhood
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

# 1.2) Create function for checking number of events ----------------------


# 2.1) Pregnancy cohort - full --------------------------------------------

# Pregnancy outcome - full cohort
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_preg_outcomes_cohort.rds") )

# check the outcome format (should be binary 0/1)
table(data$pregnancy_loss)

# Convert from No/Yes to 0/1
data <- data %>% 
  mutate(pregnancy_loss = as.numeric(pregnancy_loss == "Yes"))

# Total
preg_any_exposed <- data %>% 
  filter(exposed_any_asm == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                      pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_any_unexposed <- data %>% 
  filter(exposed_any_asm == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_any <- bind_rows(preg_any_exposed, preg_any_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Any ASM')

# Valproate
val_cohort <- data %>% 
  filter(exposed_valproate_mono == 1) %>% 
  distinct(groupID)

preg_val_exposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproatePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                             pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_val_unexposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproatePregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_val <- bind_rows(preg_val_exposed, preg_val_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Valproate')


# Topiramate
top_cohort <- data %>% 
  filter(exposed_topiramate_mono == 1) %>% 
  distinct(groupID)

preg_top_exposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramatePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_top_unexposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramatePregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_top <- bind_rows(preg_top_exposed, preg_top_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Topiramate')


# Carbamazepine
car_cohort <- data %>% 
  filter(exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

preg_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepinePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
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
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotriginePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_lam_unexposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
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
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_lev_unexposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
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
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_gab_unexposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_gab <- bind_rows(preg_gab_exposed, preg_gab_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Gabapentin')


# Pregabalin
pre_cohort <- data %>% 
  filter(exposed_pregabalin_mono == 1) %>% 
  distinct(groupID)

preg_pre_exposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_pre_unexposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_pre <- bind_rows(preg_pre_exposed, preg_pre_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Pregabalin')

# Bind together
preg_full <- bind_rows(preg_any, preg_val, preg_top, preg_car, preg_lam, preg_lev, preg_gab, preg_pre)


# 2.2) Congenital Conditions cohort - full --------------------------------

# Read in matched cohort for congenital conditions
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_congenital_cohort.rds"))

# check the outcome format (should be binary 0/1)
table(data$any_CC)

# Total
cc_any_exposed <- data %>% 
  filter(CC_exposed_any_asm == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_any_unexposed <- data %>% 
  filter(CC_exposed_any_asm == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_any <- bind_rows(cc_any_exposed, cc_any_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Any ASM')

# Valproate
val_cohort <- data %>% 
  filter(CC_exposed_valproate_mono == 1) %>% 
  distinct(groupID)

cc_val_exposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & CC_exposed_valproate_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_val_unexposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & CC_exposed_valproate_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_val <- bind_rows(cc_val_exposed, cc_val_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Valproate')


# Topiramate
top_cohort <- data %>% 
  filter(CC_exposed_topiramate_mono == 1) %>% 
  distinct(groupID)

cc_top_exposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & CC_exposed_topiramate_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_top_unexposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & CC_exposed_topiramate_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_top <- bind_rows(cc_top_exposed, cc_top_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Topiramate')


# Carbamazepine
car_cohort <- data %>% 
  filter(CC_exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

cc_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & CC_exposed_carbamazepine_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & CC_exposed_carbamazepine_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_car <- bind_rows(cc_car_exposed, cc_car_unexposed)%>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Carbamazepine')



# Lamotrigine
lam_cohort <- data %>% 
  filter(CC_exposed_lamotrigine_mono == 1) %>% 
  distinct(groupID)

cc_lam_exposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & CC_exposed_lamotrigine_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lam_unexposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & CC_exposed_lamotrigine_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lam <- bind_rows(cc_lam_exposed, cc_lam_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Lamotrigine')

# Levetiracetam
lev_cohort <- data %>% 
  filter(CC_exposed_levetiracetam_mono == 1) %>% 
  distinct(groupID)

cc_lev_exposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & CC_exposed_levetiracetam_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lev_unexposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & CC_exposed_levetiracetam_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lev <- bind_rows(cc_lev_exposed, cc_lev_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Levetiracetam')


# Gabapentin
gab_cohort <- data %>% 
  filter(CC_exposed_gabapentin_mono == 1) %>% 
  distinct(groupID)

cc_gab_exposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & CC_exposed_gabapentin_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_gab_unexposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & CC_exposed_gabapentin_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_gab <- bind_rows(cc_gab_exposed, cc_gab_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Gabapentin')


# Pregabalin
pre_cohort <- data %>% 
  filter(CC_exposed_pregabalin_mono == 1) %>% 
  distinct(groupID)

cc_pre_exposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & CC_exposed_pregabalin_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_pre_unexposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & CC_exposed_pregabalin_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                                     any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_pre <- bind_rows(cc_pre_exposed, cc_pre_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Pregabalin')

# Bind together
cc_full <- bind_rows(cc_any, cc_val, cc_top, cc_car, cc_lam, cc_lev, cc_gab, cc_pre)


# 2.3) Developmental cohort - full --------------------------------------------

# Developmental outcome - full cohort
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_developmental_cohort.rds"))

# check the outcome format (should be binary 0/1)
table(data$any_dev_excl_v_h)

# clogit() expects a 0/1 outcome
# Convert from No/Yes to 0/1
data <- data %>% 
  mutate(any_dev_concern = as.numeric(any_dev_excl_v_h == "Y"))

# Total
dev_any_exposed <- data %>% 
  filter(exposed_any_asm == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_any_unexposed <- data %>% 
  filter(exposed_any_asm == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_any <- bind_rows(dev_any_exposed, dev_any_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Any ASM')

# Valproate
val_cohort <- data %>% 
  filter(exposed_valproate_mono == 1) %>% 
  distinct(groupID)

dev_val_exposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_val_unexposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_val <- bind_rows(dev_val_exposed, dev_val_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Valproate')


# Topiramate
top_cohort <- data %>% 
  filter(exposed_topiramate_mono == 1) %>% 
  distinct(groupID)

dev_top_exposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_top_unexposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_top <- bind_rows(dev_top_exposed, dev_top_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Topiramate')


# Carbamazepine
car_cohort <- data %>% 
  filter(exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

dev_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_car <- bind_rows(dev_car_exposed, dev_car_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Carbamazepine')



# Lamotrigine
lam_cohort <- data %>% 
  filter(exposed_lamotrigine_mono == 1) %>% 
  distinct(groupID)

dev_lam_exposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_lam_unexposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_lam <- bind_rows(dev_lam_exposed, dev_lam_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Lamotrigine')

# Levetiracetam
lev_cohort <- data %>% 
  filter(exposed_levetiracetam_mono == 1) %>% 
  distinct(groupID)

dev_lev_exposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_lev_unexposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_lev <- bind_rows(dev_lev_exposed, dev_lev_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Levetiracetam')


# Gabapentin
gab_cohort <- data %>% 
  filter(exposed_gabapentin_mono == 1) %>% 
  distinct(groupID)

dev_gab_exposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_gab_unexposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_gab <- bind_rows(dev_gab_exposed, dev_gab_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Gabapentin')


# Pregabalin
pre_cohort <- data %>% 
  filter(exposed_pregabalin_mono == 1) %>% 
  distinct(groupID)

dev_pre_exposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_pre_unexposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                     any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_pre <- bind_rows(dev_pre_exposed, dev_pre_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Pregabalin')

# Bind together
dev_full <- bind_rows(dev_any, dev_val, dev_top, dev_car, dev_lam, dev_lev, dev_gab, dev_pre)



# 2.4) Combine full cohorts and save out ----------------------------------

full <- bind_rows(preg_full, cc_full, dev_full)

# 3.1) Pregnancy cohort - epilepsy only --------------------------------------------

# Pregnancy outcome - full cohort
data <- readRDS(paste0(folder_data_path,"matched_cohorts/matched_preg_outcomes_epilepsy.rds") )

# check the outcome format (should be binary 0/1)
table(data$pregnancy_loss)

# Convert from No/Yes to 0/1
data <- data %>% 
  mutate(pregnancy_loss = as.numeric(pregnancy_loss == "Yes"))

# Total
preg_any_exposed <- data %>% 
  filter(exposed_any_asm == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_any_unexposed <- data %>% 
  filter(exposed_any_asm == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_any <- bind_rows(preg_any_exposed, preg_any_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Any ASM')

# Valproate
val_cohort <- data %>% 
  filter(exposed_valproate_mono == 1) %>% 
  distinct(groupID)

preg_val_exposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproatePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_val_unexposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproatePregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_val <- bind_rows(preg_val_exposed, preg_val_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Valproate')


# Topiramate
top_cohort <- data %>% 
  filter(exposed_topiramate_mono == 1) %>% 
  distinct(groupID)

preg_top_exposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramatePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_top_unexposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramatePregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_top <- bind_rows(preg_top_exposed, preg_top_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Topiramate')


# Carbamazepine
car_cohort <- data %>% 
  filter(exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

preg_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepinePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
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
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotriginePregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_lam_unexposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
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
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_lev_unexposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
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
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_gab_unexposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_gab <- bind_rows(preg_gab_exposed, preg_gab_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Gabapentin')


# Pregabalin
pre_cohort <- data %>% 
  filter(exposed_pregabalin_mono == 1) %>% 
  distinct(groupID)

preg_pre_exposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinPregnancyExposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_pre_unexposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinPregnancyUnexposed",
         percent = n/sum(n)*100,
         pregnancy_loss = (case_when(pregnancy_loss == 0 ~ "no_outcome",  
                                     pregnancy_loss == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = pregnancy_loss, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

preg_pre <- bind_rows(preg_pre_exposed, preg_pre_unexposed) %>% 
  mutate(cohort = 'Pregnancy loss',
         ASM = 'Pregabalin')

# Bind together
preg_full_ep <- bind_rows(preg_any, preg_val, preg_top, preg_car, preg_lam, preg_lev, preg_gab, preg_pre)


# 3.2) Congenital Conditions cohort - epilepsy only --------------------------------

# Read in matched cohort for congenital conditions
data <- readRDS(paste0(folder_data_path,"matched_cohorts/matched_congenital_cohort_epilepsy.rds"))

# check the outcome format (should be binary 0/1)
table(data$any_CC)

# Total
cc_any_exposed <- data %>% 
  filter(CC_exposed_any_asm == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_any_unexposed <- data %>% 
  filter(CC_exposed_any_asm == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_any <- bind_rows(cc_any_exposed, cc_any_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Any ASM')

# Valproate
val_cohort <- data %>% 
  filter(CC_exposed_valproate_mono == 1) %>% 
  distinct(groupID)

cc_val_exposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & CC_exposed_valproate_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_val_unexposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & CC_exposed_valproate_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_val <- bind_rows(cc_val_exposed, cc_val_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Valproate')


# Topiramate
top_cohort <- data %>% 
  filter(CC_exposed_topiramate_mono == 1) %>% 
  distinct(groupID)

cc_top_exposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & CC_exposed_topiramate_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_top_unexposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & CC_exposed_topiramate_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_top <- bind_rows(cc_top_exposed, cc_top_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Topiramate')



# Carbamazepine
car_cohort <- data %>% 
  filter(CC_exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

cc_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & CC_exposed_carbamazepine_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & CC_exposed_carbamazepine_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_car <- bind_rows(cc_car_exposed, cc_car_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Carbamazepine')



# Lamotrigine
lam_cohort <- data %>% 
  filter(CC_exposed_lamotrigine_mono == 1) %>% 
  distinct(groupID)

cc_lam_exposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & CC_exposed_lamotrigine_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lam_unexposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & CC_exposed_lamotrigine_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lam <- bind_rows(cc_lam_exposed, cc_lam_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Lamotrigine')

# Levetiracetam
lev_cohort <- data %>% 
  filter(CC_exposed_levetiracetam_mono == 1) %>% 
  distinct(groupID)

cc_lev_exposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & CC_exposed_levetiracetam_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lev_unexposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & CC_exposed_levetiracetam_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lev <- bind_rows(cc_lev_exposed, cc_lev_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Levetiracetam')


# Gabapentin
gab_cohort <- data %>% 
  filter(CC_exposed_gabapentin_mono == 1) %>% 
  distinct(groupID)

cc_gab_exposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & CC_exposed_gabapentin_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) #%>% 
  #mutate(total = n_outcome + n_no_outcome)

cc_gab_unexposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & CC_exposed_gabapentin_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_gab <- bind_rows(cc_gab_exposed, cc_gab_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Gabapentin')


# Pregabalin
pre_cohort <- data %>% 
  filter(CC_exposed_pregabalin_mono == 1) %>% 
  distinct(groupID)

cc_pre_exposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & CC_exposed_pregabalin_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_pre_unexposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & CC_exposed_pregabalin_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_pre <- bind_rows(cc_pre_exposed, cc_pre_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Pregabalin')

# Bind together
cc_full_ep <- bind_rows(cc_any, cc_val, cc_top, cc_car, cc_lam, cc_lev, cc_gab, cc_pre)


# 3.3) Developmental cohort - epilepsy only --------------------------------------------

# Developmental outcome - full cohort
data <- readRDS(paste0(folder_data_path,"matched_cohorts/matched_developmental_cohort_epilepsy.rds") )

# check the outcome format (should be binary 0/1)
table(data$any_dev_excl_v_h)

# clogit() expects a 0/1 outcome
# Convert from No/Yes to 0/1
data <- data %>% 
  mutate(any_dev_concern = as.numeric(any_dev_excl_v_h == "Y"))

# Total
dev_any_exposed <- data %>% 
  filter(exposed_any_asm == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_any_unexposed <- data %>% 
  filter(exposed_any_asm == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_any <- bind_rows(dev_any_exposed, dev_any_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Any ASM')

# Valproate
val_cohort <- data %>% 
  filter(exposed_valproate_mono == 1) %>% 
  distinct(groupID)

dev_val_exposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_val_unexposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_val <- bind_rows(dev_val_exposed, dev_val_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Valproate')


# Topiramate
top_cohort <- data %>% 
  filter(exposed_topiramate_mono == 1) %>% 
  distinct(groupID)

dev_top_exposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_top_unexposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_top <- bind_rows(dev_top_exposed, dev_top_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Topiramate')


# Carbamazepine
car_cohort <- data %>% 
  filter(exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

dev_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_car <- bind_rows(dev_car_exposed, dev_car_unexposed)%>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Carbamazepine')



# Lamotrigine
lam_cohort <- data %>% 
  filter(exposed_lamotrigine_mono == 1) %>% 
  distinct(groupID)

dev_lam_exposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_lam_unexposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_lam <- bind_rows(dev_lam_exposed, dev_lam_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Lamotrigine')

# Levetiracetam
lev_cohort <- data %>% 
  filter(exposed_levetiracetam_mono == 1) %>% 
  distinct(groupID)

dev_lev_exposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_lev_unexposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_lev <- bind_rows(dev_lev_exposed, dev_lev_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Levetiracetam')


# Gabapentin
gab_cohort <- data %>% 
  filter(exposed_gabapentin_mono == 1) %>% 
  distinct(groupID)

dev_gab_exposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_gab_unexposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_gab <- bind_rows(dev_gab_exposed, dev_gab_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Gabapentin')


# Pregabalin
pre_cohort <- data %>% 
  filter(exposed_pregabalin_mono == 1) %>% 
  distinct(groupID)

dev_pre_exposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 1) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinDevelopmentalExposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_pre_unexposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 0) %>% 
  group_by(any_dev_concern) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinDevelopmentalUnexposed",
         percent = n/sum(n)*100,
         any_dev_concern = (case_when(any_dev_concern == 0 ~ "no_outcome",  
                                      any_dev_concern == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_dev_concern, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

dev_pre <- bind_rows(dev_pre_exposed, dev_pre_unexposed) %>% 
  mutate(cohort = 'Early childhood developmental concerns',
         ASM = 'Pregabalin')

# Bind together
dev_full_ep <- bind_rows(dev_any, dev_val, dev_top, dev_car, dev_lam, dev_lev, dev_gab, dev_pre)



# 3.4) Combine full cohorts and save out ----------------------------------

epilepsy <- bind_rows(preg_full_ep, cc_full_ep, dev_full_ep)


# 4.1) Congenital Conditions cohort - 12 weeks gestation only --------------------------------

# Read in matched cohort for congenital conditions
data <- readRDS(paste0(folder_data_path,"matched_cohorts/matched_congenital_cohort_12weeks.rds"))

# check the outcome format (should be binary 0/1)
table(data$any_CC)

# Total
cc_any_exposed <- data %>% 
  filter(CC_exposed_any_asm == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_any_unexposed <- data %>% 
  filter(CC_exposed_any_asm == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TotalCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_any <- bind_rows(cc_any_exposed, cc_any_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Any ASM')

# Valproate
val_cohort <- data %>% 
  filter(CC_exposed_valproate_mono == 1) %>% 
  distinct(groupID)

cc_val_exposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & CC_exposed_valproate_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_val_unexposed <- data %>% 
  filter(groupID %in% val_cohort$groupID & CC_exposed_valproate_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "ValproateCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_val <- bind_rows(cc_val_exposed, cc_val_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Valproate')


# Topiramate
top_cohort <- data %>% 
  filter(CC_exposed_topiramate_mono == 1) %>% 
  distinct(groupID)

cc_top_exposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & CC_exposed_topiramate_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_top_unexposed <- data %>% 
  filter(groupID %in% top_cohort$groupID & CC_exposed_topiramate_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "TopiramateCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_top <- bind_rows(cc_top_exposed, cc_top_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Topiramate')


# Carbamazepine
car_cohort <- data %>% 
  filter(CC_exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

cc_car_exposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & CC_exposed_carbamazepine_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_car_unexposed <- data %>% 
  filter(groupID %in% car_cohort$groupID & CC_exposed_carbamazepine_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "CarbamazepineCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_car <- bind_rows(cc_car_exposed, cc_car_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Carbamazepine')



# Lamotrigine
lam_cohort <- data %>% 
  filter(CC_exposed_lamotrigine_mono == 1) %>% 
  distinct(groupID)

cc_lam_exposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & CC_exposed_lamotrigine_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lam_unexposed <- data %>% 
  filter(groupID %in% lam_cohort$groupID & CC_exposed_lamotrigine_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LamotrigineCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lam <- bind_rows(cc_lam_exposed, cc_lam_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Lamotrigine')

# Levetiracetam
lev_cohort <- data %>% 
  filter(CC_exposed_levetiracetam_mono == 1) %>% 
  distinct(groupID)

cc_lev_exposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & CC_exposed_levetiracetam_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lev_unexposed <- data %>% 
  filter(groupID %in% lev_cohort$groupID & CC_exposed_levetiracetam_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "LevetiracetamCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_lev <- bind_rows(cc_lev_exposed, cc_lev_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Levetiracetam')


# Gabapentin
gab_cohort <- data %>% 
  filter(CC_exposed_gabapentin_mono == 1) %>% 
  distinct(groupID)

cc_gab_exposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & CC_exposed_gabapentin_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_gab_unexposed <- data %>% 
  filter(groupID %in% gab_cohort$groupID & CC_exposed_gabapentin_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "GabapentinCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_gab <- bind_rows(cc_gab_exposed, cc_gab_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Gabapentin')


# Pregabalin
pre_cohort <- data %>% 
  filter(CC_exposed_pregabalin_mono == 1) %>% 
  distinct(groupID)

cc_pre_exposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & CC_exposed_pregabalin_mono == 1) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinCongenitalExposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_pre_unexposed <- data %>% 
  filter(groupID %in% pre_cohort$groupID & CC_exposed_pregabalin_mono == 0) %>% 
  group_by(any_CC) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "PregabalinCongenitalUnexposed",
         percent = n/sum(n)*100,
         any_CC = (case_when(any_CC == 0 ~ "no_outcome",  
                             any_CC == 1 ~ "outcome"))) %>%
  pivot_wider(names_from = any_CC, values_from = c(n, percent)) %>% 
  mutate(total = n_outcome + n_no_outcome)

cc_pre <- bind_rows(cc_pre_exposed, cc_pre_unexposed) %>% 
  mutate(cohort = 'Congenital conditions',
         ASM = 'Pregabalin')

# Bind together
cc_full_12wks <- bind_rows(cc_any, cc_val, cc_top, cc_car, cc_lam, cc_lev, cc_gab, cc_pre)


full <- bind_rows(preg_full, cc_full, dev_full) %>% 
  mutate(group = 'main')
full_ep <- bind_rows(preg_full_ep, cc_full_ep, dev_full_ep) %>% 
  mutate(group = 'epilepsy')
full_12 <- cc_full_12wks %>% 
  mutate(group = '12wks')

full <- bind_rows(full, full_ep, full_12)

write_csv(full, paste0(folder_data_path,"stats/n_exp_unexp.csv"))
