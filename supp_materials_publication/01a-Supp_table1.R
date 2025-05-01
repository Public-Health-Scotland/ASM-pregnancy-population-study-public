# Name of file - T1b_descriptives.R ####
# SUpplementary table 2 for publication (a version of T1b in the internal spreadsheet)
##CHaracteristics of pregnancies in Cohrt 1, monotherapy
##remove healthboard and year
##include matched controls

#### 1. Housekeeping ####

# Approximate run time - TBD

rm(list = ls())
gc()

library(hablar)
library(dplyr)
library(tidyverse)
library(haven)
library(labelled)
library(lubridate)
library(janitor)
library(odbc)
library(phsmethods)
library(readxl)
library(arrow)
library(collapse)
source("supp_materials_publication/00.supp_setup.r")

###functions ####
#function for aggregating indications columns
aggregate_ind <- function(df, ind, indicator_name) {
  #indicator_name <- "epilepsy"
  t_df <- data %>% filter(!is.na(monotherapy_cohort) ) %>% 
    group_by(monotherapy_cohort, exposed_unexposed,{{ind}}) %>%
    summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}}== 1 ~ "yes"))) %>%
    pivot_wider(names_from = exposed_unexposed, values_from = c(n)) %>%
    group_by(monotherapy_cohort) %>%
    mutate(percent_exposed =exposed/sum(exposed)*100,
           percent_unexposed = matched_unexposed/sum(matched_unexposed)*100) %>%
    ungroup()# %>% 
  #  pivot_wider(names_from = monotherapy_cohort, 
   #             values_from = c(exposed, percent_exposed,matched_unexposed, percent_unexposed)) 
}

aggregate_data <- function(ind, indicator_name) {
  
  df <- data %>% filter(!is.na(monotherapy_cohort) ) %>% 
    group_by(monotherapy_cohort, exposed_unexposed,{{ind}})  %>% 
    summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           n = ifelse(is.na(n), 0, n)) %>% 
      pivot_wider(names_from = exposed_unexposed, values_from = c(n)) %>%
    group_by(monotherapy_cohort) %>%
    mutate(exposed =ifelse(is.na(exposed), 0, exposed), 
           matched_unexposed =ifelse(is.na(matched_unexposed), 0, matched_unexposed)) %>%
    mutate(percent_exposed =exposed/sum(exposed)*100,
           percent_unexposed = matched_unexposed/sum(matched_unexposed)*100) %>%
    ungroup() %>% 
  #  pivot_wider(names_from = monotherapy_cohort, 
  #              values_from = c(exposed, percent_exposed,matched_unexposed, percent_unexposed)) %>%
    rename("sub_indicator" = 2)
}

####


#### 2. Read in main data file ####
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_preg_outcomes_cohort.rds"))
data <- data %>% mutate(exposed_drug = (case_when(exposed_valproate_mono == 1 ~ "valproate",  
                                   exposed_topiramate_mono == 1 ~ "topiramate",
                                   exposed_carbamazepine_mono == 1 ~ "carbamazepine",
                                   exposed_lamotrigine_mono == 1 ~ "lamotrigine",
                                   exposed_levetiracetam_mono == 1 ~ "levetiracetam",
                                   exposed_gabapentin_mono == 1 ~ "gabapentin",
                                   exposed_pregabalin_mono == 1 ~ "pregabalin",
                                   T ~ NA))) %>%
  group_by(groupID) %>%
  mutate(monotherapy_cohort = max_(exposed_drug)) %>% ungroup() %>%
  mutate(exposed_unexposed = case_when(exposed_any_asm==1 ~"exposed", 
                                       exposed_any_asm==0 ~ "matched_unexposed"))


###add parity
cohort <-readRDS(paste0(folder_data_path,"data/SLiPBD_cohort_extract.rds"))
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+", T~"Unknown"))

#### 3. Totals ####
t1 <- data %>% filter(!is.na(monotherapy_cohort)) %>%
  group_by(monotherapy_cohort, exposed_unexposed) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total") 

t1 <- t1 %>% pivot_wider(names_from = c( exposed_unexposed), values_from = c(n)) %>%
 mutate(sub_indicator = NA) 


#### 4. Indication for ASM ####

t_df <- data %>% aggregate_ind(epilepsy_indication, "maternal epilepsy")

t1 <- bind_rows(t1, t_df) %>% select(-epilepsy_indication)

t_df <- data %>% aggregate_ind(mh_flag, "maternal mental health conditions") %>% select(-mh_flag)
t1 <- bind_rows(t1, t_df) 

t_df <- data %>% aggregate_ind(migraine_pain_flag, "maternal migraine or pain conditions") %>% select(-migraine_pain_flag)
t1 <- bind_rows(t1, t_df) 


data <- data %>% 
  mutate(any_indication = case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1, T ~ 0))

t_df <- data %>% aggregate_ind(any_indication, "any condition indicating asm") %>% select(-any_indication)
t1 <- bind_rows(t1, t_df) 


#### 6. Maternal age ####
t_df <- aggregate_data(maternal_age_group_conception, "maternal age at conception")

t1 <- bind_rows(t1, t_df) 

#### 7. maternal deprivation ####
t_df <- aggregate_data(mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))

t1 <- bind_rows(t1, t_df) 

#### 8. baby sex ####
t_df <- aggregate_data(baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1 <- bind_rows(t1, t_df) 

#### 9. maternal BMI at booking ####
t_df <- aggregate_data(maternal_bmi_group, "maternal BMI at antenatal booking")
t1 <- bind_rows(t1, t_df) 

#### 10. Maternal smoking at booking ####
t_df <- aggregate_data(maternal_smoking, "maternal smoking at antenatal booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1 <- bind_rows(t1, t_df) 

#### 11. Maternal high dose folic acid ####
t_df <- aggregate_ind(data, high_dose_folic_acid, "maternal high dose folic acid") %>% select(-high_dose_folic_acid)
t1 <- bind_rows(t1, t_df) 


#### drug alcohol use ####
t_df <- aggregate_ind(data, drug_alcohol_use, "Maternal drug or alcohol use") %>% select(-drug_alcohol_use)
t1 <- bind_rows(t1, t_df)

#### SMR comorbidities####
t_df <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity") %>% select(-any_smr_comorb)
t1 <- bind_rows(t1, t_df)

#### parity ####

t_df <- aggregate_data(prev_pregs_group, "Number of previous deliveries") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1 <- bind_rows(t1, t_df) 



t1 <- t1 %>% select(monotherapy_cohort, indicator, sub_indicator,
                      exposed, percent_exposed, 
                      matched_unexposed,percent_unexposed
                    
                      )

totals <- t1 %>% 
  select(c(monotherapy_cohort, indicator, sub_indicator, exposed)) %>% 
  rename(n = exposed) %>% 
  pivot_wider(names_from = c(monotherapy_cohort),
              names_glue = "{monotherapy_cohort}_{.value}",
              values_from = c(n)) %>% 
  filter(indicator == 'Total') %>% 
  select(c(indicator, sub_indicator, valproate_n, topiramate_n, carbamazepine_n, lamotrigine_n, levetiracetam_n, gabapentin_n, pregabalin_n))

t1_2 <- t1 %>% 
  select(-c(matched_unexposed, percent_unexposed)) %>% 
  rename(n = exposed,
         percent = percent_exposed) %>% 
  pivot_wider(names_from = c(monotherapy_cohort),
              names_glue = "{monotherapy_cohort}_{.value}",
              values_from = c(n, percent)) %>% 
  filter(indicator != 'Total') %>% 
  mutate(flag_remove = case_when(sub_indicator == 'no' ~ 1,
                                 T~0)) %>% 
  filter(flag_remove == 0) %>% 
  select(-c(flag_remove)) %>% 
  mutate(sub_indicator = case_when(indicator == 'any condition indicating asm' ~ 'Any condition',
                                   indicator == 'maternal epilepsy' ~ 'Maternal epilepsy',
                                   indicator == 'maternal mental health conditions' ~ 'Maternal mental health conditions',
                                   indicator == 'maternal migraine or pain conditions' ~ 'Maternal migraine or pain conditions',
                                   T~sub_indicator)) %>%
  mutate(indicator = case_when(indicator %in% c("any condition indicating asm", "maternal epilepsy",
                                                "maternal mental health conditions", "maternal migraine or pain conditions") ~ 'Condition indicating ASM use',
                               T~indicator)) %>%
  mutate(sub_indicator = case_when(sub_indicator == 'yes' ~ ' ',
                                   sub_indicator == '40+' ~ '≥40',
                                   sub_indicator == '1 (most deprived)' ~ 'SIMD Q1 (most deprived)',
                                   sub_indicator == '2' ~ 'SIMD Q2',
                                   sub_indicator == '3' ~ 'SIMD Q3',
                                   sub_indicator == '4' ~ 'SIMD Q3',
                                   sub_indicator == '5 (least deprived)' ~ 'SIMD Q5 (least deprived)',
                                   sub_indicator == 'F' ~ 'Female',
                                   sub_indicator == 'M' ~ 'Male',
                                   sub_indicator == 'Healthy weight' ~ 'Healthy weight (18.5-<25)',
                                   sub_indicator == 'Obese' ~ 'Obese (≥30)',
                                   sub_indicator == 'Overweight' ~ 'Overweight (25-<30)',
                                   sub_indicator == 'Underweight' ~ 'Underweight (<18.5)',
                                   sub_indicator == '1+' ~ '≥1',
                                   sub_indicator == 'Ex-smoker' ~ 'Former smoker',
                                   sub_indicator == 'Non-smoker' ~ 'Never smoked',
                                   sub_indicator == 'Smoker' ~ 'Current smoker',
                                   T~sub_indicator
  )) %>% 
  select(c(indicator, sub_indicator, valproate_n, valproate_percent, topiramate_n, topiramate_percent, 
           carbamazepine_n, carbamazepine_percent, lamotrigine_n, lamotrigine_percent, 
           levetiracetam_n, levetiracetam_percent, gabapentin_n, gabapentin_percent, 
           pregabalin_n, pregabalin_percent))

t1_2 <- bind_rows(totals, t1_2) %>% 
  select(c(indicator, sub_indicator, valproate_n, valproate_percent, topiramate_n, topiramate_percent, 
           carbamazepine_n, carbamazepine_percent, lamotrigine_n, lamotrigine_percent, 
           levetiracetam_n, levetiracetam_percent, gabapentin_n, gabapentin_percent, 
           pregabalin_n, pregabalin_percent))


# Re-order 
group1 <- t1_2 %>% 
  slice(c(1))
asm_condition <- t1_2 %>% 
  slice(c(2:5))
m_age <- t1_2 %>% 
  slice(c(6:12))
group2 <- t1_2 %>% 
  slice(c(13:18))
baby_sex <- t1_2 %>% 
  slice(19:21)
bmi <- t1_2 %>% 
  slice(22:26)
m_smoke <- t1_2 %>% 
  slice(27:30)
group3 <- t1_2 %>% 
  slice(31:36)

asm_condition <- asm_condition %>% 
  slice(c(4, 1, 2, 3))
m_age <- m_age %>% 
  slice(c(6, 1, 2, 3, 4, 5, 7))
baby_sex <- baby_sex %>% 
  slice(c(2, 1, 3))
bmi <- bmi %>% 
  slice(c(4, 1, 3, 2, 5))
m_smoke <- m_smoke %>% 
  slice(c(3, 1, 2, 4))

t1_2 <- bind_rows(group1, asm_condition, m_age, group2, baby_sex, bmi, m_smoke, group3)


write.csv(t1_2, paste0(folder_data_path,"supplementary_materials/supp_table1.csv"))


