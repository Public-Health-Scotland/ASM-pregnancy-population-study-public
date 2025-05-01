##01d.cc_cohort_descriptives_by_asm.r
# Script to create descriptives table of the congenital condition cohort
# for each asm exposure


#### 1. Housekeeping ####

# Approximate run time - TBD

rm(list = ls())
gc()

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

source("04-descriptives_results/00.descriptives_setup.r")

#### 2. Read in main data file ####
data <- readRDS(paste0(folder_data_path,"linkage/master_dataset_file.rds")) %>%
  filter(control_pool_CC == 1 | cases_CC == 1) %>% # restrict to only those eligible for inclusion in the congential conditions cohort
  mutate(exposed_drug = (case_when(CC_exposed_valproate_mono == 1 ~ "valproate",  
                                   CC_exposed_topiramate_mono == 1 ~ "topiramate",
                                   CC_exposed_carbamazepine_mono == 1 ~ "carbamazepine",
                                   CC_exposed_lamotrigine_mono == 1 ~ "lamotrigine",
                                   CC_exposed_levetiracetam_mono == 1 ~ "levetiracetam",   
                                   CC_exposed_pregabalin_mono == 1 ~ "pregabalin", 
                                   CC_exposed_gabapentin_mono == 1 ~ "gabapentin", 
                                   T ~ NA)))
total_babies <- data %>% nrow() 

###add parity
cohort <-readRDS(paste0(folder_data_path,"SLiPBD_cohort_extract.rds"))
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+", T~"Unknown"))

#### 3. Totals ####
t1d <- data %>% filter(CC_exposed_any_asm == 1) %>% summarise(n=n()) %>% 
  mutate(indicator = "Total",
         exposed_drug = "any_asm",
         percent = n/total_babies*100) %>% select(exposed_drug, indicator, n, percent) 


t_df <- data %>% 
  filter(CC_exposed_valproate_mono == 1 | CC_exposed_topiramate_mono == 1 | 
           CC_exposed_carbamazepine_mono ==1 | CC_exposed_lamotrigine_mono ==1 | 
           CC_exposed_levetiracetam_mono==1 | 
           CC_exposed_pregabalin_mono == 1 | 
           CC_exposed_gabapentin_mono == 1) %>% 
  group_by(exposed_drug) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total",
         percent = n/total_babies*100) 

t1d <- bind_rows(t1d, t_df) %>% pivot_wider(names_from = exposed_drug, values_from = c(n, percent)) %>%
  rename("valproate" = "n_valproate",
         "topiramate" = "n_topiramate",
         "carbamazepine" = "n_carbamazepine",
         "lamotrigine" = "n_lamotrigine",
         "levetiracetam" = "n_levetiracetam",
         "pregabalin" = "n_pregabalin",
         "gabapentin" = "n_gabapentin",
         "any_asm" = "n_any_asm") %>% mutate(sub_indicator = NA) %>%
  select(indicator, sub_indicator, 
         valproate, percent_valproate, 
         topiramate, percent_topiramate, 
         carbamazepine, percent_carbamazepine,
         lamotrigine, percent_lamotrigine,
         levetiracetam, percent_levetiracetam,
         pregabalin, percent_pregabalin,
         gabapentin, percent_gabapentin,
         any_asm, percent_any_asm) 

#### 4. Indication for ASM ####
aggregate_ind <- function(data, ind, indicator_name) {
  df <- data %>% filter(CC_exposed_valproate_mono == 1 | CC_exposed_topiramate_mono == 1 |
                          CC_exposed_carbamazepine_mono ==1 | CC_exposed_lamotrigine_mono == 1|
                          CC_exposed_levetiracetam_mono ==1 |
                          CC_exposed_pregabalin_mono == 1 | 
                          CC_exposed_gabapentin_mono == 1) %>% 
    group_by(exposed_drug, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}} == 1 ~ "yes"))) 
  
  df <- bind_rows(df, 
                  (data %>% filter(CC_exposed_any_asm == 1) %>% 
                     group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
                     mutate(indicator = indicator_name,
                            exposed_drug = "any_asm",
                            sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                                       {{ind}} == 1 ~ "yes"))))) %>%
    
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(percent_valproate = valproate/sum(valproate)*100,
           percent_topiramate = topiramate/sum(topiramate)*100,
           percent_carbamazepine = carbamazepine/sum(carbamazepine)*100,
           percent_lamotrigine = lamotrigine/sum(lamotrigine)*100,
           percent_levetiracetam = levetiracetam/sum(levetiracetam)*100,
           percent_pregabalin = pregabalin/sum(pregabalin)*100,
           percent_gabapentin = gabapentin/sum(gabapentin)*100,
           percent_any_asm = any_asm/sum(any_asm)*100)
}

t_df <- data %>% aggregate_ind(epilepsy_indication, "maternal epilepsy")

t1d <- bind_rows(t1d, t_df) %>% select(-epilepsy_indication)

t_df <- data %>% aggregate_ind(mh_flag, "maternal mental health conditions") %>% select(-mh_flag)
t1d <- bind_rows(t1d, t_df) 

t_df <- data %>% aggregate_ind(migraine_pain_flag, "maternal migraine or pain conditions") %>% select(-migraine_pain_flag)
t1d <- bind_rows(t1d, t_df) 

t_df <- data %>% mutate(any_indication = case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1, T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm") %>% select(-any_indication)
t1d <- bind_rows(t1d, t_df) 


#### 5. year of conception ####
aggregate_data <- function(ind, indicator_name) {
  df <- data %>% filter(CC_exposed_valproate_mono == 1 | CC_exposed_topiramate_mono == 1 |
                          CC_exposed_carbamazepine_mono ==1 | CC_exposed_lamotrigine_mono == 1|
                          CC_exposed_levetiracetam_mono ==1 |
                          CC_exposed_pregabalin_mono == 1 | 
                          CC_exposed_gabapentin_mono == 1) %>% 
    group_by(exposed_drug, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           n = ifelse(is.na(n), 0, n))
  
  df <- bind_rows(df, 
                  (data %>% filter(CC_exposed_any_asm == 1) %>% 
                     group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
                     mutate(indicator = indicator_name,
                            exposed_drug = "any_asm"))) %>%
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(valproate = ifelse(is.na(valproate), 0, valproate),
           topiramate = ifelse(is.na(topiramate), 0, topiramate),
           carbamazepine = ifelse(is.na(carbamazepine), 0, carbamazepine),
           lamotrigine = ifelse(is.na(lamotrigine), 0, lamotrigine),
           levetiracetam = ifelse(is.na(levetiracetam), 0, levetiracetam),
           pregabalin = ifelse(is.na(pregabalin), 0, pregabalin),
           gabapentin = ifelse(is.na(gabapentin), 0, gabapentin),
           any_asm = ifelse(is.na(any_asm), 0, any_asm)) %>%
    mutate(percent_valproate = valproate/sum(valproate)*100,
           percent_topiramate = topiramate/sum(topiramate)*100,
           percent_carbamazepine = carbamazepine/sum(carbamazepine)*100,
           percent_lamotrigine = lamotrigine/sum(lamotrigine)*100,
           percent_levetiracetam = levetiracetam/sum(levetiracetam)*100,
           percent_pregabalin = pregabalin/sum(pregabalin)*100,
           percent_gabapentin = gabapentin/sum(gabapentin)*100,
           percent_any_asm = any_asm/sum(any_asm)*100) %>% 
    rename("sub_indicator" = 1)
}

t_df <- aggregate_data(year_conception, "conception year") %>% mutate(sub_indicator = as.character(sub_indicator))
t1d <- bind_rows(t1d, t_df) 


#### 6. Maternal age ####
t_df <- aggregate_data(maternal_age_group_conception, "maternal age at conception")

t1d <- bind_rows(t1d, t_df) 

#### 7. maternal deprivation ####
t_df <- aggregate_data(mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1d <- bind_rows(t1d, t_df) 

#### 8. baby sex ####
t_df <- aggregate_data(baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1d <- bind_rows(t1d, t_df) 

#### 9. maternal BMI at booking ####
t_df <- aggregate_data(maternal_bmi_group, "maternal BMI at booking")
t1d <- bind_rows(t1d, t_df) 

#### 10. Maternal smoking at booking ####
t_df <- aggregate_data(maternal_smoking, "maternal smoking at booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1d <- bind_rows(t1d, t_df) 

#### 11. Maternal high dose folic acid ####
t_df <- aggregate_ind(data, high_dose_folic_acid, "maternal high dose folic acid") %>% select(-high_dose_folic_acid)
t1d <- bind_rows(t1d, t_df) 

#### 12. Maternal healthboard ####
Health_Board_Lookup <- read_csv(paste0(folder_data_path,"Health Board Area all years Lookup.csv"))
table(data$maternal_nhs_board_res_booking)


data <- data %>% left_join(Health_Board_Lookup, by= c("maternal_nhs_board_res_booking" ="HealthboardCode")) %>% 
  select(-year) 
data <- data %>%rename(maternal_nhs_board_res_booking_name = NRSHealthBoardAreaName)
data <- data %>% 
  mutate(mother_nhs_board =case_when(is.na(maternal_nhs_board_res_booking_name)~
                                       maternal_nhs_board_res_name_end_preg, T~ maternal_nhs_board_res_booking_name))

t_df <-  aggregate_data(mother_nhs_board, "mother_nhs_board") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))

t1d <- bind_rows(t1d, t_df)

#### drug alcohol use ####
t_df <- aggregate_ind(data, drug_alcohol_use, "Maternal drug or alcohol use") %>% select(-drug_alcohol_use)
t1d <- bind_rows(t1d, t_df)

#### SMR comorbidities####
t_df <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity") %>% select(-any_smr_comorb)
t1d <- bind_rows(t1d, t_df)

#### parity ####

t_df <- aggregate_data(prev_pregs_group, "Number of previous deliveries") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1d <- bind_rows(t1d, t_df)
#### 12. Outcome ####

# Any
# Nervous system conditions
# Eye conditions
# Ear, face, and neck conditions
# Congenital heart conditions
# Respiratory conditions
# Oro-facial clefts
# Gastro-intestinal conditions
# Abdominal wall defects
# Kidney and urinary tract conditions
# Genital conditions
# Limb conditions
# Other conditions/syndromes

# all_1_nervous_system
# all_2_eye
# all_3_ear_face_and_neck
# all_4_congenital_heart_defects
# all_5_respiratory
# all_6_oro_facial_clefts
# all_7_gastro_intestinal
# all_8_abdominal_wall_defects
# all_9_kidney_and_urinary_tract
# all_10_genital
# all_11_limb
# all_12_other_conditions
# any_CC

nerv_sys <- aggregate_ind(data, all_1_nervous_system, "outcome - nervous system conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_1_nervous_system))
eye <- aggregate_ind(data, all_2_eye, "outcome - eye conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_2_eye))
ear_face_neck <- aggregate_ind(data, all_3_ear_face_and_neck, "outcome - ear, face, and neck conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_3_ear_face_and_neck))
con_heart <- aggregate_ind(data, all_4_congenital_heart_defects, "outcome - congenital heart conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_4_congenital_heart_defects))
resp <- aggregate_ind(data, all_5_respiratory, "outcome - respiratory conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_5_respiratory))
oro_face_clefts <- aggregate_ind(data, all_6_oro_facial_clefts, "outcome - oro-facial clefts") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_6_oro_facial_clefts))
gast_intest <- aggregate_ind(data, all_7_gastro_intestinal, "outcome - gastro-intestinal conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_7_gastro_intestinal))
abd_wall <- aggregate_ind(data, all_8_abdominal_wall_defects, "outcome - abdominal wall defects") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_8_abdominal_wall_defects))
kid_urin_tract <- aggregate_ind(data, all_9_kidney_and_urinary_tract, "outcome - kidney and urinary tract conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_9_kidney_and_urinary_tract))
genital <- aggregate_ind(data, all_10_genital, "outcome - genital conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_10_genital))
limb <- aggregate_ind(data, all_11_limb, "outcome - limb conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_11_limb))
other <- aggregate_ind(data, all_12_other_conditions, "outcome - other conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_12_other_conditions))
any <- aggregate_ind(data, any_CC, "outcome - any conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, any_CC))
none <- aggregate_ind(data, any_CC, "outcome - none") %>% 
  filter(sub_indicator == 'no') %>% 
  select(-c(sub_indicator, any_CC))

outcomes <- bind_rows(nerv_sys, eye, ear_face_neck, con_heart, resp, oro_face_clefts, gast_intest, abd_wall, kid_urin_tract, genital,
                      limb, other, any, none)



# 13) Combine data --------------------------------------------------------

t1d <- bind_rows(t1d, outcomes)

write_csv(t1d, paste0(folder_data_path,"Descriptives/table_1d.csv"))
