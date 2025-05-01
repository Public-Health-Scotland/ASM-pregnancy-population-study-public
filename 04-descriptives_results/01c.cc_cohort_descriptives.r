##01c.cc_cohort_descriptives.r
# Script to create descriptives table of the congenital conditions cohort
# for any asm exposure


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
  filter(control_pool_CC == 1 | cases_CC == 1) # restrict to only those eligible for inclusion in the congential conditions cohort


###add parity
cohort <-readRDS(paste0(folder_data_path,"SLiPBD_cohort_extract.rds"))
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+", T~"Unknown"))


#### 3. Indication for ASM ####
totals <- data %>% group_by(CC_exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total",
         percent = n/sum(n)*100,
         CC_exposed_any_asm = (case_when(CC_exposed_any_asm == 0 ~ "unexposed",  
                                         CC_exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(n, percent))


#function for aggregating indications columns
aggregate_ind <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(CC_exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           CC_exposed_any_asm = (case_when(CC_exposed_any_asm == 0 ~ "n_unexposed",  
                                           CC_exposed_any_asm == 1 ~ "n_exposed")),
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}} == 1 ~ "yes"))) %>%
    pivot_wider(names_from = CC_exposed_any_asm, values_from = c(n)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
    select(-1)
  
}

epilepsy <- aggregate_ind(data, epilepsy_indication, "maternal epilepsy")
mental_health <- aggregate_ind(data, mh_flag, "maternal mental health conditions")
pain <- aggregate_ind(data, migraine_pain_flag, "maternal migraine or pain conditions")

any_indication <- data %>% mutate(any_indication = case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1, T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm")


#### 4. year of conception ####
# function
aggregate_data <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(CC_exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           CC_exposed_any_asm = (case_when(CC_exposed_any_asm == 0 ~ "n_unexposed",  
                                           CC_exposed_any_asm == 1 ~ "n_exposed"))) %>%
    pivot_wider(names_from = CC_exposed_any_asm, values_from = c(n)) %>%
    mutate(n_unexposed = case_when(is.na(n_unexposed) ~ 0, T ~ n_unexposed),
           n_exposed = case_when(is.na(n_exposed) ~ 0, T ~ n_exposed)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>% 
    rename("sub_indicator" = 1)
}

conception_year <- aggregate_data(data, year_conception, "conception year")

#### 5. Maternal age ####
age <- aggregate_data(data, maternal_age_group_conception, "maternal age at conception")


#### 6. maternal deprivation ####
simd <- aggregate_data(data, mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 7. baby sex ####
baby_sex <- aggregate_data(data, baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 8. maternal BMI at booking ####
bmi <- aggregate_data(data, maternal_bmi_group, "maternal BMI at booking")


#### 9. Maternal smoking at booking ####
smoke <- aggregate_data(data, maternal_smoking, "maternal smoking at booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 10. Maternal high dose folic acid ####
folic <- aggregate_data(data, high_dose_folic_acid, "maternal high dose folic acid") 

#### 11. Maternal drug or alcohol exposure###
drug_alcohol <- aggregate_data(data, drug_alcohol_use, "maternal drug or alcohol use") 

#### 12. Maternal healthboard ####
Health_Board_Lookup <- read_csv(paste0(folder_data_path,"Health Board Area all years Lookup.csv"))
table(data$maternal_nhs_board_res_booking)


data <- data %>% left_join(Health_Board_Lookup, by= c("maternal_nhs_board_res_booking" ="HealthboardCode")) %>% 
  select(-year) 
data <- data %>%rename(maternal_nhs_board_res_booking_name = NRSHealthBoardAreaName)
data <- data %>% 
  mutate(mother_nhs_board =case_when(is.na(maternal_nhs_board_res_booking_name)~
                                       maternal_nhs_board_res_name_end_preg, T~ maternal_nhs_board_res_booking_name))

healthboard <-  aggregate_data(data, mother_nhs_board, "mother_nhs_board") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))

#### SMR comorbidities####

comorbs <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity flag") 

#### parity###
parity <- aggregate_data(data, prev_pregs_group, "Number of previous deliveries") 

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
  select(-c(sub_indicator))
eye <- aggregate_ind(data, all_2_eye, "outcome - eye conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
ear_face_neck <- aggregate_ind(data, all_3_ear_face_and_neck, "outcome - ear, face, and neck conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
con_heart <- aggregate_ind(data, all_4_congenital_heart_defects, "outcome - congenital heart conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
resp <- aggregate_ind(data, all_5_respiratory, "outcome - respiratory conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
oro_face_clefts <- aggregate_ind(data, all_6_oro_facial_clefts, "outcome - oro-facial clefts") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
gast_intest <- aggregate_ind(data, all_7_gastro_intestinal, "outcome - gastro-intestinal conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
abd_wall <- aggregate_ind(data, all_8_abdominal_wall_defects, "outcome - abdominal wall defects") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
kid_urin_tract <- aggregate_ind(data, all_9_kidney_and_urinary_tract, "outcome - kidney and urinary tract conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
genital <- aggregate_ind(data, all_10_genital, "outcome - genital conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
limb <- aggregate_ind(data, all_11_limb, "outcome - limb conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
other <- aggregate_ind(data, all_12_other_conditions, "outcome - other conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
any <- aggregate_ind(data, any_CC, "outcome - any conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
none <- aggregate_ind(data, any_CC, "outcome - none") %>% 
  filter(sub_indicator == 'no') %>% 
  select(-c(sub_indicator))



#### 12. Combine outputs & save data ####

compare_df_cols(totals, epilepsy, mental_health, pain, any_indication, conception_year, age,
                simd, baby_sex, bmi, smoke, folic, healthboard, comorbs, parity,
                nerv_sys, eye, ear_face_neck, con_heart, resp, oro_face_clefts, gast_intest, abd_wall, kid_urin_tract, genital,
                limb, other, any, none)

conception_year <- conception_year %>% mutate(sub_indicator = as.character(sub_indicator))
folic<-folic %>% mutate(sub_indicator = as.character(sub_indicator))
drug_alcohol <- drug_alcohol %>% mutate(sub_indicator = as.character(sub_indicator))


t1c <- bind_rows(totals, epilepsy, mental_health, pain, any_indication, conception_year, age, simd,
                 baby_sex, bmi, 
                 smoke, folic,drug_alcohol, healthboard,comorbs, parity,
                 nerv_sys, eye, ear_face_neck, con_heart, resp, oro_face_clefts,
                 gast_intest, abd_wall, kid_urin_tract, genital, limb, other, any, none) %>%
  select(indicator, sub_indicator, n_exposed, percent_exposed, n_unexposed, percent_unexposed)



rm(list = setdiff(ls(), "t1c"))  # Remove all but T1c


################## Unexposed matched sample ########################


#### 2. Read in main data file ####
data <- readRDS(paste0(folder_data_path,"matched_cohorts/matched_congenital_cohort.rds"))
comorbidity_flag <-  readRDS(paste0(folder_data_path,"linkage/master_dataset_file.rds")) %>%
  select(pregnancy_id, any_smr_comorb)
data<- data %>% left_join(comorbidity_flag)

###add parity
cohort <-readRDS(paste0(folder_data_path,"SLiPBD_cohort_extract.rds"))
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+", T~"Unknown"))

#### 3. Indication for ASM ####
totals <- data %>% group_by(CC_exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total",
         percent = n/sum(n)*100,
         CC_exposed_any_asm = (case_when(CC_exposed_any_asm == 0 ~ "unexposed",  
                                         CC_exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(n, percent))


#function for aggregating indications columns
aggregate_ind <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(CC_exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           CC_exposed_any_asm = (case_when(CC_exposed_any_asm == 0 ~ "n_unexposed",  
                                           CC_exposed_any_asm == 1 ~ "n_exposed")),
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}} == 1 ~ "yes"))) %>%
    pivot_wider(names_from = CC_exposed_any_asm, values_from = c(n)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
    select(-1)
  
}

epilepsy <- aggregate_ind(data, epilepsy_indication, "maternal epilepsy")
mental_health <- aggregate_ind(data, mh_flag, "maternal mental health conditions")
pain <- aggregate_ind(data, migraine_pain_flag, "maternal migraine or pain conditions")

any_indication <- data %>% mutate(any_indication = case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1, T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm")


#### 4. year of conception ####
# function
aggregate_data <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(CC_exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           CC_exposed_any_asm = (case_when(CC_exposed_any_asm == 0 ~ "n_unexposed",  
                                           CC_exposed_any_asm == 1 ~ "n_exposed"))) %>%
    pivot_wider(names_from = CC_exposed_any_asm, values_from = c(n)) %>%
    mutate(n_unexposed = case_when(is.na(n_unexposed) ~ 0, T ~ n_unexposed),
           n_exposed = case_when(is.na(n_exposed) ~ 0, T ~ n_exposed)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>% 
    rename("sub_indicator" = 1)
}

conception_year <- aggregate_data(data, year_conception, "conception year")

#### 5. Maternal age ####
age <- aggregate_data(data, maternal_age_group_conception, "maternal age at conception")


#### 6. maternal deprivation ####
simd <- aggregate_data(data, mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 7. baby sex ####
baby_sex <- aggregate_data(data, baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 8. maternal BMI at booking ####
bmi <- aggregate_data(data, maternal_bmi_group, "maternal BMI at booking")


#### 9. Maternal smoking at booking ####
smoke <- aggregate_data(data, maternal_smoking, "maternal smoking at booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 10. Maternal high dose folic acid ####
folic <- aggregate_data(data, high_dose_folic_acid, "maternal high dose folic acid") 

#### 11. Maternal drug or alcohol exposure###
drug_alcohol <- aggregate_data(data, drug_alcohol_use, "maternal drug or alcohol use") 

#### 12. Maternal healthboard ####
Health_Board_Lookup <- read_csv(paste0(folder_data_path,"Health Board Area all years Lookup.csv"))
table(data$maternal_nhs_board_res_booking)


data <- data %>% left_join(Health_Board_Lookup, by= c("maternal_nhs_board_res_booking" ="HealthboardCode")) %>% 
  select(-year) 
data <- data %>%rename(maternal_nhs_board_res_booking_name = NRSHealthBoardAreaName)
data <- data %>% 
  mutate(mother_nhs_board =case_when(is.na(maternal_nhs_board_res_booking_name)~
                                       maternal_nhs_board_res_name_end_preg, T~ maternal_nhs_board_res_booking_name))

healthboard <-  aggregate_data(data, mother_nhs_board, "mother_nhs_board") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))

#### SMR comorbidities####
comorbs <- aggregate_data(data, any_smr_comorb, "maternal comorbidity flag") 

#### parity###
parity <- aggregate_data(data, prev_pregs_group, "Number of previous deliveries") 


#### 11. Outcome ####

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
  select(-c(sub_indicator))
eye <- aggregate_ind(data, all_2_eye, "outcome - eye conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
ear_face_neck <- aggregate_ind(data, all_3_ear_face_and_neck, "outcome - ear, face, and neck conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
con_heart <- aggregate_ind(data, all_4_congenital_heart_defects, "outcome - congenital heart conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
resp <- aggregate_ind(data, all_5_respiratory, "outcome - respiratory conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
oro_face_clefts <- aggregate_ind(data, all_6_oro_facial_clefts, "outcome - oro-facial clefts") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
gast_intest <- aggregate_ind(data, all_7_gastro_intestinal, "outcome - gastro-intestinal conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
abd_wall <- aggregate_ind(data, all_8_abdominal_wall_defects, "outcome - abdominal wall defects") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
kid_urin_tract <- aggregate_ind(data, all_9_kidney_and_urinary_tract, "outcome - kidney and urinary tract conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
genital <- aggregate_ind(data, all_10_genital, "outcome - genital conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
limb <- aggregate_ind(data, all_11_limb, "outcome - limb conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
other <- aggregate_ind(data, all_12_other_conditions, "outcome - other conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
any <- aggregate_ind(data, any_CC, "outcome - any conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator))
none <- aggregate_ind(data, any_CC, "outcome - none") %>% 
  filter(sub_indicator == 'no') %>% 
  select(-c(sub_indicator))



#### 12. Combine outputs & save data ####

compare_df_cols(totals, epilepsy, mental_health, pain, any_indication, conception_year, age, simd,
                baby_sex, bmi, smoke, folic,healthboard , comorbs, parity,
                drug_alcohol, nerv_sys, eye, ear_face_neck,
                con_heart, resp, oro_face_clefts, gast_intest, abd_wall, kid_urin_tract, genital,
                limb, other, any, none)

conception_year <- conception_year %>% mutate(sub_indicator = as.character(sub_indicator))
folic<-folic %>% mutate(sub_indicator = as.character(sub_indicator))
drug_alcohol <- drug_alcohol %>% mutate(sub_indicator = as.character(sub_indicator))
comorbs <- comorbs %>% mutate(sub_indicator = case_when(sub_indicator == 0 ~ 'no',
                                                        sub_indicator == 1 ~ 'yes')) %>% 
  mutate(sub_indicator = as.character(sub_indicator))


t1c_matched <- bind_rows(totals, epilepsy ,mental_health, pain, any_indication, conception_year,
                         age, simd, baby_sex, bmi, smoke, folic, drug_alcohol, healthboard ,comorbs, parity,
                 nerv_sys, eye, ear_face_neck, con_heart, resp, oro_face_clefts, gast_intest,
                 abd_wall, kid_urin_tract, genital,
                 limb, other, any, none) %>%
  select(indicator, sub_indicator, n_exposed, percent_exposed, n_unexposed, percent_unexposed) %>% 
  rename(n_exposed_matched = n_exposed,
         percent_exposed_matched = percent_exposed,
         n_unexposed_matched = n_unexposed,
         percent_unexposed_matched = percent_unexposed)


# Combine T1c and T1c_matched
final_t1c <- left_join(t1c, t1c_matched)

# n_exposed and n_exposed_matched should match
final_t1c %>% 
  mutate(diff = n_exposed - n_exposed_matched) %>% 
  filter(diff != 0)
# all match

final_t1c <- final_t1c %>% 
  select(-c(n_exposed_matched, percent_exposed_matched))


write_csv(final_t1c, paste0(folder_data_path,"Descriptives/table_1c.csv"))
