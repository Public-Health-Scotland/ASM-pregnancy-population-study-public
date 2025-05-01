# Name of file -Supp_table4.R ####

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

source("supp_materials_publication/00.supp_setup.r")

#function for aggregating indications columns
aggregate_ind <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                        exposed_any_asm == 1 ~ "n_exposed")),
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}} == 1 ~ "yes"))) %>%
    pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
    select(-1)
  
}
# function
aggregate_data <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                        exposed_any_asm == 1 ~ "n_exposed"))) %>%
    pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
    mutate(n_unexposed = case_when(is.na(n_unexposed) ~ 0, T ~ n_unexposed),
           n_exposed = case_when(is.na(n_exposed) ~ 0, T ~ n_exposed)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>% 
    rename("sub_indicator" = 1)
}


#### 2. Read in main data file ####
data <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds")) %>% 
  mutate(control_any_dev_rev = case_when(exposed_any_asm==0 & est_date_conception <= as.Date("2020-07-01") &
                                           pregnancy_loss=="No"  ~1, T~0),
         cases_any_dev_rev = case_when(exposed_any_asm==1 & est_date_conception <=  as.Date("2020-07-01") &
                                         pregnancy_loss=="No" ~1, T~0)) %>%
  filter(control_any_dev_rev == 1 | cases_any_dev_rev == 1) %>%
  mutate(any_dev_excl_v_h= case_when(is.na(any_dev_excl_v_h) ~"U", T~any_dev_excl_v_h )) 
# restrict to singleton livebirths conceived on or before 1st July 2020.
##include those with invalid or no review 
#(invalid and no review are removed from matched case controls)
###add parity
cohort <-readRDS(paste0(folder_data_path, "SLiPBD_cohort_extract.rds"))
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+", T~"Unknown"))

#### 2a. Totals ####

total_incl_unkn <- data %>% group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total incl unknown",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))


#### 3a.Total unknown outcomes ####
total_unkn <- data %>% filter(any_dev_excl_v_h=="U") %>%
  group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Unknown outcome",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))

###due to missing review
total_missing <- data %>% filter(is.na(data$date_review_27m)) %>%
  group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Missing review",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))

###due to invalid date/age of review
total_invalid_date <- data %>% 
  filter(!is.na(data$date_review_27m) & valid_chsp_review=="no review or invalid date of review") %>%
  group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Invalid date review",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))

##due to no information on review (but a valid date)
##these are flagged as a valid review but overall a U outcome due to assessment not being complted
total_incomplete <- data %>% 
  filter(valid_chsp_review=="valid review" & any_dev_excl_v_h=="U") %>%
  group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Incomplete review assessments",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))

#### 3b.Total known outcomes ####
totals <- data %>% filter(any_dev_excl_v_h!="U") %>%
  group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))


## 3d filter data for th rest of the outcomes####
data <- data %>% filter(any_dev_excl_v_h!="U")

#### 4 Indications####
epilepsy <- aggregate_ind(data, epilepsy_indication, "maternal epilepsy")
mental_health <- aggregate_ind(data, mh_flag, "maternal mental health conditions")
pain <- aggregate_ind(data, migraine_pain_flag, "maternal migraine or pain conditions")

any_indication <- data %>% mutate(any_indication = case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1, T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm")



#### 6. Maternal age ####
age <- aggregate_data(data, maternal_age_group_conception, "maternal age at conception")


#### 7. maternal deprivation ####
simd <- aggregate_data(data, mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 8. baby sex ####
baby_sex <- aggregate_data(data, baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 9. maternal BMI at booking ####
bmi <- aggregate_data(data, maternal_bmi_group, "maternal BMI at antenatal booking")


#### 10. Maternal smoking at booking ####
smoke <- aggregate_data(data, maternal_smoking, "maternal smoking at antenatal booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))

#### 11. Maternal high dose folic acid ####

folic <- aggregate_ind(data, high_dose_folic_acid, "maternal high dose folic acid") 

#### 12. Maternal drug or alcohol exposure###
drug_alcohol <- aggregate_ind(data, drug_alcohol_use, "maternal drug or alcohol use") 

#### 12. parity###
parity <- aggregate_data(data, prev_pregs_group, "Number of previous deliveries") 

#### SMR comorbidities####
comorbs <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity") 



#function for aggregating indications columns
aggregate_outcome <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                        exposed_any_asm == 1 ~ "n_exposed")),
           sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                      {{ind}} == 'Y' ~ "yes",
                                      {{ind}} == 'unknown' ~ "unknown"))) %>%
    pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
    select(-1)
  
}



#function for aggregating indications columns
aggregate_outcome2 <- function(df, ind, indicator_name) {
  t_df <- df %>% 
    group_by(exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                        exposed_any_asm == 1 ~ "n_exposed")),
           sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                      {{ind}} == 'Y' ~ "yes",
                                      {{ind}} == 'U' ~ "unknown"))) %>%
    pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
    select(-1)
  
}



#### 13. Combine outputs & save data ####

compare_df_cols(totals, epilepsy, mental_health, pain, any_indication,
                age, simd, baby_sex, bmi, smoke, folic, drug_alcohol,parity)


#drug_alcohol <- drug_alcohol %>% mutate(sub_indicator = as.character(sub_indicator))

t1e <- bind_rows(total_incl_unkn, total_unkn, totals, epilepsy, mental_health, pain, any_indication,
                age, simd, baby_sex, bmi, smoke, folic, drug_alcohol, parity,comorbs,
                 total_missing, total_incomplete, total_invalid_date) %>%
  select(indicator, sub_indicator, n_exposed, percent_exposed, n_unexposed, percent_unexposed)




################## Unexposed matched sample ########################
#### 2. Read in main data file ####
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_developmental_cohort.rds"))
###add comorbidity
comorbidity_flag <-  readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds")) %>%
  select(pregnancy_id, any_smr_comorb)
data<- data %>% left_join(comorbidity_flag)


###add parity
cohort <-readRDS(paste0(folder_data_path, "SLiPBD_cohort_extract.rds"))
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+",  T~"Unknown"))


#### 3. Indication for ASM ####
totals <- data %>% group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))


#function for aggregating indications columns
aggregate_ind <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                        exposed_any_asm == 1 ~ "n_exposed")),
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}} == 1 ~ "yes"))) %>%
    pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
    select(-1)
  
}

epilepsy <- aggregate_ind(data, epilepsy_indication, "maternal epilepsy")
mental_health <- aggregate_ind(data, mh_flag, "maternal mental health conditions")
pain <- aggregate_ind(data, migraine_pain_flag, "maternal migraine or pain conditions")

any_indication <- data %>% mutate(any_indication = 
                                    case_when((epilepsy_indication == 1 | 
                                                 mh_flag == 1 | migraine_pain_flag == 1) ~ 1, T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm")


####
# function to aggregate variables
aggregate_data <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                        exposed_any_asm == 1 ~ "n_exposed"))) %>%
    pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
    mutate(n_unexposed = case_when(is.na(n_unexposed) ~ 0, T ~ n_unexposed),
           n_exposed = case_when(is.na(n_exposed) ~ 0, T ~ n_exposed)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>% 
    rename("sub_indicator" = 1)
}


#### 5. Maternal age ####
age <- aggregate_data(data, maternal_age_group_conception, "maternal age at conception")


#### 6. maternal deprivation ####
simd <- aggregate_data(data, mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 7. baby sex ####
baby_sex <- aggregate_data(data, baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 8. maternal BMI at booking ####
bmi <- aggregate_data(data, maternal_bmi_group, "maternal BMI at antenatal booking")


#### 9. Maternal smoking at booking ####
smoke <- aggregate_data(data, maternal_smoking, "maternal smoking at antenatal booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 10. Maternal high dose folic acid ####
folic <- aggregate_ind(data, high_dose_folic_acid, "maternal high dose folic acid") 

#### 11. Maternal drug or alcohol exposure###
drug_alcohol <- aggregate_ind(data, drug_alcohol_use, "maternal drug or alcohol use") 


#### 12. parity###
parity <- aggregate_data(data, prev_pregs_group, "Number of previous deliveries") 

#### SMR comorbidities####
comorbs <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity") 


#function for aggregating indications columns
aggregate_outcome <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                        exposed_any_asm == 1 ~ "n_exposed")),
           sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                      {{ind}} == 'Y' ~ "yes",
                                      {{ind}} == 'unknown' ~ "unknown"))) %>%
    pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
    select(-1)
  
}



#function for aggregating indications columns
aggregate_outcome2 <- function(df, ind, indicator_name) {
  t_df <- df %>% group_by(exposed_any_asm, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                        exposed_any_asm == 1 ~ "n_exposed")),
           sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                      {{ind}} == 'Y' ~ "yes",
                                      {{ind}} == 'U' ~ "unknown"))) %>%
    pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
    mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
           percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
    select(-1)
  
}

any <- aggregate_outcome2(data, any_dev_excl_v_h, "outcome - any")


#### 12. Combine outputs & save data ####

compare_df_cols(totals, epilepsy, mental_health, pain, any_indication, age, simd, baby_sex,
                bmi, smoke, folic, drug_alcohol,comorbs)


#drug_alcohol <- drug_alcohol %>%   mutate(sub_indicator = case_when(sub_indicator == 'no' ~ 0,
#                                   sub_indicator == 'yes' ~ 1)) %>% 
#  mutate(sub_indicator = as.character(sub_indicator))


t1e_matched <-  bind_rows(total_incl_unkn, total_unkn, totals, epilepsy, mental_health, pain, any_indication,
                          age, simd, baby_sex, bmi, smoke, folic, drug_alcohol, parity,comorbs,
                          total_missing, total_incomplete, total_invalid_date) %>%
  select(indicator, sub_indicator, n_exposed, percent_exposed, n_unexposed, percent_unexposed) %>% 
  rename(n_exposed_matched = n_exposed,
         percent_exposed_matched = percent_exposed,
         n_unexposed_matched = n_unexposed,
         percent_unexposed_matched = percent_unexposed)


# Combine T1e and T1e_matched
final_t1e <- left_join(t1e, t1e_matched)

# n_exposed and n_exposed_matched should match
final_t1e %>% 
  mutate(diff = n_exposed - n_exposed_matched) %>% 
  filter(diff != 0)
# all match

final_t1e <- final_t1e %>% 
  select(-c(n_exposed_matched, percent_exposed_matched))



saveRDS(final_t1e, paste0(folder_data_path, "supplementary_materials/supp_table2.rds"))
write_csv(final_t1e, paste0(folder_data_path, "supplementary_materials/supp_table2.csv"))

