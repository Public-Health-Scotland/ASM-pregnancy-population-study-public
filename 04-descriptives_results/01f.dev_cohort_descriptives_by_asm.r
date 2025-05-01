##01f.dev_cohort_descriptives_by_asm.r
# Script to create descriptives table of the early childhood developmental outcomes cohort
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
  mutate(control_any_dev_rev = case_when(exposed_any_asm==0 & est_date_conception <= as.Date("2020-07-01") &
                                           pregnancy_loss=="No"  ~1, T~0),
         cases_any_dev_rev = case_when(exposed_any_asm==1 & est_date_conception <=  as.Date("2020-07-01") &
                                         pregnancy_loss=="No" ~1, T~0)) %>%
  filter(control_any_dev_rev == 1 | cases_any_dev_rev == 1) %>% # restrict to only those eligible for inclusion in the congential conditions cohort
  mutate(exposed_drug = (case_when(exposed_valproate_mono == 1 ~ "valproate",  
                                   exposed_topiramate_mono == 1 ~ "topiramate",
                                   exposed_carbamazepine_mono == 1 ~ "carbamazepine",
                                   exposed_lamotrigine_mono == 1 ~ "lamotrigine",
                                   exposed_levetiracetam_mono == 1 ~ "levetiracetam",
                                   exposed_gabapentin_mono == 1 ~ "gabapentin",
                                   exposed_pregabalin_mono == 1 ~ "pregabalin",
                                   T ~ NA)))%>%
  ##count all unknown dev. for those with and without reviews
  mutate(any_dev_excl_v_h= case_when(is.na(any_dev_excl_v_h) ~"U", T~any_dev_excl_v_h )) 

###add parity
cohort <-readRDS(paste0(folder_data_path,"SLiPBD_cohort_extract.rds"))
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+", T~"Unknown"))



total_births <- data %>% nrow() 

#### 3. Totals ####
t1f <- data %>% filter(exposed_any_asm == 1) %>% summarise(n=n()) %>% 
  mutate(indicator = "Total incl unknown",
         exposed_drug = "any_asm",
         percent = n/total_births*100) %>% select(exposed_drug, indicator, n, percent) 


t_df <- data %>% 
  filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 | 
           exposed_carbamazepine_mono ==1 | exposed_lamotrigine_mono==1 | exposed_levetiracetam_mono==1 |
           exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
  group_by(exposed_drug) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total incl unknown",
         percent = n/total_births*100) 

t1f <- bind_rows(t1f, t_df) %>% pivot_wider(names_from = exposed_drug, values_from = c(n, percent)) %>%
  rename("valproate" = "n_valproate",
         "topiramate" = "n_topiramate",
         "carbamazepine" = "n_carbamazepine",
         "lamotrigine" = "n_lamotrigine",
         "levetiracetam" = "n_levetiracetam",
         "gabapentin" = "n_gabapentin",
         "pregabalin" = "n_pregabalin",
         "any_asm" = "n_any_asm") %>% mutate(sub_indicator = NA) %>%
  select(indicator, sub_indicator, 
         valproate, percent_valproate, 
         topiramate, percent_topiramate, 
         carbamazepine, percent_carbamazepine,
         lamotrigine, percent_lamotrigine,
         levetiracetam, percent_levetiracetam,
         gabapentin, percent_gabapentin,
         pregabalin, percent_pregabalin,
         any_asm, percent_any_asm) 

#### 3a. Unknown outcomes ####
t1_un <- data %>% filter(exposed_any_asm == 1 & (any_dev_excl_v_h=="U") ) %>% 
  summarise(n=n()) %>% 
  mutate(indicator ="Unknown outcome",
         exposed_drug = "any_asm",
         percent = n/total_births*100) %>% select(exposed_drug, indicator, n, percent) 


t_df_un <- data %>% filter( any_dev_excl_v_h=="U") %>% 
  filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
           exposed_carbamazepine_mono==1 | exposed_lamotrigine_mono==1| exposed_levetiracetam_mono==1 | 
           exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
  group_by(exposed_drug) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Unknown outcome",
         percent = n/total_births*100) 

df <- bind_rows(t1_un, t_df_un) %>% pivot_wider(names_from = exposed_drug, values_from = c(n, percent)) %>%
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
         gabapentin, percent_gabapentin, 
         pregabalin, percent_pregabalin,
         any_asm, percent_any_asm) 

t1f <- bind_rows(t1f, df) 

#### 3b. Total without unknown outcomes ####
data <- data %>% filter(exposed_any_asm == 1 & 
                          any_dev_excl_v_h!="U"  ) 

t1_t <- data %>% 
  summarise(n=n()) %>% 
  mutate(indicator ="Total",
         exposed_drug = "any_asm",
         percent = n/total_births*100) %>% select(exposed_drug, indicator, n, percent) 


t_df_t <- data %>% filter(any_dev_excl_v_h!="U") %>% 
  filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
           exposed_carbamazepine_mono==1 | exposed_lamotrigine_mono==1| exposed_levetiracetam_mono==1 | 
           exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
  group_by(exposed_drug) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total",
         percent = n/total_births*100) 

df <- bind_rows(t1_t, t_df_t) %>% pivot_wider(names_from = exposed_drug, values_from = c(n, percent)) %>%
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
         gabapentin, percent_gabapentin, 
         pregabalin, percent_pregabalin,
         any_asm, percent_any_asm) 


t1f <- bind_rows(t1f, df) 

#### 4. Indication for ASM ####
aggregate_ind <- function(data, ind, indicator_name) {
  df <- data %>% filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 | 
                          exposed_carbamazepine_mono==1 | exposed_lamotrigine_mono==1 | 
                          exposed_levetiracetam_mono==1 | exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
    group_by(exposed_drug, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}} == 1 ~ "yes"))) 
  
  df <- bind_rows(df, 
                  (data %>% filter(exposed_any_asm == 1) %>% 
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
           percent_gabapentin = gabapentin/sum(gabapentin)*100,
           percent_pregabalin = pregabalin/sum(pregabalin)*100,
           percent_any_asm = any_asm/sum(any_asm)*100)
}

t_df <- data %>% aggregate_ind(epilepsy_indication, "maternal epilepsy")

t1f <- bind_rows(t1f, t_df) %>% select(-epilepsy_indication)

t_df <- data %>% aggregate_ind(mh_flag, "maternal mental health conditions") %>% select(-mh_flag)
t1f <- bind_rows(t1f, t_df) 

t_df <- data %>% aggregate_ind(migraine_pain_flag, "maternal migraine or pain conditions") %>% 
  select(-migraine_pain_flag)
t1f <- bind_rows(t1f, t_df) 

t_df <- data %>% mutate(any_indication =
                          case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1,
                                    T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm") %>% select(-any_indication)
t1f <- bind_rows(t1f, t_df) 


#### 5. year of conception ####
aggregate_data <- function(ind, indicator_name) {
  df <- data %>% filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 | 
                          exposed_carbamazepine_mono ==1 | exposed_lamotrigine_mono ==1 | 
                          exposed_levetiracetam_mono ==1 | exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
    group_by(exposed_drug, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           n = ifelse(is.na(n), 0, n))
  
  df <- bind_rows(df, 
                  (data %>% filter(exposed_any_asm == 1) %>% 
                     group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
                     mutate(indicator = indicator_name,
                            exposed_drug = "any_asm"))) %>%
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(valproate = ifelse(is.na(valproate), 0, valproate),
           topiramate = ifelse(is.na(topiramate), 0, topiramate),
           carbamazepine = ifelse(is.na(carbamazepine), 0, carbamazepine),
           lamotrigine = ifelse(is.na(lamotrigine), 0, lamotrigine),
           levetiracetam = ifelse(is.na(levetiracetam), 0, levetiracetam),
           gabapentin = ifelse(is.na(gabapentin), 0, gabapentin),
           pregabalin = ifelse(is.na(pregabalin), 0, pregabalin),
           any_asm = ifelse(is.na(any_asm), 0, any_asm)) %>%
    mutate(percent_valproate = valproate/sum(valproate)*100,
           percent_topiramate = topiramate/sum(topiramate)*100,
           percent_carbamazepine = carbamazepine/sum(carbamazepine)*100,
           percent_lamotrigine = lamotrigine/sum(lamotrigine)*100,
           percent_levetiracetam = levetiracetam/sum(levetiracetam)*100,
           percent_gabapentin = gabapentin/sum(gabapentin)*100,
           percent_pregabalin = pregabalin/sum(pregabalin)*100,
           percent_any_asm = any_asm/sum(any_asm)*100) %>% 
    rename("sub_indicator" = 1)
}

t_df <- aggregate_data(year_conception, "conception year") %>% mutate(sub_indicator = as.character(sub_indicator))
t1f <- bind_rows(t1f, t_df) 


#### 6. Maternal age ####
t_df <- aggregate_data(maternal_age_group_conception, "maternal age at conception")

t1f <- bind_rows(t1f, t_df) 

#### 7. maternal deprivation ####
t_df <- aggregate_data(mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1f <- bind_rows(t1f, t_df) 

#### 8. baby sex ####
t_df <- aggregate_data(baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1f <- bind_rows(t1f, t_df) 

#### 9. maternal BMI at booking ####
t_df <- aggregate_data(maternal_bmi_group, "maternal BMI at booking")
t1f <- bind_rows(t1f, t_df) 

#### 10. Maternal smoking at booking ####
t_df <- aggregate_data(maternal_smoking, "maternal smoking at booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1f <- bind_rows(t1f, t_df) 

#### 11. Maternal high dose folic acid ####
t_df <- aggregate_ind(data, high_dose_folic_acid, "maternal high dose folic acid") %>% select(-high_dose_folic_acid)
t1f <- bind_rows(t1f, t_df) 

#### 10. healthboard ####

t_df <- aggregate_data(hb_27m, "Healthboard at review") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1f <- bind_rows(t1f, t_df) 

#### drug alcohol use ####
t_df <- aggregate_ind(data, drug_alcohol_use, "Maternal drug or alcohol use") %>% select(-drug_alcohol_use)
t1f <- bind_rows(t1f, t_df)

#### SMR comorbidities####
t_df <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity") %>% select(-any_smr_comorb)
t1f <- bind_rows(t1f, t_df)

#### 10. previous pregnancies ####

t_df <- aggregate_data(prev_pregs_group, "Number of previous deliveries") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1f <- bind_rows(t1f, t_df) 


#### 12. Outcome ####

# Any
# Speech, language and communication
# Gross motor
# Fine motor
# Problem solving
# Personal/social
# Emotional/behavioural
# No early childhood developmental concern
# Unknown early childhood developmental concern status

# any_dev_excl_v_h = Y
# dev_slc_flag
# dev_gross_flag
# dev_fm_flag
# dev_prob_flag
# dev_persoc_flag
# dev_EB_flag
# any_dev_excl_v_h = N
# any_dev_excl_v_h = U/NA


aggregate_outcome <- function(data, ind, indicator_name) {
  df <- data %>% filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 | 
                          exposed_carbamazepine_mono ==1 | exposed_lamotrigine_mono ==1 | 
                          exposed_levetiracetam_mono ==1
                        | exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
    group_by(exposed_drug, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                      {{ind}} == 'Y' ~ "yes",
                                      {{ind}} == 'unknown' ~ "unknown"))) 
  
  df <- bind_rows(df, 
                  (data %>% filter(exposed_any_asm == 1) %>% 
                     group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
                     mutate(indicator = indicator_name,
                            exposed_drug = "any_asm",
                            sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                                       {{ind}} == 'Y' ~ "yes",
                                                       {{ind}} == 'unknown' ~ "unknown"))))) %>%
    
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(percent_valproate = valproate/sum(valproate)*100,
           percent_topiramate = topiramate/sum(topiramate)*100,
           percent_carbamazepine = carbamazepine/sum(carbamazepine)*100,
           percent_lamotrigine = lamotrigine/sum(lamotrigine)*100,
           percent_levetiracetam = levetiracetam/sum(levetiracetam)*100,
           percent_gabapentin = gabapentin/sum(gabapentin)*100,
           percent_pregabalin = pregabalin/sum(pregabalin)*100,
           percent_any_asm = any_asm/sum(any_asm)*100)
}


slc <- aggregate_outcome(data, dev_slc_flag, "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_outcome(data, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_outcome(data, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_outcome(data, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_outcome(data, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_outcome(data, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))


aggregate_outcome2 <- function(data, ind, indicator_name) {
  df <- data %>% filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
                          exposed_carbamazepine_mono ==1 | exposed_lamotrigine_mono ==1 |
                          exposed_levetiracetam_mono ==1 | exposed_gabapentin_mono ==1 | exposed_pregabalin_mono == 1) %>% 
    group_by(exposed_drug, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                      {{ind}} == 'Y' ~ "yes",
                                      {{ind}} == 'U' ~ "unknown"))) 
  
  df <- bind_rows(df, 
                  (data %>% filter(exposed_any_asm == 1) %>% 
                     group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
                     mutate(indicator = indicator_name,
                            exposed_drug = "any_asm",
                            sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                                       {{ind}} == 'Y' ~ "yes",
                                                       {{ind}} == 'U' ~ "unknown"))))) %>%
    
  

      
      pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
      mutate(percent_valproate = valproate/sum(valproate)*100,
             percent_topiramate = topiramate/sum(topiramate)*100,
             percent_carbamazepine = carbamazepine/sum(carbamazepine)*100,
             percent_lamotrigine = lamotrigine/sum(lamotrigine)*100,
             percent_levetiracetam = levetiracetam/sum(levetiracetam)*100,
             percent_gabapentin = gabapentin/sum(gabapentin)*100,
             percent_pregabalin = pregabalin/sum(pregabalin)*100,
             percent_any_asm = any_asm/sum(any_asm)*100)
  }
  
  any <- aggregate_outcome2(data, any_dev_excl_v_h, "outcome - any") %>% 
    select(-c(any_dev_excl_v_h))
  
  
  outcomes <- bind_rows(slc, gross, fm, prob, persoc, eb, any)
  
  
  
  # 13) Combine data --------------------------------------------------------
  
  t1f <- bind_rows(t1f, outcomes)
  

write_csv(t1f,paste0(folder_data_path,"Descriptives/table_1f.csv"))


