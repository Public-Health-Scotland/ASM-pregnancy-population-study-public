##01b.preg_cohort_descriptives_by_asm.r
# Script to create descriptives table of the pregnancy cohort
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
  mutate(exposed_drug = (case_when(exposed_valproate_mono == 1 ~ "valproate",  
                                   exposed_topiramate_mono == 1 ~ "topiramate",
                                   exposed_carbamazepine_mono == 1 ~ "carbamazepine",
                                   exposed_lamotrigine_mono == 1 ~ "lamotrigine",
                                   exposed_levetiracetam_mono == 1 ~ "levetiracetam",
                                   exposed_gabapentin_mono == 1 ~ "gabapentin",
                                   exposed_pregabalin_mono == 1 ~ "pregabalin",
                                   T ~ NA)))

total_pregs <- data %>% nrow() 

###add parity
cohort <-readRDS(paste0(folder_data_path,"SLiPBD_cohort_extract.rds"))
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+", T~"Unknown"))

#### 3. Totals ####
t1b <- data %>% filter(exposed_any_asm == 1) %>% summarise(n=n()) %>% 
  mutate(indicator = "Total incl unknown",
         exposed_drug = "any_asm",
         percent = n/total_pregs*100) %>% select(exposed_drug, indicator, n, percent) 


t_df <- data %>% 
  filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
           exposed_carbamazepine_mono==1 | exposed_lamotrigine_mono==1| exposed_levetiracetam_mono==1 |
           exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
  group_by(exposed_drug) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total incl unknown",
         percent = n/total_pregs*100) 

t1b <- bind_rows(t1b, t_df) %>% pivot_wider(names_from = exposed_drug, values_from = c(n, percent)) %>%
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
t1_un <- data %>% filter(exposed_any_asm == 1 & (pregnancy_loss=="Unknown" | pregnancy_loss=="Maternal death") ) %>% 
  summarise(n=n()) %>% 
  mutate(indicator ="Unknown outcome",
         exposed_drug = "any_asm",
         percent = n/total_pregs*100) %>% select(exposed_drug, indicator, n, percent) 


t_df_un <- data %>% filter( pregnancy_loss=="Unknown" | pregnancy_loss=="Maternal death") %>% 
  filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
           exposed_carbamazepine_mono==1 | exposed_lamotrigine_mono==1| exposed_levetiracetam_mono==1 | 
           exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
  group_by(exposed_drug) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Unknown outcome",
         percent = n/total_pregs*100) 

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

t1b <- bind_rows(t1b, df) 

#### 3b. Total without unknown outcomes ####
data <- data %>% filter(exposed_any_asm == 1 & 
                          pregnancy_loss!="Unknown" & pregnancy_loss !="Maternal death" ) 

t1_t <- data %>% 
  summarise(n=n()) %>% 
  mutate(indicator ="Total",
         exposed_drug = "any_asm",
         percent = n/total_pregs*100) %>% select(exposed_drug, indicator, n, percent) 


t_df_t <- data %>% filter( pregnancy_loss!="Unknown" & pregnancy_loss!="Maternal death") %>% 
  filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
           exposed_carbamazepine_mono==1 | exposed_lamotrigine_mono==1| exposed_levetiracetam_mono==1 | 
           exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
  group_by(exposed_drug) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total",
         percent = n/total_pregs*100) 

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


t1b <- bind_rows(t1b, df) 

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

t1b <- bind_rows(t1b, t_df) %>% select(-epilepsy_indication)

t_df <- data %>% aggregate_ind(mh_flag, "maternal mental health conditions") %>% select(-mh_flag)
t1b <- bind_rows(t1b, t_df) 

t_df <- data %>% aggregate_ind(migraine_pain_flag, "maternal migraine or pain conditions") %>% select(-migraine_pain_flag)
t1b <- bind_rows(t1b, t_df) 

t_df <- data %>% mutate(any_indication = case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1, T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm") %>% select(-any_indication)
t1b <- bind_rows(t1b, t_df) 


#### 5. year of conception ####
aggregate_data <- function(ind, indicator_name) {
  df <- data %>% filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
                          exposed_carbamazepine_mono==1 | exposed_lamotrigine_mono==1 | exposed_levetiracetam_mono==1
                        | exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
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
t1b <- bind_rows(t1b, t_df) 

#### 6. Maternal age ####
t_df <- aggregate_data(maternal_age_group_conception, "maternal age at conception")

t1b <- bind_rows(t1b, t_df) 

#### 7. maternal deprivation ####
t_df <- aggregate_data(mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))

t1b <- bind_rows(t1b, t_df) 

#### 8. baby sex ####
t_df <- aggregate_data(baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1b <- bind_rows(t1b, t_df) 

#### 9. maternal BMI at booking ####
t_df <- aggregate_data(maternal_bmi_group, "maternal BMI at booking")
t1b <- bind_rows(t1b, t_df) 

#### 10. Maternal smoking at booking ####
t_df <- aggregate_data(maternal_smoking, "maternal smoking at booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1b <- bind_rows(t1b, t_df) 

#### 11. Maternal high dose folic acid ####
t_df <- aggregate_ind(data, high_dose_folic_acid, "maternal high dose folic acid") %>% select(-high_dose_folic_acid)
t1b <- bind_rows(t1b, t_df) 

#### 10. Maternal healthboard ####
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

t1b <- bind_rows(t1b, t_df)

#### drug alcohol use ####
t_df <- aggregate_ind(data, drug_alcohol_use, "Maternal drug or alcohol use") %>% select(-drug_alcohol_use)
t1b <- bind_rows(t1b, t_df)

#### SMR comorbidities####
t_df <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity") %>% select(-any_smr_comorb)
t1b <- bind_rows(t1b, t_df)

#### parity ####

t_df <- aggregate_data(prev_pregs_group, "Number of previous deliveries") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))
t1b <- bind_rows(t1b, t_df) 

#### 12. Outcome ####
# Pregnancy outcome for mono therapies 
outcome <- data %>% mutate(outcome_group = case_when(
  fetus_outcome1 %in% c("Ectopic pregnancy", "Miscarriage","Unknown - assumed early loss", "Molar pregnancy") ~ "Early spontaneous loss",
  fetus_outcome1 %in% c("Unknown",  "Unknown - emigrated") ~ "Unknown pregnancy outcome",
  T ~ fetus_outcome1)) %>%
  filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 | exposed_carbamazepine_mono | exposed_lamotrigine_mono | exposed_levetiracetam_mono |
           exposed_gabapentin_mono | exposed_pregabalin_mono) %>% 
  group_by(exposed_drug, pregnancy_loss, outcome_group) %>% summarise(n=n()) %>% ungroup() %>% 
  mutate(indicator = case_when(pregnancy_loss == "Yes" ~ "outcome - pregnancy loss", T ~ "outcome")) #%>%

# Pregnancy outcome for any asm
t_df <- data %>% mutate(outcome_group = case_when(
  fetus_outcome1 %in% c("Ectopic pregnancy", "Miscarriage","Unknown - assumed early loss", "Molar pregnancy") ~ "Early spontaneous loss",
  fetus_outcome1 %in% c("Unknown",  "Unknown - emigrated") ~ "Unknown pregnancy outcome",
  T ~ fetus_outcome1)) %>%
  filter(exposed_any_asm == 1) %>%
  group_by(pregnancy_loss, outcome_group) %>% summarise(n=n()) %>% ungroup() %>% 
  mutate(indicator = case_when(pregnancy_loss == "Yes" ~ "outcome - pregnancy loss", T ~ "outcome"),
         exposed_drug = "any_asm") 

outcome <- bind_rows(outcome, t_df)

# total losses for monotherapies
t_df <- data %>% 
  filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
           exposed_carbamazepine_mono==1 | exposed_lamotrigine_mono==1 | exposed_levetiracetam_mono==1
         | exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
  group_by(exposed_drug, pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  filter(pregnancy_loss == "Yes") %>%
  mutate(indicator = "outcome - pregnancy loss",
         outcome_group = "any loss") 

outcome <- bind_rows(outcome, t_df)

# total losses for any asm
t_df <- data %>% 
  filter(exposed_any_asm == 1) %>%
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  filter(pregnancy_loss == "Yes") %>%
  mutate(indicator = "outcome - pregnancy loss",
         exposed_drug = "any_asm",
         outcome_group = "any loss") 

outcome <- bind_rows(outcome, t_df) 

outcome <- outcome %>%
  rename(sub_indicator = outcome_group) %>% 
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
         percent_any_asm = any_asm/sum(any_asm)*100) %>% select(-pregnancy_loss)


# 13) Combine data --------------------------------------------------------

t1b <- bind_rows(t1b, outcome)

write_csv(t1b, paste0(folder_data_path, "Descriptives/table_1b.csv"))
