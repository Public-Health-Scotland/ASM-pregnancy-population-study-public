##01a.preg_cohort_descriptives.r
# Script to create descriptives table of the pregnancy cohort
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

#
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
data <- readRDS(paste0(folder_data_path,"linkage/master_dataset_file.rds"))
data<- data %>% mutate(pregnancy_loss = 
                         case_when(fetus_outcome1=="Unknown - assumed early loss" ~ "Yes", T~pregnancy_loss))

###add parity
cohort <-readRDS(paste0(folder_data_path,"SLiPBD_cohort_extract.rds")) 
parity <- cohort %>% select(pregnancy_id, n_prev_deliveries)

data <- data %>% left_join(parity) 
data <- data %>% mutate(prev_pregs_group = case_when(n_prev_deliveries==0 ~ "0", 
                                                     n_prev_deliveries>=1 ~ "1+", T~"Unknown"))

#### 3. Totals ####
total_incl_unkn <- data %>% group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total incl unknown",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))

#### 3a.Total unknown outcomes ####
total_unkn <- data %>% filter(pregnancy_loss=="Unknown" | pregnancy_loss=="Maternal death") %>%
  group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Unknown outcome",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent)) %>% 
  select(-c(percent_exposed, percent_unexposed))

#### 3b.Total known outcomes ####
totals <- data %>% filter(pregnancy_loss!="Unknown" & pregnancy_loss !="Maternal death") %>%
  group_by(exposed_any_asm) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "Total",
         percent = n/sum(n)*100,
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "unexposed",  
                                      exposed_any_asm == 1 ~ "exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n, percent))


# Calculate the unknowns %'s as a % of all events
df <- total_incl_unkn %>% 
  select(-c(percent_exposed, percent_unexposed)) 
df <- bind_rows(df, total_unkn)

df <- df %>% 
  pivot_longer(cols = c(n_exposed, n_unexposed),
               names_to = 'exposure',
               values_to = 'n')

unkn_percent <- df %>% 
  group_by(exposure) %>% 
  summarise(percent = (n[indicator == "Unknown outcome"] / n[indicator == "Total incl unknown"])*100) %>% 
  pivot_wider(names_from = exposure, values_from = percent) %>% 
  mutate(indicator = "Unknown outcome") %>% 
  rename(percent_exposed = n_exposed,
         percent_unexposed = n_unexposed)

total_unkn <- left_join(total_unkn, unkn_percent, by = 'indicator')

rm(unkn_percent, df)



## 3d. remove unknwons ####
data <- data %>% filter(pregnancy_loss!="Unknown"  & pregnancy_loss !="Maternal death")

#### 4 Indications ####

epilepsy <- aggregate_ind(data, epilepsy_indication, "maternal epilepsy")
mental_health <- aggregate_ind(data, mh_flag, "maternal mental health conditions")
pain <- aggregate_ind(data, migraine_pain_flag, "maternal migraine or pain conditions")

any_indication <- data %>% mutate(any_indication = 
                                    case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1,
                                              T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm")

#### 5. year of conception ####

conception_year <- aggregate_data(data, year_conception, "conception year")

#### 6. Maternal age ####
age <- aggregate_data(data, maternal_age_group_conception, "maternal age at conception")


#### 7. maternal deprivation ####
simd <- aggregate_data(data, mother_simd, "maternal deprivation") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 8. baby sex ####
baby_sex <- aggregate_data(data, baby_sex, "baby sex") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))


#### 9. maternal BMI at booking ####
bmi <- aggregate_data(data, maternal_bmi_group, "maternal BMI at booking")


#### 10. Maternal smoking at booking ####
smoke <- aggregate_data(data, maternal_smoking, "maternal smoking at booking") %>%
  mutate(sub_indicator = case_when(is.na(sub_indicator) ~ "unknown", T ~ sub_indicator))

#### 11. Maternal high dose folic acid ####
folic <- aggregate_ind(data, high_dose_folic_acid, "maternal high dose folic acid") 

#### 12. Maternal drug or alcohol exposure###
drug_alcohol <- aggregate_ind(data, drug_alcohol_use, "maternal drug or alcohol use in pregnancy") 

#### 10. Maternal healthboard ####
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

## SMR comorbidities
comorbs <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity flag") 

#### parity###
parity <- aggregate_data(data, prev_pregs_group, "Number of previous deliveries") 


#### 13. Outcome ####
outcome <- data %>% mutate(outcome_group = case_when(
  fetus_outcome1 %in% c("Ectopic pregnancy", "Miscarriage", "Molar pregnancy", "Unknown - assumed early loss") ~ "Early spontaneous loss",
  fetus_outcome1 %in% c("Unknown",  "Unknown - emigrated") ~ "Unknown pregnancy outcome",
  T ~ fetus_outcome1)) %>% 
  group_by(exposed_any_asm, pregnancy_loss, outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = case_when(pregnancy_loss == "Yes" ~ "outcome - pregnancy loss", T ~ "outcome"),
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                      exposed_any_asm == 1 ~ "n_exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
  mutate(n_unexposed = case_when(is.na(n_unexposed) ~ 0, T ~ n_unexposed),
         n_exposed = case_when(is.na(n_exposed) ~ 0, T ~ n_exposed)) %>%
  mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
         percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
  rename(sub_indicator = outcome_group) %>% select(-pregnancy_loss)

any_loss <- data %>%
  mutate(outcome_group = case_when(
    fetus_outcome1 %in% c("Ectopic pregnancy", "Miscarriage", "Molar pregnancy", "Unknown - assumed early loss") ~ "Early spontaneous loss",
    fetus_outcome1 %in% c("Unknown",  "Unknown - emigrated") ~ "Unknown pregnancy outcome",
    T ~ fetus_outcome1)) %>%
  group_by(exposed_any_asm, pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "outcome - pregnancy loss",
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                      exposed_any_asm == 1 ~ "n_exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
  mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
         percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
  filter(pregnancy_loss == "Yes") %>%
  mutate(sub_indicator = "Any loss") %>% select(-pregnancy_loss)

outcome <- bind_rows(outcome, any_loss)

rm(any_loss)

#### 13. Combine outputs & save data ####

compare_df_cols(totals, epilepsy, mental_health, pain, any_indication, conception_year, age,
                simd, baby_sex, bmi, smoke, folic,healthboard, parity, outcome)

conception_year <- conception_year %>% mutate(sub_indicator = as.character(sub_indicator))

t1a <- bind_rows(total_incl_unkn, total_unkn,totals, epilepsy, mental_health, pain, 
                 any_indication, conception_year, age, simd, baby_sex, bmi, smoke, folic,
                 drug_alcohol, comorbs, healthboard, parity, outcome) %>%
  select(indicator, sub_indicator, n_exposed, percent_exposed, n_unexposed, percent_unexposed)


rm(list = setdiff(ls(), "t1a"))  # Remove all but T1a

################## Unexposed matched sample ########################


#### 2. Read in data file ####
data <- readRDS(paste0(folder_data_path,"matched_cohorts/matched_preg_outcomes_cohort.rds"))
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

any_indication <- data %>% mutate(any_indication = case_when((epilepsy_indication == 1 | mh_flag == 1 | migraine_pain_flag == 1) ~ 1, T ~ 0)) %>%
  aggregate_ind(any_indication, "any condition indicating asm")


#### 4. year of conception ####
#conception_year <- data %>% group_by(exposed_any_asm, year_conception) %>% summarise(n=n()) %>% ungroup() %>%
#  mutate(indicator = "conception year",
#         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
#                                      exposed_any_asm == 1 ~ "n_exposed"))) %>%
#  pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
#  mutate(n_unexposed = case_when(is.na(n_unexposed) ~ 0, T ~ n_unexposed),
#         n_exposed = case_when(is.na(n_exposed) ~ 0, T ~ n_exposed)) %>%
#  mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
#         percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
#  rename(sub_indicator = year_conception)

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
folic <- aggregate_ind(data, high_dose_folic_acid, "maternal high dose folic acid") 

#### 12. Maternal drug or alcohol exposure###
drug_alcohol <- aggregate_ind(data, drug_alcohol_use, "maternal drug or alcohol use in pregnancy") 

#### 10. Maternal healthboard ####
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

## SMR comorbidities####
comorbs <- aggregate_ind(data, any_smr_comorb, "maternal comorbidity flag") 

#### parity###
parity <- aggregate_data(data, prev_pregs_group, "Number of previous deliveries") 

#### 11. Outcome ####

outcome <- data %>% 
  mutate(outcome_group = case_when(
    fetus_outcome1 %in% c("Ectopic pregnancy", "Miscarriage", "Molar pregnancy",  "Unknown - assumed early loss") ~ "Early spontaneous loss",
    fetus_outcome1 %in% c("Unknown", "Unknown - emigrated") ~ "Unknown pregnancy outcome",
    T ~ fetus_outcome1)) %>% group_by(exposed_any_asm, pregnancy_loss, outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = case_when(pregnancy_loss == "Yes" ~ "outcome - pregnancy loss", T ~ "outcome"),
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                      exposed_any_asm == 1 ~ "n_exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
  mutate(n_unexposed = case_when(is.na(n_unexposed) ~ 0, T ~ n_unexposed),
         n_exposed = case_when(is.na(n_exposed) ~ 0, T ~ n_exposed)) %>%
  mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
         percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
  rename(sub_indicator = outcome_group) %>% select(-pregnancy_loss)

any_loss <- data %>% 
  group_by(exposed_any_asm, pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(indicator = "outcome - pregnancy loss",
         exposed_any_asm = (case_when(exposed_any_asm == 0 ~ "n_unexposed",  
                                      exposed_any_asm == 1 ~ "n_exposed"))) %>%
  pivot_wider(names_from = exposed_any_asm, values_from = c(n)) %>%
  mutate(n_exposed = ifelse(is.na(n_exposed), 0, n_exposed),
         n_unexposed = ifelse(is.na(n_unexposed), 0, n_unexposed)) %>% 
  mutate(percent_exposed = n_exposed/sum(n_exposed)*100,
         percent_unexposed = n_unexposed/sum(n_unexposed)*100) %>%
  filter(pregnancy_loss == "Yes") %>%
  mutate(sub_indicator = "Any loss") %>% select(-pregnancy_loss)

outcome <- bind_rows(outcome, any_loss)

rm(any_loss)


#### 12. Combine outputs & save data ####

conception_year <- conception_year %>% mutate(sub_indicator = as.character(sub_indicator))

t1a_matched <- bind_rows(totals, epilepsy, mental_health, pain, any_indication, conception_year,
                         age, simd, baby_sex, bmi, smoke, folic, drug_alcohol, comorbs, healthboard, parity, outcome) %>%
  select(indicator, sub_indicator, n_exposed, percent_exposed, n_unexposed, percent_unexposed) %>% 
  rename(n_exposed_matched = n_exposed,
         percent_exposed_matched = percent_exposed,
         n_unexposed_matched = n_unexposed,
         percent_unexposed_matched = percent_unexposed)

# Combine T1a and T1a_matched
final_t1a <- left_join(t1a, t1a_matched)

# n_exposed and n_exposed_matched should match
final_t1a %>% 
  mutate(diff = n_exposed - n_exposed_matched) %>% 
  filter(diff != 0)
# all match

final_t1a <- final_t1a %>% 
  select(-c(n_exposed_matched, percent_exposed_matched))


write_csv(final_t1a, paste0(folder_data_path,"Descriptives/table_1a.csv"))
