###01f.link_drug_alcohol_misuse.r##
# Script to link additional SMR comorbidities, drug and alcohol misuse
# to pregnancy dataset and flag indicators


library(dplyr)
library(tidyverse)
library(haven)
library(labelled)
library(lubridate)
library(janitor)
library(odbc)
library(phsmethods)
library(collapse)
source("data_linkage/00.setup_expose.r")
#### 2. Read in data ####
preg <- readRDS(paste0(data_path ,'SLiPBD_cohort_extract.rds'))
smr01 <- readRDS(paste0(data_path ,'smra_extracts/smr01_indicators.rds') )
smr02 <- readRDS(paste0(data_path ,'smra_extracts/smr02_indicators.rds'))
smr04 <- readRDS(paste0(data_path ,'smra_extracts/smr04_indicators.rds'))


# combine smr data
compare_df_cols(smr01, smr02, smr04)

##join all SMR and keep only the drug and alcohol flagged stays
smr <- rbind(smr01, smr02, smr04) %>% 
  select(-c(epilepsy_indication, mh_indication, migraine_pain_indication)) %>%
  filter(alcohol_drug_use==1)

rm(smr01, smr02, smr04)

#### 3. SMR  D & A flag ####
# join with preg data to check dates 
# date of discharge 5 years prior to, or 14 days after pregnancy end date
preg_temp <- preg %>% select(pregnancy_id, mother_upi, est_date_conception, date_end_pregnancy)


d_and_a_smr <- smr %>% left_join(preg_temp, by = c("upi_number" = "mother_upi"), relationship = "many-to-many") %>%
  mutate(in_period = case_when(discharge_date >= (date_end_pregnancy-years(5)) & 
                                 discharge_date <= (date_end_pregnancy+14) ~1, T~0 ))

# check the dates are ok
check <- d_and_a_smr %>% mutate(date_less5 = date_end_pregnancy-years(5),
                                   date_plus2week = date_end_pregnancy+14)
# end check  

d_and_a_smr <- d_and_a_smr %>% filter(in_period == 1) %>% 
  select(upi_number, pregnancy_id,  alcohol_drug_use) %>%
  unique()  %>%
  group_by(upi_number, pregnancy_id) %>%
  summarise(drug_alcohol_use = max(alcohol_drug_use)
  ) %>% ungroup()


# join smr indication flags onto preg data by pregnancy ID
preg <- preg_temp %>% left_join(d_and_a_smr, by = "pregnancy_id", relationship = "many-to-one")

#### 4. Tidy up data ####

# Remove extra UPI columns
# Fill in missing values in the flags with 0
preg <- preg %>% select(-upi_number) %>%
  mutate(drug_alcohol_use = case_when(is.na(drug_alcohol_use) ~0, T~drug_alcohol_use))
#### 6. Save data ####
preg %>% 
  saveRDS(readRDS(paste0(data_path ,'linkage/drug_alcohol_flag.rds')))
