###01c.flag_indicators##
# Script to link mental health and pain indicators from medicines and SMR dataset
# to pregnancy dataset and flag indicators



#### 1. Housekeeping ####

# Approximate run time - TBD

rm(list = ls())
gc()

# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("haven")
# install.packages("labelled")
# install.packages("lubridate")
# install.packages("gdata", repos = c("https://ppm.publichealthscotland.org/phs-cran/latest"))
# install.packages("phsmethods")

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
source("data_linkage/00.setup_expose.r")

#### 2. Read in data ####
preg <- readRDS(paste0(data_path,  'SLiPBD_cohort_extract.rds'))
drugs <- arrow::read_parquet(paste0(data_path,"scomed_extracts/full_data_file_other_meds.parquet"))
smr01 <- readRDS(paste0(data_path,'smra_extracts/smr01_indicators.rds') )
smr02 <- readRDS(paste0(data_path,'smra_extracts/smr02_indicators.rds'))
smr04 <- readRDS(paste0(data_path,'smra_extracts/smr04_indicators.rds'))


# combine smr data
compare_df_cols(smr01, smr02, smr04)

smr <- rbind(smr01, smr02, smr04)

rm(smr01, smr02, smr04)

#### 3. SMR indication flags ####
# join with preg data to check dates 
# date of discharge 5 years prior to, or 14 days after pregnancy end date
preg_temp <- preg %>% select(pregnancy_id, mother_upi, est_date_conception, date_end_pregnancy)

indication_smr <- smr %>% left_join(preg_temp, by = c("upi_number" = "mother_upi"), relationship = "many-to-many") %>%
  mutate(in_period = case_when(discharge_date >= (date_end_pregnancy-years(5)) & discharge_date <= (date_end_pregnancy+14) ~1, T~0 ))
  
# check the dates are ok
check <- indication_smr %>% mutate(date_less5 = date_end_pregnancy-years(5),
                                 date_plus2week = date_end_pregnancy+14)
# end check  
  
indication_smr <- indication_smr %>% filter(in_period == 1) %>% 
  select(upi_number, pregnancy_id, epilepsy_indication, mh_indication, migraine_pain_indication) %>%
  unique()  %>%
  group_by(upi_number, pregnancy_id) %>%
  summarise(epilepsy_indication = max(epilepsy_indication),
            mh_indication = max(mh_indication),
            migraine_pain_indication = max(migraine_pain_indication)
            ) %>% ungroup()


# join smr indication flags onto preg data by pregnancy ID
preg <- preg_temp %>% left_join(indication_smr, by = "pregnancy_id", relationship = "many-to-one")

#### 4. Prescribing indication flags ####

# flag where presc date is in the year prior to est date of conception 
# (Not during preg as in SMR indications above? - check this with Rachael)

# steps needed:
# 1 - Remove any not dispensed & folic acid - done
# 2 - flag prescription as MH or pain & migraine - done
# bnf section 0402 (plus additional sub sections 040102, 040303, 040304) = MH meds, 
# bnf subsection 040704 (plus additional sub sections 040702, 040301) = pain & migraine
# 3 - presc keep only needed variables
# 4 - Get rid of duplicate rows 

# need to remove any sodium valproate and valproic acid from bnf section 0402
vtm_bnf <- drugs %>% group_by(vtm_name, bnf_section_code) %>% summarise(count = n()) %>% ungroup()

drugs <- drugs %>% filter(!vtm_name %in% c("sodium valproate", "valproic acid")) 

indication_med <- drugs %>% filter(presc_not_supplied != "y" &
                                     (is.na(admin_not_supplied)|admin_not_supplied =="n" ) &
                                     vtm_name != "folic acid") %>%
 mutate(mh_med = case_when(bnf_section_code == "0402" | 
                             bnf_sub_section_code %in% c("040102", "040303", "040304") ~1, T~0),
        migraine_pain_med = case_when(bnf_sub_section_code %in% c("040704", "040702", "040301") ~1, T~0)) 

indication_med %>% 
  filter(mh_med == 1) %>% 
  group_by(bnf_sub_section_code) %>% 
  summarise(n())

indication_med %>% 
  filter(migraine_pain_med == 1) %>% 
  group_by(bnf_sub_section_code) %>% 
  summarise(n())

# join with preg data to check dates 
# dispensed/supplied date 1 years prior to est. conception date. 
## To clarify date discrepencies - confirmed use date of end of pregnancy for drugs 
indication_med <- indication_med %>% left_join(preg_temp, by = c("all_upi" = "mother_upi"),
                                               relationship = "many-to-many") %>%
  mutate(concep_less_year = est_date_conception-years(1),
         in_period = case_when(der_supplied_date_time >= (est_date_conception-years(1))  &
                                 der_supplied_date_time <= date_end_pregnancy ~1, T~0 )) %>%
 filter(in_period == 1) 

# Aggregate so there is one set of flags for each pregnancy 
indication_med <- indication_med %>%
   select(all_upi, pregnancy_id, der_supplied_date_time, mh_med, migraine_pain_med) %>%
   group_by(all_upi, pregnancy_id) %>%
   summarise(mh_med = max(mh_med),
             migraine_pain_med = max(migraine_pain_med)) %>% 
   ungroup()


rm(preg_temp)
# join medication indication flags onto preg data by pregnancy ID
preg <- preg %>% left_join(indication_med, by = "pregnancy_id", relationship = "many-to-one")

#### 5. Tidy up data ####

# Remove extra UPI columns
# Fill in missing values in the flags with 0
preg <- preg %>% select(-upi_number, -all_upi) %>%
  mutate(epilepsy_indication = case_when(is.na(epilepsy_indication) ~0, T~epilepsy_indication),
         mh_indication = case_when(is.na(mh_indication) ~0, T~mh_indication),
         migraine_pain_indication = case_when(is.na(migraine_pain_indication) ~0, T~migraine_pain_indication),
         mh_med = case_when(is.na(mh_med) ~0, T~mh_med),
         migraine_pain_med = case_when(is.na(migraine_pain_med) ~0, T~migraine_pain_med))


# Combine the smr and med flags 
preg <- preg %>% mutate(mh_flag = case_when((mh_indication+mh_med > 0) ~1, T~0),
                         migraine_pain_flag = case_when((migraine_pain_indication+migraine_pain_med)>0 ~1, T~0)) %>%
  select(pregnancy_id, epilepsy_indication, mh_flag, migraine_pain_flag)


#names(preg)  
#### 6. Save data ####
preg %>% 
  saveRDS(paste0(data_path,'linkage/indication_flags.rds'))
  