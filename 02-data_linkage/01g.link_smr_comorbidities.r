###01g.link_smr_comorbidities.r##
# Script to link comorbidities from SMR dataset
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
preg <- readRDS(paste0(data_path,'SLiPBD_cohort_extract.rds'))
smr_all <- readRDS(paste0(data_path,'smra_extracts/smr_comorbidities.rds') )


#### 3. SMR comorbidites flags ####
# join with preg data to check dates 
# date of discharge 5 years prior to, or 14 days after pregnancy end date
preg_temp <- preg %>% select(pregnancy_id, mother_upi, est_date_conception, date_end_pregnancy)

smr_linked <- smr_all %>%
  left_join(preg_temp, by = c("upi_number" = "mother_upi"), relationship = "many-to-many") %>%
  mutate(in_period = case_when(discharge_date >= (date_end_pregnancy-years(5)) & 
                                 admission_date <= (date_end_pregnancy+14) ~1, T~0 ))


smr_linked <- smr_linked %>% filter(in_period == 1) %>% 
  select(upi_number, pregnancy_id, 
        asthma, cancer, diabetes, demyelinating_NM, 
        gi_disease, haem, immune_joint_conn,kidney, hypertension, 
        liver, pelvic_genital_dis, 
        thyroid, skin, vte) %>%
  unique()  %>%
  group_by(upi_number, pregnancy_id) %>%
  summarise( asthma = max_(asthma),
             cancer = max_(cancer), 
             diabetes= max_(diabetes),
             demyelinating_NM = max_(demyelinating_NM ),
             gi_disease = max_(gi_disease),
             haem  =max_(haem), 
             immune_joint_conn = max_(immune_joint_conn),
             kidney = max_(kidney),
             hypertension= max_(hypertension),
             liver=max_(liver), 
             pelvic_genital_dis = max_(pelvic_genital_dis), 
             thyroid = max_(thyroid),
             skin = max_(skin),
             vte = max_(vte)  ) %>% ungroup() %>%
  mutate(total_comorbs = asthma+ cancer+ diabetes +demyelinating_NM+
         gi_disease+ haem+ immune_joint_conn+kidney+ hypertension+ 
         liver+pelvic_genital_dis+   thyroid+ skin+ vte) %>%
  mutate(any_smr_comorb = case_when(total_comorbs>0 ~1, T~0))


# join smr indication flags onto preg data by pregnancy ID
preg <- preg_temp %>% left_join(smr_linked, by = "pregnancy_id")

#### 4. Tidy up data ####

# Remove extra UPI columns
# Fill in missing values in the flags with 0
preg <- preg %>% select(-upi_number) %>%
  mutate(asthma = case_when(is.na(asthma) ~ 0, T~asthma),
        cancer = case_when(is.na(cancer) ~0,T~cancer),
        diabetes = case_when(is.na(diabetes) ~0,T~  diabetes),
        demyelinating_NM = case_when(is.na(demyelinating_NM) ~0,T~ demyelinating_NM),
        gi_disease  = case_when(is.na(gi_disease) ~0,T~ gi_disease),
        haem = case_when(is.na(haem) ~0,T~ haem),
        immune_joint_conn = case_when(is.na(immune_joint_conn) ~0,T~ immune_joint_conn) ,
        kidney = case_when(is.na(kidney) ~0,T~ kidney), 
        hypertension = case_when(is.na(hypertension) ~0,T~ hypertension)  , 
        liver = case_when(is.na(liver) ~0,T~liver), 
        pelvic_genital_dis = case_when(is.na(pelvic_genital_dis) ~0,T~ pelvic_genital_dis), 
        thyroid = case_when(is.na(thyroid) ~0,T~thyroid), 
        skin = case_when(is.na(skin) ~0,T~ skin) , 
        vte = case_when(is.na(vte) ~0,T~  vte), 
        total_comorbs= case_when(is.na(total_comorbs) ~0,T~total_comorbs), 
        any_smr_comorb = case_when(is.na(any_smr_comorb) ~0,T~  any_smr_comorb) )
 #rename comorbidity flags
names(preg)[5:18] <-paste0("comorbs_",names(preg)[5:18])
    #### 6. Save data ####
preg %>% 
  saveRDS(paste0(data_path,'linkage/smr_comorbidities_flag.rds'))


old_comorbs <- readRDS(paste0(data_path,'linkage/smr_comorbidities_flag.rds'))
saveRDS(old_comorbs, paste0(data_path,'linkage/smr_comorbidities_flag_OLD.rds'))
