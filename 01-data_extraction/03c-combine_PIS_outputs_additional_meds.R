##03c-combine_PIS_outputs_additional_meds.R ####
# Description of content;
# Script to prepare PIS data for medicines exposure dataset
# Data sets to be linked are Home Care, HEPMA, and PIS
# Script prepares PIS records where mental health and pain are prescribed/administered
# Script brings in additional mental health and pain meds


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

source("01-data_extraction/000.file_paths.r")
source("01-data_extraction/000.extracts.r")
#### 2. Read in and check PIS data - ASM ####

#### 2. Read in and check PIS data - MH & pain ####
# Adding additional meds for MH and pain flags
pis_data_1 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2010_full.xlsx'), col_names = TRUE)
pis_data_2 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2011_full.xlsx'), col_names = TRUE)
pis_data_3 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2012_full.xlsx'), col_names = TRUE)
pis_data_4 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2013_full.xlsx'), col_names = TRUE)
pis_data_5 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2014_full.xlsx'), col_names = TRUE)
pis_data_6 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2015_full.xlsx'), col_names = TRUE)
pis_data_7 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2016_full.xlsx'), col_names = TRUE)
pis_data_8 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2017_full.xlsx'), col_names = TRUE)
pis_data_9 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2018_full.xlsx'), col_names = TRUE)
pis_data_10 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2019_full.xlsx'), col_names = TRUE)
pis_data_11 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2020_full.xlsx'), col_names = TRUE)
pis_data_12 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2021_01_06.xlsx'), col_names = TRUE)
pis_data_13 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2021_07_12.xlsx'), col_names = TRUE)
pis_data_14 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2022_01_06.xlsx'), col_names = TRUE)
pis_data_15 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2022_07_12.xlsx'), col_names = TRUE)
pis_data_16 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2023_01_06.xlsx'), col_names = TRUE)
pis_data_17 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2023_07_12.xlsx'), col_names = TRUE)
pis_data_18 <- read_xlsx(paste0(folder_data_path, 'scomed_extracts/20241212_additional_meds/additional_meds_2024_01_03.xlsx'), col_names = TRUE)



# Join all of the above
pis_data <- bind_rows(pis_data_1,
                      pis_data_2,
                      pis_data_3,
                      pis_data_4,
                      pis_data_5,
                      pis_data_6,
                      pis_data_7,
                      pis_data_8,
                      pis_data_9,
                      pis_data_10,
                      pis_data_11,
                      pis_data_12,
                      pis_data_13,
                      pis_data_14,
                      pis_data_15,
                      pis_data_16,
                      pis_data_17,
                      pis_data_18
)


rm(pis_data_1,
   pis_data_2,
   pis_data_3,
   pis_data_4,
   pis_data_5,
   pis_data_6,
   pis_data_7,
   pis_data_8,
   pis_data_9,
   pis_data_10,
   pis_data_11,
   pis_data_12,
   pis_data_13,
   pis_data_14,
   pis_data_15,
   pis_data_16,
   pis_data_17,
   pis_data_18)


# Check class of each variable
class <- lapply(pis_data, class)


# Rename columns
col_names <- c('all_upi',
               'all_chi',
               'all_dob',
               'all_sex',
               'all_sex_desc',
               'all_dod',
               'all_pat_postcode_event',
               'all_pat_care_home_res',
               'der_presc_date_time',
               'der_disp_date_time',
               'claim_service_flag',
               'claim_dcvp_electronic_flag',
               'item_not_collected',
               'item_not_dispensed',
               'instal_disp_flag',
               'bnf_chapter_code',
               'bnf_chapter_descr',
               'bnf_section_code',
               'bnf_section_descr',
               'bnf_sub_section_code',
               'bnf_sub_section_descr',
               'bnf_paragraph_code',
               'bnf_paragraph_descr',
               'prescribed_product_code_current',
               'prescribed_product_code_event',
               'vtm_name',
               'vmp_name',
               'dummy_drug_descr',
               'atc_code',
               'atc_description',
               'disp_bnf_chapter_code', 
               'disp_bnf_chapter_descr',
               'disp_bnf_section_code',
               'disp_bnf_section_descr',
               'disp_bnf_sub_section_code',
               'disp_bnf_sub_section_descr',
               'disp_bnf_paragraph_code',
               'disp_bnf_paragraph_descr',
               'dispensed_product_code_current',
               'dispensed_product_code_event',
               'disp_vtm_name',
               'disp_vmp_name',
               'disp_dummy_drug_descr',
               'disp_atc_code',
               'disp_atc_description',
               'presc_quantity',
               'disp_quantity',
               'presc_formulation',
               'strength',
               'strength_uom',
               'strength_per',
               'strength_per_uom',
               'disp_formulation',
               'disp_strength',
               'disp_strength_uom',
               'disp_strength_per',
               'disp_strength_per_uom',
               'ddd_tvpm',
               'disp_ddd_tvpm',
               'prescribing_hb',
               'form_type',
               'form_type_desc',
               'claim_image_ref',
               'form_barcode',
               'prescription_line_no',
               'gic_excl_bb',
               'gic_incl_bb',
               'form_scan_ref_no',
               'claim_id',
               'primary_presc_item_flag',
               'primary_disp_item_flag'   
               
)

colnames(pis_data) <- col_names


# Save combined data - MH, pain and folic acid
arrow::write_parquet(pis_data,
                     sink = paste0(folder_data_path, "scomed_extracts/20241212_additional_meds/additional_meds_18_12.parquet"),
                     compression = "zstd")


pis_data <- arrow::read_parquet(paste0(folder_data_path, "scomed_extracts/20241212_additional_meds/additional_meds_18_12.parquet"))


##### 2.1 Check number of patients and records #####
# Check the number of patients & records for each patient
## ASM- 56,829 patients over all, 1,600,683 records
## MH, pain & folic acid - 268,143 patients over all, 2,427,647 records
## Additional MH & pain - 319,675 patients, 10,432,083 records
pats <- pis_data %>% select(all_chi) %>% group_by(all_chi) %>% summarise(records = n()) 

pats_upi <- pis_data %>% select(all_chi) %>% group_by(all_chi) %>% summarise(records = n()) 

# UPI count matches CHI count

##### 2.2 Date checks #####
## Check by quarter for comparison with previous analysis
pats_qrt <- pis_data %>% mutate(quarter = quarter(der_disp_date_time, type = "quarter", fiscal_start = 1, with_year = TRUE)) %>%
  select(all_chi, quarter) %>% group_by(quarter, all_chi) %>% summarise(records = n()) %>% group_by(quarter) %>% 
  summarise(patients = n(), records = sum(records))

# Note: Can edit the limits in the below to change where the y axis starts
pats_qrt %>% ggplot(aes(x=quarter, y=patients)) + geom_line() + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))

pats_qrt %>% ggplot(aes(x=quarter, y=records)) + geom_line() + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


pats_drugs_qrt <- pis_data %>% mutate(quarter = quarter(der_disp_date_time, type = "quarter", fiscal_start = 1, with_year = TRUE)) %>%
  select(all_chi, quarter, vtm_name) %>% funique() %>% group_by(quarter, vtm_name) %>% summarise(patients = n()) %>% ungroup()


pats_drugs_wide <- pats_drugs_qrt %>% pivot_wider(names_from = quarter, values_from = patients)

# Check some vtms below
filter(pats_drugs_qrt, vtm_name %in% c("SODIUM VALPROATE", "TOPIRAMATE", "LAMOTRIGINE", "LEVETIRACETAM")) %>%  ggplot(aes(x=quarter, y=patients)) +
  geom_line(aes(color=vtm_name)) +  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))


##### 2.3 Check how well populated various flags are #####
# Check how well populated the 'Claim installment dispensing endorsement flag' is

instal_flag <- pis_data %>% select(claim_image_ref, prescription_line_no, instal_disp_flag) %>%
  mutate(blank = case_when(is.na(instal_disp_flag) ~ 'Y',
                           .default = 'N')) %>%
  group_by(instal_disp_flag)  %>%  
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(percentage = count/sum(count)*100)

##### 3. Bring in and run PIS processing script #####


source("01-data_extraction/03-PIS_processing.R")


##### 4. Save output #####

arrow::write_parquet(pis_data_final,
                     sink = paste0(folder_data_path, "scomed_extracts/20241212_additional_meds/additional_meds_18_12.parquet"),
                     compression = "zstd")
