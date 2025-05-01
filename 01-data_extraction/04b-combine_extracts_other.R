##04b-combine_extracts_asm.R ####
# Description of content;
# Script to combine medicines datasets
# Data sets to be linked are Home Care, HEPMA, and PIS
# Script combines datasets where mental health, pain, and folic acid medicines are prescribed/administered


rm(list = ls())
gc()

library(tidyverse)
library(janitor)
library(arrow)
library(data.table)
source("01-data_extraction/000.file_paths.r")

# 1) Read in extracts -----------------------------------------------------

# other meds
hepma <- readRDS(paste0(folder_data_path, 'scomed_extracts/01-temp02-hepma_file_upi_filtered_other.rds'))

hcm <- readRDS(paste0(folder_data_path, 'scomed_extracts/02-temp02-hcm_file_other.rds'))

pis1 <- arrow::read_parquet(paste0(folder_data_path, "scomed_extracts/other_meds/other_data_05_09.parquet"))

pis2 <- arrow::read_parquet(paste0(folder_data_path, "scomed_extracts/20241212_additional_meds/additional_meds_18_12.parquet"))

# 2) Combine extracts -----------------------------------------------------

compare_df_cols(hepma, hcm, pis1, pis2)

hepma <- hepma %>% rename("dm_key2" = "admin_unique_id",
                          "dm_key1" = "presc_unique_id")

hepma$dm_key1 <- as.character(hepma$dm_key1)
hepma$dm_key2 <- as.character(hepma$dm_key2)

hcm <- hcm %>% rename("dm_key1" = "record_id",
                      "dm_key2" = "record_id2")

pis1 <- pis1 %>% select(-all_pat_ca_code_res_event, -all_pat_care_home_res, -all_pat_hb_code_res_event, -all_pat_hscp_code_res_event, 
                      -all_pat_urb_rural_code_res_event,  -dmd_code_current) %>% 
  rename("dm_key1" = "claim_image_ref",
         "dm_key2" = "form_barcode",
         "dm_key3" = "prescription_line_no",
         "atc_group_code" = "atc_code",
         "atc_group_descr" = "atc_description",
         "der_presc_date_time" = "presc_date_time",
         "der_supplied_date_time" = "supplied_date_time",
         "disp_quantity" = "supp_quantity",
         "med_formulation" = "presc_formulation"
  )

pis2 <- pis2 %>% select(-all_pat_ca_code_res_event, -all_pat_care_home_res, -all_pat_hb_code_res_event, -all_pat_hscp_code_res_event, 
                      -all_pat_urb_rural_code_res_event,  -dmd_code_current) %>% 
  rename("dm_key1" = "claim_image_ref",
         "dm_key2" = "form_barcode",
         "dm_key3" = "prescription_line_no",
         "atc_group_code" = "atc_code",
         "atc_group_descr" = "atc_description",
         "der_presc_date_time" = "presc_date_time",
         "der_supplied_date_time" = "supplied_date_time",
         "disp_quantity" = "supp_quantity",
         "med_formulation" = "presc_formulation"
  )


compare_df_cols(hepma, hcm, pis1, pis2)

data <- bind_rows(hepma, hcm, pis1, pis2)

check <- data %>% group_by(data_source) %>% slice(1)


rm(hepma, hcm, pis1, pis2)

# remove any not supplied and remove dose instruction column since not populated for all sources
# check admin not supplied reasons 
not_given_reason <- data %>% group_by(admin_reason_not_supplied) %>% summarise(count = n()) %>% ungroup()

# admin not given reasons that imply patient self administered or patient on leave should be classed as given and counted in the data
# flag these for inclusion 
data <- data %>% select(-dose_instruction) %>% 
  mutate(include_admin = case_when(admin_reason_not_supplied %in% c("self administers",
                                                                    "self administered",
                                                                    "patient on short-term pass",
                                                                    "patient on short term leave",
                                                                    "patient self administered",
                                                                    "patient on pass",
                                                                    "self administered -day surgery only",
                                                                    "given prior to admission to ward") ~ "y",
                                   is.na(admin_reason_not_supplied) ~ "y", T ~ "n"))


data <- data %>% filter(presc_not_supplied == "n" & include_admin == "y") %>% select(-include_admin)

# bring in ASM file and check that columns match

data_asm <- arrow::read_parquet(paste0(folder_data_path, 'scomed_extracts/full_data_file_asm.parquet'))

compare_df_cols(data, data_asm)

# disp_dummy_drug_descr logical in this but char in ASM. Change to char

data$disp_dummy_drug_descr <- as.character(data$disp_dummy_drug_descr)

compare_df_cols(data, data_asm)

# 3) Save full data file --------------------------------------------------

##### look at folic acid VMPs

folic <- data %>% filter(vtm_name == "folic acid")

folic_vmp <- folic %>% group_by(vmp_name) %>% summarise(n()) %>% ungroup()

# From protocol should only include: folic acid 5mg tablets, folic acid 5mg/5ml or 2.5mg/5ml solution
# easier just to remove the VMPs that we don't want. Remove the following:
# folic acid 400microgram tablets & folic acid 400micrograms/5ml oral solution sugar free
# should be 617,175 cases of folic acid remaining

data <- data %>% filter(!(vmp_name %like% "^folic acid 400"))

#check
check <- data %>% group_by(vmp_name) %>% summarise(n()) %>% ungroup


# Mental health, pain and folic acid
arrow::write_parquet(data,
                     sink = paste0(folder_data_path, 'scomed_extracts/full_data_file_other_meds.parquet'),
                     compression = "zstd")


