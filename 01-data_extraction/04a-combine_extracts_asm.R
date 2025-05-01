##04a-combine_extracts_asm.R ####
# Description of content;
# Script to combine medicines datasets
# Data sets to be linked are Home Care, HEPMA, and PIS
# Script combines datasets where anti-seizure medicines are prescribed/administered


rm(list = ls())
gc()

library(tidyverse)
library(janitor)
library(arrow)
library(data.table)
source("01-data_extraction/000.file_paths.r")

# 1) Read in extracts -----------------------------------------------------

# ASM
hepma <- readRDS(paste0(folder_data_path, 'scomed_extracts/01-temp02-hepma_file_upi_filtered_asm.rds'))

hcm <- readRDS(paste0(folder_data_path, 'scomed_extracts/02-temp02-hcm_file_asm.rds'))

pis <- arrow::read_parquet(paste0(folder_data_path, "scomed_extracts/pis/pis_data_05_09.parquet"))


# 2) Combine extracts -----------------------------------------------------

compare_df_cols(hepma, hcm, pis)

hepma <- hepma %>% rename("dm_key2" = "admin_unique_id",
                          "dm_key1" = "presc_unique_id")

hepma$dm_key1 <- as.character(hepma$dm_key1)
hepma$dm_key2 <- as.character(hepma$dm_key2)

hcm <- hcm %>% rename("dm_key1" = "record_id",
                      "dm_key2" = "record_id2")

pis <- pis %>% select(-all_pat_ca_code_res_event, -all_pat_care_home_res, -all_pat_hb_code_res_event, -all_pat_hscp_code_res_event, 
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

compare_df_cols(hepma, hcm, pis)

data <- bind_rows(hepma, hcm, pis)

check <- data %>% group_by(data_source) %>% slice(1)


rm(hepma, hcm, pis)

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


# 3) Save full data file --------------------------------------------------

# ASM
saveRDS(data, paste0(folder_data_path, 'scomed_extracts/full_data_file.rds'))


arrow::write_parquet(data,
                     sink = paste0(folder_data_path, 'scomed_extracts/full_data_file_asm.parquet'),
                     compression = "zstd")


