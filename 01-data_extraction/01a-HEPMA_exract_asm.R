##01a-HEPMA_extract_asm.R ####
# Description of content;
# Script to extract HEPMA data for medicines exposure dataset
# Data sets to be linked are Home Care, HEPMA, and PIS
# Script extracts HEPMA records where anti-seizure medicines are prescribed/administered

rm(list = ls())

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

source("01-data_extraction/000.file_paths.r")
source("01-data_extraction/000.extracts.r")
### Before running update file paths in lines 89, 313 & 317



# 1) Extract HEPMA data from database -------------------------------------

# HEPMA extract
# Relevant filters:
# Time period: 
# Gender: 
# BNF section: 
# Adapt the code below to extract the data required;
# VS - Added in UPI and CHI just incase, also added in formulation for use in DDD calculations, removed filter for sex
hepma_extract <- hepma_extract_asm  %>%
                    mutate(UPI_validity = chi_check(patient_chi_number)) %>%
                    # Drop records without a valid UPI
                    filter(!is.na(patient_chi_number) & UPI_validity == "Valid CHI") %>%
                    select(-UPI_validity)

 
# save a copy to ensure don't need to run a database query again
saveRDS(hepma_extract, paste0(folder_data_path,  'scomed_extracts/01-temp01-hepma_extract_asm.rds'))

# 2) Read in raw HEPMA extract --------------------------------------------

# Read in hepma extract
hepma <- hepma_extract
rm(hepma_extract)

# 3) source and run HEPMA processing --------------------------------------------

source("01-data_extraction/01-HEPMA_processing.R")

# Save final extract

hepma %>% 
   saveRDS( paste0(folder_data_path,'scomed_extracts/01-temp02-hepma_file_upi_filtered_asm.rds'))

hepma %>% 
  distinct(all_chi) %>% 
  summarise(n())


