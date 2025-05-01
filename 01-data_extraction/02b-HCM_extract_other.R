# Name of file - 02-HCM_extract.R ####
# Original author(s) - Morven Millar
# Original date - 12/09/2023
##01b-HCM_extract_other.R ####
# Description of content;
# Script to extract HCM data for medicines exposure dataset
# Data sets to be linked are Home Care, HEPMA, and PIS
# Script extracts HCM records where mental health, pain and folic acid medicines are prescribed/administered



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


# 1) Extract HCM data from database -------------------------------------
source("01-data_extraction/000.extracts.r")
# HCM extract
# Relevant filters:
# Time period: 
# Gender: 
# BNF section: 
# Adapt the code below to extract the data required;
# Mental Health and Pain &  folic acid 
#hmc extract which has been sourcd from file paths script
hcm_extract <- hcm_extract %>%
  mutate(UPI_validity = chi_check(patient_chi_number)) %>%
  # Drop records without a valid UPI
  filter(!is.na(patient_chi_number) & UPI_validity == "Valid CHI") %>%
  select(-UPI_validity)


# 2) source and run HCM processing --------------------------------------------

source("Data_extraction/02-HCM_processing.R")


# Save as is just now
hcm %>% 
  saveRDS(paste0(folder_data_path, 'scomed_extracts/02-temp02-hcm_file_other.rds'))




