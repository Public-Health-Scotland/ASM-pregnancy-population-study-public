# /stat_analyses/02c. the matched cohorts files for input into models for hdFA supplementary analyses
# Matched cohorts created in /control_matching/Matching_controls_FA_strata.r


# 1) Housekeeping ---------------------------------------------------------

rm(list = ls())
gc()

library(dplyr)
library(tidyverse)
library(arrow)
#source filepaths
source("05-stat_analysis/00.stat_setup.r")


# 2) Read in data ---------------------------------------------------------

# Read in matched cohorts for pregnancy outcomes
p_full <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_preg_outcomes_folic_cohort.rds"))


# Read in matched cohorts for congenital conditions
cc_full <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_congenital_folic_cohort.rds"))


# Read in matched cohorts for developmental outcomes
dev_full <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_developmental_folic_cohort.rds"))



# 3) Format data files ----------------------------------------------------
# 3.1) Pregnancy outcomes cohort ------------------------------------------
# 3.1.1) Full -------------------------------------------------------------

# check the outcome format (should be binary 0/1)
table(p_full$pregnancy_loss)

# clogit() expects a 0/1 outcome
# Convert from No/Yes to 0/1
p_full <- p_full %>% 
  mutate(any_loss = as.numeric(pregnancy_loss == "Yes"))

# Check derivation
table(p_full$pregnancy_loss, p_full$any_loss, useNA = "ifany")

# Check distrubutions of matching variables for cases and controls
# actual matching variable is groupID but this is made from gest_first_exposed 
# and year_conception.
boxplot(gest_first_exposed ~ any_loss,
        data = p_full)

boxplot(year_conception ~ any_loss,
        data = p_full)

# Fix age group names/simd to remove arithmatic operators and NA's
p_full %>% 
  group_by(maternal_age_group_conception, mother_simd) %>% 
  summarise(n()) %>% 
  print(n = 40)

p_full <- p_full %>% 
  mutate(maternal_age_group_conception = case_when(maternal_age_group_conception == '<20' ~ 'less than 20',
                                                   maternal_age_group_conception == '20-24' ~ '20 to 24',
                                                   maternal_age_group_conception == '25-29' ~ '25 to 29',
                                                   maternal_age_group_conception == '30-34' ~ '30 to 34',
                                                   maternal_age_group_conception == '35-39' ~ '35 to 39',
                                                   maternal_age_group_conception == '40+' ~ '40 plus',
                                                   T ~ maternal_age_group_conception),
         mother_simd = case_when(is.na(mother_simd) ~ 'Unknown',
                                 T ~ mother_simd))


# 3.2) Congenital conditions cohort ---------------------------------------
# 3.2.1) Full --------------------------------------

# check the outcome format (should be binary 0/1)
table(cc_full$any_CC)

# Check distrubutions of matching variables for cases and controls
# actual matching variable is groupID but this is made from gest_first_exposed 
# and year_conception.
boxplot(gest_first_exposed ~ any_CC,
        data = cc_full)

boxplot(year_conception ~ any_CC,
        data = cc_full)


# Fix age group names / simd to remove arithmatic operators
cc_full %>% 
  group_by(maternal_age_group_conception, mother_simd) %>% 
  summarise(n()) %>% 
  print(n = 40)

cc_full <- cc_full %>% 
  mutate(maternal_age_group_conception_collapsed = case_when(maternal_age_group_conception %in% c('<20','20-24') ~ 'less than 25',
                                                             maternal_age_group_conception %in% c('25-29','30-34') ~ '25 to 34',
                                                             maternal_age_group_conception %in% c('35-39','40+') ~ '35 plus',
                                                             T ~ maternal_age_group_conception),
         maternal_age_group_conception = case_when(maternal_age_group_conception == '<20' ~ 'less than 20',
                                                   maternal_age_group_conception == '20-24' ~ '20 to 24',
                                                   maternal_age_group_conception == '25-29' ~ '25 to 29',
                                                   maternal_age_group_conception == '30-34' ~ '30 to 34',
                                                   maternal_age_group_conception == '35-39' ~ '35 to 39',
                                                   maternal_age_group_conception == '40+' ~ '40 plus',
                                                   T ~ maternal_age_group_conception),
         mother_simd = case_when(is.na(mother_simd) ~ 'Unknown',
                                 T ~ mother_simd),
         mother_simd_collapsed = case_when(mother_simd %in% c('1 (most deprived)','2') ~ '1 to 2',
                                           mother_simd %in% c('3', '4', '5 (least deprived)') ~ '3 to 5',
                                           T ~ mother_simd)) 

# 3.3) Developmental cohort -----------------------------------------------
# 3.3.1) Full -------------------------------------------------------------

# check the outcome format (should be binary 0/1)
table(dev_full$any_dev_excl_v_h)

# clogit() expects a 0/1 outcome
# Convert from No/Yes to 0/1
dev_full <- dev_full %>% 
  mutate(any_dev_concern = as.numeric(any_dev_excl_v_h == "Y"))

# Check derivation
table(dev_full$any_dev_excl_v_h, dev_full$any_dev_concern, useNA = "ifany")

# Check distrubutions of matching variables for cases and controls
# actual matching variable is groupID but this is made from gest_first_exposed 
# and year_conception.
boxplot(gest_first_exposed ~ any_dev_concern,
        data = dev_full)

boxplot(year_conception ~ any_dev_concern,
        data = dev_full)

# Fix covariates to remove arithmatic operators
dev_full %>% 
  group_by(maternal_age_group_conception, mother_simd, maternal_smoking) %>% 
  summarise(n()) %>% 
  print(n = 125)

dev_full <- dev_full %>% 
  mutate(maternal_age_group_conception = case_when(maternal_age_group_conception == '<20' ~ 'less than 20',
                                                   maternal_age_group_conception == '20-24' ~ '20 to 24',
                                                   maternal_age_group_conception == '25-29' ~ '25 to 29',
                                                   maternal_age_group_conception == '30-34' ~ '30 to 34',
                                                   maternal_age_group_conception == '35-39' ~ '35 to 39',
                                                   maternal_age_group_conception == '40+' ~ '40 plus',
                                                   T ~ maternal_age_group_conception),
         mother_simd = case_when(is.na(mother_simd) ~ 'Unknown',
                                 T ~ mother_simd),
         maternal_smoking = case_when(maternal_smoking == 'ex-smoker' ~ 'ex smoker',
                                      maternal_smoking == 'non-smoker' ~ 'non smoker',
                                      T ~ maternal_smoking),
         parity = case_when(n_prev_deliveries == 0 ~ '0',
                            n_prev_deliveries >= 1 ~ '1+',
                            is.na(n_prev_deliveries) ~ 'Unknown'))

dev_full %>% 
  group_by(n_prev_deliveries, parity) %>% 
  summarise(n())

# 4) Save out files for models --------------------------------------------

arrow::write_parquet(p_full,
                     sink = paste0(folder_data_path, "stats/temp_preg_folic_data.parquet"),
                     compression = "zstd")


arrow::write_parquet(cc_full,
                     sink = paste0(folder_data_path, "stats/temp_cc_folic_data.parquet"),
                     compression = "zstd")

arrow::write_parquet(dev_full,
                     sink = paste0(folder_data_path, "stats/temp_dev_folic_data.parquet"),
                     compression = "zstd")
