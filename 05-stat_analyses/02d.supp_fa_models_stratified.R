# /stat_analyses/02d.supp_fa_models_stratified.R
# Code to run the hdFA supplementary models stratified on hdFA status
# Run models for only those with a significant interaction term in the
# full adjusted model with hdFA and exposure:hdFA created in 
# /stat_analyses/supp_fa_models.r



# 1) Housekeeping ---------------------------------------------------------

rm(list = ls())
gc()

library(dplyr)
library(tidyverse)
library(survival)
library(gtsummary)


# Custom function to format p-values
custom_pvalue_fun <- function(x) {
  ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3))
}

#source filepaths
source("05-stat_analysis/00.stat_setup.r")

# 2) Read in data ---------------------------------------------------------

p_full <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_preg_folic_data.parquet"))


dev_full <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_dev_folic_data.parquet"))

# 3) Pregnancy cohort - adjusted models ----------------------------------

# With Folic Acid
p_fa <- p_full %>% 
  filter(high_dose_folic_acid == 1)

# No folic acid
p_no_fa <- p_full %>% 
  filter(high_dose_folic_acid == 0)

# Any ASM -----------------------------------------------------------------

# With folic acid
p_fa_any <- clogit(any_loss ~
                      exposed_any_asm +
                      epilepsy_indication + mh_flag + migraine_pain_flag +
                      maternal_age_group_conception + mother_simd + drug_alcohol_use +
                      any_smr_comorb +
                      strata(groupID),
                    data = p_fa)

p_fa_any_summary <- tbl_regression(p_fa_any, exponentiate = TRUE, 
                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_any <- clogit(any_loss ~
                         exposed_any_asm +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = p_no_fa)

p_no_fa_any_summary <- tbl_regression(p_no_fa_any, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')


# Crude (Unadjusted)
# With folic acid
p_fa_any_crude <- clogit(any_loss ~
                     exposed_any_asm +
                     strata(groupID),
                   data = p_fa)

p_fa_any_summary_crude <- tbl_regression(p_fa_any_crude, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_any_crude <- clogit(any_loss ~
                        exposed_any_asm +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_any_summary_crude <- tbl_regression(p_no_fa_any_crude, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')

# Carbamazepine -----------------------------------------------------------

# With folic acid
p_fa_car <- clogit(any_loss ~
                     exposed_carbamazepine_mono +
                     epilepsy_indication + mh_flag + migraine_pain_flag +
                     maternal_age_group_conception + mother_simd + drug_alcohol_use +
                     any_smr_comorb +
                     strata(groupID),
                   data = p_fa)

p_fa_car_summary <- tbl_regression(p_fa_car, exponentiate = TRUE) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_car <- clogit(any_loss ~
                        exposed_carbamazepine_mono +
                        epilepsy_indication + mh_flag + migraine_pain_flag +
                        maternal_age_group_conception + mother_simd + drug_alcohol_use +
                        any_smr_comorb +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_car_summary <- tbl_regression(p_no_fa_car, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')


# Crude (Unadjusted)
# With folic acid
p_fa_car_crude <- clogit(any_loss ~
                     exposed_carbamazepine_mono +
                     strata(groupID),
                   data = p_fa)

p_fa_car_summary_crude <- tbl_regression(p_fa_car_crude, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_car_crude <- clogit(any_loss ~
                        exposed_carbamazepine_mono +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_car_summary_crude <- tbl_regression(p_no_fa_car_crude, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')


# Lamotrigine -----------------------------------------------------------

# With folic acid
p_fa_lam <- clogit(any_loss ~
                     exposed_lamotrigine_mono +
                     epilepsy_indication + mh_flag + migraine_pain_flag +
                     maternal_age_group_conception + mother_simd + drug_alcohol_use +
                     any_smr_comorb +
                     strata(groupID),
                   data = p_fa)

p_fa_lam_summary <- tbl_regression(p_fa_lam, exponentiate = TRUE, 
                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_lam <- clogit(any_loss ~
                        exposed_lamotrigine_mono +
                        epilepsy_indication + mh_flag + migraine_pain_flag +
                        maternal_age_group_conception + mother_simd + drug_alcohol_use +
                        any_smr_comorb +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_lam_summary <- tbl_regression(p_no_fa_lam, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')



# Crude (unadjusted)
# With folic acid
p_fa_lam_crude <- clogit(any_loss ~
                     exposed_lamotrigine_mono +
                     strata(groupID),
                   data = p_fa)

p_fa_lam_summary_crude <- tbl_regression(p_fa_lam_crude, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_lam_crude <- clogit(any_loss ~
                        exposed_lamotrigine_mono +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_lam_summary_crude <- tbl_regression(p_no_fa_lam_crude, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')

# Levetiracetam -----------------------------------------------------------

# With folic acid
p_fa_lev <- clogit(any_loss ~
                     exposed_levetiracetam_mono +
                     epilepsy_indication + mh_flag + migraine_pain_flag +
                     maternal_age_group_conception + mother_simd + drug_alcohol_use +
                     any_smr_comorb +
                     strata(groupID),
                   data = p_fa)

p_fa_lev_summary <- tbl_regression(p_fa_lev, exponentiate = TRUE, 
                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_lev <- clogit(any_loss ~
                        exposed_levetiracetam_mono +
                        epilepsy_indication + mh_flag + migraine_pain_flag +
                        maternal_age_group_conception + mother_simd + drug_alcohol_use +
                        any_smr_comorb +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_lev_summary <- tbl_regression(p_no_fa_lev, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')


# Crude (unadjusted)
# With folic acid
p_fa_lev_crude <- clogit(any_loss ~
                     exposed_levetiracetam_mono +
                     strata(groupID),
                   data = p_fa)

p_fa_lev_summary_crude <- tbl_regression(p_fa_lev_crude, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_lev_crude <- clogit(any_loss ~
                        exposed_levetiracetam_mono +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_lev_summary_crude <- tbl_regression(p_no_fa_lev_crude, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')

# Gabapentin -----------------------------------------------------------

# With folic acid
p_fa_gab <- clogit(any_loss ~
                     exposed_gabapentin_mono +
                     epilepsy_indication + mh_flag + migraine_pain_flag +
                     maternal_age_group_conception + mother_simd + drug_alcohol_use +
                     any_smr_comorb +
                     strata(groupID),
                   data = p_fa)

p_fa_gab_summary <- tbl_regression(p_fa_gab, exponentiate = TRUE, 
                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_gab <- clogit(any_loss ~
                        exposed_gabapentin_mono +
                        epilepsy_indication + mh_flag + migraine_pain_flag +
                        maternal_age_group_conception + mother_simd + drug_alcohol_use +
                        any_smr_comorb +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_gab_summary <- tbl_regression(p_no_fa_gab, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')



# Crude (unadjusted)
# With folic acid
p_fa_gab_crude <- clogit(any_loss ~
                     exposed_gabapentin_mono +
                     strata(groupID),
                   data = p_fa)

p_fa_gab_summary_crude <- tbl_regression(p_fa_gab_crude, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
p_no_fa_gab_crude <- clogit(any_loss ~
                        exposed_gabapentin_mono +
                        strata(groupID),
                      data = p_no_fa)

p_no_fa_gab_summary_crude <- tbl_regression(p_no_fa_gab_crude, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')


p_full_models <- bind_rows(p_fa_any_summary, p_no_fa_any_summary, p_fa_car_summary, p_no_fa_car_summary,
                           p_fa_lam_summary, p_no_fa_lam_summary, p_fa_lev_summary, p_no_fa_lev_summary,
                           p_fa_gab_summary, p_no_fa_gab_summary)

write_csv(p_full_models,paste0(folder_data_path, "stats/p_stratified.csv"))


# Crude (unadjusted)
p_full_models_crude <- bind_rows(p_fa_any_summary_crude, p_no_fa_any_summary_crude, p_fa_car_summary_crude, 
                                 p_no_fa_car_summary_crude,
                           p_fa_lam_summary_crude, p_no_fa_lam_summary_crude, p_fa_lev_summary_crude, 
                           p_no_fa_lev_summary_crude,
                           p_fa_gab_summary_crude, p_no_fa_gab_summary_crude)

write_csv(p_full_models_crude,paste0(folder_data_path, "stats/p_stratified_crude.csv"))


# 5) Developmental cohort - adjusted models ----------------------------------

# With Folic Acid
dev_fa <-dev_full %>% 
  filter(high_dose_folic_acid == 1)

# No folic acid
dev_no_fa <- dev_full %>% 
  filter(high_dose_folic_acid == 0)

# Any ASM -----------------------------------------------------------------

# With folic acid
dev_fa_any <- clogit(any_dev_concern ~
                     exposed_any_asm +
                     epilepsy_indication + mh_flag + migraine_pain_flag +
                     maternal_age_group_conception + mother_simd + drug_alcohol_use +
                     any_smr_comorb +
                     strata(groupID),
                   data = dev_fa)

dev_fa_any_summary <- tbl_regression(dev_fa_any, exponentiate = TRUE, 
                                     pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
dev_no_fa_any <- clogit(any_dev_concern ~
                        exposed_any_asm +
                        epilepsy_indication + mh_flag + migraine_pain_flag +
                        maternal_age_group_conception + mother_simd + drug_alcohol_use +
                        any_smr_comorb +
                        strata(groupID),
                      data = dev_no_fa)

dev_no_fa_any_summary <- tbl_regression(dev_no_fa_any, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')



# Crude (unadjusted)
# With folic acid
dev_fa_any_crude <- clogit(any_dev_concern ~
                       exposed_any_asm +
                       strata(groupID),
                     data = dev_fa)

dev_fa_any_summary_crude <- tbl_regression(dev_fa_any_crude, exponentiate = TRUE, 
                                           pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
dev_no_fa_any_crude <- clogit(any_dev_concern ~
                          exposed_any_asm +
                          strata(groupID),
                        data = dev_no_fa)

dev_no_fa_any_summary_crude <- tbl_regression(dev_no_fa_any_crude, exponentiate = TRUE, 
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')

# carbamazepine -----------------------------------------------------------------

# With folic acid
dev_fa_car <- clogit(any_dev_concern ~
                      exposed_carbamazepine_mono +
                      epilepsy_indication + mh_flag + migraine_pain_flag +
                      maternal_age_group_conception + mother_simd + drug_alcohol_use +
                      any_smr_comorb +
                      strata(groupID),
                    data = dev_fa)

dev_fa_car_summary <- tbl_regression(dev_fa_car, exponentiate = TRUE, 
                                     pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
dev_no_fa_car <- clogit(any_dev_concern ~
                         exposed_carbamazepine_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = dev_no_fa)

dev_no_fa_car_summary <- tbl_regression(dev_no_fa_car, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')


# Crude (unadjusted)
# With folic acid
dev_fa_car_crude <- clogit(any_dev_concern ~
                       exposed_carbamazepine_mono +
                       strata(groupID),
                     data = dev_fa)

dev_fa_car_summary_crude <- tbl_regression(dev_fa_car_crude, exponentiate = TRUE, 
                                           pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'Yes')


# No folic acid
dev_no_fa_car_crude <- clogit(any_dev_concern ~
                          exposed_carbamazepine_mono +
                          strata(groupID),
                        data = dev_no_fa)

dev_no_fa_car_summary_crude <- tbl_regression(dev_no_fa_car_crude, exponentiate = TRUE, 
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci) %>% 
  mutate(FA = 'No')


dev_full_models <- bind_rows(dev_fa_any_summary, dev_no_fa_any_summary, dev_fa_car_summary, dev_no_fa_car_summary)

write_csv(dev_full_models,paste0(folder_data_path, "stats/dev_stratified.csv"))


dev_full_models_crude <- bind_rows(dev_fa_any_summary_crude, dev_no_fa_any_summary_crude, 
                                   dev_fa_car_summary_crude, dev_no_fa_car_summary_crude)

write_csv(dev_full_models_crude, paste0(folder_data_path, "stats/dev_stratified_crude.csv"))
