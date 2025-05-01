# /stat_analyses/02a.supp_fa_models.R
# Code to run the hdFA supplementary models
# these include (for each cohort);
# Any ASM;
#   Exposure only
#   hdFA only
#   Exposure + hdFA only
#   Exposure + hdFA + exposure:hdFA
#   Full adjusted model with hdFA and exposure:hdFA
# For all other ASM monotherapies
#   Exposure + hdFA only
#   Full adjusted model with hdFA and exposure:hdFA

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

p_full <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_preg_data.parquet"))

cc_full <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_cc_data.parquet"))

dev_full <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_dev_data.parquet"))

# 3) Pregnancy cohort - models -----------------------------------

# Any ASM -----------------------------------------------------------------

#### Exposure only
p_exp_only <- clogit(any_loss ~ 
                       exposed_any_asm + 
                       strata(groupID),
                     data = p_full)

#### FA only
p_fa_only <- clogit(any_loss ~
                      high_dose_folic_acid +
                      strata(groupID),
                    data = p_full)


### Exposure + Folic acid 
p_exp_fa_any <- clogit(any_loss ~
                  exposed_any_asm +
                  high_dose_folic_acid +
                  strata(groupID),
                data = p_full)

### Exposure + Folic acid + Interaction
p_exp_fa_inter_any <- clogit(any_loss ~
                         exposed_any_asm +
                         high_dose_folic_acid +
                         exposed_any_asm:high_dose_folic_acid +
                         strata(groupID),
                       data = p_full)

### full model w/ Folic acid interaction
p_inter_any <- clogit(any_loss ~
                      exposed_any_asm +
                      epilepsy_indication + mh_flag + migraine_pain_flag +
                      maternal_age_group_conception + mother_simd + drug_alcohol_use +
                      any_smr_comorb +
                        high_dose_folic_acid +
                      exposed_any_asm:high_dose_folic_acid +
                      strata(groupID),
                    data = p_full)


# generate summary tables
## Exposure only
p_exp_only_summary <- tbl_regression(p_exp_only, exponentiate = TRUE, 
                                     pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(p_exp_only_summary, paste0(folder_data_path, "stats/p_ASM_only.csv"))


## FA only
p_fa_only_summary <- tbl_regression(p_fa_only, exponentiate = TRUE, 
                                    pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(p_fa_only_summary, paste0(folder_data_path, "stats/p_hdFA_only.csv"))


### Exposure + Folic acid
p_exp_fa_any_summary <- tbl_regression(p_exp_fa_any, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(p_exp_fa_any_summary, paste0(folder_data_path, "stats/p_ASM_hdFA.csv"))


## Exposure + FA + interaction
p_exp_fa_inter_any_summary <- tbl_regression(p_exp_fa_inter_any, exponentiate = TRUE, 
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(p_exp_fa_inter_any_summary, paste0(folder_data_path, "stats/p_ASM_hdFA_interaction.csv"))


### full model w/ Folic acid interaction
p_inter_any_summary <- tbl_regression(p_inter_any, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

# old full model
# p_any <- clogit(any_loss ~
#                       exposed_any_asm +
#                       high_dose_folic_acid +
#                       epilepsy_indication + mh_flag + migraine_pain_flag +
#                       maternal_age_group_conception + mother_simd + drug_alcohol_use +
#                       any_smr_comorb +
#                       exposed_any_asm:high_dose_folic_acid +
#                       strata(groupID),
#                     data = p_full)


# Valproate ---------------------------------------------------------------

# Exposure + Folic Acid
p_exp_fa_val <- clogit(any_loss ~
                        exposed_valproate_mono +
                        high_dose_folic_acid +
                        strata(groupID),
                      data = p_full)


# Interaction
p_inter_val <- clogit(any_loss ~
                        exposed_valproate_mono +
                        epilepsy_indication + mh_flag + migraine_pain_flag +
                        maternal_age_group_conception + mother_simd + drug_alcohol_use +
                        any_smr_comorb +
                        high_dose_folic_acid +
                        exposed_valproate_mono:high_dose_folic_acid +
                        strata(groupID),
                      data = p_full)


# Generate summary tables
# Exposure + Folic Acid
p_exp_fa_val_summary <- tbl_regression(p_exp_fa_val, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
p_inter_val_summary <- tbl_regression(p_inter_val, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)




# Topiramate --------------------------------------------------------------

# Exposure + Folic Acid
p_exp_fa_top <- clogit(any_loss ~
                         exposed_topiramate_mono +
                         high_dose_folic_acid +
                         strata(groupID),
                       data = p_full)


# Interaction
p_inter_top <- clogit(any_loss ~
                         exposed_topiramate_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                        high_dose_folic_acid +
                         exposed_topiramate_mono:high_dose_folic_acid +
                         strata(groupID),
                       data = p_full)


# Exposure + Folic Acid
p_exp_fa_top_summary <- tbl_regression(p_exp_fa_top, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
p_inter_top_summary <- tbl_regression(p_inter_top, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)




# Carbamazepine -----------------------------------------------------------

# Exposure + Folic Acid
p_exp_fa_car <- clogit(any_loss ~
                            exposed_carbamazepine_mono +
                            high_dose_folic_acid +
                            strata(groupID),
                          data = p_full)


# Interaction
p_inter_car <- clogit(any_loss ~
                            exposed_carbamazepine_mono +
                            epilepsy_indication + mh_flag + migraine_pain_flag +
                            maternal_age_group_conception + mother_simd + drug_alcohol_use +
                            any_smr_comorb +
                        high_dose_folic_acid +
                            exposed_carbamazepine_mono:high_dose_folic_acid +
                            strata(groupID),
                          data = p_full)


# Exposure + Folic Acid
p_exp_fa_car_summary <- tbl_regression(p_exp_fa_car, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
p_inter_car_summary <- tbl_regression(p_inter_car, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)




# Lamotrigine -------------------------------------------------------------

# Exposure + Folic Acid
p_exp_fa_lam <- clogit(any_loss ~
                          exposed_lamotrigine_mono +
                          high_dose_folic_acid +
                          strata(groupID),
                        data = p_full)


# Interaction
p_inter_lam <- clogit(any_loss ~
                          exposed_lamotrigine_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                        high_dose_folic_acid +
                          exposed_lamotrigine_mono:high_dose_folic_acid +
                          strata(groupID),
                        data = p_full)


# Exposure + Folic Acid
p_exp_fa_lam_summary <- tbl_regression(p_exp_fa_lam, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
p_inter_lam_summary <- tbl_regression(p_inter_lam, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)





# Levetiracetam -----------------------------------------------------------

# Exposure + Folic Acid
p_exp_fa_lev <- clogit(any_loss ~
                            exposed_levetiracetam_mono +
                            high_dose_folic_acid +
                            strata(groupID),
                          data = p_full)


# Interaction
p_inter_lev <- clogit(any_loss ~
                            exposed_levetiracetam_mono +
                            epilepsy_indication + mh_flag + migraine_pain_flag +
                            maternal_age_group_conception + mother_simd + drug_alcohol_use +
                            any_smr_comorb +
                        high_dose_folic_acid +
                            exposed_levetiracetam_mono:high_dose_folic_acid +
                            strata(groupID),
                          data = p_full)


# Exposure + Folic Acid
p_exp_fa_lev_summary <- tbl_regression(p_exp_fa_lev, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
p_inter_lev_summary <- tbl_regression(p_inter_lev, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)



# Gabapentin --------------------------------------------------------------

# Exposure + Folic Acid
p_exp_fa_gab <- clogit(any_loss ~
                         exposed_gabapentin_mono +
                         high_dose_folic_acid +
                         strata(groupID),
                       data = p_full)

# Interaction
p_inter_gab <- clogit(any_loss ~
                         exposed_gabapentin_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                        high_dose_folic_acid +
                         exposed_gabapentin_mono:high_dose_folic_acid +
                         strata(groupID),
                       data = p_full)


# Exposure + Folic Acid
p_exp_fa_gab_summary <- tbl_regression(p_exp_fa_gab, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
p_inter_gab_summary <- tbl_regression(p_inter_gab, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)



# Pregabalin --------------------------------------------------------------

# Exposure + Folic Acid
p_exp_fa_pre <- clogit(any_loss ~
                         exposed_pregabalin_mono +
                         high_dose_folic_acid +
                         strata(groupID),
                       data = p_full)

# Interaction
p_inter_pre <- clogit(any_loss ~
                         exposed_pregabalin_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                        high_dose_folic_acid +
                         exposed_pregabalin_mono:high_dose_folic_acid +
                         strata(groupID),
                       data = p_full)


# Exposure + Folic Acid
p_exp_fa_pre_summary <- tbl_regression(p_exp_fa_pre, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
p_inter_pre_summary <- tbl_regression(p_inter_pre, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)




# Bind results + save out -------------------------------------------------

# Exposure + Folic Acid models
p_exp_fa_full <- bind_rows(p_exp_fa_any_summary, p_exp_fa_val_summary, p_exp_fa_top_summary, 
                           p_exp_fa_car_summary, p_exp_fa_lam_summary, p_exp_fa_lev_summary, 
                           p_exp_fa_gab_summary, p_exp_fa_pre_summary)

write_csv(p_exp_fa_full, paste0(folder_data_path, "stats/p_exp_fa_full.csv"))


# Interaction
p_inter_full <- bind_rows(p_inter_any_summary, p_inter_val_summary, p_inter_top_summary,
                          p_inter_car_summary, p_inter_lam_summary, p_inter_lev_summary, 
                          p_inter_gab_summary, p_inter_pre_summary)

write_csv(p_inter_full,paste0(folder_data_path, "stats/p_inter_full.csv"))




# 4) Congenital cohort - adjusted models ----------------------------------
# Any ASM -----------------------------------------------------------------
## exp only
cc_exp_only <- clogit(any_CC ~
                       CC_exposed_any_asm +
                       strata(groupID),
                     data = cc_full)

## FA only
cc_fa_only <- clogit(any_CC ~
                   high_dose_folic_acid +
                   strata(groupID),
                 data = cc_full)

# Exposure + FA
cc_exp_fa_any <- clogit(any_CC ~
                   CC_exposed_any_asm +
                   high_dose_folic_acid +
                   strata(groupID),
                 data = cc_full)

## exp + FA + interaction
cc_exp_fa_inter <- clogit(any_CC ~
                       CC_exposed_any_asm +
                       high_dose_folic_acid +
                       CC_exposed_any_asm:high_dose_folic_acid +
                       strata(groupID),
                     data = cc_full)


# Interaction
cc_inter_any <- clogit(any_CC ~
                   CC_exposed_any_asm +
                   epilepsy_indication + mh_flag + migraine_pain_flag +
                   maternal_age_group_conception + mother_simd + drug_alcohol_use +
                   any_smr_comorb +
                     high_dose_folic_acid +
                   CC_exposed_any_asm:high_dose_folic_acid +
                   strata(groupID),
                 data = cc_full)


## Exp only
cc_exp_only_summary <- tbl_regression(cc_exp_only, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(cc_exp_only_summary,paste0(folder_data_path, "stats/cc_ASM_only.csv"))


## FA only
cc_fa_only_summary <- tbl_regression(cc_fa_only, exponentiate = TRUE, 
                                     pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(cc_fa_only_summary,paste0(folder_data_path, "stats/cc_hdFA_only.csv"))

# Exposure + FA
cc_exp_fa_any_summary <- tbl_regression(cc_exp_fa_any, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)

write_csv(cc_exp_fa_any_summary, paste0(folder_data_path, "stats/cc_ASM_hdFA.csv"))


## Exposure + fa + interaction
cc_exp_fa_inter_summary <- tbl_regression(cc_exp_fa_inter, exponentiate = TRUE, 
                                          pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(cc_exp_fa_inter_summary, paste0(folder_data_path, "stats/cc_ASM_hdFA_interaction.csv"))



# Interaction
cc_inter_any_summary <- tbl_regression(cc_inter_any, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 | row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)



# Valproate ---------------------------------------------------------------

# Exposure + FA
cc_exp_fa_val <- clogit(any_CC ~
                         CC_exposed_valproate_mono +
                         high_dose_folic_acid +
                         strata(groupID),
                       data = cc_full)


# Interaction
cc_inter_val <- clogit(any_CC ~
                         CC_exposed_valproate_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         high_dose_folic_acid +
                         CC_exposed_valproate_mono:high_dose_folic_acid +
                         strata(groupID),
                       data = cc_full)


# Exposure + FA
cc_exp_fa_val_summary <- tbl_regression(cc_exp_fa_val, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Interaction
cc_inter_val_summary <- tbl_regression(cc_inter_val, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 | row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Topiramate --------------------------------------------------------------

# Exposure +FA
cc_exp_fa_top <- clogit(any_CC ~
                       CC_exposed_topiramate_mono +
                       high_dose_folic_acid +
                       strata(groupID),
                     data = cc_full)

# Interaction
cc_inter_top <- clogit(any_CC ~
                          CC_exposed_topiramate_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception_collapsed + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                         high_dose_folic_acid +
                          CC_exposed_topiramate_mono:high_dose_folic_acid +
                          strata(groupID),
                        data = cc_full)


# Exposure + FA
cc_exp_fa_top_summary <- tbl_regression(cc_exp_fa_top, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Interaction
cc_inter_top_summary <- tbl_regression(cc_inter_top, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 | row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)



# Carbamazepine -----------------------------------------------------------

# Exposure +FA
cc_exp_fa_car <- clogit(any_CC ~
                             CC_exposed_carbamazepine_mono +
                             high_dose_folic_acid +
                             strata(groupID),
                           data = cc_full)

# Interaction
cc_inter_car <- clogit(any_CC ~
                             CC_exposed_carbamazepine_mono +
                             epilepsy_indication + mh_flag + migraine_pain_flag +
                             maternal_age_group_conception + mother_simd + drug_alcohol_use +
                             any_smr_comorb +
                         high_dose_folic_acid +
                             CC_exposed_carbamazepine_mono:high_dose_folic_acid +
                             strata(groupID),
                           data = cc_full)


# Exposure + FA
cc_exp_fa_car_summary <- tbl_regression(cc_exp_fa_car, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Interaction
cc_inter_car_summary <- tbl_regression(cc_inter_car, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 |row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Lamotrigine -------------------------------------------------------------

# Exposure + FA
cc_exp_fa_lam <- clogit(any_CC ~
                           CC_exposed_lamotrigine_mono +
                           high_dose_folic_acid +
                           strata(groupID),
                         data = cc_full)


# Interaction
cc_inter_lam <- clogit(any_CC ~
                           CC_exposed_lamotrigine_mono +
                           epilepsy_indication + mh_flag + migraine_pain_flag +
                           maternal_age_group_conception + mother_simd + drug_alcohol_use +
                           any_smr_comorb +
                         high_dose_folic_acid +
                           CC_exposed_lamotrigine_mono:high_dose_folic_acid +
                           strata(groupID),
                         data = cc_full)


# Exposure + FA
cc_exp_fa_lam_summary <- tbl_regression(cc_exp_fa_lam, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 | row_number()==2 | row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# INteraction
cc_inter_lam_summary <- tbl_regression(cc_inter_lam, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 |row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)



# Levetiracetam -----------------------------------------------------------

# Exposure +FA
cc_exp_fa_lev <- clogit(any_CC ~
                             CC_exposed_levetiracetam_mono +
                             high_dose_folic_acid +
                             strata(groupID),
                           data = cc_full)

# Interaction
cc_inter_lev <- clogit(any_CC ~
                             CC_exposed_levetiracetam_mono +
                             epilepsy_indication + mh_flag + migraine_pain_flag +
                             maternal_age_group_conception + mother_simd + drug_alcohol_use +
                             any_smr_comorb +
                         high_dose_folic_acid +
                             CC_exposed_levetiracetam_mono:high_dose_folic_acid +
                             strata(groupID),
                           data = cc_full)


# Exposure + FA
cc_exp_fa_lev_summary <- tbl_regression(cc_exp_fa_lev, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Interaction
cc_inter_lev_summary <- tbl_regression(cc_inter_lev, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 |row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Gabapentin --------------------------------------------------------------

# Exposure + FA
cc_exp_fa_gab <- clogit(any_CC ~
                          CC_exposed_gabapentin_mono +
                          high_dose_folic_acid +
                          strata(groupID),
                        data = cc_full)

# Interaction
cc_inter_gab <- clogit(any_CC ~
                          CC_exposed_gabapentin_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                         high_dose_folic_acid +
                          CC_exposed_gabapentin_mono:high_dose_folic_acid +
                          strata(groupID),
                        data = cc_full)


# Exposure + FA 
cc_exp_fa_gab_summary <- tbl_regression(cc_exp_fa_gab, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%  
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Interaction
cc_inter_gab_summary <- tbl_regression(cc_inter_gab, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 | row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)




# Pregabalin --------------------------------------------------------------

# Exposure + FA
cc_exp_fa_pre <- clogit(any_CC ~
                          CC_exposed_pregabalin_mono +
                          high_dose_folic_acid +
                          strata(groupID),
                        data = cc_full)

# Interaction
cc_inter_pre <- clogit(any_CC ~
                          CC_exposed_pregabalin_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                         high_dose_folic_acid +
                          CC_exposed_pregabalin_mono:high_dose_folic_acid +
                          strata(groupID),
                        data = cc_full)


# Exposure +FA
cc_exp_fa_pre_summary <- tbl_regression(cc_exp_fa_pre, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)


# Interaction
cc_inter_pre_summary <- tbl_regression(cc_inter_pre, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>% 
  filter(row_number()==1 | row_number()==n()) %>% 
  janitor::clean_names() %>% 
  rename(ci = x95_percent_ci)




# Bind rows and save out --------------------------------------------------------
# Exposure + Folic Acid models
cc_exp_fa_full <- bind_rows(cc_exp_fa_any_summary, cc_exp_fa_val_summary, cc_exp_fa_top_summary, 
                           cc_exp_fa_car_summary, cc_exp_fa_lam_summary, cc_exp_fa_lev_summary, 
                           cc_exp_fa_gab_summary, cc_exp_fa_pre_summary)

write_csv(cc_exp_fa_full, paste0(folder_data_path, "stats/cc_exp_fa_full.csv"))


# Interaction
cc_inter_full <- bind_rows(cc_inter_any_summary, cc_inter_val_summary, cc_inter_top_summary,
                          cc_inter_car_summary, cc_inter_lam_summary, cc_inter_lev_summary, 
                          cc_inter_gab_summary, cc_inter_pre_summary)

write_csv(cc_inter_full, paste0(folder_data_path, "stats/cc_inter_full.csv"))




# 5) Developmental cohort - adjusted models -------------------------------

# Any ASM -----------------------------------------------------------------

## exp only
dev_exp_only <- clogit(any_dev_concern ~
                        exposed_any_asm +
                        strata(groupID),
                      data = dev_full)

## FA only
dev_fa_only <- clogit(any_dev_concern ~
                    high_dose_folic_acid +
                    strata(groupID),
                  data = dev_full)

# Exposure + FA
dev_exp_fa_any <- clogit(any_dev_concern ~
                    exposed_any_asm +
                    high_dose_folic_acid +
                    strata(groupID),
                  data = dev_full)

# Exposure + FA + interaction
dev_exp_fa_inter <- clogit(any_dev_concern ~
                           exposed_any_asm +
                           high_dose_folic_acid +
                             exposed_any_asm:high_dose_folic_acid +
                           strata(groupID),
                         data = dev_full)

# Interaction
dev_inter_any <- clogit(any_dev_concern ~
                    exposed_any_asm +
                    epilepsy_indication + mh_flag + migraine_pain_flag +
                    maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    any_smr_comorb +
                      high_dose_folic_acid +
                    exposed_any_asm:high_dose_folic_acid +
                    strata(groupID),
                  data = dev_full)


## exp only
dev_exp_only_summary <- tbl_regression(dev_exp_only, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(dev_exp_only_summary, paste0(folder_data_path, "stats/dev_ASM_only.csv"))


## FA only
dev_fa_only_summary <- tbl_regression(dev_fa_only, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(dev_fa_only_summary,paste0(folder_data_path, "stats/dev_hdFA_only.csv"))

# Exposure + FA
dev_exp_fa_any_summary <- tbl_regression(dev_exp_fa_any, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(dev_exp_fa_any_summary, paste0(folder_data_path, "stats/dev_ASM_hdFA.csv"))

# Exposure + FA + interaction
dev_exp_fa_inter_summary <- tbl_regression(dev_exp_fa_inter, exponentiate = TRUE, 
                                           pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

write_csv(dev_exp_fa_inter_summary, paste0(folder_data_path, "stats/dev_ASM_hdFA_interaction.csv"))

# Interaction
dev_inter_any_summary <- tbl_regression(dev_inter_any, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Valproate ---------------------------------------------------------------

# Exposure + FA
dev_exp_fa_val <- clogit(any_dev_concern ~
                          exposed_valproate_mono +
                          high_dose_folic_acid +
                          strata(groupID),
                        data = dev_full)


# Interaction
dev_inter_val <- clogit(any_dev_concern ~
                          exposed_valproate_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                          high_dose_folic_acid +
                          exposed_valproate_mono:high_dose_folic_acid +
                          strata(groupID),
                        data = dev_full)


# Exposure + FA
dev_exp_fa_val_summary <- tbl_regression(dev_exp_fa_val, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
dev_inter_val_summary <- tbl_regression(dev_inter_val, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)




# Topiramate --------------------------------------------------------------

# Exposure + FA
dev_exp_fa_top <- clogit(any_dev_concern ~
                           exposed_topiramate_mono +
                           high_dose_folic_acid +
                           strata(groupID),
                         data = dev_full)


# Interaction
dev_inter_top <- clogit(any_dev_concern ~
                           exposed_topiramate_mono +
                           epilepsy_indication + mh_flag + migraine_pain_flag +
                           maternal_age_group_conception + mother_simd + drug_alcohol_use +
                           any_smr_comorb +
                          high_dose_folic_acid +
                           exposed_topiramate_mono:high_dose_folic_acid +
                           strata(groupID),
                         data = dev_full)


# Exposure + FA
dev_exp_fa_top_summary <- tbl_regression(dev_exp_fa_top, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
dev_inter_top_summary <- tbl_regression(dev_inter_top, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)



# Carbamazepine -----------------------------------------------------------

# Exposure + FA
dev_exp_fa_car <- clogit(any_dev_concern ~
                              exposed_carbamazepine_mono +
                              high_dose_folic_acid +
                              strata(groupID),
                            data = dev_full)


# Interaction
dev_inter_car <- clogit(any_dev_concern ~
                              exposed_carbamazepine_mono +
                              epilepsy_indication + mh_flag + migraine_pain_flag +
                              maternal_age_group_conception + mother_simd + drug_alcohol_use +
                              any_smr_comorb +
                          high_dose_folic_acid +
                              exposed_carbamazepine_mono:high_dose_folic_acid +
                              strata(groupID),
                            data = dev_full)


# Exposure + FA
dev_exp_fa_car_summary <- tbl_regression(dev_exp_fa_car, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
dev_inter_car_summary <- tbl_regression(dev_inter_car, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)



# Lamotrigine -------------------------------------------------------------

# Exposure + FA
dev_exp_fa_lam <- clogit(any_dev_concern ~
                            exposed_lamotrigine_mono +
                            high_dose_folic_acid +
                            strata(groupID),
                          data = dev_full)

# Interaction
dev_inter_lam <- clogit(any_dev_concern ~
                            exposed_lamotrigine_mono +
                            epilepsy_indication + mh_flag + migraine_pain_flag +
                            maternal_age_group_conception + mother_simd + drug_alcohol_use +
                            any_smr_comorb +
                          high_dose_folic_acid +
                            exposed_lamotrigine_mono:high_dose_folic_acid +
                            strata(groupID),
                          data = dev_full)

# Exposure + FA
dev_exp_fa_lam_summary <- tbl_regression(dev_exp_fa_lam, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
dev_inter_lam_summary <- tbl_regression(dev_inter_lam, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1| row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)



# Levetiracetam -----------------------------------------------------------

# Exposure + FA
dev_exp_fa_lev <- clogit(any_dev_concern ~
                              exposed_levetiracetam_mono +
                              high_dose_folic_acid +
                              strata(groupID),
                            data = dev_full)

# Interaction
dev_inter_lev <- clogit(any_dev_concern ~
                              exposed_levetiracetam_mono +
                              epilepsy_indication + mh_flag + migraine_pain_flag +
                              maternal_age_group_conception + mother_simd + drug_alcohol_use +
                              any_smr_comorb +
                          high_dose_folic_acid +
                              exposed_levetiracetam_mono:high_dose_folic_acid +
                              strata(groupID),
                            data = dev_full)


# Exposure + FA
dev_exp_fa_lev_summary <- tbl_regression(dev_exp_fa_lev, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
dev_inter_lev_summary <- tbl_regression(dev_inter_lev, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Gabapentin --------------------------------------------------------------

# Exposure + FA
dev_exp_fa_gab <- clogit(any_dev_concern ~
                           exposed_gabapentin_mono +
                           high_dose_folic_acid +
                           strata(groupID),
                         data = dev_full)

# Interaction
dev_inter_gab <- clogit(any_dev_concern ~
                           exposed_gabapentin_mono +
                           epilepsy_indication + mh_flag + migraine_pain_flag +
                           maternal_age_group_conception + mother_simd + drug_alcohol_use +
                           any_smr_comorb +
                          high_dose_folic_acid +
                           exposed_gabapentin_mono:high_dose_folic_acid +
                           strata(groupID),
                         data = dev_full)


# Exposure + FA
dev_exp_fa_gab_summary <- tbl_regression(dev_exp_fa_gab, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
dev_inter_gab_summary <- tbl_regression(dev_inter_gab, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)



# Pregabalin --------------------------------------------------------------

# Exposure + FA
dev_exp_fa_pre <- clogit(any_dev_concern ~
                           exposed_pregabalin_mono +
                           high_dose_folic_acid +
                           strata(groupID),
                         data = dev_full)

# Interaction
dev_inter_pre <- clogit(any_dev_concern ~
                           exposed_pregabalin_mono +
                           epilepsy_indication + mh_flag + migraine_pain_flag +
                           maternal_age_group_conception + mother_simd + drug_alcohol_use +
                           any_smr_comorb +
                          high_dose_folic_acid +
                           exposed_pregabalin_mono:high_dose_folic_acid +
                           strata(groupID),
                         data = dev_full)


# Exposure + FA
dev_exp_fa_pre_summary <- tbl_regression(dev_exp_fa_pre, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Interaction
dev_inter_pre_summary <- tbl_regression(dev_inter_pre, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1 | row_number()==n()) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)


# Bind rows and save out --------------------------------------------------

# Exposure + FA
dev_exp_fa_full <- bind_rows(dev_exp_fa_any_summary, dev_exp_fa_val_summary, dev_exp_fa_top_summary,
                             dev_exp_fa_car_summary, dev_exp_fa_lam_summary, dev_exp_fa_lev_summary, 
                             dev_exp_fa_gab_summary, dev_exp_fa_pre_summary)

write_csv(dev_exp_fa_full, paste0(folder_data_path, "stats/dev_exp_fa_full.csv"))


# Interaction
dev_inter_full <- bind_rows(dev_inter_any_summary, dev_inter_val_summary, dev_inter_top_summary,
                             dev_inter_car_summary, dev_inter_lam_summary, dev_inter_lev_summary, 
                             dev_inter_gab_summary, dev_inter_pre_summary)

write_csv(dev_inter_full, paste0(folder_data_path, "stats/dev_inter_full.csv"))


