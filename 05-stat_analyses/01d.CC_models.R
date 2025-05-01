# /stat_analyses/01d.CC_models.R

# 1) Housekeeping ---------------------------------------------------------

rm(list = ls())
gc()

library(dplyr)
library(tidyverse)
library(survival)
library(gtsummary)
library(writexl)

# Custom function to format p-values
custom_pvalue_fun <- function(x) {
  ifelse(x < 0.001, "<0.001", formatC(x, format = "f", digits = 3))
}
#source filepaths
source("05-stat_analysis/00.stat_setup.r")
# 2) Read in data ---------------------------------------------------------

cc_full <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_cc_data.parquet"))

cc_ep <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_cc_ep_data.parquet"))

cc_12wks <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_cc_12wks_data.parquet"))


# 3) Models - full cohort---------------------------------------------------------------
# 3.1) Adjusted models ----------------------------------------------------

# based on n events we can do the full adjusted model for some ASMs in the full CC cohort
# topiramate required an adjusted model with maternal age collapsed

# any ASM
cc_any <- clogit(any_CC ~
                  CC_exposed_any_asm +
                  epilepsy_indication + mh_flag + migraine_pain_flag +
                  maternal_age_group_conception + mother_simd + drug_alcohol_use +
                  any_smr_comorb +
                  strata(groupID),
                data = cc_full)

# valproate
cc_valproate <- clogit(any_CC ~
                         CC_exposed_valproate_mono +
                        epilepsy_indication + mh_flag + migraine_pain_flag +
                        maternal_age_group_conception + mother_simd + drug_alcohol_use +
                        any_smr_comorb +
                        strata(groupID),
                      data = cc_full)
# topirmate
cc_topiramate <- clogit(any_CC ~
                          CC_exposed_topiramate_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception_collapsed + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = cc_full)

# carbamazepine
cc_carbamazepine <- clogit(any_CC ~
                             CC_exposed_carbamazepine_mono +
                            epilepsy_indication + mh_flag + migraine_pain_flag +
                            maternal_age_group_conception + mother_simd + drug_alcohol_use +
                            any_smr_comorb +
                            strata(groupID),
                          data = cc_full)

# lamotrigine
cc_lamotrigine <- clogit(any_CC ~
                           CC_exposed_lamotrigine_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                          strata(groupID),
                        data = cc_full)

# levetiracetam
cc_levetiracetam <- clogit(any_CC ~
                             CC_exposed_levetiracetam_mono +
                            epilepsy_indication + mh_flag + migraine_pain_flag +
                            maternal_age_group_conception + mother_simd + drug_alcohol_use +
                            any_smr_comorb +
                            strata(groupID),
                          data = cc_full)

# gabapentin
cc_gabapentin <- clogit(any_CC ~
                          CC_exposed_gabapentin_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = cc_full)

# pregabalin
cc_pregabalin <- clogit(any_CC ~
                          CC_exposed_pregabalin_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = cc_full)

# generate summary tables
cc_any_summary <- tbl_regression(cc_any, exponentiate = TRUE,
                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_valproate_summary <- tbl_regression(cc_valproate, exponentiate = TRUE,
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_topiramate_summary <- tbl_regression(cc_topiramate, exponentiate = TRUE,
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_carbamazepine_summary <- tbl_regression(cc_carbamazepine, exponentiate = TRUE,
                                           pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_lamotrigine_summary <- tbl_regression(cc_lamotrigine, exponentiate = TRUE,
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_levetiracetam_summary <- tbl_regression(cc_levetiracetam, exponentiate = TRUE,
                                           pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_gabapentin_summary <- tbl_regression(cc_gabapentin, exponentiate = TRUE,
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_pregabalin_summary <- tbl_regression(cc_pregabalin, exponentiate = TRUE,
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_full_adjusted <- bind_rows(cc_any_summary, cc_valproate_summary, cc_topiramate_summary,
                    cc_carbamazepine_summary, cc_lamotrigine_summary,
                    cc_levetiracetam_summary, cc_gabapentin_summary,
                    cc_pregabalin_summary)

write_csv(cc_full_adjusted, paste0(folder_data_path, "stats/cc_full_adjusted.csv"))




# 3.2) Unadjusted models --------------------------------------------------

# any ASM
unadj_cc_any <- clogit(any_CC ~
                        CC_exposed_any_asm +
                        strata(groupID),
                      data = cc_full)

# valproate
unadj_cc_valproate <- clogit(any_CC ~
                              CC_exposed_valproate_mono +
                              strata(groupID),
                            data = cc_full)
# topirmate
unadj_cc_topiramate <- clogit(any_CC ~
                               CC_exposed_topiramate_mono +
                               strata(groupID),
                             data = cc_full)

# carbamazepine
unadj_cc_carbamazepine <- clogit(any_CC ~
                                  CC_exposed_carbamazepine_mono +
                                  strata(groupID),
                                data = cc_full)

# lamotrigine
unadj_cc_lamotrigine <- clogit(any_CC ~
                                CC_exposed_lamotrigine_mono +
                                strata(groupID),
                              data = cc_full)

# levetiracetam
unadj_cc_levetiracetam <- clogit(any_CC ~
                                  CC_exposed_levetiracetam_mono +
                                  strata(groupID),
                                data = cc_full)

# gabapentin
unadj_cc_gabapentin <- clogit(any_CC ~
                               CC_exposed_gabapentin_mono +
                               strata(groupID),
                             data = cc_full)

# pregabalin
unadj_cc_pregabalin <- clogit(any_CC ~
                               CC_exposed_pregabalin_mono +
                               strata(groupID),
                             data = cc_full)

# generate summary tables
unadj_cc_any_summary <- tbl_regression(unadj_cc_any, exponentiate = TRUE,
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_valproate_summary <- tbl_regression(unadj_cc_valproate, exponentiate = TRUE,
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_topiramate_summary <- tbl_regression(unadj_cc_topiramate, exponentiate = TRUE,
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_carbamazepine_summary <- tbl_regression(unadj_cc_carbamazepine, exponentiate = TRUE,
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_lamotrigine_summary <- tbl_regression(unadj_cc_lamotrigine, exponentiate = TRUE,
                                               pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_levetiracetam_summary <- tbl_regression(unadj_cc_levetiracetam, exponentiate = TRUE,
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_gabapentin_summary <- tbl_regression(unadj_cc_gabapentin, exponentiate = TRUE,
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_pregabalin_summary <- tbl_regression(unadj_cc_pregabalin, exponentiate = TRUE,
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_full <- bind_rows(unadj_cc_any_summary, unadj_cc_valproate_summary, unadj_cc_topiramate_summary,
                          unadj_cc_carbamazepine_summary, unadj_cc_lamotrigine_summary,
                          unadj_cc_levetiracetam_summary, unadj_cc_gabapentin_summary,
                          unadj_cc_pregabalin_summary)

write_csv(unadj_cc_full, paste0(folder_data_path, "stats/cc_full_unadjusted.csv"))



# 4) Models - epilepsy only cohort -----------------------------------------
# 4.1) Adjusted models ----------------------------------------------------

# based on n events we can do the full adjusted model for some ASMs in the epilepsy CC cohort
# valproate, topiramate, carbamazepine, gabapentin, and pregabalin all require an unadjusted model

# any ASM
cc_ep_any <- clogit(any_CC ~ CC_exposed_any_asm + 
                     epilepsy_indication + mh_flag + migraine_pain_flag + 
                     maternal_age_group_conception + mother_simd + 
                     drug_alcohol_use + any_smr_comorb +
                     strata(groupID),
                   data = cc_ep)


# Lamotrigine
cc_ep_lamotrigine <- clogit(any_CC ~ CC_exposed_lamotrigine_mono + 
                             epilepsy_indication + mh_flag + migraine_pain_flag + 
                             maternal_age_group_conception + mother_simd + 
                             drug_alcohol_use + any_smr_comorb +
                             strata(groupID),
                           data = cc_ep)


# Levetiracetam
cc_ep_levetiracetam <- clogit(any_CC ~ CC_exposed_levetiracetam_mono + 
                               epilepsy_indication + mh_flag + migraine_pain_flag + 
                               maternal_age_group_conception + mother_simd + 
                               drug_alcohol_use + any_smr_comorb +
                               strata(groupID),
                             data = cc_ep)


# generate summary tables
cc_ep_any_summary <- tbl_regression(cc_ep_any, exponentiate = TRUE,
                                    pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_ep_lamotrigine_summary <- tbl_regression(cc_ep_lamotrigine, exponentiate = TRUE,
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_ep_levetiracetam_summary <- tbl_regression(cc_ep_levetiracetam, exponentiate = TRUE,
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_ep_full <- bind_rows(cc_ep_any_summary,
                       cc_ep_lamotrigine_summary,
                       cc_ep_levetiracetam_summary)

write_csv(cc_ep_full, paste0(folder_data_path, "stats/cc_ep_full_adjusted.csv"))

# 4.2) Unadjusted models ----------------------------------------------------

# any ASM
unadj_cc_ep_any <- clogit(any_CC ~ CC_exposed_any_asm + 
                           strata(groupID),
                         data = cc_ep)

# Valproate
unadj_cc_ep_valproate <- clogit(any_CC ~ CC_exposed_valproate_mono + 
                                 strata(groupID),
                               data = cc_ep)

# Topiramate
unadj_cc_ep_topiramate <- clogit(any_CC ~ CC_exposed_topiramate_mono +
                                  strata(groupID),
                                data = cc_ep)

# Carbamazepine
unadj_cc_ep_carbamazepine <- clogit(any_CC ~ CC_exposed_carbamazepine_mono +
                                     strata(groupID),
                                   data = cc_ep)

# Lamotrigine
unadj_cc_ep_lamotrigine <- clogit(any_CC ~ CC_exposed_lamotrigine_mono + 
                                   strata(groupID),
                                 data = cc_ep)


# Levetiracetam
unadj_cc_ep_levetiracetam <- clogit(any_CC ~ CC_exposed_levetiracetam_mono + 
                                     strata(groupID),
                                   data = cc_ep)

# Gabapentin
unadj_cc_ep_gabapentin <- clogit(any_CC ~ CC_exposed_gabapentin_mono + 
                                  strata(groupID),
                                data = cc_ep)

# Pregabalin
unadj_cc_ep_pregabalin <- clogit(any_CC ~ CC_exposed_pregabalin_mono + 
                                  strata(groupID),
                                data = cc_ep)


# generate summary tables
unadj_cc_ep_any_summary <- tbl_regression(unadj_cc_ep_any, exponentiate = TRUE,
                                          pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_ep_valproate_summary <- tbl_regression(unadj_cc_ep_valproate, exponentiate = TRUE,
                                                pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_ep_topiramate_summary <- tbl_regression(unadj_cc_ep_topiramate, exponentiate = TRUE,
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_ep_carbamazepine_summary <- tbl_regression(unadj_cc_ep_carbamazepine, exponentiate = TRUE,
                                                    pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_ep_lamotrigine_summary <- tbl_regression(unadj_cc_ep_lamotrigine, exponentiate = TRUE,
                                                  pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_ep_levetiracetam_summary <- tbl_regression(unadj_cc_ep_levetiracetam, exponentiate = TRUE,
                                                    pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_ep_gabapentin_summary <- tbl_regression(unadj_cc_ep_gabapentin, exponentiate = TRUE,
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_ep_pregabalin_summary <- tbl_regression(unadj_cc_ep_pregabalin, exponentiate = TRUE,
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_ep_full <- bind_rows(unadj_cc_ep_any_summary, unadj_cc_ep_valproate_summary, unadj_cc_ep_topiramate_summary,
                             unadj_cc_ep_carbamazepine_summary, unadj_cc_ep_lamotrigine_summary,
                             unadj_cc_ep_levetiracetam_summary, unadj_cc_ep_gabapentin_summary,
                             unadj_cc_ep_pregabalin_summary)

write_csv(unadj_cc_ep_full, paste0(folder_data_path, "stats/cc_ep_full_unadjusted.csv"))



# 5) Models - 12 weeks gestation only cohort -----------------------------------------
# 5.1) Adjusted models ----------------------------------------------------

# based on n events we can do the full adjusted model for some ASMs in the full CC cohort
# topiramate requires an adjusted model with maternal age and deprivation collapsed

# any ASM
cc_12wks_any <- clogit(any_CC ~
                   CC_exposed_any_asm +
                   epilepsy_indication + mh_flag + migraine_pain_flag +
                   maternal_age_group_conception + mother_simd + drug_alcohol_use +
                   any_smr_comorb +
                   strata(groupID),
                 data = cc_12wks)

# valproate
cc_12wks_valproate <- clogit(any_CC ~
                         CC_exposed_valproate_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = cc_12wks)
# topirmate
cc_12wks_topiramate <- clogit(any_CC ~
                          CC_exposed_topiramate_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception_collapsed + mother_simd_collapsed + drug_alcohol_use +
                          any_smr_comorb +
                          strata(groupID),
                        data = cc_12wks)

# carbamazepine
cc_12wks_carbamazepine <- clogit(any_CC ~
                             CC_exposed_carbamazepine_mono +
                             epilepsy_indication + mh_flag + migraine_pain_flag +
                             maternal_age_group_conception + mother_simd + drug_alcohol_use +
                             any_smr_comorb +
                             strata(groupID),
                           data = cc_12wks)

# lamotrigine
cc_12wks_lamotrigine <- clogit(any_CC ~
                           CC_exposed_lamotrigine_mono +
                           epilepsy_indication + mh_flag + migraine_pain_flag +
                           maternal_age_group_conception + mother_simd + drug_alcohol_use +
                           any_smr_comorb +
                           strata(groupID),
                         data = cc_12wks)

# levetiracetam
cc_12wks_levetiracetam <- clogit(any_CC ~
                             CC_exposed_levetiracetam_mono +
                             epilepsy_indication + mh_flag + migraine_pain_flag +
                             maternal_age_group_conception + mother_simd + drug_alcohol_use +
                             any_smr_comorb +
                             strata(groupID),
                           data = cc_12wks)

# gabapentin
cc_12wks_gabapentin <- clogit(any_CC ~
                          CC_exposed_gabapentin_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                          strata(groupID),
                        data = cc_12wks)

# pregabalin
cc_12wks_pregabalin <- clogit(any_CC ~
                          CC_exposed_pregabalin_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                          strata(groupID),
                        data = cc_12wks)

# generate summary tables
cc_12wks_any_summary <- tbl_regression(cc_12wks_any, exponentiate = TRUE,
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_12wks_valproate_summary <- tbl_regression(cc_12wks_valproate, exponentiate = TRUE,
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_12wks_topiramate_summary <- tbl_regression(cc_12wks_topiramate, exponentiate = TRUE,
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_12wks_carbamazepine_summary <- tbl_regression(cc_12wks_carbamazepine, exponentiate = TRUE,
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_12wks_lamotrigine_summary <- tbl_regression(cc_12wks_lamotrigine, exponentiate = TRUE,
                                               pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_12wks_levetiracetam_summary <- tbl_regression(cc_12wks_levetiracetam, exponentiate = TRUE,
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_12wks_gabapentin_summary <- tbl_regression(cc_12wks_gabapentin, exponentiate = TRUE,
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_12wks_pregabalin_summary <- tbl_regression(cc_12wks_pregabalin, exponentiate = TRUE,
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

cc_12wks_full <- bind_rows(cc_12wks_any_summary, cc_12wks_valproate_summary, cc_12wks_topiramate_summary,
                     cc_12wks_carbamazepine_summary, cc_12wks_lamotrigine_summary,
                     cc_12wks_levetiracetam_summary, cc_12wks_gabapentin_summary,
                     cc_12wks_pregabalin_summary)

write_csv(cc_12wks_full, paste0(folder_data_path, "stats/cc_12wks_full_adjusted.csv"))




# 5.2) Unadjusted models --------------------------------------------------

# any ASM
unadj_cc_12wks_any <- clogit(any_CC ~
                         CC_exposed_any_asm +
                         strata(groupID),
                       data = cc_12wks)

# valproate
unadj_cc_12wks_valproate <- clogit(any_CC ~
                               CC_exposed_valproate_mono +
                               strata(groupID),
                             data = cc_12wks)
# topirmate
unadj_cc_12wks_topiramate <- clogit(any_CC ~
                                CC_exposed_topiramate_mono +
                                strata(groupID),
                              data = cc_12wks)

# carbamazepine
unadj_cc_12wks_carbamazepine <- clogit(any_CC ~
                                   CC_exposed_carbamazepine_mono +
                                   strata(groupID),
                                 data = cc_12wks)

# lamotrigine
unadj_cc_12wks_lamotrigine <- clogit(any_CC ~
                                 CC_exposed_lamotrigine_mono +
                                 strata(groupID),
                               data = cc_12wks)

# levetiracetam
unadj_cc_12wks_levetiracetam <- clogit(any_CC ~
                                   CC_exposed_levetiracetam_mono +
                                   strata(groupID),
                                 data = cc_12wks)

# gabapentin
unadj_cc_12wks_gabapentin <- clogit(any_CC ~
                                CC_exposed_gabapentin_mono +
                                strata(groupID),
                              data = cc_12wks)

# pregabalin
unadj_cc_12wks_pregabalin <- clogit(any_CC ~
                                CC_exposed_pregabalin_mono +
                                strata(groupID),
                              data = cc_12wks)

# generate summary tables
unadj_cc_12wks_any_summary <- tbl_regression(unadj_cc_12wks_any, exponentiate = TRUE,
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_12wks_valproate_summary <- tbl_regression(unadj_cc_12wks_valproate, exponentiate = TRUE,
                                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_12wks_topiramate_summary <- tbl_regression(unadj_cc_12wks_topiramate, exponentiate = TRUE,
                                                    pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_12wks_carbamazepine_summary <- tbl_regression(unadj_cc_12wks_carbamazepine, exponentiate = TRUE,
                                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_12wks_lamotrigine_summary <- tbl_regression(unadj_cc_12wks_lamotrigine, exponentiate = TRUE,
                                                     pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_12wks_levetiracetam_summary <- tbl_regression(unadj_cc_12wks_levetiracetam, exponentiate = TRUE,
                                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_12wks_gabapentin_summary <- tbl_regression(unadj_cc_12wks_gabapentin, exponentiate = TRUE,
                                                    pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_12wks_pregabalin_summary <- tbl_regression(unadj_cc_12wks_pregabalin, exponentiate = TRUE,
                                                    pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_cc_12wks_full <- bind_rows(unadj_cc_12wks_any_summary, unadj_cc_12wks_valproate_summary, unadj_cc_12wks_topiramate_summary,
                           unadj_cc_12wks_carbamazepine_summary, unadj_cc_12wks_lamotrigine_summary,
                           unadj_cc_12wks_levetiracetam_summary, unadj_cc_12wks_gabapentin_summary,
                           unadj_cc_12wks_pregabalin_summary)

write_csv(unadj_cc_12wks_full, paste0(folder_data_path, "stats/cc_12wks_full_unadjusted.csv"))



