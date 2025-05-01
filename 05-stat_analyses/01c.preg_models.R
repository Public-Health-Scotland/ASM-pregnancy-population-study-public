# /stat_analyses/01c.preg_models.R

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

p_full <- arrow::read_parquet(paste0(folder_data_path, 'stats/temp_preg_data.parquet'))

p_ep <- arrow::read_parquet(paste0(folder_data_path, 'stats/temp_preg_ep_data.parquet'))


# 3) Models - full cohort---------------------------------------------------------------
# 3.1) Adjusted models ----------------------------------------------------

# based on n events we can do the full adjusted model for all ASMs in the full preg cohort

# any ASM
p_any <- clogit(any_loss ~
                  exposed_any_asm +
                  epilepsy_indication + mh_flag + migraine_pain_flag +
                  maternal_age_group_conception + mother_simd + drug_alcohol_use +
                  any_smr_comorb +
                  strata(groupID),
                data = p_full)

# valproate
p_valproate <- clogit(any_loss ~
                        exposed_valproate_mono +
                        epilepsy_indication + mh_flag + migraine_pain_flag +
                        maternal_age_group_conception + mother_simd + drug_alcohol_use +
                        any_smr_comorb +
                        strata(groupID),
                      data = p_full)
# topirmate
p_topiramate <- clogit(any_loss ~
                         exposed_topiramate_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = p_full)

# carbamazepine
p_carbamazepine <- clogit(any_loss ~
                            exposed_carbamazepine_mono +
                            epilepsy_indication + mh_flag + migraine_pain_flag +
                            maternal_age_group_conception + mother_simd + drug_alcohol_use +
                            any_smr_comorb +
                            strata(groupID),
                          data = p_full)

# lamotrigine
p_lamotrigine <- clogit(any_loss ~
                          exposed_lamotrigine_mono +
                          epilepsy_indication + mh_flag + migraine_pain_flag +
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          any_smr_comorb +
                          strata(groupID),
                        data = p_full)

# levetiracetam
p_levetiracetam <- clogit(any_loss ~
                            exposed_levetiracetam_mono +
                            epilepsy_indication + mh_flag + migraine_pain_flag +
                            maternal_age_group_conception + mother_simd + drug_alcohol_use +
                            any_smr_comorb +
                            strata(groupID),
                          data = p_full)

# gabapentin
p_gabapentin <- clogit(any_loss ~
                         exposed_gabapentin_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = p_full)

# pregabalin
p_pregabalin <- clogit(any_loss ~
                         exposed_pregabalin_mono +
                         epilepsy_indication + mh_flag + migraine_pain_flag +
                         maternal_age_group_conception + mother_simd + drug_alcohol_use +
                         any_smr_comorb +
                         strata(groupID),
                       data = p_full)

# generate summary tables
p_any_summary <- tbl_regression(p_any, exponentiate = TRUE, 
                                pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_valproate_summary <- tbl_regression(p_valproate, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_topiramate_summary <- tbl_regression(p_topiramate, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_carbamazepine_summary <- tbl_regression(p_carbamazepine, exponentiate = TRUE, 
                                          pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_lamotrigine_summary <- tbl_regression(p_lamotrigine, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_levetiracetam_summary <- tbl_regression(p_levetiracetam, exponentiate = TRUE, 
                                          pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_gabapentin_summary <- tbl_regression(p_gabapentin, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_pregabalin_summary <- tbl_regression(p_pregabalin, exponentiate = TRUE, 
                                       pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_full_adjusted <- bind_rows(p_any_summary, p_valproate_summary, p_topiramate_summary,
                    p_carbamazepine_summary, p_lamotrigine_summary,
                    p_levetiracetam_summary, p_gabapentin_summary,
                    p_pregabalin_summary)

write_csv(p_full_adjusted, paste0(folder_data_path, 'stats/p_full_adjusted.csv'))



# 3.2) Unadjusted models --------------------------------------------------

# any ASM
unadj_p_any <- clogit(any_loss ~
                  exposed_any_asm +
                  strata(groupID),
                data = p_full)

# valproate
unadj_p_valproate <- clogit(any_loss ~
                        exposed_valproate_mono +
                        strata(groupID),
                      data = p_full)
# topirmate
unadj_p_topiramate <- clogit(any_loss ~
                         exposed_topiramate_mono +
                         strata(groupID),
                       data = p_full)

# carbamazepine
unadj_p_carbamazepine <- clogit(any_loss ~
                            exposed_carbamazepine_mono +
                            strata(groupID),
                          data = p_full)

# lamotrigine
unadj_p_lamotrigine <- clogit(any_loss ~
                          exposed_lamotrigine_mono +
                          strata(groupID),
                        data = p_full)

# levetiracetam
unadj_p_levetiracetam <- clogit(any_loss ~
                            exposed_levetiracetam_mono +
                            strata(groupID),
                          data = p_full)

# gabapentin
unadj_p_gabapentin <- clogit(any_loss ~
                         exposed_gabapentin_mono +
                         strata(groupID),
                       data = p_full)

# pregabalin
unadj_p_pregabalin <- clogit(any_loss ~
                         exposed_pregabalin_mono +
                         strata(groupID),
                       data = p_full)

# generate summary tables
unadj_p_any_summary <- tbl_regression(unadj_p_any, exponentiate = TRUE, 
                                      pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_valproate_summary <- tbl_regression(unadj_p_valproate, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_topiramate_summary <- tbl_regression(unadj_p_topiramate, exponentiate = TRUE, 
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_carbamazepine_summary <- tbl_regression(unadj_p_carbamazepine, exponentiate = TRUE, 
                                                pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_lamotrigine_summary <- tbl_regression(unadj_p_lamotrigine, exponentiate = TRUE, 
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_levetiracetam_summary <- tbl_regression(unadj_p_levetiracetam, exponentiate = TRUE, 
                                                pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_gabapentin_summary <- tbl_regression(unadj_p_gabapentin, exponentiate = TRUE, 
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_pregabalin_summary <- tbl_regression(unadj_p_pregabalin, exponentiate = TRUE, 
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_full <- bind_rows(unadj_p_any_summary, unadj_p_valproate_summary, unadj_p_topiramate_summary,
                    unadj_p_carbamazepine_summary, unadj_p_lamotrigine_summary,
                    unadj_p_levetiracetam_summary, unadj_p_gabapentin_summary,
                    unadj_p_pregabalin_summary)

write_csv(unadj_p_full, paste0(folder_data_path, 'stats/p_full_unadjusted.csv'))



# 4) Models - epilepsy only cohort -----------------------------------------
# 4.1) Adjusted models ----------------------------------------------------

# based on n events we can do the full adjusted model for all ASMs in the epilepsy only preg cohort

# any ASM
p_ep_any <- clogit(any_loss ~ exposed_any_asm + 
                     epilepsy_indication + mh_flag + migraine_pain_flag + 
                     maternal_age_group_conception + mother_simd + 
                     drug_alcohol_use + any_smr_comorb +
                    strata(groupID),
                  data = p_ep)

# Valproate
p_ep_valproate <- clogit(any_loss ~ exposed_valproate_mono + 
                           epilepsy_indication + mh_flag + migraine_pain_flag + 
                           maternal_age_group_conception + mother_simd + 
                           drug_alcohol_use + any_smr_comorb +
                    strata(groupID),
                  data = p_ep)

# Topiramate
p_ep_topiramate <- clogit(any_loss ~ exposed_topiramate_mono + 
                            epilepsy_indication + mh_flag + migraine_pain_flag +
                            maternal_age_group_conception + mother_simd + 
                            drug_alcohol_use + any_smr_comorb +
                    strata(groupID),
                  data = p_ep)

# Carbamazepine
p_ep_carbamazepine <- clogit(any_loss ~ exposed_carbamazepine_mono +
                                epilepsy_indication + mh_flag + migraine_pain_flag + 
                                maternal_age_group_conception + mother_simd + 
                                drug_alcohol_use + any_smr_comorb +
                    strata(groupID),
                  data = p_ep)

# Lamotrigine
p_ep_lamotrigine <- clogit(any_loss ~ exposed_lamotrigine_mono + 
                             epilepsy_indication + mh_flag + migraine_pain_flag + 
                             maternal_age_group_conception + mother_simd + 
                             drug_alcohol_use + any_smr_comorb +
                    strata(groupID),
                  data = p_ep)


# Levetiracetam
p_ep_levetiracetam <- clogit(any_loss ~ exposed_levetiracetam_mono + 
                               epilepsy_indication + mh_flag + migraine_pain_flag + 
                               maternal_age_group_conception + mother_simd + 
                               drug_alcohol_use + any_smr_comorb +
                    strata(groupID),
                  data = p_ep)

# Gabapentin
p_ep_gabapentin <- clogit(any_loss ~ exposed_gabapentin_mono + 
                            epilepsy_indication + mh_flag + migraine_pain_flag + 
                            maternal_age_group_conception + mother_simd + 
                            drug_alcohol_use + any_smr_comorb +
                    strata(groupID),
                  data = p_ep)

# Pregabalin
p_ep_pregabalin <- clogit(any_loss ~ exposed_pregabalin_mono + 
                            epilepsy_indication + mh_flag + migraine_pain_flag + 
                            maternal_age_group_conception + mother_simd + 
                            drug_alcohol_use + any_smr_comorb +
                    strata(groupID),
                  data = p_ep)


# generate summary tables
p_ep_any_summary <- tbl_regression(p_ep_any, exponentiate = TRUE, 
                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_ep_valproate_summary <- tbl_regression(p_ep_valproate, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_ep_topiramate_summary <- tbl_regression(p_ep_topiramate, exponentiate = TRUE, 
                                          pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_ep_carbamazepine_summary <- tbl_regression(p_ep_carbamazepine, exponentiate = TRUE, 
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_ep_lamotrigine_summary <- tbl_regression(p_ep_lamotrigine, exponentiate = TRUE, 
                                           pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_ep_levetiracetam_summary <- tbl_regression(p_ep_levetiracetam, exponentiate = TRUE, 
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_ep_gabapentin_summary <- tbl_regression(p_ep_gabapentin, exponentiate = TRUE, 
                                          pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_ep_pregabalin_summary <- tbl_regression(p_ep_pregabalin, exponentiate = TRUE, 
                                          pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

p_ep_full <- bind_rows(p_ep_any_summary, p_ep_valproate_summary, p_ep_topiramate_summary,
                    p_ep_carbamazepine_summary, p_ep_lamotrigine_summary,
                    p_ep_levetiracetam_summary, p_ep_gabapentin_summary,
                    p_ep_pregabalin_summary)

write_csv(p_ep_full,paste0(folder_data_path, 'stats/p_ep_full_adjusted.csv'))

# 4.2) Unadjusted models ----------------------------------------------------

# any ASM
unadj_p_ep_any <- clogit(any_loss ~ exposed_any_asm + 
                     strata(groupID),
                   data = p_ep)

# Valproate
unadj_p_ep_valproate <- clogit(any_loss ~ exposed_valproate_mono + 
                           strata(groupID),
                         data = p_ep)

# Topiramate
unadj_p_ep_topiramate <- clogit(any_loss ~ exposed_topiramate_mono +
                            strata(groupID),
                          data = p_ep)

# Carbamazepine
unadj_p_ep_carbamazepine <- clogit(any_loss ~ exposed_carbamazepine_mono +
                                strata(groupID),
                              data = p_ep)

# Lamotrigine
unadj_p_ep_lamotrigine <- clogit(any_loss ~ exposed_lamotrigine_mono + 
                             strata(groupID),
                           data = p_ep)


# Levetiracetam
unadj_p_ep_levetiracetam <- clogit(any_loss ~ exposed_levetiracetam_mono + 
                               strata(groupID),
                             data = p_ep)

# Gabapentin
unadj_p_ep_gabapentin <- clogit(any_loss ~ exposed_gabapentin_mono + 
                            strata(groupID),
                          data = p_ep)

# Pregabalin
unadj_p_ep_pregabalin <- clogit(any_loss ~ exposed_pregabalin_mono + 
                            strata(groupID),
                          data = p_ep)


# generate summary tables
unadj_p_ep_any_summary <- tbl_regression(unadj_p_ep_any, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_ep_valproate_summary <- tbl_regression(unadj_p_ep_valproate, exponentiate = TRUE, 
                                               pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_ep_topiramate_summary <- tbl_regression(unadj_p_ep_topiramate, exponentiate = TRUE, 
                                                pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_ep_carbamazepine_summary <- tbl_regression(unadj_p_ep_carbamazepine, exponentiate = TRUE, 
                                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_ep_lamotrigine_summary <- tbl_regression(unadj_p_ep_lamotrigine, exponentiate = TRUE, 
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_ep_levetiracetam_summary <- tbl_regression(unadj_p_ep_levetiracetam, exponentiate = TRUE, 
                                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_ep_gabapentin_summary <- tbl_regression(unadj_p_ep_gabapentin, exponentiate = TRUE, 
                                                pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_ep_pregabalin_summary <- tbl_regression(unadj_p_ep_pregabalin, exponentiate = TRUE, 
                                                pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_p_ep_full <- bind_rows(unadj_p_ep_any_summary, unadj_p_ep_valproate_summary, unadj_p_ep_topiramate_summary,
                             unadj_p_ep_carbamazepine_summary, unadj_p_ep_lamotrigine_summary,
                             unadj_p_ep_levetiracetam_summary, unadj_p_ep_gabapentin_summary,
                             unadj_p_ep_pregabalin_summary)

write_csv(unadj_p_ep_full, paste0(folder_data_path, 'stats/p_ep_full_unadjusted.csv'))








