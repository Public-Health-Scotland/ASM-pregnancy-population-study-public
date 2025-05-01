# /stat_analyses/01e.dev_models.R

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

dev_full <- arrow::read_parquet(paste0(folder_data_path, "stats/temp_dev_data.parquet"))

dev_ep <- arrow::read_parquet(paste0(folder_data_path, "data/stats/temp_dev_ep_data.parquet"))

# 
# # 3) Models - full cohort---------------------------------------------------------------
# 
# # based on n events we can do the full adjusted model for all ASMs in the full developmental cohort
# 
# # 3.1) Adjusted models ------------------------------------------------------------
# 
# # any ASM
dev_any <- clogit(any_dev_concern ~ 
                    exposed_any_asm + 
                    epilepsy_indication + mh_flag + migraine_pain_flag + 
                    maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + 
                    any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_full)

# Valproate
dev_valproate <- clogit(any_dev_concern ~ 
                          exposed_valproate_mono + 
                          epilepsy_indication + mh_flag + migraine_pain_flag + 
                          maternal_age_group_conception + mother_simd + drug_alcohol_use +
                          baby_sex + maternal_bmi_group + maternal_smoking + 
                          any_smr_comorb + parity +
                          strata(groupID),
                        data = dev_full)

# Topiramate
dev_topiramate <- clogit(any_dev_concern ~ 
                           exposed_topiramate_mono + 
                           epilepsy_indication + mh_flag + migraine_pain_flag + 
                           maternal_age_group_conception + mother_simd + drug_alcohol_use +
                           baby_sex + maternal_bmi_group + maternal_smoking + 
                           any_smr_comorb + parity +
                           strata(groupID),
                  data = dev_full)

# Carbamazepine
dev_carbamazepine <- clogit(any_dev_concern ~ 
                              exposed_carbamazepine_mono + 
                              epilepsy_indication + mh_flag + migraine_pain_flag +
                              maternal_age_group_conception + mother_simd + drug_alcohol_use +
                              baby_sex + maternal_bmi_group + maternal_smoking + 
                              any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_full)


# with Lamotrigine
dev_lamotrigine <- clogit(any_dev_concern ~ 
                    exposed_lamotrigine_mono + 
                    epilepsy_indication + mh_flag + migraine_pain_flag + 
                    maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + 
                    any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_full)

# Levetiracetam
dev_levetiracetam <- clogit(any_dev_concern ~ 
                    exposed_levetiracetam_mono + 
                    epilepsy_indication + mh_flag + migraine_pain_flag +
                    maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + 
                    any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_full)


# gabapentin
dev_gabapentin <- clogit(any_dev_concern ~ 
                    exposed_gabapentin_mono + 
                    epilepsy_indication + mh_flag + migraine_pain_flag + 
                    maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + 
                    any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_full)


# pregabalin
dev_pregabalin <- clogit(any_dev_concern ~ 
                    exposed_pregabalin_mono + 
                    epilepsy_indication + mh_flag + migraine_pain_flag + 
                    maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + 
                    any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_full)

# generate summary tables
dev_any_summary <- tbl_regression(dev_any, exponentiate = TRUE, 
                                  pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_valproate_summary <- tbl_regression(dev_valproate, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_topiramate_summary <- tbl_regression(dev_topiramate, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_carbamazepine_summary <- tbl_regression(dev_carbamazepine, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_lamotrigine_summary <- tbl_regression(dev_lamotrigine, exponentiate = TRUE, 
                                          pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_levetiracetam_summary <- tbl_regression(dev_levetiracetam, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_gabapentin_summary <- tbl_regression(dev_gabapentin, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_pregabalin_summary <- tbl_regression(dev_pregabalin, exponentiate = TRUE, 
                                         pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_full_adjusted <- bind_rows(dev_any_summary, dev_valproate_summary, dev_topiramate_summary,
                    dev_carbamazepine_summary, dev_lamotrigine_summary,
                    dev_levetiracetam_summary, dev_gabapentin_summary,
                    dev_pregabalin_summary)

write_csv(dev_full_adjusted, paste0(folder_data_path, "stats/dev_full_adjusted.csv"))


# # 3.2) Unadjusted models ------------------------------------------------------------
# 
# # any ASM
unadj_dev_any <- clogit(any_dev_concern ~ 
                    exposed_any_asm + 
                    strata(groupID),
                  data = dev_full)

# Valproate
unadj_dev_valproate <- clogit(any_dev_concern ~ 
                          exposed_valproate_mono + 
                          strata(groupID),
                        data = dev_full)

# Topiramate
unadj_dev_topiramate <- clogit(any_dev_concern ~ 
                           exposed_topiramate_mono + 
                           strata(groupID),
                         data = dev_full)

# Carbamazepine
unadj_dev_carbamazepine <- clogit(any_dev_concern ~ 
                              exposed_carbamazepine_mono + 
                              strata(groupID),
                            data = dev_full)


# with Lamotrigine
unadj_dev_lamotrigine <- clogit(any_dev_concern ~ 
                            exposed_lamotrigine_mono + 
                            strata(groupID),
                          data = dev_full)

# Levetiracetam
unadj_dev_levetiracetam <- clogit(any_dev_concern ~ 
                              exposed_levetiracetam_mono + 
                              strata(groupID),
                            data = dev_full)


# gabapentin
unadj_dev_gabapentin <- clogit(any_dev_concern ~ 
                           exposed_gabapentin_mono + 
                           strata(groupID),
                         data = dev_full)


# pregabalin
unadj_dev_pregabalin <- clogit(any_dev_concern ~ 
                           exposed_pregabalin_mono + 
                           strata(groupID),
                         data = dev_full)

# generate summary tables
unadj_dev_any_summary <- tbl_regression(unadj_dev_any, exponentiate = TRUE, 
                                        pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_valproate_summary <- tbl_regression(unadj_dev_valproate, exponentiate = TRUE, 
                                              pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_topiramate_summary <- tbl_regression(unadj_dev_topiramate, exponentiate = TRUE, 
                                               pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_carbamazepine_summary <- tbl_regression(unadj_dev_carbamazepine, exponentiate = TRUE, 
                                                  pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_lamotrigine_summary <- tbl_regression(unadj_dev_lamotrigine, exponentiate = TRUE, 
                                                pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_levetiracetam_summary <- tbl_regression(unadj_dev_levetiracetam, exponentiate = TRUE, 
                                                  pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_gabapentin_summary <- tbl_regression(unadj_dev_gabapentin, exponentiate = TRUE, 
                                               pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_pregabalin_summary <- tbl_regression(unadj_dev_pregabalin, exponentiate = TRUE, 
                                               pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_full <- bind_rows(unadj_dev_any_summary, unadj_dev_valproate_summary, unadj_dev_topiramate_summary,
                            unadj_dev_carbamazepine_summary, unadj_dev_lamotrigine_summary,
                            unadj_dev_levetiracetam_summary, unadj_dev_gabapentin_summary,
                            unadj_dev_pregabalin_summary)

write_csv(unadj_dev_full, paste0(folder_data_path, "stats/dev_full_unadjusted.csv"))

# 4) Models - epilepsy only cohort---------------------------------------------------------------

# based on n events we can do the full adjusted model for some ASMs in the full developmental cohort
# topiramate and gabapentin require unadjusted models
# pregabalin requires adjusted model with m.age, m.deprivation, m.bmi and m.smoking collapsed

# 4.1) Adjusted models ------------------------------------------------------------

# any ASM
dev_ep_any <- clogit(any_dev_concern ~ exposed_any_asm + epilepsy_indication + mh_flag + migraine_pain_flag + maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_ep)


# valproate
dev_ep_valproate <- clogit(any_dev_concern ~ exposed_valproate_mono + epilepsy_indication + mh_flag + migraine_pain_flag + maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_ep)



# carbamazepine
dev_ep_carbamazepine <- clogit(any_dev_concern ~ exposed_carbamazepine_mono + epilepsy_indication + mh_flag + migraine_pain_flag + maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_ep)



# lamotrigine
dev_ep_lamotrigine <- clogit(any_dev_concern ~ exposed_lamotrigine_mono + epilepsy_indication + mh_flag + migraine_pain_flag + maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_ep)



# levetiracetam
dev_ep_levetiracetam <- clogit(any_dev_concern ~ exposed_levetiracetam_mono + epilepsy_indication + mh_flag + migraine_pain_flag + maternal_age_group_conception + mother_simd + drug_alcohol_use +
                    baby_sex + maternal_bmi_group + maternal_smoking + any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_ep)


# Pregabalin
dev_ep_pregabalin <- clogit(any_dev_concern ~ exposed_pregabalin_mono + epilepsy_indication + mh_flag + migraine_pain_flag + maternal_age_group_conception + mother_simd_collapsed + drug_alcohol_use +
                    baby_sex + maternal_bmi_group_collapsed + maternal_smoking_collapsed + any_smr_comorb + parity +
                    strata(groupID),
                  data = dev_ep)


# generate summary tables
dev_ep_any_summary <- tbl_regression(dev_ep_any, exponentiate = TRUE, 
                                     pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_ep_valproate_summary <- tbl_regression(dev_ep_valproate, exponentiate = TRUE, 
                                           pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_ep_carbamazepine_summary <- tbl_regression(dev_ep_carbamazepine, exponentiate = TRUE, 
                                               pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_ep_lamotrigine_summary <- tbl_regression(dev_ep_lamotrigine, exponentiate = TRUE, 
                                             pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_ep_levetiracetam_summary <- tbl_regression(dev_ep_levetiracetam, exponentiate = TRUE, 
                                               pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_ep_pregabalin_summary <- tbl_regression(dev_ep_pregabalin, exponentiate = TRUE, 
                                            pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

dev_ep_full <- bind_rows(dev_ep_any_summary, dev_ep_valproate_summary,
                       dev_ep_carbamazepine_summary, dev_ep_lamotrigine_summary,
                       dev_ep_levetiracetam_summary, 
                       dev_ep_pregabalin_summary)

write_csv(dev_ep_full, paste0(folder_data_path, "stats/dev_ep_full_adjusted.csv"))


# 4.2) Unadjusted models ------------------------------------------------------------

# any ASM
unadj_dev_ep_any <- clogit(any_dev_concern ~ exposed_any_asm + 
                       strata(groupID),
                     data = dev_ep)


# valproate
unadj_dev_ep_valproate <- clogit(any_dev_concern ~ exposed_valproate_mono + 
                             strata(groupID),
                           data = dev_ep)

# topiramate
unadj_dev_ep_topiramate <- clogit(any_dev_concern ~ exposed_topiramate_mono + 
                             strata(groupID),
                           data = dev_ep)


# carbamazepine
unadj_dev_ep_carbamazepine <- clogit(any_dev_concern ~ exposed_carbamazepine_mono + 
                                 strata(groupID),
                               data = dev_ep)



# lamotrigine
unadj_dev_ep_lamotrigine <- clogit(any_dev_concern ~ exposed_lamotrigine_mono +
                               strata(groupID),
                             data = dev_ep)



# levetiracetam
unadj_dev_ep_levetiracetam <- clogit(any_dev_concern ~ exposed_levetiracetam_mono + 
                                 strata(groupID),
                               data = dev_ep)


# Gabapentin
unadj_dev_ep_gabapentin <- clogit(any_dev_concern ~ exposed_gabapentin_mono + 
                              strata(groupID),
                            data = dev_ep)

# Pregabalin
unadj_dev_ep_pregabalin <- clogit(any_dev_concern ~ exposed_pregabalin_mono + 
                              strata(groupID),
                            data = dev_ep)


# generate summary tables
unadj_dev_ep_any_summary <- tbl_regression(unadj_dev_ep_any, exponentiate = TRUE, 
                                           pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_ep_valproate_summary <- tbl_regression(unadj_dev_ep_valproate, exponentiate = TRUE, 
                                                 pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_ep_topiramate_summary <- tbl_regression(unadj_dev_ep_topiramate, exponentiate = TRUE, 
                                                  pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_ep_carbamazepine_summary <- tbl_regression(unadj_dev_ep_carbamazepine, exponentiate = TRUE, 
                                                     pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_ep_lamotrigine_summary <- tbl_regression(unadj_dev_ep_lamotrigine, exponentiate = TRUE, 
                                                   pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_ep_levetiracetam_summary <- tbl_regression(unadj_dev_ep_levetiracetam, exponentiate = TRUE, 
                                                     pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_ep_gabapentin_summary <- tbl_regression(unadj_dev_ep_gabapentin, exponentiate = TRUE, 
                                                  pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_ep_pregabalin_summary <- tbl_regression(unadj_dev_ep_pregabalin, exponentiate = TRUE, 
                                                  pvalue_fun = custom_pvalue_fun) %>%
  gtsummary::as_tibble() %>%
  filter(row_number()==1) %>%
  janitor::clean_names() %>%
  rename(ci = x95_percent_ci)

unadj_dev_ep_full <- bind_rows(unadj_dev_ep_any_summary, unadj_dev_ep_valproate_summary, unadj_dev_ep_topiramate_summary,
                               unadj_dev_ep_carbamazepine_summary, unadj_dev_ep_lamotrigine_summary,
                               unadj_dev_ep_levetiracetam_summary, unadj_dev_ep_gabapentin_summary,
                               unadj_dev_ep_pregabalin_summary)

write_csv(unadj_dev_ep_full, paste0(folder_data_path, "stats/dev_ep_full_unadjusted.csv"))


