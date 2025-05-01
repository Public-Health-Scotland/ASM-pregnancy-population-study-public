library(tidyverse)
source("supp_materials_publication/00.supp_setup.r")


# ST4 - Table 2 equivalent for propensity score results
st_4_p <- read.csv( paste0(folder_data_path, "stats/propensity_models_preg_outcomes.csv"))%>%
filter(model_type=="doubley robust") %>%
  mutate(outcome = "Pregnancy loss") %>%
  select(c(variable, outcome, 
           pregnancy_lossNo_0, pregnancy_lossYes_0, 
           pregnancy_lossNo_1, pregnancy_lossYes_1,
           perc_not_loss_0, perc_loss_0, 
           perc_not_loss_1, perc_loss_1)) %>% 
  rename(ASM = variable,
         cohort = outcome,
         unexposed_n_no_outcome = pregnancy_lossNo_0,
         unexposed_n_outcome = pregnancy_lossYes_0,
         exposed_n_no_outcome = pregnancy_lossNo_1,
         exposed_n_outcome = pregnancy_lossYes_1,
         unexposed_percent_no_outcome = perc_not_loss_0,
         unexposed_percent_outcome = perc_loss_0 ,
         exposed_percent_no_outcome = perc_not_loss_1,
         exposed_percent_outcome = perc_loss_1)

st_4_cc <- read.csv(paste0(folder_data_path, "stats/propensity_models_CC_outcomes.csv"))%>%
  filter(model_type=="doubley robust") %>%
  mutate(outcome = "Congenital conditions") %>%
  select(c(variable, outcome, 
           any_CC0_0, any_CC1_0, 
           any_CC0_1, any_CC1_1,
           perc_noCC_0, perc_CC_0, 
           perc_noCC_1, perc_CC_1)) %>% 
  rename(ASM = variable,
         cohort = outcome,
         unexposed_n_no_outcome = any_CC0_0,
         unexposed_n_outcome = any_CC1_0,
         exposed_n_no_outcome = any_CC0_1,
         exposed_n_outcome = any_CC1_1,
         unexposed_percent_no_outcome = perc_noCC_0,
         unexposed_percent_outcome = perc_CC_0 ,
         exposed_percent_no_outcome = perc_noCC_1,
         exposed_percent_outcome = perc_CC_1)

st_4_dev <- read.csv(paste0(folder_data_path, "stats/propensity_models_development_outcomes.csv"))%>%
  filter(model_type=="doubley robust") %>%
  mutate(outcome = "Early childhood developmental concerns") %>%
  select(c(variable, outcome, 
           any_dev_concernN_0, any_dev_concernY_0, 
           any_dev_concernN_1, any_dev_concernY_1,
           perc_No_concern_0, perc_concern_0, 
           perc_No_concern_1, perc_concern_1)) %>% 
  rename(ASM = variable,
         cohort = outcome,
         unexposed_n_no_outcome = any_dev_concernN_0,
         unexposed_n_outcome = any_dev_concernY_0,
         exposed_n_no_outcome = any_dev_concernN_1,
         exposed_n_outcome = any_dev_concernY_1,
         unexposed_percent_no_outcome = perc_No_concern_0,
         unexposed_percent_outcome = perc_concern_0 ,
         exposed_percent_no_outcome = perc_No_concern_1,
         exposed_percent_outcome = perc_concern_1)

st_4 <- rbind(st_4_p, st_4_cc, st_4_dev)

rm(st_4_p, st_4_cc, st_4_dev)

st_4_table <- st_4 %>% 
  mutate(unexposed_total = unexposed_n_no_outcome + unexposed_n_outcome,
         exposed_total = exposed_n_no_outcome + exposed_n_outcome) %>% 
  select(c(ASM, cohort, 
           exposed_total, exposed_n_outcome, exposed_percent_outcome, 
           unexposed_total, unexposed_n_outcome, unexposed_percent_outcome)) %>% 
  mutate(ASM = case_when(ASM == 'exposed_any_asm' ~ 'Any ASM',
                              ASM == 'exposed_valproate_mono' ~ 'Valproate',
                              ASM == 'exposed_topiramate_mono' ~ 'Topiramate',
                              ASM == 'exposed_carbamazepine_mono' ~ 'Carbamazepine',
                              ASM == 'exposed_lamotrigine_mono' ~ 'Lamotrigine',
                              ASM == 'exposed_levetiracetam_mono' ~ 'Levetiracetam',
                              ASM == 'exposed_gabapentin_mono' ~ 'Gabapentin',
                              ASM == 'exposed_pregabalin_mono' ~ 'Pregabalin',
                              ASM  == 'CC_exposed_any_asm' ~ 'Any ASM',
                              ASM  == 'CC_exposed_valproate_mono' ~ 'Valproate',
                              ASM  == 'CC_exposed_topiramate_mono' ~ 'Topiramate',
                              ASM  == 'CC_exposed_carbamazepine_mono' ~ 'Carbamazepine',
                              ASM  == 'CC_exposed_lamotrigine_mono' ~ 'Lamotrigine',
                              ASM  == 'CC_exposed_levetiracetam_mono' ~ 'Levetiracetam',
                              ASM  == 'CC_exposed_gabapentin_mono' ~ 'Gabapentin',
                              ASM  == 'CC_exposed_pregabalin_mono' ~ 'Pregabalin')) %>% 
  group_by(cohort) %>% 
  arrange(match(ASM, c('Any ASM', 'Valproate', 'Topiramate', 'Carbamazepine', 'Lamotrigine', 'Levetiracetam', 'Gabapentin', 'Pregabalin'))) %>% 
  ungroup() # maintain correct order

# Save file out for loading in markdown document
write_csv(st_4_table, paste0(folder_data_path, "supplementary_materials/supp_table4.csv"))


# ST5 - Table 2 equivalent for results restricting to women with epilepsy
# Read in data
# file created in /stat_analyses/01-extract_events.R
st_5 <- read.csv( paste0(folder_data_path, "/stats/n_exp_unexp.csv"))

# Prepare df for re-shaping
st_5_table <- st_5 %>% 
  filter(group == 'epilepsy') %>% # keep to the main analyses cohort
  mutate(exposure = str_extract(indicator, "Exposed")) %>% 
  mutate(exposure = case_when(is.na(exposure) ~ 'Unexposed',
                              T~exposure)) %>% # flag exposed/unexposed cohort
  select(c(ASM, cohort, exposure, total, n_outcome, percent_outcome))

# Re-shape df into required format for markdown table
st_5_table <- st_5_table %>% 
  pivot_wider(names_from = exposure,
              names_glue = "{exposure}_{.value}",
              values_from = c(total, n_outcome, percent_outcome)) %>% # have separate n and % variables for exposed/unexposed groups
  select(c(ASM, cohort, Exposed_total, Exposed_n_outcome, Exposed_percent_outcome, Unexposed_total, Unexposed_n_outcome, Unexposed_percent_outcome)) %>% 
  group_by(cohort) %>% 
  arrange(match(ASM, c('Any ASM', 'Valproate', 'Topiramate', 'Carbamazepine', 'Lamotrigine', 'Levetiracetam', 'Gabapentin', 'Pregabalin'))) %>% 
  ungroup() # maintain correct order

# Save file out for loading in markdown document
write_csv(st_5_table, paste0(folder_data_path, "supplementary_materials/supp_table5.csv"))


# ST7 - Table 2 equivalent for hdFA stratified results
# Read in data
# file created in /stat_analyses/01-extract_events.R
st_7 <- read.csv( paste0(folder_data_path, "stats/n_exp_unexp_fa_strat.csv"))

# Prepare df for re-shaping
st_7_table <- st_7 %>% 
  mutate(exposure = str_extract(indicator, "Exposed")) %>% 
  mutate(exposure = case_when(is.na(exposure) ~ 'Unexposed',
                              T~exposure)) %>% # flag exposed/unexposed cohort
  select(c(ASM, cohort, high_dose_folic_acid, exposure, total, n_outcome, percent_outcome))

# Re-shape df into required format for markdown table
st_7_table <- st_7_table %>% 
  pivot_wider(names_from = exposure,
              names_glue = "{exposure}_{.value}",
              values_from = c(total, n_outcome, percent_outcome)) %>% # have separate n and % variables for exposed/unexposed groups
  select(c(ASM, cohort, high_dose_folic_acid, Exposed_total, Exposed_n_outcome, Exposed_percent_outcome, Unexposed_total, Unexposed_n_outcome, Unexposed_percent_outcome)) %>% 
  mutate(high_dose_folic_acid = case_when(high_dose_folic_acid == 0 ~ 'No',
                                          T~ 'Yes')) %>% 
  group_by(cohort) %>% 
  arrange(match(ASM, c('Any ASM', 'Valproate', 'Topiramate', 'Carbamazepine', 'Lamotrigine', 'Levetiracetam', 'Gabapentin', 'Pregabalin'))) %>% 
  ungroup() # maintain correct order

# Save file out for loading in markdown document
write_csv(st_7_table, paste0(folder_data_path, "supplementary_materials/supp_table7.csv"))


# ST9 - Table 2 equivalent for results restricting to women with epilepsy
# Read in data
# file created in /stat_analyses/01-extract_events.R
st_9 <- read.csv( paste0(folder_data_path, "/stats/n_exp_unexp.csv"))

# Prepare df for re-shaping
st_9_table <- st_9 %>% 
  filter(group == '12wks') %>% # keep to the main analyses cohort
  mutate(exposure = str_extract(indicator, "Exposed")) %>% 
  mutate(exposure = case_when(is.na(exposure) ~ 'Unexposed',
                              T~exposure)) %>% # flag exposed/unexposed cohort
  select(c(ASM, cohort, exposure, total, n_outcome, percent_outcome))

# Re-shape df into required format for markdown table
st_9_table <- st_9_table %>% 
  pivot_wider(names_from = exposure,
              names_glue = "{exposure}_{.value}",
              values_from = c(total, n_outcome, percent_outcome)) %>% # have separate n and % variables for exposed/unexposed groups
  select(c(ASM, cohort, Exposed_total, Exposed_n_outcome, Exposed_percent_outcome, Unexposed_total, Unexposed_n_outcome, Unexposed_percent_outcome)) %>% 
  group_by(cohort) %>% 
  arrange(match(ASM, c('Any ASM', 'Valproate', 'Topiramate', 'Carbamazepine', 'Lamotrigine', 'Levetiracetam', 'Gabapentin', 'Pregabalin'))) %>% 
  ungroup() # maintain correct order

# Save file out for loading in markdown document
write_csv(st_9_table, paste0(folder_data_path, "supplementary_materials/supp_table9.csv"))
