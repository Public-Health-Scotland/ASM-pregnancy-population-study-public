
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)
library(forcats)
library(readxl)
library(patchwork)

source("supp_materials_publication/00.supp_setup.r")

##pregnancy outcomes###
###load adjusted individual covariates models####
# Adjusted Pregnancy cohort files
p_full_adjusted <- read_csv(paste0(folder_data_path, "stats/p_full_adjusted.csv")) %>%
  mutate(outcome = "Pregnancy outcome", model = "adjusted") 
p_full_unadjusted <- read_csv(paste0(folder_data_path, "stats/p_full_unadjusted.csv"))%>%
  mutate(outcome = "Pregnancy outcome",  model = "crude") 

cc_full_adjusted <- read_csv(paste0(folder_data_path, "stats/cc_full_adjusted.csv"))%>%
  mutate(outcome = "Congenital condition", model = "adjusted") 
cc_full_unadjusted <- read_csv(paste0(folder_data_path, "stats/cc_full_unadjusted.csv"))%>%
  mutate(outcome = "Congenital condition",  model = "crude") 
dev_full_adjusted <- read_csv(paste0(folder_data_path, "stats/dev_full_adjusted.csv")) %>%
  mutate(outcome= "Developmental concern", model = "adjusted") 
dev_full_unadjusted <- read_csv(paste0(folder_data_path, "stats/dev_full_unadjusted.csv")) %>%
  mutate(outcome= "Developmental concern", model = "crude") 

full_covars <- rbind(p_full_adjusted,p_full_unadjusted, cc_full_adjusted,
                     cc_full_unadjusted, dev_full_adjusted, dev_full_unadjusted)
# prepare df for plot
full_covars <- full_covars %>% 
  # split CI object into two; low and high
  mutate(conf.low = str_sub(ci, start = 1,4),
         conf.high = str_sub(ci, start = -4)) %>% 
  # adjust variable types
  mutate(or = as.numeric(or),
         conf.low = as.numeric(conf.low),
         conf.high = as.numeric(conf.high)) %>% 
  # rename ASM categories
  mutate(exposure = case_when(characteristic == 'exposed_any_asm' ~ 'Any ASM',
                              characteristic == 'exposed_valproate_mono' ~ 'Valproate',
                              characteristic == 'exposed_topiramate_mono' ~ 'Topiramate',
                              characteristic == 'exposed_carbamazepine_mono' ~ 'Carbamazepine',
                              characteristic == 'exposed_lamotrigine_mono' ~ 'Lamotrigine',
                              characteristic == 'exposed_levetiracetam_mono' ~ 'Levetiracetam',
                              characteristic == 'exposed_gabapentin_mono' ~ 'Gabapentin',
                              characteristic == 'exposed_pregabalin_mono' ~ 'Pregabalin',
                              characteristic == 'CC_exposed_any_asm' ~ 'Any ASM',
                              characteristic == 'CC_exposed_valproate_mono' ~ 'Valproate',
                              characteristic == 'CC_exposed_topiramate_mono' ~ 'Topiramate',
                              characteristic == 'CC_exposed_carbamazepine_mono' ~ 'Carbamazepine',
                              characteristic == 'CC_exposed_lamotrigine_mono' ~ 'Lamotrigine',
                              characteristic == 'CC_exposed_levetiracetam_mono' ~ 'Levetiracetam',
                              characteristic == 'CC_exposed_gabapentin_mono' ~ 'Gabapentin',
                              characteristic == 'CC_exposed_pregabalin_mono' ~ 'Pregabalin')) %>% 
  
  # Change ASM categories object to factor to control position in plot
  mutate(exposure = fct_relevel(exposure, "Pregabalin", "Gabapentin", "Levetiracetam", "Lamotrigine",
                                "Carbamazepine", "Topiramate", "Valproate", "Any ASM"))

##propensity models####
p_prop_results <- read.csv( paste0(folder_data_path, "stats/propensity_models_preg_outcomes.csv"))%>%
  filter(model_type=="doubley robust") %>%
  mutate(outcome = "Pregnancy outcome")%>%
  select( OR , `X2.5..`,`X97.5..`,  `pvalue`, variable, model_type, outcome )
cc_prop_results <- read.csv(paste0(folder_data_path, "stats/propensity_models_CC_outcomes.csv"))%>%
  mutate(outcome = "Congenital condition")%>%filter(model_type=="doubley robust") %>%
  select( OR , `X2.5..`,`X97.5..`,  `pvalue`, variable, model_type, outcome )
dev_prop_results <- read.csv(paste0(folder_data_path, "stats/propensity_models_development_outcomes.csv"))%>%
  mutate(outcome= "Developmental concern")%>%filter(model_type=="doubley robust") %>%
  select( OR , `X2.5..`,`X97.5..`,  `pvalue`, variable, model_type, outcome )

prop_results_all<- rbind(p_prop_results, cc_prop_results, dev_prop_results)
prop_results_all<- prop_results_all %>%
  # rename ASM categories
  mutate(exposure = case_when(variable == 'exposed_any_asm' ~ 'Any ASM',
                              variable == 'exposed_valproate_mono' ~ 'Valproate',
                              variable == 'exposed_topiramate_mono' ~ 'Topiramate',
                              variable == 'exposed_carbamazepine_mono' ~ 'Carbamazepine',
                              variable == 'exposed_lamotrigine_mono' ~ 'Lamotrigine',
                              variable == 'exposed_levetiracetam_mono' ~ 'Levetiracetam',
                              variable == 'exposed_gabapentin_mono' ~ 'Gabapentin',
                              variable == 'exposed_pregabalin_mono' ~ 'Pregabalin',
                              variable  == 'CC_exposed_any_asm' ~ 'Any ASM',
                              variable  == 'CC_exposed_valproate_mono' ~ 'Valproate',
                              variable  == 'CC_exposed_topiramate_mono' ~ 'Topiramate',
                              variable  == 'CC_exposed_carbamazepine_mono' ~ 'Carbamazepine',
                              variable  == 'CC_exposed_lamotrigine_mono' ~ 'Lamotrigine',
                              variable  == 'CC_exposed_levetiracetam_mono' ~ 'Levetiracetam',
                              variable  == 'CC_exposed_gabapentin_mono' ~ 'Gabapentin',
                              variable  == 'CC_exposed_pregabalin_mono' ~ 'Pregabalin')) %>% 
  # Change ASM categories object to factor to control position in plot
  mutate(exposure = fct_relevel(exposure, "Pregabalin", "Gabapentin", "Levetiracetam", "Lamotrigine",
                                "Carbamazepine", "Topiramate", "Valproate", "Any ASM")) %>%
  mutate(model  = case_when(model_type == "doubley robust" ~"Propensity score"))

prop_results_all <- prop_results_all %>% filter(model == "Propensity score") %>% 
  rename(p_value= pvalue) %>%
  rename(conf.low = `X2.5..`, conf.high = `X97.5..`) %>%
  mutate(p_value = as.character(p_value)) %>% 
  mutate(p_value = case_when(p_value == '0' ~ '<0.001',
                             T~p_value)) %>% 
  select(exposure, OR, conf.low, conf.high, p_value, model, outcome)

full_adjusted_all <- full_covars %>%
  rename(OR=or) %>%
  select(exposure, OR, conf.low, conf.high,p_value, model, outcome)

df_plots_all <-rbind(prop_results_all, full_adjusted_all)


saveRDS(df_plots_all,paste0(folder_data_path, "supplementary_materials/combined_plots_data.rds" ))
