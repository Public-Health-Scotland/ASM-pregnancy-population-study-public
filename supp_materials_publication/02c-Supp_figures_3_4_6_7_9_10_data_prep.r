
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
p_full_adjusted <- read_csv(paste0(folder_data_path, "stats/p_ep_full_adjusted.csv")) %>%
  mutate(outcome = "Pregnancy outcome", model = "adjusted") 
p_full_unadjusted <- read_csv(paste0(folder_data_path, "stats/p_ep_full_unadjusted.csv") )%>%
  mutate(outcome = "Pregnancy outcome",  model = "crude") 

cc_full_adjusted <- read_csv(paste0(folder_data_path, "stats/cc_ep_full_adjusted.csv")) %>%
  mutate(outcome = "Congenital condition", model = "adjusted") 
cc_full_unadjusted <- read_csv(paste0(folder_data_path, "stats/cc_ep_full_unadjusted.csv")) %>%
  mutate(outcome = "Congenital condition",  model = "crude") 
dev_full_adjusted <- read_csv(paste0(folder_data_path, "stats/dev_ep_full_adjusted.csv")) %>%
  mutate(outcome= "Developmental concern", model = "adjusted") 
dev_full_unadjusted <- read_csv(paste0(folder_data_path, "stats/dev_ep_full_unadjusted.csv")) %>%
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



full_adjusted_all <- full_covars %>%
  rename(OR=or) %>%
  select(exposure, OR, conf.low, conf.high,p_value, model, outcome)

df_plots_all <-rbind( full_adjusted_all)


saveRDS(df_plots_all,paste0(folder_data_path, "supplementary_materials/combined_plots_ep_data.rds" ))



###CC 12 weeks ####

cc_full_adjusted <- read_csv(paste0(folder_data_path, "stats/cc_12wks_full_adjusted.csv"))%>%
  mutate(outcome = "Congenital condition", model = "adjusted") 
cc_full_unadjusted <- read_csv(paste0(folder_data_path, "stats/cc_12wks_full_unadjusted.csv")) %>%
  mutate(outcome = "Congenital condition",  model = "crude") 


full_covars <- rbind(cc_full_adjusted,
                     cc_full_unadjusted)
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



full_adjusted_all <- full_covars %>%
  rename(OR=or) %>%
  select(exposure, OR, conf.low, conf.high,p_value, model, outcome)

saveRDS(full_adjusted_all,paste0(folder_data_path, "supplementary_materials/combined_plots_cc12wks_data.rds" ))



##stratified FA resuls

dev_stratified <- read_csv(paste0(folder_data_path, "stats/dev_stratified.csv")) %>%
  mutate(outcome = "Developmental concern")
p_stratified <- read_csv(paste0(folder_data_path, "stats/p_stratified.csv")) %>%
  mutate(outcome = "Pregnancy outcome")


full_covars <- rbind(dev_stratified ,
                     p_stratified)

# Read in the model outputs with the ineraction term
p_inter_full <- read_csv(paste0(folder_data_path, "stats/p_inter_full.csv")) %>%
  mutate(outcome="Pregnancy outcome", model = "fa_interaction")
dev_inter_full <- read_csv(paste0(folder_data_path, "stats/dev_inter_full.csv")) %>%
  mutate(outcome= "Developmental concern", model = "fa_interaction")

#  keep only the model and interaction p_value
p_inter_full <- p_inter_full %>% 
  filter(grepl("\\*", characteristic)) %>% 
  select(characteristic, p_value, outcome) %>% 
  rename(interaction_p_value = p_value) %>% 
  mutate(characteristic = sub(" .*", "", characteristic))
  

dev_inter_full <- dev_inter_full %>% 
  filter(grepl("\\*", characteristic)) %>% 
  select(characteristic, p_value, outcome) %>% 
  rename(interaction_p_value = p_value) %>% 
  mutate(characteristic = sub(" .*", "", characteristic))

full_inter <- rbind(p_inter_full,
                    dev_inter_full)

# Add to the model df
full_covars <- left_join(full_covars, full_inter, by = c("characteristic", "outcome"))

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



full_adjusted_all <- full_covars %>%
  rename(OR=or) %>%
  select(exposure, OR, conf.low, conf.high,p_value, interaction_p_value, FA, outcome)

saveRDS(full_adjusted_all,
        paste0(folder_data_path, "supplementary_materials/combined_plots_hdFA_strata_data.rds" ))

