###Folic acid tables data prep
library(dplyr)
library(readr)
library(arrow)
library(stringr)
library(forcats)
source("supp_materials_publication/00.supp_setup.r")

##Model results for main cohort and interactions####
###interaction results####
p_inter_full <- read_csv(paste0(folder_data_path , "stats/p_inter_full.csv")) %>%
  mutate(outcome="Pregnancy loss", model = "fa_interaction")
cc_inter_full <- read_csv(paste0(folder_data_path ,"stats/cc_inter_full.csv"))%>%
  mutate(outcome = "Congenital conditions", model = "fa_interaction")
dev_inter_full <- read_csv(paste0(folder_data_path ,"stats/dev_inter_full.csv"))%>%
  mutate(outcome= "Early childhood developmental concerns", model = "fa_interaction")

fa_mods <-rbind(p_inter_full, cc_inter_full, dev_inter_full) %>%
  mutate(folic = case_when(str_detect(characteristic, "folic")~ 1,T~0)) %>%
  mutate(exposure= case_when( substr(characteristic,9,11)=="any" & folic ==0 ~"Any ASM", 
                              substr(characteristic,9,11)=="val" & folic ==0 ~"Valproate",
                              substr(characteristic,9,11)=="top" & folic ==0 ~ "Topiramate", 
                              substr(characteristic,9,11)=="car" & folic ==0 ~ "Carbamazepine", 
                              substr(characteristic,9,11)=="lam" & folic ==0 ~ "Lamotrigine" , 
                              substr(characteristic,9,11)=="lev" & folic ==0 ~ "Levetiracetam" , 
                              substr(characteristic,9,11)=="gab" & folic ==0 ~  "Gabapentin", 
                              substr(characteristic,9,11)=="pre"  & folic ==0~ "Pregabalin" ,
                              substr(characteristic,12,14)=="any" & folic ==0 ~"Any ASM", 
                              substr(characteristic,12,14)=="val" & folic ==0 ~"Valproate",
                              substr(characteristic,12,14)=="top" & folic ==0 ~ "Topiramate", 
                              substr(characteristic,12,14)=="car" & folic ==0 ~ "Carbamazepine", 
                              substr(characteristic,12,14)=="lam" & folic ==0 ~ "Lamotrigine" , 
                              substr(characteristic,12,14)=="lev" & folic ==0 ~ "Levetiracetam" , 
                              substr(characteristic,12,14)=="gab" & folic ==0 ~  "Gabapentin", 
                              substr(characteristic,12,14)=="pre"  & folic ==0~ "Pregabalin" ,
                              substr(characteristic,9,11)=="any" & folic ==1 ~"Any ASM x hdFA", 
                              substr(characteristic,9,11)=="val" & folic ==1 ~"Valproate x hdFA", 
                              substr(characteristic,9,11)=="top" & folic ==1 ~ "Topiramate x hdFA", 
                              substr(characteristic,9,11)=="car" & folic ==1 ~ "Carbamazepine x hdFA",  
                              substr(characteristic,9,11)=="lam" & folic ==1 ~ "Lamotrigine x hdFA", 
                              substr(characteristic,9,11)=="lev" & folic ==1 ~ "Levetiracetam x hdFA", 
                              substr(characteristic,9,11)=="gab" & folic ==1 ~  "Gabapentin x hdFA",  
                              substr(characteristic,9,11)=="pre"  & folic ==1~ "Pregabalin x hdFA", 
                              substr(characteristic,12,14)=="any" & folic ==1 ~"Any ASM x hdFA", 
                              substr(characteristic,12,14)=="val" & folic ==1 ~"Valproate x hdFA", 
                              substr(characteristic,12,14)=="top" & folic ==1 ~ "Topiramate x hdFA", 
                              substr(characteristic,12,14)=="car" & folic ==1 ~ "Carbamazepine x hdFA",  
                              substr(characteristic,12,14)=="lam" & folic ==1 ~ "Lamotrigine x hdFA", 
                              substr(characteristic,12,14)=="lev" & folic ==1 ~ "Levetiracetam x hdFA", 
                              substr(characteristic,12,14)=="gab" & folic ==1 ~  "Gabapentin x hdFA",  
                              substr(characteristic,12,14)=="pre"  & folic ==1~ "Pregabalin x hdFA")) %>% 
  select(-characteristic, -folic)

###main model results####
# Adjusted Pregnancy cohort files
p_full_adjusted <- read_csv(paste0(folder_data_path ,"stats/p_full_adjusted.csv")) %>%
  mutate(outcome = "Pregnancy loss", model = "adjusted") 
p_full_unadjusted <- read_csv(paste0(folder_data_path , "stats/p_full_unadjusted.csv"))%>%
  mutate(outcome = "Pregnancy loss",  model = "crude") 

cc_full_adjusted <- read_csv(paste0(folder_data_path ,"stats/cc_full_adjusted.csv"))%>%
  mutate(outcome = "Congenital conditions", model = "adjusted") 
cc_full_unadjusted <- read_csv(paste0(folder_data_path ,"stats/cc_full_unadjusted.csv"))%>%
  mutate(outcome = "Congenital conditions",  model = "crude") 
dev_full_adjusted <- read_csv(paste0(folder_data_path ,"stats/dev_full_adjusted.csv")) %>%
  mutate(outcome= "Early childhood developmental concerns", model = "adjusted") 
dev_full_unadjusted <- read_csv(paste0(folder_data_path ,"stats/dev_full_unadjusted.csv")) %>%
  mutate(outcome= "Early childhood developmental concerns", model = "crude") 

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
                                "Carbamazepine", "Topiramate", "Valproate", "Any ASM")) %>%
  select(-c( characteristic)) %>%
  select(outcome, exposure, model,or, ci, p_value)

names(full_covars)
names(fa_mods)

##baind and save all model results
df_mods <- rbind(full_covars, fa_mods)
write.csv(df_mods ,
         paste0(folder_data_path ,"supplementary_materials/supp_table6_model_outputs.csv"))


##main cohorts####
##cohorts (main) for cohort numbers
p_full <- arrow::read_parquet(paste0(folder_data_path ,"stats/temp_preg_data.parquet")) %>%
   mutate(outcome = "Pregnancy loss") %>%
  select(pregnancy_id, outcome, starts_with("exposed_"), starts_with("CC_exposed"),
         groupID,
         unexposed,high_dose_folic_acid, pregnancy_loss) 
cc_full <- arrow::read_parquet(paste0(folder_data_path ,'stats/temp_cc_data.parquet'))%>%
  mutate(outcome = "Congenital conditions") %>%
  select(pregnancy_id,  outcome,starts_with("exposed_"), starts_with("CC_exposed"), groupID,
         unexposed,high_dose_folic_acid, any_CC) 
dev_full <- arrow::read_parquet(paste0(folder_data_path ,'stats/temp_dev_data.parquet')) %>%
  mutate(outcome="Early childhood developmental concerns") %>%
  select(pregnancy_id, outcome, starts_with("exposed_"), starts_with("CC_exposed"), groupID,
         unexposed,high_dose_folic_acid, any_dev_concern) 

table(p_full$pregnancy_loss)
p_full <- p_full %>% mutate(has_outcome = case_when(pregnancy_loss=="Yes" ~ "Yes", T~"No")) %>%
  select(-pregnancy_loss)
cc_full <- cc_full %>% mutate(has_outcome = case_when(any_CC==1 ~ "Yes", any_CC==0 ~"No")) %>%
  select(-any_CC)
dev_full <- dev_full %>% mutate(has_outcome = case_when(any_dev_concern==1 ~ "Yes", any_dev_concern==0 ~"No")) %>%
  select(-any_dev_concern)
#################################
cohorts_full <- rbind(p_full, cc_full, dev_full) 
cohorts_full <- cohorts_full %>% 
  mutate(exposure = case_when(outcome !="Congenital conditions" &
                              exposed_valproate_mono==1 ~ 'Valproate',
                            outcome !="Congenital conditions" &
                              exposed_topiramate_mono==1 ~ 'Topiramate',
                            outcome !="Congenital conditions" &
                              exposed_carbamazepine_mono==1 ~ 'Carbamazepine',
                            outcome !="Congenital conditions" &
                              exposed_lamotrigine_mono==1 ~ 'Lamotrigine',
                            outcome !="Congenital conditions" &
                              exposed_levetiracetam_mono==1 ~ 'Levetiracetam',
                            outcome !="Congenital conditions" &
                              exposed_gabapentin_mono==1 ~ 'Gabapentin',
                            outcome !="Congenital conditions" &
                              exposed_pregabalin_mono==1 ~ 'Pregabalin',
                            outcome =="Congenital conditions" &
                              CC_exposed_valproate_mono==1 ~ 'Valproate',
                            outcome =="Congenital conditions" &
                              CC_exposed_topiramate_mono==1 ~ 'Topiramate',
                            outcome =="Congenital conditions" & 
                              CC_exposed_carbamazepine_mono==1 ~ 'Carbamazepine',
                            outcome == "Congenital conditions" &
                              CC_exposed_lamotrigine_mono==1 ~ 'Lamotrigine',
                            outcome == "Congenital conditions" &
                              CC_exposed_levetiracetam_mono==1 ~ 'Levetiracetam',
                            outcome == "Congenital conditions" &
                              CC_exposed_gabapentin_mono==1 ~ 'Gabapentin',
                            outcome == "Congenital conditions" &
                              CC_exposed_pregabalin_mono==1 ~ 'Pregabalin')) 

cohorts_full <- cohorts_full %>% 
  group_by(outcome, groupID) %>%
  mutate(group_exposure=hablar::max_(exposure)) %>% ungroup()

##any_asm####
exposed <- cohorts_full %>% 
  mutate(exposed_any_asm = case_when(outcome=="Congenital conditions" & CC_exposed_any_asm==1 ~1, 
                                     outcome=="Congenital conditions" & CC_exposed_any_asm==0 ~0,
                                     T~exposed_any_asm)) %>%
    group_by(outcome,exposed_any_asm) %>% summarise(total = n())
out_by_exposed <- cohorts_full %>% group_by(outcome,exposed_any_asm, has_outcome) %>% count() 
df <- left_join(exposed, out_by_exposed)   %>%
  mutate(percent_outcome = n/total *100) %>% filter(has_outcome=="Yes") %>%
  mutate(exposed_any_asm = case_when(exposed_any_asm==0 ~ "unexposed", T~ "exposed")) %>%
  mutate(group_exposure="Any ASM")#%>%
#  pivot_wider(names_from = (exposed_any_asm), values_from = c(total, n, percent_outcome))

exposed <- cohorts_full %>% 
  filter(!is.na(group_exposure)) %>%
           group_by(outcome,group_exposure,exposure) %>%
  summarise(total = n())

###sort this so that for CC we group on CC_exposed
out_by_exposed <- cohorts_full %>% 
  filter(!is.na(group_exposure)) %>%
  group_by(outcome,group_exposure,exposure, has_outcome) %>% count() 
df_mono <- left_join(exposed, out_by_exposed)   %>%
  mutate(percent_outcome = n/total *100) %>% filter(has_outcome=="Yes") %>%
  mutate(exposed_any_asm = case_when(is.na(exposure) ~  "unexposed", T~ "exposed")) 

names(df)
names(df_mono)

df_all <- bind_rows(df, df_mono)
write.csv(df_all ,
          paste0(folder_data_path ,'supplementary_materials/supp_table6_n_events.csv'))







