# Supp10 - Outcomes by specific developmental outcome -------------------------

# Any
# Speech, language and communication
# Gross motor
# Fine motor
# Problem solving
# Personal/social
# Emotional/behavioural
# No early childhood developmental concern
# Unknown early childhood developmental concern status
source("supp_materials_publication/00.supp_setup.r")


data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_developmental_cohort.rds")) %>%
  mutate(control_any_dev_rev = case_when(exposed_any_asm==0 & est_date_conception <= as.Date("2020-07-01") &
                                           pregnancy_loss=="No"  ~1, T~0),
         cases_any_dev_rev = case_when(exposed_any_asm==1 & est_date_conception <=  as.Date("2020-07-01") &
                                         pregnancy_loss=="No" ~1, T~0)) %>%
  filter(control_any_dev_rev == 1 | cases_any_dev_rev == 1) %>% # restrict to only those eligible for inclusion in the developmental cohort
  mutate(exposed_drug = (case_when(exposed_valproate_mono == 1 ~ "valproate",  
                                   exposed_topiramate_mono == 1 ~ "topiramate",
                                   exposed_carbamazepine_mono == 1 ~ "carbamazepine",
                                   exposed_lamotrigine_mono == 1 ~ "lamotrigine",
                                   exposed_levetiracetam_mono == 1 ~ "levetiracetam",
                                   exposed_gabapentin_mono == 1 ~ "gabapentin",
                                   exposed_pregabalin_mono == 1 ~ "pregabalin",
                                   T ~ NA)))%>%
  ##count all unknown dev. for those with and without reviews
  mutate(any_dev_excl_v_h= case_when(is.na(any_dev_excl_v_h) ~"U", T~any_dev_excl_v_h )) 



# Specific concerns
aggregate_outcome <- function(data, ind, indicator_name) {
  df <- data %>% filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 | 
                          exposed_carbamazepine_mono ==1 | exposed_lamotrigine_mono ==1 | 
                          exposed_levetiracetam_mono ==1
                        | exposed_gabapentin_mono==1 | exposed_pregabalin_mono==1) %>% 
    group_by(exposed_drug, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                      {{ind}} == 'Y' ~ "yes",
                                      {{ind}} == 'unknown' ~ "unknown"))) 
  
  df <- bind_rows(df, 
                  (data %>% filter(exposed_any_asm == 1) %>% 
                     group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
                     mutate(indicator = indicator_name,
                            exposed_drug = "any_asm",
                            sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                                       {{ind}} == 'Y' ~ "yes",
                                                       {{ind}} == 'unknown' ~ "unknown"))))) %>%
    
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(percent_valproate = valproate/sum(valproate)*100,
           percent_topiramate = topiramate/sum(topiramate)*100,
           percent_carbamazepine = carbamazepine/sum(carbamazepine)*100,
           percent_lamotrigine = lamotrigine/sum(lamotrigine)*100,
           percent_levetiracetam = levetiracetam/sum(levetiracetam)*100,
           percent_gabapentin = gabapentin/sum(gabapentin)*100,
           percent_pregabalin = pregabalin/sum(pregabalin)*100,
           percent_any_asm = any_asm/sum(any_asm)*100)
}


slc <- aggregate_outcome(data, dev_slc_flag, "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_outcome(data, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_outcome(data, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_outcome(data, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_outcome(data, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_outcome(data, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))


# Any / No dev concern
aggregate_outcome2 <- function(data, ind, indicator_name) {
  df <- data %>% filter(exposed_valproate_mono == 1 | exposed_topiramate_mono == 1 |
                          exposed_carbamazepine_mono ==1 | exposed_lamotrigine_mono ==1 |
                          exposed_levetiracetam_mono ==1 | exposed_gabapentin_mono ==1 | exposed_pregabalin_mono == 1) %>% 
    group_by(exposed_drug, {{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                      {{ind}} == 'Y' ~ "yes",
                                      {{ind}} == 'U' ~ "unknown"))) 
  
  df <- bind_rows(df, 
                  (data %>% filter(exposed_any_asm == 1) %>% 
                     group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
                     mutate(indicator = indicator_name,
                            exposed_drug = "any_asm",
                            sub_indicator = (case_when({{ind}} == 'N' ~ "no",  
                                                       {{ind}} == 'Y' ~ "yes",
                                                       {{ind}} == 'U' ~ "unknown"))))) %>%
    
    
    
    
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(percent_valproate = valproate/sum(valproate)*100,
           percent_topiramate = topiramate/sum(topiramate)*100,
           percent_carbamazepine = carbamazepine/sum(carbamazepine)*100,
           percent_lamotrigine = lamotrigine/sum(lamotrigine)*100,
           percent_levetiracetam = levetiracetam/sum(levetiracetam)*100,
           percent_gabapentin = gabapentin/sum(gabapentin)*100,
           percent_pregabalin = pregabalin/sum(pregabalin)*100,
           percent_any_asm = any_asm/sum(any_asm)*100)
}

any <- aggregate_outcome2(data, any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_full_exposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)

dev_full_exposed <- dev_full_exposed %>% 
  mutate(indicator = case_when(sub_indicator == 'no' ~ 'outcome - none',
                               T~indicator)) %>% 
  select(-c(sub_indicator))


# Supp10 - matched unexposed -----------------------------------------------
# Any ASM -----------------------------------------------------------------
# Function to count outcomes for matched unexposed cohort
data <- data %>% 
  group_by(groupID) %>%
  mutate(group_exposure = hablar::max_(exposed_drug) ) %>%
  ungroup()

# Specific concerns
aggregate_drug <- function(data,  ind, indicator_name) {
  
  df <- data %>% 
    filter(exposed_any_asm == 0) %>% 
    group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_drug = "any_asm",
           sub_indicator = (case_when({{ind}} == "N" ~ "no",  
                                      {{ind}} == "Y" ~ "yes"))) %>% 
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(percent_any_asm = any_asm/sum(any_asm)*100) 
}


slc <- aggregate_drug(data, dev_slc_flag, "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_drug(data, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_drug(data, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_drug(data, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_drug(data, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_drug(data, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))


any <- aggregate_drug(data, any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_any_unexposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)

dev_any_unexposed <- dev_any_unexposed %>% 
  mutate(indicator = case_when(sub_indicator == 'no' ~ 'outcome - none',
                               T~indicator)) %>% 
  select(-c(sub_indicator))

# Valproate ---------------------------------------------------------------
# Function to count outcomes for matched unexposed cohort

aggregate_drug <- function(data, drug_name, ind, indicator_name) {
  # drug_name="valproate"
  #  ind = dev_slc_flag
  #  indicator_name =  "outcome - speech, language, and communication"
  
  df <- data %>% 
    filter(exposed_any_asm == 0 & group_exposure== drug_name) %>% 
    group_by({{ind}}) %>%
    #  group_by(dev_slc_flag) %>% 
    summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_drug = "any_asm",
           sub_indicator = (case_when({{ind}} == "N" ~ "no",  
                                      {{ind}} == "Y" ~ "yes"))) %>% 
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(percent_unexposd = any_asm/sum(any_asm)*100) 
  names(df)[4] <- drug_name
  names(df)[5] <- paste0("percent_", drug_name)
  df
}

slc <- aggregate_drug(data, drug_name="valproate", dev_slc_flag,
                      "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_drug(data, drug_name="valproate", dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_drug(data, drug_name="valproate", dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_drug(data, drug_name="valproate", dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_drug(data, drug_name="valproate", dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_drug(data, drug_name="valproate", dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))

any <- aggregate_drug(data, drug_name="valproate", any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_val_unexposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)


# Topiramate --------------------------------------------------------------
drug_name <-"topiramate"
slc <- aggregate_drug(data, drug_name=drug_name, dev_slc_flag,
                      "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_drug(data, drug_name=drug_name, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_drug(data, drug_name=drug_name, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_drug(data,drug_name=drug_name, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_drug(data, drug_name=drug_name, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_drug(data,drug_name=drug_name, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))

any <- aggregate_drug(data, drug_name=drug_name, any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_top_unexposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)


# Carbamazepine --------------------------------------------------------------

drug_name <-"carbamazepine"
slc <- aggregate_drug(data, drug_name=drug_name, dev_slc_flag,
                      "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_drug(data, drug_name=drug_name, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_drug(data, drug_name=drug_name, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_drug(data,drug_name=drug_name, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_drug(data, drug_name=drug_name, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_drug(data,drug_name=drug_name, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))

any <- aggregate_drug(data, drug_name=drug_name, any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_car_unexposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)


# Lamotrigine --------------------------------------------------------------
drug_name <-"lamotrigine"
slc <- aggregate_drug(data, drug_name=drug_name, dev_slc_flag,
                      "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_drug(data, drug_name=drug_name, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_drug(data, drug_name=drug_name, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_drug(data,drug_name=drug_name, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_drug(data, drug_name=drug_name, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_drug(data,drug_name=drug_name, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))

any <- aggregate_drug(data, drug_name=drug_name, any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_lam_unexposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)


# Levetiracetam --------------------------------------------------------------
drug_name <-"levetiracetam"
slc <- aggregate_drug(data, drug_name=drug_name, dev_slc_flag,
                      "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_drug(data, drug_name=drug_name, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_drug(data, drug_name=drug_name, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_drug(data,drug_name=drug_name, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_drug(data, drug_name=drug_name, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_drug(data,drug_name=drug_name, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))

any <- aggregate_drug(data, drug_name=drug_name, any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_lev_unexposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)

# Gabapentin --------------------------------------------------------------

drug_name <-"gabapentin"
slc <- aggregate_drug(data, drug_name=drug_name, dev_slc_flag,
                      "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_drug(data, drug_name=drug_name, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_drug(data, drug_name=drug_name, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_drug(data,drug_name=drug_name, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_drug(data, drug_name=drug_name, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_drug(data,drug_name=drug_name, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))

any <- aggregate_drug(data, drug_name=drug_name, any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_gab_unexposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)

# Pregabalin --------------------------------------------------------------

drug_name <-"pregabalin"
slc <- aggregate_drug(data, drug_name=drug_name, dev_slc_flag,
                      "outcome - speech, language, and communication") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_slc_flag))
gross <- aggregate_drug(data, drug_name=drug_name, dev_gross_flag, "outcome - gross motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_gross_flag))
fm <- aggregate_drug(data, drug_name=drug_name, dev_fm_flag, "outcome - fine motor") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_fm_flag))
prob <- aggregate_drug(data,drug_name=drug_name, dev_prob_flag, "outcome - problem solving") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_prob_flag))
persoc <- aggregate_drug(data, drug_name=drug_name, dev_persoc_flag, "outcome - personal / social") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_persoc_flag))
eb <- aggregate_drug(data,drug_name=drug_name, dev_EB_flag, "outcome - emotional / behavioural") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, dev_EB_flag))

any <- aggregate_drug(data, drug_name=drug_name, any_dev_excl_v_h, "outcome - any") %>% 
  select(-c(any_dev_excl_v_h))


dev_pre_unexposed <- bind_rows(slc, gross, fm, prob, persoc, eb, any)

rm(slc, gross, fm, prob, persoc, eb, any)

# All matched unexposed ---------------------------------------------------


dev_full_unexposed <- left_join(dev_val_unexposed, dev_top_unexposed, by = c("indicator", "sub_indicator"))
dev_full_unexposed <- left_join(dev_full_unexposed, dev_car_unexposed, by = c("indicator", "sub_indicator"))
dev_full_unexposed <- left_join(dev_full_unexposed, dev_lam_unexposed, by = c("indicator", "sub_indicator"))
dev_full_unexposed <- left_join(dev_full_unexposed, dev_lev_unexposed, by = c("indicator", "sub_indicator"))
dev_full_unexposed <- left_join(dev_full_unexposed, dev_gab_unexposed, by = c("indicator", "sub_indicator"))
dev_full_unexposed <- left_join(dev_full_unexposed, dev_pre_unexposed, by = c("indicator", "sub_indicator"))

dev_full_unexposed <- dev_full_unexposed %>% 
  mutate(indicator = case_when(sub_indicator == 'no' ~ 'outcome - none',
                               T~indicator)) %>% 
  select(-c(sub_indicator))

dev_full_unexposed <- left_join(dev_full_unexposed, dev_any_unexposed, by = c("indicator"))

rm(dev_any_unexposed, dev_val_unexposed, dev_top_unexposed, dev_car_unexposed,
   dev_lam_unexposed, dev_lev_unexposed, dev_gab_unexposed, dev_pre_unexposed)



# All exposed and matched unexposed ---------------------------------------

dev_full_exposed <- dev_full_exposed %>% 
  mutate(exposure = "exposed")

dev_full_unexposed <- dev_full_unexposed %>% 
  mutate(exposure = "unexposed")

dev_full <- bind_rows(dev_full_exposed, dev_full_unexposed)

dev_full_n <- dev_full %>% 
  select(-c(starts_with("percent")))

dev_full_percent <- dev_full %>% 
  select(indicator,exposure, starts_with("percent"))

# Re-shape df into required format for markdown table
dev_full_n <- dev_full_n %>% 
  pivot_longer(cols = carbamazepine:any_asm,
               names_to = "ASM",
               values_to = "n")

dev_full_percent <- dev_full_percent %>% 
  pivot_longer(cols = percent_valproate:percent_any_asm,
               names_to = "ASM",
               values_to = "percent") %>% 
  mutate(ASM = substr(ASM, 9, nchar(ASM)))

dev_full <- left_join(dev_full_n, dev_full_percent, by = c("indicator", "exposure", "ASM"))


supp10_table <- dev_full %>% 
  pivot_wider(names_from = exposure,
              names_glue = "{exposure}_{.value}",
              values_from = c(n, percent)) %>% # have separate n and % variables for exposed/unexposed groups
  select(c(ASM, indicator, exposed_n, exposed_percent, unexposed_n, unexposed_percent)) %>% 
  mutate(ASM = case_when(ASM == 'any_asm' ~ 'Any ASM',
                         ASM == 'valproate' ~ 'Valproate',
                         ASM == 'topiramate' ~ 'Topiramate',
                         ASM == 'carbamazepine' ~ 'Carbamazepine',
                         ASM == 'lamotrigine' ~ 'Lamotrigine',
                         ASM == 'levetiracetam' ~ 'Levetiracetam',
                         ASM == 'gabapentin' ~ 'Gabapentin', 
                         ASM == 'pregabalin' ~ 'Pregabalin',
                         T~NA)) %>% 
  arrange(match(indicator, c('outcome - none', 'outcome - any'))) %>% 
  group_by(indicator) %>% 
  arrange(match(ASM, c('Any ASM', 'Valproate', 'Topiramate', 'Carbamazepine', 'Lamotrigine', 'Levetiracetam', 'Gabapentin', 'Pregabalin'))) %>% 
  ungroup() # maintain correct order

write_csv(supp10_table, paste0(folder_data_path, "markdown_results/supp_table10.csv"))

