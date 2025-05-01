# Supp 8 - Outcomes by specific congenital conditions ---------------------------------


# Nervous system conditions
# Eye conditions
# Ear, face, and neck conditions
# Congenital heart conditions
# Respiratory conditions
# Oro-facial clefts
# Gastro-intestinal conditions
# Abdominal wall defects
# Kidney and urinary tract conditions
# Genital conditions
# Limb conditions
# Other conditions/syndromes

source("supp_materials_publication/00.supp_setup.r")

# Congenital condition - full cohort
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_congenital_cohort.rds")) %>% 
  filter(control_pool_CC == 1 | cases_CC == 1) %>% # restrict to only those eligible for inclusion in the congential conditions cohort
  mutate(exposed_drug = (case_when(CC_exposed_valproate_mono == 1 ~ "valproate",  
                                   CC_exposed_topiramate_mono == 1 ~ "topiramate",
                                   CC_exposed_carbamazepine_mono == 1 ~ "carbamazepine",
                                   CC_exposed_lamotrigine_mono == 1 ~ "lamotrigine",
                                   CC_exposed_levetiracetam_mono == 1 ~ "levetiracetam",   
                                   CC_exposed_pregabalin_mono == 1 ~ "pregabalin", 
                                   CC_exposed_gabapentin_mono == 1 ~ "gabapentin", 
                                   T ~ NA)))




# Function to count outcomes for exposed cohort
aggregate_ind <- function(data, ind, indicator_name) {
  
  df <- data %>% filter(CC_exposed_any_asm == 1) %>% 
    group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_drug = "any_asm",
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}} == 1 ~ "yes"))) %>%
    
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(percent_any_asm = any_asm/sum(any_asm)*100)
}

nerv_sys <- aggregate_ind(data, all_1_nervous_system, "outcome - nervous system conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_1_nervous_system))
eye <- aggregate_ind(data, all_2_eye, "outcome - eye conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_2_eye))
ear_face_neck <- aggregate_ind(data, all_3_ear_face_and_neck, "outcome - ear, face, and neck conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_3_ear_face_and_neck))
con_heart <- aggregate_ind(data, all_4_congenital_heart_defects, "outcome - congenital heart conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_4_congenital_heart_defects))
resp <- aggregate_ind(data, all_5_respiratory, "outcome - respiratory conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_5_respiratory))
oro_face_clefts <- aggregate_ind(data, all_6_oro_facial_clefts, "outcome - oro-facial clefts") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_6_oro_facial_clefts))
gast_intest <- aggregate_ind(data, all_7_gastro_intestinal, "outcome - gastro-intestinal conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_7_gastro_intestinal))
abd_wall <- aggregate_ind(data, all_8_abdominal_wall_defects, "outcome - abdominal wall defects") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_8_abdominal_wall_defects))
kid_urin_tract <- aggregate_ind(data, all_9_kidney_and_urinary_tract, "outcome - kidney and urinary tract conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_9_kidney_and_urinary_tract))
genital <- aggregate_ind(data, all_10_genital, "outcome - genital conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_10_genital))
limb <- aggregate_ind(data, all_11_limb, "outcome - limb conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_11_limb))
other <- aggregate_ind(data, all_12_other_conditions, "outcome - other conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_12_other_conditions))
any <- aggregate_ind(data, any_CC, "outcome - any conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, any_CC))
none <- aggregate_ind(data, any_CC, "outcome - none") %>% 
  filter(sub_indicator == 'no') %>% 
  select(-c(sub_indicator, any_CC))


cc_full_exposed <- bind_rows(nerv_sys, eye, ear_face_neck, con_heart, resp, oro_face_clefts, gast_intest, abd_wall,
                             kid_urin_tract, genital, limb, other, any, none)

rm(nerv_sys, eye, ear_face_neck, con_heart, resp, oro_face_clefts, gast_intest, abd_wall,
   kid_urin_tract, genital, limb, other, any, none)




# Supp8 - matched unexposed -----------------------------------------------


# Any ASM -----------------------------------------------------------------
# Function to count outcomes for matched unexposed cohort
aggregate_drug <- function(data, ind, indicator_name) {
  
  
  df <- data %>% filter(CC_exposed_any_asm == 0) %>% 
    group_by({{ind}}) %>% summarise(n=n()) %>% ungroup() %>%
    mutate(indicator = indicator_name,
           exposed_drug = "any_asm",
           sub_indicator = (case_when({{ind}} == 0 ~ "no",  
                                      {{ind}} == 1 ~ "yes"))) %>% 
    pivot_wider(names_from = exposed_drug, values_from = c(n)) %>% 
    mutate(percent_any_asm = any_asm/sum(any_asm)*100) 
}

nerv_sys <- aggregate_drug(data, all_1_nervous_system, "outcome - nervous system conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_1_nervous_system))
eye <- aggregate_drug(data, all_2_eye, "outcome - eye conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_2_eye))
ear_face_neck <- aggregate_drug(data, all_3_ear_face_and_neck, "outcome - ear, face, and neck conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_3_ear_face_and_neck))
con_heart <- aggregate_drug(data, all_4_congenital_heart_defects, "outcome - congenital heart conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_4_congenital_heart_defects))
resp <- aggregate_drug(data, all_5_respiratory, "outcome - respiratory conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_5_respiratory))
oro_face_clefts <- aggregate_drug(data, all_6_oro_facial_clefts, "outcome - oro-facial clefts") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_6_oro_facial_clefts))
gast_intest <- aggregate_drug(data, all_7_gastro_intestinal, "outcome - gastro-intestinal conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_7_gastro_intestinal))
abd_wall <- aggregate_drug(data, all_8_abdominal_wall_defects, "outcome - abdominal wall defects") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_8_abdominal_wall_defects))
kid_urin_tract <- aggregate_drug(data, all_9_kidney_and_urinary_tract, "outcome - kidney and urinary tract conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_9_kidney_and_urinary_tract))
genital <- aggregate_drug(data, all_10_genital, "outcome - genital conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_10_genital))
limb <- aggregate_drug(data, all_11_limb, "outcome - limb conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_11_limb))
other <- aggregate_drug(data, all_12_other_conditions, "outcome - other conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, all_12_other_conditions))
any <- aggregate_drug(data, any_CC, "outcome - any conditions") %>% 
  filter(sub_indicator == 'yes') %>% 
  select(-c(sub_indicator, any_CC))
none <- aggregate_drug(data, any_CC, "outcome - none") %>% 
  filter(sub_indicator == 'no') %>% 
  select(-c(sub_indicator, any_CC))


cc_full_unexposed <- bind_rows(nerv_sys, eye, ear_face_neck, con_heart, resp, oro_face_clefts, gast_intest, abd_wall,
                               kid_urin_tract, genital, limb, other, any, none)

rm(nerv_sys, eye, ear_face_neck, con_heart, resp, oro_face_clefts, gast_intest, abd_wall,
   kid_urin_tract, genital, limb, other, any, none)


# All exposed and matched unexposed ---------------------------------------

cc_full_exposed <- cc_full_exposed %>% 
  mutate(exposure = "exposed")

cc_full_unexposed <- cc_full_unexposed %>% 
  mutate(exposure = "unexposed")

cc_full <- bind_rows(cc_full_exposed, cc_full_unexposed)

cc_full_n <- cc_full %>% 
  select(-c(percent_any_asm))

cc_full_percent <- cc_full %>% 
  select(-c(any_asm))

# Re-shape df into required format for markdown table
cc_full_n <- cc_full_n %>% 
  pivot_longer(cols = any_asm,
               names_to = "ASM",
               values_to = "n")

cc_full_percent <- cc_full_percent %>% 
  pivot_longer(cols = percent_any_asm,
               names_to = "ASM",
               values_to = "percent") %>% 
  mutate(ASM = substr(ASM, 9, nchar(ASM)))

cc_full <- left_join(cc_full_n, cc_full_percent, by = c("indicator", "exposure", "ASM"))


supp8_table <- cc_full %>% 
  pivot_wider(names_from = exposure,
              names_glue = "{exposure}_{.value}",
              values_from = c(n, percent)) %>% # have separate n and % variables for exposed/unexposed groups
  select(c(ASM, indicator, exposed_n, exposed_percent, unexposed_n, unexposed_percent)) %>% 
  mutate(ASM = case_when(ASM == 'any_asm' ~ 'Any ASM',
                         T~NA)) %>%
  arrange(match(indicator, c('outcome - none', 'outcome - any conditions'))) 


write_csv(supp8_table, paste0(folder_data_path, "markdown_results/supp_table8.csv"))


