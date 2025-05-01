library(tidyverse)

source("supp_materials_publication/00.supp_setup.r")

# Supp 3 - Outcomes by specific loss type ---------------------------------

# Early spontaneous loss
# Spontaneous stillbirth
# Termination of pregnancy


# Pregnancy outcome - full cohort
data <- readRDS(paste0(folder_data_path, "matched_cohorts/matched_preg_outcomes_cohort.rds"))%>% 
  mutate(outcome_group = case_when(
    fetus_outcome1 %in% c("Ectopic pregnancy", "Miscarriage","Unknown - assumed early loss", "Molar pregnancy", "Stillbirth") ~ "Spontaneous loss",
    fetus_outcome1 %in% c("Unknown",  "Unknown - emigrated", "Maternal death") ~ "Unknown pregnancy outcome",
    fetus_outcome1 %in% c("Termination") ~ "Termination of pregnancy",
    T ~ fetus_outcome1)) 

# check the outcomes 
data %>% 
  group_by(pregnancy_loss, outcome_group) %>% 
  summarise(n())


# Any ASM
# Any loss / live birth
preg_any_exposed1 <- data %>% 
  filter(exposed_any_asm == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Any ASM",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

preg_any_unexposed1 <- data %>% 
  filter(exposed_any_asm == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Any ASM",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

# Specific loss
preg_any_exposed2 <- data %>% 
  filter(exposed_any_asm == 1) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Any ASM",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))

preg_any_unexposed2 <- data %>% 
  filter(exposed_any_asm == 0) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Any ASM",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))


preg_any <- bind_rows(preg_any_exposed1, preg_any_unexposed1, preg_any_exposed2, preg_any_unexposed2)
rm(preg_any_exposed1, preg_any_unexposed1, preg_any_exposed2, preg_any_unexposed2)

# Valproate
val_cohort <- data %>% 
  filter(exposed_valproate_mono == 1) %>% 
  distinct(groupID)

# Any loss / live birth
preg_val_exposed1 <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Valproate",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

preg_val_unexposed1 <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Valproate",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

# Specific loss
preg_val_exposed2 <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 1) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Valproate",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))

preg_val_unexposed2 <- data %>% 
  filter(groupID %in% val_cohort$groupID & exposed_valproate_mono == 0) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Valproate",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))


preg_val <- bind_rows(preg_val_exposed1, preg_val_unexposed1, preg_val_exposed2, preg_val_unexposed2)

rm(preg_val_exposed1, preg_val_unexposed1, preg_val_exposed2, preg_val_unexposed2, val_cohort)

# topiramate
top_cohort <- data %>% 
  filter(exposed_topiramate_mono == 1) %>% 
  distinct(groupID)

# Any loss / live birth
preg_top_exposed1 <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Topiramate",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

preg_top_unexposed1 <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Topiramate",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

# Specific loss

preg_top_exposed2 <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 1) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Topiramate",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))

preg_top_unexposed2 <- data %>% 
  filter(groupID %in% top_cohort$groupID & exposed_topiramate_mono == 0) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Topiramate",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))


preg_top <- bind_rows(preg_top_exposed1, preg_top_unexposed1, preg_top_exposed2, preg_top_unexposed2)

rm(preg_top_exposed1, preg_top_unexposed1, preg_top_exposed2, preg_top_unexposed2, top_cohort)

# Carbamazepine
car_cohort <- data %>% 
  filter(exposed_carbamazepine_mono == 1) %>% 
  distinct(groupID)

# any loss / live birth
preg_car_exposed1 <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Carbamazepine",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

preg_car_unexposed1 <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Carbamazepine",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

# specific loss
preg_car_exposed2 <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 1) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Carbamazepine",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))

preg_car_unexposed2 <- data %>% 
  filter(groupID %in% car_cohort$groupID & exposed_carbamazepine_mono == 0) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Carbamazepine",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))


preg_car <- bind_rows(preg_car_exposed1, preg_car_unexposed1, preg_car_exposed2, preg_car_unexposed2)

rm(preg_car_exposed1, preg_car_unexposed1, preg_car_exposed2, preg_car_unexposed2, car_cohort)

# Lamotrigine
lam_cohort <- data %>% 
  filter(exposed_lamotrigine_mono == 1) %>% 
  distinct(groupID)

# Any loss / live birth
preg_lam_exposed1 <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Lamotrigine",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

preg_lam_unexposed1 <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Lamotrigine",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

# Specific loss
preg_lam_exposed2 <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 1) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Lamotrigine",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))

preg_lam_unexposed2 <- data %>% 
  filter(groupID %in% lam_cohort$groupID & exposed_lamotrigine_mono == 0) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Lamotrigine",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))


preg_lam <- bind_rows(preg_lam_exposed1, preg_lam_unexposed1, preg_lam_exposed2, preg_lam_unexposed2)

rm(preg_lam_exposed1, preg_lam_unexposed1, preg_lam_exposed2, preg_lam_unexposed2, lam_cohort)



# Levetiracetam
lev_cohort <- data %>% 
  filter(exposed_levetiracetam_mono == 1) %>% 
  distinct(groupID)

# Any loss / live birth
preg_lev_exposed1 <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Levetiracetam",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

preg_lev_unexposed1 <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Levetiracetam",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))


# Specific loss
preg_lev_exposed2 <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 1) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Levetiracetam",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))

preg_lev_unexposed2 <- data %>% 
  filter(groupID %in% lev_cohort$groupID & exposed_levetiracetam_mono == 0) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Levetiracetam",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))


preg_lev <- bind_rows(preg_lev_exposed1, preg_lev_unexposed1, preg_lev_exposed2, preg_lev_unexposed2)

rm(preg_lev_exposed1, preg_lev_unexposed1, preg_lev_exposed2, preg_lev_unexposed2, lev_cohort)


# Gabapentin
gab_cohort <- data %>% 
  filter(exposed_gabapentin_mono == 1) %>% 
  distinct(groupID)

# Any loss / live birth
preg_gab_exposed1 <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Gabapentin",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

preg_gab_unexposed1 <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Gabapentin",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

# Specific loss
preg_gab_exposed2 <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 1) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Gabapentin",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))

preg_gab_unexposed2 <- data %>% 
  filter(groupID %in% gab_cohort$groupID & exposed_gabapentin_mono == 0) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Gabapentin",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))


preg_gab <- bind_rows(preg_gab_exposed1, preg_gab_unexposed1, preg_gab_exposed2, preg_gab_unexposed2)

rm(preg_gab_exposed1, preg_gab_unexposed1, preg_gab_exposed2, preg_gab_unexposed2, gab_cohort)

# Pregabalin
pre_cohort <- data %>% 
  filter(exposed_pregabalin_mono == 1) %>% 
  distinct(groupID)

# Live birth / any loss
preg_pre_exposed1 <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 1) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Pregabalin",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))

preg_pre_unexposed1 <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 0) %>% 
  group_by(pregnancy_loss) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Pregabalin",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, pregnancy_loss, n, percent))


# Specific loss
preg_pre_exposed2 <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 1) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Pregabalin",
         exposure = "Exposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))

preg_pre_unexposed2 <- data %>% 
  filter(groupID %in% pre_cohort$groupID & exposed_pregabalin_mono == 0) %>% 
  group_by(outcome_group) %>% summarise(n=n()) %>% ungroup() %>%
  mutate(ASM = "Pregabalin",
         exposure = "Unexposed",
         percent = n/sum(n)*100) %>%
  select(c(ASM, exposure, outcome_group, n, percent))


preg_pre <- bind_rows(preg_pre_exposed1, preg_pre_unexposed1, preg_pre_exposed2, preg_pre_unexposed2)
rm(preg_pre_exposed1, preg_pre_unexposed1, preg_pre_exposed2, preg_pre_unexposed2, pre_cohort)

# Bind together
preg_full <- bind_rows(preg_any, preg_val, preg_top, preg_car, preg_lam, preg_lev, preg_gab, preg_pre)


# Re-shape df into required format for markdown table
supp3_table <- preg_full %>% 
  filter(is.na(outcome_group) | outcome_group %in% c('Spontaneous loss', 'Termination of pregnancy')) %>% 
  mutate(outcome_group = case_when(is.na(outcome_group) ~ pregnancy_loss,
                                   T~outcome_group)) %>% 
  mutate(outcome_group = case_when(outcome_group == 'Yes' ~ 'Any loss',
                                   outcome_group == 'No' ~ 'Live birth',
                                   T~outcome_group)) %>% 
  select(-c(pregnancy_loss)) %>% 
  pivot_wider(names_from = exposure,
              names_glue = "{exposure}_{.value}",
              values_from = c(n, percent)) %>% # have separate n and % variables for exposed/unexposed groups
  select(c(ASM, outcome_group, Exposed_n, Exposed_percent, Unexposed_n, Unexposed_percent)) %>% 
  group_by(outcome_group) %>% 
  arrange(match(ASM, c('Any ASM', 'Valproate', 'Topiramate', 'Carbamazepine', 'Lamotrigine', 'Levetiracetam', 'Gabapentin', 'Pregabalin'))) %>% 
  ungroup() # maintain correct order

write_csv(supp3_table, paste0(folder_data_path, "supplementary_materials/supp_table3.csv"))





