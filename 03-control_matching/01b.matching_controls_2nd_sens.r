## 01b.matching_controls_2nd_sens.r
## script to run matching process on main analysis file
## for secondary and sensitivity stat analyses

##########################
###MAtching controls - secondary an sensitivity analyses####
########################
##matches for secondary and sensitivity analyses##

##setup####
library(MatchIt)
library(dplyr)
library(lubridate)
library(arrow)
library(stringr)
library(hablar)


source("03-control_matching/00.setup_matching.r")
##Criteria: 
## gestation 1st exposed (this might be tricky to code in the matching algorithm)
## year of conception(exact)
## plus additiional criteria for the secondary analyses

###Link Congenital conditions reaching 12 weeks ####
df <- readRDS(paste0(data_path, "linkage/master_dataset_file.rds"))
df$gest_first_exposed <-as.numeric(df$gest_first_exposed)

##filter those belonging to congenital conditions cohort only.
#additional filter  - must reach 12 weeks
df <- df %>% filter(cases_CC==1 | control_pool_CC==1) %>%
  filter(gest_end_pregnancy >=12)
outcome <- "congenital"
source("control_matching/matching_process.r")
##subgrouped on gestation####
###first split the dataframe into a list of dfs
count_group_members <-matched_dataset1_full %>% group_by(groupID) %>% count()
table(count_group_members$n)

saveRDS(matched_dataset1_full,paste0(data_path, "matched_cohorts/matched_congenital_cohort_12weeks.rds") )


# New 18/03/2025 - add in new any_CC flag 
cc_matched <- readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort_12weeks.rds"))
main_file <- readRDS(paste0(data_path,"linkage/master_dataset_file.rds"))

cc_matched %>% 
  group_by(any_CC) %>% 
  summarise(n())
# 2,066 any cc

# Remove old any_CC
cc_matched <- cc_matched %>%
  select(-c(any_CC))

# keep pregnancy id for matching and new flags
main_file <- main_file %>%
  select(c(pregnancy_id,
           any_CC))

# Join new flags onto existing matched cohort
cc_matched_new <- left_join(cc_matched, main_file, by = c("pregnancy_id"))

cc_matched_new %>% 
  group_by(any_CC) %>% 
  summarise(n())
# 1,912 any cc


# Save out new cc matched cohort file
saveRDS(cc_matched_new,paste0(data_path, "matched_cohorts/matched_congenital_cohort_12weeks.rds") )


## Link cohorts of epilepsy indication only####

### pregnancy outcomes ####

df <- readRDS(paste0(data_path, "linkage/master_dataset_file.rds"))
df$gest_first_exposed <-as.numeric(df$gest_first_exposed)
df <- df %>% filter(pregnancy_loss != "Unknown" & pregnancy_loss !="Maternal death")
table(df$epilepsy_indication, df$exposed_any_asm)
df <- df %>% filter(epilepsy_indication==1)

###
#everything else is the same
outcome <- "pregnancy"
source("control_matching/matching_process.r")

count_group_members <-matched_dataset1_full %>% group_by(groupID) %>% count()
table(count_group_members$n)
table(df$exposed_any_asm)
saveRDS(matched_dataset1_full,paste0(data_path, "matched_cohorts/matched_preg_outcomes_epilepsy.rds") )

# New 20/12/2024 - add updated indicators to matched file
preg_matched <- readRDS(paste0(data_path, "matched_cohorts/matched_preg_outcomes_epilepsy.rds"))
main_file <- readRDS(paste0(data_path,"linkage/master_dataset_file.rds"))

# Remove old mh and pain flags
preg_matched <- preg_matched %>% 
  select(-c(mh_flag, migraine_pain_flag))

# keep pregnancy id for matching and new flags
main_file <- main_file %>% 
  select(c(pregnancy_id, 
           mh_flag, migraine_pain_flag,
           drug_alcohol_use))

# Join new flags onto existing matched cohort
preg_matched_new <- left_join(preg_matched, main_file, by = c("pregnancy_id"))

# Save out new pregnancy matched cohort file
saveRDS(preg_matched_new,paste0(data_path, "matched_cohorts/matched_preg_outcomes_epilepsy.rds") )

###Link Congenital conditions epilepsy only ####
df <- readRDS(paste0(data_path, "linkage/master_dataset_file.rds"))
df$gest_first_exposed <-as.numeric(df$gest_first_exposed)

##filter those belonging to congenital conditions cohort only.
#additional filter  - epilepsy
df <- df %>% filter(cases_CC==1 | control_pool_CC==1) %>%
  filter(epilepsy_indication==1)

outcome <- "congenital"
source("control_matching/matching_process.r")

count_group_members <-matched_dataset1_full %>% group_by(groupID) %>% count()
table(count_group_members$n)

saveRDS(matched_dataset1_full,paste0(data_path, "matched_cohorts/matched_congenital_cohort_epilepsy.rds") )



# New 18/03/2025 - add in new any_CC flag 
cc_matched <- readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort_epilepsy.rds"))
main_file <- readRDS(paste0(data_path,"linkage/master_dataset_file.rds"))

cc_matched %>% 
  group_by(any_CC) %>% 
  summarise(n())
# 555 any cc

# Remove old any_CC
cc_matched <- cc_matched %>%
  select(-c(any_CC))

# keep pregnancy id for matching and new flags
main_file <- main_file %>%
  select(c(pregnancy_id,
           any_CC))

# Join new flags onto existing matched cohort
cc_matched_new <- left_join(cc_matched, main_file, by = c("pregnancy_id"))

cc_matched_new %>% 
  group_by(any_CC) %>% 
  summarise(n())
# 536 any cc


# Save out new cc matched cohort file
saveRDS(cc_matched_new,paste0(data_path, "matched_cohorts/matched_congenital_cohort_epilepsy.rds") )


# ### Link dev outcomes epilepsy only ####
df <- readRDS(paste0(data_path, "linkage/master_dataset_file.rds"))
df$gest_first_exposed <-as.numeric(df$gest_first_exposed)

##filter those belonging to developmental concerns cohort only.
##and epilepsy indication
df <- df %>% filter(cases_dev==1 | control_pool_dev==1) %>%
         filter(epilepsy_indication==1)
table(df$cases_dev)

outcome <- "developmental"
source("control_matching/matching_process.r")

count_group_members <-matched_dataset1_full %>% group_by(groupID) %>% count()
table(count_group_members$n)
table(df$exposed_any_asm)
saveRDS(matched_dataset1_full,paste0(data_path, "matched_cohorts/matched_developmental_cohort_epilepsy.rds") )


# New 20/12/2024 - add updated indicators to matched file
dev_matched <- readRDS(paste0(data_path, "matched_cohorts/matched_developmental_cohort_epilepsy.rds"))
main_file <- readRDS(paste0(data_path,"linkage/master_dataset_file.rds"))

# Remove old mh and pain flags
dev_matched <- dev_matched %>% 
  select(-c(mh_flag, migraine_pain_flag))

# keep pregnancy id for matching and new flags
main_file <- main_file %>% 
  select(c(pregnancy_id, 
           mh_flag, migraine_pain_flag,
           drug_alcohol_use))

# Join new flags onto existing matched cohort
dev_matched_new <- left_join(dev_matched, main_file, by = c("pregnancy_id"))

# Save out new dev matched cohort file
saveRDS(dev_matched_new,paste0(data_path, "matched_cohorts/matched_developmental_cohort_epilepsy.rds") )
