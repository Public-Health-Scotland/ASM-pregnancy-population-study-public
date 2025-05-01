## 02a.matching_controls_FA_strata.r
## script to run matching process on main analysis file
## for folic acid stat analyses


##########################
###MAtching controls####
########################
##notes: requires a reasonably large session size to run, especially
# to run the match for the pregnancy outcome cohort - the output of the 
# matching for that cohort is just under 10GB. Others are are smaller.
# total memory for whole script to run ~ 15GB
# each lapply(matchit()) section takes several minutes to run. 

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
##

##Link pregnancy outcomes cohort####
df <-  readRDS(paste0(data_path, "linkage/master_dataset_file.rds"))
df$gest_first_exposed <-as.numeric(df$gest_first_exposed)
outcome <- "pregnancy"
df <- df %>% filter(pregnancy_loss != "Unknown" & pregnancy_loss !="Maternal death")
source("control_matching/matching_process_folic.r")
# 
saveRDS(matched_dataset1_full,paste0(data_path, "matched_cohorts/matched_preg_outcomes_folic_cohort.rds") )


###Link Congenital conditions cohorts####
df <-  readRDS(paste0(data_path, "linkage/master_dataset_file.rds"))
df$gest_first_exposed <-as.numeric(df$gest_first_exposed)
##filter those belonging to congenital conditions cohort only.
df <- df %>% filter(cases_CC==1 | control_pool_CC==1)

#define outcome (different process for congenital as 
# the cases flag is CC_exposed_any_asm rather than exposed_any_asm)
outcome <- "congenital"
# #match and extract the matched cohort
source("control_matching/matching_process_folic.r")
# 
saveRDS(matched_dataset1_full,paste0(data_path, "matched_cohorts/matched_congenital_folic_cohort.rds") )
# 

# New 18/03/2025 - add in new any_CC flag 
cc_matched <- readRDS(paste0(data_path, "matched_cohorts/matched_congenital_folic_cohort.rds"))
main_file <- readRDS(paste0(data_path,"linkage/master_dataset_file.rds"))

cc_matched %>% 
  group_by(any_CC) %>% 
  summarise(n())
# 2,292 any cc

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
# 2,126 any cc

# Save out new cc matched cohort file
saveRDS(cc_matched_new,paste0(data_path, "matched_cohorts/matched_congenital_folic_cohort.rds") )

###Link developmental cohorts####
df <-  readRDS(paste0(data_path, "linkage/master_dataset_file.rds"))
df$gest_first_exposed <-as.numeric(df$gest_first_exposed)
outcome <- "developmental"
##filter those belonging to developmental concerns cohort only.
df <- df %>% filter(cases_dev==1 | control_pool_dev==1)
#match and extract the matched cohort
source("control_matching/matching_process_folic.r")
# 
saveRDS(matched_dataset1_full,paste0(data_path, "matched_cohorts/matched_developmental_folic_cohort.rds") )
# 
 

##check on n controls able to match####
preg_out_cohort <- readRDS(paste0(data_path, "matched_cohorts/matched_preg_outcomes_cohort.rds") )
count_group_members <-preg_out_cohort  %>% group_by(groupID) %>% count()
table(count_group_members$n)
table(preg_out_cohort$exposed_any_asm)

cc_cohort <- readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort.rds") )
count_group_members <-cc_cohort %>% group_by(groupID) %>% count()
table(count_group_members$n)
table(cc_cohort$control_pool_CC)

dev_cohort <- readRDS(paste0(data_path, "matched_cohorts/matched_developmental_cohort.rds") )
count_group_members <- dev_cohort  %>% group_by(groupID) %>% count()
table(count_group_members$n)
table(dev_cohort$exposed_any_asm)
##all have 11 members so successful 10:1 match