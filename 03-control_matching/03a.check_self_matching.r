##03a. check_self_matching.r
## script to check for self matches from the matched controls datasets
## created in 01a-02a
## after, run 03b.drop_self_matches.r


#check for self matching
library(dplyr)
source("03-control_matching/00.setup_matching.r")

matched_cohort1 <-
  readRDS(paste0(data_path, "matched_cohorts/matched_preg_outcomes_cohort.rds"))

#names(matched_preg_outcomes_cohort)
#length(unique(matched_cohort1$groupID))
##check for self matching
check_matches <- matched_cohort1 %>% group_by( groupID,exposed_any_asm, mother_upi) %>%
  summarise(count_in_group = n())

check_control_matches <- matched_cohort1 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)

control_matches <- matched_cohort1 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup

controls_drop_1 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 1", match_type="control-control")

##so there are some cases where the same woman contributes more than one control in the group
##but are there any where they are the same person as the case? 

case_ids <- matched_cohort1 %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort1 %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))


table(control_ids$row_ID  %in% case_ids$row_ID)
###49 instances

controls_drop2 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID) %>%
  mutate(cohort="Cohort 1", match_type="case-control") %>% select(-row_ID)


controls_drop_c1 <- rbind(controls_drop_1, controls_drop2)
rm(controls_drop_1, controls_drop2, matched_cohort1)

###Cohort 1 epilepsy####
matched_cohort1_ep <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_preg_outcomes_epilepsy.rds"))


case_ids <- matched_cohort1_ep %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort1_ep %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))


table(control_ids$row_ID  %in% case_ids$row_ID)
controls_drop1 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID)%>%
  mutate(cohort="Cohort 1 ep", match_type="case-control") %>% select(-row_ID)
#just 1

###cCOhort 1 ep controls

check_control_matches <- matched_cohort1_ep %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)
#1     2     3 
#25171   471    19 
control_matches <- matched_cohort1_ep %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup
controls_drop2 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 1 ep", match_type="control-control")

controls_drop_c1_ep <- rbind(controls_drop1, controls_drop2)
rm(controls_drop1, controls_drop2, matched_cohort1_ep)

##Cohort2####
matched_cohort2 <-readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort.rds"))

case_ids <- matched_cohort2 %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort2 %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

table(control_ids$row_ID  %in% case_ids$row_ID)
##36
controls_drop1 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID)%>%
  mutate(cohort="Cohort 2", match_type="case-control") %>% select(-row_ID)

##control control matched
check_control_matches <- matched_cohort2 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)
#1     2     3 
#25171   471    19 
control_matches <- matched_cohort2 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup
controls_drop2 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 2", match_type="control-control")

controls_drop_c2 <- rbind(controls_drop1, controls_drop2)
rm(controls_drop1, controls_drop2)


##Cohort 2 epilepsy
matched_cohort2_ep <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort_epilepsy.rds"))


case_ids <- matched_cohort2_ep %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort2_ep %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

table(control_ids$row_ID  %in% case_ids$row_ID)
##36
controls_drop1 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID)%>%
  mutate(cohort="Cohort 2 ep", match_type="case-control") %>% select(-row_ID)

##control control matched
check_control_matches <- matched_cohort2_ep %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)
#1     2     3 
#25171   471    19 
control_matches <- matched_cohort2_ep%>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup
controls_drop2 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 2 ep", match_type="control-control")

controls_drop_c2_ep <- rbind(controls_drop1, controls_drop2)
rm(controls_drop1, controls_drop2, matched_cohort2_ep)
##cohort 2 12 weeks

matched_cohort2_12 <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort_12weeks.rds"))


case_ids <- matched_cohort2_12 %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort2_12 %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

table(control_ids$row_ID  %in% case_ids$row_ID)
##36
controls_drop1 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID)%>%
  mutate(cohort="Cohort 2 12wk", match_type="case-control") %>% select(-row_ID)

##control control matched
check_control_matches <- matched_cohort2_12 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)
#1     2     3 
#25171   471    19 
control_matches <- matched_cohort2_12 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup
controls_drop2 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 2 12wk", match_type="control-control")

controls_drop_c2_12 <- rbind(controls_drop1, controls_drop2)
rm(controls_drop1, controls_drop2, matched_cohort2_12)

##cohort 3####
matched_cohort3 <- readRDS(paste0(data_path, "matched_cohorts/matched_developmental_cohort.rds"))


case_ids <- matched_cohort3 %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort3 %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

table(control_ids$row_ID  %in% case_ids$row_ID)
##36
controls_drop1 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID)%>%
  mutate(cohort="Cohort 3", match_type="case-control") %>% select(-row_ID)

##control control matched
check_control_matches <- matched_cohort3 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)
#1     2     3 
#25171   471    19 
control_matches <- matched_cohort3 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup
controls_drop2 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 3", match_type="control-control")

controls_drop_c3 <- rbind(controls_drop1, controls_drop2)
rm(controls_drop1, controls_drop2, matched_cohort3)



#cohort3 epilepsy
matched_cohort3_ep <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_developmental_cohort_epilepsy.rds"))

case_ids <- matched_cohort3_ep %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort3_ep %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

table(control_ids$row_ID  %in% case_ids$row_ID)
##36
controls_drop1 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID)%>%
  mutate(cohort="Cohort 3 ep", match_type="case-control") %>% select(-row_ID)

##control control matched
check_control_matches <- matched_cohort3_ep %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)
#1 
#12930 
control_matches <- matched_cohort3_ep %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
#no self matches in this cohort




all_drops <- 
  rbind(controls_drop_c1,
        controls_drop_c1_ep, controls_drop_c2, controls_drop_c2_12, controls_drop_c2_ep, controls_drop_c3)
all_drops %>% group_by(cohort, match_type) %>% summarise(number_to_drop = n())


saveRDS(all_drops,paste0(data_path, "matched_cohorts/controls_to_drop.rds" ))




###check folic acid cohorts####
#- run with antiexact so should not have case - control the same person
# but may have >1 control being the same

#check for self matching
library(dplyr)
matched_cohort1 <-
  readRDS(paste0(data_path, "matched_cohorts/matched_preg_outcomes_folic_cohort.rds"))

#names(matched_preg_outcomes_cohort)
#length(unique(matched_cohort1$groupID))
##check for self matching
check_matches <- matched_cohort1 %>% group_by( groupID,exposed_any_asm, mother_upi) %>%
  summarise(count_in_group = n())

check_control_matches <- matched_cohort1 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
##there are some control-control matches
table(check_control_matches$count_in_group)

control_matches <- matched_cohort1 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup

controls_drop_1 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 1 FA", match_type="control-control")

##so there are some cases where the same woman contributes more than one control in the group
##but are there any where they are the same person as the case? 

case_ids <- matched_cohort1 %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort1 %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))


table(control_ids$row_ID  %in% case_ids$row_ID)
###none
controls_drop_c1 <- controls_drop_1


##congenital folic acid
##Cohort2####
matched_cohort2 <-readRDS(paste0(data_path, "matched_cohorts/matched_congenital_folic_cohort.rds"))

case_ids <- matched_cohort2 %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort2 %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

table(control_ids$row_ID  %in% case_ids$row_ID)
##none
controls_drop1 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID)%>%
  mutate(cohort="Cohort 2 FA", match_type="case-control") %>% select(-row_ID)

##control control matched
check_control_matches <- matched_cohort2 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)

#    1     2     3 
# 81295  1126    14 
control_matches <- matched_cohort2 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup
controls_drop2 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 2 FA", match_type="control-control")

controls_drop_c2  <-controls_drop2

##developmental folate####
##cohort 3####
matched_cohort3 <- readRDS(paste0(data_path, "matched_cohorts/matched_developmental_folic_cohort.rds"))

case_ids <- matched_cohort3 %>% filter(exposed_any_asm==1) %>%
  select(groupID, mother_upi,pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

control_ids <- matched_cohort3 %>% filter(exposed_any_asm==0) %>%
  select(groupID, mother_upi, pregnancy_id) %>% mutate(row_ID = paste(mother_upi, "_", groupID))

table(control_ids$row_ID  %in% case_ids$row_ID)
##36
controls_drop1 <- control_ids %>% filter(row_ID  %in% case_ids$row_ID)%>%
  mutate(cohort="Cohort 3 FA", match_type="case-control") %>% select(-row_ID)

##control control matched
check_control_matches <- matched_cohort3 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  summarise(count_in_group = n())
table(check_control_matches$count_in_group)
#    1     2 
#48884     8 
 
control_matches <- matched_cohort3 %>% filter(exposed_any_asm==0) %>%
  group_by( groupID, mother_upi) %>%
  mutate(count_in_group = n()) %>%
  ungroup() %>% filter(count_in_group>1)
##select one from each group to delete - we can create a random order by ordering on pregnancyid 
keep <- control_matches  %>% arrange(groupID, pregnancy_id) %>% 
  group_by(groupID) %>% slice(1) %>% ungroup
controls_drop2 <- control_matches %>% filter(!pregnancy_id %in% keep$pregnancy_id) %>%
  select(mother_upi, pregnancy_id, groupID) %>%
  mutate(cohort="Cohort 3 FA", match_type="control-control")

controls_drop_c3 <- rbind(controls_drop1, controls_drop2)
rm(controls_drop1, controls_drop2, matched_cohort3)


###combine

all_drops <- 
  rbind(controls_drop_c1,
         controls_drop_c2, controls_drop_c3)
all_drops %>% group_by(cohort, match_type) %>% summarise(number_to_drop = n())


saveRDS(all_drops,paste0(data_path, "matched_cohorts/controls_to_drop_FA.rds" ))

