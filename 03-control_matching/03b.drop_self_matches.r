##03b. drop_self_matches.r
## script to remove self matches from the matched controls datasets
## create in 01a-02a
## run after 03a.check_self_matching.r

##Remove self-self matches from matched controls
rm(list = ls())
gc()

###MAin analysis
source("03-control_matching/00.setup_matching.r")
##Run after check_self_matching.r file.
library(dplyr)
##load file of prgnancy IDs to be removed##
all_drops <- readRDS(paste0(data_path, "matched_cohorts/controls_to_drop.rds" ))
table(all_drops$cohort)
##load cohorts in sequence, identify the correct rows in the matched datasets to be dropped
##drop duplicates and resave the matched cohort files
matched_cohort1 <-
  readRDS(paste0(data_path, "matched_cohorts/matched_preg_outcomes_cohort.rds"))

df<- matched_cohort1
cohort_name <-"Cohort 1" 
cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
#neet ot match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)

dropped_all <- dropped
saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_preg_outcomes_cohort.rds"))

##cohort 1 epilepsy
matched_cohort1_ep <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_preg_outcomes_epilepsy.rds"))

df<- matched_cohort1_ep
cohort_name <-"Cohort 1 ep" 

cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
#neet ot match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)


dropped_all <- rbind(dropped_all, dropped)

saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_preg_outcomes_epilepsy.rds"))

##cohort 2####
matched_cohort2 <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort.rds"))

df<- matched_cohort2
cohort_name <-"Cohort 2" 

cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
#neet ot match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)


dropped_all <- rbind(dropped_all, dropped)

saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_congenital_cohort.rds"))

##cohort 2 epilepsy####
matched_cohort2ep <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort_epilepsy.rds"))

df<- matched_cohort2ep
cohort_name <-"Cohort 2 ep" 

cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
table(cohort_drop$match_type)
#neet ot match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)


dropped_all <- rbind(dropped_all, dropped)

saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_congenital_cohort_epilepsy.rds"))


##cohort 2 12weeks####

matched_cohort2_12 <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_congenital_cohort_12weeks.rds"))

df<- matched_cohort2_12
cohort_name <-"Cohort 2 12wk" 

cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
table(cohort_drop$match_type)
#neet ot match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)

dropped_all <- rbind(dropped_all, dropped)

saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_congenital_cohort_12weeks.rds"))

table(dropped_all$cohort)

##cohort 3 ####
##only control-control matches here.
matched_cohort3 <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_developmental_cohort.rds"))

df<- matched_cohort3
cohort_name <-"Cohort 3" 

cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
table(cohort_drop$match_type)
#need to match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)

dropped_all <- rbind(dropped_all, dropped)

saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_developmental_cohort.rds"))

table(dropped_all$cohort)
table(all_drops$cohort)
saveRDS(dropped_all , paste0(data_path, "matched_cohorts/dropped_controls_all_cohorts.rds"))


##folic acid cohorts####
library(dplyr)
##load file of prgnancy IDs to be removed##
all_drops <- readRDS(paste0(data_path, "matched_cohorts/controls_to_drop_FA.rds" ))
table(all_drops$cohort)
##load cohorts in sequence, identify the correct rows in the matched datasets to be dropped
##drop duplicates and resave the matched cohort files
matched_cohort1 <-
  readRDS(paste0(data_path, "matched_cohorts/matched_preg_outcomes_folic_cohort.rds"))

df<- matched_cohort1
cohort_name <-"Cohort 1 FA" 
cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
#neet ot match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)

dropped_all <- dropped
saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_preg_outcomes_folic_cohort.rds"))

##cohort 2####
matched_cohort2 <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_congenital_folic_cohort.rds"))

df<- matched_cohort2
cohort_name <-"Cohort 2 FA" 

cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
#neet ot match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)


dropped_all <- rbind(dropped_all, dropped)

saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_congenital_folic_cohort.rds"))
##cohort 3 ####
##only control-control matches here.
matched_cohort3 <- 
  readRDS(paste0(data_path, "matched_cohorts/matched_developmental_folic_cohort.rds"))

df<- matched_cohort3
cohort_name <-"Cohort 3 FA" 

cohort_drop <- all_drops %>% filter(cohort==cohort_name) %>%
  mutate(drop=1)
table(cohort_drop$match_type)
#need to match pregnancy ID and group ID to drop that pregnancy ONLY when it appears as a "self-match"
#need group ID and the pregnancy ID to match.
df <- left_join(df, cohort_drop)

nrow(cohort_drop)
table(df$drop)
matched_cohort <- df %>% filter(is.na(drop))

dropped <-  df %>% filter(drop==1) %>% mutate(cohort=cohort_name)
nrow(matched_cohort) +nrow(dropped) == nrow(df)

dropped_all <- rbind(dropped_all, dropped)

saveRDS(matched_cohort , paste0(data_path, "matched_cohorts/matched_developmental_folic_cohort.rds"))
saveRDS(dropped_all , paste0(data_path, "matched_cohorts/dropped_controls_folic_cohorts.rds"))
