##04a.propensity_score_matching_main.r
## script to run matching process on main analysis file
## for propensity score analyses

##########################
###Matching controls - propensity score####
########################
##notes: requires a reasonably large session size to run,
# total memory for pregnancy outcomes to run ~ 30GB (all + all the individuals exposures)
# Time to run matching - pregnancy outcomes = ~ 20-25 mins


##setup####
library(MatchIt)
library(dplyr)
library(lubridate)
library(arrow)
library(stringr)
library(hablar)
library(cobalt)
source("03-control_matching/00.setup_matching.r")

##Criteria: 
## gestation 1st exposed (this might be tricky to code in the matching algorithm)
## propensity score based on the following covariates 
##
##just the variables for propensity score and IDing the controls
#•	Maternal epilepsy
#•	Maternal mental health conditions
#•	Maternal migraine or pain conditions
#•	Year of conception
##•	Maternal age at conception
##•	Maternal deprivation based on postcode at antenatal booking, or, if missing, end of pregnancy.
#PSs for analyses examining early childhood developmental concerns will be based on the covariates listed above plus:
#  •	Maternal BMI at antenatal booking
#•	Maternal smoking at antenatal booking.

##Link pregnancy outcomes cohort any asm####
df1 <-  readRDS(paste0(data_path, "linkage/master_dataset_file.rds")) %>% 
  mutate(pregnancy_loss = case_when(fetus_outcome1=="Unknown - assumed early loss"~ "Yes" , T~pregnancy_loss) ) %>% 
  mutate(maternal_age_group_conception = case_when(maternal_age_group_conception == '<20' ~ 'less than 20',
                                                   maternal_age_group_conception == '20-24' ~ '20 to 24',
                                                   maternal_age_group_conception == '25-29' ~ '25 to 29',
                                                   maternal_age_group_conception == '30-34' ~ '30 to 34',
                                                   maternal_age_group_conception == '35-39' ~ '35 to 39',
                                                   maternal_age_group_conception == '40+' ~ '40 plus',
                                                   T ~ maternal_age_group_conception),
         mother_simd = case_when(is.na(mother_simd) ~ 'Unknown',
                                 T ~ mother_simd))

df1$gest_first_exposed <-as.numeric(df1$gest_first_exposed)

df1 <- df1 %>% mutate(asm_exposure = case_when(exposed_carbamazepine_mono ==1~ "carbamazepine", 
                                               exposed_gabapentin_mono ==1 ~ "gabapentin", 
                                               exposed_lamotrigine_mono ==1 ~ "lamotrigine", 
                                               exposed_levetiracetam_mono == 1 ~ "levetiracetam", 
                                               exposed_pregabalin_mono == 1 ~ "pregabalin", 
                                               exposed_topiramate_mono ==1 ~"topiramate", 
                                               exposed_valproate_mono ==1 ~ "valproate"))
outcome <- "pregnancy"
df <- df1 %>% 
   filter(pregnancy_loss != "Unknown" & pregnancy_loss !="Maternal death")

source("control_matching/propensity_matching_process.r")

saveRDS(matched_dataset1,
        paste0(data_path, "matched_cohorts/propensity_score_match_preg_outcomes_cohort.rds") )

matched_dataset1 <- 
  readRDS(paste0(data_path, "matched_cohorts/propensity_score_match_preg_outcomes_cohort.rds") )


###Individual exposures####
##Link pregnancy outcomes cohort####
df1 <- readRDS(paste0(data_path, "linkage/master_dataset_file.rds") ) %>% 
  mutate(pregnancy_loss = case_when(fetus_outcome1=="Unknown - assumed early loss"~ "Yes" , T~pregnancy_loss) ) %>% 
  mutate(maternal_age_group_conception = case_when(maternal_age_group_conception == '<20' ~ 'less than 20',
                                                   maternal_age_group_conception == '20-24' ~ '20 to 24',
                                                   maternal_age_group_conception == '25-29' ~ '25 to 29',
                                                   maternal_age_group_conception == '30-34' ~ '30 to 34',
                                                   maternal_age_group_conception == '35-39' ~ '35 to 39',
                                                   maternal_age_group_conception == '40+' ~ '40 plus',
                                                   T ~ maternal_age_group_conception),
         mother_simd = case_when(is.na(mother_simd) ~ 'Unknown',
                                 T ~ mother_simd))
df1$gest_first_exposed <-as.numeric(df1$gest_first_exposed)

df1$gest_first_exposed <-as.numeric(df1$gest_first_exposed)
df1 <- df1 %>% mutate(asm_exposure = case_when(exposed_carbamazepine_mono ==1~ "carbamazepine", 
                                               exposed_gabapentin_mono ==1 ~ "gabapentin", 
                                               exposed_lamotrigine_mono ==1 ~ "lamotrigine", 
                                               exposed_levetiracetam_mono == 1 ~ "levetiracetam", 
                                               exposed_pregabalin_mono == 1 ~ "pregabalin", 
                                               exposed_topiramate_mono ==1 ~"topiramate", 
                                               exposed_valproate_mono ==1 ~ "valproate"))
outcome <- "pregnancy"

###all monotherapies
asm_names <- c("carbamazepine",  "gabapentin", "lamotrigine", "levetiracetam",
               "pregabalin", "topiramate", "valproate")

##NB to run the loop right through in one go would need about 60k+memory I would guess
# did run the last 3 together on 40K session with plenty space left
for(a in 1:length(asm_names)){
asm_names <- c("carbamazepine",  "gabapentin", "lamotrigine", "levetiracetam",
                 "pregabalin", "topiramate", "valproate")

df <- df1 %>% filter(pregnancy_loss != "Unknown" & pregnancy_loss !="Maternal death") %>% 
  filter(exposed_any_asm==0 |asm_exposure==asm_names[a] )
print(paste0("beginning matching process for ", asm_names[a]," exposures" ))

source("control_matching/propensity_matching_process.r")

  
  file_name <- paste0("propensity_score_match_preg_outcomes_",asm_names[a] ,  ".rds")
saveRDS(matched_dataset1,paste0(data_path,"matched_cohorts/", file_name ) )

}


## Congenital conditions main####
##Link pregnancy outcomes cohort any asm####
df1 <- readRDS(paste0(data_path, "linkage/master_dataset_file.rds")) %>% 
  mutate(pregnancy_loss = case_when(fetus_outcome1=="Unknown - assumed early loss"~ "Yes" , T~pregnancy_loss) ) %>% 
  mutate(maternal_age_group_conception = case_when(maternal_age_group_conception == '<20' ~ 'less than 20',
                                                   maternal_age_group_conception == '20-24' ~ '20 to 24',
                                                   maternal_age_group_conception == '25-29' ~ '25 to 29',
                                                   maternal_age_group_conception == '30-34' ~ '30 to 34',
                                                   maternal_age_group_conception == '35-39' ~ '35 to 39',
                                                   maternal_age_group_conception == '40+' ~ '40 plus',
                                                   T ~ maternal_age_group_conception),
         mother_simd = case_when(is.na(mother_simd) ~ 'Unknown',
                                 T ~ mother_simd))
df1$gest_first_exposed <-as.numeric(df1$gest_first_exposed)
df1 <- df1 %>% mutate(asm_exposure = case_when(CC_exposed_carbamazepine_mono ==1~ "carbamazepine", 
                                               CC_exposed_gabapentin_mono ==1 ~ "gabapentin", 
                                               CC_exposed_lamotrigine_mono ==1 ~ "lamotrigine", 
                                               CC_exposed_levetiracetam_mono == 1 ~ "levetiracetam", 
                                               CC_exposed_pregabalin_mono == 1 ~ "pregabalin", 
                                               CC_exposed_topiramate_mono ==1 ~"topiramate", 
                                               CC_exposed_valproate_mono ==1 ~ "valproate"))
outcome <- "congenital"
df <- df1 %>% filter(control_pool_CC==1| cases_CC==1)

source("control_matching/propensity_matching_process.r")

saveRDS(matched_dataset1,
        paste0(data_path, "matched_cohorts/propensity_score_match_CC_outcomes_cohort.rds") )

###all monotherapies CC####
asm_names <- c("carbamazepine",  "gabapentin", "lamotrigine", "levetiracetam",
               "pregabalin", "topiramate", "valproate")

##
for(a in 1:length(asm_names)){
  asm_names <- c("carbamazepine",  "gabapentin", "lamotrigine", "levetiracetam",
                 "pregabalin", "topiramate", "valproate")
  
  df <- df1 %>% filter(control_pool_CC==1| cases_CC==1) %>% 
    filter(CC_exposed_any_asm==0 |asm_exposure==asm_names[a] )
  print(paste0("beginning matching process for ", asm_names[a]," exposures" ))
  
  source("control_matching/propensity_matching_process.r")
  
  
  file_name <- paste0("propensity_score_match_CC_outcomes_",asm_names[a] ,  ".rds")
  saveRDS(matched_dataset1,paste0(data_path,"matched_cohorts/", file_name ) )
  
}

## Child development main####
df1 <- readRDS(paste0(data_path, "linkage/master_dataset_file.rds")) %>% 
  mutate(pregnancy_loss = case_when(fetus_outcome1=="Unknown - assumed early loss"~ "Yes" , T~pregnancy_loss) ) %>% 
  mutate(maternal_age_group_conception = case_when(maternal_age_group_conception == '<20' ~ 'less than 20',
                                                   maternal_age_group_conception == '20-24' ~ '20 to 24',
                                                   maternal_age_group_conception == '25-29' ~ '25 to 29',
                                                   maternal_age_group_conception == '30-34' ~ '30 to 34',
                                                   maternal_age_group_conception == '35-39' ~ '35 to 39',
                                                   maternal_age_group_conception == '40+' ~ '40 plus',
                                                   T ~ maternal_age_group_conception),
         mother_simd = case_when(is.na(mother_simd) ~ 'Unknown',
                                 T ~ mother_simd))
df1$gest_first_exposed <-as.numeric(df1$gest_first_exposed)

df1 <- df1 %>% mutate(parity = case_when(n_prev_pregnancies ==0 ~"0",
                           n_prev_pregnancies >=1 ~"1+", T~"Unknown"))

df1 <- df1 %>% mutate(asm_exposure = case_when(exposed_carbamazepine_mono ==1~ "carbamazepine", 
                                               exposed_gabapentin_mono ==1 ~ "gabapentin", 
                                               exposed_lamotrigine_mono ==1 ~ "lamotrigine", 
                                               exposed_levetiracetam_mono == 1 ~ "levetiracetam", 
                                               exposed_pregabalin_mono == 1 ~ "pregabalin", 
                                               exposed_topiramate_mono ==1 ~"topiramate", 
                                               exposed_valproate_mono ==1 ~ "valproate"))
outcome <- "developmental"
df <- df1 %>% filter(control_pool_dev==1| cases_dev==1)

source("control_matching/propensity_matching_process.r")

saveRDS(matched_dataset1,
        paste0(data_path, "matched_cohorts/propensity_score_match_development_cohort.rds") )

###all monotherapies CC####
asm_names <- c("carbamazepine",  "gabapentin", "lamotrigine", "levetiracetam",
               "pregabalin", "topiramate", "valproate")

##
for(a in 1:length(asm_names)){
  asm_names <- c("carbamazepine",  "gabapentin", "lamotrigine", "levetiracetam",
                 "pregabalin", "topiramate", "valproate")
  
  df <- df1 %>% filter(control_pool_dev==1| cases_dev==1) %>% 
    filter(exposed_any_asm==0 |asm_exposure==asm_names[a] )
  print(paste0("beginning matching process for ", asm_names[a]," exposures" ))
  
  source("control_matching/propensity_matching_process.r")
  
  
  file_name <- paste0("propensity_score_match_development_",asm_names[a] ,  ".rds")
  saveRDS(matched_dataset1,paste0(data_path,"matched_cohorts/", file_name ) )
  
}

####
##Assessing balance of covariates ####
##nOw need to assess balance of covariates, and consider if we need to change parameters for matching
# at the mo we use no calipers on any variable and no limits on prop score  - just use NND with replacement.


cohort_id <- "preg_main"
##Mean difference tables####
### Pregnancy outcomes cohort####

matched_dataset1 <- 
  readRDS(paste0(data_path, "matched_cohorts/propensity_score_match_preg_outcomes_cohort.rds") )
matched_tab <- 
  bal.tab(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
            maternal_age_conception+ mother_simd, data = matched_dataset1, disp = "means")

unmatched_tab <-
  bal.tab(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
            maternal_age_conception+ mother_simd, data = df, disp = "means")

str(matched_tab)

tab1 <- unmatched_tab$Balance
tab1$var_name <- rownames(tab1)
tab1 <- tab1 %>% select(var_name, Type, M.0.Un, M.1.Un, Diff.Un) 
names(tab1) <- c("variable names", "Type", "Mean Unmatched - Untreated", 
                 "Mean Unmatched - treated", "Std. mean diff unmatched")

tab2 <- matched_tab$Balance
tab2$var_name <- rownames(tab2)
tab2 <- tab2 %>% select(var_name, Type, M.0.Un, M.1.Un, Diff.Un) 
names(tab2) <- c("variable names","Type", "Mean Matched - Untreated",
                 "Mean Matched - treated", "Std. mean diff Matched")
tab <- left_join(tab1, tab2)

saveRDS(matched_tab, paste0(data_path, "matched_cohorts/checking_balance/", cohort_id, "_matched_tab.rds"))
saveRDS(unmatched_tab, paste0(data_path, "matched_cohorts/checking_balance/", cohort_id, "_unmatched_tab.rds"))
##balance plots####
###Age####
bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_group_conception+ mother_simd, data = matched_dataset1,  
         treat = matched_dataset1$exposed_any_asm, var.name = "maternal_age_conception",
         sample.names="Matched on propensity scores")

bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_group_conception+ mother_simd, data = df,  
         treat = df$exposed_any_asm, var.name = "maternal_age_conception", sample.names = "Unmatched")

###Year conception####
bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = matched_dataset1,  
         treat = matched_dataset1$exposed_any_asm, var.name = "year_conception",
         sample.names="Matched on propensity scores")

bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = df,  
         treat = df$exposed_any_asm, var.name = "year_conception", sample.names = "Unmatched")

### SIMD####
bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = matched_dataset1,  
         treat = matched_dataset1$exposed_any_asm, var.name = "mother_simd",
         sample.names="Matched on propensity scores")

bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = df,  
         treat = df$exposed_any_asm, var.name = "mother_simd", sample.names = "Unmatched")

### indicator flags####
bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = matched_dataset1,  
         treat = matched_dataset1$exposed_any_asm, var.name = "epilepsy_indication",
         sample.names="Matched on propensity scores")

bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = df,  
         treat = df$exposed_any_asm, var.name = "epilepsy_indication", sample.names = "Unmatched")


bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = matched_dataset1,  
         treat = matched_dataset1$exposed_any_asm, var.name = "mh_flag",
         sample.names="Matched on propensity scores")

bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = df,  
         treat = df$exposed_any_asm, var.name = "mh_flag", sample.names = "Unmatched")

bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = matched_dataset1,  
         treat = matched_dataset1$exposed_any_asm, var.name = "migraine_pain_flag",
         sample.names="Matched on propensity scores")

bal.plot(exposed_any_asm ~ year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
           maternal_age_conception+ mother_simd, data = df,  
         treat = df$exposed_any_asm, var.name = "migraine_pain_flag", sample.names = "Unmatched")


