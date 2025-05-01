##01c.developmental_propensity_models.r


###Propensity score modelling main analyses####
library(dplyr)
library(MatchIt)
library(marginaleffects)
library(survival)
library(tidyr)

source("05-stat_analysis/00.stat_setup.r")



###Any ASM####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_development_cohort.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
df <- df %>%  filter(cases_dev ==1 | control_pool_dev==1)
outcomes <- df %>% select(pregnancy_id, any_dev_excl_v_h)
matched_df <- left_join(matched_any, outcomes)
matched_df <- matched_df %>%
  mutate(any_concern_bin = case_when(any_dev_excl_v_h=="Y" ~1, 
                                     any_dev_excl_v_h=="N" ~0, ))
outcome <- "developmental"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
events <- matched_df %>% group_by(exposed_any_asm, any_dev_excl_v_h) %>% count %>%
  pivot_wider(names_from = any_dev_excl_v_h, values_from  =n, names_prefix = "any_dev_concern") %>%
  mutate(perc_concern = any_dev_concernY/(any_dev_concernN+any_dev_concernY)*100, 
         perc_No_concern = any_dev_concernN/(any_dev_concernN+any_dev_concernY)*100 ) %>%
 # select(-any_dev_concernN)  %>% 
  pivot_wider(names_from = exposed_any_asm,
              values_from = c(any_dev_concernN,any_dev_concernY,perc_No_concern, perc_concern))

events$variable <- "exposed_any_asm"
#2000+ events

fit1 <- clogit(any_concern_bin~ exposed_any_asm + baby_sex+
                 strata(subclass),
               data = matched_df #,#weights = weights
)
summary(fit1)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit1)),
               exp(confint(fit1)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_only <- round(OR.CI, 3)

or_prop_only <-(as.data.frame(or_prop_only))
or_prop_only$variable <- rownames(or_prop_only)
or_prop_only$model_type <- "propensity score only"
or_prop_only <- or_prop_only %>% filter(variable== "exposed_any_asm")

###adjust additionally for year only
fit2 <- clogit(any_concern_bin ~ exposed_any_asm +  year_conception + baby_sex+
                 strata(subclass),
               data = matched_df )
summary(fit2)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),
               exp(confint(fit2)),"pvalue" = summary(fit2)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_any_asm")


results <- rbind(or_prop_all, or_prop_only)
results$outcome <- "developmental"
results

###gabapentin####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_development_gabapentin.rds") )

matched_df <- left_join(matched_any, outcomes)
matched_df <- matched_df %>%
  mutate(any_concern_bin = case_when(any_dev_excl_v_h=="Y" ~1, 
                                     any_dev_excl_v_h=="N" ~0, ))
outcome <- "developmental"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
events1 <- matched_df %>% group_by(exposed_gabapentin_mono, any_dev_excl_v_h) %>% count %>%
  pivot_wider(names_from = any_dev_excl_v_h, values_from  =n, names_prefix = "any_dev_concern") %>%
  mutate(perc_concern = any_dev_concernY/(any_dev_concernN+any_dev_concernY)*100, 
         perc_No_concern = any_dev_concernN/(any_dev_concernN+any_dev_concernY)*100 ) %>%
  pivot_wider(names_from = exposed_gabapentin_mono,
              values_from = c(any_dev_concernN,any_dev_concernY,perc_No_concern, perc_concern))

events1$variable <- "exposed_gabapentin_mono"
events<- rbind(events, events1)
#500+ events

fit1 <- clogit(any_concern_bin~ exposed_gabapentin_mono + baby_sex+
                 strata(subclass),
               data = matched_df #,#weights = weights
)
summary(fit1)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit1)),
               exp(confint(fit1)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_only <- round(OR.CI, 3)

or_prop_only <-(as.data.frame(or_prop_only))
or_prop_only$variable <- rownames(or_prop_only)
or_prop_only$model_type <- "propensity score only"
or_prop_only <- or_prop_only %>% filter(variable== "exposed_gabapentin_mono")

###adjust additionally for year only
fit2 <- clogit(any_concern_bin ~ exposed_gabapentin_mono +  year_conception + baby_sex+
                 strata(subclass),
               data = matched_df )
summary(fit2)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),
               exp(confint(fit2)),"pvalue" = summary(fit2)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_gabapentin_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "developmental"
results  <- rbind(results,results1 )



### Pregabalin ####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_development_pregabalin.rds") )

matched_df <- left_join(matched_any, outcomes)
matched_df <- matched_df %>%
  mutate(any_concern_bin = case_when(any_dev_excl_v_h=="Y" ~1, 
                                     any_dev_excl_v_h=="N" ~0, ))
outcome <- "developmental"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
events1 <- matched_df %>% group_by(exposed_pregabalin_mono, any_dev_excl_v_h) %>% count %>%
  pivot_wider(names_from = any_dev_excl_v_h, values_from  =n, names_prefix = "any_dev_concern") %>%
  mutate(perc_concern = any_dev_concernY/(any_dev_concernN+any_dev_concernY)*100, 
         perc_No_concern = any_dev_concernN/(any_dev_concernN+any_dev_concernY)*100 ) %>%
  pivot_wider(names_from = exposed_pregabalin_mono,
              values_from = c(any_dev_concernN,any_dev_concernY,perc_No_concern, perc_concern))
events1$variable <- "exposed_pregabalin_mono"
events<- rbind(events, events1)
#500+ events

fit1 <- clogit(any_concern_bin~ exposed_pregabalin_mono + baby_sex+
                 strata(subclass),
               data = matched_df #,#weights = weights
)
summary(fit1)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit1)),
               exp(confint(fit1)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_only <- round(OR.CI, 3)

or_prop_only <-(as.data.frame(or_prop_only))
or_prop_only$variable <- rownames(or_prop_only)
or_prop_only$model_type <- "propensity score only"
or_prop_only <- or_prop_only %>% filter(variable== "exposed_pregabalin_mono")

###adjust additionally for year only
fit2 <- clogit(any_concern_bin ~ exposed_pregabalin_mono +  year_conception +  baby_sex+
                 strata(subclass),
               data = matched_df )
summary(fit2)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),
               exp(confint(fit2)),"pvalue" = summary(fit2)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_pregabalin_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "developmental"
results  <- rbind(results,results1 )

### Carbamazepine ####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_development_carbamazepine.rds") )

matched_df <- left_join(matched_any, outcomes)
matched_df <- matched_df %>%
  mutate(any_concern_bin = case_when(any_dev_excl_v_h=="Y" ~1, 
                                     any_dev_excl_v_h=="N" ~0, ))
outcome <- "developmental"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
events1 <- matched_df %>% group_by(exposed_carbamazepine_mono, any_dev_excl_v_h) %>% count %>%
  pivot_wider(names_from = any_dev_excl_v_h, values_from  =n, names_prefix = "any_dev_concern") %>%
  mutate(perc_concern = any_dev_concernY/(any_dev_concernN+any_dev_concernY)*100, 
         perc_No_concern = any_dev_concernN/(any_dev_concernN+any_dev_concernY)*100 ) %>%
  pivot_wider(names_from = exposed_carbamazepine_mono,
              values_from = c(any_dev_concernN,any_dev_concernY,perc_No_concern, perc_concern))

events1$variable <- "exposed_carbamazepine_mono"
events<- rbind(events, events1)
events
#150+ events

fit1 <- clogit(any_concern_bin~ exposed_carbamazepine_mono + baby_sex+
                 strata(subclass),
               data = matched_df #,#weights = weights
)
summary(fit1)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit1)),
               exp(confint(fit1)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_only <- round(OR.CI, 3)

or_prop_only <-(as.data.frame(or_prop_only))
or_prop_only$variable <- rownames(or_prop_only)
or_prop_only$model_type <- "propensity score only"
or_prop_only <- or_prop_only %>% filter(variable== "exposed_carbamazepine_mono")

##adjust additionally for year,and also MH, migraine, as these are unbalanced
fit2 <- clogit(any_concern_bin ~ exposed_carbamazepine_mono +  year_conception +
                 mh_flag +
                  baby_sex+
                 strata(subclass),
               data = matched_df )
summary(fit2)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),
               exp(confint(fit2)),"pvalue" = summary(fit2)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_carbamazepine_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "developmental"
results  <- rbind(results,results1 )
results 


### Lamotrigine ####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_development_lamotrigine.rds") )

matched_df <- left_join(matched_any, outcomes)
matched_df <- matched_df %>%
  mutate(any_concern_bin = case_when(any_dev_excl_v_h=="Y" ~1, 
                                     any_dev_excl_v_h=="N" ~0, ))
outcome <- "developmental"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
events1 <- matched_df %>% group_by(exposed_lamotrigine_mono, any_dev_excl_v_h) %>% count %>%
  pivot_wider(names_from = any_dev_excl_v_h, values_from  =n, names_prefix = "any_dev_concern") %>%
  mutate(perc_concern = any_dev_concernY/(any_dev_concernN+any_dev_concernY)*100, 
         perc_No_concern = any_dev_concernN/(any_dev_concernN+any_dev_concernY)*100 ) %>%
  pivot_wider(names_from = exposed_lamotrigine_mono,
              values_from = c(any_dev_concernN,any_dev_concernY,perc_No_concern, perc_concern))

events1$variable <- "exposed_lamotrigine_mono"
events<- rbind(events, events1)
events
#400+ events

fit1 <- clogit(any_concern_bin~ exposed_lamotrigine_mono + baby_sex+
                 strata(subclass),
               data = matched_df #,#weights = weights
)
summary(fit1)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit1)),
               exp(confint(fit1)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_only <- round(OR.CI, 3)

or_prop_only <-(as.data.frame(or_prop_only))
or_prop_only$variable <- rownames(or_prop_only)
or_prop_only$model_type <- "propensity score only"
or_prop_only <- or_prop_only %>% filter(variable== "exposed_lamotrigine_mono")

###adjust additionally for year  and migraine/pain
fit2 <- clogit(any_concern_bin ~ exposed_lamotrigine_mono +  year_conception +
                     
                  baby_sex+
                 strata(subclass),
               data = matched_df )
summary(fit2)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),
               exp(confint(fit2)),"pvalue" = summary(fit2)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_lamotrigine_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "developmental"
results  <- rbind(results,results1 )
results 

### Levetiracetam ####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_development_levetiracetam.rds") )

matched_df <- left_join(matched_any, outcomes)
matched_df <- matched_df %>%
  mutate(any_concern_bin = case_when(any_dev_excl_v_h=="Y" ~1, 
                                     any_dev_excl_v_h=="N" ~0, ))
outcome <- "developmental"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
events1 <- matched_df %>% group_by(exposed_levetiracetam_mono, any_dev_excl_v_h) %>% count %>%
  pivot_wider(names_from = any_dev_excl_v_h, values_from  =n, names_prefix = "any_dev_concern") %>%
  mutate(perc_concern = any_dev_concernY/(any_dev_concernN+any_dev_concernY)*100, 
         perc_No_concern = any_dev_concernN/(any_dev_concernN+any_dev_concernY)*100 ) %>%
  pivot_wider(names_from = exposed_levetiracetam_mono,
              values_from = c(any_dev_concernN,any_dev_concernY,perc_No_concern, perc_concern))

events1$variable <- "exposed_levetiracetam_mono"
events<- rbind(events, events1)
events
#200+ events

fit1 <- clogit(any_concern_bin~ exposed_levetiracetam_mono + baby_sex+
                 strata(subclass),
               data = matched_df #,#weights = weights
)
summary(fit1)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit1)),
               exp(confint(fit1)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_only <- round(OR.CI, 3)

or_prop_only <-(as.data.frame(or_prop_only))
or_prop_only$variable <- rownames(or_prop_only)
or_prop_only$model_type <- "propensity score only"
or_prop_only <- or_prop_only %>% filter(variable== "exposed_levetiracetam_mono")

###adjust additionally for year only
fit2 <- clogit(any_concern_bin ~ exposed_levetiracetam_mono +  year_conception +baby_sex+
                 strata(subclass),
               data = matched_df )
summary(fit2)

# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),
               exp(confint(fit2)),"pvalue" = summary(fit2)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_levetiracetam_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "developmental"
results  <- rbind(results,results1 )
results 


### Topiramate ####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_development_topiramate.rds") )

matched_df <- left_join(matched_any, outcomes)
matched_df <- matched_df %>%
  mutate(any_concern_bin = case_when(any_dev_excl_v_h=="Y" ~1, 
                                     any_dev_excl_v_h=="N" ~0, ))
outcome <- "developmental"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
events1 <- matched_df %>% group_by(exposed_topiramate_mono, any_dev_excl_v_h) %>% count %>%
  pivot_wider(names_from = any_dev_excl_v_h, values_from  =n, names_prefix = "any_dev_concern") %>%
  mutate(perc_concern = any_dev_concernY/(any_dev_concernN+any_dev_concernY)*100, 
         perc_No_concern = any_dev_concernN/(any_dev_concernN+any_dev_concernY)*100 ) %>%
  pivot_wider(names_from = exposed_topiramate_mono,
              values_from = c(any_dev_concernN,any_dev_concernY,perc_No_concern, perc_concern))

events1$variable <- "exposed_topiramate_mono"
events<- rbind(events, events1)
events
#71 events - cant do all of the variables

fit1 <- clogit(any_concern_bin~ exposed_topiramate_mono+ baby_sex+
                 strata(subclass),
               data = matched_df #,#weights = weights
)
summary(fit1)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit1)),
               exp(confint(fit1)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_only <- round(OR.CI, 3)

or_prop_only <-(as.data.frame(or_prop_only))
or_prop_only$variable <- rownames(or_prop_only)
or_prop_only$model_type <- "propensity score only"
or_prop_only <- or_prop_only %>% filter(variable== "exposed_topiramate_mono")

##cant add all just add least balanced and baby sex
##epilepsy migraine and age all have values >0.05
fit2 <- clogit(any_concern_bin ~ exposed_topiramate_mono + 
                 year_conception + 
                    migraine_pain_flag +
                       baby_sex+
                 strata(subclass),
               data = matched_df )
summary(fit2)

# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),
               exp(confint(fit2)),"pvalue" = summary(fit2)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_topiramate_mono")




results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "developmental"
results  <- rbind(results,results1 )
results 

### Valproate ####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_development_valproate.rds") )

matched_df <- left_join(matched_any, outcomes)
matched_df <- matched_df %>%
  mutate(any_concern_bin = case_when(any_dev_excl_v_h=="Y" ~1, 
                                     any_dev_excl_v_h=="N" ~0, ))
outcome <- "developmental"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
events1 <- matched_df %>% group_by(exposed_valproate_mono, any_dev_excl_v_h) %>% count %>%
  pivot_wider(names_from = any_dev_excl_v_h, values_from  =n, names_prefix = "any_dev_concern") %>%
  mutate(perc_concern = any_dev_concernY/(any_dev_concernN+any_dev_concernY)*100, 
         perc_No_concern = any_dev_concernN/(any_dev_concernN+any_dev_concernY)*100 ) %>%
  pivot_wider(names_from = exposed_valproate_mono,
              values_from = c(any_dev_concernN,any_dev_concernY,perc_No_concern, perc_concern))

events1$variable <- "exposed_valproate_mono"
events<- rbind(events, events1)
events
#96 events - cant do all of the variables

fit1 <- clogit(any_concern_bin~ exposed_valproate_mono + baby_sex+
                 strata(subclass),
               data = matched_df #,#weights = weights
)
summary(fit1)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit1)),
               exp(confint(fit1)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_only <- round(OR.CI, 3)

or_prop_only <-(as.data.frame(or_prop_only))
or_prop_only$variable <- rownames(or_prop_only)
or_prop_only$model_type <- "propensity score only"
or_prop_only <- or_prop_only %>% filter(variable== "exposed_valproate_mono")

##additionally adjust for year, MH, pain, parity, smoking
fit2 <- clogit(any_concern_bin ~ exposed_valproate_mono + 
                  year_conception + 
                   mh_flag +
                 migraine_pain_flag +
                 # drug_alcohol_use+any_smr_comorb+
                # maternal_age_group_conception+
                # mother_simd+
                 # maternal_bmi_group +
                 maternal_smoking+
            #     parity +
                 baby_sex+
                 strata(subclass),
               data = matched_df )
summary(fit2)

# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),
               exp(confint(fit2)),"pvalue" = summary(fit2)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_valproate_mono")




results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "developmental"
results  <- rbind(results,results1 )

results
events
results <- left_join(results, events)

write.csv(results, paste0(folder_data_path, "stats/propensity_models_development_outcomes.csv"), row.names=FALSE)

