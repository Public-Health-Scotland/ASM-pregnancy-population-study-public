##01b.congenital_propensity_models.r


###Propensity score modelling main analyses####
library(dplyr)
library(MatchIt)
library(marginaleffects)
library(survival)
library(tidyr)
#source filepaths
source("05-stat_analysis/00.stat_setup.r")

##Link pregnancy outcomes cohort####
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
df$gest_first_exposed <-as.numeric(df$gest_first_exposed)
outcome <- "congenital"
df <- df %>% filter(cases_CC==1 | control_pool_CC==1)

###COngenital condition outcomes###
##All ASM####

##Balance checks
##balance checks showed that the following variables were not sufficiently balanced so should
# not be added to the models: 


##methods for estimand


# "It’s usually a good idea to include treatment-covariate interactions, which we do below,
#but this is not always necessary, especially when excellent balance has been achieved. "
##check variable balance - what do you do if not balanced? drop them? include interactions?
##overall, gabapenting and pregabalin have pretty well balanced covars.
# but i think with the others, there are always some not balanced,
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_CC_outcomes_cohort.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds")) %>%
  filter(cases_CC==1 | control_pool_CC==1)
outcomes <- df %>% select(pregnancy_id,any_CC)
matched_df <- left_join(matched_any, outcomes)
outcome <- "congenital"

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#


events <- matched_df %>% group_by(CC_exposed_any_asm, any_CC) %>% count %>%
  pivot_wider(names_from = any_CC, values_from  =n, names_prefix = "any_CC") %>%
  mutate(perc_CC = any_CC1/(any_CC0+any_CC1)*100, 
         perc_noCC = any_CC0/(any_CC0+any_CC1)*100, ) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(any_CC0,any_CC1, perc_CC, perc_noCC))

events$variable <- "CC_exposed_any_asm"
#500+ events

fit1 <- clogit(any_CC ~ CC_exposed_any_asm + 
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

fit2 <- clogit(any_CC ~ CC_exposed_any_asm +  year_conception + #epilepsy_indication +
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
or_prop_all <- or_prop_all %>% filter(variable== "CC_exposed_any_asm")


results <- rbind(or_prop_all, or_prop_only)
results$outcome <- "congenital"
results

## Carbamazepine####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_CC_outcomes_carbamazepine.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,any_CC)
matched_df <- left_join(matched_any, outcomes)


events1 <- matched_df %>% group_by(CC_exposed_any_asm, any_CC) %>% count %>%
  pivot_wider(names_from = any_CC, values_from  =n, names_prefix = "any_CC") %>%
  mutate(perc_CC = any_CC1/(any_CC0+any_CC1)*100, 
         perc_noCC = any_CC0/(any_CC0+any_CC1)*100, ) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(any_CC0,any_CC1, perc_CC, perc_noCC))


events1$variable <- "CC_exposed_carbamazepine_mono"
events<- rbind(events, events1)

##34 events  - 5 events per variable- so can only add up to 6 binary variabels
##could get away with mh flag and simd as the least balanced (not bad though)

fit1 <- clogit(any_CC ~ CC_exposed_carbamazepine_mono + 
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

fit2 <- clogit(any_CC ~ CC_exposed_carbamazepine_mono + 
                 year_conception +
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
or_prop_all <- or_prop_all %>% filter(variable== "CC_exposed_carbamazepine_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "congenital"

results <- rbind(results, results1)

###Gabapentin ####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_CC_outcomes_gabapentin.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,any_CC)
matched_df <- left_join(matched_any, outcomes)


events1 <- matched_df %>% group_by(CC_exposed_any_asm, any_CC) %>% count %>%
  pivot_wider(names_from = any_CC, values_from  =n, names_prefix = "any_CC") %>%
  mutate(perc_CC = any_CC1/(any_CC0+any_CC1)*100, 
         perc_noCC = any_CC0/(any_CC0+any_CC1)*100, ) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(any_CC0,any_CC1, perc_CC, perc_noCC))


events1$variable <- "CC_exposed_gabapentin_mono"
events<- rbind(events, events1)

##110 events  - enoug h for full model
fit1 <- clogit(any_CC ~ CC_exposed_gabapentin_mono + 
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

fit2 <- clogit(any_CC ~ CC_exposed_gabapentin_mono + 
                  year_conception+
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
or_prop_all <- or_prop_all %>% filter(variable== "CC_exposed_gabapentin_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "congenital"

results <- rbind(results, results1)



###lamotrigine####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_CC_outcomes_lamotrigine.rds") )
#outcomes <- df %>% select(pregnancy_id,any_CC)
matched_df <- left_join(matched_any, outcomes)

events1 <- matched_df %>% group_by(CC_exposed_any_asm, any_CC) %>% count %>%
  pivot_wider(names_from = any_CC, values_from  =n, names_prefix = "any_CC") %>%
  mutate(perc_CC = any_CC1/(any_CC0+any_CC1)*100, 
         perc_noCC = any_CC0/(any_CC0+any_CC1)*100, ) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(any_CC0,any_CC1, perc_CC, perc_noCC))

events1$variable <- "CC_exposed_lamotrigine_mono"
events<- rbind(events, events1)

##71 events  - not enough for full model - collapse either age or simd
fit1 <- clogit(any_CC ~ CC_exposed_lamotrigine_mono+ 
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

fit2 <- clogit(any_CC ~ CC_exposed_lamotrigine_mono+ 
                 year_conception+
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
or_prop_all <- or_prop_all %>% filter(variable== "CC_exposed_lamotrigine_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "congenital"

results <- rbind(results, results1)


###leveteracitam####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_CC_outcomes_levetiracetam.rds") )
matched_df <- left_join(matched_any, outcomes)

events1 <- matched_df %>% group_by(CC_exposed_any_asm, any_CC) %>% count %>%
  pivot_wider(names_from = any_CC, values_from  =n, names_prefix = "any_CC") %>%
  mutate(perc_CC = any_CC1/(any_CC0+any_CC1)*100, 
         perc_noCC = any_CC0/(any_CC0+any_CC1)*100, ) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(any_CC0,any_CC1, perc_CC, perc_noCC))

events1$variable <- "CC_exposed_levetiracetam_mono"
events<- rbind(events, events1)

##39 events  - not enough for full model - enough for only 7 parameters
fit1 <- clogit(any_CC ~ CC_exposed_levetiracetam_mono+ 
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
##ajust - year only
fit2 <- clogit(any_CC ~  CC_exposed_levetiracetam_mono + 
                 year_conception+
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
or_prop_all <- or_prop_all %>% filter(variable== "CC_exposed_levetiracetam_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "congenital"

results <- rbind(results, results1)


###pregabalin####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_CC_outcomes_pregabalin.rds") )

matched_df <- left_join(matched_any, outcomes)

events1 <- matched_df %>% group_by(CC_exposed_any_asm, any_CC) %>% count %>%
  pivot_wider(names_from = any_CC, values_from  =n, names_prefix = "any_CC") %>%
  mutate(perc_CC = any_CC1/(any_CC0+any_CC1)*100, 
         perc_noCC = any_CC0/(any_CC0+any_CC1)*100, ) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(any_CC0,any_CC1, perc_CC, perc_noCC))


events1$variable <- "CC_exposed_pregabalin_mono"
events<- rbind(events, events1)

##72 events  - not enough for full model - collapse age or simd
fit1 <- clogit(any_CC ~ CC_exposed_pregabalin_mono+ 
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


fit2 <- clogit(any_CC ~  CC_exposed_pregabalin_mono + 
                 year_conception+
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
or_prop_all <- or_prop_all %>% filter(variable== "CC_exposed_pregabalin_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "congenital"

results <- rbind(results, results1)

###topiramate####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_CC_outcomes_topiramate.rds") )

matched_df <- left_join(matched_any, outcomes)

events1 <- matched_df %>% group_by(CC_exposed_any_asm, any_CC) %>% count %>%
  pivot_wider(names_from = any_CC, values_from  =n, names_prefix = "any_CC") %>%
  mutate(perc_CC = any_CC1/(any_CC0+any_CC1)*100, 
         perc_noCC = any_CC0/(any_CC0+any_CC1)*100, ) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(any_CC0,any_CC1, perc_CC, perc_noCC))

events1$variable <- "CC_exposed_topiramate_mono"
events<- rbind(events, events1)

##13 events  - only 2 parameters
fit1 <- clogit(any_CC ~ CC_exposed_topiramate_mono+ 
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

fit2 <- clogit(any_CC ~   CC_exposed_topiramate_mono+ 
                 year_conception+
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
or_prop_all <- or_prop_all %>% filter(variable== "CC_exposed_topiramate_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "congenital"

results <- rbind(results, results1)

###valproate####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_CC_outcomes_valproate.rds") )
#outcomes <- df %>% select(pregnancy_id,any_CC)
matched_df <- left_join(matched_any, outcomes)


events1 <- matched_df %>% group_by(CC_exposed_any_asm, any_CC) %>% count %>%
  pivot_wider(names_from = any_CC, values_from  =n, names_prefix = "any_CC") %>%
  mutate(perc_CC = any_CC1/(any_CC0+any_CC1)*100, 
         perc_noCC = any_CC0/(any_CC0+any_CC1)*100, ) %>%
  pivot_wider(names_from = CC_exposed_any_asm, values_from = c(any_CC0,any_CC1, perc_CC, perc_noCC))

events1$variable <- "CC_exposed_valproate_mono"
events<- rbind(events, events1)

##29 events  -
fit1 <- clogit(any_CC ~ CC_exposed_valproate_mono+
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
or_prop_only$model_type <- "propensity score only"####

fit2 <- clogit(any_CC ~  CC_exposed_valproate_mono+
                 year_conception+
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
or_prop_all <- or_prop_all %>% filter(variable== "CC_exposed_valproate_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "congenital"

results <- rbind(results, results1)

events
results
results <- results %>% mutate(model_type=case_when(model_type=="doubley robust - limited variables"~ 
                                                   "doubley robust", T~model_type))
results <- left_join(results, events)

write.csv(results, paste0(folder_data_path, "stats/propensity_models_CC_outcomes.csv"), row.names=FALSE)
