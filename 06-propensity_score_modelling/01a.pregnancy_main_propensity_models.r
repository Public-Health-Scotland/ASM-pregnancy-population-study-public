##01a.pregnancy_main_propensity_models.r

###Propensity score modelling main analyses####
library(dplyr)
library(MatchIt)
library(marginaleffects)
library(survival)
library(tidyr)

#source filepaths
source("05-stat_analysis/00.stat_setup.r")

###Pregnancy outcomes###
##All ASM####

##Balance checks
##balance checks showed that the following variables were not sufficiently balanced so should
# not be added to the models: 


##methods for estimand

#https://cran.r-project.org/web/packages/MatchIt/vignettes/estimating-effects.html
# "It’s usually a good idea to include treatment-covariate interactions, which we do below,
#but this is not always necessary, especially when excellent balance has been achieved. "
##check variable balance - what do you do if not balanced? drop them? include interactions?
##overall, gabapenting and pregabalin have pretty well balanced covars.
# but i think with the others, there are always some not balanced,
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_preg_outcomes_cohort.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,pregnancy_loss)
matched_df <- left_join(matched_any, outcomes)

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
matched_df <- matched_df %>%
  mutate(pregnancy_loss_bin = case_when(pregnancy_loss=="No" ~0,
                                       pregnancy_loss=="Yes" ~1))
table(matched_df$pregnancy_loss)

events <- matched_df %>% group_by(exposed_any_asm, pregnancy_loss) %>% count %>%
  pivot_wider(names_from = pregnancy_loss, values_from  =n, names_prefix = "pregnancy_loss") %>%
  mutate(perc_loss = pregnancy_lossYes/(pregnancy_lossNo+pregnancy_lossYes)*100,
         perc_not_loss = pregnancy_lossNo/(pregnancy_lossNo+pregnancy_lossYes)*100,) %>%
#  select(-pregnancy_lossNo)  %>% 
  pivot_wider(names_from = exposed_any_asm, values_from = c(pregnancy_lossNo,pregnancy_lossYes, perc_loss,perc_not_loss))

events$variable <- "exposed_any_asm"
#6108 events

##clogit


fit1 <- clogit(pregnancy_loss_bin ~ exposed_any_asm + 
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

##adjusted - just year as none that are >0.05 apart

fit2 <- clogit(pregnancy_loss_bin ~  exposed_any_asm +  year_conception+
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
results$outcome <- "pregnancy outcome"
results
####Valproate####
#year conception not blananced well. MH, comorbidity, and drug alochol also not great
matched_v <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_preg_outcomes_valproate.rds") )
df <- readRDS(paste0(folder_data_path,"linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,pregnancy_loss)
matched_df <- left_join(matched_v, outcomes)


matched_df<- matched_df %>%
  mutate(pregnancy_loss_bin = case_when(pregnancy_loss=="No" ~0,
                                        pregnancy_loss=="Yes" ~1))


events1 <- matched_df %>% group_by(exposed_valproate_mono, pregnancy_loss) %>% count %>%
  pivot_wider(names_from = pregnancy_loss, values_from  =n, names_prefix = "pregnancy_loss") %>%
  mutate(perc_loss = pregnancy_lossYes/(pregnancy_lossNo+pregnancy_lossYes)*100,
         perc_not_loss = pregnancy_lossNo/(pregnancy_lossNo+pregnancy_lossYes)*100,) %>%
  #  select(-pregnancy_lossNo)  %>% 
  pivot_wider(names_from = exposed_valproate_mono, 
              values_from = c(pregnancy_lossNo,pregnancy_lossYes, perc_loss,perc_not_loss))

events1$variable <- "exposed_valproate_mono"
events<- rbind(events, events1)


table(matched_df$pregnancy_loss)
##85 events  - full model OK.

fit1 <- clogit(pregnancy_loss_bin ~ exposed_valproate_mono + 
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
##adjusted - just year as none that are >0.05 apart
fit2 <- clogit(pregnancy_loss_bin ~ exposed_valproate_mono +  year_conception +
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
results1$outcome <- "pregnancy outcome"

results <- rbind(results, results1)

## Carbamazepine####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_preg_outcomes_carbamazepine.rds") )
df <- readRDS(paste0(folder_data_path,"linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,pregnancy_loss)
matched_df <- left_join(matched_any, outcomes)

##
#
matched_df <- matched_df %>%
  mutate(pregnancy_loss_bin = case_when(pregnancy_loss=="No" ~0,
                                        pregnancy_loss=="Yes" ~1))


events1 <- matched_df %>% group_by(exposed_any_asm, pregnancy_loss) %>% count %>%
  pivot_wider(names_from = pregnancy_loss, values_from  =n, names_prefix = "pregnancy_loss") %>%
  mutate(perc_loss = pregnancy_lossYes/(pregnancy_lossNo+pregnancy_lossYes)*100,
         perc_not_loss = pregnancy_lossNo/(pregnancy_lossNo+pregnancy_lossYes)*100,) %>%
  #  select(-pregnancy_lossNo)  %>% 
  pivot_wider(names_from = exposed_any_asm, 
              values_from = c(pregnancy_lossNo,pregnancy_lossYes, perc_loss,perc_not_loss))

events1$variable <- "exposed_carbamazepine_mono"
events<- rbind(events, events1)
table(matched_df$pregnancy_loss)
##278 events  - full model OK.

fit1 <- clogit(pregnancy_loss_bin ~ exposed_carbamazepine_mono + 
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

fit2 <- clogit(pregnancy_loss_bin ~ exposed_carbamazepine_mono + 
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
or_prop_all <- or_prop_all %>% filter(variable== "exposed_carbamazepine_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "pregnancy outcome"

results <- rbind(results, results1)



###Gabapentin####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_preg_outcomes_gabapentin.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,pregnancy_loss)
matched_df <- left_join(matched_any, outcomes)

##
#
matched_df <- matched_df %>%
  mutate(pregnancy_loss_bin = case_when(pregnancy_loss=="No" ~0,
                                        pregnancy_loss=="Yes" ~1))

events1 <- matched_df %>% group_by(exposed_any_asm, pregnancy_loss) %>% count %>%
  pivot_wider(names_from = pregnancy_loss, values_from  =n, names_prefix = "pregnancy_loss") %>%
  mutate(perc_loss = pregnancy_lossYes/(pregnancy_lossNo+pregnancy_lossYes)*100,
         perc_not_loss = pregnancy_lossNo/(pregnancy_lossNo+pregnancy_lossYes)*100,) %>%
  pivot_wider(names_from = exposed_any_asm, 
              values_from = c(pregnancy_lossNo,pregnancy_lossYes, perc_loss,perc_not_loss))

events1$variable <- "exposed_gabapentin_mono"
events<- rbind(events, events1)
##1546 events  - full model OK.

fit1 <- clogit(pregnancy_loss_bin ~ exposed_gabapentin_mono + 
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

###just include year.
fit2 <- clogit(pregnancy_loss_bin ~ exposed_gabapentin_mono + 
                 year_conception + 
                 
                 strata(subclass),
               data = matched_df )
summary(fit2)
# OR and 95% CI
OR.CI <-cbind("OR" = exp(coef(fit2)),
              exp(confint(fit2)),"pvalue" = summary(fit1)$coeff[,5])
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_gabapentin_mono")


results1 <- rbind(or_prop_all, or_prop_only)
#results1 <-or_prop_only
results1$outcome <- "pregnancy outcome"

results <- rbind(results, results1)


## lamotrigine ####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_preg_outcomes_lamotrigine.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,pregnancy_loss)
matched_df <- left_join(matched_any, outcomes)

##
#
matched_df <- matched_df %>%
  mutate(pregnancy_loss_bin = case_when(pregnancy_loss=="No" ~0,
                                        pregnancy_loss=="Yes" ~1))

events1 <- matched_df %>% group_by(exposed_any_asm, pregnancy_loss) %>% count %>%
  pivot_wider(names_from = pregnancy_loss, values_from  =n, names_prefix = "pregnancy_loss") %>%
  mutate(perc_loss = pregnancy_lossYes/(pregnancy_lossNo+pregnancy_lossYes)*100,
         perc_not_loss = pregnancy_lossNo/(pregnancy_lossNo+pregnancy_lossYes)*100,) %>%
  pivot_wider(names_from = exposed_any_asm, 
              values_from = c(pregnancy_lossNo,pregnancy_lossYes, perc_loss,perc_not_loss))

events1$variable <- "exposed_lamotrigine_mono"
events<- rbind(events, events1)
##85 events  - full model OK.

fit1 <- clogit(pregnancy_loss_bin ~ exposed_lamotrigine_mono + 
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

fit2 <- clogit(pregnancy_loss_bin ~ exposed_lamotrigine_mono + 
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
or_prop_all <- or_prop_all %>% filter(variable== "exposed_lamotrigine_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "pregnancy outcome"

results <- rbind(results, results1)

## Leveteracetam ####

matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_preg_outcomes_levetiracetam.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,pregnancy_loss)
matched_df <- left_join(matched_any, outcomes)

##
#
matched_df <- matched_df %>%
  mutate(pregnancy_loss_bin = case_when(pregnancy_loss=="No" ~0,
                                        pregnancy_loss=="Yes" ~1))



events1 <- matched_df %>% group_by(exposed_levetiracetam_mono, pregnancy_loss) %>% count %>%
  pivot_wider(names_from = pregnancy_loss, values_from  =n, names_prefix = "pregnancy_loss") %>%
  mutate(perc_loss = pregnancy_lossYes/(pregnancy_lossNo+pregnancy_lossYes)*100,
         perc_not_loss = pregnancy_lossNo/(pregnancy_lossNo+pregnancy_lossYes)*100,) %>%
  pivot_wider(names_from = exposed_levetiracetam_mono, 
              values_from = c(pregnancy_lossNo,pregnancy_lossYes, perc_loss,perc_not_loss))

events1$variable <- "exposed_levetiracetam_mono"
events<- rbind(events, events1)
##85 events  - full model OK.

fit1 <- clogit(pregnancy_loss_bin ~ exposed_levetiracetam_mono + 
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

fit2 <- clogit(pregnancy_loss_bin ~ exposed_levetiracetam_mono + 
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
or_prop_all <- or_prop_all %>% filter(variable== "exposed_levetiracetam_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "pregnancy outcome"

results <- rbind(results, results1)


###pregabalin####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_preg_outcomes_pregabalin.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,pregnancy_loss)
matched_df <- left_join(matched_any, outcomes)

##lets just assume its all balanced atm 
#(it is actually pretty good for most vars for all ASM)
#
matched_df <- matched_df %>%
  mutate(pregnancy_loss_bin = case_when(pregnancy_loss=="No" ~0,
                                        pregnancy_loss=="Yes" ~1))



events1 <- matched_df %>% group_by(exposed_pregabalin_mono, pregnancy_loss) %>% count %>%
  pivot_wider(names_from = pregnancy_loss, values_from  =n, names_prefix = "pregnancy_loss") %>%
  mutate(perc_loss = pregnancy_lossYes/(pregnancy_lossNo+pregnancy_lossYes)*100,
         perc_not_loss = pregnancy_lossNo/(pregnancy_lossNo+pregnancy_lossYes)*100,) %>%
  pivot_wider(names_from = exposed_pregabalin_mono, 
              values_from = c(pregnancy_lossNo,pregnancy_lossYes, perc_loss,perc_not_loss))

events1$variable <- "exposed_pregabalin_mono"
events<- rbind(events, events1)
table(matched_df$pregnancy_loss)
##85 events  - full model OK.

fit1 <- clogit(pregnancy_loss_bin ~ exposed_pregabalin_mono + 
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

fit2 <- clogit(pregnancy_loss_bin ~ exposed_pregabalin_mono + 
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
or_prop_all <- or_prop_all %>% filter(variable== "exposed_pregabalin_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "pregnancy outcome"

results <- rbind(results, results1)



###topiramate####
matched_any <- readRDS(paste0(folder_data_path, "matched_cohorts/propensity_score_match_preg_outcomes_topiramate.rds") )
df <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
outcomes <- df %>% select(pregnancy_id,pregnancy_loss)
matched_df <- left_join(matched_any, outcomes)

##
#
matched_df <- matched_df %>%
  mutate(pregnancy_loss_bin = case_when(pregnancy_loss=="No" ~0,
                                        pregnancy_loss=="Yes" ~1))



events1 <- matched_df %>% group_by(exposed_topiramate_mono, pregnancy_loss) %>% count %>%
  pivot_wider(names_from = pregnancy_loss, values_from  =n, names_prefix = "pregnancy_loss") %>%
  mutate(perc_loss = pregnancy_lossYes/(pregnancy_lossNo+pregnancy_lossYes)*100,
         perc_not_loss = pregnancy_lossNo/(pregnancy_lossNo+pregnancy_lossYes)*100,) %>%
  pivot_wider(names_from = exposed_topiramate_mono, 
              values_from = c(pregnancy_lossNo,pregnancy_lossYes, perc_loss,perc_not_loss))

events1$variable <- "exposed_topiramate_mono"
events<- rbind(events, events1)

table(matched_df$pregnancy_loss)
##85 events  - full model OK.

fit1 <- clogit(pregnancy_loss_bin ~ exposed_topiramate_mono + 
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

fit2 <- clogit(pregnancy_loss_bin ~ exposed_topiramate_mono + 
                 year_conception +
                 strata(subclass),
               data = matched_df )
summary(fit2)
# OR and 95% CI
OR.CI <- cbind("OR" = exp(coef(fit2)),"pvalue" = summary(fit2)$coeff[,5],
               exp(confint(fit2)))
or_prop_all <- round(OR.CI, 3)
or_prop_all <-(as.data.frame(or_prop_all))
or_prop_all$variable <- rownames(or_prop_all)
or_prop_all$model_type <- "doubley robust"
or_prop_all <- or_prop_all %>% filter(variable== "exposed_topiramate_mono")


results1 <- rbind(or_prop_all, or_prop_only)
results1$outcome <- "pregnancy outcome"

results <- rbind(results, results1)
results
events
results <- left_join(results, events)

write.csv(results, paste0(folder_data_path, "stats/propensity_models_preg_outcomes.csv"), row.names=FALSE)
