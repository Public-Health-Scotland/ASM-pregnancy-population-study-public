library(tidyverse)


# M1. Table 1 - characteristics of pregnancies --------------------------------

# Read in data 
# data file created in /Descriptives/T1a_descriptives.R
t_1 <- read_csv(paste0(folder_data_path,'Descriptives/table_1a.csv'))

# Function to capitalize the first word in a string
capitalise_first_word <- function(string) {
  str_to_sentence(string)
}

t_1 <- t_1 %>% 
  mutate(indicator = capitalise_first_word(indicator)) %>% 
  mutate(sub_indicator = capitalise_first_word(sub_indicator))

# Re-shape df into format required for markdown table
t_1_table <- t_1 %>% 
  filter(!indicator %in% c('Conception year', 'Mother_nhs_board', 'Outcome', 'Outcome - pregnancy loss')) %>% # remove unnecessary indicators
  mutate(indicator = case_when(indicator == 'Total incl unknown' ~ 'Total',
                               indicator == 'Unknown outcome' ~ 'N with unknown pregnancy outcome status',
                               indicator == 'Total' ~ 'N available for analysis',
                               indicator == 'Maternal age at conception' ~ 'Maternal age at conception (years)',
                               indicator == 'Baby sex' ~ 'Baby sex\u00B9',
                               indicator == 'Maternal bmi at booking' ~ 'Maternal BMI at booking (kg/m\u00B2)\u00B9',
                               indicator == 'Maternal smoking at booking' ~ 'Maternal smoking at booking\u00B9',
                               indicator == 'Maternal high dose folic acid' ~ 'Maternal high dose folic acid\u00B2 \u2074',
                               indicator == 'Maternal drug or alcohol use in pregnancy' ~ 'Maternal drug or alcohol use\u2074',
                               indicator == 'Maternal comorbidity flag' ~ 'Maternal comorbidity\u2074',
                               indicator == 'Number of previous deliveries' ~ 'Number of previous deliveries\u00B9',
                               T~indicator)) %>%  # rename indicators 
  mutate(flag_remove = case_when(sub_indicator == 'No' ~ 1,
                                 T~0)) %>% 
  filter(flag_remove == 0) %>% 
  select(-c(flag_remove)) %>% 
  mutate(sub_indicator = case_when(indicator == 'Any condition indicating asm' ~ 'Any condition\u2074',
                                   indicator == 'Maternal epilepsy' ~ 'Maternal epilepsy\u2074',
                                   indicator == 'Maternal mental health conditions' ~ 'Maternal mental health conditions\u2074',
                                   indicator == 'Maternal migraine or pain conditions' ~ 'Maternal migraine or pain conditions\u2074',
                                   T~sub_indicator)) %>%
  mutate(indicator = case_when(indicator %in% c("Any condition indicating asm", "Maternal epilepsy",
                                                "Maternal mental health conditions", "Maternal migraine or pain conditions") ~ 'Condition indicating ASM use\u00B3',
                                                T~indicator)) %>% 
  mutate(percent_exposed = case_when(indicator == 'Total' ~ NA,
                                     indicator == 'N available for analysis' ~ NA,
                                     T~ percent_exposed),
         percent_unexposed = case_when(indicator == 'Total' ~ NA,
                                       indicator == 'N available for analysis' ~ NA,
                                       T~ percent_unexposed),
         percent_unexposed_matched = case_when(indicator == 'N available for analysis' ~ NA,
                                       T~ percent_unexposed_matched))


t_1_table <- t_1_table %>% 
  mutate(sub_indicator = case_when(sub_indicator == 'Yes' ~ ' ',
                                   sub_indicator == '40+' ~ '≥40',
                                   sub_indicator == '1 (most deprived)' ~ 'SIMD Q1 (most deprived)',
                                   sub_indicator == '2' ~ 'SIMD Q2',
                                   sub_indicator == '3' ~ 'SIMD Q3',
                                   sub_indicator == '4' ~ 'SIMD Q4',
                                   sub_indicator == '5 (least deprived)' ~ 'SIMD Q5 (least deprived)',
                                   sub_indicator == 'F' ~ 'Female',
                                   sub_indicator == 'M' ~ 'Male',
                                   sub_indicator == 'Healthy weight' ~ 'Healthy weight (18.5-<25)',
                                   sub_indicator == 'Obese' ~ 'Obese (≥30)',
                                   sub_indicator == 'Overweight' ~ 'Overweight (25-<30)',
                                   sub_indicator == 'Underweight' ~ 'Underweight (<18.5)',
                                   sub_indicator == '1+' ~ '≥1',
                                   sub_indicator == 'Ex-smoker' ~ 'Former smoker',
                                   sub_indicator == 'Non-smoker' ~ 'Never smoked',
                                   sub_indicator == 'Smoker' ~ 'Current smoker',
                                   T~sub_indicator
                                   ))

# Re-order m age group
group1 <- t_1_table %>% 
  slice(c(1:3))
asm_condition <- t_1_table %>% 
  slice(c(4:7))
m_age <- t_1_table %>% 
  slice(c(8:14))
group2 <- t_1_table %>% 
  slice(c(15:20))
baby_sex <- t_1_table %>% 
  slice(21:23)
bmi <- t_1_table %>% 
  slice(24:28)
m_smoke <- t_1_table %>% 
  slice(29:32)
group3 <- t_1_table %>% 
  slice(33:38)

asm_condition <- asm_condition %>% 
  slice(c(4, 1, 2, 3))
m_age <- m_age %>% 
  slice(c(6, 1, 2, 3, 4, 5, 7))
baby_sex <- baby_sex %>% 
  slice(c(2, 1, 3))
bmi <- bmi %>% 
  slice(c(4, 1, 3, 2, 5))
m_smoke <- m_smoke %>% 
  slice(c(3, 1, 2, 4))

t_1_table <- bind_rows(group1, asm_condition, m_age, group2, baby_sex, bmi, m_smoke, group3)

# Save file out for loading in markdown document
write_csv(t_1_table, paste0(folder_data_path,"markdown_results/t_1.csv") )


# M2. Fig. 2 Distribution of exposures by year dispensed ----------------------

# Read in data
# Data file created in /data_linkage/Create_main_analysis_file.r
df_fig2 <- readRDS(paste0(folder_data_path,"linkage/master_dataset_file.rds"))%>%
  mutate(control_pool_dev = case_when(exposed_any_asm==0 & est_date_conception <= as.Date("2020-07-01") &
                                        pregnancy_loss=="No"  & valid_chsp_review =="valid review" ~1, T~0),
         cases_dev = case_when(exposed_any_asm==1 & est_date_conception <=  as.Date("2020-07-01") &
                                 pregnancy_loss=="No" & valid_chsp_review =="valid review" ~1, T~0)) 


exposed <- df_fig2 %>% filter(exposed_any_asm==1)

exposed %>% 
  group_by(pregnancy_loss) %>% 
  summarise(n())
# Need to remove the unknowns and maternal death

exposed <- exposed %>% 
  filter(pregnancy_loss == 'Yes' | pregnancy_loss == 'No')


table(year(exposed$first_exposed_any_asm))

exposed %>% 
  group_by(year_conception) %>% 
  summarise(n())

exposed <- exposed %>% mutate(year_of_exposure = year(first_exposed_any_asm))

exposed<- exposed %>%
  mutate(asm_group = case_when(exposed_carbamazepine_mono ==1 ~ "CAR-m",
                               exposed_carbamazepine_poly==1 ~ "CAR-p",
                               exposed_lamotrigine_mono==1 ~"LAM-m", 
                               exposed_lamotrigine_poly==1 ~"LAM-p", 
                               exposed_levetiracetam_mono==1~ "LEV-m", 
                               exposed_levetiracetam_poly==1~ "LEV-p",
                               exposed_topiramate_mono==1~ "TOP-m", 
                               exposed_topiramate_poly==1~ "TOP-p", 
                               exposed_valproate_mono == 1~"VAL-m", 
                               exposed_valproate_poly == 1~"VAL-p", 
                               exposed_pregabalin_mono == 1~"PRE-m", 
                               exposed_pregabalin_mono == 1~"PRE-p", 
                               exposed_gabapentin_mono == 1~"GAB-m", 
                               exposed_gabapentin_poly == 1~"GAB-p", 
                               exposed_any_asm==1 ~"OTHER")) %>% 
  mutate(asm_group2 = case_when(exposed_carbamazepine_mono ==1 ~ "Carbamazepine",
                                exposed_lamotrigine_mono==1 ~"Lamotrigine", 
                                exposed_levetiracetam_mono==1~ "Levetiracetam", 
                                exposed_topiramate_mono==1~ "Topiramate", 
                                exposed_valproate_mono == 1~"Valproate", 
                                exposed_pregabalin_mono== 1 ~ "Pregabalin",
                                exposed_gabapentin_mono ==1 ~ "Gabapentin",
                                exposed_any_asm==1 ~"OTHER"))


mono_exposed <-exposed %>% filter(asm_group2 !="OTHER")
exposed_any <- exposed %>% mutate(asm_group2 ="Any ASM")

all_exposed <- rbind(exposed_any, mono_exposed)
all_exposed <- all_exposed %>% 
  mutate(asm_group2 = forcats::fct_relevel(asm_group2,"Any ASM", "Valproate","Topiramate",
                                           "Carbamazepine",  "Lamotrigine","Levetiracetam",
                                           "Gabapentin",  "Pregabalin"))


fig2 <- ggplot(all_exposed  , aes(x=year_conception)) +
  geom_bar() +
  facet_wrap(vars(asm_group2))+
  xlab("Year of conception") + 
  ylab("N pregnancies") +
  theme_bw()
fig2 +theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), 
                               "cm")) 

ggsave(paste0(folder_data_path,"markdown_results/fig_2.png"), width = 10, height = 7)




# M3. Table 2 - outcomes by cohort --------------------------------------------

# Read in data
# file created in /stat_analyses/01-extract_events.R
t_2 <- read_csv(paste0(folder_data_path,"stats/n_exp_unexp.csv"))

# Prepare df for re-shaping
t_2_table <- t_2 %>% 
  filter(group == 'main') %>% # keep to the main analyses cohort
  mutate(exposure = str_extract(indicator, "Exposed")) %>% 
  mutate(exposure = case_when(is.na(exposure) ~ 'Unexposed',
                              T~exposure)) %>% # flag exposed/unexposed cohort
  select(c(ASM, cohort, exposure, total, n_outcome, percent_outcome))

# Re-shape df into required format for markdown table
t_2_table <- t_2_table %>% 
  pivot_wider(names_from = exposure,
              names_glue = "{exposure}_{.value}",
              values_from = c(total, n_outcome, percent_outcome)) %>% # have separate n and % variables for exposed/unexposed groups
  select(c(ASM, cohort, Exposed_total, Exposed_n_outcome, Exposed_percent_outcome, Unexposed_total, Unexposed_n_outcome, Unexposed_percent_outcome)) %>% 
  mutate(cohort = case_when(cohort == 'Pregnancy loss' ~ 'Pregnancy loss\u00B9',
                            cohort == 'Congenital conditions' ~ 'Congenital conditions\u00B2',
                            cohort == 'Early childhood developmental concerns' ~ 'Early childhood developmental concerns\u00B3',
                            T~cohort)) %>% # add subscript 1 to Any ASM and cohorts for footnotes
  group_by(cohort) %>% 
  arrange(match(ASM, c('Any ASM', 'Valproate', 'Topiramate', 'Carbamazepine', 'Lamotrigine', 'Levetiracetam', 'Gabapentin', 'Pregabalin'))) %>% 
  ungroup()   # maintain correct order

# Save file out for loading in markdown document
write_csv(t_2_table, paste0(folder_data_path,"markdown_results/t_2.csv"))






# T2 - 7 main model results
# Read in number of exposed / unexposed
stats_n <- read_csv(paste0(folder_data_path,"stats/n_exp_unexp.csv"))

# Read in main model results (adjusted and unadjusted)
p_main_unadj <- read_csv(paste0(folder_data_path,"stats/p_full_unadjusted.csv"))
p_main_adj <- read_csv(paste0(folder_data_path,"stats/p_full_adjusted.csv"))
cc_main_unadj <- read_csv(paste0(folder_data_path,"stats/cc_full_unadjusted.csv"))
cc_main_adj <- read_csv(paste0(folder_data_path,"data/stats/cc_full_adjusted.csv"))
dev_main_unadj <- read_csv(paste0(folder_data_path,"stats/dev_full_unadjusted.csv"))
dev_main_adj <- read_csv(paste0(folder_data_path,"stats/dev_full_adjusted.csv"))

# Add outcome and model flags
p_main_unadj <- p_main_unadj %>% 
  mutate(outcome = 'Pregnancy loss',
         model = 'unadjusted')
p_main_adj <- p_main_adj %>% 
  mutate(outcome = 'Pregnancy loss',
         model = 'adjusted') 

cc_main_unadj <- cc_main_unadj %>% 
  mutate(outcome = 'Congenital conditions',
         model = 'unadjusted')
cc_main_adj <- cc_main_adj %>% 
  mutate(outcome = 'Congenital conditions',
         model = 'adjusted',
         p_value = as.character(p_value))  

dev_main_unadj <- dev_main_unadj %>% 
  mutate(outcome = 'Early childhood developmental concerns',
         model = 'unadjusted')
dev_main_adj <- dev_main_adj %>% 
  mutate(outcome = 'Early childhood developmental concerns',
         model = 'adjusted')  

# Combine main model results into one dataframe
main_models <- bind_rows(p_main_unadj, p_main_adj, cc_main_unadj, cc_main_adj, dev_main_adj, dev_main_unadj)

#rm(p_main_unadj, p_main_adj, cc_main_unadj, cc_main_adj, dev_main_adj, dev_main_unadj)

# Prepare the main models results into table
main_models_table <- main_models %>% 
  pivot_wider(
    names_from = model,
    names_glue = "{model}_{.value}",
    values_from = c(or, ci, p_value)
  ) %>% 
  mutate(ASM = case_when((characteristic == 'exposed_any_asm' | characteristic == 'CC_exposed_any_asm') ~ 'Any ASM',
                         (characteristic == 'exposed_valproate_mono' | characteristic == 'CC_exposed_valproate_mono') ~ 'Valproate',
                         (characteristic == 'exposed_topiramate_mono' | characteristic == 'CC_exposed_topiramate_mono') ~ 'Topiramate',
                         (characteristic == 'exposed_carbamazepine_mono' | characteristic == 'CC_exposed_carbamazepine_mono') ~ 'Carbamazepine',
                         (characteristic == 'exposed_lamotrigine_mono' | characteristic == 'CC_exposed_lamotrigine_mono') ~ 'Lamotrigine',
                         (characteristic == 'exposed_levetiracetam_mono' | characteristic == 'CC_exposed_levetiracetam_mono') ~ 'Levetiracetam',
                         (characteristic == 'exposed_gabapentin_mono' | characteristic == 'CC_exposed_gabapentin_mono') ~ 'Gabapentin',
                         (characteristic == 'exposed_pregabalin_mono' | characteristic == 'CC_exposed_pregabalin_mono') ~ 'Pregabalin',
                         T~NA)) %>% 
  select(c(ASM, outcome, unadjusted_or, unadjusted_ci, unadjusted_p_value, adjusted_or, adjusted_ci, adjusted_p_value)) %>% 
  group_by(outcome) %>% 
  arrange(match(ASM, c('Any ASM', 'Valproate', 'Topiramate', 'Carbamazepine', 
                       'Lamotrigine', 'Levetiracetam', 'Gabapentin', 'Pregabalin'))) %>% 
  ungroup()

# Prepare number of exposed / unexposed dataframe for matching to main model results
main_events <- stats_n %>% 
  mutate(exposure = str_extract(indicator, "Exposed")) %>% 
  mutate(exposure = case_when(is.na(exposure) ~ 'Unexposed',
                              T~exposure)) %>% 
  filter(group == 'main') %>% 
  select(-c(indicator, group)) %>% 
  rename(outcome = cohort) %>% 
  pivot_wider(
    names_from = exposure,
    names_glue = "{exposure}_{.value}",
    values_from = c(n_no_outcome, n_outcome, percent_no_outcome, percent_outcome, total)
  )

# Combine number of exposed / unexposed to main model results
main_models_table <- left_join(main_models_table, main_events, by = c("outcome", "ASM")) %>% 
  select(c(ASM, outcome, 
           Exposed_total, Exposed_n_outcome, Exposed_percent_outcome, 
           Unexposed_total, Unexposed_n_outcome, Unexposed_percent_outcome,
           unadjusted_or, unadjusted_ci, unadjusted_p_value, 
           adjusted_or, adjusted_ci, adjusted_p_value
  ))

write_csv(main_models_table,paste0(folder_data_path,"markdown_results/t_2_7.csv"))
