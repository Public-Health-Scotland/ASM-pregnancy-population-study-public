##02-create_main_analysis_file.r
# script to combine all files created in 01a - 01f to create main analysis file for 
# project

###################################
###Creating main analysis file####
##################################

#Setup####
#libraries
library(dplyr)
library(janitor)
library(lubridate)
library(tidyr)
library(hablar)

source("data_linkage/00.setup_expose.r")
#filepaths

#values
unknowns <- c("Unknown","Unknown - emigrated")
losses <- c( "Ectopic pregnancy", "Miscarriage", "Molar pregnancy", 
             "Mulitple pregnancy fetal loss", "Stillbirth", "Termination",
             "Unknown - assumed early loss")


##Load data files####
SLiPBD_cohort_extract <- readRDS(paste0(data_path, "SLiPBD_cohort_extract.rds"))
exposure_flags <- readRDS(paste0(data_path, "linkage/exposure_flags.rds"))
chsp <- readRDS(paste0(data_path, "linkage/chsp_flags.rds"))
folate <- readRDS(paste0(data_path, "linkage/folate_flags.rds"))
indication_flags <- readRDS(paste0(data_path, "linkage/indication_flags.rds"))
sliccd_outcomes <- readRDS(paste0(data_path, "linkage/slipbd_sliccd_final.RDS"))
new_smoking <- readRDS( paste0(data_path, "linkage/new_smoking_flag.rds"))
drug_alcohol <- readRDS(paste0(data_path, "linkage/drug_alcohol_flag.rds"))
smr_comorbs <-readRDS(paste0(data_path,"linkage/smr_comorbidities_flag.rds"))
# VS - added selection for limb and other conditions
sliccd_outcomes <- sliccd_outcomes %>% 
  select(pregnancy_id, ALL_0_ALL_CONDITIONS, ALL_1_NERVOUS_SYSTEM, ALL_2_EYE, 
         ALL_3_EAR_FACE_AND_NECK, ALL_4_CONGENITAL_HEART_DEFECTS, ALL_5_RESPIRATORY, ALL_6_ORO_FACIAL_CLEFTS, 
         ALL_7_GASTRO_INTESTINAL, ALL_8_ABDOMINAL_WALL_DEFECTS, ALL_9_KIDNEY_AND_URINARY_TRACT, ALL_10_GENITAL,
         ALL_11_LIMB, ALL_12_OTHER_CONDITIONS) %>%
  clean_names()

##remove multiples
multi_pregs <- SLiPBD_cohort_extract %>% filter(total_fetuses_this_pregnancy>1)
##number of multiple pregs excluded
nrow(multi_pregs)

SLiPBD_cohort_extract <- SLiPBD_cohort_extract %>% filter(total_fetuses_this_pregnancy==1)
##Link all flags, outcomes####
main_file <-left_join(SLiPBD_cohort_extract, exposure_flags)
main_file <-left_join(main_file, indication_flags)
main_file <-left_join(main_file, folate)
main_file <-left_join(main_file, sliccd_outcomes)
main_file <-left_join(main_file, chsp)
main_file <-left_join(main_file, new_smoking)
main_file <-left_join(main_file, drug_alcohol)
main_file <-left_join(main_file, smr_comorbs)
names(main_file)
#table(main_file$maternal_smoking, main_file$x_booking_smoking_status, useNA="always")
##Derive variables####

#derive from slipbd data
main_file <- main_file %>% 
  #recode unknown simd to NA
  mutate(maternal_SIMD_booking = case_when(maternal_SIMD_booking=="Unknown" ~NA,
                                           T ~ maternal_SIMD_booking),
         maternal_SIMD_end_preg = case_when(maternal_SIMD_end_preg =="Unknown" ~NA,
                                            T~ maternal_SIMD_end_preg)) %>%
  #grouping variables
  mutate(year_conception = year(est_date_conception),
         year_end_pregnancy = year(date_end_pregnancy), 
         mother_simd = coalesce(maternal_SIMD_booking, maternal_SIMD_end_preg),
         maternal_age_group_conception =
           case_when(maternal_age_conception <20 ~"<20", 
                     maternal_age_conception >=20 & maternal_age_conception <=24~ "20-24", 
                     maternal_age_conception >=25 & maternal_age_conception <=29~ "25-29", 
                     maternal_age_conception >=30 & maternal_age_conception <=34~ "30-34", 
                     maternal_age_conception >=35 & maternal_age_conception <=39~ "35-39", 
                     maternal_age_conception >=40~ "40+", T~"Unknown"), 
         maternal_bmi_group = case_when(maternal_bmi < 18.5 ~"Underweight", 
                                        maternal_bmi >= 18.5 & maternal_bmi < 25 ~"Healthy weight", 
                                        maternal_bmi >= 25 & maternal_bmi < 30 ~"Overweight",
                                        maternal_bmi >= 30 ~"Obese", T~ "Unknown" ), 
         pregnancy_loss = case_when(fetus_outcome1 %in% losses ~"Yes", 
                                    fetus_outcome1== "Live birth" ~"No",
                                    fetus_outcome1 %in% unknowns ~"Unknown", 
                                    fetus_outcome1 =="Maternal death" ~ "Maternal death")) %>%
  mutate(mother_simd =case_when(is.na(mother_simd) ~ "Unknown", T ~mother_simd)) %>% 
  select(-maternal_SIMD_booking, maternal_SIMD_end_preg, -fetus_outcome2,
         -selective_reduction_flag, abortion_of_fetus_died_in_utero) %>%
  #derive exposure year and exposure gestation
  #because we are only interested in monotherapy for the individual drugs we can
  #just compute one year and gest of first exposure, as for monotherapy thie will be
  # the date and gestation of exposure to that drug
  #, and for polytherapy we are dropping those anyway so dont really care
  mutate(year_exposed_any = year(first_exposed_any_asm), 
         gest_first_exposed = floor((first_exposed_any_asm - est_date_conception)/7) + 2) %>%
  mutate_at(vars(exposed_any_asm, exposed_carbamazepine_mono, exposed_carbamazepine_poly, exposed_lamotrigine_mono, 
                 exposed_lamotrigine_poly, exposed_levetiracetam_mono, exposed_levetiracetam_poly,          
                 exposed_topiramate_mono, exposed_topiramate_poly, exposed_valproate_mono, 
                 exposed_valproate_poly, 
                 exposed_pregabalin_mono, exposed_pregabalin_poly, 
                 exposed_gabapentin_mono, exposed_gabapentin_poly,
                 unexposed, CC_unexposed, 
                 CC_exposed_any_asm, CC_exposed_carbamazepine_mono, CC_exposed_carbamazepine_poly, 
                 CC_exposed_lamotrigine_mono, CC_exposed_lamotrigine_poly, 
                 CC_exposed_levetiracetam_mono, CC_exposed_levetiracetam_poly,          
                 CC_exposed_topiramate_mono, CC_exposed_topiramate_poly, CC_exposed_valproate_mono, 
                 CC_exposed_valproate_poly, CC_exposed_pregabalin_mono, CC_exposed_pregabalin_poly,
                 CC_exposed_gabapentin_mono, CC_exposed_gabapentin_poly), ~replace_na(., 0)) %>%
  ##folc
  rename(high_dose_folic_acid = folic_flag) %>%
  #CC - flag_any CC
  select(-all_0_all_conditions) %>%
  mutate_at(vars(all_1_nervous_system, all_2_eye, all_3_ear_face_and_neck, 
                 all_4_congenital_heart_defects, all_5_respiratory, all_6_oro_facial_clefts, 
                 all_7_gastro_intestinal, all_8_abdominal_wall_defects, 
                 all_9_kidney_and_urinary_tract, all_10_genital,
                 all_11_limb, all_12_other_conditions), ~replace_na(., 0)) %>%
  mutate(any_CC = case_when(
    all_1_nervous_system == 1 | all_2_eye == 1 | all_3_ear_face_and_neck == 1 | 
    all_4_congenital_heart_defects == 1 | all_5_respiratory == 1 | all_6_oro_facial_clefts == 1 | 
    all_7_gastro_intestinal == 1 | all_8_abdominal_wall_defects == 1 | 
    all_9_kidney_and_urinary_tract == 1 | all_10_genital == 1 |
    all_11_limb == 1 | all_12_other_conditions == 1 ~ 1,
    T~0))%>%
  # VS - added all_11_limb and all_12_other_conditions above 
  ##CHSP - label tools. drop vars not needed
  #age at review - we need to decide on limits to exclude those with invalid/unbelievable review dates vs birth dates
  # VS - advice from Susanne was to go for 23-38 months so edited below
  mutate(age_review_complete_months = interval( date_end_pregnancy, date_review_27m)%/% months(1) ) %>%
  mutate(valid_chsp_review = case_when( age_review_complete_months>=23 & 
                                          age_review_complete_months <= 38  ~ "valid review", 
                                        pregnancy_loss != "No" ~ NA, # NA for non-live outcomes
                                        T~ "no review or invalid date of review")) %>%  #this is a date check only, 
  #can have valid review with incomplete assessment for all items
  ##recode dev outcome to U for a review with no information (but valid dates)
  ##and recode to NA for a review outside of correct age range
  mutate(any_dev_excl_v_h = case_when(no_info==1 & valid_chsp_review=="valid review" ~ "U" ,
                                      valid_chsp_review=="no review or invalid date of review" ~ NA,
                                      T~any_dev_excl_v_h))%>%
  ##flag cohort membership and control eligibility
  ##ASM exposure already flagged and that determines membership of the pregnancy outcomes
  #cases cohort vs control pool (everyone not exposed)
  # Other cohorts also have date and other restrictions:
  # CC cohort - any pregnancy with estimated date of conception from 1 April 2010 to 2 April 2021 inclusive
  ##exposed cases have to be exposed before week 20 to count for the CC cohort.
  ##
  # 
  mutate(control_pool_CC = case_when(CC_exposed_any_asm==0 & est_date_conception <= as.Date("2021-04-02") ~1, T~0),
         cases_CC = case_when(CC_exposed_any_asm==1 & est_date_conception <= as.Date("2021-04-02") &
                                gest_first_exposed < 20 ~1, T~0),
         #Dev outcomes cohort. LIVE BIRTHS only with date of conception between April 2010 and 1 July 2020 
         #AND have a review (with valid date) and an informative outcome (Y or N)
         control_pool_dev = case_when(exposed_any_asm==0 & est_date_conception <= as.Date("2020-07-01") &
                                        pregnancy_loss=="No"  & valid_chsp_review =="valid review" & 
                                        any_dev_excl_v_h != "U" ~1, T~0),
         cases_dev = case_when(exposed_any_asm==1 & est_date_conception <=  as.Date("2020-07-01") &
                                 pregnancy_loss=="No" & valid_chsp_review =="valid review"  & 
                                 any_dev_excl_v_h != "U" ~1, T~0)) 



##infilling missing data:
# if any of the three covariates relating to the condition indicating ASM exposure 
#Maternal epilepsy; Maternal mental health conditions; Maternal migraine or pain conditions) 
#is Yes for any pregnancy to an individual woman, we will set that variable to Yes for all
#subsequent pregnancies to that woman. Similarly, if a woman is classified as a current or former 
#smoker in any pregnancy, then as a never smoker in a subsequent pregnancy, we will recode the 
#subsequent pregnancy to former smoker. Lastly, if a woman has deprivation status (SIMD quintile) 
#available for at least one, but not all, pregnancies, we will infill missing values using the SIMD
#quintile recorded for the most recent previous (or, if none, the earliest subsequent) pregnancy.


##infill smoking####
#select women with >1 pregnancy in cohort
selected_women <- main_file %>% group_by(mother_upi) %>% count() %>% filter(n>1)
#table(selected_women$n)

##create a lookup for the first pregnancies with smoker or non smoker
infill_lookup1 <- main_file %>% filter(mother_upi %in% selected_women$mother_upi) %>%
  arrange(mother_upi,est_date_conception) %>% 
  group_by(mother_upi) %>%
  mutate(preg_no = row_number()) %>%
  ungroup() 
infill_lookup <- infill_lookup1 %>% 
  select(pregnancy_id, mother_upi,est_date_conception, x_booking_smoking_status, preg_no) %>%
  filter(x_booking_smoking_status=="smoker" | x_booking_smoking_status=="ex-smoker") %>%
  select(mother_upi, preg_no, x_booking_smoking_status) %>% ungroup() %>% 
  group_by(mother_upi, x_booking_smoking_status) %>%
  summarise(infill_pregno = first_(preg_no))

lookup_smoker <- infill_lookup  %>%
  filter(x_booking_smoking_status=="smoker") %>% rename(smoking_lookup= x_booking_smoking_status)
lookup_exsmoker <- infill_lookup  %>%
  filter(x_booking_smoking_status=="ex-smoker") %>% rename(smoking_lookup= x_booking_smoking_status)

infill_smoke <- main_file %>% filter(mother_upi %in% selected_women$mother_upi) %>%
  arrange(mother_upi,est_date_conception) %>% 
  group_by(mother_upi) %>%
  mutate(preg_no = row_number()) %>%
  ungroup() %>%
  left_join(lookup_smoker) %>%
  mutate(maternal_smoking_revised = case_when(x_booking_smoking_status=="non-smoker" &
                                                infill_pregno < preg_no & smoking_lookup=="smoker" ~ "ex-smoker", 
                                              T~x_booking_smoking_status))%>%
  select(mother_upi, pregnancy_id, preg_no, x_booking_smoking_status, maternal_smoking_revised , everything()) %>%
  select(-smoking_lookup, -infill_pregno) %>%
  left_join(lookup_exsmoker) %>% 
  mutate(maternal_smoking_revised = case_when(x_booking_smoking_status=="non-smoker" &
                                                infill_pregno < preg_no & smoking_lookup=="ex-smoker" ~ "ex-smoker", 
                                              T~x_booking_smoking_status)) %>%
  select(pregnancy_id,preg_no,  maternal_smoking_revised )
names(infill_smoke)

main_file <- main_file %>% 
  arrange(mother_upi,est_date_conception) %>% 
  group_by(mother_upi) %>%
  mutate(preg_no = row_number()) %>%
  ungroup() %>%
  left_join(infill_smoke) %>%  ##joins on preg no and mother upi
  mutate(x_booking_smoking_status =
           case_when(!is.na(x_booking_smoking_status) & !is.na(maternal_smoking_revised) &
                       x_booking_smoking_status !=maternal_smoking_revised ~maternal_smoking_revised, 
                     T~x_booking_smoking_status))
table(main_file$x_booking_smoking_status)
table(main_file$x_booking_smoking_status, main_file$maternal_smoking_revised, useNA = "always")
###remove xtra variables and rename final smoking variable to maternal smoking
main_file <- main_file %>% mutate(x_booking_smoking_status =
                                    case_when(is.na(x_booking_smoking_status) ~ "unknown", 
                                              T~x_booking_smoking_status)) %>%
  select(-maternal_smoking_revised, -maternal_smoking) %>%
  rename(maternal_smoking = x_booking_smoking_status)

##infill indicators####
infill_lookup_indicators <- main_file %>% filter(mother_upi %in% selected_women$mother_upi) %>%
  arrange(mother_upi,est_date_conception) %>% 
  group_by(mother_upi) %>%
  mutate(preg_no = row_number()) %>%
  select(pregnancy_id, mother_upi,est_date_conception, mh_flag, epilepsy_indication, migraine_pain_flag, preg_no) %>%
  filter(migraine_pain_flag==1 | mh_flag==1 | epilepsy_indication==1 ) %>%
  ungroup() 

mh_ind <- infill_lookup_indicators %>% filter( mh_flag==1) %>%
  arrange(mother_upi)%>% 
  group_by(mother_upi, mh_flag) %>%
  summarise(fill_mh_pregno = min_(preg_no)) %>%
  rename(fillmh = mh_flag) %>%ungroup()

main_file <- main_file %>%
  left_join(mh_ind) %>%
  mutate(mh_flag = case_when(mh_flag==0 & fill_mh_pregno < preg_no ~ fillmh, T~mh_flag)) %>%
  select(-fillmh, -fill_mh_pregno)

ep_ind <- infill_lookup_indicators %>% filter(epilepsy_indication==1) %>%
  arrange(mother_upi)%>% 
  group_by(mother_upi, epilepsy_indication) %>%
  summarise(fill_ep_pregno = min_(preg_no)) %>%
  rename(fillep = epilepsy_indication) %>%ungroup()

main_file <- main_file %>%
  left_join(ep_ind) %>%
  mutate(epilepsy_indication = case_when(epilepsy_indication==0 & fill_ep_pregno < preg_no ~ fillep,
                                         T~epilepsy_indication)) %>%
  select(-fillep, -fill_ep_pregno)


pain_ind <- infill_lookup_indicators %>% filter(migraine_pain_flag==1) %>%
  arrange(mother_upi)%>% 
  group_by(mother_upi, migraine_pain_flag) %>%
  summarise(fill_mp_pregno = min_(preg_no)) %>%
  rename(fillmp = migraine_pain_flag) %>% ungroup()

main_file <- main_file %>%
  left_join(pain_ind) %>%
  mutate(migraine_pain_flag = case_when(migraine_pain_flag==0 & fill_mp_pregno < preg_no ~ fillmp,
                                        T~migraine_pain_flag)) %>%
  select(-fillmp, -fill_mp_pregno)

#table(main_file$age_review_complete_months)
#hist(main_file$age_review_complete_months, breaks=63)

###infill SIMD####
#If a woman has SIMD available for at least one but not all
#pregnancies then infill missing values using SIMD recorded
#for most recent previous (or if none the earliest subsequent)
#pregnancy

# remove women with only unknown simd for all pregnancies 
unknown_simd <- main_file %>% filter(mother_upi %in% selected_women$mother_upi) %>%
  group_by(mother_upi) %>% 
  mutate(same = +(n_distinct(mother_simd) == 1)) %>% # flag if simd is the same across all pregnancies
  ungroup() %>% 
  mutate(unknown_simd = case_when(same == 1 & mother_simd == 'Unknown' ~ 1,
                                  T~0)) %>% # flag where simd is unknown across all pregnancies
  filter(unknown_simd == 1) # keep only unknowns across all pregnancies

# create simd lookup for remaining women
infill_lookup_simd <- main_file %>% filter(mother_upi %in% selected_women$mother_upi) %>%
  filter(!(mother_upi %in% unknown_simd$mother_upi)) %>% 
  arrange(mother_upi,est_date_conception) %>% 
  group_by(mother_upi) %>%
  mutate(preg_no = row_number()) %>%
  select(pregnancy_id, mother_upi, est_date_conception, mother_simd, preg_no) %>% 
  ungroup()

infill_simd <- infill_lookup_simd %>% 
  mutate(new_simd = case_when(mother_simd == 'Unknown' ~ NA,
                              T~mother_simd)) %>% # make unknown NA for next function to work
  group_by(mother_upi) %>% 
  fill(new_simd, .direction = "downup") # fill the NA simd with previous SIMD in group unless NA then fill with subsequent 

main_file <- main_file %>%
  left_join(infill_simd) %>%
  mutate(mother_simd = case_when(mother_simd == 'Unknown'~ new_simd, 
                                 T~mother_simd)) %>% # fill SIMD variable with new simd for those which are unknown
  select(-new_simd)

main_file <- main_file %>% rename(no_info_27m = no_info, incomplete_info_27m = incomplete_info)


##save main file####
saveRDS(main_file, paste0(data_path, "linkage/master_dataset_file.rds"))
