###01d.sliccd_flags.r##
# Script to link congential conditions from SLiCCD 
# to pregnancy dataset and flag indicators



#### 1. Housekeeping ####

# Approximate run time - TBD

rm(list = ls())
gc()

# install.packages("dplyr")
# install.packages("tidyverse")
# install.packages("haven")
# install.packages("labelled")
# install.packages("lubridate")
# install.packages("gdata", repos = c("https://ppm.publichealthscotland.org/phs-cran/latest"))
# install.packages("phsmethods")

library(dplyr)
library(tidyverse)
library(haven)
library(labelled)
library(lubridate)
library(janitor)
library(odbc)
library(phsmethods)
library(readxl)
library(arrow)
library(collapse)
source("data_linkage/00.setup_expose.r")

#### 2. Read in data and create triplicate ID ####
sliccd <- readRDS(paste0(data_path, 'sliccd_extract_for_MiP_v2.rds')) %>%
  mutate(triplicate_id = case_when((pregnancy_end_type == "Livebirth") ~ 
                                     paste0("B",as.character(substr(birth_year_of_registration,3,4)), 
                                            as.character(birth_registration_district), 
                                             as.character(str_pad(birth_entry_number, width = 4, pad="0"))), 
                                   (pregnancy_end_type == "Stillbirth") ~ 
                                     paste0("S", as.character(substr(stillbirth_year_of_registration,3,4)),
                                            as.character(stillbirth_registration_district), 
                                            as.character(str_pad(stillbirth_entry_number, width = 4, pad="0"))), T~NA))

# triplicate ID now added in new version of sliccd file. Check how this compares with one created above.
check <- sliccd %>% filter(triplicate_id != nrs_triplicate_id)
# all match but doesn't count for NAs

# check whereone is blank and the other isn't
check <- sliccd %>% filter((is.na(triplicate_id) &  !is.na(nrs_triplicate_id)) | 
                             (!is.na(triplicate_id) & is.na(nrs_triplicate_id)))

check2 <- check %>% group_by(nrs_triplicate_id, triplicate_id) %>% summarise(count = n()) %>% ungroup()
# looks like I can drop the triplicate ID created above. 

sliccd <- sliccd %>% select(-triplicate_id)


#### 3. have a look at data ####
sliccd %>% group_by(ALL_0_ALL_CONDITIONS) %>% summarise(n()) %>% ungroup()

sliccd %>% group_by(ALL_13_GENETIC_CONDITIONS) %>% summarise(n()) %>% ungroup()

# how many records with genetic condition = 1 also have another condition = 1 (removing multiples)
 sliccd %>% filter(ALL_13_GENETIC_CONDITIONS == 1 &
                    (      ALL_1_NERVOUS_SYSTEM == 1 |
                           ALL_2_EYE == 1 |
                           ALL_3_EAR_FACE_AND_NECK == 1 |
                           ALL_4_CONGENITAL_HEART_DEFECTS == 1 |
                           ALL_5_RESPIRATORY == 1 |
                           ALL_6_ORO_FACIAL_CLEFTS == 1 |
                           ALL_7_GASTRO_INTESTINAL == 1 |
                           ALL_8_ABDOMINAL_WALL_DEFECTS == 1 |
                           ALL_9_KIDNEY_AND_URINARY_TRACT == 1 |
                           ALL_10_GENITAL == 1 |
                           ALL_11_LIMB == 1 |
                           ALL_12_OTHER_CONDITIONS == 1)) %>% filter(multiple_pregnancy %in% c(0, 1)) %>%
   group_by(pregnancy_end_type) %>% summarise(count = n()) %>% ungroup() 

upi <- sliccd %>% group_by(cardriss_baby_upi) %>% summarise(count = n()) %>% ungroup() %>% 
  filter(count > 1)
# All records appear only once in v2

check <- sliccd %>% filter(cardriss_baby_upi %in% upi$cardriss_baby_upi)
# all good in v2

sliccd %>% group_by(multiple_pregnancy) %>% summarise(count = n()) %>% ungroup()

#### 4. Remove records where all_13_genetic_condition = 1 unless an other condition is also present  ####
# make lookup with EUROCAT groupings & remove multiple births
eurocat_group <- sliccd %>% filter(ALL_13_GENETIC_CONDITIONS == 0 | (ALL_13_GENETIC_CONDITIONS == 1 &
           (      ALL_1_NERVOUS_SYSTEM == 1 |
                    ALL_2_EYE == 1 |
                    ALL_3_EAR_FACE_AND_NECK == 1 |
                    ALL_4_CONGENITAL_HEART_DEFECTS == 1 |
                    ALL_5_RESPIRATORY == 1 |
                    ALL_6_ORO_FACIAL_CLEFTS == 1 |
                    ALL_7_GASTRO_INTESTINAL == 1 |
                    ALL_8_ABDOMINAL_WALL_DEFECTS == 1 |
                    ALL_9_KIDNEY_AND_URINARY_TRACT == 1 |
                    ALL_10_GENITAL == 1 |
                    ALL_11_LIMB == 1 |
                    ALL_12_OTHER_CONDITIONS == 1))) %>%
  select(sliccd_id,
         nrs_triplicate_id,
         cardriss_mother_upi,
         cardriss_baby_upi,
         date_end_of_pregnancy,
         pregnancy_end_type,
         gestation,
         birthweight,
         mother_dob,
         mother_age,
         mother_postcode,
         sex,
         multiple_pregnancy,
         birth_year_of_registration,
         birth_registration_district,
         birth_entry_number,
         stillbirth_year_of_registration,
         stillbirth_registration_district,
         stillbirth_entry_number,
         ALL_0_ALL_CONDITIONS,
         ALL_1_NERVOUS_SYSTEM,
         ALL_2_EYE,
         ALL_3_EAR_FACE_AND_NECK,
         ALL_4_CONGENITAL_HEART_DEFECTS,
         ALL_5_RESPIRATORY,
         ALL_6_ORO_FACIAL_CLEFTS,
         ALL_7_GASTRO_INTESTINAL,
         ALL_8_ABDOMINAL_WALL_DEFECTS,
         ALL_9_KIDNEY_AND_URINARY_TRACT,
         ALL_10_GENITAL,
         ALL_11_LIMB,
         ALL_12_OTHER_CONDITIONS)%>%
  filter(multiple_pregnancy %in% c(0, 1))

eurocat_group %>% group_by(cardriss_baby_upi) %>% summarise(count = n()) %>% 
  filter(count > 1) # no repeated records in v2


#### 5. Read in and join SLiPBD data  ####
slipbd <- readRDS(paste0(data_path, 'SLiPBD_cohort_extract.rds'))

#### 6a. Link live births & stillbirths with triplicate ID ####
#   records by triplicate id where available

sliccd_unmatched <- eurocat_group %>% filter(!is.na(nrs_triplicate_id))

slipbd_flagged <- left_join(slipbd, sliccd_unmatched, by = "nrs_triplicate_id", keep = TRUE)

# how many didn't match? 982 all in 2010
no_match <- sliccd_unmatched %>% filter(!(sliccd_id %in% slipbd_flagged$sliccd_id))

# check dates for non-matches 
no_match %>% summarise(min_date = min(date_end_of_pregnancy),
                       max_date = max(date_end_of_pregnancy))
# all in 2010 so expected not to match

# check if any extra would match on baby chi - none
check <- slipbd %>% filter(baby_chi %in% no_match$cardriss_baby_upi)

# save out not matching records 
# compare to previous 
no_match_old <- readRDS(paste0(data_path, "linkage/sliccd_triplicate_no_match.RDS")) # no difference

saveRDS(no_match, paste0(data_path, "linkage/sliccd_triplicate_no_match.RDS"))

sliccd_matched <- eurocat_group %>% filter(sliccd_id %in% slipbd_flagged$sliccd_id)

# create new unmatched for sliccd and slibpd

sliccd_unmatched <- eurocat_group %>% filter(!(sliccd_id %in% slipbd_flagged$sliccd_id))

slipbd_matched <- slipbd_flagged %>% filter(!(is.na(sliccd_id)))

slipbd_unmatched <- slipbd %>% filter(!(pregnancy_id %in% slipbd_matched$pregnancy_id))

#### 6b. Match remaining on baby CHI ####

slipbd_flagged <- left_join(slipbd_unmatched, sliccd_unmatched, 
                            by = c("baby_chi" = "cardriss_baby_upi"), keep = TRUE) %>% 
  filter(!(is.na(sliccd_id)))
# 0 additional matches 

# how many didn't match? 2942 
no_match <- sliccd_unmatched %>% filter(!(sliccd_id %in% slipbd_flagged$sliccd_id))


# Add additional matches to slipbd_matched

compare_df_cols(slipbd_flagged, slipbd_matched)

slipbd_matched <- rbind(slipbd_matched, slipbd_flagged)

# adjust sliccd matched 
sliccd_matched <- eurocat_group %>% filter(sliccd_id %in% slipbd_matched$sliccd_id)

# create new unmatched for sliccd and slibpd
sliccd_unmatched <- eurocat_group %>% filter(!(sliccd_id %in% slipbd_matched$sliccd_id))

slipbd_unmatched <- slipbd %>% filter(!(pregnancy_id %in% slipbd_matched$pregnancy_id))


#### 6c. Match on mother upi ####
# note: will need to have a check for pregnancy end dates and remove where this is too far apart
slipbd_flagged <- left_join(slipbd_unmatched, sliccd_unmatched, by = c("mother_upi" = "cardriss_mother_upi"), keep = TRUE) %>% 
  filter(!(is.na(sliccd_id)))
# many to many relationships here see if this can be reduced.
# check diff between end of pregnancy dates and remove any that are too far apart


slipbd_flagged <- slipbd_flagged %>% 
  mutate(diff_days = (date_end_pregnancy %--% date_end_of_pregnancy) / lubridate::period(1, "days"))

check <- slipbd_flagged %>% group_by(diff_days) %>% summarise(count = n()) %>% ungroup()

## removing any mismatches

duplicates <- slipbd_flagged %>% group_by(sliccd_id) %>% summarise(count = n()) %>% ungroup() %>%
  filter(count > 1)
# 1866 sliccd ids dupicated
# if we assume anything in 4 week period is a match then what does this do to the data? 

slipbd_flagged2 <- slipbd_flagged %>% filter(abs(diff_days)<29 )

duplicates <- slipbd_flagged2 %>% group_by(sliccd_id) %>% summarise(count = n()) %>% ungroup() %>%
  filter(count > 1)
# no duplicates remaining, 1,841 matches

# what if we used 6 weeks?
slipbd_flagged2 <- slipbd_flagged %>% filter(abs(diff_days)<43 )

duplicates <- slipbd_flagged2 %>% group_by(sliccd_id) %>% summarise(count = n()) %>% ungroup() %>%
  filter(count > 1)
# 0 duplicates remaining, 1,845 matches

### check these extra records 
check <- slipbd_flagged2 %>% filter(abs(diff_days) > 28)

### end check

# what if we used 8 weeks?
slipbd_flagged2 <- slipbd_flagged %>% filter(abs(diff_days)<57 )

duplicates <- slipbd_flagged2 %>% group_by(sliccd_id) %>% summarise(count = n()) %>% ungroup() %>%
  filter(count > 1)
# 0 duplicates remaining, 1,845 matches

# what if we used 12 weeks?
slipbd_flagged2 <- slipbd_flagged %>% filter(abs(diff_days)<85 )

duplicates <- slipbd_flagged2 %>% group_by(sliccd_id) %>% summarise(count = n()) %>% ungroup() %>%
  filter(count > 1)
# 1 duplicate remaining, 1,850 matches
rm(slipbd_flagged2)

# use 6 weeks for the moment and discuss with team
slipbd_flagged <- slipbd_flagged %>% filter(abs(diff_days)<43 )

## check accuracy of the match ##
# check where mother dob matches
check <- slipbd_flagged %>% mutate(dob_match = case_when(mother_dob.x == mother_dob.y ~ 1, T~0)) %>%
  filter(dob_match == 0) %>% relocate(mother_upi, cardriss_mother_upi, mother_dob.x, mother_dob.y)
# 3 cases of mother upi match but mother DOB not matching. This appears to be an issue in the sliccd data rather than with the matching.
# sliccd dob doesn't match the DoB according to CHI. CHIs appear to be genuine. 


# check where mother postcode matches
check <- slipbd_flagged %>% mutate(postcode_booking_match = case_when(str_replace_all(maternal_postcode_booking, fixed(" "), "") == str_replace_all(mother_postcode, fixed(" "), "") ~ 1, T~0),
                                   postcode_end_match = case_when(str_replace_all(maternal_postcode_end_preg, fixed(" "), "") == str_replace_all(mother_postcode, fixed(" "), "") ~ 1, T~0)) %>%
  filter(postcode_booking_match == 0 & postcode_end_match == 0) %>% relocate(mother_upi, cardriss_mother_upi, maternal_postcode_booking, maternal_postcode_end_preg, mother_postcode) 
# postcodes don't match in 84 cases - assume sliccd postcode is postcode at time when condition was recorded so some mismatch to be expected?
# check how well other fields match for these
  check %>% group_by(diff_days) %>% summarise(n()) %>% ungroup() 
# end of pregnancy dates within 4 days of each other in all cases
check %>% group_by(pregnancy_end_type, fetus_outcome1, fetus_outcome2) %>% summarise(n()) %>% ungroup()
# 77 cases match pregnancy end type.  

# check where mother age matches (or within a year)
check <- slipbd_flagged %>% mutate(age_match = case_when(maternal_age_conception == mother_age ~ 1, T~0),
                                   age_diff = maternal_age_conception - mother_age) %>%
  filter(age_match == 0 & abs(age_diff)>1) %>% relocate(mother_upi, cardriss_mother_upi, maternal_age_conception, mother_age, age_diff, mother_dob.x, mother_dob.y)
# all matched within a year

## end checks ##

# how many didn't match? 
no_match <- sliccd_unmatched %>% filter(!(sliccd_id %in% slipbd_flagged$sliccd_id))
# 1097 left to match

# Add additional matches to slipbd_matched

compare_df_cols(slipbd_flagged, slipbd_matched)
# flagged   # matched
slipbd_flagged <- slipbd_flagged %>% select(-diff_days)

slipbd_matched <- rbind(slipbd_matched, slipbd_flagged)

# adjust sliccd matched 
sliccd_matched <- eurocat_group %>% filter(sliccd_id %in% slipbd_matched$sliccd_id)

# create new unmatched for sliccd and slibpd
sliccd_unmatched <- eurocat_group %>% filter(!(sliccd_id %in% slipbd_matched$sliccd_id))

slipbd_unmatched <- slipbd %>% filter(!(pregnancy_id %in% slipbd_matched$pregnancy_id))

#### 6d. Match on mother_dob, preg end date, postcode (at preg end) & sex ####
# slipbd                      # sliccd
# mother_dob                  # mother_dob
# date_end_pregnancy          # date_end_of_pregnancy
# maternal_postcode_end_preg  # mother_postcode
# baby_sex                    # sex

# need to take all spaces out of postcodes for matching
# need to change baby_sex from M,F,NA to 1,2,3 for matching 
slipbd_unmatched <- slipbd_unmatched %>% mutate(match_pcode = str_replace_all(maternal_postcode_end_preg, fixed(" "), ""),
                                                match_sex = case_when(baby_sex == "M" ~ "1", 
                                                                      baby_sex == "F" ~ "2",
                                                                      T ~ "3"))

sliccd_unmatched <- sliccd_unmatched %>% mutate(match_pcode = str_replace_all(mother_postcode, fixed(" "), ""))

slipbd_flagged <- left_join(slipbd_unmatched, sliccd_unmatched, by = c("mother_dob" = "mother_dob",
                                                                       "date_end_pregnancy" = "date_end_of_pregnancy",
                                                                       "match_pcode" = "match_pcode",
                                                                       "match_sex" = "sex"), keep = TRUE) %>% 
  filter(!(is.na(sliccd_id)))

# 45 matches but could be some duplicates 
check <- slipbd_flagged %>% group_by(sliccd_id) %>% summarise(count = n()) %>% ungroup()
# 3 duplicated sliccd records - looks like potentially being caused by slibpd containing duplicates where one CHI is dummy and the est conception dates are 
# ever so slightly different.
check <- slipbd_flagged %>% group_by(pregnancy_id) %>% summarise(count = n()) %>% ungroup()
# 0 duplicated pregnancies

# how many didn't match? 
no_match <- sliccd_unmatched %>% filter(!(sliccd_id %in% slipbd_flagged$sliccd_id))
# 1055 left to match (1024 of these have pregnancy ending in 2010) 

temp <- no_match %>% filter(date_end_of_pregnancy > as_date("2010-12-31"))

# manually looking for the 31 non matches in slipb by mothers chi

# updated 
# 16 have other pregs but none that would align with the unmatched sliccd record
# 5 where preg end date in sliccd is in the middle of another pregnancy in slipbd
## note that end dates are months apart
# 8 have no records in slipbd
# 2 have dummy mother chi in sliccd

# Add additional matches to slipbd_matched

compare_df_cols(slipbd_flagged, slipbd_matched)
# flagged   # matched
slipbd_flagged <- slipbd_flagged %>% select(-match_pcode.x, -match_pcode.y, -match_sex)

slipbd_matched <- rbind(slipbd_matched, slipbd_flagged)

# adjust sliccd matched 
sliccd_matched <- eurocat_group %>% filter(sliccd_id %in% slipbd_matched$sliccd_id)

# create new unmatched for sliccd and slibpd
sliccd_unmatched <- eurocat_group %>% filter(!(sliccd_id %in% slipbd_matched$sliccd_id))

slipbd_unmatched <- slipbd %>% filter(!(pregnancy_id %in% slipbd_matched$pregnancy_id))

#### 7. Check for duplicated records & save files ####
check <- slipbd_matched %>% group_by(sliccd_id) %>% summarise(count = n()) %>% ungroup()
# 3 duplicated sliccd records 

check <- slipbd_matched %>% group_by(pregnancy_id) %>% summarise(count = n()) %>% ungroup()
# 3 duplicated pregnancies - possibly duplicated records in sliccd (at least the are very similar)


###### look into missing 3 sliccd stillbirths in full slipbd ###### 
# read in full slipbd database to check for records
slipbd_db <- readRDS(paste0(slipbd_path,"slipbd_database.rds"))

# select only stillbirths remaining
temp_still <- temp %>% filter(pregnancy_end_type == "Stillbirth")

upi <- slipbd_db %>% filter(maternal_postcode_booking %in% temp_still$mother_postcode | maternal_postcode_end_preg %in% temp_still$mother_postcode)

## join missing on
# date end preg
# outcome (?)
# postcode
# gestation

slipbd_temp <- left_join(slipbd_db, temp, by = c("date_end_pregnancy" = "date_end_of_pregnancy",
                                                           "fetus_outcome1" = "pregnancy_end_type",
                                                           "maternal_postcode_end_preg" = "mother_postcode",
                                                           "gest_end_pregnancy" = "gestation"), keep = TRUE) %>% 
  filter(!(is.na(sliccd_id)))
  
# manually chek the matches
slipbd_temp <- slipbd_temp %>% relocate(sliccd_id)
# 13331 - linked to preg ID that's already been matched to
# 12498 - links to unmatched preg ID in slipbd_unmatched
# 37589 - linked to preg ID that's already been matched to

# redo above linkage with slipbd_unmatched
slipbd_flagged <- left_join(slipbd_unmatched, sliccd_unmatched, by = c("date_end_pregnancy" = "date_end_of_pregnancy",
                                                                       "fetus_outcome1" = "pregnancy_end_type",
                                                                       "maternal_postcode_end_preg" = "mother_postcode",
                                                                       "gest_end_pregnancy" = "gestation"), keep = TRUE) %>% 
  filter(!(is.na(sliccd_id)))


# 1 new match, 2 remaining sliccd stillbirths assumed duplicates

slipbd_matched <- rbind(slipbd_matched, slipbd_flagged)

# adjust sliccd matched 
sliccd_matched <- eurocat_group %>% filter(sliccd_id %in% slipbd_matched$sliccd_id)

# create new unmatched for sliccd and slibpd
sliccd_unmatched <- eurocat_group %>% filter(!(sliccd_id %in% slipbd_matched$sliccd_id))

slipbd_unmatched <- slipbd %>% filter(!(pregnancy_id %in% slipbd_matched$pregnancy_id))

# join slipbd matched and unmatched back up and tidy up the columns
compare_df_cols(slipbd_matched, slipbd_unmatched)

full_slipbd <- bind_rows(slipbd_matched, slipbd_unmatched)

# Save unmatched records
# how many didn't match? 
no_match <- sliccd_unmatched %>% filter(!(sliccd_id %in% slipbd_flagged$sliccd_id))
# 1054 left unmatched (1024 of these have pregnancy ending in 2010) 

temp <- no_match %>% filter(date_end_of_pregnancy > as_date("2010-12-31"))

saveRDS(temp, paste0(data_path, "linkage/sliccd_unmatched_after2010.RDS"))

check <- full_slipbd %>% unique()

## Check for duplicated records & save files ####
check2 <- full_slipbd %>% group_by(sliccd_id) %>% summarise(count = n()) %>% ungroup()
# 3 duplicated sliccd records 

check2 <- full_slipbd %>% group_by(pregnancy_id) %>% summarise(count = n()) %>% ungroup()
# 3 duplicated pregnancies - possibly duplicated records in sliccd (at least the are very similar)

########

saveRDS(full_slipbd, paste0(data_path,"linkage/slipbd_sliccd_joins.RDS"))



sliccd_unmatched_pre2010 <- no_match %>% filter(date_end_of_pregnancy <= as_date("2010-12-31"))
saveRDS(sliccd_unmatched_pre2010, paste0(data_path,"linkage/sliccd_unmatched_pre2010.RDS"))

#### Check the pre2010 unmatched records ####

# create rough/estimated date of conception 

sliccd_unmatched_pre2010 <- sliccd_unmatched_pre2010 %>% mutate(est_conception_date = (date_end_of_pregnancy-days((gestation-2)*7)))

# check records with est conception date after April 2010
conceptions_early2010 <- sliccd_unmatched_pre2010 %>% filter(est_conception_date > as_date("2010-03-31"))
# 3 records 

# none of above have triplicate IDs or valid baby UPI so try with mother UPI first (2 of 3 have valid mother UPI)
early2010_flagged <- left_join(conceptions_early2010, slipbd_db, by = c("cardriss_mother_upi" = "mother_upi"), keep = TRUE) %>% 
  filter(!(is.na(pregnancy_id)))
# many to many relationships here see if this can be reduced.
# check diff between end of pregnancy dates and remove any that are too far apart

early2010_flagged <- early2010_flagged %>%  mutate(diff_days = (date_end_pregnancy %--% date_end_of_pregnancy) / lubridate::period(1, "days"))

# remove matches with end dates over 6 weeks apart
early2010_flagged <- early2010_flagged %>% filter(abs(diff_days)<43 )
# no additional matches

# Match on mother_dob, preg end date, postcode (at preg end) & sex #

# need to take all spaces out of postcodes for matching
# need to change baby_sex from M,F,NA to 1,2,3 for matching 
slipbd_db <- slipbd_db %>% mutate(match_pcode = str_replace_all(maternal_postcode_end_preg, fixed(" "), ""),
                                                match_sex = case_when(baby_sex == "M" ~ "1", 
                                                                      baby_sex == "F" ~ "2",
                                                                      T ~ "3"))

conceptions_early2010 <- conceptions_early2010 %>% mutate(match_pcode = str_replace_all(mother_postcode, fixed(" "), ""))

early2010_flagged <- left_join(conceptions_early2010, slipbd_db, by = c("mother_dob" = "mother_dob",
                                                                        "date_end_of_pregnancy" = "date_end_pregnancy",
                                                                       "match_pcode" = "match_pcode",
                                                                       "sex" = "match_sex"), keep = TRUE) %>% 
  filter(!(is.na(pregnancy_id)))
# no additional matches


#### 8. Tidy up the data ####

# remove unneeded columns - 
full_slipbd_final <- full_slipbd %>% select(pregnancy_id,
                                      mother_upi,
                                      baby_chi,
                                      sliccd_id,
                                      ALL_0_ALL_CONDITIONS,
                                      ALL_1_NERVOUS_SYSTEM,
                                      ALL_2_EYE,
                                      ALL_3_EAR_FACE_AND_NECK,
                                      ALL_4_CONGENITAL_HEART_DEFECTS,
                                      ALL_5_RESPIRATORY,
                                      ALL_6_ORO_FACIAL_CLEFTS,
                                      ALL_7_GASTRO_INTESTINAL,
                                      ALL_8_ABDOMINAL_WALL_DEFECTS,
                                      ALL_9_KIDNEY_AND_URINARY_TRACT,
                                      ALL_10_GENITAL,
                                      ALL_11_LIMB,
                                      ALL_12_OTHER_CONDITIONS
                                      ) %>%
  mutate(ALL_0_ALL_CONDITIONS = case_when(is.na(ALL_0_ALL_CONDITIONS) ~ 0, T ~ ALL_0_ALL_CONDITIONS),
         ALL_1_NERVOUS_SYSTEM = case_when(is.na(ALL_1_NERVOUS_SYSTEM) ~ 0, T ~ ALL_1_NERVOUS_SYSTEM),
         ALL_2_EYE = case_when(is.na(ALL_2_EYE) ~ 0, T ~ ALL_2_EYE),
         ALL_3_EAR_FACE_AND_NECK = case_when(is.na(ALL_3_EAR_FACE_AND_NECK) ~ 0, T ~ ALL_3_EAR_FACE_AND_NECK),
         ALL_4_CONGENITAL_HEART_DEFECTS = case_when(is.na(ALL_4_CONGENITAL_HEART_DEFECTS) ~ 0, T ~ ALL_4_CONGENITAL_HEART_DEFECTS),
         ALL_5_RESPIRATORY = case_when(is.na(ALL_5_RESPIRATORY) ~ 0, T ~ ALL_5_RESPIRATORY),
         ALL_6_ORO_FACIAL_CLEFTS = case_when(is.na(ALL_6_ORO_FACIAL_CLEFTS) ~ 0, T ~ ALL_6_ORO_FACIAL_CLEFTS),
         ALL_7_GASTRO_INTESTINAL = case_when(is.na(ALL_7_GASTRO_INTESTINAL) ~ 0, T ~ ALL_7_GASTRO_INTESTINAL),
         ALL_8_ABDOMINAL_WALL_DEFECTS = case_when(is.na(ALL_8_ABDOMINAL_WALL_DEFECTS) ~ 0, T ~ ALL_8_ABDOMINAL_WALL_DEFECTS),
         ALL_9_KIDNEY_AND_URINARY_TRACT = case_when(is.na(ALL_9_KIDNEY_AND_URINARY_TRACT) ~ 0, T ~ ALL_9_KIDNEY_AND_URINARY_TRACT),
         ALL_10_GENITAL = case_when(is.na(ALL_10_GENITAL) ~ 0, T ~ ALL_10_GENITAL),
         ALL_11_LIMB = case_when(is.na(ALL_11_LIMB) ~ 0, T ~ ALL_11_LIMB),
         ALL_12_OTHER_CONDITIONS = case_when(is.na(ALL_12_OTHER_CONDITIONS) ~ 0, T ~ ALL_12_OTHER_CONDITIONS)
         ) 


saveRDS(full_slipbd_final, paste0(data_path,"linkage/slipbd_sliccd_final.RDS"))






