##00.UPI_list_pop_study.r
###UPIs for pregnancy outcomes population study
library(dplyr)
source("01-data_extraction/000.file_paths.r")
###latest SLiPBD ###

slipbd_database <- readRDS(paste0(slipbd_data_path, "archive/slipbd_database_old_2024_10.rds"))

###TIme period - conceptions between 2010 and  2023 (conceptions 2023 - will use outcomes up to MArch 2024)
upi_list_old <- slipbd_database_old %>% 
  filter(est_date_conception >= as.Date("2010-04-01") & est_date_conception <= as.Date("2023-07-02")) 
upi_list <- slipbd_database %>% 
  filter(est_date_conception >= as.Date("2010-04-01") & est_date_conception <= as.Date("2023-07-02")) 

##compare old and new to see which chi need updating
new_chi <- upi_list %>% filter(!mother_upi %in% upi_list_old$mother_upi) %>% select(mother_upi) %>%
  mutate(valid_chi = phsmethods::chi_check(mother_upi)) %>%
  filter(valid_chi == "Valid CHI")

#save new chi for PIS extracts####

saveRDS(new_chi, paste0(folder_data_path, "SLiPBD_upi_list_new.rds"))
##adjust en date for emigration
# slice to take one record for multiples
# dont need multiples but want to document the number that are dropped
cohort <- upi_list %>% select(pregnancy_id, mother_upi,mother_dob,  est_date_conception,
                              nrs_triplicate_id, baby_chi,
                              date_end_pregnancy, date_maternal_death, 
                              date_maternal_emigration, fetus_outcome1, fetus_outcome2,
                              total_fetuses_this_pregnancy, 
                              maternal_age_conception, maternal_age_conception, maternal_bmi,
                              maternal_postcode_booking,  maternal_postcode_end_preg, 
                              maternal_SIMD_booking,maternal_SIMD_end_preg, 
                              gest_end_pregnancy,gestation_ascertainment,
                              maternal_bmi, baby_sex, maternal_smoking, 
                              maternal_nhs_board_res_name_end_preg, maternal_nhs_board_res_booking, 
                              infant_death, infant_deaths_triplicate_id, date_infant_death,
                              selective_reduction_flag, 
                              abortion_of_fetus_died_in_utero, 
                              n_prev_pregnancies, n_prev_deliveries)%>%
  group_by(pregnancy_id) %>% slice(1) %>% #take 1st row of multiple pregs, keep multiples for the descriptive stages
  mutate(date_end_pregnancy = case_when(!is.na(date_maternal_emigration) ~date_maternal_emigration,
                                        T~date_end_pregnancy)) %>% ungroup() %>% select(-date_maternal_emigration)

saveRDS(cohort,  paste0(folder_data_path,"SLiPBD_cohort_extract.rds"))

upi_list <- upi_list %>%
  select(mother_upi) %>% unique()

upi_list <- upi_list %>% mutate(valid_chi = phsmethods::chi_check(mother_upi)) %>% filter(valid_chi=="Valid CHI")


saveRDS(upi_list, paste0(folder_data_path,"SLiPBD_upi_list.rds"))


upi_list2 <- slipbd_database %>% 
  filter(est_date_conception >= as.Date("2023-07-01") & est_date_conception <= as.Date("2023-07-02"))


upi_list2 <- upi_list2 %>% mutate(valid_chi = phsmethods::chi_check(mother_upi)) %>% filter(valid_chi=="Valid CHI")
upi_list3 <- slipbd_database %>% 
  filter(est_date_conception >= as.Date("2010-04-01") & est_date_conception <= as.Date("2023-06-30")) %>%
  select(mother_upi) %>% unique()

upi_list2 <- upi_list2 %>% filter(! mother_upi %in% upi_list3$mother_upi) %>%
  select(mother_upi) %>% unique()


saveRDS(upi_list2, paste0(folder_data_path,"SLiPBD_upi_list_additions.rds"))
