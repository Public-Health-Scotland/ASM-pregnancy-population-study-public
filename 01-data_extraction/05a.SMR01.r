##05a.SMR01 extract
#run setup file
source("01-data_extraction/000.file_paths.r")
source("01-data_extraction/05.setup_SMR_data_extract.r")
###Extract ICD10 codes for epilepsy, relevant MH and other indications
##get chi list
upi_list <- readRDS(paste0(folder_data_path, "SLiPBD_upi_list.rds" ))
upi <- upi_list %>% select(mother_upi)
names(upi) <- "MOTHER_UPI"

##upload chi
dbRemoveTable(SMRAConnection,"MIP")
dbWriteTable(SMRAConnection, "MIP", upi)

#The table will be uploaded to a schema that is your username. To see a list of tables that you have uploaded run:

dbListTables(SMRAConnection,schema=toupper(Sys.info()[["user"]]))
##extract from 2005

data_smr01_temp_1

##Idenitfy diagnosis groups
smr01 <- data_smr01_temp_1  %>% 
  mutate(mh_indication = case_when(substr(main_condition,1,3) %in% ment_health ~1, 
                                 substr(other_condition_1,1,3) %in% ment_health ~1, 
                                 substr(other_condition_2,1,3) %in% ment_health ~1, 
                                 substr(other_condition_3,1,3) %in% ment_health ~1, 
                                 substr(other_condition_4,1,3) %in% ment_health ~1, 
                                 substr(other_condition_5,1,3) %in% ment_health ~1, 
                                 T~0), 
       epilepsy_indication = case_when(substr(main_condition,1,3) %in% epilepsy ~1, 
                                       substr(other_condition_1,1,3) %in% epilepsy ~1, 
                                       substr(other_condition_2,1,3) %in% epilepsy ~1, 
                                       substr(other_condition_3,1,3) %in% epilepsy ~1, 
                                       substr(other_condition_4,1,3) %in% epilepsy ~1, 
                                       substr(other_condition_5,1,3) %in% epilepsy ~1, 
                                       T~0), 
       migraine_pain_indication = case_when(substr(main_condition,1,3) %in% migraine_pain ~1, 
                                           substr(other_condition_1,1,3) %in% migraine_pain ~1, 
                                           substr(other_condition_2,1,3) %in% migraine_pain ~1, 
                                           substr(other_condition_3,1,3) %in% migraine_pain ~1, 
                                           substr(other_condition_4,1,3) %in% migraine_pain ~1, 
                                           substr(other_condition_5,1,3) %in% migraine_pain ~1, 
                                           substr(main_condition,1,4) %in% migraine_pain ~1, 
                                           substr(other_condition_1,1,4) %in% migraine_pain ~1, 
                                           substr(other_condition_2,1,4) %in% migraine_pain ~1, 
                                           substr(other_condition_3,1,4) %in% migraine_pain ~1, 
                                           substr(other_condition_4,1,4) %in% migraine_pain ~1, 
                                           substr(other_condition_5,1,4) %in% migraine_pain ~1, 
                                           T~0)) %>%
  mutate(alcohol_drug_use = case_when(substr(main_condition,1,3) %in% alcohol_drug ~1, 
                                       substr(other_condition_1,1,3) %in% alcohol_drug ~1, 
                                       substr(other_condition_2,1,3) %in% alcohol_drug ~1, 
                                       substr(other_condition_3,1,3) %in% alcohol_drug ~1, 
                                       substr(other_condition_4,1,3) %in% alcohol_drug ~1, 
                                       substr(other_condition_5,1,3) %in% alcohol_drug ~1, 
                                       substr(main_condition,1,4) %in% alcohol_drug ~1, 
                                       substr(other_condition_1,1,4) %in% alcohol_drug ~1, 
                                       substr(other_condition_2,1,4) %in% alcohol_drug ~1, 
                                      substr(other_condition_3,1,4) %in% alcohol_drug ~1, 
                                       substr(other_condition_4,1,4) %in% alcohol_drug ~1, 
                                       substr(other_condition_5,1,4) %in% alcohol_drug ~1, 
                                      T~0)) %>% 
  filter(mh_indication==1 | epilepsy_indication==1 |migraine_pain_indication==1 |alcohol_drug_use ==1 )

smr01 <- smr01 %>% select(upi_number, dob, dr_postcode, admission_date, discharge_date,
                                    epilepsy_indication, mh_indication, migraine_pain_indication, alcohol_drug_use)

saveRDS(smr01, paste0(folder_data_path, "smra_extracts/smr01_indicators.rds"))

