# 01-HEPMA_processing.R ####
# processing file for HEPMA data

# 1) Read in raw HEPMA extract --------------------------------------------
source("01-data_extraction/000.file_paths.r")
source("01-data_extraction/000.extracts.r")
# filter to SLiPBD UPI list
upi_list <-  readRDS(paste0(folder_data_path,"SLiPBD_upi_list.rds")) %>% select(mother_upi)

hepma <- hepma %>% filter(patient_upi_number %in% upi_list$mother_upi)


# make derived date variables
# reporting_date = der_presc_date
# admin_given_date = der_supplied_date
hepma <- hepma %>% 
  mutate(der_presc_date_time = reporting_date) %>% 
  rename(der_supplied_date_time = admin_given_date_time)

# check instances where der_presc_date_time is blank and flag as will have been filled with admin_scheduled_date
hepma %>% 
  filter(is.na(presc_start_date_time)) %>% 
  summarise(n())


# Flag these as date in der_presc_date_time will actually be admin_scheduled_date_time instead
hepma <- hepma %>% 
  mutate(default_presc_date_flag = case_when(is.na(presc_start_date_time) ~ "y",
                                             TRUE ~ NA)) %>% 
  select(-c(presc_start_date_time))

hepma %>% 
  group_by(default_presc_date_flag) %>% 
  summarise(n())


hepma %>% 
  filter(is.na(der_presc_date_time)) %>% 
  summarise(n())

# Check that der_given_date_time is only blank when presc_has_no_associated_admin = "Y" 
hepma %>% 
  filter(is.na(der_supplied_date_time)) %>% 
  group_by(prescription_has_no_associated_admin) %>% 
  summarise(n())

# If there are instances where der_supplied_date_time is not null - fix these
hepma <- hepma %>% 
  mutate(der_supplied_date_time = case_when(prescription_has_no_associated_admin == "Y" ~ NA,
                                            TRUE ~ der_supplied_date_time))

hepma %>% 
  filter(is.na(der_supplied_date_time)) %>% 
  summarise(n())

# make patient age at prescription date variable
# The age in years between the derived Patient Date of Birth and the Prescription Start Date.
# make patient age at given date variable
# The age in years between the derived Patient Date of Birth and the Admin Scheduled Date
hepma <- hepma %>% 
  mutate(pat_age_presc_date = age_calculate(patient_date_of_birth, der_presc_date_time),
         pat_age_supplied_date = age_calculate(patient_date_of_birth, der_supplied_date_time))

# If der_supplied_date_time = NA (prescription has no associated admin)
# make pat_age_supplied_date = pat_age_presc_date
hepma <- hepma %>% 
  mutate(pat_age_supplied_date = case_when(prescription_has_no_associated_admin == "Y" ~ pat_age_presc_date,
                                           TRUE ~ pat_age_supplied_date))


# Check age range
hepma %>% 
  group_by(pat_age_supplied_date) %>% 
  summarise(n()) %>% 
  print(n = 100)


# match to geography lookup by patient_postcode to get HB of res, CA of res, HSCP of res, urban/rural class, SIMD
geo_lookup <- readRDS(paste0(lookup_path, "Unicode/Deprivation/postcode_2024_2_simd2020v2.rds")) %>% 
  # Check if updated version available
  select(c(pc7, hb2019name, ca2019name, hscp2019name, simd2020v2_sc_quintile,)) %>% 
  rename(patient_postcode = pc7,
         all_pat_hb_res_event = hb2019name,
         all_pat_ca_res_event = ca2019name,
         all_pat_hscp_res_event = hscp2019name,
         all_pat_simd_res_event = simd2020v2_sc_quintile) 


hepma <- left_join(hepma, geo_lookup, by = 'patient_postcode')

rm(geo_lookup)

urb_lookup <- readRDS(paste0(lookup_path,'Unicode/Geography/Urban Rural Classification/postcode_urban_rural_2020.rds')) %>% 
  select(c(pc7, UR6_2020_name)) %>% 
  rename(patient_postcode = pc7,
         all_pat_urb_rural_res_event = UR6_2020_name)


hepma <- left_join(hepma, urb_lookup, by = 'patient_postcode')

rm(urb_lookup)

# BNF variables
# Need BNF chapter code, BNF chapter description,
# BNF section code, BNF section description,
# BNF sub section code, BNF sub section description,
# BNF paragraph code, BNF paragraph description
# Only have BNF chapter code (dmd_bnf_code), 
# can use HEPMA table bnf_code as a lookup to fill in this information

# Check the info we do have
hepma %>% 
  group_by(dmd_bnf_code) %>% 
  summarise(n())

# Read in lookup
bnf_lookup ## sourced from 00.file_paths.R

# Add variables from bnf lookup to main file
bnf_lookup <- bnf_lookup %>% 
  rename(dmd_bnf_code = bnf_code)

hepma <- left_join(hepma, bnf_lookup, by = 'dmd_bnf_code')

# rename variables
hepma <- hepma %>% 
  rename(bnf_chapter_code = bnf_chapter,
         bnf_chapter_descr = bnf_chapter_description,
         bnf_section_code = bnf_section,
         bnf_section_descr = bnf_section_description,
         bnf_sub_section_code = bnf_sub_section,
         bnf_sub_section_descr = bnf_sub_section_description,
         bnf_paragraph_code = bnf_paragraph,
         bnf_paragraph_descr = bnf_paragraph_description,
         vtm_name = dmd_vtm_name,
         vmp_name = dmd_vmp_name) %>% 
  select(-c(dmd_bnf_code))

hepma %>% 
  group_by(bnf_chapter_code,
           bnf_chapter_descr,
           bnf_section_code,
           bnf_section_descr,
           bnf_sub_section_code,
           bnf_sub_section_descr,
           bnf_paragraph_code,
           bnf_paragraph_descr,
           dmd_code) %>% 
  summarise(n()) #%>% 
#view()

rm(bnf_lookup)

# dmd_code = if no dmd code populate dmd code using 'DD00003'. 
# vtm_name = if no dmd code then populate vtm name with 'DUMMY VTM'. 
# vmp_name = if no dmd code then populate vmp name with 'DUMMY DRUG'
# dummy_drug_descr = if no dmd code then populate dummy_drug_descr using medication_name.
hepma %>% 
  filter(is.na(dmd_code)) %>% 
  summarise(n())


hepma <- hepma %>% 
  mutate(dmd_missing_flag = case_when(is.na(dmd_code) ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(dmd_code = case_when(dmd_missing_flag == 1 ~ 'DD00003',
                              TRUE ~ dmd_code)) %>% 
  mutate(vtm_name = case_when(dmd_missing_flag == 1 ~ 'DUMMY VTM',
                              TRUE ~ vtm_name)) %>% 
  mutate(vmp_name = case_when(dmd_missing_flag == 1 ~ 'DUMMY DRUG',
                              TRUE ~ vmp_name)) %>% 
  mutate(dummy_drug_descr = case_when(dmd_missing_flag == 1 ~ medication_name,
                                      TRUE ~ NA)) %>% 
  select(-c(dmd_missing_flag))


# Make prescribed and dispensed quantity variables
hepma %>% 
  group_by(admin_not_given) %>% 
  summarise(n())


hepma <- hepma %>% 
  mutate(presc_quantity = 1) %>% 
  mutate(disp_quantity = case_when(admin_not_given == "Y" ~ 0,
                                   admin_not_given == "N" ~ 1,
                                   TRUE ~ 999)) 

hepma %>% 
  group_by(presc_quantity, disp_quantity) %>% 
  summarise(n())


# Make a data source flag
hepma <- hepma %>% 
  mutate(data_source = 'hepma')

# Make a default form type variable
hepma <- hepma %>% 
  mutate(form_type = 'hepma',
         form_type_desc = 'hepma')

# rename variables
hepma <- hepma %>% 
  rename(all_chi = patient_chi_number,
         all_upi = patient_upi_number,
         all_dob = patient_date_of_birth,
         all_sex = patient_sex,
         all_sex_desc = patient_sex_desc,
         all_dod = patient_date_of_death,
         all_pat_postcode_event = patient_postcode,
         presc_not_supplied = prescription_has_no_associated_admin,
         admin_not_supplied = admin_not_given,
         admin_reason_not_supplied = admin_reason_not_given,
         dmd_code_event = dmd_code,
         atc_group_code = dmd_atc_code,
         atc_group_descr = dmd_atc_code_description,
         dose_instruction = med_instruction,
         ddd_tvpm = dmd_ddd_conversion_factor,
         hb_treatment = treatment_health_board_name,
         med_formulation = medication_formulation) %>% 
  select(c(all_chi,
           all_upi,
           all_dob,
           pat_age_presc_date,
           pat_age_supplied_date,
           all_sex,
           all_sex_desc,
           all_dod,
           all_pat_postcode_event,
           all_pat_hb_res_event,
           all_pat_ca_res_event,
           all_pat_hscp_res_event,
           all_pat_urb_rural_res_event,
           all_pat_simd_res_event,
           der_presc_date_time,
           der_supplied_date_time,
           default_presc_date_flag,
           presc_not_supplied,
           admin_not_supplied,
           admin_reason_not_supplied,
           bnf_chapter_code,
           bnf_chapter_descr,
           bnf_section_code,
           bnf_section_descr,
           bnf_sub_section_code,
           bnf_sub_section_descr,
           bnf_paragraph_code,
           bnf_paragraph_descr,
           dmd_code_event,
           vtm_name,
           vmp_name,
           dummy_drug_descr,
           atc_group_code,
           atc_group_descr,
           presc_quantity,
           disp_quantity,
           dose_instruction,
           med_formulation,
           med_strength,
           ddd_tvpm,
           hb_treatment,
           data_source,
           form_type,
           form_type_desc,
           presc_unique_id,
           admin_unique_id))


# Make all strings lower case
hepma <- hepma %>% 
  mutate(across(where(is.character), tolower)) 



