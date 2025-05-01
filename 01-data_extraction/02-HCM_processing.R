# 02-HCM_processing.R ####
# processing file for HCM data

# 2) Read in raw HCM extract --------------------------------------------
source("01-data_extraction/000.file_paths.r")
source("01-data_extraction/000.extracts.r")

# filter to SLiPBD UPI list
upi_list <-  readRDS(paste0(folder_data_path,'SLiPBD_upi_list.rds')) %>% select(mother_upi)

hcm <- hcm_extract %>% filter(patient_upi_number %in% upi_list$mother_upi)


# make derived date variables
# supply_date = der_presc_date
# supply_date = der_supplied_date
hcm <- hcm %>% 
  mutate(supply_date = as_datetime(supply_date)) %>% 
  rename(der_presc_date_time = supply_date) %>% 
  mutate(der_supplied_date_time = der_presc_date_time)


# for all cases flag with default_presc_date_flag variable = 'Y' 
# as using supply date for der_presc_date
hcm <- hcm %>% 
  mutate(default_presc_date_flag = 'Y')

# Check for missing dates
hcm %>% 
  filter(is.na(der_presc_date_time) | is.na(der_supplied_date_time)) %>% 
  summarise(n())
# all dates are filled


# make patient age at prescription date variable
# The age in years between the derived Patient Date of Birth and the der_presc_date.
# make patient age at given date variable
# The age in years between the derived Patient Date of Birth and the der_given_date.
hcm <- hcm %>% 
  mutate(pat_age_presc_date = age_calculate(patient_date_of_birth, der_presc_date_time),
         pat_age_supplied_date = age_calculate(patient_date_of_birth, der_supplied_date_time))

# Check age range
hcm %>% 
  group_by(pat_age_supplied_date) %>% 
  summarise(n()) %>% 
  print(n = 100)


# match to geography lookup by patient_postcode to get HB of res, CA of res, HSCP of res, urban/rural class, SIMD
geo_lookup <- readRDS(paste0(lookup_path,'Unicode/Deprivation/postcode_2024_2_simd2020v2.rds')) %>% 
  select(c(pc7, hb2019name, ca2019name, hscp2019name, simd2020v2_sc_quintile)) %>% 
  rename(patient_postcode = pc7,
         all_pat_hb_res_event = hb2019name,
         all_pat_ca_res_event = ca2019name,
         all_pat_hscp_res_event = hscp2019name,
         all_pat_simd_res_event = simd2020v2_sc_quintile) 


hcm <- left_join(hcm, geo_lookup, by = 'patient_postcode')

rm(geo_lookup)

urb_lookup <- 
  readRDS(paste0(lookup_path,'Unicode/Geography/Urban Rural Classification/postcode_urban_rural_2020.rds')) %>% 
  select(c(pc7, UR6_2020_name)) %>% 
  rename(patient_postcode = pc7,
         all_pat_urb_rural_res_event = UR6_2020_name)

hcm <- left_join(hcm, urb_lookup, by = 'patient_postcode')

rm(urb_lookup)


# BNF variables
# Need BNF chapter code, BNF chapter description,
# BNF section code, BNF section description,
# BNF sub section code, BNF sub section description,
# BNF paragraph code, BNF paragraph description
# Only have BNF chapter code (dmd_bnf_code), BNF paragraph code (dmd_bnf_paragraph_code),
# and BNF paragraph description (dmd_bnf_paragraph_description)
# can use HCM table bnf_code as a lookup to fill in this information

# Check the info we do have
hcm %>% 
  group_by(dmd_bnf_code) %>% 
  summarise(n())

# Read in lookup - in future check if we can get formulation and/or route from here 
bnf_lookup 

# Add variables from bnf lookup to main file
bnf_lookup <- bnf_lookup %>% 
  rename(dmd_bnf_code = bnf_code)
hcm <- left_join(hcm, bnf_lookup, by = 'dmd_bnf_code')

# rename variables
hcm <- hcm %>% 
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

hcm %>% 
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
# dummy_drug_descr = if no dmd code then populate dummy_drug_descr using medication_name_submitted.
hcm %>% 
  filter(is.na(dmd_code)) %>% 
  summarise(n())
# there are 0 missing in this case but create code for future instances

hcm <- hcm %>% 
  mutate(dmd_missing_flag = case_when(is.na(dmd_code) ~ 1,
                                      TRUE ~ 0)) %>% 
  mutate(dmd_code = case_when(dmd_missing_flag == 1 ~ 'DD00003',
                              TRUE ~ dmd_code)) %>% 
  mutate(vtm_name = case_when(dmd_missing_flag == 1 ~ 'DUMMY VTM',
                              TRUE ~ vtm_name)) %>% 
  mutate(vmp_name = case_when(dmd_missing_flag == 1 ~ 'DUMMY DRUG',
                              TRUE ~ vmp_name)) %>% 
  mutate(dummy_drug_descr = case_when(dmd_missing_flag == 1 ~ medication_name_submitted,
                                      TRUE ~ NA)) %>% 
  select(-c(dmd_missing_flag))



# Check for negative supply quantities 
hcm %>% 
  group_by(supply_quantity) %>% 
  summarise(n())
# We need to find their matching +ve record and aggregate into one row

# Create a flag for events with negative quantities
hcm <- hcm %>% 
  mutate(negative_quantity_flag = case_when(supply_quantity < 0 ~ 1,
                                            TRUE ~ 0))


# That will add 1, 2, 3 up to n() (number of rows) in the order that they're in but because we 
# also included "group_by()" first, it will restart the numbering in each group.
# This means that 1 will indicate first record within each grouping and >=2 will be duplicate case.

# Flag duplicate cases which have the same quantity
hcm <- hcm %>%
  arrange(der_presc_date_time, supply_id, patient_chi_number, medication_name_submitted, sending_location_name, supply_quantity) %>% # sort cases by grouping variables
  group_by(der_presc_date_time, supply_id, patient_chi_number, medication_name_submitted, sending_location_name, supply_quantity, .drop=FALSE) %>% # Set .drop=FALSE  in order to keep blank/NAs in the aggregation output
  mutate(gr_order = 1:n()) %>%
  ungroup()

hcm %>% 
  group_by(gr_order) %>% 
  summarise(n())

# Flag duplicates which have different quantities 
hcm <- hcm %>%
  group_by(der_presc_date_time, supply_id, patient_chi_number, medication_name_submitted, sending_location_name, gr_order, .drop=FALSE) %>% 
  mutate(total = n()) %>% # Calculate number of instances per grouping combination
  ungroup() %>%
  mutate(multi = case_when(total==1 ~ 0,  # 0 'Group of records with no duplicates'
                           total>1 ~ 1,   # 1 'Group of records with duplicates'.
                           TRUE ~ NA_real_)) %>%
  arrange(der_presc_date_time, supply_id, patient_chi_number, medication_name_submitted, sending_location_name, gr_order) %>% # Sort cases
  mutate(order = 1:n()) %>% # Create numbers 1, 2, 3, ...   for cases from top to bottom.
  group_by(der_presc_date_time, supply_id, patient_chi_number, medication_name_submitted, sending_location_name, gr_order, .drop=FALSE) %>% 
  mutate(order_min = min(order)) %>%
  ungroup() %>%
  mutate(flag = case_when(multi==0 ~ 1,  # Set flag to 1:  for unique cases   and   for first record within a group with the same LINKNO and CIS (on the basis of sorting).
                          order==order_min ~ 1,   
                          TRUE ~ 99))

hcm %>% 
  group_by(multi) %>% 
  summarise(n())


hcm <- hcm %>% 
  mutate(record_id2 = case_when(flag == 99 ~ record_id)) %>%
  mutate(supply_quantity2 = case_when(flag == 99 ~ supply_quantity)) %>% 
  group_by(order_min) %>% 
  fill(record_id2, .direction = c("up")) %>% 
  fill(supply_quantity2, .direction = c("up")) %>% 
  filter(flag != 99) %>% 
  ungroup() %>% 
  select(-c(gr_order, total, multi, order, order_min, flag))


hcm <- hcm %>% 
  mutate(presc_quantity = case_when(supply_quantity >= 0 ~ supply_quantity,
                                    supply_quantity2 >= 0 ~ supply_quantity2,
                                    TRUE ~ NA)) %>% 
  mutate(disp_quantity = case_when(supply_quantity < 0 ~ supply_quantity,
                                   supply_quantity2 < 0 ~ supply_quantity2,
                                   TRUE ~ supply_quantity)) %>% 
  mutate(disp_quantity = case_when(disp_quantity < 0 ~ 0,
                                   TRUE ~ disp_quantity)) %>% 
  mutate(presc_not_supplied = case_when(disp_quantity == 0 ~ 'Y',
                                        TRUE ~ 'N'))

# Check;
# presc_not_supplied = Y only when disp_quantity = 0
# presc_quantity = disp_quantity unless disp_quantity == 0
# 0 disp_quantity = # of negative cases flagged above
hcm %>% 
  filter(disp_quantity == 0) %>% 
  group_by(presc_not_supplied) %>% 
  summarise(n())


hcm <- hcm %>% 
  mutate(flag = case_when(presc_quantity != disp_quantity ~ 1,
                          TRUE ~ NA)) 

hcm %>% 
  filter(disp_quantity > 0) %>% 
  group_by(flag) %>% 
  summarise(n())


hcm %>% 
  filter(disp_quantity == 0) %>% 
  summarise(n())


# Check quantity vars
hcm %>% 
  group_by(presc_quantity, disp_quantity) %>% 
  summarise(n())


# Make a data source flag
hcm <- hcm %>%  
  mutate(data_source = 'HCM')

# Make a default form type variable
hcm <- hcm %>% 
  mutate(form_type = 'hcm',
         form_type_desc = 'hcm')


# rename variables
hcm <- hcm %>% 
  rename(all_chi = patient_chi_number,
         all_upi = patient_upi_number,
         all_dob = patient_date_of_birth,
         all_sex = patient_sex,
         all_sex_desc = patient_sex_desc,
         all_dod = patient_date_of_death,
         all_pat_postcode_event = patient_postcode,
         dmd_code_event = dmd_code,
         atc_group_code = dmd_atc_code,
         atc_group_descr = dmd_atc_description,
         ddd_tvpm = dmd_ddd_conversion_factor,
         # dose_instruction = submitted_dose_instruction,
         hb_treatment = treatment_health_board_name) %>% 
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
           # dose_instruction,
           ddd_tvpm,
           hb_treatment,
           data_source,
           form_type,
           form_type_desc,
           record_id,
           record_id2))


# Make all strings lower case
hcm <- hcm %>% 
  mutate(across(where(is.character), tolower)) 


