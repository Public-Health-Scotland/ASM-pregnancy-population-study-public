# 03-PIS_processingR ####
# processing file for PIS data


#### 1. Create flags ####
# Serial prescribing flag = Y if claim service flag = C, else = N
pis_data <- pis_data %>% 
  mutate(serial_presc_flag = case_when(
    claim_service_flag == 'C' ~ 'Y',
    claim_service_flag !='C' ~ 'N'
  ))

# Default prescribed date flag = Y if claim DCVP electronic flag = N & Presc Date is end of the month
## Identify where prescribed date is last day of the month

# check the earliest presc date to make sure the seq below goes back far enough:
min(pis_data$der_presc_date_time) # earliest is in 2000 so adjust below for this

pis_data <- pis_data %>%
  mutate(presc_end_month = case_when(
    as.Date(der_presc_date_time) %in% c(seq(from = as.Date("1999-12-01"), to = as.Date(Sys.Date()),by="months")-1) ~ "Y",
    .default = "N"
  ))

## Flag where dcvp electronic flag = N and presc date at end of month
pis_data <- pis_data  %>% mutate(default_presc_date_flag = case_when(
  claim_dcvp_electronic_flag == 'N' & presc_end_month == 'Y' ~ 'Y',
  .default = 'N'
))

# Default disp date flag = Y if:
## claim DCVP electronic flag = N & disp Date is end of the month 
## or disp quantity = 0, regardless of claim DCVP electronic flag value
## or where item has not been dispensed 

# check the earliest disp date to make sure the seq below goes back far enough:
min(pis_data$der_disp_date_time) # earliest is in 2010 so adjust below for this.

## Identify where dispensed date is last day of the month
pis_data <- pis_data %>%
  mutate(disp_end_month = case_when(
    as.Date(der_disp_date_time) %in% c(seq(from = as.Date("2008-02-01"), to = as.Date(Sys.Date()),by="months")-1) ~ "Y",
    .default = "N"
  ))

## Flag where dcvp electronic flag = N and disp date at end of month
pis_data <- pis_data %>% mutate(default_disp_date_flag = case_when(
  (claim_dcvp_electronic_flag == 'N' & disp_end_month == 'Y') | disp_quantity == 0 | item_not_dispensed == "Y" ~ 'Y',
  .default = 'N'
))

# Remove unwanted variables
pis_data <- pis_data %>% select(-presc_end_month, -disp_end_month)


# Prescription not given flag
## If item not collected or item not dispensed = Y then Prescription not given flag = Y else N
## For pre May 2023 where gic incl bb = 0 & gic excl bb != 0 then mark item as not collected
pis_data <- pis_data %>% mutate(item_not_collected =  case_when((gic_incl_bb == 0 & gic_excl_bb != 0) ~ 'Y',
                                                                .default = item_not_collected))


pis_data <- pis_data %>% mutate(presc_not_supplied = case_when(
  (item_not_collected == 'Y' | item_not_dispensed == 'Y' | disp_quantity == 0 )~ 'Y',
  .default = 'N'
))


#### 2. Concatenate Strength variables #### 

pis_data <- pis_data %>% mutate(med_strength = case_when(
  is.na(strength_per) ~ paste0(strength, strength_uom),
  .default = paste0(strength, strength_uom, " per ", strength_per, strength_per_uom)),
  disp_med_strength = case_when(
    is.na(disp_strength_per) ~ paste0(disp_strength, disp_strength_uom),
    .default = paste0(disp_strength, disp_strength_uom, " per ", disp_strength_per, disp_strength_per_uom))
)


#### 3. Geography lookup ####
# match to geography lookup by patient_postcode to get HB of res, CA of res, HSCP of res, urban/rural class, SIMD
geo_lookup <- readRDS(paste0(lookup_path,'Unicode/Deprivation/postcode_2024_2_simd2020v2.rds')) %>% 
  select(c(pc7, hb2019name, hb2019, ca2019name, ca2019, hscp2019name, hscp2019, simd2020v2_sc_quintile)) %>% 
  rename(all_pat_postcode_event = pc7,
         all_pat_hb_res_event = hb2019name,
         all_pat_hb_code_res_event = hb2019,
         all_pat_ca_res_event = ca2019name,
         all_pat_ca_code_res_event = ca2019,
         all_pat_hscp_res_event = hscp2019name,
         all_pat_hscp_code_res_event = hscp2019,
         all_pat_simd_res_event = simd2020v2_sc_quintile) 


pis_data <- left_join(pis_data, geo_lookup, by = 'all_pat_postcode_event')

rm(geo_lookup)

urb_lookup <- readRDS(paste0(lookup_path,'Unicode/Geography/Urban Rural Classification/postcode_urban_rural_2020.rds')) %>% 
  select(c(pc7, UR6_2020_name, UR6_2020)) %>% 
  rename(all_pat_postcode_event = pc7,
         all_pat_urb_rural_res_event = UR6_2020_name,
         all_pat_urb_rural_code_res_event = UR6_2020)


pis_data <- left_join(pis_data, urb_lookup, by = 'all_pat_postcode_event')

rm(urb_lookup)

#### 4. Make patient age variables  ####

pis_data <- pis_data %>%
  mutate(chi_dob = dob_from_chi(all_chi, 
                                min_date = as.Date("1930-01-01"), 
                                max_date = as.Date("2024-01-01")))

pis_data <- pis_data %>% 
  mutate(pat_age_presc_date = age_calculate(chi_dob, der_presc_date_time),
         pat_age_supplied_date = age_calculate(chi_dob, der_disp_date_time))

# Check age range
pis_data %>% 
  group_by(pat_age_supplied_date) %>% 
  summarise(n()) %>% 
  print(n = 100)


# Bring in SLiPBD UPI file and check against it
upi_list <-  readRDS(folder_data_path, 'SLiPBD_upi_list.rds') %>% select(mother_upi)

upi_check <- select(pis_data, all_upi)  %>% unique() %>% filter(!(all_upi %in% upi_list$mother_upi))
# all upis are in SLiBD list

#### 5. Select only required fields and Save data out ####

# Add source flag
pis_data_final <- pis_data %>% 
  mutate(data_source = 'PIS') %>% 
  select(all_upi,
         all_chi, # all_chi
         all_dob,
         pat_age_presc_date,
         pat_age_supplied_date,
         all_sex,
         all_sex_desc,
         all_dod,
         all_pat_postcode_event,
         all_pat_hb_code_res_event,
         all_pat_hb_res_event,
         all_pat_ca_code_res_event,
         all_pat_ca_res_event,
         all_pat_hscp_code_res_event,
         all_pat_hscp_res_event,
         all_pat_urb_rural_code_res_event,
         all_pat_urb_rural_res_event,
         all_pat_simd_res_event,
         all_pat_care_home_res,
         der_presc_date_time, # presc_date_time
         der_disp_date_time, # supplied_date_time
         serial_presc_flag,
         default_presc_date_flag,
         default_disp_date_flag,
         presc_not_supplied,
         instal_disp_flag,
          bnf_chapter_code,
         bnf_chapter_descr,
         bnf_section_code,
         bnf_section_descr,
         bnf_sub_section_code,
         bnf_sub_section_descr,
         bnf_paragraph_code,
         bnf_paragraph_descr,
         prescribed_product_code_event, #10 dmd_code_event
         prescribed_product_code_current, # dmd_code_current
         vtm_name, #vtm_name
         vmp_name, # vmp_name
         dummy_drug_descr, # dummy_drug_descr
         atc_code,
         atc_description,
         disp_bnf_chapter_code,
         disp_bnf_chapter_descr,
         disp_bnf_section_code,
         disp_bnf_section_descr,
         disp_bnf_sub_section_code,
         disp_bnf_sub_section_descr,
         disp_bnf_paragraph_code,
         disp_bnf_paragraph_descr,
         dispensed_product_code_event, #20 disp_dmd_code_event
         dispensed_product_code_current,
         disp_vtm_name, # disp_vtm_name
         disp_vmp_name,
         disp_dummy_drug_descr,
         disp_atc_code,
         disp_atc_description,
         presc_quantity, 
         disp_quantity, #supp_quantity
         med_strength,
         disp_med_strength,
         presc_formulation,
         disp_formulation,
         ddd_tvpm, # presc_ddd_tvpm
         prescribing_hb, # hb_treatment
         disp_ddd_tvpm,
         form_type,
         data_source,
         claim_image_ref,
         form_barcode,
         prescription_line_no,
         form_scan_ref_no,
         claim_id,
         primary_presc_item_flag,
         primary_disp_item_flag # 38
         
         
  ) 

#pis_unique <- pis_data_final %>% unique()
#rm(pis_unique)

# rename columns to match shared data extract
pis_data_final <- pis_data_final %>% 
  rename(presc_date_time  = der_presc_date_time, 
         supplied_date_time = der_disp_date_time,
         default_supplied_date_flag = default_disp_date_flag,
         dmd_code_event = prescribed_product_code_event, 
         dmd_code_current = prescribed_product_code_current, 
         supp_quantity = disp_quantity, 
         hb_treatment = prescribing_hb,  
         disp_dmd_code_event = dispensed_product_code_event, 
         disp_dmd_code_current = dispensed_product_code_current, 
         supp_quantity = disp_quantity
  )


# Make all strings lower case
pis_data_final <- pis_data_final %>% 
  mutate(across(where(is.character), tolower))   


