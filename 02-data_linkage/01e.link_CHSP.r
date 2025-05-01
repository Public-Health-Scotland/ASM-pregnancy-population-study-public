###01e.link_CHSP.r##
# Script to link early childhood developmental concerns from CHSP-PS
# to pregnancy dataset and flag indicators

library(dplyr)
library(stringr)
library(arrow)
library(phsmethods)
library(lubridate)

source("data_linkage/00.setup_expose.r")

#load data####
CHSP_extract <- arrow::read_parquet(paste0(data_path,"27m_review/IR2024-00643-27m.parquet"))
pregs <- readRDS(paste0(data_path, "SLiPBD_cohort_extract.rds")) %>%
  filter(fetus_outcome1 =="Live birth" | fetus_outcome2=="Live birth") 

#table(CHSP_extract$chi %in% slipbd_database$baby_chi, lubridate::year(CHSP_extract$date_exm))
#table(CHSP_extract$chi %in% pregs$baby_chi, lubridate::year(CHSP_extract$date_exm))
##a lot of CHIS not in SLiPBD - check that the UPI are up to date? 
#table(chi_check(pregs$baby_chi))
#table(chi_check(CHSP_extract$chi))

#Variable formatting and cleaning####
# VS - think dev_persoc have been included in the below, so added it in
CHSP_extract <- CHSP_extract %>%
  mutate(any_dev_excl_v_h = case_when( dev_slc %in% c("C", "P") | dev_prob %in% c("C", "P")|
                                                 dev_EB %in% c("C", "P") |
                                                 dev_gross %in% c("C", "P") |
                                                 dev_persoc %in% c("C", "P") |
                                                 dev_fm %in% c("C", "P") ~ "Y", T~"N"),
    any_new_dev_excl_v_h = case_when( dev_slc=="C" | dev_prob=="C" |
                                           dev_EB=="C" |
                                           dev_gross=="C" |
                                           dev_persoc=="C" |
                                           dev_fm=="C" ~ "Y", T~"N"),
         incomplete_info = case_when( dev_slc %in% c("X", "I") | dev_prob %in% c("X", "I") |
                                        dev_EB %in% c("X", "I") |
                                        dev_gross %in% c("X", "I") |#
                                        dev_persoc %in% c("X", "I") |
                                        dev_fm %in% c("X", "I")  ~ 1, T~0),
         ###old codes do not map to dev_slc so coed as NA
        no_info =  case_when( dev_slc %in% c("X", "I", "NA") & dev_prob %in% c("X", "I", "NA") &
                                          dev_EB %in% c("X", "I", "NA") &
                                          dev_gross %in% c("X", "I", "NA") &
                                          dev_persoc %in% c("X", "I", "NA") &
                                          dev_fm %in% c("X", "I", "NA")  ~ 1, T~0))                       
#indivdual flags for tables
CHSP_extract <- CHSP_extract %>%
  mutate( dev_EB_flag=case_when( dev_EB %in% c("C", "P") ~ "Y", 
                                   dev_EB %in% c("X", "I") ~"unknown", 
                                   dev_EB %in% "NA"~ "NA",
                                   dev_EB == "N"~ "N"), 
          dev_slc_flag= case_when( dev_slc %in% c("C", "P") ~ "Y", 
                             dev_slc %in% c("X", "I") ~"unknown", 
                             dev_slc %in% "NA"~ "NA",
                             dev_slc == "N"~ "N"),
         dev_prob_flag=case_when( dev_prob %in% c("C", "P") ~ "Y", 
                             dev_prob %in% c("X", "I") ~"unknown", 
                             dev_prob %in% "NA"~ "NA",
                             dev_prob == "N"~ "N"), 
         dev_gross_flag=case_when( dev_gross %in% c("C", "P") ~ "Y", 
                                   dev_gross %in% c("X", "I") ~"unknown", 
                                   dev_gross %in% "NA"~ "NA",
                                   dev_gross == "N"~ "N"),
         dev_persoc_flag=case_when(dev_persoc %in% c("C", "P") ~ "Y", 
                                   dev_persoc %in% c("X", "I") ~"unknown", 
                                   dev_persoc %in% "NA"~ "NA",
                                   dev_persoc == "N"~ "N"),
         dev_fm_flag=case_when(dev_fm %in% c("C", "P") ~ "Y", 
                               dev_fm %in% c("X", "I") ~"unknown", 
                               dev_fm %in% "NA"~ "NA",
                               dev_fm == "N"~ "N"))
#recode hb of asessment        
CHSP_extract <- CHSP_extract %>% 
  mutate(hb_27m = case_when(hb_sys=="A" ~ "Ayrshire & Arran",
                            hb_sys=="B" ~ "Borders",
                            hb_sys=="C" ~ "Argyll & Clyde",
                            hb_sys=="F" ~ "Fife",
                            hb_sys=="G" ~ "Greater Glasgow",
                            hb_sys=="H" ~ "Highland",
                            hb_sys=="L" ~ "Lanarkshire",
                            hb_sys=="N" ~ "Grampian",
                            hb_sys=="R" ~ "Orkney",
                            hb_sys=="S" ~ "Lothian",
                            hb_sys=="T" ~ "Tayside",
                            hb_sys=="V" ~ "Forth Valley",
                            hb_sys=="W" ~ "Western Isles",
                            hb_sys=="Y" ~ "Dumfries & Galloway",
                            hb_sys=="Z" ~ "Shetland",
                            T~"missing")) 

#table(CHSP_extract$any_dev_excl_v_h, CHSP_extract$incomplete_info)                                   

##check within-CHSP dates by deriving dob from CHI

CHSP_extract <- CHSP_extract %>% 
  mutate(chi_dob = dob_from_chi(chi, min_date = as.Date("2000-01-01"), max_date =as.Date("2024-09-17"))) %>%
  mutate(chi_age_days = difftime(date_exm, chi_dob, "days")) %>%
  mutate(chi_age_yrs = round(as.numeric(chi_age_days)/365.25,2))
table(CHSP_extract$chi_age_yrs)

##join to pregnancy cohort####
pregs_chsp <- left_join(pregs, CHSP_extract, by =c("baby_chi"= "chi"))
#checking on age at test moved to later - for now keep all links

##Filter singletons and limited variables####
pregs_chsp <- pregs_chsp %>% filter(total_fetuses_this_pregnancy==1) %>%
  select(pregnancy_id, baby_chi,sex, mother_upi,date_exm, postcode_review, postcode_birth,hb_27m,
       any_dev_excl_v_h,smokers, prim_smok,
       incomplete_info, no_info, dev_slc_flag,dev_EB_flag, dev_fm_flag, dev_gross_flag, 
       dev_persoc_flag, dev_prob_flag,tool1,tool2,tool3,tool4 ) %>%
  rename(date_review_27m = date_exm, sex_child_27m =sex, 
         pc_review_27m = postcode_review, pc_birth_27m = postcode_birth, 
         smokers_27m = smokers, prim_smok_27m = prim_smok) 

CHSP_unlinked <- CHSP_extract %>% filter(!chi %in% pregs_chsp$baby_chi)

table(substr(pregs_chsp$pc_birth_27m,1,2))
table(substr(CHSP_unlinked$postcode_birth,1,2))
##about 65% of unlinked have no postcode birth recorded - does this indicate born elsewhere?
##in which case we dont expect to link them
##some of the unlinked are multiples, or just out of date range
##maybe not all of them - a few with no postocde DO link but only .2% of the total that link!
##still ~ 1k - 1.5k a year we outght to eb able to link though.
table((CHSP_unlinked$postcode_birth==""))


#save file####
saveRDS(pregs_chsp, paste0(data_path,"linkage/chsp_flags.rds"))

##QA of postcodes age at test etc####
#and overall linakage
##Checking linkage to the entire dataset to get a handle on 
##the actual proportion ofnon-linking babies (and any apparently bad links )
slipbd_database <- readRDS(paste0(slipbd_path2 , "data/archive/slipbd_database.rds"))
lb_CHECK <- slipbd_database %>% 
  filter(fetus_outcome1 =="Live birth")# %>%
 # filter(date_end_pregnancy >= as.Date("2010-01-01"))

chsp_nomatch <- CHSP_extract %>% filter(!chi %in% lb_CHECK$baby_chi)
table(pregs_chsp$postcode_birth=="")
table(chsp_nomatch$postcode_birth=="")

##link to total dataset
chsp_link_all_lb <- left_join(lb_CHECK, CHSP_extract,by =c("baby_chi"= "chi")) %>%
  filter(!is.na(date_exm))

table(year(chsp_link_all_lb$date_end_pregnancy))
##some really early or late links 
table(chsp_link_all_lb$chi_dob - chsp_link_all_lb$date_end_pregnancy)
#the dob and the CHI agree
#why these babies are listed as having their 27-26 month review years early or late is a mystery!
table(chsp_link_all_lb$postcode_birth=="")
##should be between 2.25 and 3 years of age at review.
#allow .1 either side (means we are not discounting those only a few days out)
table(chsp_link_all_lb$chi_age_yrs <= 3.1 & chsp_link_all_lb$chi_age_yrs >=2.15)
#<1% outside this age range at test
table((chsp_link_all_lb$chi_age_yrs <= 3.1 & chsp_link_all_lb$chi_age_yrs >=2.15)[chsp_link_all_lb$postcode_birth==""])
table((chsp_link_all_lb$chi_age_yrs <= 3.1 & chsp_link_all_lb$chi_age_yrs >=2.15)[chsp_link_all_lb$postcode_birth!=""])

table(lb_CHECK$total_fetuses_this_pregnancy>1)
#22493/nrow(lb_CHECK)  # ~3% of birth records
