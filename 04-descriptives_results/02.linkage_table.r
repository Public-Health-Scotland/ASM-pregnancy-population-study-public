##02.linkage_table.r
# script to produce numbers for flow table


#libraries####
library(dplyr)
library(stringr)
library(arrow)
library(phsmethods)
library(lubridate)


source("04-descriptives_results/00.descriptives_setup.r")

#filepaths


##data####
master_dataset_file <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds"))
slipbd_database <- readRDS(paste0(slipbd_path,"archive/slipbd_database.rds"))

##preg outcomes ####
table(master_dataset_file$fetus_outcome1, master_dataset_file$pregnancy_loss)
preg_outcomes <- master_dataset_file %>%
  #reassign unknown assumed losses to loss group
  mutate(pregnancy_loss = case_when(fetus_outcome1 =="Unknown - assumed early loss" ~ "Yes", 
                                    T~ pregnancy_loss)) %>%
  group_by(exposed_any_asm, pregnancy_loss)%>%
  count()  
preg_outcomes
preg_outcomes <- preg_outcomes %>% ungroup() %>%
  mutate(Outcome = paste("Pregnancy loss", pregnancy_loss))%>%
  select(exposed_any_asm, Outcome, n)




##Congenital conditions
cc_outcomes <- master_dataset_file %>%
  filter(control_pool_CC==1|  cases_CC==1) %>%
  group_by(CC_exposed_any_asm, any_CC)%>%
  count()

cc_outcomes<- cc_outcomes %>% ungroup() %>%
  mutate(Outcome = paste("Any congenital condition", any_CC))%>%
  select(CC_exposed_any_asm, Outcome, n) %>%
  rename(exposed_any_asm = CC_exposed_any_asm)



##CHSP outcome
#DEV_outcomes <- master_dataset_file %>%
#  filter(control_pool_dev==1|  cases_dev==1) %>%
#  group_by(exposed_any_asm, any_dev_excl_v_h) %>%
#  count()


##problem is that we didnt flag those with an invalid review 
#(wrong date or missing) or live births with no review and we need those numebrs for table
DEV_outcomes2 <- master_dataset_file %>%
  #Dev outcomes cohort. LIVE BIRTHS only with date of conception between April 2010 and 1 July 2020 
  filter( est_date_conception <= as.Date("2020-07-01") & pregnancy_loss=="No" ) %>%
  ##group unknowns and NAs
  mutate(any_dev_excl_v_h = case_when(any_dev_excl_v_h== "U" | is.na(any_dev_excl_v_h)~NA, 
                                      T~any_dev_excl_v_h))%>%
  group_by(exposed_any_asm, any_dev_excl_v_h)%>%
 count()
DEV_outcomes2 <- DEV_outcomes2 %>% ungroup() %>%
  mutate(Outcome = paste("Any developmental concern", any_dev_excl_v_h )) %>%
  select(exposed_any_asm, Outcome, n)


#combine outputs into one table for csv
all_outcomes <- rbind(preg_outcomes, cc_outcomes, DEV_outcomes2)

###to get N in CHSP only####
CHSP_extract <- arrow::read_parquet(paste0(folder_data_path,"27m_review/IR2024-00643-27m.parquet"))
##Checking linkage to the entire dataset to get a handle on 
##the actual proportion ofnon-linking babies (and any apparently bad links )

lb_CHECK <- slipbd_database %>% 
  filter(fetus_outcome1 =="Live birth")

chsp_nomatch <- CHSP_extract %>% filter(!chi %in% lb_CHECK$baby_chi) 

chsp_nomatch <- chsp_nomatch %>% 
  mutate(chi_dob = dob_from_chi(chi, min_date = as.Date("2000-01-01"), max_date =as.Date("2024-09-17"))) %>%
  mutate(chi_age_days = difftime(date_exm, chi_dob, "days")) %>%
  mutate(chi_age_yrs = round(as.numeric(chi_age_days)/365.25,2))

table(year(chsp_nomatch$dob))
table(chsp_nomatch$chi_age_yrs)

n <- nrow(chsp_nomatch)
exposed_any_asm <- NA
Outcome <- "CHSP only"
table(chsp_nomatch$postcode_birth=="")

chsp_only_n <- cbind(exposed_any_asm, Outcome, n)

all_outcomes <- rbind(all_outcomes, chsp_only_n)

#write.csv(all_outcomes, paste0(folder_data_path, "Descriptives/table_linkage.csv" ))

postcodes_dev <- master_dataset_file %>%
  #Dev outcomes cohort. LIVE BIRTHS only with date of conception between April 2010 and 1 July 2020 
  filter( est_date_conception <= as.Date("2020-07-01") & pregnancy_loss=="No" ) 
table(postcodes_dev$pc_birth_27m  =="")

####SLICCD unmatched####
###estimate the conception date to count only those that are in theory valid for linkage to this cohort

sliccd_unmatched_pre2010 <-  readRDS(paste0(folder_data_path, "linkage/sliccd_unmatched_pre2010.RDS"))
sliccd_unmatched_after2010 <- readRDS(paste0(folder_data_path, "linkage/sliccd_unmatched_after2010.RDS"))
nrow(sliccd_unmatched_pre2010)
nrow(sliccd_unmatched_after2010)
#sliccd_unmatched_pre2010 <- sliccd_unmatched_pre2010  %>%
#  mutate(est_conception_date = date_end_of_pregnancy - (gestation-2)*7)
#table(year(sliccd_unmatched_pre2010$est_conception_date), month(sliccd_unmatched_pre2010$est_conception_date))
##there are a few conceptiondates after Apriil 2010 in that file

sliccd_unmatched <- rbind(sliccd_unmatched_after2010 ,sliccd_unmatched_pre2010)
sliccd_unmatched <- sliccd_unmatched %>%
  mutate(est_conception_date = date_end_of_pregnancy - (gestation-2)*7)

table(year(sliccd_unmatched$est_conception_date), month(sliccd_unmatched$est_conception_date))
##remove any with conception date before 1st april 2010
sliccd_unmatched <- sliccd_unmatched %>% filter(est_conception_date >= as.Date("2010-04-01"))
nrow(sliccd_unmatched)

table(sliccd_unmatched$multiple_pregnancy)
#0  1 
#14 23 
sliccd_single_unmatched <- 
  sliccd_unmatched %>% filter(multiple_pregnancy==0)

n <- nrow(sliccd_single_unmatched )
exposed_any_asm <- NA
Outcome <- "SLICCD only"

sliccd_n <- cbind(exposed_any_asm, Outcome, n)

all_outcomes <- rbind(all_outcomes, sliccd_n)

write.csv(all_outcomes, paste0(folder_data_path, "Descriptives/table_linkage.csv" ))

#only 14 unlinked singletons