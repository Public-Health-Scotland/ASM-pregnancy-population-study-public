###01b.link_folate##
# Script to link folic acid dataset to pregnancy dataset and flag folic acid status

library(hablar)
source("data_linkage/00.setup_expose.r")
meds <- read_parquet(paste0(data_path, "scomed_extracts/full_data_file_other_meds.parquet"))
pregs <- readRDS(paste0(data_path, "SLiPBD_cohort_extract.rds")) %>%
  select(mother_upi, pregnancy_id, est_date_conception)

table(meds$vtm_name)

# VS - removed filter for prescription not supplied != "y" and admin not supplied == "n" or blank as handled in the 04b-combine_extracts script
folic <- meds %>% filter(vtm_name == "folic acid")%>% 
  filter(der_supplied_date_time >= as.Date("2010-01-01")) %>%
    select(all_upi, vtm_name, der_supplied_date_time)

link <- left_join(pregs, folic, by=c("mother_upi" = "all_upi")) %>%
  mutate(supply_date = as.Date(der_supplied_date_time)) %>%
  mutate(day_diff = (supply_date - est_date_conception)) %>%
  mutate(folic_flag = case_when(day_diff <=69 & day_diff >= -(84) ~1, T~0))

folic_flags <- link %>% group_by(mother_upi, pregnancy_id, est_date_conception) %>%
  summarise(folic_flag = max_(folic_flag)) %>% ungroup()

table(folic_flags$folic_flag, lubridate::year(folic_flags$est_date_conception))

folic_flags <- folic_flags %>% select(-est_date_conception)
saveRDS(folic_flags, paste0(data_path,"linkage/folate_flags.rds"))
