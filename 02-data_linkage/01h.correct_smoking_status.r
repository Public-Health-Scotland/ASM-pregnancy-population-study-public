##01h-correct_smoking_status
# implement a fix to correct smoking status from smr02 data

source("data_linkage/00.setup_expose.r")

#read in slipbd and identifiers file#####
slipbd<- readRDS(paste0(slipbd_path2 ,"data/archive/slipbd_database.rds"))
dataset_identifiers <- readRDS(paste0(slipbd_path2 ,"data/archive/dataset_identifiers.rds"))
slipbd <- left_join(slipbd, dataset_identifiers)


##read in bookings and smr02
antenatal_booking <- readRDS(paste0(slipbd_path2 ,"data/historic/antenatal_booking.rds"))
smr02_data <- readRDS(paste0(slipbd_path2 ,"data/historic/smr02_data.rds"))

table(smr02_data$smr02_booking_smoking_history, year(smr02_data$smr02_admission_date), useNA="always")

join_slipbd <- left_join(slipbd, smr02_data)
join_slipbd <- left_join(join_slipbd, antenatal_booking, 
                         by= c("anbooking_mother_upi","antenatal_booking_date"= "anbooking_booking_date"  ))
names(antenatal_booking)
##Save corrected smoking status and IDs
join_slipbd <-join_slipbd  %>%
  mutate(anbooking_smoking_status = case_when(anbooking_smoking_status %in% c("9", "N", "3", "4") ~NA,
                                              T~anbooking_smoking_status)) %>%
  mutate(x_booking_smoking_status = coalesce(anbooking_smoking_status, as.character(smr02_booking_smoking_history))) %>% 
  mutate(x_booking_smoking_status= as.character(x_booking_smoking_status)) %>% 
  mutate(x_booking_smoking_status = case_when(x_booking_smoking_status== "0" ~ "non-smoker", 
                                              x_booking_smoking_status== "1" ~ "smoker", 
                                              x_booking_smoking_status== "2" ~ "ex-smoker",
                                              x_booking_smoking_status== "3" ~ NA,
                                              x_booking_smoking_status== "4" ~ NA, 
                                              T~NA)) 
table(join_slipbd$x_booking_smoking_status, join_slipbd$maternal_smoking, useNA="always")

table(join_slipbd$x_booking_smoking_status,  useNA="always")
table(join_slipbd$maternal_smoking,  useNA="always")

table(join_slipbd$x_booking_smoking_status,  useNA="always")


smoking_file <- join_slipbd %>% select(pregnancy_id, x_booking_smoking_status) %>% 
  group_by(pregnancy_id) %>% slice(1)

saveRDS(smoking_file, paste0(data_path, "linkage/new_smoking_flag.rds"))

SLiPBD_cohort_extract <- readRDS(paste0(data_path, "SLiPBD_cohort_extract.rds"))