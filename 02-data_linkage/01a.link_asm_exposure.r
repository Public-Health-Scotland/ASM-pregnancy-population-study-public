###01a.link_asm_exposure##
# Script to link ASMs dataset to pregnancy dataset and flag exsposure 
source("data_linkage/00.setup_expose.r")
##read in files
asm <- read_parquet(paste0(data_path, "scomed_extracts/full_data_file_asm.parquet"))
pregs <- readRDS(paste0(data_path, "SLiPBD_cohort_extract.rds"))

med_names <- c("carbamazepine", "lamotrigine", "levetiracetam", "topiramate" , "valproate", "all prescribing")
###Select variabels needed from the ASM

##ensure both forms of valproate in one group.
asm  <- asm %>%
  mutate(vtm_group_name = case_when(vtm_name=="sodium valproate"| vtm_name=="valproic acid" ~ "valproate", 
                                    vtm_name=="phenytoin"|vtm_name== "phenytoin sodium"~ "phenytoin", 
                                    vtm_name=="phenobarbital"  | vtm_name=="phenobarbital sodium"~"phenobarbital", 
                                    T~vtm_name)) %>% 
  mutate(supply_date = as.Date(der_presc_date_time))


names(asm)

asm  <- asm %>% select(all_chi, all_upi, all_dob, vtm_group_name,  der_presc_date_time, supply_date, 
                       vtm_name)

##create an any ASM group
all_asm <- asm %>% mutate(vtm_group_name = "all prescribing") 
asm <- rbind(asm, all_asm)

###minimal pregnancy details- start, end, and UPI. 
###once linked we can join the other details back on later
pregs <- pregs %>% select(pregnancy_id, mother_upi, est_date_conception, date_end_pregnancy) %>%
  group_by(pregnancy_id) %>% slice(1)  #take 1st row of multiple pregs, keep multiples for the descriptive stages


###flag supplied during####
##Find the all asm and unexposed groups first
#keep date first supplied during
#flag for prescribed <20 weeks, congenital conditions exposure
med <- "all prescribing"

  asm_df <- asm %>% filter(vtm_group_name== med)
  df <- left_join(pregs,asm_df, by= c("mother_upi"= "all_upi"))
  df <- df %>% mutate(prescribed_during =
                        case_when( supply_date >= (est_date_conception-28)  & 
                                     supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
    #separate flag for congenital conditions exposure as only counts up to 20 weeks
    #for this outcome
    mutate(CC_prescribed_during =
             case_when(supply_date >= (est_date_conception-28)  & 
                       supply_date < (est_date_conception +(18*7)) & 
                         supply_date <= (date_end_pregnancy+1)~1, T~0 )) 
  
   df <- df %>% group_by(mother_upi, pregnancy_id,  est_date_conception, date_end_pregnancy) %>% 
     summarise(prescribed_during = max_(prescribed_during), 
               CC_prescribed_during = max_(CC_prescribed_during)) %>% ungroup() 

table(df$prescribed_during, useNA="always")
table(df$CC_prescribed_during, useNA="always")
table(df$prescribed_during,df$CC_prescribed_during, useNA="always")

#separate cases and controls
controls <- df %>% filter(prescribed_during==0) %>% mutate(unexposed=1, CC_unexposed=1)

cases_all <- df %>% filter(prescribed_during==1)%>%
  mutate(exposed_any_asm=1) %>%
  mutate(CC_exposed_any_asm = case_when(CC_prescribed_during==1 ~ 1 ,T~0), 
         CC_unexposed = case_when(CC_prescribed_during==0 ~ 1 ,T~0)  )

##now find first date prescribed for the exposed
df <- left_join(cases_all,asm_df, by= c("mother_upi"= "all_upi"))
df <- df %>% mutate(prescribed_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  mutate(CC_prescribed_during =
           case_when(supply_date >= (est_date_conception-28)  & 
                      supply_date < (est_date_conception +(18*7)) & 
                      supply_date <= (date_end_pregnancy+1)~1, T~0 )) %>%
  filter(prescribed_during==1)

df <- df %>% group_by(mother_upi, pregnancy_id) %>%
  summarise(first_exposed_any_asm = min_(supply_date))

cases_all <- left_join(cases_all,df)
cases_all <- cases_all %>%
  mutate(CC_first_exposed_any_asm = case_when(CC_exposed_any_asm==1 ~ first_exposed_any_asm, T~NA))

##add flags for each of 5 medications ####
#select medication 
##Carbamazepine####
asm_df <- asm %>% filter(vtm_group_name== "carbamazepine") 
#only need to join to the all asm group this time around
df <- left_join(cases_all,  asm_df, by= c("mother_upi"= "all_upi"))
#flag prescribed, and select the first prescribed during date
df <- df %>% mutate(prescribed_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  filter(prescribed_during==1) %>% 
  group_by(mother_upi, pregnancy_id,  est_date_conception, date_end_pregnancy) %>%
  summarise(carbamazepine_first_exposed = min_(supply_date)) %>% ungroup() %>%
  mutate(exposed=1) %>%
  mutate(CC_exposed = case_when(carbamazepine_first_exposed >= (est_date_conception-28)  & 
                                  carbamazepine_first_exposed < (est_date_conception +(18*7)) & 
                                  carbamazepine_first_exposed <= (date_end_pregnancy+1)~1, T~0 )) %>% 
  mutate(weeks = (carbamazepine_first_exposed- est_date_conception)/7 +2 )
table(df$weeks, df$CC_exposed)

df <- df %>% select(-weeks)
##second step is to ID mono vs poly- therapy.
#take the carbemazipine group an join to asm file
# first excluding selected ASM and "any prescribing"
asm_df <- asm %>% filter(vtm_group_name!= "carbamazepine" & vtm_group_name!= "all prescribing") 
df_mono <- left_join(df, asm_df, by= c("mother_upi"= "all_upi"))
df_mono <- df_mono %>% mutate(prescribed_other_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  mutate(CC_prescribed_other_during =
           case_when(supply_date >= (est_date_conception-28)  & 
                       supply_date < (est_date_conception +(18*7)) & 
                       supply_date <= (date_end_pregnancy+1)~1, T~0 ))
#filter to exposed only
df_mono <- df_mono %>% group_by(mother_upi, pregnancy_id,  est_date_conception, exposed, CC_exposed) %>% 
  summarise(prescribed_other_during = max_(prescribed_other_during),
            CC_prescribed_other_during = max_(CC_prescribed_other_during) ) %>%
  ungroup()

df_mono <- df_mono %>% 
  mutate(exposed_carbamazepine_mono = case_when(exposed==1 & prescribed_other_during==0 ~1, 
                                                 T~0)) %>%
  mutate(exposed_carbamazepine_poly = case_when(exposed==1 & prescribed_other_during==1 ~1, 
                                                T~0)) %>%
  mutate(CC_exposed_carbamazepine_mono = case_when(CC_exposed==1 & CC_prescribed_other_during==0 ~1, 
                                                T~0)) %>%
  mutate(CC_exposed_carbamazepine_poly = case_when(CC_exposed==1 & CC_prescribed_other_during==1 ~1, 
                                                T~0)) %>%
  select(-c(prescribed_other_during, exposed, CC_exposed, CC_prescribed_other_during  ))

df_mono <- left_join(df_mono , df %>% select(mother_upi, pregnancy_id, carbamazepine_first_exposed))

table(df_mono$exposed_carbamazepine_mono)
table(df_mono$exposed_carbamazepine_poly)
cases_all<- left_join(cases_all, df_mono)

###Lamotrigine####
asm_df <- asm %>% filter(vtm_group_name== "lamotrigine") 
#only need to join to the all asm group this time around
df <- left_join(cases_all,  asm_df, by= c("mother_upi"= "all_upi"))
#flag prescribed, and select the first prescribed during date
df <- df %>% mutate(prescribed_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  filter(prescribed_during==1) %>% 
  group_by(mother_upi, pregnancy_id,  est_date_conception, date_end_pregnancy) %>%
  summarise(lamotrigine_first_exposed = min_(supply_date)) %>% ungroup() %>%
  mutate(exposed=1) %>%
  mutate(CC_exposed = case_when(lamotrigine_first_exposed >= (est_date_conception-28)  & 
                                  lamotrigine_first_exposed < (est_date_conception +(18*7)) & 
                                  lamotrigine_first_exposed <= (date_end_pregnancy+1)~1, T~0 )) %>% 
  mutate(weeks = (lamotrigine_first_exposed- est_date_conception)/7 +2 )
table(df$weeks, df$CC_exposed)

df <- df %>% select(-weeks)
##second step is to ID mono vs poly- therapy.
#take the carbemazipine group an join to asm file
# first excluding selected ASM and "any prescribing"
asm_df <- asm %>% filter(vtm_group_name!= "lamotrigine" & vtm_group_name!= "all prescribing") 
df_mono <- left_join(df, asm_df, by= c("mother_upi"= "all_upi"))
df_mono <- df_mono %>% mutate(prescribed_other_during =
                                case_when( supply_date >= (est_date_conception-28)  & 
                                             supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  mutate(CC_prescribed_other_during =
           case_when(supply_date >= (est_date_conception-28)  & 
                       supply_date < (est_date_conception +(18*7)) & 
                       supply_date <= (date_end_pregnancy+1)~1, T~0 ))
#filter to exposed only
df_mono <- df_mono %>% group_by(mother_upi, pregnancy_id,  est_date_conception, exposed, CC_exposed) %>% 
  summarise(prescribed_other_during = max_(prescribed_other_during),
            CC_prescribed_other_during = max_(CC_prescribed_other_during) ) %>%
  ungroup()

df_mono <- df_mono %>% 
  mutate(exposed_lamotrigine_mono = case_when(exposed==1 & prescribed_other_during==0 ~1, 
                                                T~0)) %>%
  mutate(exposed_lamotrigine_poly = case_when(exposed==1 & prescribed_other_during==1 ~1, 
                                                T~0)) %>%
  mutate(CC_exposed_lamotrigine_mono = case_when(CC_exposed==1 & CC_prescribed_other_during==0 ~1, 
                                                   T~0)) %>%
  mutate(CC_exposed_lamotrigine_poly = case_when(CC_exposed==1 & CC_prescribed_other_during==1 ~1, 
                                                   T~0)) %>%
  select(-c(prescribed_other_during, exposed, CC_exposed, CC_prescribed_other_during  ))

df_mono <- left_join(df_mono , df %>% select(mother_upi, pregnancy_id, lamotrigine_first_exposed))

#table(df_mono$exposed_lamotrigine_mono)
#table(df_mono$exposed_lamotrigine_poly)

cases_all<- left_join(cases_all, df_mono)

##
###levetiracetam####
asm_df <- asm %>% filter(vtm_group_name== "levetiracetam") 
#only need to join to the all asm group this time around
df <- left_join(cases_all,  asm_df, by= c("mother_upi"= "all_upi"))
#flag prescribed, and select the first prescribed during date
df <- df %>% mutate(prescribed_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  filter(prescribed_during==1) %>% 
  group_by(mother_upi, pregnancy_id,  est_date_conception, date_end_pregnancy) %>%
  summarise(levetiracetam_first_exposed = min_(supply_date)) %>% ungroup() %>%
  mutate(exposed=1) %>%
  mutate(CC_exposed = case_when(levetiracetam_first_exposed >= (est_date_conception-28)  & 
                                  levetiracetam_first_exposed < (est_date_conception +(18*7)) & 
                                  levetiracetam_first_exposed <= (date_end_pregnancy+1)~1, T~0 )) %>% 
  mutate(weeks = (levetiracetam_first_exposed- est_date_conception)/7 +2 )
table(df$weeks, df$CC_exposed)

df <- df %>% select(-weeks)
##second step is to ID mono vs poly- therapy.
#take the levetiracetam group an join to asm file
# first excluding selected ASM and "any prescribing"
asm_df <- asm %>% filter(vtm_group_name!= "levetiracetam" & vtm_group_name!= "all prescribing") 
df_mono <- left_join(df, asm_df, by= c("mother_upi"= "all_upi"))
df_mono <- df_mono %>% mutate(prescribed_other_during =
                                case_when( supply_date >= (est_date_conception-28)  & 
                                             supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  mutate(CC_prescribed_other_during =
           case_when(supply_date >= (est_date_conception-28)  & 
                       supply_date < (est_date_conception +(18*7)) & 
                       supply_date <= (date_end_pregnancy+1)~1, T~0 ))
#filter to exposed only
df_mono <- df_mono %>% group_by(mother_upi, pregnancy_id,  est_date_conception, exposed, CC_exposed) %>% 
  summarise(prescribed_other_during = max_(prescribed_other_during),
            CC_prescribed_other_during = max_(CC_prescribed_other_during) ) %>%
  ungroup()

df_mono <- df_mono %>% 
  mutate(exposed_levetiracetam_mono = case_when(exposed==1 & prescribed_other_during==0 ~1, 
                                              T~0)) %>%
  mutate(exposed_levetiracetam_poly = case_when(exposed==1 & prescribed_other_during==1 ~1, 
                                              T~0)) %>%
  mutate(CC_exposed_levetiracetam_mono = case_when(CC_exposed==1 & CC_prescribed_other_during==0 ~1, 
                                                 T~0)) %>%
  mutate(CC_exposed_levetiracetam_poly = case_when(CC_exposed==1 & CC_prescribed_other_during==1 ~1, 
                                                 T~0)) %>%
  select(-c(prescribed_other_during, exposed, CC_exposed, CC_prescribed_other_during  ))

df_mono <- left_join(df_mono , df %>% select(mother_upi, pregnancy_id, levetiracetam_first_exposed))

table(df_mono$exposed_levetiracetam_mono)
table(df_mono$exposed_levetiracetam_poly)


cases_all<- left_join(cases_all, df_mono)


##
###topiramate####
asm_df <- asm %>% filter(vtm_group_name== "topiramate") 
#only need to join to the all asm group this time around
df <- left_join(cases_all,  asm_df, by= c("mother_upi"= "all_upi"))
#flag prescribed, and select the first prescribed during date
df <- df %>% mutate(prescribed_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  filter(prescribed_during==1) %>% 
  group_by(mother_upi, pregnancy_id,  est_date_conception, date_end_pregnancy) %>%
  summarise(topiramate_first_exposed = min_(supply_date)) %>% ungroup() %>%
  mutate(exposed=1) %>%
  mutate(CC_exposed = case_when(topiramate_first_exposed >= (est_date_conception-28)  & 
                                  topiramate_first_exposed < (est_date_conception +(18*7)) & 
                                  topiramate_first_exposed <= (date_end_pregnancy+1)~1, T~0 )) %>% 
  mutate(weeks = (topiramate_first_exposed- est_date_conception)/7 +2 )
table(df$weeks, df$CC_exposed)

df <- df %>% select(-weeks)
##second step is to ID mono vs poly- therapy.
#take the carbemazipine group an join to asm file
# first excluding selected ASM and "any prescribing"
asm_df <- asm %>% filter(vtm_group_name!= "topiramate" & vtm_group_name!= "all prescribing") 
df_mono <- left_join(df, asm_df, by= c("mother_upi"= "all_upi"))
df_mono <- df_mono %>% mutate(prescribed_other_during =
                                case_when( supply_date >= (est_date_conception-28)  & 
                                             supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  mutate(CC_prescribed_other_during =
           case_when(supply_date >= (est_date_conception-28)  & 
                       supply_date < (est_date_conception +(18*7)) & 
                       supply_date <= (date_end_pregnancy+1)~1, T~0 ))
#filter to exposed only
df_mono <- df_mono %>% group_by(mother_upi, pregnancy_id,  est_date_conception, exposed, CC_exposed) %>% 
  summarise(prescribed_other_during = max_(prescribed_other_during),
            CC_prescribed_other_during = max_(CC_prescribed_other_during) ) %>%
  ungroup()

df_mono <- df_mono %>% 
  mutate(exposed_topiramate_mono = case_when(exposed==1 & prescribed_other_during==0 ~1, 
                                              T~0)) %>%
  mutate(exposed_topiramate_poly = case_when(exposed==1 & prescribed_other_during==1 ~1, 
                                              T~0)) %>%
  mutate(CC_exposed_topiramate_mono = case_when(CC_exposed==1 & CC_prescribed_other_during==0 ~1, 
                                                 T~0)) %>%
  mutate(CC_exposed_topiramate_poly = case_when(CC_exposed==1 & CC_prescribed_other_during==1 ~1, 
                                                 T~0)) %>%
  select(-c(prescribed_other_during, exposed, CC_exposed, CC_prescribed_other_during  ))

df_mono <- left_join(df_mono , df %>% select(mother_upi, pregnancy_id, topiramate_first_exposed))

table(df_mono$exposed_topiramate_mono)
table(df_mono$exposed_topiramate_poly)
cases_all<- left_join(cases_all, df_mono)


###Valproate####
asm_df <- asm %>% filter(vtm_group_name== "valproate") 
#only need to join to the all asm group this time around
df <- left_join(cases_all,  asm_df, by= c("mother_upi"= "all_upi"))
#flag prescribed, and select the first prescribed during date
df <- df %>% mutate(prescribed_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  filter(prescribed_during==1) %>% 
  group_by(mother_upi, pregnancy_id,  est_date_conception, date_end_pregnancy) %>%
  summarise(valproate_first_exposed = min_(supply_date)) %>% ungroup() %>%
  mutate(exposed=1) %>%
  mutate(CC_exposed = case_when(valproate_first_exposed >= (est_date_conception-28)  & 
                                  valproate_first_exposed < (est_date_conception +(18*7)) & 
                                  valproate_first_exposed <= (date_end_pregnancy+1)~1, T~0 )) %>% 
  mutate(weeks = (valproate_first_exposed- est_date_conception)/7 +2 )
table(df$weeks, df$CC_exposed)

df <- df %>% select(-weeks)
##second step is to ID mono vs poly- therapy.
#take the carbemazipine group an join to asm file
# first excluding selected ASM and "any prescribing"
asm_df <- asm %>% filter(vtm_group_name!= "valproate" & vtm_group_name!= "all prescribing") 
df_mono <- left_join(df, asm_df, by= c("mother_upi"= "all_upi"))
df_mono <- df_mono %>% mutate(prescribed_other_during =
                                case_when( supply_date >= (est_date_conception-28)  & 
                                             supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  mutate(CC_prescribed_other_during =
           case_when(supply_date >= (est_date_conception-28)  & 
                       supply_date < (est_date_conception +(18*7)) & 
                       supply_date <= (date_end_pregnancy+1)~1, T~0 ))
#filter to exposed only
df_mono <- df_mono %>% group_by(mother_upi, pregnancy_id,  est_date_conception, exposed, CC_exposed) %>% 
  summarise(prescribed_other_during = max_(prescribed_other_during),
            CC_prescribed_other_during = max_(CC_prescribed_other_during) ) %>%
  ungroup()

df_mono <- df_mono %>% 
  mutate(exposed_valproate_mono = case_when(exposed==1 & prescribed_other_during==0 ~1, 
                                              T~0)) %>%
  mutate(exposed_valproate_poly = case_when(exposed==1 & prescribed_other_during==1 ~1, 
                                              T~0)) %>%
  mutate(CC_exposed_valproate_mono = case_when(CC_exposed==1 & CC_prescribed_other_during==0 ~1, 
                                                 T~0)) %>%
  mutate(CC_exposed_valproate_poly = case_when(CC_exposed==1 & CC_prescribed_other_during==1 ~1, 
                                                 T~0)) %>%
  select(-c(prescribed_other_during, exposed, CC_exposed, CC_prescribed_other_during  ))

df_mono <- left_join(df_mono , df %>% select(mother_upi, pregnancy_id, valproate_first_exposed))

table(df_mono$exposed_valproate_mono)
table(df_mono$exposed_valproate_poly)


cases_all<- left_join(cases_all, df_mono)

names(cases_all)


#Pregabalin ####
#select medication
asm_df <- asm %>% filter(vtm_group_name== "pregabalin") 
#only need to join to the all asm group this time around
df <- left_join(cases_all,  asm_df, by= c("mother_upi"= "all_upi"))
#flag prescribed, and select the first prescribed during date
df <- df %>% mutate(prescribed_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  filter(prescribed_during==1) %>% 
  group_by(mother_upi, pregnancy_id,  est_date_conception, date_end_pregnancy) %>%
  summarise(pregabalin_first_exposed = min_(supply_date)) %>% ungroup() %>%
  mutate(exposed=1) %>%
  mutate(CC_exposed = case_when(pregabalin_first_exposed >= (est_date_conception-28)  & 
                                  pregabalin_first_exposed < (est_date_conception +(18*7)) & 
                                  pregabalin_first_exposed <= (date_end_pregnancy+1)~1, T~0 )) %>% 
  mutate(weeks = (pregabalin_first_exposed- est_date_conception)/7 +2 )
table(df$weeks, df$CC_exposed)

df <- df %>% select(-weeks)
##second step is to ID mono vs poly- therapy.
#take the carbemazipine group an join to asm file
# first excluding selected ASM and "any prescribing"
asm_df <- asm %>% filter(vtm_group_name!= "pregabalin" & vtm_group_name!= "all prescribing") 
df_mono2 <- left_join(df, asm_df, by= c("mother_upi"= "all_upi"))
df_mono2 <- df_mono2 %>% mutate(prescribed_other_during =
                                  case_when( supply_date >= (est_date_conception-28)  & 
                                               supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  mutate(CC_prescribed_other_during =
           case_when(supply_date >= (est_date_conception-28)  & 
                       supply_date < (est_date_conception +(18*7)) & 
                       supply_date <= (date_end_pregnancy+1)~1, T~0 ))
#filter to exposed only
df_mono2 <- df_mono2 %>% group_by(mother_upi, pregnancy_id,  est_date_conception, exposed, CC_exposed) %>% 
  summarise(prescribed_other_during = max_(prescribed_other_during),
            CC_prescribed_other_during = max_(CC_prescribed_other_during) ) %>%
  ungroup()

df_mono2 <- df_mono2 %>% 
  mutate(exposed_pregabalin_mono = case_when(exposed==1 & prescribed_other_during==0 ~1, 
                                             T~0)) %>%
  mutate(exposed_pregabalin_poly = case_when(exposed==1 & prescribed_other_during==1 ~1, 
                                             T~0)) %>%
  mutate(CC_exposed_pregabalin_mono = case_when(CC_exposed==1 & CC_prescribed_other_during==0 ~1, 
                                                T~0)) %>%
  mutate(CC_exposed_pregabalin_poly = case_when(CC_exposed==1 & CC_prescribed_other_during==1 ~1, 
                                                T~0)) %>%
  select(-c(prescribed_other_during, exposed, CC_exposed, CC_prescribed_other_during  ))

df_mono2 <- left_join(df_mono2 , df %>% select(mother_upi, pregnancy_id, pregabalin_first_exposed))


cases_all<- left_join(cases_all, df_mono2)


#gabapentin ####
#select medication
asm_df <- asm %>% filter(vtm_group_name== "gabapentin") 
#only need to join to the all asm group this time around
df <- left_join(cases_all,  asm_df, by= c("mother_upi"= "all_upi"))
#flag prescribed, and select the first prescribed during date
df <- df %>% mutate(prescribed_during =
                      case_when( supply_date >= (est_date_conception-28)  & 
                                   supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  filter(prescribed_during==1) %>% 
  group_by(mother_upi, pregnancy_id,  est_date_conception, date_end_pregnancy) %>%
  summarise(gabapentin_first_exposed = min_(supply_date)) %>% ungroup() %>%
  mutate(exposed=1) %>%
  mutate(CC_exposed = case_when(gabapentin_first_exposed >= (est_date_conception-28)  & 
                                  gabapentin_first_exposed < (est_date_conception +(18*7)) & 
                                  gabapentin_first_exposed <= (date_end_pregnancy+1)~1, T~0 )) %>% 
  mutate(weeks = (gabapentin_first_exposed- est_date_conception)/7 +2 )
table(df$weeks, df$CC_exposed)

df <- df %>% select(-weeks)
##second step is to ID mono vs poly- therapy.
#take the carbemazipine group an join to asm file
# first excluding selected ASM and "any prescribing"
asm_df <- asm %>% filter(vtm_group_name!= "gabapentin" & vtm_group_name!= "all prescribing") 
df_mono2 <- left_join(df, asm_df, by= c("mother_upi"= "all_upi"))
df_mono2 <- df_mono2 %>% mutate(prescribed_other_during =
                                  case_when( supply_date >= (est_date_conception-28)  & 
                                               supply_date <= (date_end_pregnancy)~1, T~0 )) %>%
  mutate(CC_prescribed_other_during =
           case_when(supply_date >= (est_date_conception-28)  & 
                       supply_date < (est_date_conception +(18*7)) & 
                       supply_date <= (date_end_pregnancy+1)~1, T~0 ))
#filter to exposed only
df_mono2 <- df_mono2 %>% group_by(mother_upi, pregnancy_id,  est_date_conception, exposed, CC_exposed) %>% 
  summarise(prescribed_other_during = max_(prescribed_other_during),
            CC_prescribed_other_during = max_(CC_prescribed_other_during) ) %>%
  ungroup()

df_mono2 <- df_mono2 %>% 
  mutate(exposed_gabapentin_mono = case_when(exposed==1 & prescribed_other_during==0 ~1, 
                                             T~0)) %>%
  mutate(exposed_gabapentin_poly = case_when(exposed==1 & prescribed_other_during==1 ~1, 
                                             T~0)) %>%
  mutate(CC_exposed_gabapentin_mono = case_when(CC_exposed==1 & CC_prescribed_other_during==0 ~1, 
                                                T~0)) %>%
  mutate(CC_exposed_gabapentin_poly = case_when(CC_exposed==1 & CC_prescribed_other_during==1 ~1, 
                                                T~0)) %>%
  select(-c(prescribed_other_during, exposed, CC_exposed, CC_prescribed_other_during  ))

df_mono2 <- left_join(df_mono2 , df %>% select(mother_upi, pregnancy_id, gabapentin_first_exposed))


cases_all<- left_join(cases_all, df_mono2)


##Add controls back on#####
##code all exposed and unexposed to 0/1
asm_flags <- bind_rows(cases_all, controls)
names(asm_flags)

asm_flags <- asm_flags %>%
  mutate_if(is.numeric, ~tidyr::replace_na(., 0))
table(asm_flags$exposed_carbamazepine_mono)

table(asm_flags$exposed_lamotrigine_mono)
table(asm_flags$CC_exposed_lamotrigine_mono)
table(asm_flags$exposed_levetiracetam_mono)
table(asm_flags$CC_exposed_levetiracetam_mono)
table(asm_flags$exposed_topiramate_mono)
table(asm_flags$CC_exposed_topiramate_mono)
table(asm_flags$exposed_valproate_mono)
table(asm_flags$CC_exposed_valproate_mono)

table(asm_flags$exposed_pregabalin_mono)
table(asm_flags$CC_exposed_pregabalin_mono)
table(asm_flags$exposed_gabapentin_mono)
table(asm_flags$CC_exposed_gabapentin_mono)
###Save cases and control pool
asm_flags <- asm_flags %>% select(-c(prescribed_during, CC_prescribed_during))
saveRDS(asm_flags, paste0(data_path,"linkage/exposure_flags.rds" ))








##how many multiples have we got
#just out of interest, these will be removed later.
#and will record stats on those removed later as well
pregs <- readRDS(paste0(data_path, "SLiPBD_cohort_extract.rds"))

multiple_ind <- pregs %>% select(pregnancy_id,total_fetuses_this_pregnancy) %>%
  group_by(pregnancy_id) %>% slice(1)

check_multis <- left_join(asm_flags, multiple_ind)
table(check_multis$exposed_any_asm, check_multis$total_fetuses_this_pregnancy>1, useNA="always")
#we will lose ~1%
#( which is less than the % of multiples in live births, but we treat early losses as singltons as we have no info!)



