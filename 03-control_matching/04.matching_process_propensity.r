##04.matching_process_propensity.r
## script for matching process for propensity score analyses


###Propensity score matching###
#first stratifying to ensure controls selected only from those that reach gestation of exposure
##input is df , which should be filtered on any other limitations first 
## eg cohort membership, epilepsy only .

##
###Step 1: compute prop score on whole dataset
if(outcome =="pregnancy"){
p.score <- 
    fitted(glm(exposed_any_asm ~
                 year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
                 drug_alcohol_use+any_smr_comorb+
                 maternal_age_group_conception+ mother_simd , data = df, family = binomial))
}else{
  if(outcome =="congenital"){
    p.score <- 
      fitted(glm(CC_exposed_any_asm ~
                   year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
                   drug_alcohol_use+any_smr_comorb+
                   maternal_age_group_conception+ mother_simd , data = df, family = binomial))
  }
  else {
    if(outcome =="developmental")  
      p.score <- 
        fitted(glm(exposed_any_asm ~
                     year_conception +epilepsy_indication + mh_flag + migraine_pain_flag +
                     drug_alcohol_use+any_smr_comorb+
                     maternal_age_group_conception+ mother_simd + maternal_bmi_group +maternal_smoking +parity,
                   data = df, family = binomial))
  }
}


##Step 2 add th p.score to the df
df <- df %>% #filter(year_conception==2011) %>%
  filter(!is.na(maternal_age_group_conception) & !is.na(mother_simd))
##nb no p.score computed if NA values 
df$p.score <- p.score

##step 3: use gest splitter to generate all the dataframes for the matching.

#splitting function#####
gest_splitter <- function( gest_limit, data, outcome){
  #     data <- df
  #  gest_limit =44
  if(outcome !="congenital"){
    check <-  data  %>% filter(gest_first_exposed ==gest_limit)
  }else{
    if(outcome =="congenital"){
      check <-  data  %>% filter(CC_exposed_any_asm==1 & gest_first_exposed ==gest_limit)
    }
  }
  if(outcome !="congenital" & nrow(check)!=0){
    df1 <- data  %>% filter(gest_first_exposed ==gest_limit |
                              (exposed_any_asm==0 & gest_end_pregnancy>=gest_limit) )
    df1
  }else{
    if(outcome =="congenital" & nrow(check)!=0){
      
      df1 <- data  %>% filter(CC_exposed_any_asm==1 & gest_first_exposed ==gest_limit |
                                (CC_exposed_any_asm==0 & gest_end_pregnancy>=gest_limit) )
      df1
      
    }
  }
  
  # df1
}

####data prep####
###select variables for matching, and also retain baby_sex (needed for the dev outcomes) and folate
if(outcome =="developmental"){
  df_min <- df %>% select(pregnancy_id, mother_upi, exposed_any_asm, CC_exposed_any_asm, 
                        gest_first_exposed, drug_alcohol_use, any_smr_comorb,parity,
                        year_conception, gest_end_pregnancy, epilepsy_indication, mh_flag, migraine_pain_flag, 
                        maternal_age_group_conception, mother_simd, baby_sex, maternal_bmi_group,
                        maternal_smoking, high_dose_folic_acid, parity, p.score)
}else{
  if(outcome !="developmental"){
    df_min <- df %>% select(pregnancy_id,mother_upi, exposed_any_asm, CC_exposed_any_asm, 
                            gest_first_exposed, drug_alcohol_use, any_smr_comorb,
                            year_conception, gest_end_pregnancy, epilepsy_indication, mh_flag, migraine_pain_flag, 
                            maternal_age_group_conception, mother_simd, baby_sex, high_dose_folic_acid, p.score) 
  
  }
}

gests <- as.list(-2:44)
df_list <- lapply(gests, gest_splitter, df_min, outcome=outcome)

#remove any blanks
df_list_nomissing <- df_list[lapply(df_list,length)>0]

###Step4 - now we have a choice -
#either make the split into a giant df  and run matchit once (i tihnk this will prob be faster)
##

group_names <- paste0("gest_grp",1:length(df_list_nomissing))
names(df_list_nomissing) <-group_names
#test_list <- mapply(cbind, df_list_nomissing, "gest_group"=group_names, SIMPLIFY=F)
#df_list_nomissing <- mapply(cbind, df_list_nomissing, "gest_group"=group_names, SIMPLIFY=F)
#bind_rows is simpler and adds the names as a column using .id
check_df <- bind_rows(df_list_nomissing, .id = "gest_group")

if(outcome =="pregnancy"){
  print("pregnancy outcome matching")
m.out <- matchit(exposed_any_asm ~
                     year_conception +epilepsy_indication + mh_flag + 
                   drug_alcohol_use+ any_smr_comorb+
                     migraine_pain_flag + maternal_age_group_conception+ mother_simd ,
                   data = check_df ,
                   distance = check_df$p.score, exact = "gest_group", anti_exact = ~mother_upi,
                   ratio = 1, replace = TRUE, verbose=TRUE)
}else{
  if(outcome =="congenital"){
    print("congenital matching")
m.out <- matchit(CC_exposed_any_asm ~
                   year_conception +epilepsy_indication + mh_flag +  drug_alcohol_use+
                   any_smr_comorb+
                   migraine_pain_flag + maternal_age_group_conception+ mother_simd ,
        data = check_df ,
        distance = check_df$p.score, exact = "gest_group",anti_exact = ~mother_upi,
        ratio = 1, replace = TRUE, verbose=TRUE) 
  }else{
if(outcome =="developmental"){
  print("developmental matching")
      m.out <- matchit(exposed_any_asm ~
                         year_conception +epilepsy_indication + mh_flag + 
                         drug_alcohol_use+ any_smr_comorb+
                         parity+
                         migraine_pain_flag + maternal_age_group_conception+ mother_simd+
                        maternal_bmi_group +maternal_smoking,
                       data = check_df ,
                       distance = check_df$p.score, exact = "gest_group",anti_exact = ~mother_upi,
                       ratio = 1, replace = TRUE, verbose=TRUE)
    
  }
  }
}

###ok thats quite quick as weights alreay done and takes 20k mmeory to this point on all asm data

matched_df <- get_matches(m.out)
exp_flags <- df %>% select(pregnancy_id, exposed_carbamazepine_mono, exposed_lamotrigine_mono, 
                           exposed_levetiracetam_mono, exposed_topiramate_mono,
                           exposed_valproate_mono, 
                           exposed_gabapentin_mono, exposed_pregabalin_mono, 
                           CC_exposed_carbamazepine_mono, CC_exposed_gabapentin_mono, 
                           CC_exposed_lamotrigine_mono, CC_exposed_levetiracetam_mono, 
                           CC_exposed_topiramate_mono, CC_exposed_pregabalin_mono, 
                           CC_exposed_valproate_mono)

matched_dataset1 <- matched_df  %>% left_join(exp_flags)
