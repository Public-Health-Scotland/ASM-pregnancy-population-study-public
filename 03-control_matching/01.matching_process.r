##01.matcning_process.r
## matching process script for main, secondary, and sensitivity stat analyses


###matching on year of conception,
#first stratifying to ensure controls selected only from those that reach gestation of exposure
##input is df , which should be filtered on any other limitations first 
## eg cohort memebership, epilepsy only .
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

#testing <- gest_splitter( gest_limit=10, data=df_min, outcome = "congenital")

####data prep####

df_min <- df %>% select(pregnancy_id, exposed_any_asm, CC_exposed_any_asm, gest_first_exposed,
                        year_conception, gest_end_pregnancy)


gests <- as.list(-2:44)
df_list <- lapply(gests, gest_splitter, df_min, outcome=outcome)

#remove any blanks
df_list_nomissing <- df_list[lapply(df_list,length)>0]

##matching function
match_condition <- function(x, outcome){
  if(outcome !="congenital"){
  matchit(exposed_any_asm ~ year_conception  ,
          data = x,
          exact = ~ year_conception,
          ratio = 10, replace = TRUE, verbose=TRUE)
  }else{
  if(outcome =="congenital"){
    matchit(CC_exposed_any_asm ~ year_conception  ,
            data = x,
            exact = ~ year_conception,
            ratio = 10, replace = TRUE, verbose=TRUE)
  }
  }
  }
#run matching on year of conception on each subset####
m_obj_exact <- lapply(df_list_nomissing ,
                      match_condition, outcome=outcome)

##get the matched rows
m_data_exact <- lapply(seq_along(df_list_nomissing), function(i) {
  get_matches(m_obj_exact[[i]], data = df_list_nomissing[[i]])
} 
)

for(i in 1:length(m_data_exact)){
  m_data_exact[[i]]$df_id <-i
}

matched_dataset1 <- bind_rows(m_data_exact)

##create unique group numbers for matched groups(subclass repeatd in each dataset in the list)
matched_dataset1 <- matched_dataset1 %>% 
  mutate(groupID = paste0(df_id,str_pad(subclass, width = 4, pad="0"))) %>%
  select(-c(id, weights,subclass))

##link to full dataframe
matched_dataset1_full <- matched_dataset1 %>% left_join(df)
