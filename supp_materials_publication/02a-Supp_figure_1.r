##Supplementary figure S1 (old figure 3)

library(dplyr)
library(stringr)
library(arrow)
library(phsmethods)
library(lubridate)
library(ggplot2)
library(tidyr)
library(Hmisc)
##

source("supp_materials_publication/00.supp_setup.r")

##data####
master_dataset_file <- readRDS(paste0(folder_data_path, "linkage/master_dataset_file.rds")) %>%
  filter(pregnancy_loss=="No" | pregnancy_loss=="Yes")
  
exposed <- master_dataset_file %>% filter(exposed_any_asm==1)

##Figure 2   ##### 
#Distribution of exposures by year dispensed
#table(year(exposed$first_exposed_any_asm))

exposed <- exposed %>% mutate(year_of_exposure = year(first_exposed_any_asm))

exposed<- exposed %>%
  mutate(asm_group2 = case_when(exposed_carbamazepine_mono ==1 ~ "Carbamazepine",
                                exposed_lamotrigine_mono==1 ~"Lamotrigine", 
                                exposed_levetiracetam_mono==1~ "Levetiracetam", 
                                exposed_topiramate_mono==1~ "Topiramate", 
                                exposed_valproate_mono == 1~"Valproate", 
                                exposed_gabapentin_mono== 1~"Gabapentin", 
                                exposed_pregabalin_mono== 1~"Pregabalin", 
                                exposed_any_asm==1 ~"OTHER")) %>% filter(asm_group2!="OTHER")


exposed_any <- master_dataset_file %>% filter(exposed_any_asm==1) %>%
  mutate(year_of_exposure = year(first_exposed_any_asm)) %>%
  mutate(asm_group2 = "Any ASM")

all_exposed <- rbind(exposed, exposed_any)

ggplot(all_exposed  , aes(x=gest_first_exposed)) +
  geom_bar() +
  facet_wrap(vars(asm_group2))+
  xlab("Gestation (week) first exposed") + 
  ylab("N prengnancies") +
  labs(fill='ASM exposure')+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

saveRDS(all_exposed, paste0(folder_data_path, "supplementary_materials/FigS1_data.rds"))
fig3 <- ggplot(all_exposed  , aes(x=gest_first_exposed)) +
  geom_bar() +
  facet_wrap(vars(asm_group2))+
  xlab("Gestation (week) first exposed") + 
  ylab("N prengnancies") +
  labs(fill='ASM exposure')+
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
tiff(paste0(folder_data_path, "supplementary_materials/FigureS1.jpeg"),units="in", width=5, height=4, res=300)
fig3
dev.off()

