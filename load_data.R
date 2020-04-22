
# load packages -----------------------------------------------------------


library(tidyverse)


# load data ---------------------------------------------------------------

load("../Data/ukb_merged.RData")
ukb_data_processed <- ukb_merged 

rm(ukb_merged)

read.csv(file = "../Data/shrinemedicationlist.csv")  -> medlist
read.csv(file = "../Data/occupation_asthmarisk_v2.csv")  -> occ_asthma


# filtered jobs -----------------------------------------------------------

occ_asthma %>% as_tibble() %>%
  filter(Risk.for.Asthma=="Y" | Risk.for.Asthma=="M") %>%
  dplyr::select(Number) %>% unlist -> asthmarisk_occs

occ_asthma %>% as_tibble() %>%
  filter(Medical.Required=="Y") %>%
  dplyr::select(Number) %>% unlist -> medrequired_occs

# exclusion criteria ------------------------------------------------------

nCI_code_cols <- paste("X20002.0.",0:32,sep="")
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")

ukb_data_processed %>% 
  as_tibble %>%
  unite(col=nCI_code,nCI_code_cols,sep=",",remove=TRUE) %>%
  mutate(nCI_code=paste0(",",nCI_code,",")) %>%
  unite(col=Job_SOC,Job_SOC_cols,sep=",",remove=TRUE) %>%
  mutate(Job_SOC=paste0(",",Job_SOC,",")) %>%
  mutate(Job_AsthmaRisk=grepl(paste0(",",paste0(asthmarisk_occs,collapse = ",|,"),","),Job_SOC)) %>% 
  mutate(Job_MedRequired=grepl(paste0(",",paste0(medrequired_occs,collapse = ",|,"),","),Job_SOC)) %>% 
  as_tibble() -> ukb_data_processed

