
library(lubridate)

source("load_data.R")

rd <- 2 # digits to round to
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")

ukb_data_processed %>% 
  as_tibble %>%
  mutate(JiNS=ifelse(Job_involves_shift_work_int=="Never/rarely","No shift work",Job_involves_night_shift_work_int))%>%
  filter(!(JiNS %in% c(NA,"Do not know","Prefer not to answer"))) %>%
  dplyr::rename(Age_started_prev=X2867.0.0,
                Age_stopped_prev=X2897.0.0,
                Age_started_curr=X3436.0.0,
                Numperday_curr=X3456.0.0,
                Numperday_prev=X2887.0.0,
                EntryDate=X53.0.0
  ) %>%
  #mutate(Age=2008-Year_of_birth) %>%
  mutate(Age=year(as.Date(EntryDate))-year(as.Date(paste0(Year_of_birth,"-01-01")))) %>%
  mutate(Packyears_prev=(Age_stopped_prev-Age_started_prev)*Numperday_prev/20) %>%
  mutate(Packyears_curr=(Age-Age_started_curr)*Numperday_curr/20) %>%
  mutate(Packyears=ifelse(is.na(Packyears_prev),Packyears_curr,Packyears_prev)) %>%
  dplyr::select(Asthma_int,X2316.0.0,JiNS,Sex,Year_of_birth,Smoking=X20116.0.0,Alcohol=X20117.0.0,
                Ethnicity=X21000.0.0,TDI=X189.0.0,BMI=X21001.0.0,Asthma_age_diagnosed_int,X20154.0.0,
                SleepDur=X1160.0.0,
                DaysWalked=X864.0.0,DaysModerate=X884.0.0,DaysVigorous=X904.0.0,
                nCI_code,
                Job_SOC,
                Chronotype=X1180.0.0,Packyears,Packyears_curr,Packyears_prev,
                LengthofWW=X767.0.0,NuminHouse=X709.0.0,MatSmoking=X1787.0.0,Breastfed=X1677.0.0,
                Alcintake=X1558.0.0,Birthweight=X20022.0.0,UrbanRural=X20118.0.0,Job_AsthmaRisk,
                Job_MedRequired) %>%
  mutate(Hypertension=grepl(",1065,",nCI_code)) %>%
  mutate(High_Cholesterol=grepl(",1473,",nCI_code)) %>%
  mutate(Sleep_Apnoea=grepl(",1123",nCI_code)) %>%
  mutate(COPD=grepl(",1112",nCI_code)) %>%
  mutate(Emp_Chron_Bronchitis=grepl(",1113",nCI_code)) %>%
  mutate(COPDorEmpChronBron=COPD|Emp_Chron_Bronchitis) %>%
  mutate(Bronchiectasis=grepl(",1114",nCI_code)) %>%
  mutate(Inter_lung_disease=grepl(",1115",nCI_code)) %>%
  mutate(Other_resp_probs=grepl(",1117",nCI_code)) %>%
  mutate(GastroOesReflux=grepl(",1138",nCI_code)) %>%
  mutate(JG1=grepl(",1",Job_SOC)) %>%
  mutate(JG2=grepl(",2",Job_SOC)) %>%
  mutate(JG3=grepl(",3",Job_SOC)) %>%
  mutate(JG4=grepl(",4",Job_SOC)) %>%
  mutate(JG5=grepl(",5",Job_SOC)) %>%
  mutate(JG6=grepl(",6",Job_SOC)) %>%
  mutate(JG7=grepl(",7",Job_SOC)) %>%
  mutate(JG8=grepl(",8",Job_SOC)) %>%
  mutate(JG9=grepl(",9",Job_SOC)) %>%
  mutate(JiNS=plyr::revalue(JiNS, c("Always"="Always","Usually"="Sometimes","Sometimes"="Sometimes"))) -> ukb_data_filtered


ukb_data_filtered %>% 
  group_by(JiNS) %>%
  dplyr::summarise(N=n(),
            `Age (years)`=paste0(round(mean(2008-Year_of_birth),rd)," (",round(sd(2008-Year_of_birth),rd),")"), #age at 2008 (midpoint of study)
            `Sex (% male)`=round(sum(Sex)/n()*100,rd),
            `BMI (kg/m^2)` = paste0(round(mean(BMI,na.rm=T),rd)," (",round(sd(BMI,na.rm=T),rd),")"),
            `Never`=round(sum(Smoking==0,na.rm=T)/sum(!is.na(Smoking))*100,rd),
            `Previous`=round(sum(Smoking==1,na.rm=T)/sum(!is.na(Smoking))*100,rd),
            `Current`=round(sum(Smoking==2,na.rm=T)/sum(!is.na(Smoking))*100,rd),
            `Smoking pack-years`=paste0(round(mean(Packyears,na.rm=T),rd)," (",round(sd(Packyears,na.rm=T),rd),")"),
            #`Packyears_curr`=paste0(round(mean(Packyears_curr,na.rm=T),rd)," (",round(sd(Packyears_curr,na.rm=T),rd),")"),
            #`Packyears_prev`=paste0(round(mean(Packyears_prev,na.rm=T),rd)," (",round(sd(Packyears_prev,na.rm=T),rd),")"),
            ##
            #`Not Alcohol`=round(sum(Alcohol==0,na.rm=T)/sum(!is.na(Alcohol))*100,rd),
            `Daily alcohol intake (%)`=round(sum(Alcintake==1,na.rm=T)/sum(!is.na(Alcintake))*100,rd),
            ##
            `Sleep Duration (h)`=paste0(round(mean(SleepDur,na.rm=T),rd)," (",round(sd(SleepDur,na.rm=T),rd),")"),
            `Morning Chronotype (%)`=round(sum(Chronotype%in%c(1))/sum(!is.na(Chronotype))*100,rd),
            `Evening Chronotype (%)`=round(sum(Chronotype%in%c(4))/sum(!is.na(Chronotype))*100,rd),
            ##
            `White British`=round(sum(Ethnicity%in%c(1001),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
            `White Other`=round(sum(Ethnicity%in%c(1,1002,1003),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
            `Mixed`=round(sum(Ethnicity%in%c(2,2001,2002,2003,2004),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
            `Asian`=round(sum(Ethnicity%in%c(3,3001,3002,3003,3004),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
            `Black`=round(sum(Ethnicity%in%c(4,4001,4002,4003),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
            `Chinese`=round(sum(Ethnicity%in%c(5),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
            `Other`=round(sum(Ethnicity%in%c(1),na.rm=T)/sum(!is.na(Ethnicity))*100,rd),
            ##
            `Weekly work hours`=paste0(round(mean(LengthofWW,na.rm=T),rd)," (",round(sd(LengthofWW,na.rm=T),rd),")"),
            `Job Asthma Risk (%)`=round(sum(Job_AsthmaRisk,na.rm=T)/sum(!is.na(Job_AsthmaRisk))*100,rd),
            `Job Medical Required (%)`=round(sum(Job_MedRequired,na.rm=T)/sum(!is.na(Job_MedRequired))*100,rd),
            ##
            `Single Occupancy (%)`=round(sum(NuminHouse==1,na.rm=T)/sum(!is.na(NuminHouse))*100,rd),
            `Urban area (%)`=round(sum(UrbanRural%in%c(1,5,11,12),na.rm=T)/sum(!is.na(UrbanRural))*100,rd),
            `Townsend Index`=paste0(round(median(TDI,na.rm=T),2)," (",paste0(round(quantile(TDI,c(0.25,0.75),na.rm=T),2),collapse = " to "),")"),
            ##
            `Maternal Smoking (%)`=round(sum(MatSmoking==1,na.rm=T)/sum(!is.na(MatSmoking))*100,rd),
            `Breastfed as baby (%)`=round(sum(Breastfed==1,na.rm=T)/sum(!is.na(Breastfed))*100,rd),
            `Birth Weight (kg)`=paste0(round(mean(Birthweight,na.rm=T),rd)," (",round(sd(Birthweight,na.rm=T),rd),")"),
            ##
            `Hypertension (%)`=round(sum(Hypertension)/n()*100,rd),
            `High Cholesterol (%)`=round(sum(High_Cholesterol)/n()*100,rd),
            `Sleep Apnoea (%)`=round(sum(Sleep_Apnoea)/n()*100,rd),
            #`Chronic Obstructive Airways Disease/COPD`=round(sum(COPD)/n()*100,rd),
            #`Emphysema/Chronic Bronchitis`=round(sum(Emp_Chron_Bronchitis)/n()*100,rd),
            `Chronic Obstructive Airways Disease(COPD)/Emphysema/Chronic Bronchitis (%)`=round(sum(COPDorEmpChronBron)/n()*100,rd),
            `Bronchiectasis (%)`=round(sum(Bronchiectasis)/n()*100,rd),
            `Interstitial Lung Disease (%)`=round(sum(Inter_lung_disease)/n()*100,rd),
            `Other Respiratory Problems (%)`=round(sum(Other_resp_probs)/n()*100,rd),
            `Gastro-Oesophageal Reflux (%)`=round(sum(GastroOesReflux)/n()*100,rd)
            ) -> tab1_data

save(tab1_data,file="data/table1.RData")

