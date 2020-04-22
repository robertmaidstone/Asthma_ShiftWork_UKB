
source("load_data.R")

# ORmodelrun function -----------------------------------------------------

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

ORmodelrun<-function(model_data,DependentVar,model_vec,model_names){
  model_vec<-paste(DependentVar,"~",model_vec)
  
  model_data %>%
    dplyr::select(DependentVar,JiNS) %>% table(useNA = "always") %>%
    as.data.frame() -> tab_1
  eval(parse(text=paste0("tab_1 %>% filter(!is.na(",DependentVar,"),!is.na(JiNS)) -> tab_1")))
  tab_1 %>%
    spread(DependentVar,Freq) %>% 
    filter(JiNS != "Prefer not to answer")%>% 
    filter(JiNS != "Do not know") -> tab_1
  
  model_list <-list()
  for(i in 1:length(model_vec)){
    modeli<-model_vec[i]
    model_name_i <- model_names[i]
    
    model_data %>%
      filter(!is.na(JiNS)) %>%
      filter(!is.na(DependentVar)) %>%
      filter(JiNS != "Do not know")%>%
      filter(JiNS != "Prefer not to answer") %>%
      glm(data = .,modeli,family = binomial(link="logit")) -> mod3
    
    model_list[[i]] <- mod3
    
    exp(cbind(coef(mod3)[2:4],confint.default(mod3,2:4))) %>%
      as_tibble(rownames="row") %>% 
      round_df(2) %>%
      mutate(OR=paste0(V1," (",`2.5 %`,"-",`97.5 %`,")")) -> kk_temp
    eval(parse(text=paste0("kk_temp %>% dplyr::select(row,\"",model_name_i,"\"=OR)-> kk_temp")))
    kk_temp %>%
      mutate(row=(strsplit(row,split = "JiNS") %>% unlist %>% .[(1:3)*2])) -> kk_temp
    
    tab_1<-merge(tab_1,kk_temp,by.x="JiNS",by.y="row",all = T)
  }
  return(list(tab_1,model_list))
}

# common code -------------------------------------------------------------

nCI_code_cols <- paste("X20002.0.",0:32,sep="")
med_code_cols <- paste("X20003.0.",0:47,sep="")
Job_SOC_cols <- paste("X22617.0.",0:39,sep="")

medlist %>% dplyr::select(Code_description) %>%
  unlist %>%
  lapply(function(x){strsplit(as.character(x),split = " ")[[1]][1]}) %>%
  unlist %>%
  paste0(",",.,",",collapse="|") -> patterns_all

medlist %>% 
  filter(Relevance=="mod/severe") %>%
  dplyr::select(Code_description) %>%
  unlist %>%
  lapply(function(x){strsplit(as.character(x),split = " ")[[1]][1]}) %>%
  unlist %>%
  paste0(",",.,",",collapse="|") -> patterns_ms

ukb_data_processed %>% 
  as_tibble %>%
  dplyr::rename(Age_started_prev=X2867.0.0,
                Age_stopped_prev=X2897.0.0,
                Age_started_curr=X3436.0.0,
                Numperday_curr=X3456.0.0,
                Numperday_prev=X2887.0.0
  ) %>%
  mutate(Age=2008-Year_of_birth) %>%
  mutate(JiNS=ifelse(Job_involves_shift_work_int=="Never/rarely","No shift work",Job_involves_night_shift_work_int))%>%
  mutate(Packyears_prev=(Age_stopped_prev-Age_started_prev)*Numperday_prev/20) %>%
  mutate(Packyears_curr=(Age-Age_started_curr)*Numperday_curr/20) %>%
  mutate(Packyears=ifelse(is.na(Packyears_prev),Packyears_curr,Packyears_prev)) %>%
  dplyr::select(Asthma_int,X2316.0.0,JiNS,Sex,Year_of_birth,Smoking=X20116.0.0,Alcohol=X20117.0.0,
                Ethnicity=X21000.0.0,TDI=X189.0.0,BMI=X21001.0.0,Asthma_age_diagnosed_int,X20154.0.0,
                SleepDur=X1160.0.0,
                DaysWalked=X864.0.0,DaysModerate=X884.0.0,DaysVigorous=X904.0.0,
                med_code_cols,
                Job_SOC,
                nCI_code,
                Packyears,Chronotype=X1180.0.0,Packyears,
                LengthofWW=X767.0.0,
                Alcintake=X1558.0.0,
                Job_AsthmaRisk,
                Job_MedRequired) %>%
  mutate(Hypertension=grepl(",1065,",nCI_code)) %>%
  mutate(High_Cholesterol=grepl(",1473,",nCI_code)) %>%
  mutate(Sleep_Apnoea=grepl(",1123",nCI_code)) %>%
  mutate(COPD=grepl(",1112",nCI_code)) %>%
  mutate(Emp_Chron_Bronchitis=grepl(",1113",nCI_code)) %>%
  mutate(Bronchiectasis=grepl(",1114",nCI_code)) %>%
  mutate(Inter_lung_disease=grepl(",1115",nCI_code)) %>%
  mutate(Other_resp_probs=grepl(",1117",nCI_code)) %>%
  mutate(GastroOesReflux=grepl(",1138",nCI_code)) %>%
  mutate(Asthma2=grepl(",1111,",nCI_code)) %>%
  unite(col=med_code,med_code_cols,sep=",",remove=TRUE) %>%
  mutate(med_code=paste0(",",med_code,",")) %>%
  mutate(Asthma_med_all=(grepl(patterns_all,med_code))) %>%
  mutate(Asthma_med_ms=(grepl(patterns_ms,med_code))) %>%
  mutate(Asthma_def=Asthma_med_all & Asthma2) %>%
  mutate(Asthma_def_ms=Asthma_med_ms & Asthma2) %>%
  mutate(FEV1lt80=X20154.0.0<80) %>%
  mutate(JiNS=factor(JiNS,levels=c("No shift work","Never/rarely","Sometimes","Usually","Always"))) %>%
  mutate(Smoking=factor(Smoking,levels=c(0,-3,1,2))) %>%
  mutate(Alcohol=factor(Alcohol,levels=c(0,-3,1,2))) %>% 
  mutate(Ethnicity=factor(Ethnicity,levels=c(1,sort(unique(Ethnicity))[-3]))) %>%
  mutate(JiNS=plyr::revalue(JiNS, c("Always"="Always","Usually"="Sometimes","Sometimes"="Sometimes"))) -> model_data

# model_data %>%
#   filter(#Hypertension==T,
#     #High_Cholesterol==T,
#     #Sleep_Apnoea==F,
#     COPD==F,
#     Emp_Chron_Bronchitis==F#,
#     #Bronchiectasis==F,
#     #Inter_lung_disease==F,
#     #Other_resp_probs==F#,
#     #GastroOesReflux==F
#   ) -> model_data

# running -----------------------------------------------------------------

model_vec<-c("JiNS + Sex + Year_of_birth",
             #"JiNS + Sex + Year_of_birth + TDI + SleepDur + Packyears + Alcintake + LengthofWW",
             "JiNS + Sex + Year_of_birth + Smoking + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous + BMI + Packyears + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired",
             "JiNS + Sex + Year_of_birth + Smoking + Alcohol + Ethnicity + TDI + SleepDur + DaysWalked + DaysModerate + DaysVigorous + BMI + Packyears + Alcintake + Chronotype + LengthofWW + Job_AsthmaRisk + Job_MedRequired")
model_names <- c("Model 1: Age and Sex adjusted OR (95% CI)",
                 #"Model 2: Multivariate adjusted OR (95% CI)",
                 "Model 2: Multivariable adjusted OR (95% CI)",
                 "Model 3: Model 2 covariates + Sleep Duration (95% CI)")


model_data %>% filter(X2316.0.0 %in% c(0,1)) -> model_data_temp
DependentVar <- "X2316.0.0"
ORmodelrun(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> table3_WWJiNS

DependentVar <- "FEV1lt80"
ORmodelrun(model_data,DependentVar,model_vec,model_names)[[1]] -> table4_FEVJiNS


model_data %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def==TRUE)) -> model_data_temp
DependentVar <- "Asthma_def"
ORmodelrun(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab6asthmadef

##

model_data %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def_ms==TRUE)) -> model_data_temp
DependentVar <- "Asthma_def_ms"
ORmodelrun(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms


model_vec<-c("JiNS + Sex + Year_of_birth",
             #"JiNS + Sex + Year_of_birth + TDI + SleepDur + Packyears + Alcintake + LengthofWW",
             "JiNS + Sex + Year_of_birth + Smoking + Alcohol + Ethnicity + TDI + DaysWalked + DaysModerate + DaysVigorous + BMI + Packyears + Alcintake + LengthofWW + Job_AsthmaRisk + Job_MedRequired",
             "JiNS + Sex + Year_of_birth + Smoking + Alcohol + Ethnicity + TDI + SleepDur + DaysWalked + DaysModerate + DaysVigorous + BMI + Packyears + Alcintake + LengthofWW + Job_AsthmaRisk + Job_MedRequired")


model_data %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def_ms==TRUE)) %>%
  filter(Chronotype==1) -> model_data_temp
DependentVar <- "Asthma_def_ms"
ORmodelrun(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_morn

model_data %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def_ms==TRUE)) %>%
  filter(Chronotype==4) -> model_data_temp
DependentVar <- "Asthma_def_ms"
ORmodelrun(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_eve

model_data %>% filter((Asthma_med_all == FALSE&Asthma2==FALSE) |
                        (Asthma_def_ms==TRUE)) %>%
  filter(Chronotype%in% c(2,3)) -> model_data_temp
DependentVar <- "Asthma_def_ms"
ORmodelrun(model_data_temp,DependentVar,model_vec,model_names)[[1]] -> tab7asthmadefms_inter

##

save(table3_WWJiNS,table4_FEVJiNS,tab6asthmadef,tab7asthmadefms,tab7asthmadefms_morn,tab7asthmadefms_eve,tab7asthmadefms_inter,
     file="data/ORtablesdata.RData")

