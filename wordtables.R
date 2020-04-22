

# packages ----------------------------------------------------------------

library(officer)
library(flextable)
library(magrittr)
library(tidyverse)

# load data ---------------------------------------------------------------

load("data/ORtablesdata.RData")
load("data/table1.RData")

# table 1 -----------------------------------------------------------------

tab1_data %>%
  gather(Key,Value,-JiNS) %>%
  mutate(Key=factor(Key,levels=names(tab1_data))) %>%
  spread(JiNS,Value) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,Sometimes,Always)-> out_tab

out_tab[1,-1]  %>% as.numeric() %>% sum() -> N_t1

out_tab[1,-1] <- formatC(out_tab[1,-1],format="d",big.mark=",")

out_tab %>% mutate(Key=as.character(Key)) %>%
  add_row(Key="Smoker (%)", .before = which(out_tab$Key=="Never")) -> out_tab
out_tab %>%
  add_row(Key="Ethnicity (%)", .before = which(out_tab$Key=="White British")) -> out_tab

out_tab %>%
  filter(nchar(Key)>21) %>% dplyr::select(Key) %>% unlist -> long_keys

out_tab$Key[out_tab$Key%in%long_keys] <- c(
"Daily alcohol\nintake (%)",
"Morning\nChronotype (%)",
"Evening\nChronotype (%)",
"Job Medical\nRequired (%)",                   
"Chronic Obstructive Airways Disease(COPD)\n/Emphysema/Chronic Bronchitis (%)",
"Interstitial Lung\nDisease (%)",               
"Other Respiratory\nProblems (%)",
"Gastro-Oesophageal\nReflux (%)")

tab_keys<-c("Never","Previous","Current",
  "White British","White Other","Mixed","Asian","Black","Chinese","Other")
out_tab$Key[out_tab$Key%in%tab_keys] <- paste0("\t",tab_keys)

tab1 <- flextable(data = out_tab) %>% 
  theme_booktabs() %>% 
  add_header_row(values=c("","Current work schedule","Current work schedule","Current work schedule","Current work schedule"),top=TRUE) %>%
  merge_at(i = 1, j = 2:5, part = "header") %>%
  set_header_labels(Key = "", 
                    `No shift work` = "Day workers",
                    `Never/rarely` = "Shift work, but\nnever or rarely night\nshifts",
                    Sometimes = "Irregular shift work\nincluding nights",
                    Always = "Permanent night\nshift work") %>%
  align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
  align(j = 1:5, i = 1, align = "center", part = "head") %>%
  align(j = 1:5, i = 2, align = "center", part = "head") %>%
  align(j = 2:5, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
  #set_header_labels() %>% 
  bg(i = (1:(dim(out_tab)[1]/2))*2-1, bg = "gray90") %>% 
  fontsize(size=8,part = "all") %>%
  border(i = 2, j = 1, border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
  border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
  bold(j = 1, i = ~ is.na(Always), bold = TRUE, part = "body" ) %>%
  rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
  autofit() %>% 
  width(2:5,rep(1.5,4)) %>% 
  height_all(.2) %>% 
  width(1,1.5)


# ft_ORtable --------------------------------------------------------------

ft_ORtables<-function(out_tab){
  tab2 <- flextable(data = out_tab) %>% 
    theme_booktabs() %>% 
    add_header_row(values=c("","Current work schedule","Current work schedule","Current work schedule","Current work schedule"),top=TRUE) %>%
    merge_at(i = 1, j = 2:5, part = "header") %>%
    set_header_labels(Key = "", 
                      `No shift work` = "Day workers",
                      `Never/rarely` = "Shift work, but\nnever or rarely night\nshifts",
                      Sometimes = "Irregular shift work\nincluding nights",
                      Always = "Permanent night\nshift work") %>%
    align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
    align(j = 1:5, i = 1, align = "center", part = "head") %>%
    align(j = 1:5, i = 2, align = "center", part = "head") %>%
    align(j = 2:5, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
    #set_header_labels() %>% 
    bg(i = (1:ceiling(dim(out_tab)[1]/2))*2-1, bg = "gray90") %>% 
    fontsize(size=10,part = "all") %>%
    border(i = 2, j = 1, border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
    border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
    bold(j = 1, i = ~ is.na(Always), bold = TRUE, part = "body" ) %>%
    rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
    autofit() %>% 
    width(2:5,rep(1.5,4)) %>% 
    width(1,1.5)
  
  return(tab2)
}

# table 2 -----------------------------------------------------------------

tab7asthmadefms %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-JiNS) %>%
  spread(JiNS,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`No shift work`=ifelse(is.na(`No shift work`),1,`No shift work`)) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,`Sometimes`,`Always`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_t2

tab2 <- ft_ORtables(out_tab)


# tab3 --------------------------------------------------------------------

tab6asthmadef %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-JiNS) %>%
  spread(JiNS,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`No shift work`=ifelse(is.na(`No shift work`),1,`No shift work`)) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,`Sometimes`,`Always`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum()  -> N_t3

tab3 <- ft_ORtables(out_tab)


# tab4 --------------------------------------------------------------------

table3_WWJiNS %>%
  rename(`Total sample size`="0",`Total cases`="1") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-JiNS) %>%
  spread(JiNS,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`No shift work`=ifelse(is.na(`No shift work`),1,`No shift work`)) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,`Sometimes`,`Always`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum()  -> N_t4

tab4 <- ft_ORtables(out_tab)

# tab5 --------------------------------------------------------------------

table4_FEVJiNS %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-JiNS) %>%
  spread(JiNS,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`No shift work`=ifelse(is.na(`No shift work`),1,`No shift work`)) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,`Sometimes`,`Always`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum()  -> N_t5

tab5 <- ft_ORtables(out_tab)

# lifetime work table -----------------------------------------------------

load("data/ORtablesdata.sumNSW.RData")

tab7asthmadefms.sumNSW %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-sum.MNSorNS.group) %>%
  spread(sum.MNSorNS.group,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`none`=ifelse(is.na(`none`),1,`none`)) %>%
  dplyr::select(Key,`none`,`lessthan5`,`gt5lt10`,`greaterthan10`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum()  -> N_lifetimework

tab_lifetimework  <- flextable(data = out_tab) %>% 
  theme_booktabs() %>% 
  add_header_row(values=c("",rep("Lifetime duration of night shift work",4)),top=TRUE) %>%
  merge_at(i = 1, j = 2:5, part = "header") %>%
  set_header_labels(Key = "", 
                    `none` = "None",
                    `lessthan5` = "< 5 years",
                    gt5lt10 = "5-10 years",
                    greaterthan10 = "\u2265 10 years") %>%
  align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
  align(j = 1:5, i = 1, align = "center", part = "head") %>%
  align(j = 1:5, i = 2, align = "center", part = "head") %>%
  align(j = 2:5, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
  #set_header_labels() %>% 
  bg(i = (1:ceiling(dim(out_tab)[1]/2))*2-1, bg = "gray90") %>% 
  fontsize(size=10,part = "all") %>%
  border(i = 2, j = 1, border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
  border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
  rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
  autofit() %>% 
  width(2:5,rep(1.5,4)) %>% 
  width(1,1.5)

pval.temp<-c()

for(i in 1:length(pval.sumNSW)){
  pval.temp[i]<-ifelse(pval.sumNSW[i]<0.01,"<0.01",round(pval.sumNSW[i],2))
}
pval.temp<-c("","",pval.temp)

tab_lifetimework  <- flextable(data = cbind(out_tab,pval.temp)) %>% 
  theme_booktabs() %>% 
  add_header_row(values=c("",rep("Lifetime duration of night shift work",4),""),top=TRUE) %>%
  merge_at(i = 1, j = 2:5, part = "header") %>%
  set_header_labels(Key = "", 
                    `none` = "None",
                    `lessthan5` = "< 5 years",
                    gt5lt10 = "5-10 years",
                    greaterthan10 = "\u2265 10 years",
                    pval.temp = "p-value \nfor trend") %>%
  align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
  align(j = 1:6, i = 1, align = "center", part = "head") %>%
  align(j = 1:6, i = 2, align = "center", part = "head") %>%
  align(j = 2:6, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
  #set_header_labels() %>% 
  bg(i = (1:ceiling(dim(out_tab)[1]/2))*2-1, bg = "gray90") %>% 
  fontsize(size=10,part = "all") %>%
  border(i = 2, j = c(1,6), border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
  border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
  rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
  autofit() %>% 
  width(2:5,rep(1.3,4)) %>% 
  width(6,.8) %>% 
  width(1,1.5)

cbind(out_tab,pval.temp) -> out_tab_life
# shift work frequency -----------------------------------------------------

load("data/ORtablesdata.freqNSW.RData")

tab7asthmadefms.freqNSW %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-freq.MNSorNS.group) %>%
  spread(freq.MNSorNS.group,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`none`=ifelse(is.na(`none`),1,`none`)) %>%
  dplyr::select(Key,`none`,`lessthan5`,`gt5lt10`,`greaterthan10`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum()  -> N_freq

pval.temp<-c()

for(i in 1:length(pval.freqNSW)){
  pval.temp[i]<-ifelse(pval.freqNSW[i]<0.01,"<0.01",round(pval.freqNSW[i],2))
}
pval.temp<-c("","",pval.temp)

tab_freq  <- flextable(data = cbind(out_tab,pval.temp)) %>% 
  theme_booktabs() %>% 
  add_header_row(values=c("",rep("Frequency of night shift work",4),""),top=TRUE) %>%
  merge_at(i = 1, j = 2:5, part = "header") %>%
  set_header_labels(Key = "", 
                    `none` = "None",
                    `lessthan5` = "< 5/month",
                    gt5lt10 = "5-10/month",
                    greaterthan10 = "\u2265 10/month",
                    pval.temp = "p-value \nfor trend") %>%
  align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
  align(j = 1:6, i = 1, align = "center", part = "head") %>%
  align(j = 1:6, i = 2, align = "center", part = "head") %>%
  align(j = 2:6, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
  #set_header_labels() %>% 
  bg(i = (1:ceiling(dim(out_tab)[1]/2))*2-1, bg = "gray90") %>% 
  fontsize(size=10,part = "all") %>%
  border(i = 2, j = c(1,6), border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
  border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
  rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
  autofit() %>% 
  width(2:5,rep(1.3,4)) %>% 
  width(6,.8) %>% 
  width(1,1.5)

cbind(out_tab,pval.temp) -> out_tab_freq
# stratified by chronotype ------------------------------------------------

tab7asthmadefms_morn %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-JiNS) %>%
  spread(JiNS,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`No shift work`=ifelse(is.na(`No shift work`),1,`No shift work`)) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,`Sometimes`,`Always`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_MSmorn

tabMS_morn <- ft_ORtables(out_tab)

tab7asthmadefms_eve %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-JiNS) %>%
  spread(JiNS,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`No shift work`=ifelse(is.na(`No shift work`),1,`No shift work`)) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,`Sometimes`,`Always`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_MSeve

tabMS_eve <- ft_ORtables(out_tab)


# ft_ORtable --------------------------------------------------------------

ft_chronotables<-function(out_tab){
  tab2 <- flextable(data = out_tab) %>% 
    theme_booktabs() %>% 
    add_header_row(values=c("","Chronotype","Chronotype","Chronotype"),top=TRUE) %>%
    merge_at(i = 1, j = 2:4, part = "header") %>%
    set_header_labels(Key = "") %>%
    align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
    align(j = 1:4, i = 1, align = "center", part = "head") %>%
    align(j = 1:4, i = 2, align = "center", part = "head") %>%
    align(j = 2:4, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
    #set_header_labels() %>% 
    bg(i = (1:ceiling(dim(out_tab)[1]/2))*2-1, bg = "gray90") %>% 
    fontsize(size=10,part = "all") %>%
    border(i = 2, j = 1, border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
    border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
    bold(j = 1, i = ~ is.na(`Definitely an evening person`), bold = TRUE, part = "body" ) %>%
    rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
    autofit() %>% 
    width(2:4,rep(1.5,3)) %>% 
    width(1,1.5)
  
  return(tab2)
}

# table 2 -----------------------------------------------------------------

load("data/chronotablesdata_inter.RData")

tab7asthmadefms %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-Chronotype) %>%
  spread(Chronotype,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`Intermediate chronotype`=ifelse(is.na(`Intermediate chronotype`),1,`Intermediate chronotype`)) -> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_t2_chrono

tab2_chrono <- ft_chronotables(out_tab)


# all asthma chrono -------------------------------------------------------

tab6asthmadef %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-Chronotype) %>%
  spread(Chronotype,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`Intermediate chronotype`=ifelse(is.na(`Intermediate chronotype`),1,`Intermediate chronotype`)) -> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_t6_chrono

tab6_chrono <- ft_chronotables(out_tab)


# grs ---------------------------------------------------------------------

ft_grstables<-function(out_tab,pval.temp){
  tab2 <- flextable(data = cbind(out_tab,pval.temp)) %>% 
    theme_booktabs() %>% 
    add_header_row(values=c("","GRS quartile","GRS quartile","GRS quartile","GRS quartile",""),top=TRUE) %>%
    merge_at(i = 1, j = 2:5, part = "header") %>%
    set_header_labels(Key = "",
                      q1="1st quartile",
                      q2="2nd quartile",
                      q3="3rd quartile",
                      q4="4th quartile",
                      pval.temp = "p-value \nfor trend") %>%
    align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
    align(j = 1:6, i = 1, align = "center", part = "head") %>%
    align(j = 1:6, i = 2, align = "center", part = "head") %>%
    align(j = 2:6, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
    #set_header_labels() %>% 
    bg(i = (1:ceiling(dim(out_tab)[1]/2))*2-1, bg = "gray90") %>% 
    fontsize(size=10,part = "all") %>%
    border(i = 2, j = c(1,6), border.top  = fp_border(width = 2,color = "white"),part = "head") %>%
    border(i = 1, border.top  = fp_border(width = 2,color = "black"),part = "head") %>%
    bold(j = 1, i = ~ is.na(`q1`), bold = TRUE, part = "body" ) %>%
    rotate(i=2, align = 'top', rotation = 'tblr',part = "head") %>%
    autofit() %>% 
    width(2:5,rep(1.3,4)) %>% 
    width(6,.8) %>% 
    width(1,1.5)
  
  return(tab2)
}

load("data/ORtablesdata_grs.RData")

tab7asthmadefms.GRS %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-GRS_group) %>%
  spread(GRS_group,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(q1=ifelse(is.na(q1),1,q1)) -> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_grs_ms

pval.temp<-c()

for(i in 1:length(tab7asthmadefms.GRS_pval)){
  pval.temp[i]<-ifelse(tab7asthmadefms.GRS_pval[i]<0.01,"<0.01",round(tab7asthmadefms.GRS_pval[i],2))
}
pval.temp<-c("","",pval.temp)

tabms_grs <- ft_grstables(out_tab,pval.temp)


# all asthma -------------------------------------------------------

tab6asthmadef.GRS %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-GRS_group) %>%
  spread(GRS_group,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(q1=ifelse(is.na(q1),1,q1)) -> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_grs

pval.temp<-c()

for(i in 1:length(tab6asthmadef.GRS_pval)){
  pval.temp[i]<-ifelse(tab6asthmadef.GRS_pval[i]<0.01,"<0.01",round(tab6asthmadef.GRS_pval[i],2))
}
pval.temp<-c("","",pval.temp)

tab6_grs <- ft_grstables(out_tab,pval.temp)

####

tab7asthmadefms.JiNS.grsq1 %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-JiNS) %>%
  spread(JiNS,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`No shift work`=ifelse(is.na(`No shift work`),1,`No shift work`)) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,`Sometimes`,`Always`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_MSgrsq1

tabMS_grsq1 <- ft_ORtables(out_tab)

##

tab7asthmadefms.JiNS.grsq4 %>%
  rename(`Total sample size`="FALSE",`Total cases`="TRUE") %>%
  mutate(`Total sample size` = `Total sample size`+ `Total cases`) %>%
  mutate(`Total cases` = paste0(formatC(`Total cases`,format="d",big.mark=",")," (",round(`Total cases`/`Total sample size` * 100,2),"%)")) %>%
  mutate(`Total sample size` = formatC(`Total sample size`,format="d",big.mark="," )) %>%
  rename(`Total cases (% of total sample size)`=`Total cases`) %>%
  gather(Key,Value,-JiNS) %>%
  spread(JiNS,Value) %>%
  slice(c(4,5,1,2,3)) %>%
  mutate(`No shift work`=ifelse(is.na(`No shift work`),1,`No shift work`)) %>%
  dplyr::select(Key,`No shift work`,`Never/rarely`,`Sometimes`,`Always`)-> out_tab

out_tab[2,-1]  %>% gsub(pattern = ",",replacement = "",x = .[]) %>% as.numeric() %>% sum() -> N_MSgrsq4

tabMS_grsq4 <- ft_ORtables(out_tab)


source("wordtables_interaction_source.R")

# mod details -------------------------------------------------------------
mod_deets <- "Model 2 covariates: age, sex, smoking status, smoking pack years, alcohol status, daily alcohol intake, ethnicity, Townsend deprivation index, days exercised (walked, moderate and vigorous), BMI, chronotype, length of working week, job asthma risk and job medical required. Model 3 data are adjusted for Model 2 covariates plus sleep duration."
mod_deets_chrono <- "Model 2 covariates: age, sex, smoking status, smoking pack years, alcohol status, daily alcohol intake, ethnicity, Townsend deprivation index, days exercised (walked, moderate and vigorous), BMI, length of working week, job asthma risk and job medical required. Model 3 data are adjusted for Model 2 covariates plus sleep duration."
mod_deets_chrono_int <- "Model 2 was used here with covariates: age, sex, smoking status, smoking pack years, alcohol status, daily alcohol intake, ethnicity, Townsend deprivation index, days exercised (walked, moderate and vigorous), BMI, length of working week, job asthma risk and job medical required. Interaction p-value is calculated using a LRT test comparing the model with and without an interaction term."

mod_deets_grs <- "Model 2 covariates: age, sex, smoking status, smoking pack years, alcohol status, daily alcohol intake, Townsend deprivation index, days exercised (walked, moderate and vigorous), BMI, chronotype, length of working week, job asthma risk and job medical required. Model 3 data are adjusted for Model 2 covariates plus sleep duration."
mod_deets_grs_int <- "Model 2 was used here with covariates: age, sex, smoking status, smoking pack years, alcohol status, daily alcohol intake, ethnicity, Townsend deprivation index, days exercised (walked, moderate and vigorous), BMI, chronotype, length of working week, job asthma risk and job medical required. Interaction p-value is calculated using a LRT test comparing the model with and without an interaction term."

# write document ----------------------------------------------------------

read_docx() %>% 
  body_add_par(paste0("Table 1: Clinical characteristics by current night shift work exposure (N = ",formatC(N_t1,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab1) %>%
  body_add_par("Data are mean (SD), median (IQR) or percentages. Positive values of the Townsend index indicate high material deprivation, negative values indicate relative affluence. Diagnosis of conditions here (hypetension, high cholesterol, sleep apnoea, COPD/emphysema/chronic bronchitis, bronchiectasis, interstitial lung disease, other respiratory problems and gastro-oesophageal reflux) came from participants self-reporting a doctor diagnosis.") %>%
  body_add_break() %>%
  body_add_par(paste0("Table 2: Adjusted odds (95% CI) of moderate/severe asthma by current shift work exposure (N = ",formatC(N_t2,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab2) %>%
  body_add_par(mod_deets) %>%
  body_add_break() %>%
  body_add_par(paste0("Table 3: Adjusted odds (95% CI) of experiencing wheeze or whistling in the chest in the last year  by current shift work exposure (N = ",formatC(N_t4,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab4) %>%
  body_add_par(mod_deets) %>%
  body_add_break() %>%
  body_add_par(paste0("Table 4: Adjusted odds (95% CI) of having a critical FEV1 predicted percentage (<80%) by current shift work exposure (N = ",formatC(N_t5,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab5) %>%
  body_add_par(mod_deets) %>%
  body_add_break() %>%
  body_add_par(paste0("Table 5: Adjusted odds (95% CI) of moderate/severe asthma by lifetime duration of shift work including nights (N = ",formatC(N_lifetimework,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab_lifetimework) %>%
  body_add_par(mod_deets) %>%
  body_add_break() %>%
  body_add_par(paste0("Table 6: Adjusted odds (95% CI) of moderate/severe asthma by average monthly frequency of shifts worked that include night shifts (N = ",formatC(N_freq,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab_freq) %>%
  body_add_par(mod_deets) %>%
  body_add_break() %>%
  body_add_par(paste0("Table 7: Adjusted odds (95% CI) of any asthma by chronotype (N = ",formatC(N_t6_chrono,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab6_chrono) %>%
  body_add_par(mod_deets_chrono) %>%
  body_add_break() %>%
  body_add_par(paste0("Supplemental Table 1: Adjusted odds (95% CI) of any asthma by current shift work exposure (N = ",formatC(N_t3,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab3) %>%
  body_add_par(mod_deets) %>%
  body_add_break() %>%
  body_add_par(paste0("Supplemental Table 2: Adjusted odds (95% CI) of moderate/severe asthma by chronotype (N = ",formatC(N_t2_chrono,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab2_chrono) %>%
  body_add_par(mod_deets_chrono) %>%
  body_add_break() %>%
  body_add_par("Supplemental Table 3: Adjusted odds (95% CI) and association of moderate/severe asthma and current night shift work exposure by chronotype") %>%
  body_add_flextable(tab_chrono_interaction) %>%
  body_add_par(mod_deets_chrono_int) %>%
  body_add_break() %>%
  body_add_par(paste0("Supplemental Table 4: Adjusted odds (95% CI) of moderate/severe asthma by genetic risk score (GRS) quartile (N = ",formatC(N_grs_ms,format="d",big.mark=","),")")) %>%
  body_add_flextable(tabms_grs) %>%
  body_add_par(mod_deets_grs) %>%
  body_add_break() %>%
  body_add_par(paste0("Supplemental Table 5: Adjusted odds (95% CI) of any asthma by genetic risk score (GRS) quartile (N = ",formatC(N_grs,format="d",big.mark=","),")")) %>%
  body_add_flextable(tab6_grs) %>%
  body_add_par(mod_deets_grs) %>%
  body_add_break() %>%
  body_add_par("Supplemental Table 6: Adjusted odds (95% CI) and association of moderate/severe asthma and current night shift work exposure by genetic risk") %>%
  body_add_flextable(tab_grs_interaction) %>%
  body_add_par(mod_deets_grs_int) %>%
  print(target = "tables_all.docx")
