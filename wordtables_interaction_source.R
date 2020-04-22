
load("data/ORtablesdata.RData")
load("data/interactionterms.RData")


n_num_morn <- c(sum(tab7asthmadefms_morn$`FALSE`) + sum(tab7asthmadefms_morn$`TRUE`),sum(tab7asthmadefms_morn$`TRUE`)) %>% formatC(format="d",big.mark="," )
n_num_eve <- c(sum(tab7asthmadefms_eve$`FALSE`) + sum(tab7asthmadefms_eve$`TRUE`),sum(tab7asthmadefms_eve$`TRUE`))%>% formatC(format="d",big.mark="," )
n_num_inter <- c(sum(tab7asthmadefms_inter$`FALSE`) + sum(tab7asthmadefms_inter$`TRUE`),sum(tab7asthmadefms_inter$`TRUE`))%>% formatC(format="d",big.mark="," )


rbind((tab7asthmadefms_morn %>% mutate(JiNS=plyr::revalue(JiNS,c(`No shift work` = "Day workers",
                                                                 `Never/rarely` = "Shift work, but never or rarely night\nshifts",
                                                                 Sometimes = "Irregular shift work including nights",
                                                                 Always = "Permanent night shift work"))) %>%
         dplyr::select(JiNS,`Model 2: Multivariable adjusted OR (95% CI)`) %>%
  mutate(`Model 2: Multivariable adjusted OR (95% CI)`=ifelse(is.na(`Model 2: Multivariable adjusted OR (95% CI)`),1,`Model 2: Multivariable adjusted OR (95% CI)`)) %>%
  rename(`Current work schedule`=JiNS)),
  (tab7asthmadefms_inter%>% mutate(JiNS=plyr::revalue(JiNS,c(`No shift work` = "Day workers",
                                                             `Never/rarely` = "Shift work, but never or rarely night\nshifts",
                                                             Sometimes = "Irregular shift work including nights",
                                                             Always = "Permanent night shift work")))  %>%
     dplyr::select(JiNS,`Model 2: Multivariable adjusted OR (95% CI)`) %>%
     mutate(`Model 2: Multivariable adjusted OR (95% CI)`=ifelse(is.na(`Model 2: Multivariable adjusted OR (95% CI)`),1,`Model 2: Multivariable adjusted OR (95% CI)`)) %>%
     rename(`Current work schedule`=JiNS)),
(tab7asthmadefms_eve%>% mutate(JiNS=plyr::revalue(JiNS,c(`No shift work` = "Day workers",
                                                         `Never/rarely` = "Shift work, but never or rarely night\nshifts",
                                                         Sometimes = "Irregular shift work including nights",
                                                         Always = "Permanent night shift work")))  %>%
   dplyr::select(JiNS,`Model 2: Multivariable adjusted OR (95% CI)`) %>%
   mutate(`Model 2: Multivariable adjusted OR (95% CI)`=ifelse(is.na(`Model 2: Multivariable adjusted OR (95% CI)`),1,`Model 2: Multivariable adjusted OR (95% CI)`)) %>%
   rename(`Current work schedule`=JiNS))
)%>%
  rename(`OR (95% CI)`=`Model 2: Multivariable adjusted OR (95% CI)`)-> out_tab


out_tab %>% mutate(`Current work schedule`=as.character(`Current work schedule`)) %>%
  add_row(`Current work schedule`=paste0("Definite morning chronotype (N= ",n_num_morn[1],", ",n_num_morn[2]," cases)"), .before = 1)%>%
  add_row(`Current work schedule`=paste0("Intermediate chronotype (N= ",n_num_inter[1],", ",n_num_inter[2]," cases)"), .before = 6) %>% 
  add_row(`Current work schedule`=paste0("Definite evening chronotype (N= ",n_num_eve[1],", ",n_num_eve[2]," cases)"), .before = 11)-> out_tab

out_tab %>% 
  mutate(P_interaction=round(int_chrono$`Pr(>Chisq)`[2],2)) %>% 
  mutate(`OR (95% CI)`=ifelse(is.na(`OR (95% CI)`),`Current work schedule`,`OR (95% CI)`)) %>%
  mutate(P_interaction=c("",P_interaction[-c(1,2)],""))-> out_tab


flextable(data = out_tab) %>% 
  theme_booktabs() %>% 
  align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
  align(j = 1, i = 1, align = "left", part = "head") %>%
  align(j = 2:3, i = 1, align = "center", part = "head") %>%
  align(j = 2:3, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
  valign(j = 3, i = 1:(dim(out_tab)[1]), valign = "top", part = "body") %>%
  bg(i = c(1,6,11),j=1:2, bg = "gray90") %>% 
  bold(j = 1, i = ~ (`Current work schedule` == `OR (95% CI)`), bold = TRUE, part = "body" ) %>%
  fontsize(size=10,part = "all") %>%
  rotate(i=1, align = 'top', rotation = 'tblr',part = "head") %>%
  merge_h(i=c(1,6,11)) %>%
  merge_v(j=3) %>%
  autofit() %>%
  width(1,3) -> tab_chrono_interaction

load("data/ORtablesdata_grs.RData")


n_num_q1 <- c(sum(tab7asthmadefms.JiNS.grsq1$`FALSE`) + sum(tab7asthmadefms.JiNS.grsq1$`TRUE`),sum(tab7asthmadefms.JiNS.grsq1$`TRUE`)) %>% formatC(format="d",big.mark="," )
n_num_q2 <- c(sum(tab7asthmadefms.JiNS.grsq2$`FALSE`) + sum(tab7asthmadefms.JiNS.grsq2$`TRUE`),sum(tab7asthmadefms.JiNS.grsq2$`TRUE`))%>% formatC(format="d",big.mark="," )
n_num_q3 <- c(sum(tab7asthmadefms.JiNS.grsq3$`FALSE`) + sum(tab7asthmadefms.JiNS.grsq3$`TRUE`),sum(tab7asthmadefms.JiNS.grsq3$`TRUE`))%>% formatC(format="d",big.mark="," )
n_num_q4 <- c(sum(tab7asthmadefms.JiNS.grsq4$`FALSE`) + sum(tab7asthmadefms.JiNS.grsq4$`TRUE`),sum(tab7asthmadefms.JiNS.grsq4$`TRUE`))%>% formatC(format="d",big.mark="," )


rbind((tab7asthmadefms.JiNS.grsq1 %>% mutate(JiNS=plyr::revalue(JiNS,c(`No shift work` = "Day workers",
                                                                 `Never/rarely` = "Shift work, but never or rarely night\nshifts",
                                                                 Sometimes = "Irregular shift work including nights",
                                                                 Always = "Permanent night shift work"))) %>%
         dplyr::select(JiNS,`Model 2: Multivariable adjusted OR (95% CI)`) %>%
         mutate(`Model 2: Multivariable adjusted OR (95% CI)`=ifelse(is.na(`Model 2: Multivariable adjusted OR (95% CI)`),1,`Model 2: Multivariable adjusted OR (95% CI)`)) %>%
         rename(`Current work schedule`=JiNS)),
      (tab7asthmadefms.JiNS.grsq2%>% mutate(JiNS=plyr::revalue(JiNS,c(`No shift work` = "Day workers",
                                                                 `Never/rarely` = "Shift work, but never or rarely night\nshifts",
                                                                 Sometimes = "Irregular shift work including nights",
                                                                 Always = "Permanent night shift work")))  %>%
         dplyr::select(JiNS,`Model 2: Multivariable adjusted OR (95% CI)`) %>%
         mutate(`Model 2: Multivariable adjusted OR (95% CI)`=ifelse(is.na(`Model 2: Multivariable adjusted OR (95% CI)`),1,`Model 2: Multivariable adjusted OR (95% CI)`)) %>%
         rename(`Current work schedule`=JiNS)),
      (tab7asthmadefms.JiNS.grsq3%>% mutate(JiNS=plyr::revalue(JiNS,c(`No shift work` = "Day workers",
                                                               `Never/rarely` = "Shift work, but never or rarely night\nshifts",
                                                               Sometimes = "Irregular shift work including nights",
                                                               Always = "Permanent night shift work")))  %>%
         dplyr::select(JiNS,`Model 2: Multivariable adjusted OR (95% CI)`) %>%
         mutate(`Model 2: Multivariable adjusted OR (95% CI)`=ifelse(is.na(`Model 2: Multivariable adjusted OR (95% CI)`),1,`Model 2: Multivariable adjusted OR (95% CI)`)) %>%
         rename(`Current work schedule`=JiNS)),
      (tab7asthmadefms.JiNS.grsq4%>% mutate(JiNS=plyr::revalue(JiNS,c(`No shift work` = "Day workers",
                                                                      `Never/rarely` = "Shift work, but never or rarely night\nshifts",
                                                                      Sometimes = "Irregular shift work including nights",
                                                                      Always = "Permanent night shift work")))  %>%
         dplyr::select(JiNS,`Model 2: Multivariable adjusted OR (95% CI)`) %>%
         mutate(`Model 2: Multivariable adjusted OR (95% CI)`=ifelse(is.na(`Model 2: Multivariable adjusted OR (95% CI)`),1,`Model 2: Multivariable adjusted OR (95% CI)`)) %>%
         rename(`Current work schedule`=JiNS))
)%>%
  rename(`OR (95% CI)`=`Model 2: Multivariable adjusted OR (95% CI)`)-> out_tab


out_tab %>% mutate(`Current work schedule`=as.character(`Current work schedule`)) %>%
  add_row(`Current work schedule`=paste0("GRS first quartile (lowest) (N= ",n_num_q1[1],", ",n_num_q1[2]," cases)"), .before = 1)%>%
  add_row(`Current work schedule`=paste0("GRS second quartile (N= ",n_num_q2[1],", ",n_num_q2[2]," cases)"), .before = 6)%>%
  add_row(`Current work schedule`=paste0("GRS third quartile (N= ",n_num_q3[1],", ",n_num_q3[2]," cases)"), .before = 11)%>%
  add_row(`Current work schedule`=paste0("GRS fourth quartile (highest) (N= ",n_num_q4[1],", ",n_num_q4[2]," cases)"), .before = 16)-> out_tab

out_tab %>% 
  mutate(P_interaction=round(int_grs$`Pr(>Chisq)`[2],2)) %>% 
  mutate(`OR (95% CI)`=ifelse(is.na(`OR (95% CI)`),`Current work schedule`,`OR (95% CI)`)) %>%
  mutate(P_interaction=c("",P_interaction[-c(1,2)],""))-> out_tab


flextable(data = out_tab) %>% 
  theme_booktabs() %>% 
  align(j = 1, i = 1:(dim(out_tab)[1]), align = "left", part = "body") %>%
  align(j = 1, i = 1, align = "left", part = "head") %>%
  align(j = 2:3, i = 1, align = "center", part = "head") %>%
  align(j = 2:3, i = 1:(dim(out_tab)[1]), align = "center", part = "body") %>%
  valign(j = 3, i = 1:(dim(out_tab)[1]), valign = "top", part = "body") %>%
  bg(i = c(1,6,11,16),j=1:2, bg = "gray90") %>% 
  bold(j = 1, i = ~ (`Current work schedule` == `OR (95% CI)`), bold = TRUE, part = "body" ) %>%
  fontsize(size=10,part = "all") %>%
  rotate(i=1, align = 'top', rotation = 'tblr',part = "head") %>%
  merge_h(i=c(1,6,11,16)) %>%
  merge_v(j=3) %>%
  autofit() %>%
  width(1,3) -> tab_grs_interaction

