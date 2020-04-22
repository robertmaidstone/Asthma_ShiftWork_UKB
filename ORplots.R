library(tidyverse)
library(RColorBrewer)

load("data/ORtablesdata.sumNSW.RData")
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


tab7asthmadefms.sumNSW %>% gather(Model,OR,-sum.MNSorNS.group,-`FALSE`,-`TRUE`) %>%
  #mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  separate(OR,into=c("OR","CI"),sep = " ") %>%
  mutate(CI=str_remove(CI,"[(]")) %>%
  mutate(CI=str_remove(CI,"[)]")) %>%
  separate(CI,into=c("LCI","UCI"),sep="-") %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) %>%
  mutate(sum.MNSorNS.group=factor(sum.MNSorNS.group,
                                  labels=c("None (referent)","< 5 years","5-10 years","> 10 years"))) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  as_tibble -> OR_plot_data
  
pd_width <- 0.6
  
OR_plot_data %>%
  #filter(!is.na(LCI)) %>%
  ggplot(aes(y=OR,x=sum.MNSorNS.group,colour=Model,shape=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.6,2.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(OR_plot_data$sum.MNSorNS.group)))+
  scale_colour_manual(values = cbPalette[2:4],
                      name = element_blank(),
                      labels = c("Model 3", "Model 2","Model 1"),guide = guide_legend(reverse=TRUE)) +
  scale_shape_manual(values=c(17,15,16),name = element_blank(),
                     labels = c("Model 3", "Model 2","Model 1"),guide = guide_legend(reverse=TRUE))+
  ggtitle("a")+guides(colour=FALSE,shape=FALSE)-> p_sum

load("data/ORtablesdata.freqNSW.RData")

tab7asthmadefms.freqNSW %>% 
  gather(Model,OR,-freq.MNSorNS.group,-`FALSE`,-`TRUE`) %>%
  #mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  separate(OR,into=c("OR","CI"),sep = " ") %>%
  mutate(CI=str_remove(CI,"[(]")) %>%
  mutate(CI=str_remove(CI,"[)]")) %>%
  separate(CI,into=c("LCI","UCI"),sep="-") %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) %>%
  mutate(freq.MNSorNS.group=factor(freq.MNSorNS.group,
                                  labels=c("None (referent)","< 5/month","5-10/month","> 10/month"))) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  as_tibble -> OR_plot_data

pd_width <- 0.6

OR_plot_data %>%
  #filter(!is.na(LCI)) %>%
  ggplot(aes(y=OR,x=freq.MNSorNS.group,colour=Model,shape=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank())+
  ylab("Odds Ratio") +
  ylim(c(.6,2.5)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(OR_plot_data$freq.MNSorNS.group)))+
  scale_colour_manual(values = cbPalette[2:4],
                      name = element_blank(),
                      labels = c("Model 3", "Model 2","Model 1"),guide = guide_legend(reverse=TRUE))+
  scale_shape_manual(values=c(17,15,16),name = element_blank(),
                     labels = c("Model 3", "Model 2","Model 1"),guide = guide_legend(reverse=TRUE))+
  ggtitle("b")-> p_freq

library(patchwork)
p_sum+p_freq + plot_layout(guides = "collect")

#######################

#load("data/ORtablesdata.freqNSW.RData")
load("data/ORtablesdata.RData")
tab7asthmadefms %>% 
  gather(Model,OR,-JiNS,-`FALSE`,-`TRUE`) %>%
  #mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  separate(OR,into=c("OR","CI"),sep = " ") %>%
  mutate(CI=str_remove(CI,"[(]")) %>%
  mutate(CI=str_remove(CI,"[)]")) %>%
  separate(CI,into=c("LCI","UCI"),sep="-") %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) %>%
  mutate(JiNS=factor(JiNS,
                     labels=c("Day workers (referent)",
                              "Shift work, but never or\n rarely night shifts",
                              "Irregular shift work\n including nights",
                              "Permanent night shift\n work"))) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  as_tibble -> OR_plot_data

pd_width <- 0.6

OR_plot_data %>%
  #filter(!is.na(LCI)) %>%
  ggplot(aes(y=OR,x=JiNS,colour=Model,shape=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank()) +
  ylab("Odds Ratio") +
  scale_y_continuous(limits=c(.9,1.9),breaks=c(1,1.2,1.4,1.6,1.8)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(OR_plot_data$JiNS)))+
  scale_colour_manual(values = cbPalette[2:4],
                      name = element_blank(),
                      labels = c("Model 3", "Model 2","Model 1"),guide = guide_legend(reverse=TRUE))+
  scale_shape_manual(values=c(17,15,16),name = element_blank(),
                      labels = c("Model 3", "Model 2","Model 1"),guide = guide_legend(reverse=TRUE))-> p_ast_ms


############

table3_WWJiNS %>% 
  gather(Model,OR,-JiNS,-`0`,-`1`) %>%
  #mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  separate(OR,into=c("OR","CI"),sep = " ") %>%
  mutate(CI=str_remove(CI,"[(]")) %>%
  mutate(CI=str_remove(CI,"[)]")) %>%
  separate(CI,into=c("LCI","UCI"),sep="-") %>%
  mutate(LCI=as.numeric(LCI)) %>%
  mutate(UCI=as.numeric(UCI)) %>%
  mutate(OR=as.numeric(OR)) %>%
  separate(Model,into="Model",sep = ":") %>%
  mutate(Model=factor(Model,levels=c("Model 3","Model 2","Model 1"))) %>%
  mutate(JiNS=factor(JiNS,
                     labels=c("Day workers (referent)",
                              "Shift work, but never or\n rarely night shifts",
                              "Irregular shift work\n including nights",
                              "Permanent night shift\n work"))) %>%
  mutate(OR=ifelse(is.na(OR),1,OR)) %>%
  mutate(LCI=ifelse(is.na(OR),NULL,LCI)) %>%
  mutate(UCI=ifelse(is.na(OR),NULL,UCI)) %>%
  as_tibble -> OR_plot_data

pd_width <- 0.6


OR_plot_data %>%
  #filter(!is.na(LCI)) %>%
  ggplot(aes(y=OR,x=JiNS,colour=Model,shape=Model)) + 
  geom_hline(aes(yintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbar(aes(ymax = UCI, ymin = LCI), size = .5, width = .4,
                position = position_dodge(width = pd_width)) +
  geom_point(position = position_dodge(width = pd_width)) + 
  theme_bw() +
  theme(axis.title.y = element_blank(),
        legend.position=c(0.8, 0.85),
        legend.background = element_blank()) +
  ylab("Odds Ratio") +
  scale_y_continuous(limits=c(.9,1.6),breaks=c(1,1.2,1.4,1.6)) +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(OR_plot_data$JiNS)))+
  scale_colour_manual(values = cbPalette[2:4],
                      name = element_blank(),
                      labels = c("Model 3", "Model 2","Model 1"),guide = guide_legend(reverse=TRUE))+
  scale_shape_manual(values=c(17,15,16),name = element_blank(),
                     labels = c("Model 3", "Model 2","Model 1"),guide = guide_legend(reverse=TRUE))-> p_ast_ww


p_ast_ms
p_ast_ww
p_sum+p_freq -> p_patch
ggsave(filename = "fig3_life_freq_V3.png",plot = (p_patch),
       height = 4,width = 8)
ggsave(filename = "fig1_ast_ms_V3.png",plot = (p_ast_ms),
       height = 4,width = 4)
ggsave(filename = "fig2_ast_ww_V3.png",plot = (p_ast_ww),
       height = 4,width = 4)
