library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(mgcv)
library(itsadug)
library(beepr)
setwd("C:/Users/echen/Box/Sacramento Winter run Chinook/Otoliths")
#########################################
#### Summary Microstructure Stats #######
#########################################
Microstructure<-read.csv("Microstructure data with chemistry.csv")
Microstructure<-Microstructure %>%
  filter(Escapement_Y == 2018|Escapement_Y == 2019)
#creating a variable "blank_bg" to have white backgrounds on plots
blank_bg<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background = element_blank(), axis.line = element_line(colour = "black"))
####### Life History IDs ##########
AMEfishlist<-unique(Microstructure$Sample_ID[which(Microstructure$Habitat == "American River")])
LASfishlist<-unique(Microstructure$Sample_ID[which(Microstructure$Habitat == "Lassen Tributaries")])
HatSlowfishlist<-unique(Microstructure$Sample_ID[which(Microstructure$Origin == "Hatchery" & Microstructure$Habitat == "Sacramento River")])
MicrostructureDELN<-Microstructure %>% #Natural Delta rearers
  filter(Habitat == "Delta" & Origin == "Natural") %>%
  group_by(Sample_ID) %>%
  summarise(Dif = max(Inc_distance) - min(Inc_distance)) %>%
  filter(Dif >= 120)
MicrostructureDELH<-Microstructure %>% #Hatchery Delta rearers
  filter(Sample_ID %in% HatSlowfishlist & Habitat == "Delta") %>%
  group_by(Sample_ID) %>%
  summarise(Dif = max(Inc_distance) - min(Inc_distance)) %>%
  filter(Dif >= 120)
DELFEAfishlist<-c(MicrostructureDELN$Sample_ID, MicrostructureDELH$Sample_ID) 
Microstructure<- Microstructure %>%
  mutate(Space_Use= as.factor(ifelse(Sample_ID %in%LASfishlist, "Lassen tributaries",
                                     ifelse(Sample_ID %in%AMEfishlist,
                                            "American River",
                                            ifelse(Sample_ID %in% DELFEAfishlist, 
                                     "Delta/Feather","no tributary" ))))) 

AMEfish<-Microstructure %>% filter(Sample_ID %in% AMEfishlist)
LASfish<-Microstructure %>% filter(Sample_ID %in% LASfishlist & Origin == "Natural")
Nontrib<-Microstructure %>% filter(!Sample_ID %in% LASfishlist & !Sample_ID %in% AMEfishlist& Origin == "Natural")
Hatch<-Microstructure %>% filter(Origin == "Hatchery")
Microstructure<- Microstructure %>%
  mutate(Life_History= as.factor(ifelse(Origin == "Hatchery", ifelse(Sample_ID %in% HatSlowfishlist, "Sacramento rearing", "migrant"),
                                        ifelse(Sample_ID %in%LASfishlist, "Lassen tributaries", ifelse(Sample_ID %in%AMEfishlist, "American River", ifelse(Sample_ID %in% DELFEAfishlist, "Delta/Feather", "no tributary")))))) 
Entry_age<-Microstructure %>%
  group_by(Sample_ID, Habitat,Origin) %>%
  summarise(Enter_Age = min(Inc_no)) %>%
  group_by(Habitat, Origin)%>%
  summarise(Mean_Enter_Age = mean(Enter_Age), sd = sd(Enter_Age)) 
###########  Residence Time   #############
Microstructure_summarise <- Microstructure%>%
  group_by(Sample_ID, Escapement_Y, Origin, Life_History) %>%
  summarise(LastHatch = max(Inc_no[which(Habitat == "Hatchery")]),
            FirstSac = max(min(Inc_no[which(Habitat == "Sacramento River")]),max(Inc_no[which(Habitat == "Hatchery")])) ,
            LastSac = max(Inc_no[which(Habitat == "Sacramento River")]),
            FirstAme = min(Inc_no[which(Habitat == "American River")]),
            LastAme = max(Inc_no[which(Habitat == "American River")]),
            FirstLas = min(Inc_no[which(Habitat == "Lassen Tributaries")]),
            LastLas = max(Inc_no[which(Habitat == "Lassen Tributaries")]),
            FirstDel = min(Inc_no[which(Habitat == "Delta")]),
            LastDel = max(Inc_no[which(Habitat == "Delta")]),
            FirstOc = min(Inc_no[which(Habitat == "Ocean")]))
#Scary warnings are from Inf
Microstructure_summarise[Microstructure_summarise < 0]<-0
Microstructure_summarise$FirstSac[Microstructure_summarise$FirstSac > 1000]<-0
Microstructure_summarise$FirstLas[Microstructure_summarise$FirstLas > 1000]<-0
Microstructure_summarise$FirstDel[Microstructure_summarise$FirstDel > 1000]<-0
Microstructure_summarise$FirstAme[Microstructure_summarise$FirstAme > 1000]<-0
Microstructure_summarise$FirstOc[Microstructure_summarise$FirstOc > 1000]<-0

Microstructure_summarise <- Microstructure_summarise%>%
  mutate(DaysHatch = LastHatch) %>%
  mutate(DaysLas = LastLas-FirstLas) %>%
  mutate(DaysSac = LastSac-FirstSac) %>%
  mutate(DaysAme = LastAme-FirstAme) %>%
  mutate(DaysDel = LastDel-FirstDel) 
Residence<-pivot_longer(Microstructure_summarise, cols = c(DaysHatch, DaysLas, DaysSac, DaysAme, DaysDel), 
                                    names_to = "Habitat", values_to = "Days")
Residence$Habitat <- factor(Residence$Habitat , levels=c("DaysHatch", "DaysSac", "DaysLas", "DaysAme", "DaysDel"))
levels(Residence$Habitat) <- c("Hatchery", "Sacramento mainstem", "Lassen tributaries", "American River", "Delta")

#Removing hatchery samples that have no Sac to distinguish Hatchery versus Delta from Hatchery and Delta count
Sac<-unique(Microstructure$Sample_ID[which(Microstructure$Habitat == "Sacramento River" )])
Residence<-Residence[!(Residence$Habitat == "Hatchery" & !Residence$Sample_ID %in% Sac),]
Residence<-Residence[!(Residence$Habitat == "Delta" & !Residence$Sample_ID %in% Sac),]

Residence$Days[Residence$Days < 0]<-0
ggplot(Residence, aes(y=Days, x=Habitat, fill= Origin))+
  geom_boxplot(outlier.shape = NA, colour = "black")+
  geom_point(position=position_jitterdodge(jitter.width = .3), size = .3)+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  blank_bg
# ggsave("Fig4.jpg", plot = last_plot(),width= 8, height=6)
Residence_time<-Residence %>%
  group_by(Origin, Habitat) %>%
  summarise(Time = mean(Days), sd = sd(Days), n = n())
#Mann Whitney tests for residence times differences in mainstem and delta
wilcox.test(Residence$Days[which(Residence$Origin == "Natural" & Residence$Habitat == "Sacramento mainstem")],Residence$Days[which(Residence$Origin == "Hatchery" & Residence$Habitat == "Sacramento mainstem")])
wilcox.test(Residence$Days[which(Residence$Origin == "Natural" & Residence$Habitat == "Delta")],Residence$Days[which(Residence$Origin == "Hatchery" & Residence$Habitat == "Delta")])
######## Age of fish at ocean entry #############
ocean_size<-read.csv("Ocean_entry_size.csv")
unfinished<-Microstructure %>% #removing samples that are incomplete from calculating ocean entry
  group_by(Sample_ID) %>%
  summarise(stop = max(Inc_no)) %>%
  filter(stop < 150)
#Sometimes counting ends before ocean entry, especially later ocean entry over 200 days, so interpolating ocean entry increment
ocean_size<-Microstructure %>%
  filter(!Sample_ID %in% unfinished$Sample_ID) %>%
  left_join(ocean_size) %>%
  mutate(difference = abs(Inc_distance - Enter_Size)) 
ocean_inc<-as.data.frame(unique(ocean_size$Sample_ID))
colnames(ocean_inc)<-"Sample_ID"
ocean_inc$Sample_ID<-NA
ocean_inc$inc<-NA
ocean_inc$dif<-NA
ocean_inc$last_growth<-NA
ocean_size<-split(ocean_size, f = ocean_size$Sample_ID)
for(i in 1:211){ #interpolating ocean entry by using the mean incremement width in the final 10 measurements
  length<-nrow(ocean_size[[i]])
  ocean_inc$Sample_ID[i]<-ocean_size[[i]]$Sample_ID[1]
  ocean_inc$inc[i]<-ocean_size[[i]]$Inc_no[which(ocean_size[[i]]$difference == min(ocean_size[[i]]$difference))]
  ocean_inc$dif[i]<-ocean_size[[i]]$difference[which(ocean_size[[i]]$difference == min(ocean_size[[i]]$difference))]
  ocean_inc$last_growth[i]<-mean(c(ocean_size[[i]]$Between_inc_dist[length],
                                    ocean_size[[i]]$Between_inc_dist[length-1],
                                    ocean_size[[i]]$Between_inc_dist[length-2],
                                    ocean_size[[i]]$Between_inc_dist[length-3],
                                    ocean_size[[i]]$Between_inc_dist[length-4],
                                   ocean_size[[i]]$Between_inc_dist[length-5],
                                   ocean_size[[i]]$Between_inc_dist[length-6],
                                   ocean_size[[i]]$Between_inc_dist[length-7],
                                   ocean_size[[i]]$Between_inc_dist[length-8],
                                   ocean_size[[i]]$Between_inc_dist[length-9],
                                   ocean_size[[i]]$Between_inc_dist[length-10]))
                                    
}
ocean_inc<-ocean_inc %>%
  mutate(Ocean_Entry_Age = inc+round(dif/last_growth)) %>%
  select(Sample_ID, Ocean_Entry_Age)
Ocean_Entry<-ocean_inc %>%
  left_join(Microstructure %>% select(Sample_ID, Escapement_Y, Origin, Life_History) %>% distinct())
#ANOVA whether HvW and 2018 and 2019 differ in Ocean entry age
mean(Ocean_Entry$Ocean_Entry_Age)
Ocean_Entry<-Ocean_Entry %>%
  filter(Escapement_Y == 2018|Escapement_Y == 2019)
sample_size<-Ocean_Entry %>%
  group_by(Origin, Escapement_Y) %>%
  summarise(Total = n())
model<-aov(Ocean_Entry$Ocean_Entry_Age~as.factor(Ocean_Entry$Escapement_Y)+Ocean_Entry$Origin)
summary(model)
#Age
ggplot(Ocean_Entry, aes(x=Ocean_Entry_Age, fill= Origin))+
  geom_density(alpha=.5)+
  xlim(60, 280)+
  annotate("text", x=60, y=.019, label="b)", size = 8)+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")+
  labs(x="Age (days)")
#mean and standard deviation of ocean entry ages
summary(Ocean_Entry$Ocean_Entry_Age[which(Ocean_Entry$Origin == "Natural"& Ocean_Entry$Escapement_Y == 2019)])
sd(Ocean_Entry$Ocean_Entry_Age[which(Ocean_Entry$Origin == "Natural"& Ocean_Entry$Escapement_Y == 2019)])
summary(Ocean_Entry$Ocean_Entry_Age[which(Ocean_Entry$Origin == "Natural"& Ocean_Entry$Escapement_Y == 2018)])
sd(Ocean_Entry$Ocean_Entry_Age[which(Ocean_Entry$Origin == "Natural"& Ocean_Entry$Escapement_Y == 2018)])
summary(Ocean_Entry$Ocean_Entry_Age[which(Ocean_Entry$Origin == "Hatchery"& Ocean_Entry$Escapement_Y == 2019)])
sd(Ocean_Entry$Ocean_Entry_Age[which(Ocean_Entry$Origin == "Hatchery"& Ocean_Entry$Escapement_Y == 2019)])
summary(Ocean_Entry$Ocean_Entry_Age[which(Ocean_Entry$Origin == "Hatchery"& Ocean_Entry$Escapement_Y == 2018)])
sd(Ocean_Entry$Ocean_Entry_Age[which(Ocean_Entry$Origin == "Hatchery"& Ocean_Entry$Escapement_Y == 2018)])
###########################################
########### Relative Growth  ##############
###########################################
Microstructure$Inc_distance_scaled<-as.vector(scale(Microstructure$Inc_distance))
Microstructure$Sample_ID<-as.factor(Microstructure$Sample_ID)
Microstructure$Escapement_Y<-as.factor(Microstructure$Escapement_Y)
#Removing hatchery samples that have no Sac to distinguish Hatchery versus Delta from Hatchery and Delta count
Sac<-unique(Microstructure$Sample_ID[which(Microstructure$Habitat == "Sacramento River" & Microstructure$Origin == "Hatchery")])
library(plyr)
Growth<-Microstructure %>%
  filter(Inc_no != 0 & Inc_no != 1 ) %>%
  filter(Between_inc_dist < 7) %>%
  filter(Sample_ID %in% Sac|Origin == "Natural")%>%
  mutate(Inc_distance = round_any(Inc_distance,1,f = floor)) %>% #rounding to floor so values are integers
  group_by(Sample_ID, Inc_distance, Origin, Habitat, Escapement_Y, Space_Use) %>%
  dplyr::summarise(Avg_width = mean(Between_inc_dist)) %>%
  filter(Inc_distance < 850)
detach("package:plyr", unload=TRUE)
Growth$Habitat<- factor(Growth$Habitat, levels=c('Sacramento River', 'Hatchery', 'Lassen Tributaries', 'American River', 'Delta','Ocean' ))
Growth$Origin<- factor(Growth$Origin, levels=c('Natural', 'Hatchery'))
Growth$Escapement_Y<-factor(Growth$Escapement_Y, levels = c('2018', '2019'))
Growth$Space_Use<-factor(Growth$Space_Use, levels = c('no tributary', 'Delta/Feather', 'Lassen tributaries', 'American River'))
#Chosen GAMM model dAIC = 0
gamm_model<-gamm(Avg_width ~ s(Inc_distance)+Origin+Habitat+Escapement_Y, data=Growth, random = list(Sample_ID=~1), correlation = corAR1(form = ~ Inc_distance|Sample_ID))
#other candidate models
#with Origin, Habitat, Year, Life History. dAIC = 4
# gamm_model1<-gamm(Avg_width ~ s(Inc_distance)+Origin+Habitat+Escapement_Y+Space_Use, data=Growth, random = list(Sample_ID=~1), correlation = corAR1(form = ~ Inc_distance|Sample_ID))#with Origin, Habitat, Year, Life History. dAIC = 4
##### with Origin, Year. dAIC = 17 #######
# gamm_model2<-gamm(Avg_width ~ s(Inc_distance)+Origin+Escapement_Y, data=Growth, random = list(Sample_ID=~1), correlation = corAR1(form = ~ Inc_distance|Sample_ID))
#####  with Origin. dAIC = 39 ##### 
# gamm_model3<-gamm(Avg_width ~ s(Inc_distance)+Origin, data=Growth, random = list(Sample_ID=~1), correlation = corAR1(form = ~ Inc_distance|Sample_ID))
#####  with no covariates. dAIC = 48 ##### 
# gamm_model4<-gamm(Avg_width ~ s(Inc_distance), data=Growth, random = list(Sample_ID=~1), correlation = corAR1(form = ~ Inc_distance|Sample_ID))
beep()
summary(gamm_model[[2]]) 
coeff<-as.vector(summary(gamm_model[[2]])[1]$p.coeff[2:8])
sd<-as.vector(summary(gamm_model[[2]])[2]$se[2:8])
Covariate<-c("Hatchery-origin", 'Hatchery', 'Lassen Tributaries', 'American River', 'Delta','Ocean','2019')
coeffvals<-as.data.frame(cbind(coeff,sd,Covariate))
coeffvals$Covariate<- factor(coeffvals$Covariate, levels=c('2019', 'Ocean', 'Delta','American River','Lassen Tributaries','Hatchery','Hatchery-origin'))
coeffvals$category<-c("origin",rep("habitat", 5), rep("year",1))
ggplot(coeffvals, aes(x = Covariate, y = as.numeric(coeff)))+
  geom_point(size = c(3,2,3,2,2,3,3))+
  ylim(-.45,.45)+
  geom_errorbar(aes(ymin=as.numeric(coeff)-as.numeric(sd), ymax=as.numeric(coeff)+as.numeric(sd)), size = c(2,1,2,1,1,2,2)/2)+
  ylab("Estimate")+
  facet_grid(~category, 
             scales = "free_x", # Let the x axis vary across facets.
             space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x") +
  geom_hline(yintercept=0, 
               color = "red", size=.5)+
  scale_y_continuous(position = "right")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.text.y = element_text(angle = 90, vjust = 0.5, hjust=0.6),
        strip.text.x = element_text(angle = 180),axis.title.x = element_text(angle=180))
# ggsave("Fig5.jpeg", plot = last_plot(), width = 3 , height = 7)
gamran<-gamm_model$lme
acf(resid(gamm_model$lme, type = "normalized"))
hist(resid(gamm_model$lme, type = "normalized"))
qqnorm(resid(gamm_model$lme, type = "normalized"))
qqline(resid(gamm_model$lme, type = "normalized"))
##########################################
#Size of fish at hatchery release age 118
At118<-Microstructure %>%
  filter(Origin == "Natural" & Inc_no == 118) 
mean(At118$Inc_distance)
sd(At118$Inc_distance)
#Growth plots
#Removing hatchery samples that have no Sac to distinguish Hatchery versus Delta from Hatchery and Delta count
Sac<-unique(Microstructure$Sample_ID[which(Microstructure$Habitat == "Sacramento River" )])
Nomigrants<-Microstructure[!(Microstructure$Habitat == "Hatchery" & !Microstructure$Sample_ID %in% Sac),]
Nomigrants<-Nomigrants[!(Nomigrants$Habitat == "Delta" & !Nomigrants$Sample_ID %in% Sac),]
##########################################
#Mean growth by increment by habitat and origin
GrowthbyInc<- Nomigrants %>%
  filter(Inc_no != 0 & Inc_no != 1 ) %>%
  filter(Between_inc_dist < 7) %>%
  group_by(Habitat, Origin, Inc_no, Sample_ID) %>%
  summarise(Avg_width_persamp = mean(Between_inc_dist)) %>%
  group_by(Habitat, Origin, Inc_no) %>%
  summarise(Avg_width = mean(Avg_width_persamp), N = n(), lower = quantile(Avg_width_persamp, probs= .25),upper = quantile(Avg_width_persamp, probs= .75) )%>%
  filter(N > 10)

#By habitat
a<-ggplot(GrowthbyInc %>% filter (Habitat == "Hatchery"|Habitat == "Sacramento River"), aes(y = Avg_width, x = Inc_no, group = interaction(Habitat,Origin), col = interaction(Habitat,Origin)))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = interaction(Habitat,Origin)), alpha=0.2,linetype = 0)+
  scale_fill_manual(values = c("Hatchery.Hatchery"="#a67300","Sacramento River.Hatchery"= "#E69F00", "Sacramento River.Natural"="#56B4E9"), name = "", labels = c("Hatchery (in Hatchery)", " Hatchery (in mainstem)", "Natural")) +
  xlim(1,200)+
  ylim(1.5, 4.1)+
  xlab("Otolith Increment")+
  ylab("Increment width (um)")+
  annotate("text", x=36, y=4, label="a) Hatchery and mainstem", size=5,fontface = 'italic')+
  scale_color_manual(values=c("grey","#E69F00","#56B4E9"), name = "", labels = c("Hatchery (in Hatchery)", " Hatchery (in mainstem)", "Natural"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")
b<-ggplot(GrowthbyInc %>% filter (Habitat == "Delta"), aes(y = Avg_width, x = Inc_no, group = Origin, col = Origin))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = Origin), alpha=0.2,linetype = 0)+
  scale_fill_manual(values = c("Hatchery"="#E69F00","Natural"="#56B4E9"), name = "")+
  xlim(1,200)+
  ylim(1.5, 4.1)+
  xlab("Otolith Increment")+
  ylab("Increment width (um)")+
  annotate("text", x=20, y=4, label="b) Delta/Feather", size=5,fontface = 'italic')+
  scale_color_manual(values=c("#E69F00","#56B4E9"), name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")
c<-ggplot(GrowthbyInc %>% filter (Habitat == "Ocean"), aes(y = Avg_width, x = Inc_no, group = Origin, col = Origin))+
  geom_line(size = 2)+
  geom_ribbon(aes(ymin=lower, ymax=upper, fill = Origin), alpha=0.2,linetype = 0)+
  scale_fill_manual(values = c("Hatchery"="#E69F00","Natural"="#56B4E9"), name = "")+
  xlim(1,200)+
  ylim(1.5, 4.1)+
  xlab("Otolith Increment")+
  ylab("Increment width (um)")+
  annotate("text", x=23, y=4, label="c) Estuary/Ocean", size=5,fontface = 'italic')+
  scale_color_manual(values=c("#E69F00","#56B4E9"), name = "")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")
plot_grid(a,b,c, nrow=3)
# ggsave("Fig6.jpg", plot = last_plot(), width = 8, height = 10)