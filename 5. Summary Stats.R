#########################################
#### Summary Microchemistry Stats #######
#########################################
setwd("C:/Users/echen/Box/Sacramento Winter run Chinook/Otoliths")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(cowplot)
Microchemistry07<-read.csv("Interpolated Microchemistry with Habitat 2007.csv")
Microchemistry08<-read.csv("Interpolated Microchemistry with Habitat 2008.csv")
Microchemistry09<-read.csv("Interpolated Microchemistry with Habitat 2009.csv")
Microchemistry15<-read.csv("Interpolated Microchemistry with Habitat 2015.csv")
Microchemistry16<-read.csv("Interpolated Microchemistry with Habitat 2016.csv")
Microchemistry17<-read.csv("Interpolated Microchemistry with Habitat 2017.csv")
Microchemistry18<-read.csv("Interpolated Microchemistry with Habitat 2018.csv")
Microchemistry19<-read.csv("Interpolated Microchemistry with Habitat 2019.csv")
Microchemistry<-rbind(Microchemistry07, rbind(Microchemistry08, rbind(Microchemistry09, rbind(Microchemistry15,
                                                                                     rbind(Microchemistry16, rbind(Microchemistry17, rbind(Microchemistry18, Microchemistry19)))))))
#Sample Size  
length(unique(Microchemistry$Sample_ID)) #Total sample size
length(unique(Microchemistry$Sample_ID[which(Microchemistry$Origin == "Natural")])) #Natural sample size
length(unique(Microchemistry$Sample_ID[which(Microchemistry$Origin == "Hatchery")])) #Hatchery sample size
############## Categorizing life histories   #################3
#American River rearers
AMEfishlist<-unique(Microchemistry$Sample_ID[which(Microchemistry$Habitat == "American River")])
#Lassen tributaries rearers
LASfishlist<-unique(Microchemistry$Sample_ID[which(Microchemistry$Habitat == "Lassen Tributaries")])
#Hatchery fish that had measurements in Sac
HatSlowfishlist<-unique(Microchemistry$Sample_ID[which(Microchemistry$Origin == "Hatchery" & Microchemistry$Habitat == "Sacramento River")])
#Delta rearers
MicrochemistryDELN<-Microchemistry %>% #Natural Delta rearers
  filter(Habitat == "Delta" & Origin == "Natural") %>%
  group_by(Sample_ID) %>%
  summarise(Dif = max(Distance_um) - min(Distance_um)) %>%
  filter(Dif >= 120)
MicrochemistryDELH<-Microchemistry %>% #Hatchery Delta rearers
  filter(Sample_ID %in% HatSlowfishlist & Habitat == "Delta") %>%
  group_by(Sample_ID) %>%
  summarise(Dif = max(Distance_um) - min(Distance_um)) %>%
  filter(Dif >= 120)
DELFEAfishlist<-c(MicrochemistryDELN$Sample_ID, MicrochemistryDELH$Sample_ID) 
Microchemistry<- Microchemistry %>%
  mutate(Space_Use= as.factor(ifelse(Sample_ID %in% DELFEAfishlist, "Delta/Feather",  ifelse(Sample_ID %in%AMEfishlist, "American River", ifelse(Sample_ID %in%LASfishlist, "Lassen tributaries","Sacramento mainstem only" ))))) 
####### Habitat Use ##################
Life_History<-Microchemistry %>%
  select(Sample_ID, Space_Use, Escap_yr, Origin) %>%
  distinct()
#Life History of Natural Origin fish overall
Life_History %>%
  filter(Origin == "Natural") %>%
  group_by(Escap_yr, Space_Use) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  group_by(Space_Use) %>%
  summarise(average = mean(freq), sd = sd(freq))
#Life History of Hatchery fish overall
Life_History %>%
  filter(Origin == "Hatchery") %>%
  filter(Sample_ID %in% HatSlowfishlist) %>%
  group_by(Escap_yr, Space_Use) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  group_by(Space_Use) %>%
  summarise(average = mean(freq), sd = sd(freq))

#Life History of Natural Origin fish each year
LH_Nat_year<-Life_History%>%
  filter(Origin == "Natural") %>%
  group_by(Escap_yr, Space_Use) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))
#Life History of Hatchery fish each year
LH_Hat_year<-Life_History%>%
  filter(Origin == "Hatchery") %>%
  filter(Sample_ID %in% HatSlowfishlist) %>%
  group_by(Escap_yr, Space_Use) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n))
#Life History plots
a<-ggplot(LH_Hat_year, aes(fill=Space_Use, y=freq, x=as.factor(Escap_yr))) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("#98A799","#D9E4F4","#F6F4CD", "#CAE2C2"))+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 0),
        legend.position = "none")+
  labs(x="Escapement Year", y="Proportion",fill = "Juvenile Habitat Use")
b<-ggplot(LH_Nat_year, aes(fill=Space_Use, y=freq, x=as.factor(Escap_yr))) +
  geom_bar(position="fill", stat="identity")+
  scale_fill_manual(values = c("#98A799","#D9E4F4","#F6F4CD", "#CAE2C2"))+
  theme(plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x = element_text(angle = 0))+
  labs(x="Escapement Year", y="Proportion",fill = "Juvenile Habitat Use")
plot_grid(b,a, nrow=1)
####### Entry size to each habitat ##################
Entry_size<-Microchemistry %>%
  group_by(Sample_ID, Habitat,Origin) %>%
  summarise(Enter_Size = min(Distance_um)) %>%
  group_by(Habitat, Origin)%>%
  summarise(Mean_Enter_Size = mean(Enter_Size), sd = sd(Enter_Size)) 

#T-test whether HvW differ in Ocean entry size
ocean_size<-Microchemistry %>%
  group_by(Sample_ID, Habitat, Origin, Escap_yr) %>%
  summarise(Enter_Size = min(Distance_um)) %>%
  filter(Habitat == "Ocean")
sd(ocean_size$Enter_Size[which(ocean_size$Origin == "Hatchery")])
sd(ocean_size$Enter_Size[which(ocean_size$Origin == "Natural")])
sd(ocean_size$Enter_Size) #variation in entry size overall
model<-aov(ocean_size$Enter_Size~as.factor(ocean_size$Escap_yr)+ocean_size$Origin)
summary(model)
ocean_size %>% #average entry size by year
  group_by(Escap_yr) %>%
  summarise(mean = mean(Enter_Size))
ggplot(ocean_size, aes(x=Enter_Size, fill=as.factor(Origin)))+
  geom_density(alpha=.5)+
  annotate("text", x=300, y=.008, label="a)", size = 8)+
  xlim(300,1000)+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top")+
  labs(x="Otolith radius (um)")
ocean_size<-ocean_size[,c(1,5)]
write.csv(ocean_size, "Ocean_entry_size.csv", row.names = FALSE) # for microstructure measurements

####### Hatchery migrants versus rearers ##################
Hatchery<-Microchemistry %>%
  filter(Origin == "Hatchery") %>%
  mutate(Migration_speed = ifelse(Sample_ID %in% HatSlowfishlist, "Sacramento rearing", "Fast migrant")) %>%
  select(Sample_ID, Migration_speed, Escap_yr)%>%
  distinct() %>%
  group_by(Escap_yr,Migration_speed) %>%
  summarise(n = n()) %>%
  mutate(freq = n/sum(n)) %>%
  filter(Migration_speed == "Sacramento rearing") %>%
  filter(Escap_yr != 2008)
1-mean(Hatchery$freq)
###########################################
###########       Profile plots    #############
###########################################
#of the life history types
subset<-Microchemistry %>%
  filter(Space_Use == "Sacramento mainstem only" & Origin == "Natural")
subset<-Microchemistry %>%
  filter(Space_Use == "Lassen tributaries" & Origin == "Natural")%>%
  filter(!Sample_ID %in% AMEfishlist)
subset<-Microchemistry %>%
  filter(Space_Use == "American River" & Origin == "Natural")
subset<-Microchemistry %>%
  filter(Space_Use == "Delta/Feather" & Origin == "Natural") %>%
  filter(!Sample_ID %in% AMEfishlist)

#of hatchery life history types
subset<-Microchemistry %>%
  filter(Space_Use == "Sacramento mainstem only" & Origin == "Hatchery" & Sample_ID %in% HatSlowfishlist)
subset<-Microchemistry %>%
  filter(Space_Use == "Lassen tributaries" & Origin == "Hatchery"& Sample_ID %in% HatSlowfishlist)%>%
  filter(!Sample_ID %in% AMEfishlist)
subset<-Microchemistry %>%
  filter(Space_Use == "American River" & Origin == "Hatchery"& Sample_ID %in% HatSlowfishlist)
subset<-Microchemistry %>%
  filter(Space_Use == "Delta/Feather" & Origin == "Hatchery"& Sample_ID %in% HatSlowfishlist) %>%
  filter(!Sample_ID %in% AMEfishlist)

plotIDs<-sample(subset$Sample_ID,25)
Microchemistry_subset<-Microchemistry %>%
  filter(Sample_ID %in% plotIDs)
#HvW by year (Supplemental Figure)
Microchemistry_subset<-Microchemistry %>%
  filter(Origin == "Hatchery" & Escap_yr == 2015)
l<-ggplot(Microchemistry_subset, aes(y=Sr8786_norm, x=Distance_um, group=Sample_ID)) +
  labs(x="Otolith radius (um)", y="Sr87/86",color = "Origin")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.title = element_text(size = 20))+
  annotate("rect", xmin = 0, xmax =1000, ymin = .7035, ymax = 0.70467, alpha = .5, fill="#eeea99")+ #Lassen
  annotate("rect", xmin = 0, xmax =1000, ymin = 0.70467, ymax = 0.706, alpha = .5, fill="#96c685")+ #Sacramento
  annotate("rect", xmin = 0, xmax =1000, ymin = .706, ymax = 0.7082, alpha = .5, fill="#b3c9ed")+ #Delta
  annotate("rect", xmin = 0, xmax =1000, ymin = 0.7082, ymax = 0.7102, alpha = .5, fill="#325035")+ #American
  geom_line()+
  xlim(0, 1000)+
  ggtitle("Hatchery – 2015")+
  annotate("text", x=875, y=.7042, label="Lassen tributaries")+
  annotate("text", x=865, y=.7055, label="Sacramento mainstem")+
  annotate("text", x=865, y=.707, label="Hatchery/Delta/Feather")+
  # annotate("text", x=895, y=.707, label="Delta/Feather")+
  # annotate("text", x=775, y=.7096, label="Estuary/Ocean or American River")
  annotate("text", x=870, y=.7096, label="American River or")+
  annotate("text", x=870, y=.7085, label="Estuary/Ocean")

plot_grid(e,m,f,n,g,o,h,p, ncol = 2)
# ggsave("SuppFigure1b.png", plot=last_plot(), width=169*2.2, height = 380*1.2, units = "mm", device = "png")