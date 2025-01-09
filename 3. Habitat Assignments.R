setwd("C:/Users/echen/Box/Sacramento Winter run Chinook/Otoliths/")
library(dplyr)
library(beepr)
#########################################################
###########           Habitat Assignments     ###########
#########################################################
Microchemistry07<-read.csv("Interpolated Microchemistry 2007.csv")
Microchemistry08<-read.csv("Interpolated Microchemistry 2008.csv")
Microchemistry09<-read.csv("Interpolated Microchemistry 2009.csv")
Microchemistry15<-read.csv("Interpolated Microchemistry 2015.csv")
Microchemistry16<-read.csv("Interpolated Microchemistry 2016.csv")
Microchemistry17<-read.csv("Interpolated Microchemistry 2017.csv")
Microchemistry18<-read.csv("Interpolated Microchemistry 2018.csv")
Microchemistry19<-read.csv("Interpolated Microchemistry 2019.csv")
Microchemistry<-rbind(Microchemistry07, rbind(Microchemistry08, rbind(Microchemistry09, rbind(Microchemistry15,
                  rbind(Microchemistry16, rbind(Microchemistry17, rbind(Microchemistry18, Microchemistry19)))))))
SrV<-read.csv("breakpoints.csv")
Microchemistry<-left_join(Microchemistry, SrV)
################## Assign to habitat ####################
#Basic assignment
Assign_Habitat<-function(Isotopic_Range, Origin, Distance){
  if(Origin == "Hatchery" & Distance < 350){ #Forcing early measurements of hatchery fish to be hatchery
    "Hatchery"
  }
  else if (Isotopic_Range > 0.7082){
    "Ocean"
  }
  else if (Isotopic_Range > .706){
    "Delta"
  }
  else if (Isotopic_Range > .70467){
    "Sacramento River"
  }
  else ("Lassen Tributaries")
}
Microchemistry$Habitat<-unlist(mapply(Assign_Habitat, 
                                      Microchemistry$Sr8786_norm,
                                      Microchemistry$Origin,
                                      Microchemistry$Distance_um))
#Adjustment 2
#Changing when fish are still have maternal signature, which shows up as Delta or Ocean. Changing Delta to Sacramento in these cases. 
#For Hatchery fish, Delta signals before Sacramento are the Hatchery
DelPre<-function(Habitat, Sample_ID, Distance, Origin){
  if(Habitat =="Delta"|Habitat =="Ocean"){
    if(Distance < max(Microchemistry$Distance_um[which(Microchemistry$Habitat == "Sacramento River"&
                                                       Microchemistry$Sample_ID == Sample_ID)])){
      if(Origin == "Natural"){
        "Sac"
      }
      else if(Origin == "Hatchery"){
        "Hat"
      }
      else("NA")
    }   
    else("NA")
  }
  else("NA")
}

Microchemistry$Habitat_Adjust1<-unlist(mapply(DelPre,
                                              Microchemistry$Habitat,
                                              Microchemistry$Sample_ID,
                                              Microchemistry$Distance_um,
                                              Microchemistry$Origin))
beep("mario")
#warnings are from when samples don't have SAC
for(i in 1:length(Microchemistry$Sample_ID)){
  if(Microchemistry$Habitat_Adjust1[i]=="Sac"){
    Microchemistry$Habitat[i]<-"Sacramento River"
  }
  if(Microchemistry$Habitat_Adjust1[i]=="Hat"){
    Microchemistry$Habitat[i]<-"Hatchery"
  }
}
#Adjustment 2
Microchemistry<-Microchemistry %>%
  mutate(Habitat_Adjust2 = ifelse(Distance_um >200 & Distance_um <= SrVbreak & Sr8786_norm > .7082, "AME", "NA")) %>%
  mutate(Habitat_Adjust2 = ifelse(Distance_um < 350 & Origin == "Hatchery","NA", Habitat_Adjust2)) %>%
  mutate(Habitat_Adjust2 = ifelse(is.na(Habitat_Adjust2),"NA", Habitat_Adjust2))
for(i in 1:length(Microchemistry$Sample_ID)){
  if(Microchemistry$Habitat_Adjust2[i]=="AME"){
    Microchemistry$Habitat[i]<-"American River"
  }
}
AMEfish<-unique(Microchemistry$Sample_ID[which(Microchemistry$Habitat_Adjust2 == "AME")])
#Adjustment 3 Delta measurements before AME are AME
DEL_AME<-function(Habitat, Sample_ID, Distance){
  if(Sample_ID %in% AMEfish){
    if(Habitat == "Delta" & Distance < min(Microchemistry$Distance_um[which(Microchemistry$Habitat == "American River"&
                                                                        Microchemistry$Sample_ID == Sample_ID)])){
      "AME"
      }
      else("NA")
    }   
    else("NA")
  }
Microchemistry$Habitat_Adjust3<-unlist(mapply(DEL_AME,
                                              Microchemistry$Habitat,
                                              Microchemistry$Sample_ID,
                                              Microchemistry$Distance_um))
for(i in 1:length(Microchemistry$Sample_ID)){
  if(Microchemistry$Habitat_Adjust3[i]=="AME"){
    Microchemistry$Habitat[i]<-"American River"
  }
}

#Adjustment 4 All measurements after Freshwater Exit are ocean
FWexit<-Microchemistry %>%
  filter(Habitat == "Ocean")%>%
  group_by(Sample_ID) %>%
  summarise(Exit_um = min(Distance_um))
Microchemistry<-Microchemistry %>%
  left_join(FWexit) %>%
  mutate(Exit_um = ifelse(is.na(Exit_um), 1500, Exit_um))
Microchemistry <- Microchemistry %>%
  mutate(Habitat = ifelse(Distance_um > Exit_um, "Ocean", Habitat))

Microchemistry<-Microchemistry %>%
  select(Sample_ID, Distance_um, Sr8786_norm, SrV, Escap_yr, Origin,Habitat)
Microchemistry07<-Microchemistry %>%
  filter(Escap_yr == 2007)
write.csv(Microchemistry07, "Interpolated Microchemistry with Habitat 2007.csv", row.names = FALSE)
Microchemistry08<-Microchemistry %>%
  filter(Escap_yr == 2008)
write.csv(Microchemistry08, "Interpolated Microchemistry with Habitat 2008.csv", row.names = FALSE)
Microchemistry09<-Microchemistry %>%
  filter(Escap_yr == 2009)
write.csv(Microchemistry09, "Interpolated Microchemistry with Habitat 2009.csv", row.names = FALSE)
Microchemistry15<-Microchemistry %>%
  filter(Escap_yr == 2015)
write.csv(Microchemistry15, "Interpolated Microchemistry with Habitat 2015.csv", row.names = FALSE)
Microchemistry16<-Microchemistry %>%
  filter(Escap_yr == 2016)
write.csv(Microchemistry16, "Interpolated Microchemistry with Habitat 2016.csv", row.names = FALSE)
Microchemistry17<-Microchemistry %>%
  filter(Escap_yr == 2017)
write.csv(Microchemistry17, "Interpolated Microchemistry with Habitat 2017.csv", row.names = FALSE)
Microchemistry18<-Microchemistry %>%
  filter(Escap_yr == 2018)
write.csv(Microchemistry18, "Interpolated Microchemistry with Habitat 2018.csv", row.names = FALSE)
Microchemistry19<-Microchemistry %>%
  filter(Escap_yr == 2019)
write.csv(Microchemistry19, "Interpolated Microchemistry with Habitat 2019.csv", row.names = FALSE)
