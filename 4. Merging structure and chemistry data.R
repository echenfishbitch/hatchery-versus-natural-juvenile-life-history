library(dplyr)
library(beepr)
setwd("C:/Users/echen/Box/Sacramento Winter run Chinook/Otoliths/")
Microchemistry18<-read.csv("Interpolated Microchemistry with Habitat 2018.csv")
Microchemistry19<-read.csv("Interpolated Microchemistry with Habitat 2019.csv")
Microchemistry<-rbind(Microchemistry18, Microchemistry19)
Microstructure<-read.csv("AllMicrostructure_data.csv")
############################################
#remove isotope samples that don't have a corresponding microstructure sample and vice versa
Microchemistry<- Microchemistry%>%
  filter(Sample_ID %in% intersect(unique(Microchemistry$Sample_ID),unique(Microstructure$Sample_ID))) 
Microstructure<- Microstructure%>%
  filter(Sample_ID %in% intersect(unique(Microstructure$Sample_ID),unique(Microchemistry$Sample_ID)))  %>%
  filter(Analysis_no == 1)
#Finding the nearest isotope measurement for each increment
for(i in 1:length(Microstructure$Inc_distance)){ #length(Microstructure$Inc_distance)
  Microchemistry_Sample<- Microchemistry %>%
    filter(Microchemistry$Sample_ID == Microstructure$Sample_ID[i])
  #only find nearest chem measurement from sample ID's microchemistry sample
  Microstructure$Sr8786[i]<-Microchemistry_Sample$Sr8786_norm[which.min(abs(Microchemistry_Sample$Distance_um - Microstructure$Inc_distance[i]))]  
  Microstructure$Distance_um[i]<-Microchemistry_Sample$Distance_um[which.min(abs(Microchemistry_Sample$Distance_um - Microstructure$Inc_distance[i]))]
  Microstructure$Habitat[i]<-Microchemistry_Sample$Habitat[which.min(abs(Microchemistry_Sample$Distance_um - Microstructure$Inc_distance[i]))]
  Microstructure$Origin[i]<-Microchemistry_Sample$Origin[1]
  
}
beep("mario")
Microstructure<-Microstructure %>%
  mutate(lineup_diff = Distance_um - Inc_distance) %>%
  filter(lineup_diff < 2) #removing part of one sample where early chemistry measurements weren't taken
#Removing ones where we stopped microchem measurements early
Microstructure<-Microstructure[which(Microstructure$lineup_diff > -1),]
Microchemistry<-Microstructure %>%
  select(Sample_ID, Escapement_Y, Origin,Inc_no, Inc_distance, Between_inc_dist, Sr8786, Habitat)
# write.csv(Microchemistry, "Microstructure data with chemistry.csv", row.names = FALSE)
