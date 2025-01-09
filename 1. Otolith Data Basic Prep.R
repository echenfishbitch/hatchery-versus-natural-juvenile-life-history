library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(gridExtra)

setwd("C:/Users/echen/Box/Sacramento Winter run Chinook/Otoliths/")
################################################
#########    Microstructure      ###############
################################################
#Cleaning increment data collected for 2018 and 2019
Microstructure_1819<-read.csv('Microstructure Measurements.csv') #From Google Sheet
Microstructure_1819$Inc_no[which(Microstructure_1819$Inc_no == 'EXOG')]<-0
Microstructure_1819$Inc_no<-as.numeric(Microstructure_1819$Inc_no)
Samples<-read.csv('Transect Angles.csv')
colnames(Samples)[1]<-"Sample_ID"
Microstructure_1819<-Microstructure_1819 %>%
  left_join(Samples)%>%
  filter
  mutate(Transect.angle = as.numeric(Transect.angle))
#samples where 2 transects were taken, using specific transect angle for that region of interest
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_5028")]<-c(rep(4, 174), rep(0, 24))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "WR18_5467")]<-c(rep(13, 102), rep(2, 255-102))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "WR18_7090")]<-c(rep(17, 111), rep(12, 205-111))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "WR18_7127")]<-c(rep(0, 145), rep(1, 242-145))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "WR18_7362")]<-c(rep(8, 83), rep(27, 194-83))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_5047")]<-c(rep(14, 141), rep(24, 205-141))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_5668")]<-c(rep(22, 146), rep(20, 204-146))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_5157")]<-c(rep(2, 129), rep(16, 204-129))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_7082")]<-c(rep(2, 108), rep(24, 196-108))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_7572")]<-c(rep(18, 189), rep(3, 201-189))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_5045")]<-c(rep(5, 105), rep(10, 202-105))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_80117")]<-c(rep(22, 92), rep(0, 201-92))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_7121")]<-c(rep(18, 135), rep(8, 201-135))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "EE19_9085")]<-c(rep(12, 120), rep(6, 208-120))
Microstructure_1819$Transect.angle[which(Microstructure_1819$Sample_ID == "WR18_5488")]<-c(rep(13, 175), rep(17, 203-175))
Microstructure_1819<-Microstructure_1819 %>%
  mutate(Inc_distance = Inc_distance*cos(Transect.angle*pi/180)) %>% #revising inc_distance
  mutate(Between_inc_dist = NA)
for(i in 2:45695){ #calculating distance between increments 
  Microstructure_1819$Between_inc_dist[i] = Microstructure_1819$Inc_distance[i] - Microstructure_1819$Inc_distance[i-1]
}
Microstructure_1819<-Microstructure_1819 %>% #negatives include first values and jumps from one transect to the next
  mutate(Between_inc_dist = ifelse(Between_inc_dist < 0 , NA, Between_inc_dist))
write.csv(Microstructure_1819, "AllMicrostructure_data.csv", row.names = FALSE)
################################################
#########     Microchemistry     ###############
################################################
#Merging 2018 and 2019 Sr Data
Microchemistry_old<-read.csv("0717 SrRatios.csv") #Reading in product of 1_data_compilation.R
Microchemistry_1819 <- read.csv("1819 SrRatios.csv") #Reading in 18,2019 microchemistry data
Microchemistry<-rbind(Microchemistry_old,Microchemistry_1819)
################################################
#########    Demographic Data    ###############
################################################
CWTdata<-read.csv("CWTcodes.csv")
CWTdata<-CWTdata %>%
  mutate(Origin = ifelse(is.na(CWT.Code), "Natural", "Hatchery")) %>% 
  select(Year, Sample.Number)%>%
  mutate(Sample.Number = as.character(Sample.Number))
colnames(CWTdata)[1]<-c("Escap_yr")
#Getting agency ID from lab ID
for(i in 1:nrow(Microchemistry)){
  Microchemistry$Sample.Number[i]<-ifelse(Microchemistry$Escap_yr[i] == 2016, sub('.......', '', Microchemistry$Sample_ID[i]), sub('.....', '', Microchemistry$Sample_ID[i]))
}
Microchemistry<-Microchemistry %>%
  left_join(CWTdata) 
#############    Origin Data     ################
Hatchery<-read.csv("Hatchery Fish IDs.csv")
#appending fish that had adipose fins but clearly hatchery microchemistry
additions<-as.data.frame(c("WR15-80413","WR15-80508","WR15-80535","WR15-80542","WR15-80695","WR15-80703","WR15-80640","WR15-80693","WR15-80707","WR15-80709","WR15-80717",
                                 "WR18_5430",  "WR18_7538",  "EE19_5089",  "EE19_7876",  "EE19_80178", "WR18_7566","EE19_5323","EE19_5706","WR18_80610"))
colnames(additions)<-c("Sample_ID")
Hatchery<-rbind(Hatchery, additions)
Microchemistry<-Microchemistry %>% 
  mutate(Origin = as.factor(ifelse(Microchemistry$Sample_ID %in% Hatchery$Sample_ID, "Hatchery", "Natural")))%>%
  left_join(Hatchery) %>%
  select(Sample_ID, Distance_um,Sr8786_norm, SrV, SrV.notes, Escap_yr,Origin)
# write.csv(Microchemistry, "AllMicrochemistryData.csv", row.names = FALSE)
#########################################################
####         Interpolating Sr8786 between           #####
#############     measured points        ################
#########################################################
#interpolate is a function that creates blank space for every sample and merges it
#interpolate expects the Sample ID and adds the blank space for that sample to Microchemistry file
interpolate<-function(SampleID){
  series<-seq(
    floor(min(Microchemistry$Distance_um[Microchemistry$Sample_ID == SampleID]))+1,
    ceiling(max(Microchemistry$Distance_um[Microchemistry$Sample_ID == SampleID]))-1)
  range_space_ID<-as.data.frame(cbind(rep(SampleID, length(series)), series,  rep(NA, length(series)),  rep(NA, length(series))))
  colnames(range_space_ID)<-c("Sample_ID", "Distance_um","Sr8786_norm", "SrV")
  range_space_ID$Distance_um<-as.numeric(range_space_ID$Distance_um)
  range_space_ID
}
Samples<-unique(Microchemistry$Sample_ID)
range_space<-lapply(Samples, interpolate)

# For each interpolated point, find the lower and upper bound of actual bond. Then use the approx function
# to find Sr for that radius point. Each sample is it's own item in a list. 
#Skip interpolating points that have a measured value and just use the measured value.
for(i in 1:length(Samples)){
  for(j in 1:length(range_space[[i]]$Sample_ID)){ #for each interpolated point j
    #what is the closest lower measurement
    lower_lim<-max(Microchemistry$Distance_um[which(Microchemistry$Distance_um <= range_space[[i]]$Distance_um[j] & Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j])])
    #what is the closest upper measurement
    upper_lim<-min(Microchemistry$Distance_um[which(Microchemistry$Distance_um >= range_space[[i]]$Distance_um[j] & Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j])])
########## Sr8786
     #find the point if the interpolated point isn't already measured
    interpolated_Sr8786<-if(lower_lim != upper_lim){
      approx(c(lower_lim, upper_lim),
             c(Microchemistry$Sr8786_norm[which(Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j] & Microchemistry$Distance_um == lower_lim)][1], #the [1] is for some samples that have duplicates on the same spot but I've checked them and they are duplicate SrValues
               Microchemistry$Sr8786_norm[which(Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j] & Microchemistry$Distance_um == upper_lim)][1]),
             xout=range_space[[i]]$Distance_um[j])
    }
    else(NA) #skip ones that have actual measures
    #use the actual measures if it's on a measured point, else take the interpolated value
    range_space[[i]]$Sr8786_norm[j]<-ifelse(lower_lim == upper_lim,
                                                Microchemistry$Sr8786_norm[which(Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j] & Microchemistry$Distance_um == upper_lim)],
                                                interpolated_Sr8786$y)
########## SrV    
    #find the point if the interpolated point isn't already measured
    interpolated_SrV<-if(lower_lim != upper_lim & !anyNA(Microchemistry$SrV[which(Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j][1])])){
      approx(c(lower_lim, upper_lim),
             c(Microchemistry$SrV[which(Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j] & Microchemistry$Distance_um == lower_lim)][1], #the [1] is for some samples that have duplicates on the same spot but I've checked them and they are duplicate SrValues
               Microchemistry$SrV[which(Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j] & Microchemistry$Distance_um == upper_lim)][1]),
             xout=range_space[[i]]$Distance_um[j])
    }
    else(NA) #skip ones that have actual measures
    #use the actual measures if it's on a measured point, else take the interpolated value, 
    #if its one of the samples without SrV, make that NA
    range_space[[i]]$SrV[j]<-ifelse(lower_lim == upper_lim ,
                                            Microchemistry$SrV[which(Microchemistry$Sample_ID == range_space[[i]]$Sample_ID[j] & Microchemistry$Distance_um == upper_lim)],
                                    ifelse(is.na(interpolated_SrV), NA, interpolated_SrV$y))
    
  }
}

#Compiling data from all samples into Interpolated
Interpolated<-NA
for(i in 1:length(Samples)){
  Interpolated<-rbind(Interpolated,range_space[[i]])
}
Interpolated<-Interpolated[-1,]

Interpolated <-Interpolated %>%
  left_join(distinct(Microchemistry %>% select(Sample_ID, Escap_yr, Origin)))
Interpolated$SrV<-as.numeric(Interpolated$SrV)
Interpolated$Sr8786_norm<-as.numeric(Interpolated$Sr8786_norm)
Interpolated07<-Interpolated %>%
  filter(Escap_yr == 2007)
write.csv(Interpolated07, "Interpolated Microchemistry 2007.csv", row.names = FALSE)
Interpolated08<-Interpolated %>%
  filter(Escap_yr == 2008)
write.csv(Interpolated08, "Interpolated Microchemistry 2008.csv", row.names = FALSE)
Interpolated09<-Interpolated %>%
  filter(Escap_yr == 2009)
write.csv(Interpolated09, "Interpolated Microchemistry 2009.csv", row.names = FALSE)
Interpolated15<-Interpolated %>%
  filter(Escap_yr == 2015)
write.csv(Interpolated15, "Interpolated Microchemistry 2015.csv", row.names = FALSE)
Interpolated16<-Interpolated %>%
  filter(Escap_yr == 2016)
write.csv(Interpolated16, "Interpolated Microchemistry 2016.csv", row.names = FALSE)
Interpolated17<-Interpolated %>%
  filter(Escap_yr == 2017)
write.csv(Interpolated17, "Interpolated Microchemistry 2017.csv", row.names = FALSE)
Interpolated18<-Interpolated %>%
  filter(Escap_yr == 2018)
write.csv(Interpolated18, "Interpolated Microchemistry 2018.csv", row.names = FALSE)
Interpolated19<-Interpolated %>%
  filter(Escap_yr == 2019)
write.csv(Interpolated19, "Interpolated Microchemistry 2019.csv", row.names = FALSE)
