###############################
###################################
#
# 01_ Potential crop_CWR NCP
#
###################################
###############################

### Authors: Pierre-Louis Rey
### Contact: pierre-louis.rey@unil.ch

PATH<-"YOUR_PATHWAY"
setwd(PATH)

# Packages required
library(readxl)
library(data.table)

CWR <- read_excel("01_CWR.xlsx") # list of Crop Wild Relatives (CWR) available on Boserup et al. 2021
View(CWR)

# To obtain species considered as useful for CWR after to have established the correspondance name established with Dr. Pascal Vittoz.
#
## not necessary if you have species with the good denomination, worked directly with the excel file. 
l<-CWR[,c("OLD","NEW")] 
n<-data.frame(unique(CWR[,"NEW"]))
CWR2<-l[l$OLD %in% CWR$CWR.list,]
n2<-data.frame(unique(CWR2$NEW))
n$CWR<-NA
n[which(n$NEW %in% n2$unique.CWR2.NEW.) ,"CWR"]<-1 # Obtain value +1 (positive link) between species and the NCP
n[which(is.na(n[,"CWR"])),"CWR"]<-0 # If else obtain 0 (neutral link) between species and the NCP

# Save data
fwrite(n,paste0(PATH,"01_Potentialcrop_CWR.csv"))
