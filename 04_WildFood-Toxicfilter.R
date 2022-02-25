###############################
###################################
#
# 04_ Wild Food NCP
#
###################################
###############################

### Authors: Pierre-Louis Rey
### Contact: pierre-louis.rey@unil.ch

PATH<-"YOUR_PATHWAY"
setwd(PATH)

# Packages required
library(data.table)
library(readxl)
library(stringr)

# AIM: Obtain a list of toxic species doesn't including species who could be consumable (e.g. exclude species as Vaccinuum myrtillum - consumable blue berry but toxic leaf )
toxic <- read_excel("04_WildFood_toxic.xlsx") # Data furnished by Infoflora extracted from Flora Helvetica book
VP <- read_excel("list_tracheophytes_correspondances_names.xlsx") # To obtain toxic species after to have established the correspondance name established with Dr. Pascal Vittoz.
## not necessary if you have species with the good denomination, worked directly with the excel file. 

toxic$short<-word(toxic$toxic,1,2,sep=" ")
VP$OLD_bis<-word(VP$OLD,1,2,sep=" ")
n<-data.frame(unique(VP[,2]))
TOX<-VP[VP$OLD_bis %in% toxic$short,]
n2<-data.frame(TOX=unique(TOX$NEW))
n$TOX<-NA
n[which(n$NEW %in% n2$TOX),"TOX"]<-"-1" # Obtain value -1 (negative link) between species and the NCP
n[which(is.na(n[,"TOX"])),"TOX"]<-0 # If else obtain 0 (neutral link) between species and the NCP

# Save data
fwrite(n,"04_WildFood_Toxicfilter.csv")
# N.B: a work between this new data table and the table containing the consumable species (+1 - positive link between species and the NCP) is necessary to obtain the final results of this NCP.
#      Remind, the consumable species list is established by bibliography only

