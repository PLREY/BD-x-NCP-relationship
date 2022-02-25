###############################
###################################
#
# 15_ Scientific interest NCP
#
###################################
###############################

### Authors: Pierre-Louis Rey
### Contact: pierre-louis.rey@unil.ch



# devtools::install_github("juba/rwos")
# https://github.com/juba/rwos/blob/master/examples.R
# code for query: https://apps.webofknowledge.com/WOS_AdvancedSearch_input.do?SID=E1T3XOerV2aP4t335VB&product=WOS&search_mode=AdvancedSearch

PATH<-"YOUR_PATHWAY"
setwd(PATH)

# Packages required
x<-c("data.table","rwos","readxl","stringr")
lapply(x, require, character.only = TRUE)

# AIM: Obtain the number of records for each species into the website "Web of Science" (website storing all types of scientific publications) to known the scientific interest

SP <- read_excel("your_species_list.xlsx") # Based on the species list containing the species name and the taxonomic group of the species ("vertebrates" and "plantes" in this study) 

SP.pl<-subset(SP,SP$GR=="Plantes")
SP.pl<-data.frame(spp=word(SP.pl$spp,1,2))
SP.ve<-subset(SP,SP$GR=="Vertebrates")

# RWOS loop
# we use a delay function because we can ask only a request of 2  query per sec.

sid=wos_authenticate(username = NULL, password = NULL, url = "http://search.webofknowledge.com") #Your token id for WoS


cit<-1
Pl.rec.cit<-data.table() #data table for tracheophytes
Ve.rec.cit<-data.table() #data table for vertebrates

# This query (that can be submitted directly species by species with the site web of science - https://www.webofscience.com/wos/woscc/advanced-search) allows to search the scientific name of each species
# into an article, book or book chapter stored between 1970-2020 range into web of science. 
while(cit <= length(SP.pl$spp)){
    Query<-paste0("((TS=(",SP.pl[cit,"spp"]," SAME species) OR TI=(",SP.pl[cit,"spp"],") OR AK=(",SP.pl[cit,"spp"],") AND PY=(1970-2020)) AND (DT=(Article OR Book OR Book Chapter)))") # The query
    wos<-wos_search(sid=sid, query=Query)
    wos.tmp<-wos$results
    wos.tmp<-data.table(spp=SP.pl[cit,"spp"],records=wos.tmp[[1]])
    Pl.rec.cit<-rbind(Pl.rec.cit,wos.tmp)
  cit<-cit+1
  print(paste("    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=",cit,"-",SP.pl[cit,"spp"],"DONE -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="))
  Sys.sleep(1.2)
}

# Save data
fwrite(Pl.rec.cit,"15_ScientificInterest_tracheophytes.csv")
fwrite(Ve.rec.cit,"15_ScientificInterest_vertebrates.csv")

# N.B: After summing each publications for each species, if  the species had a value upper than the 3rd quartile of the number of publications for the entire of the taxonomic group, 
#      the species obtains a positive link (+1) with the NCP,  if else species obtains a neutral link (0). This operation is made with the excel tool.

