###############################
###################################
#
# 16_ Species symbol NCP
#
###################################
###############################

### Authors: Pierre-Louis Rey
### Contact: pierre-louis.rey@unil.ch

PATH<-"YOUR_PATHWAY"
setwd(PATH)

# Packages required
library(readxl)
library(tidyverse)
library(rvest)
source('google-search-fx.R') #function to search correctly a Google URL, contains equally the "google.url" function

# AIM: To quantify the number of results reflecting which species are considered like "cultural icon", "emblematic species", determining a social cohesion in switzerland. 
#      This R analytical approach is based on the Google analytics, more precisely on the count of each link (french, italian, deutsch, english language with a ".ch" domain) containing all following terms: 
#      "scientific name"; "emblematic", "switzerland" and "nature".    
#
#      Each taxonomic group (plant and vertebrate) was treated seperately.


# To obtain a connection with your search tool

get_hit_count <- function(google.url) {
  # setting user agent! Check link if issue
  # se <- session("https://httpbin.org/user-agent") 
  # UA <- se$response$request$options$useragent
  UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.124 Safari/537.36"
  httr::set_config(httr::user_agent(UA))
  
  # getting hit count
  raw.count <- google.url %>% 
    session() %>% 
    html_nodes(xpath="//div[@id='result-stats']") %>% 
    html_text()
  
  # cleaning out text to have only hits 
  semi.raw.count <- strsplit(raw.count, "\\D+") %>% unlist
  hit.count.char <- semi.raw.count %>% 
    head(-2) %>% 
    tail(-1)
  hit.count <- paste(hit.count.char, collapse ="") %>% as.numeric()
  hit.count
}


#
### Create the loop to obtain the number of query based on our own URL content
#

DB <- read_excel("ALL_NAMES.xlsx") # species names (Scientific names, french names, italian names, german names, and english names), taxonomic group and class
TOT<-data.frame(NAME=DB[,"SCIENTIFIC NAME"],EN=as.numeric(0),DE=as.numeric(0),FR=as.numeric(0),IT=as.numeric(0),Count.google=as.numeric(0),Count.google.outEN=as.numeric(0), DB[,c("CLASSIS","ORGANISMAL Group")]) # Final data to complete with the loop

for(i in 1:nrow(DB)){

  # Searching for "Naturpark" in English
  S.terms<-paste0("'","'",DB[i,"EN NAME"],"'","+'emblematic'+'Switzerland'+'nature'","'")
  search.url.EN <- get_search_url(search.term=S.terms,domain = ".ch", domain.site=".ch",language = 'en', quotes=F, n.pages=1)
  TOT[i,"EN"]<-get_hit_count(search.url.EN)
  #Sys.sleep(sample(seq(1, 20, by=0.1), 1))
  
  # Searching for "Naturpark" in German
  S.terms<-paste0("'","'",DB[i,"DE NAME"],"'","+'symbolträchtig'+'Schweiz'+'natur'","'")
  search.url.DE <- get_search_url(search.term=S.terms,domain = ".ch", domain.site=".ch", quotes=F, n.pages=1)
  TOT[i,"DE"]<-get_hit_count(search.url.DE)
  #Sys.sleep(sample(seq(1, 20, by=0.1), 1))
  
  # Searching for "Naturpark" in French
  S.terms<-paste0("'","'",DB[i,"FR NAME"],"'","+'emblématique'+'Suisse'+'nature'","'")
  search.url.FR <- get_search_url(search.term=S.terms,domain = ".ch", domain.site=".ch",language = 'fr', quotes=F, n.pages=1)
  TOT[i,"FR"]<-get_hit_count(search.url.FR)
  #Sys.sleep(sample(seq(1, 20, by=0.1), 1))
  
  # Searching for "Naturpark" in Italian
  S.terms<-paste0("'","'",DB[i,"IT NAME"],"'","+'emblematico'+'Svizzera'+'natura'","'")
  search.url.IT <- get_search_url(search.term=S.terms,domain = ".ch", domain.site=".ch",language = 'it', quotes=F, n.pages=1)
  TOT[i,"IT"]<-get_hit_count(search.url.IT)
  
  TOT[i,"Count.google"]<-sum(TOT[i,2:5], na.rm=T)
  TOT[i,"Count.google.outEN"]<-sum(TOT[i,3:5], na.rm=T)
  #Sys.sleep(sample(seq(1, 20, by=0.1), 1))
  print(i)
}
#save.image("yourpathway.RData")

# Evaluation of NCP - 0 if value< 3rd quartile, else 1
#
# After summing each links for each species, if  the species had a value upper than the 3rd quartile of the number of links for the entire of the taxonomic group, 
# the species obtains a positive link (+1) with the NCP,  if else species obtains a neutral link (0). 

vert<-TOT[TOT$ORGANISMAL.Group=="Vertebrates",c("SCIENTIFIC.NAME","Count.google")]
trach<-TOT[TOT$ORGANISMAL.Group=="Tracheophytes",c("SCIENTIFIC.NAME","Count.google")]

vert$score<-NA
for (i in 1:nrow(vert)){
if(vert[i,"Count.google"]<summary(vert$Count.google)[[5]]){ #summary(vert$Count.google)[[5]]) corresponds to the third quartile value
  vert[i,"score"]<-0}
  else {vert[i,"score"]<-1}
  }

trach$score<-NA
for (i in 1:nrow(trach)){
  if(trach[i,"Count.google"]<summary(trach$Count.google)[[5]]){ #summary(vert$Count.google)[[5]]) corresponds to the third quartile value
    trach[i,"score"]<-0}
  else {trach[i,"score"]<-1}
}

# Save datas
fwrite(trach,"16_SpeciesSymbol_tracheophytes.csv")
fwrite(vert,"16_SpeciesSymbol_vertebrates.csv")
