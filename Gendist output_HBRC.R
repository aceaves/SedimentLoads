####################################################################################################
#Date: 24/01/2024
#Author: Tim Norris
#Description: Script developed to replicate tidyda files required for import into Sedrate via the Gendist tool.   

#Sys.setenv(https_proxy="http://wwwproxy.wairc.govt.nz:8080")

##Install libraries 

library(dplyr)
library(leaflet)
library(lubridate)
library(KiQSr)
library(data.table)
library(stringr)
library(readxl)
library(ggplot2)
library(KiQSr)
library(readxl)
library(data.table)
library(rlist)
library(writexl)
library(tidyr)
###################################################################################
#Pull in site information 

Siteinfo <- read_excel("I:/306 HCE Project/R_analysis/Rating curves/Gendist_flowDist/Updated sedrate inputs_HBRC.xlsx", 
                       sheet = "Site names") %>% filter(!is.na(`Site No.`))  %>% mutate(SiteNo = gsub("\\_", "", `Site No.`))

#test<- "516/22"

# "414/13/TSS/SSG.P"     Mangaokewa TSS
#  414/13/SSC/AS.P      Mangaokewa SSC
# "414/13/Flow/Cmd.P"    Mangaokewa Flow
##########################################################

##### The Sediment record function extracts all TSS/SSC data and the associated flow data. 
##### The final excel output is stored in the designated repository and is ready for input into Sedrate. 
##### Please note that input files should be checked carefully to unsure data is QC'd prior to entering into Sedrate. 

options(scipen = 100, digits = 4)
options(warn=-1) # Suppresses warnings to ensure that the function can be looped accross a lis of sites 



###Site summary table. This is required for the loop function. 
Siteinfo <- read_excel("I:/306 HCE Project/R_analysis/Rating curves/Gendist_flowDist/Updated sedrate inputs.xlsx", 
                       sheet = "Site names") %>% filter(!is.na(`Site No.`))  %>% mutate(SiteNo = gsub("\\_", "", `Site No`))



##### Select site from 
selected_site <- "222/16"
Site <- filter(Siteinfo, `WISKI Code`  == selected_site)
Site_Name <- unique(Site$SiteNo) 
name <- paste0("              ",Site_Name, "              1 INSTANT" )  #### this replicates the Tidyda formatting


####Import flow data 
flow <- read_excel("I:/306 HCE Project/R_analysis/Rating curves/Gendist_flowDist/Kaniwhaniwha_22216_1.xlsx", 
                   col_types = c("date", "date", "numeric")) %>%  
  mutate(Date = as.Date(Date), Time1 = paste0(Time)) %>% mutate(Time = substr(Time1, start = 12, stop = 19)) %>% 
  mutate(Time = ifelse(Time == "", "00:00:00", Time)) %>% 
  mutate(value = value * 1000)    %>% mutate(timestamp = paste0(Date, Time)) %>% 
  mutate(timestamp = as.POSIXct(timestamp, "%Y-%m-%d %H:%M:%S")) %>%   select(timestamp, Date, Time, value) 


#####Format flow data 
flow1 <- flow %>%  mutate(  Date = as.Date(timestamp), Time = substr(timestamp, start = 12, stop = 19)) %>% mutate(value = value * 1000) %>%  mutate(value1 = as.numeric(paste0(round(value,0)))) %>%  mutate(value1 = ifelse(is.na(value1), "", value1)) %>%  mutate(col1= "  ") %>%
                   mutate(Time = ifelse(Time == "", "00:00:00", Time)) %>%  
                   mutate(Time = gsub("\\:", "", Time), Date = gsub("\\-", "", Date)) %>% select( col1, value1, Date, Time) %>% 
                   mutate(Date = ifelse(value1 == "", "", Date)) %>% 
                   mutate(Time = ifelse(value1 == "", "", Time)) %>% 
                   mutate(col1 = ifelse(value1 == "", name, col1))  
                  
  


#######Table for export 
Final <-  unite(flow1, col= "test", c('col1', 'value1', 'Date', 'Time'), sep="    ",  remove = TRUE, na.rm = FALSE)




####rename (merged) column heading 
colnames(Final) <- name                                           

   
########Export text file                                          
Final1 <- as.data.frame(sapply(Final , function(x) gsub("\"", "", x)))
write.table(mapply(format, Final1, justify=c("right")), "C:/Users/timn/Desktop/Gendist_flowDist/test.txt", row.names = FALSE, quote=F)






