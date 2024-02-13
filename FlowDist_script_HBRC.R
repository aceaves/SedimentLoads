####################################################################################################
#Date: 22/12/2023
#Author: Tim Norris
#Description: Script developed to create flow distribution tables for long-term flow monitoring sites. The script extracts flow data from the WISKI database 
            # and generates table with 200 'bins'. The flow distribution tables are uploaded to the Sedrate access database (). A full description of the 
            # processing workflow can be found here:  

# Connect to the WRC proxy 
#Sys.setenv(https_proxy="http://wwwproxy.wairc.govt.nz:8080")

# Load packages 
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(reshape2)
library(writexl)
library(openxlsx)
library(tidyverse)
library(janitor)
library("stringr") 
library(multcompView)
library(RColorBrewer)
library(Cairo)
library(ragg)
library(gt)
library(writexl)
library(data.table)
library(dplyr)
#library(KiQSr)
library(Hilltop)

#########################################################################################################################
############ Data inputs  ################

#Set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/306 HCE Project/Sites/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")

# Get site list or measurement list for respective sites 
sitelist <- SiteList(dfile, "")
#measurementlist <- Hilltop::MeasurementList(dfile, sitelist)
Hilltop::SiteList(dfile)

# Date range. 
date1 <- "01-March-2022 00:00:00"
date2 <- "01-March-2023 00:00:00"

#Measurements/data that we want to pull from the Hilltop file 
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Flow") 

###############################################################################

#Loop 1 through sites-----------------------------------------------------------
name <- data.frame(sitelist)
name$Sites <- as.character(name$sitelist)

site_no <- length(sitelist)
site_id <- sitelist

method <- ""
interval <- ""

for(j in 1:site_no){
  Multiple_sites <- GetData(dfile, site_id[j] ,measurement, date1, date2) 
  # You will struggle to use this format in most packages 
  # do this to make it more useful
  Multiple_sites_id <- do.call(rbind, lapply(Multiple_sites, function(x) cbind(zoo::fortify.zoo(x),
                                                                               SiteName = attr(x, 'SiteName'), Measurement = attr(x, 'Measurement')))) %>% 
    dplyr::rename(zoodata='x') %>% 
    dplyr::rename(Site=SiteName)
  
  if(j==1){
    melt <- Multiple_sites_id } 
  else
  {melt<- rbind(melt, Multiple_sites_id)
  }
}
# End loop 1 -------------------------------------------------------------------

# Rename column names for the new dataframe called 'melt' 
colnames(melt) <- c("SampleTaken", "Flow", "SiteName","Measurement")

# Data pulled from Hilltop has different time frequencies. 
# The aggregate function is used to aggregate data to 15 minute intervals.
melt$SampleTaken <-  lubridate::floor_date(melt$SampleTaken, "15 minutes")

Flow <- filter(melt, Measurement == "Flow")
Flow$Flow <- as.numeric(Flow$Flow, na.rm = TRUE)
Flow <- Flow %>% group_by(SampleTaken, SiteName, Measurement) %>%
  summarise(Flow = mean(Flow))
Flow <- Flow[,c(1,2,4)]



#################################################################################
### Tim's code from here:


###Site summary table. This is required for the loop function. 
Siteinfo <- read_excel("I:/306 HCE Project/R_analysis/Rating curves/Gendist_flowDist/Updated sedrate inputs_HBRC.xlsx", 
                       sheet = "Site names") %>% filter(!is.na(`Site No`))  %>% mutate(SiteNo = gsub("\\_", "", `Site No`))

selected_site <- "22802"
Site <- filter(Siteinfo, `WISKI Code`  == selected_site)
## load full flow record 
#flow <- read_excel("I:/306 HCE Project/R_analysis/Rating curves/Gendist_flowDist/Kaniwhaniwha_22216_1.xlsx", 
#                    col_types = c("date", "date", "numeric")) %>%  
#        mutate(Date = as.Date(Date), Time1 = paste0(Time)) %>% 
#        mutate(Time = substr(Time1, start = 12, stop = 19)) %>% 
#        mutate(Time = ifelse(Time == "", "00:00:00", Time)) %>% 
#        mutate(value = value * 1000)    %>% 
#        mutate(timestamp = paste0(Date, Time)) %>% 
#        mutate(timestamp = as.POSIXct(timestamp, "%Y-%m-%d %H:%M:%S")) %>%   
#        select(timestamp, Date, Time, value) 
        
#####################################################################################################################################

# Defines the number of bins in the flow distribution table - 200 is the default 
int <- 200

options(warn=-1) # Suppresses warnings to ensure that the function can be looped across a list of sites 

#___________________________________________________________________________________________________________________________________________________________

options(digits = 6)

##### Calculates the total time (seconds) associated with each flow value 

Flow1 <-  Flow %>% mutate(time_diff = lead(SampleTaken)-(SampleTaken)) %>% 
          mutate(diff_secs  = as.numeric(time_diff , units = 'secs')) %>% 
          mutate(value = paste0(round(Flow,0))) %>% 
          mutate(value = as.numeric(Flow)) %>% 
          filter(!is.na(diff_secs), !is.na(Flow))


##### Summary table of flow statistics from flow record

summary <- summarise(Flow1, max = max(value), min = min(value), sd = sd(value), median = median(value), mean = mean(value), `flow duration`= sum(diff_secs), Interval = (max-min)/int, 
                     Interval1 = Interval/2, Start = min - Interval1, End = max + Interval1) 

# Define the Flowdist function
Flowdist <- function(selected_site) {
  Flowdist <- data.frame(Flowband = 1:200, 
                         Diff = summary$Interval, 
                         Band_min = summary$min, 
                         Band_max = summary$min + summary$Interval) %>% 
    mutate(Flow2 = (Band_max + Band_min) / 2) %>% 
    mutate(Diff = ifelse(Flowband == 1, 0, Diff))  %>% 
    mutate(cumsum = cumsum(Diff), Flow2 = Flow2 + cumsum) %>% 
    mutate(Band_min = Band_min + cumsum, Band_max = Band_max + cumsum)
  
  Final <- Flowdist %>% 
    mutate(binned_r_value = as.numeric(cut(Flowdist$Band_max, breaks = c(Flowdist$Band_max)))) %>% 
    mutate(binned_r_value = ifelse(is.na(binned_r_value), 0, binned_r_value))
  
  # Assuming flow1 is a data frame you want to use
  Final_flow <- flow1 %>% 
    mutate(binned_r_value = as.numeric(cut(flow1$value, breaks = c(Flowdist$Band_max)))) %>%
    group_by(binned_r_value) %>% 
    mutate(binned_r_value = ifelse(is.na(binned_r_value), 0, binned_r_value)) %>%  
    mutate(Site = selected_site) %>% 
    group_by(binned_r_value) %>%    
    summarise(Duration = sum(diff_secs))  
  
  Flowdist_export <- merge(Final, Final_flow, by = "binned_r_value", all = TRUE) %>% 
    mutate(Duration = ifelse(is.na(Duration), 0, Duration)) %>%  
    mutate(Flowband = 200:1) %>% 
    arrange(Flowband) %>% 
    mutate(SiteNo = Site$SiteNo) %>% 
    select(SiteNo, Flowband, Flow2, Duration)
  
  return(Flowdist_export)
}

# Loop function over sites 
Loop <- Siteinfo$'Site No'
for (i in 1:length(Loop)) {
  tryCatch({
    result <- Flowdist(Loop[i])
    # Do something with the result if needed
    write.csv(Flowdist_export, paste("FlowDist_processed/", i, ".csv", sep = ""))
  }, error = function(e) {
    warning(paste("Error for site:", Loop[i], " - ", conditionMessage(e)))
  })
}


#####################################################################################################################
############################################END######################################################################
