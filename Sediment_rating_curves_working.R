# This script has been written to predict sediment loads for respective rivers in Hawke's Bay. Rating curves have been generated using Sedrate and the model output used to predict sediment loads
# Edited Ashton Eaves 20230314

library(Hilltop)
library(dplyr)
library(scales)
library(ggplot2)
library(tidyverse)
library(hms) 
library(lubridate) 
library(gt)

#set file path to ISCO Hilltop file 
dfile <- HilltopData("N:/HilltopData/WQ_E_Working/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")

# Get measurement list for respective sites 
sitelist <- SiteList(dfile, "")
#measurementlist <- Hilltop::MeasurementList(dfile, sitelist)

Hilltop::SiteList(dfile)

# Date range. 
date1 <- "01-Mar-2021 00:00:00"
date2 <- "01-Mar-2023 00:00:00"

#Measurements/data that we want to pull from the Hilltop file 
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Flow")  

#Loop 1 through sites--------------------------------------------------
name <- data.frame(sitelist)
name$Sites <- as.character(name$sitelist)
  
site_no <- length(sitelist)
site_id <- sitelist

method <- ""
interval <- ""
  
for(j in 1:site_no){
    Multiple_sites <- GetData(dfile, site_id[j] ,measurement, date1, date2) # You will struggle to use this format in most packages 
      #do this to make it more useful
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

#________________________________________________________________________________________________________________________________________________________________________________________
# Rename column names for the new dataframe called 'melt' 
colnames(melt) <- c("SampleTaken", "Flow", "SiteName","Measurement")
  
# Data pulled from Hilltop has different time frequencies. The aggregate function is used to aggregate data to 15 minute intervals
melt$SampleTaken <-  lubridate::floor_date(melt$SampleTaken, "15 minutes")
  
  
Flow <- filter(melt, Measurement == "Flow")
Flow$Flow <- as.numeric(Flow$Flow)
Flow <- Flow %>% group_by(SampleTaken, SiteName, Measurement) %>%
  summarise(Flow = mean(Flow)) 
  
SSC <- filter(melt, Measurement %in% c("Suspended Sediment Concentration", "Suspended Solids"))
SSC <- as.data.frame(sapply(SSC, gsub, pattern = "<|>", replacement = ""))
SSC$SampleTaken <- as.POSIXct(SSC$SampleTaken, format = "%Y-%m-%d %H:%M:%S")
SSC$SampleTaken <- lubridate::round_date(SSC$SampleTaken, "15 minutes") 
SSC$SampleTaken <-as.character(SSC$SampleTaken) 
Flow$SampleTaken <-as.character(Flow$SampleTaken) 
  
merged <- merge(Flow, SSC, by = "SampleTaken" )
merged <- merged[,c(1,2,3,4,5,7)]
  
colnames(merged) <- c('SampleTaken','Site','Measurment', 'Flow', 'Conc', 'Measurement2')
merged$SampleTaken <- as.POSIXct(merged$SampleTaken, format = "%Y-%m-%d %H:%M:%S")
merged$Conc <- as.numeric(merged$Conc)
  
merged$Measurement2[merged$Measurement2 == 'Suspended Sediment Concentration'] <- "SSC"
merged$Measurement2[merged$Measurement2 == 'Suspended Solids'] <- "SS"
merged1 <- filter(merged, SampleTaken > "2018-06-30" & Measurement2 == 'SSC')
  
###############################################################################
# Convert time/date to as.POSIXct 
  
Flow$SampleTaken <- as.POSIXct(Flow$SampleTaken , format = "%Y-%m-%d %H:%M:%S")
Flow$Flow <- as.numeric(Flow$Flow) # Convert fklow column to numeric 
Flow$Flowlog <- log(Flow$Flow) # Take natural log of flow data 
Flow$concLog <- (Flow$Flowlog*1.089-7.004) # Predict ln (concentration) based on equation calulated in the Sedrate software 
Flow$predConc <- exp(Flow$concLog)*1.3 # Apply bias correction factor (calculated in Sedrate)
Flow$load <- (Flow$predConc*Flow$Flow*900)/1000000000 # Convert concentration to load and mg to T
  
# Remove any N/As from the datset 
test <- Flow %>%
 mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
test <- within(test, acc_sum <- Reduce("+", load, accumulate = TRUE))
  
test$summary <- test$acc_sum/1000000
  
colnames(test)
  
write.csv(merged, file = "merged.csv", row.names = FALSE)
  
############################################################################################################
#ggplot exports:
setwd('M:/E_Science/Projects/306 HCE Project/R_analysis/Rating curves/Outputs/test')
  
#Loop 2 through sites-----------------------------------------------------------
for (i in sitelist) { 

  test1 <- filter(test, SiteName == i)
  
  ###############################
  #Export Flowplot to a PNG file
  filename <- paste("FLOW_", i, ".png", sep="")
  png(filename, width=1200, height=800)
    
  Flowplot <- ggplot(data = test1) + 
    geom_path(aes(x = SampleTaken, y = Flow), colour = 'black', size = 0.4) + theme_bw() + 
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") + 
    scale_y_continuous(labels = comma_format())+
    theme(axis.text = element_text(colour = 'black', size = 10), axis.title  = element_text(colour = 'black', size = 10)) + 
    xlab('Date') + ylab('Flow (l/s)')
    
  print(Flowplot)
  dev.off()
  
  ################################
  #Export Sample SSC plot to a PNG file
  filename <- paste("SSC_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  SSC <- ggplot(data = test1) + 
    geom_path(data = test1, aes(x = SampleTaken, y = Flow), colour = "black", size = 0.4)+  
    geom_point(data = merged, aes(x = SampleTaken, y = Flow, color = Measurement2), size = 1.5)+
    scale_color_manual(values = c("#009E73","#0072B2"), name = "Sample Type")+
    theme_bw() +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") + 
    scale_y_continuous(labels = comma_format())+
    theme(axis.text = element_text(colour = "black", size = 10), axis.title  = element_text(colour = "black", size = 10)) +
    theme(legend.title = element_text(size = 9, colour = "black")) + 
    xlab("Date")+ 
    ylab("Flow (l/s)")
  
  print(SSC)
  dev.off()
  
  ################################
  #Export Cumulative Sediment1 plot to a PNG file
  filename <- paste("CUMSSC_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  CUMSSC <- ggplot(data = test1) + 
    geom_line(data = test1, aes(x = SampleTaken, y = predConc)) + theme_bw() + 
    geom_line(data = test1, aes(x = SampleTaken, y = summary*500), colour = 'red')+
    scale_y_continuous(name = "SSC (mg/l)",expand = c(0,0,0.2,2), sec.axis = sec_axis(~./500, name = "Cumulative sediment (T)")) 
  
  print(CUMSSC)
  dev.off()
  
  ################################
  #Export Cumulative Sediment2 plot to a PNG file
  filename <- paste("CUMSSC2_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  CUMSSC2 <- ggplot(data = test1) + 
    geom_line(data = test1, aes(x = SampleTaken, y = predConc)) + theme_bw() + 
    geom_point(data = merged1, aes(x = SampleTaken, y = Conc, color = Measurement2), size = 1.5)+
    geom_line(data = test1, aes(x = SampleTaken, y = summary*500), colour = 'red')+
    scale_y_continuous(name = "SSC (mg/l)",expand = c(0,0,0.2,2), sec.axis = sec_axis(~./500, name = "Cumulative sediment (T)")) 
  
  print(CUMSSC2)
  dev.off()
  
  #summary(test$summary)
  
}
#Loop 2 completed---------------------------------------------------------------

######### NOT SURE ABOUT CODE AFTER HERE #######################################

#SSC$SampleTaken <- lubridate::round_date(SSC$SampleTaken, "15 minutes") 
#SSC$SampleTaken <-as.character(SSC$SampleTaken) 
#Flow$SampleTaken <-as.character(Flow$SampleTaken) 


#merged <- merge(Flow, SSC, by = "SampleTaken" )
#merged <- merged[,c(1,2,3,5)]

#colnames(merged) <- c('DateTime', 'Flow', 'Site', 'Conc')

#merged1 <- select(merged, DateTime, Flow, Conc)
#merged1$DateTime <- as.POSIXct(merged1$DateTime, format = "%Y-%m-%d %H:%M:%S")
#merged1$Date <- format(as.POSIXct(merged1$DateTime,format='%m/%d/%Y %H:%M:%S'),format='%Y%m%d')
#merged1$Date2 <- format(as.POSIXct(merged1$DateTime,format='%m/%d/%Y %H:%M:%S'),format='%d/%m/%Y')


#merged1$Time <- format(merged1$DateTime, format = "%H%M%S")
#merged1$Time1 <- format(merged1$DateTime, format = "%I:%M:%S %p")
##merged1$Time1 <- hms::as_hms(lubridate::parse_date_time(merged1$Time2,"1IMS p"))


#merged1$Flow <- as.numeric(merged1$Flow)
#merged1$Flow<- formatC(merged1$Flow, digits = 0, format = "f")
#merged1$Flow <- as.character(merged1$Flow)
#merged1$Conc <- as.numeric(merged1$Conc)
#merged1$new <- ""
#library(dplyr)


#merged1 <- merged1 %>% filter(!Conc > 6000)

#Final <- data.frame(merged1$Flow, merged1$Date,merged1$Time)


#nrow(Final)


##write.table(Final, file = "test1.txt",row.names=FALSE, quote = FALSE, sep="\t")
##write_delim(Tim, file = "test1.txt", append = FALSE)
#print(Final)
#print(Final[30:230,],right=T, row.names = FALSE)
#print(Final[201:296,],right=T, row.names = FALSE)


##gt_tbl <- gt(merged1)
#setwd('C:/Users/tim.norris/Desktop')
#xlsx::write.xlsx(merged1, 'merged2.xlsx')

#merged1$Flow <- as.numeric(merged1$Flow )

#Final$merged1.Flow <- as.numeric(Final$merged1.Flow)
#summary(merged1$Conc)
