################################################################################
# This script has been written to predict sediment loads for respective rivers in Hawke's Bay. 
# Rating curves have been generated using regressions generated in SSC_flow_regressions.R to predict sediment loads.
# Edited by Ashton Eaves and tracked using Github: https://github.com/aceaves/SedimentRatingCurves

library(Hilltop)
library(dplyr)
library(scales)
library(ggplot2)
library(tidyverse)
library(hms) 
library(lubridate) 
library(gt)
library(taskscheduleR)
library(openxlsx)
library(zoo)
library(xts)
library(readxl)

################################################################################
#Set up task scheduler

# # Set the path to the Rscript.exe file
# rscript_path <- file.path(R.home("bin"), "x64", "C:/Program Files/R/R-4.2.3/bin/x64/Rscript.exe")
# 
# # Set the path to the R script to be scheduled
# script_path <- "M:/E_Science/Projects/306 HCE Project/R_analysis/Rating curves/git/Sediment_rating_curves_working.R"
# 
# # Schedule the R script to run weekly at 7:00 AM
# taskscheduler_create(taskname = "My R Script",
#                      rscript = rscript_path,
#                      args = script_path,
#                      schedule = "WeekLY",
#                      starttime = "07:00",
#                      startdate = format(Sys.Date(), "%Y-%m-%d"))

################################################################################

#Set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/306 HCE Project/Sites/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")

# Get measurement list for respective sites 
sitelist <- SiteList(dfile, "")
#measurementlist <- Hilltop::MeasurementList(dfile, sitelist)
Hilltop::SiteList(dfile)

# Date range. 
date1 <- "01-February-2018 00:00:00"
date2 <- "01-February-2023 00:00:00"

#Measurements/data that we want to pull from the Hilltop file 
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Flow")  

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

#Set working directory for outputs and customise as needed (date etc)
setwd('I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs')

# Create an empty data frame to store statistics or empty any existing data in the dataframe
statistics_table_ratings <- data.frame(
  site_name = character(),
  Min = numeric(),
  Q1 = numeric(),
  Median = numeric(),
  Mean = numeric(),
  Q3 = numeric(),
  Max = numeric(),
  Sum = numeric(),
  stringsAsFactors = FALSE)

###############################################################################

# Read regression file into a data frame
regression_output <- "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/Regressions/statistics_output.xlsx"
regression <- read.xlsx(regression_output)
regression$Slope <- as.numeric(regression$Slope)
regression$Intercept <- as.numeric(regression$Intercept)
regression$RSquared <- as.numeric(regression$RSquared)
regression$Slope_SE <- as.numeric(regression$Slope_SE)
regression$Intercept_SE <- as.numeric(regression$Intercept_SE)

subval_regression <- select(regression, SiteName, Slope, Intercept, RSquared, Slope_SE, Intercept_SE)
# Print the data
print(subval_regression)

##########################################

# Convert time/date to as.POSIXct 
Flow$SampleTaken <- as.POSIXct(Flow$SampleTaken , format = "%Y-%m-%d %H:%M:%S")
# Convert flow column to numeric
Flow$Flow <- as.numeric(Flow$Flow) 
# Take natural log of flow data
#Original: Flow$Flowlog <- log(Flow$Flow)  
Flow$Flowlog <- Flow$Flow 

# Predict ln (concentration) based on equation calculated in the Sedrate software
# Original for Tuki: Flow$concLog <- (Flow$Flowlog*1.089-7.004) 
#Flow$concLog <- (Flow$Flowlog*1.089-6.5) #Calibrated to Tukituki
#Flow$concLog <- (Flow$Flowlog*1.089) #Calibrated to Aropaoanui
Flow$concLog <- (Flow$Flowlog) #Calibrate using regression function below.
# Apply bias correction factor (calculated in Sedrate)
# Original: Flow$predConc <- exp(Flow$concLog)*1.3

#Loop 2 through sites-----------------------------------------------------------
for (i in sitelist) { 

  # Assuming 'SiteName' is the key for the lookup
  lookup_site <- i  # Replace with the actual site name you are interested in

  # Perform lookup to get the corresponding values (Slope and Intercept)
  lookup_result <- subval_regression[subval_regression$SiteName == lookup_site, c("Slope", "Intercept", "Slope_SE")]
  
  # Check if the lookup was successful
  if (nrow(lookup_result) > 0) {
    # Use the looked-up values in your calculation
    slope_value <- lookup_result$Slope
    intercept_value <- lookup_result$Intercept
    slope_SE_value <- exp(lookup_result$Slope_SE)
    #Apply Sedrate correction factors
    Flow$concLog <- Flow$Flowlog * slope_value / 2.13 / 1000
    Flow$predConc <- Flow$concLog # *slope_SE_value
  } else {
    cat("Site not found in the lookup table:", lookup_site, "\n")
  }

  # Convert concentration to load and mg to T
  Flow$load <- (Flow$predConc*Flow$Flow*900)/1000000000 
  # Remove any N/As from the dataset 
  measure <- Flow %>%
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .)))
  # Accumulate load
  measure <- within(measure, AccumLoad <- Reduce("+", load, accumulate = TRUE)/100)
  measure$SummaryAllSites <- measure$AccumLoad
  # Group the data by Site and apply the cumsum function within each group
  measure <- measure %>%
    group_by(SiteName) %>%
    mutate(AccumLoadSite = cumsum(load)/100)


################################################################################
#Exports:

  measure1 <- filter(measure, SiteName == i)
  merged2 <- filter(merged, Site == i)
  merged3 <- filter(merged1, Site == i)
  # Get summary statistics for the current iteration for load
  measure1 <- within(measure1, AccumLoad1 <- Reduce("+", load, accumulate = TRUE)/100)
  measure1$summary1 <- measure1$AccumLoad1
  summary_stats <- summary(measure1$summary1)
  
  # Create a new row with statistics
  new_row <- data.frame(
    SiteName = i,
    Min = round(as.numeric(summary_stats[1]), 2),
    Q1 = round(as.numeric(summary_stats[2]), 2),
    Median = round(as.numeric(summary_stats[3]), 2),
    Mean = round(as.numeric(summary_stats[4]), 2),
    Q3 = round(as.numeric(summary_stats[5]), 2),
    Max = round(as.numeric(summary_stats[6]), 2),
    Sum = sum(measure1$load, na.rm = TRUE),
    stringsAsFactors = FALSE
  )
  
  # Add the new row to the statistics table
  statistics_table_ratings <- rbind(statistics_table_ratings, new_row)
  print(summary_stats)
  
  ###############################
  #Export Flowplot to a PNG file
  filename <- paste("FLOW_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  Flowplot <- ggplot(data = measure1) +
    geom_path(aes(x = SampleTaken, y = Flow/1000), colour = 'blue', size = 0.4) + 
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") +
    scale_y_continuous(labels = comma_format(), name = "Flow (m3/s)")
  
  print(Flowplot)
  dev.off()
  
  ###############################
  # Export Sample SSC plot to a PNG file
  filename <- paste("SSC_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  SSC <- ggplot(data = measure1) +
    geom_path(data = measure1, aes(x = SampleTaken, y = Flow/1000), colour = "blue", size = 0.4)+
    geom_point(data = merged2, aes(x = SampleTaken, y = Flow/1000, color = Measurement2), size = 1.5)+
    scale_color_manual(values = c("#009E73",'coral1'), name = "Sample Type")+
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") +
    scale_y_continuous(labels = comma_format(),  name = "Flow (m3/s)")
  
  print(SSC)
  dev.off()
  
  ################################
  # Export Cumulative Sediment1 plot to a PNG file
  filename <- paste("CUMSSC_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  CUMSSC <- ggplot(data = measure1) +
    geom_line(data = measure1, aes(x = SampleTaken, y = predConc), colour = 'darkgoldenrod') +
    geom_line(data = measure1, aes(x = SampleTaken, y = summary1*0.1), colour = 'red')+
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") +
    scale_y_continuous(name = "SSC (mg/l)",expand = c(0,0,0.2,2), sec.axis = sec_axis(~./0.1, name = "Cumulative sediment (T)"))
  
  print(CUMSSC)
  dev.off()
  
  ################################
  # Export Cumulative Sediment2 plot to a PNG file
  filename <- paste("CUMSSC2_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  CUMSSC2 <- ggplot(data = measure1) +
    geom_line(data = measure1, aes(x = SampleTaken, y = predConc), colour = 'darkgoldenrod') +
    geom_point(data = merged3, aes(x = SampleTaken, y = Conc, color = Measurement2),colour = 'coral1', size = 1.5)+
    geom_line(data = measure1, aes(x = SampleTaken, y = summary1*1), colour = 'red')+
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") +
    scale_y_continuous(name = "SSC (mg/l)",expand = c(0,0,0.2,2), sec.axis = sec_axis(~./1, name = "Cumulative sediment (T)"))
  
  print(CUMSSC2)
  dev.off()
  
}
#Loop 2 completed---------------------------------------------------------------


# Print the resulting table
print(statistics_table_ratings)
#Table outputs
write.csv(statistics_table_ratings, file = "statistics_table_ratings.csv", row.names = FALSE)

#Subset output for speed
#measure <- subset(measure, select = -c(Flowlog, concLog, AccumLoad))
write.csv(measure, file = "measure.csv", row.names = FALSE)
#Write also to app directory for Shiny
write.csv(measure, file = "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/app/measure.csv", row.names = FALSE)

################################################################################
