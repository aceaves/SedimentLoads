################################################################################
# This script has been written to predict sediment loads for respective rivers in Hawke's Bay. 
# Rating curves have been generated using regressions generated in SSC_flow_regressions.R to predict sediment loads.
# Edited by Ashton Eaves from March 2023 and tracked using Github: https://github.com/aceaves/SedimentRatingCurves

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

# Set the locale to ensure proper date-time parsing
Sys.setlocale("LC_TIME", "C")

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
############ Data inputs  ################

#Set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/306 HCE Project/Sites/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")

# Get site list or measurement list for respective sites 
sitelist <- SiteList(dfile, "")
Hilltop::SiteList(dfile)

# Date range. 
date1 <- "01-March-2021 00:00:00"
date2 <- "01-March-2023 00:00:00"

#Measurements/data that we want to pull from the Hilltop file 
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Flow")  

# Read regression file into a data frame
regression_output <- "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/Regressions/regression_output_excel.xlsx"
regression <- read.xlsx(regression_output)

# Print the data
print(regression)

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
melt$SampleTaken <- as.POSIXct(melt$SampleTaken, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)


Flow <- filter(melt, Measurement == "Flow")
Flow$Flow <- as.numeric(Flow$Flow, na.rm = TRUE)
Flow <- Flow %>% group_by(SampleTaken, SiteName, Measurement) %>%
  summarise(Flow = mean(Flow))
Flow <- Flow[,c(1,2,4)]
#Flow <- na.omit(Flow)
# Omit rows with negative values in columns 1, 2, and 4
Flow <- Flow[!( Flow[, 3] < 0), ]
# More data cleaning 
# Remove rows with dodgy flow for the specified SiteName
Flow <- subset(Flow, !(SiteName == "Mangamaire Stream at Cooks Tooth Rd" & Flow > 434459))
Flow <- subset(Flow, !(SiteName == "Waiau River at Ardkeen" & Flow < 100000))

###############################################################################

#Loop 2 through sites-----------------------------------------------------------

# Create an empty list to store the results
Load_list <- list()

# Create an empty data frame to store statistics or empty any existing data in the dataframe
Statistics_Load <- data.frame(
  site_name = character(),
  Min = numeric(),
  Q1 = numeric(),
  Median = numeric(),
  Mean = numeric(),
  Q3 = numeric(),
  Max = numeric(),
  Sum = numeric(),
  stringsAsFactors = FALSE)

# Iterate over sitelist
for (i in sitelist) { 
  # Subset Flow for the current SiteName
  Flow1 <- filter(Flow, SiteName == i)
  
  # Assuming 'SiteName' is the key for the lookup
  lookup_site <- i
  
  # Perform lookup to get the corresponding predicted concentration values for regression type
  lookup_result <- regression[regression$SiteName == lookup_site, c("RegressionType", "Slope", "Linear_Intercept", "Exp_Power", "Exp_X", "X_Squared", "Poly_X", "Poly_Intercept", "Log", "Log_Intercept", "Power_X", "Power_Exp")]
  
  # Check if the lookup was successful
  if (nrow(lookup_result) > 0) {
    regression_type <- lookup_result$RegressionType
    
    if (regression_type == "Exponential") {
      # Handle exponential regression coefficients
      Flow1$PredConc <- lookup_result$Exp_Power * exp(lookup_result$Exp_X * Flow1$Flow) 
    } else if (regression_type == "Polynomial") {
      # Handle polynomial regression coefficients
      Flow1$PredConc <- lookup_result$X_Squared * Flow1$Flow^2 + lookup_result$Poly_X * Flow1$Flow + lookup_result$Poly_Intercept
    } else if (regression_type == "Log") {
      # Handle log regression coefficients
      Flow1$PredConc <- lookup_result$Log * log(Flow1$Flow) + lookup_result$Log_Intercept
    } else if (regression_type == "Power") {
      # Handle power regression coefficients
      Flow1$PredConc <- lookup_result$Power_X * Flow1$Flow ^ lookup_result$Power_Exp
    } else if (regression_type == "Linear") {
      # Handle power regression coefficients
      Flow1$PredConc <- lookup_result$Slope * Flow1$Flow + lookup_result$Linear_Intercept
    } else {
      # Handle other regression types if needed
    }
    
    #### Calculates the total time associated with each flow value 
    Flow1$TimeDiff <- lead(Flow1$SampleTaken)-(Flow1$SampleTaken)
    # Replace NA values with 900
    Flow1$TimeDiff[is.na(Flow1$TimeDiff)] <- 900
        # Set values greater than 900 to 900
    Flow1$TimeDiff[Flow1$TimeDiff > 900] <- 900
    Flow1$DiffSecs <- as.numeric(Flow1$TimeDiff, units = 'secs')
    Flow1$DiffHours <- pmin(as.numeric(Flow1$TimeDiff, units = 'hours'), 0.25)

    # Apply conversion factor taken from: 
    # https://geology.humboldt.edu/courses/geology550/550_handouts/suspended_load_computation.pdf  
    #Flow1$Load <- (Flow1$PredConc * Flow1$Flow/1000 * Flow1$DiffHours * (0.0864 / 24))
    Flow1$Load <- (Flow1$PredConc * Flow1$Flow)
    # Convert load from mg to T
    Flow1$Load <- Flow1$Load / 1000000000 
    
    # Accumulate load per site
    Flow1 <- within(Flow1, AccumLoad <- Reduce("+", Load*Flow1$DiffSecs, accumulate = TRUE)) 
    
    # Get summary statistics for the current iteration for load
    summary_stats <- summary(Flow1$Load)
    
    # Create a new row with statistics
    new_row <- data.frame(
      SiteName = i,
      Min = round(as.numeric(summary_stats[1]), 2),
      Q1 = round(as.numeric(summary_stats[2]), 2),
      Median = round(as.numeric(summary_stats[3]), 2),
      Mean = round(as.numeric(summary_stats[4]), 2),
      Q3 = round(as.numeric(summary_stats[5]), 2),
      Max = round(as.numeric(summary_stats[6]), 2),
      Sum = tail(Flow1$AccumLoad, 1),
      stringsAsFactors = FALSE
    )
    
    # Append the current iteration to the Load_list
    Load_list[[i]] <- Flow1
    
    # Append the current iteration to the Statistics_Load
    Statistics_Load <- rbind(Statistics_Load, new_row)
  } else {
    cat("Site not found in the lookup table:", lookup_site, "\n")
  } 
}

# Combine the list of data frames into a single data frame
Load_list <- do.call(rbind, Load_list)

# End loop 2 -------------------------------------------------------------------

# Export data to CSV for use in Sedrate
#write.csv(Load_list, file = "Load_list_Mar2022Mar2023.csv", row.names = FALSE)

#### Export for use in FlowDist & Gendist ######################################
#### Do not need to do every time
# Convert DateTime to character with format including time
#Flow$SampleTaken <- format(Flow$SampleTaken, "%Y-%m-%d %H:%M:%S")

# Export data to CSV for use in Sedrate
#write.csv(Flow, file = "Flow_Mar2018Mar2023.csv", row.names = FALSE)

################################################################################

measure <- Load_list
  
SSC <- filter(melt, Measurement %in% c("Suspended Sediment Concentration", "Suspended Solids"))
SSC <- as.data.frame(sapply(SSC, gsub, pattern = "<|>", replacement = ""))
SSC$SampleTaken <- as.POSIXct(SSC$SampleTaken, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)
SSC$SampleTaken <- lubridate::round_date(SSC$SampleTaken, "15 minutes") 

# Convert to character to merge flow and concentration
Flow$SampleTaken <-as.character(Flow$SampleTaken) 
SSC$SampleTaken <-as.character(SSC$SampleTaken) 
merged <- merge(Flow, SSC, by = c("SampleTaken", "SiteName"))
# Remove unnecessary columns
merged <- merged[,c(1,2,3,4,5)]
colnames(merged) <- c('SampleTaken','Site', 'Flow', 'Conc', 'Measurement2')
# Convert back to date-time and Conc to numeric
merged$SampleTaken <- as.POSIXct(merged$SampleTaken, format = "%Y-%m-%d %H:%M:%S")
merged$Conc <- as.numeric(merged$Conc)
Flow$SampleTaken <- as.POSIXct(Flow$SampleTaken , format = "%Y-%m-%d %H:%M:%S")
# Filter out SSC from SS
merged$Measurement2[merged$Measurement2 == 'Suspended Sediment Concentration'] <- "SSC"
merged$Measurement2[merged$Measurement2 == 'Suspended Solids'] <- "SS"
merged1 <- filter(merged, Measurement2 == 'SSC')

#####  Write out merged1 for external regression analysis ######################
#write.csv(merged1, file = "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/merged1.csv", row.names = FALSE)

###############################################################################

#Set working directory for outputs and customise as needed (date etc)
setwd('I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs')

##########################################

#Loop 3 through sites-----------------------------------------------------------
for (i in sitelist) { 

###  Exports  ##################################################################

  measure1 <- filter(measure, SiteName == i)
  merged2 <- filter(merged, Site == i)
  merged3 <- filter(merged1, Site == i)

  ###############################
  #Export Flowplot to a PNG file
  filename <- paste("FLOW_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  Flowplot <- ggplot(data = measure1) +
    geom_path(aes(x = SampleTaken, y = Flow/1000), colour = 'blue', size = 0.4) + 
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") +
    scale_y_continuous(labels = comma_format(), name = "Flow (m"^"3/s"~")")
  
  print(Flowplot)
  dev.off()
  
  ###############################
  # Export Sample SSC plot to a PNG file
  filename <- paste("SSC_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  SSC <- ggplot(data = measure1) +
    geom_path(data = measure1, aes(x = SampleTaken, y = Flow/1000), colour = "blue", size = 0.4)+
    geom_line(data = measure1, aes(x = SampleTaken, y = PredConc), colour = 'darkgoldenrod') +

    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") +
    scale_y_continuous(labels = comma_format(),  name = "Flow (m"^"3/s"~")", sec.axis = sec_axis(~./1, name = "SSC (mg/l)"))
  
  print(SSC)
  dev.off()
  
  ################################
  # Export Cumulative Sediment1 plot to a PNG file
  filename <- paste("CUMSSC_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  CUMSSC <- ggplot(data = measure1) +
    geom_line(data = measure1, aes(x = SampleTaken, y = PredConc), colour = 'darkgoldenrod') +
    geom_line(data = measure1, aes(x = SampleTaken, y = AccumLoad), colour = 'coral1')+
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") +
    scale_y_continuous(name = "SSC (mg/l)",expand = c(0,0,0.2,2), sec.axis = sec_axis(~./1, name = "Cumulative sediment (T)"))
  
  print(CUMSSC)
  dev.off()
  
  ################################
  # Export Cumulative Sediment2 plot to a PNG file
  filename <- paste("CUMSSC2_", i, ".png", sep="")
  png(filename, width=1200, height=800)
  
  CUMSSC2 <- ggplot(data = measure1) +
    geom_line(data = measure1, aes(x = SampleTaken, y = PredConc), colour = 'darkgoldenrod') +
    geom_point(data = merged3, aes(x = SampleTaken, y = Conc, color = Measurement2),colour = 'aquamarine4', size = 1.5)+
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") +
    scale_y_continuous(name = "SSC (mg/l)",expand = c(0,0,0.2,2))
  
  print(CUMSSC2)
  dev.off()
  
}
#Loop 3 completed---------------------------------------------------------------

###### More Outputs  ##########################
# Print the resulting table
print(Statistics_Load)

#Table outputs
write.csv(Statistics_Load, file = "Statistics_Load_Mar2021_Mar2023.csv", row.names = FALSE)

measure$Flow <- measure$Flow/1000
measure2 <- filter(measure, SiteName != "Aropaoanui River at Aropaoanui" 
                   & SiteName != "Karamu Stream at Floodgates" 
                   & SiteName != "Mangakuri River at Nilsson Road"
                   & SiteName != "Mangaone River at Rissington"
                   & SiteName != "Wharerangi Stream at Codds")
measure2 <- measure2[,c(1,2,3,4,8,9)]
write.csv(measure2, file = "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/measure_Mar2021_June2023.csv", row.names = FALSE)

################################################################################
