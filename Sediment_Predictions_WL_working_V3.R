################################################################################
# This script has been written to predict sediment loads for respective rivers in Hawke's Bay. 
# Rating curves have been generated using regressions generated in SSC_flow_regressions.R to predict sediment loads.
# Edited by Ashton Eaves from March 2023 and tracked using Github: https://github.com/aceaves/SedimentRatingCurves

################################################################################

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
library(grid)
library(HBRCDataAccess)

# Set the locale to ensure proper date-time parsing
Sys.setlocale("LC_TIME", "C")

############ Data inputs  ################

#Set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/306 HCE Project/Hilltop/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")
#dfile <- HilltopData("I:/320_Surface Water Hydrology/Hilltop_Workfiles/HilltopStitchedModelData/GabrielleModelled_Ashton.hts")

# Get site list or measurement list for respective sites 
sitelist <- SiteList(dfile, "")
Hilltop::SiteList(dfile)

# Subset the list for analysis after Cyclone Gabby as many sites have no data after.
#sitelist <- sitelist[sitelist == "Waimaunu Stream at Duncans"] # | 
#                      sitelist == "Waikatuku Strm off Harrison Rd"]
#sitelist <- sitelist[sitelist == "Ngaruroro River at Fernhill"] 
#sitelist <- sitelist[sitelist == "Karamu Stream at Floodgates" | 
#                       sitelist == "Tukituki River at Red Bridge"]

# Subset the list for analysis during Cyclone Gabby with Todd's modelled data:
#sitelist <- sitelist[sitelist == "Wairoa River at Marumaru" | 
#                      sitelist == "Tutaekuri River at Puketapu HBRC Site"]

# Subset the list for analysis during Cyclone Gabby with Todd's modelled data:
#sitelist <- sitelist[sitelist == "Wairoa River at Marumaru"]

# Date range. 
date1 <- "01-Jul-2021 00:00:00"
date2 <- "12-Feb-2023 00:00:00"

#Measurements/data that we want to pull from the Hilltop file
# Modify "Flow" to "Flow - CGModel" to pull modelled Gabrielle flows.
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Flow") # Adjust for CG Model. Change to "Flow - CGModel" otherwise just "Flow".

# Read regression file into a data frame
regression_output <- "I:/306 HCE Project/R_analysis/SedimentLoads/Outputs/Regressions/regression_output_excel.xlsx"
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

# Initialize 'melt' before the loop if not already initialized
melt <- NULL

for (j in 1:site_no) {
  #Edit here:
  Multiple_sites <- GetData(dfile, site_id[j], measurement, date1, date2)
  print(paste("Data for site", site_id[j], ":", length(Multiple_sites)))
  
  # Process only if Multiple_sites is not null and has elements
  if (!is.null(Multiple_sites) && length(Multiple_sites) > 0) {
    processed_sites <- lapply(Multiple_sites, function(x) {
      if (NROW(x) > 0) {  # Ensure that x has rows
        cbind(zoo::fortify.zoo(x), SiteName = attr(x, 'SiteName'), Measurement = attr(x, 'Measurement'))
      } else {
        NULL  # Return NULL if x is empty
      }
    })
    
    # Filter out NULL entries before attempting to rbind
    processed_sites <- Filter(NROW, processed_sites)
    
    if (length(processed_sites) > 0) {
      Multiple_sites_id <- do.call(rbind, processed_sites) %>%
        dplyr::rename(zoodata = 'x') %>%
        dplyr::rename(Site = SiteName)
      
      # Initialize or bind to 'melt'
      if (is.null(melt)) {
        melt <- Multiple_sites_id
      } else {
        melt <- rbind(melt, Multiple_sites_id)
      }
    } else {
      warning(paste("Processed data is empty for site", site_id[j], "in iteration", j))
    }
  } else {
    warning(paste("No data for site", site_id[j], "in iteration", j))
  }
}
# End loop 1 -------------------------------------------------------------------

# Rename column names for the new dataframe called 'melt' 
colnames(melt) <- c("SampleTaken", "Flow", "SiteName","Measurement")

# Data pulled from Hilltop has different time frequencies. 
# The aggregate function is used to aggregate data to 15 minute intervals.
melt$SampleTaken <-  lubridate::floor_date(melt$SampleTaken, "15 minutes")
melt$SampleTaken <- as.POSIXct(melt$SampleTaken, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)


Flow <- filter(melt, Measurement == "Flow") # Another adjustment for CG Model. Change to "Flow - CGModel" otherwise just "Flow"
Flow$Flow <- as.numeric(Flow$Flow, na.rm = TRUE)
# Remove duplicate timesteps
Flow <- Flow %>% group_by(SampleTaken, SiteName, Measurement) %>%
  summarise(Flow = mean(Flow))
Flow <- Flow[,c(1,2,4)]
# Omit rows with negative values in columns 1, 2, and 4
Flow <- Flow[!( Flow[, 3] < 0), ]
# More data cleaning 
# Remove rows with dodgy flow for the specified SiteName
Flow <- subset(Flow, !(SiteName == "Mangamaire Stream at Cooks Tooth Rd" & Flow > 434459))
Flow <- subset(Flow, !(SiteName == "Waiau River at Ardkeen" & Flow < 100000))

###############################################################################

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
#write.csv(merged1, file = "I:/306 HCE Project/R_analysis/SedimentLoads/Outputs/merged1.csv", row.names = FALSE)

###############################################################################

# Rerun this piece to remove midnight NAs.

# Data pulled from Hilltop has different time frequencies. 
# The aggregate function is used to aggregate data to 15 minute intervals.
melt$SampleTaken <-  lubridate::floor_date(melt$SampleTaken, "15 minutes")
melt$SampleTaken <- as.POSIXct(melt$SampleTaken, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)

Flow <- filter(melt, Measurement == "Flow") # Another adjustment for CG Model. Change to "Flow - CGModel" otherwise just "Flow"
Flow$Flow <- as.numeric(Flow$Flow, na.rm = TRUE)
# Remove duplicate timesteps
Flow <- Flow %>% group_by(SampleTaken, SiteName, Measurement) %>%
  summarise(Flow = mean(Flow))
Flow <- Flow[,c(1,2,4)]
# Omit rows with negative values in columns 1, 2, and 4
Flow <- Flow[!( Flow[, 3] < 0), ]

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
  Flow1 <- dplyr::filter(Flow, SiteName == i)
  
  # Assuming 'SiteName' is the key for the lookup
  lookup_site <- i
  
  # Perform lookup to get the corresponding predicted concentration values for regression type
  lookup_result <- regression[regression$SiteName == lookup_site, c("RegressionType", "Slope", "Linear_Intercept", "Exp_Power", "Exp_X", "X_Squared", "Poly_X", "Poly_Intercept", "Log", "Log_Intercept", "Power_X", "Power_Exp")]
  
  # Check if the lookup was successful
  if (nrow(lookup_result) > 0) {
    regression_type <- lookup_result$RegressionType
    
    # Process according to regression type
    if (regression_type == "Exponential") {
      Flow1$PredConc <-  exp(lookup_result$Exp_X * Flow1$Flow) * lookup_result$Exp_Power
    } else if (regression_type == "Polynomial") {
      Flow1$PredConc <- lookup_result$X_Squared * Flow1$Flow^2 + lookup_result$Poly_X * Flow1$Flow + lookup_result$Poly_Intercept
    } else if (regression_type == "Log") {
      Flow1$PredConc <- lookup_result$Log * log(Flow1$Flow) + lookup_result$Log_Intercept
    } else if (regression_type == "Power") {
      Flow1$PredConc <- lookup_result$Power_X * Flow1$Flow ^ lookup_result$Power_Exp
    } else if (regression_type == "Linear") {
      Flow1$PredConc <- lookup_result$Slope * Flow1$Flow + lookup_result$Linear_Intercept # slope in l/s
    } 
    
    # Remove negative values from regressions
    Flow1$PredConc[Flow1$PredConc < 0] <- 0
    
    # Ensure SampleTaken is in POSIXct format (date-time)
    Flow1$SampleTaken <- as.POSIXct(Flow1$SampleTaken)
    # Calculate the time difference between consecutive samples
    Flow1$TimeDiff <- difftime(dplyr::lead(Flow1$SampleTaken), Flow1$SampleTaken, units = "secs")
    # Check for negative time differences and replace with 900 seconds for midnight rollover
    Flow1$TimeDiff <- ifelse(Flow1$TimeDiff < 0, 900, Flow1$TimeDiff)
    # Replace NA with 900 seconds (15 minutes)
    Flow1$TimeDiff[is.na(Flow1$TimeDiff)] <- 900
    # Ensure no time difference exceeds 900 seconds
    Flow1$TimeDiff <- pmin(as.numeric(Flow1$TimeDiff), 900)
    # Convert time differences to hours (up to 0.25 hours)
    Flow1$DiffHours <- Flow1$TimeDiff / 3600
    # Optional: Convert back to seconds if needed
    Flow1$DiffSecs <- Flow1$TimeDiff
    
    # Calculate Load
    Flow1$Load <- Flow1$PredConc * Flow1$Flow
    Flow1$Load <- Flow1$Load / 1000000000  # Convert load from mg to T
    
    # Initialize AccumLoad and calculate it
    Flow1$AccumLoad <- numeric(nrow(Flow1))
    if (length(Flow1$Load) > 0) {
      Flow1$AccumLoad <- Reduce("+", Flow1$Load * Flow1$DiffSecs, accumulate = TRUE)
    }
    
    # Get summary statistics for the current iteration for load
    if (!is.null(Flow1$Load) && length(Flow1$Load) > 0 && !all(is.na(Flow1$Load))) {
      summary_stats <- summary(Flow1$Load)
      min_val <- round(as.numeric(summary_stats[1]), 2) 
      min_val <- if (!is.na(min_val)) min_val else NA
      q1_val <- round(as.numeric(summary_stats[2]), 2) 
      q1_val <- if (!is.na(q1_val)) q1_val else NA
      median_val <- round(as.numeric(summary_stats[3]), 2) 
      median_val <- if (!is.na(median_val)) median_val else NA
      mean_val <- round(as.numeric(summary_stats[4]), 2) 
      mean_val <- if (!is.na(mean_val)) mean_val else NA
      q3_val <- round(as.numeric(summary_stats[5]), 2) 
      q3_val <- if (!is.na(q3_val)) q3_val else NA
      max_val <- round(as.numeric(summary_stats[6]), 2) 
      max_val <- if (!is.na(max_val)) max_val else NA
    } else {
      min_val <- NA
      q1_val <- NA
      median_val <- NA
      mean_val <- NA
      q3_val <- NA
      max_val <- NA
    }
    
    if (!is.null(Flow1$AccumLoad) && length(Flow1$AccumLoad) > 0) {
      sum_val <- tail(Flow1$AccumLoad, 1)
    } else {
      sum_val <- NA
    }
    
    # Create a new row with statistics
    new_row <- data.frame(
      SiteName = i,
      Min = min_val,
      Q1 = q1_val,
      Median = median_val,
      Mean = mean_val,
      Q3 = q3_val,
      Max = max_val,
      Sum = sum_val,
      stringsAsFactors = FALSE
    )
    
    # Append the current iteration to the Load_list
    Load_list[[i]] <- Flow1
    
    # Append the current iteration to the Statistics_Load
    if (!any(is.na(new_row))) {
      Statistics_Load <- rbind(Statistics_Load, new_row)
    } else {
      warning(paste("Invalid data or summary statistics for site:", i))
    }
    
  } else {
    cat("Site not found in the lookup table:", lookup_site, "\n")
  } 
}
# End loop 2 -------------------------------------------------------------------

################################################################################

#Set working directory for outputs and customise as needed (date etc)
setwd('I:/306 HCE Project/R_analysis/SedimentLoads/Outputs')

# Create measure data frame
# If measure is a list of data frames, bind them into a single data frame
measure_df <- bind_rows(Load_list)
# Ensure SiteName is character in both sitelist and measure_df
measure_df$SiteName <- as.character(measure_df$SiteName)
sitelist <- as.character(sitelist)

###### Main output table ########
# Print the resulting table
print(Statistics_Load)

##Load table output ******Make sure the dates line up with data inputs
write.csv(Statistics_Load, file = "Statistics_Load_July2021_Feb203_WL.csv", row.names = FALSE)

#####  Plot Exports  ###########################################################


#Loop 3 through sites-----------------------------------------------------------
for (i in sitelist) { 
  
  measure1 <- filter(measure_df, SiteName == i)
  merged2 <- filter(merged, Site == i)
  merged3 <- filter(merged1, Site == i)
  # Disable scientific notation
  options(scipen = 999)
  
  ###############################
  #Export Flowplot to a PNG file
  filename <- paste("FLOW_", i, ".png", sep="")
  png(filename, width=1000, height=500)
  
  Flowplot <- ggplot(data = measure1) +
    geom_path(aes(x = SampleTaken, y = Flow/1000), colour = '#00364a', size = 0.4) + 
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") + # use for normal graphs
    #scale_x_datetime(date_labels = "%d %b %Y", date_breaks = "1 days", name = "Date") + # use for event graphs
    scale_y_continuous(name = "Flow (m"^"3"/s~")", labels = comma) +
    theme(
      axis.title = element_text(size = 17),    # Axis titles font size
      axis.text = element_text(size = 15),     # Axis labels font size
      plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),  # Top, right, bottom, left margins
    )
  print(Flowplot)
  dev.off()
  
  ################################
  # Export Cumulative Sediment1 plot to a PNG file
  filename <- paste("CUMSSC_", i, ".png", sep="")
  png(filename, width=1000, height=500)
  
  CUMSSC <- ggplot(data = measure1) +
    geom_line(data = measure1, aes(x = SampleTaken, y = AccumLoad), colour = '#f15d49') +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") + # use for normal graphs
    #scale_x_datetime(date_labels = "%d %b %Y", date_breaks = "1 days", name = "Date") + # use for event graphs
    scale_y_continuous(name = "Cumulative sediment (T)", labels = comma) +
    theme(
      axis.title = element_text(size = 17),    # Axis titles font size
      axis.text = element_text(size = 15),     # Axis labels font size
      plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),  # Top, right, bottom, left margins
    )
  print(CUMSSC)
  dev.off()
  
  ################################
  # Export SSC with point samples plot to a PNG file
  filename <- paste("SSC2_", i, ".png", sep="")
  png(filename, width=1000, height=500)
  
  # Create a new dataset with 12-hour offset for the points
  merged3_offset <- merged3 %>%
    mutate(SampleTaken = SampleTaken + lubridate::hours(12))
  
  SSC2 <- ggplot(data = measure1) +
    geom_line(data = measure1, aes(x = SampleTaken, y = PredConc), colour = '#92a134') +
    geom_point(data = merged3_offset, aes(x = SampleTaken, y = Conc, color = Measurement2),colour = '#eebd1c', size = 2) +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") + # use for normal graphs
    #scale_x_datetime(date_labels = "%d %b %Y", date_breaks = "1 days", name = "Date") + # use for event graphs
    scale_y_continuous(name = "SSC (mg/l)", labels = comma) +
    theme(
      axis.title = element_text(size = 17),    # Axis titles font size
      axis.text = element_text(size = 15),     # Axis labels font size
      plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),  # Top, right, bottom, left margins
    )
  print(SSC2)
  dev.off()
  
  ################################
  # Export SSC with point samples plot to a PNG file
  filename <- paste("SSC3_", i, ".png", sep="")
  png(filename, width=1000, height=500)
  
  # Multiply Flow values by a factor to extend the vertical range
  flow_scaling_factor <- 2
  
  SSC3 <- ggplot(data = measure1) +
    # Primary axis: geom_path for Flow, scaled to extend the vertical range
    geom_path(aes(x = SampleTaken, y = (Flow / 1000) * flow_scaling_factor), colour = '#00364a', size = 0.4) + 
    geom_line(aes(x = SampleTaken, y = PredConc), colour = '#92a134') +
    geom_point(data = merged3_offset, aes(x = SampleTaken, y = Conc, color = Measurement2),colour = '#eebd1c', size = 2) +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "3 months", name = "Date") + 
        # Primary y-axis for SSC and secondary y-axis for Flow with adjusted scaling
    scale_y_continuous(
      name = "SSC (mg/l)",  # Primary axis label for SSC
      labels = scales::comma,  # Comma formatting for SSC values
      limits = c(0, NA),  # Extend the range to start at 0
      sec.axis = sec_axis(~ . / flow_scaling_factor, name = "Flow (m³/s)", labels = scales::comma)  # Adjust secondary axis with inverse transformation
    ) +
    theme(
      axis.title = element_text(size = 17),    # Axis titles font size
      axis.text = element_text(size = 15),     # Axis labels font size
      plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),  # Margins
      legend.position = "none"  # Remove legend
    )
  
  # Print the plot and save as PNG
  print(SSC3)
  dev.off()
  
}
#Loop 3 completed---------------------------------------------------------------

###### More Outputs  ###########################################################

############## Clean up measure_df for export 

# Convert to cumecs
measure_df$Flow <- measure_df$Flow/1000
# Define the time adjustment
time_adjustment <- minutes(15)

# Fill NA values with adjusted date from the next row
for (i in seq_len(nrow(measure_df))) {
  if (is.na(measure_df$SampleTaken[i])) {
    # If the current row is NA, get the next row's date and adjust it
    if (i < nrow(measure_df)) {
      next_date <- measure_df$SampleTaken[i + 1]
      if (!is.na(next_date)) {
        measure_df$SampleTaken[i] <- next_date - time_adjustment
      }
    }
  }
}

# Remove unnecessary columns
measure_df2 <- measure_df[,c(1,2,3,4,8,9)]
# Print the result
head(measure_df2)

#measure_df3 <- filter(measure_df2, SiteName != "Aropaoanui River at Aropaoanui" 
#                   & SiteName != "Karamu Stream at Floodgates" 
#                   & SiteName != "Mangakuri River at Nilsson Road"
#                   & SiteName != "Mangaone River at Rissington"
#                   & SiteName != "Wairoa River at Marumaru"
#                   & SiteName != "Wharerangi Stream at Codds")
write.csv(measure_df2, file = "I:/306 HCE Project/R_analysis/SedimentLoads/Outputs/measure_df_July2021_Feb2023_WL.csv", row.names = FALSE)

################################################################################
