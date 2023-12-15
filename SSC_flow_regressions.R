###############################################################################
# SSC to flow regressions
# Run before Sediment Ratings Curves script.
# Created by Dr Ashton Eaves, Senior Land Scientist, HBRC
# December 2023
#______________________________________________________________________________

library(Hilltop)
library(scales)
library(ggplot2)
library(tidyverse)
library(hms) 
library(lubridate) 
library(gt)
library(taskscheduleR)
library(scales)
library(HBRCDataAccess)
library(dplyr)
library(writexl)


library(zoo)
library(xts)


#Get flow data #################################################################

#Set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/306 HCE Project/Sites/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")

# Get measurement list for respective sites 
sitelist <- SiteList(dfile, "")
#measurementlist <- Hilltop::MeasurementList(dfile, sitelist)
Hilltop::SiteList(dfile)

# Date range. 
date1 <- "01-February-2018 00:00:00"
date2 <- "01-December-2023 00:00:00"

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
# End loop ---------------------------------------------------------------------

#Required: Need to have hilltop manager/hydro working on same computer as Rstudio install for Hilltop package.  Bit versions of R and hydrolib must match.


#Hilltop measurement "request as" name 
M <- c("Water Temperature (D-Opto)")  #DOpto

#processed tidbit and DOpto data is in Hilltop Allsites file
EP <-    "//hydro/hydro/Hilltop/Archive/Allsites.hts"   #filepath of hilltop file with water temp measurements


FindHydroYear<- function (dateVect){
  #function to return hydro year from date  
  
  yr <- year(dateVect) 
  m <- month(dateVect)
  
  ifelse( m < 7, yr-1, yr)
  
}

#get data from Hilltop
HData <- HilltopData(EP)  ##get HilltopDataObjs
sites <- unlist(c(SiteList(HData, M) %>% select(Site)))      #Return sites in hilltop file for given measurement

#loop combines each sites data in single df
for (s in sites) {
  
  SiteData <- GetData(HData,siteName = s, measurement = M, startTime = "",endTime = "")
  SiteName <- xtsAttributes(SiteData)$SiteName # get sitename
  SiteData<- fortify.zoo(SiteData)
  SiteData$Site <- SiteName
  
  
  #create DF of Daily CRI for all sites 
  if (!exists("AllSites")) {
    AllSites <- SiteData
    
  }
  
  else
    AllSites <- rbind(SiteData,AllSites)
  
}





# Rename column names for the new dataframe called 'melt'
new_colnames <- c("SampleTaken", "Flow", "SiteName", "Measurement")

# Ensure the number of new column names matches the number of columns in 'melt'
if (length(new_colnames) == ncol(melt)) {
  colnames(melt) <- new_colnames
} else {
  warning("Number of new column names does not match the number of columns in 'melt'")
}

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


colnames(merged) <- c('SampleTaken','Site','Measurement', 'Flow', 'Conc', 'Measurement2')
merged$SampleTaken <- as.POSIXct(merged$SampleTaken, format = "%Y-%m-%d %H:%M:%S")
merged$Conc <- as.numeric(merged$Conc)

merged$Measurement2[merged$Measurement2 == 'Suspended Sediment Concentration'] <- "SSC"
merged$Measurement2[merged$Measurement2 == 'Suspended Solids'] <- "SS"
merged1 <- filter(merged, SampleTaken > "2018-06-30" & Measurement2 == 'SSC')

subset_merged1 <- select(merged1, SampleTaken, Site, Measurement, Flow)

head(subset_merged1, 10)

####    Output    ##############################################################

#Set working directory for outputs
setwd('./Outputs/Regressions')

# Create an empty data frame to store statistics
statistics_table <- data.frame(
  Site = character(),
  Iteration = integer(),
  Slope = numeric(),
  Intercept = numeric(),
  RSquared = numeric(),
  stringsAsFactors = FALSE
)

#Loop 2 through sites-----------------------------------------------------------
for (i in sitelist) { 

  ###  Get puddle SSC data  ####################################################
  
  MyData <- getPuddleData(
    query_option = "fullPuddleHilltop",
    fromDate = "01-02-2018",
    toDate = "01-12-2023",
    catchments = "",
#   sites = "Aropaoanui River at Aropaoanui",
    sites = i,
    projects = "340204",
    measurements = "Suspended Sediment Concentration",
    detids = ""
  )
  head(MyData, 10)

  #Remove unnecessary columns and tidy time
  subset_MyData <- select(MyData, Time, Site, result, DetID)
  subset_MyData <- subset_MyData %>% rename(SampleTaken = Time)
  subset_MyData$SampleTaken <- lubridate::round_date(subset_MyData$SampleTaken, "15 minutes") 
  head(subset_MyData, 10)
  
  ### Merge SSC samples and flow  ##########
  
  merged_df <- merge(subset_MyData, subset_merged1, by = c("SampleTaken", "Site"))
  head(merged_df, 10)
  
  merged_df$result <-as.numeric(merged_df$result) 
  merged_df$Flow <-as.numeric(merged_df$Flow)
  #Convert to cumecs
  merged_df$Flow <-(merged_df$Flow)/1000
  
  # Extract a unique Site Name (assuming the Site column contains categorical values)
  site_name <- unique(merged_df$Site)
  # Extract start and end dates
  start_date <- min(merged_df$SampleTaken)
  end_date <- max(merged_df$SampleTaken)
  
  ###  Stats  ##############################
  
  # Check for non-missing values
  if (sum(!is.na(merged_df$Flow) & !is.na(merged_df$result)) > 1) {
    # Fit linear regression only if there are non-missing values
    lm_model <- lm(result ~ Flow, data = merged_df, na.action = na.exclude)
    
    # Extract coefficients and R-squared
    coef_slope <- round(coef(lm_model)[2], 2)
    coef_intercept <- round(coef(lm_model)[1], 2)
    r_squared <- round(summary(lm_model)$r.squared, 2)
    
    # Create a new row with statistics
    new_row <- data.frame(
      Site = site_name,
      Iteration = i,  # You may want to specify an iteration number here
      Slope = coef_slope,
      Intercept = coef_intercept,
      RSquared = r_squared
    )
    
    # Append the row to the statistics_table
    statistics_table <- rbind(statistics_table, new_row)
  } else {
    cat("Skipping site:", site_name, "due to missing values\n")
  }
  
  # Define the formula text
  formula_text <- paste(coef_intercept, "~", coef_slope, "* x")
  # Convert formula_text to a formula
  my_formula <- as.formula(formula_text)
  # Create R-squared text
  r_squared_text <- paste("R-squared =", r_squared)
  
  # Append the row to the statistics_table
  statistics_table <- rbind(statistics_table, new_row)

  
  ######     Export plots     ##############
  
  # Export Sample SSC plot to a PNG file
  filename <- paste("SSC_Flow_Regression_", site_name, ".png", sep="")
  png(filename, width=1200, height=800)

  # Create a ggplot2 plot with a scatterplot and regression line
  Regression <- ggplot(merged_df, aes(x = Flow, y = result)) +
    geom_point(color = "Black", size = 2, shape = 19, stroke = 1) +
    geom_point(color = "darkgoldenrod", size = 1, shape = 19, stroke = 1) +
    geom_smooth(method = "lm", color = "red", se = FALSE, formula = y ~ x) +
    labs(
      title = paste("Regression Plot:", site_name, " (", start_date, " to ", end_date, ")"),
      x = expression("Flow (m"^"3"/"s)"),
      y = "SSC (mg/l)"
    ) +
    theme_minimal() +  # Optional: Customize the theme if needed
    theme(legend.position = "none") +  # Optional: Remove legend if not needed
    annotate(
      "text", 
      x = min(merged_df$Flow), 
      y = max(merged_df$result), 
      label = formula_text, 
      hjust = 0, 
      vjust = 1,
      color = "blue",
      size = 4
    ) +
    annotate(
      "text", 
      x = min(merged_df$Flow), 
      y = max(merged_df$result) - 400, 
      label = r_squared_text, 
      hjust = 0, 
      vjust = 1,
      color = "forestgreen",
      size = 4
    )
    # Print and close the plot
  print(Regression)
  dev.off()  # Close the PNG device
  
}
#End loop ----------------------------------------------------------------------

#Filter out duplicates
statistics_table <- distinct(statistics_table, Site, .keep_all = TRUE)
# Print the resulting statistical data frame
print(statistics_table)
# Specify the Excel file path and name
excel_file <- "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/Regressions/statistics_output.xlsx"
# Write the data frame to an Excel file
write_xlsx(statistics_table, excel_file)



