################################################################################
#Script to calculate turbidity and in turn SSC from turbidity
#Created 16/08/2023 by Ashton Eaves

# Load libraries 
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

#set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/306 HCE Project/Hilltop/ISCO_Processing.dsn")

# Get site list or measurement list for respective sites 
sitelist <- SiteList(dfile, "")
Hilltop::SiteList(dfile)

#ISCO sites subset
sitelist <- sitelist[sitelist == "Karamu Stream at Floodgates" | 
                       sitelist == "Tukituki River at Red Bridge"]

################################################################################
#Measurements/data that we want to pull from the Hilltop file 
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', 'Turbidity (FNU)', "Flow")  

# Date range. 
date1 <- "19-Feb-2023 00:00:00"
date2 <- "01-Jul-2024 00:00:00"

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

# Turbidity rolling min loop-------------------------------------

for (j in 1:site_no) {
  
  # Step 1: Filter data for Turbidity (FNU)
  turbidity_fnu <- melt %>%
    filter(Measurement == "Turbidity (FNU)")
  
  # Ensure that there is data in turbidity_fnu before proceeding
  if (nrow(turbidity_fnu) > 0) {
    
    # Step 2: Ensure 'zoodata' is numeric, and filter out rows with no valid zoodata values
    turbidity_fnu <- turbidity_fnu %>%
      mutate(zoodata = as.numeric(zoodata)) %>%
      filter(!is.na(zoodata))  # Keep only rows where zoodata is not NA
    # Check if there are still rows left after filtering invalid zoodata values
    if (nrow(turbidity_fnu) > 0) {
      # Step 3: Perform rolling minimum (for a window of 4, for example)
      turbidity_fnu <- turbidity_fnu %>%
        group_by(Site) %>%  # Adjust based on actual column name, e.g., 'Site'
        mutate(RollingMin = rollapply(zoodata, width = 4, FUN = min, align = "right", fill = NA))
      # Step 4: Select only relevant columns using 'Index' instead of 'SampleTaken'
      turbidity_fnu <- turbidity_fnu %>%
        select(Index, Site, RollingMin)

    } else {
      warning(paste("No valid zoodata for site", site_id[j]))
    }
  } else {
    warning(paste("No data for Measurement 'Turbidity (FNU)' for site", site_id[j]))
  }
}
# End loop ----- 
  
# Rename turbidity_fnu 
colnames(turbidity_fnu) <- c("SampleTaken", "SiteName","FNU_RollingMin")
turbidity_fnu$FNU_RollingMin <- as.character(turbidity_fnu$FNU_RollingMin)

# Rename column names for the new dataframe called 'melt' 
colnames(melt) <- c("SampleTaken", "Flow", "SiteName","Measurement")

# Ensure there are no NA or empty names
if (any(is.na(colnames(melt))) || any(colnames(melt) == "")) {
  stop("Column names contain NA or empty values.")
}

# Now prepare turbidity_fnu by replacing Flow values with FNU_RollingMin values
turbidity_fnu <- turbidity_fnu %>%
  mutate(Flow = FNU_RollingMin,     # Transfer values from FNU_RollingMin to Flow
         Measurement = "FNU_RollingMin") %>%  # Set Measurement to FNU_RollingMin
  select(SampleTaken, Flow, SiteName, Measurement)  # Select relevant columns in correct order

# Remove old "Turbidity (FNU)" entries from melt
melt <- melt %>%
  filter(Measurement != "Turbidity (FNU)")

# Combine the updated turbidity_fnu back into melt
melt <- bind_rows(melt, turbidity_fnu)

# Data pulled from Hilltop has different time frequencies. 
# The aggregate function is used to aggregate data to 15 minute intervals.
melt$SampleTaken <-  lubridate::floor_date(melt$SampleTaken, "15 minutes")
melt$SampleTaken <- as.POSIXct(melt$SampleTaken, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)

Flow <- filter(melt, Measurement == "Flow")
Flow$Flow <- as.numeric(Flow$Flow, na.rm = TRUE)
Flow <- Flow %>% group_by(SampleTaken, SiteName, Measurement) %>%
  summarise(Flow = mean(Flow))
Flow <- Flow[,c(1,2,4)]

###############################################################################

SSC <- filter(melt, Measurement %in% c("Suspended Sediment Concentration", "Suspended Solids", "FNU_RollingMin"))
SSC <- as.data.frame(sapply(SSC, gsub, pattern = "<|>", replacement = ""))
SSC$SampleTaken <- as.POSIXct(SSC$SampleTaken, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)
SSC$SampleTaken <- lubridate::round_date(SSC$SampleTaken, "15 minutes") 

# Convert to character to merge flow and concentration
Flow$SampleTaken <-as.character(Flow$SampleTaken) 
SSC$SampleTaken <-as.character(SSC$SampleTaken) 
merged <- merge(Flow, SSC, by = c("SampleTaken", "SiteName"))
# Rename merged columns
colnames(merged) <- c('SampleTaken','Site', 'Flow', 'Conc', 'Measurement2')
# Convert back to date-time and Conc to numeric
merged$SampleTaken <- as.POSIXct(merged$SampleTaken, format = "%Y-%m-%d %H:%M:%S")
merged$Conc <- as.numeric(merged$Conc)
Flow$SampleTaken <- as.POSIXct(Flow$SampleTaken , format = "%Y-%m-%d %H:%M:%S")

# Recode the Measurement2 column
merged <- merged %>%
  mutate(Measurement2 = case_when(
    Measurement2 == 'Suspended Sediment Concentration' ~ 'SSC',
    Measurement2 == 'Suspended Solids' ~ 'SS',
    Measurement2 == 'FNU_RollingMin' ~ 'FNU_Min',
    TRUE ~ Measurement2
  ))

# Reshape the data from long to wide format, suppressing the warning by allowing list columns
merged_wide <- merged %>%
  pivot_wider(names_from = Measurement2, values_from = Conc, values_fn = list)

# Filter out rows where the list columns contain 'Null'
merged_wide2 <- merged_wide %>%
  filter(!sapply(SSC, function(x) all(x == 'Null')))
# Drop SS as empty
merged_wide2 <- merged_wide2[,c(1,2,3,4,6)]

# Check for multi value rows - sedi gaugings etc.
#multi_value_rows <- sapply(merged_wide2$FNU_Min, length) > 1
#which(multi_value_rows)
#multi_value_rows <- sapply(merged_wide2$SSC, length) > 1
#which(multi_value_rows)

merged_wide2$FNU_Min <- as.numeric(unlist(merged_wide2$FNU_Min))
merged_wide2$SSC <- as.numeric(unlist(merged_wide2$SSC))

merged_wide2$Flow <- as.numeric(merged_wide2$Flow)
merged_wide2$FNU_Min <- as.numeric(merged_wide2$FNU_Min)
merged_wide2$SSC <- as.numeric(merged_wide2$SSC)

# Group by timestamp and site (if relevant) and calculate the mean for FNU_Min
merged_wide2$FNU_Min <- sapply(merged_wide2$FNU_Min, function(x) as.numeric(mean(x)))
merged_wide2$SSC <- sapply(merged_wide2$SSC, function(x) as.numeric(mean(x)))

# Remove rows where FNU_Min or SSC are NA
merged_wide2 <- merged_wide2 %>%
  filter(!is.na(FNU_Min) & !is.na(SSC))

########### Define regression ##################################################



#####  Write out merged_wide2 for external regression analysis ######################
write.csv(merged_wide2, file = "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/merged_wide2.csv", row.names = FALSE)

###############################################################################

summary(merged1$Value)


Final$merged1.Value <- as.numeric(Final$merged1.Value)
summary(merged1$Value)

###### Add filter to remove outliers

# Calculate the mean and standard deviation of the values
mean_value <- mean(merged1$Value)
sd_value <- sd(merged1$Value)

# Define a threshold for identifying high outliers (e.g., 3 times the standard deviation)
#threshold <- mean_value + 3 * sd_value
threshold <- 200

# Subset the data frame to remove rows with high outliers
filtered_df <- merged1[merged1$Value <= threshold, ]

# Print the filtered data frame
#print(filtered_df)

#_________________________________________________________________________________________
# Graph SSC vs turbidity

# Fit the linear regression model
lm_model <- lm(Conc ~ Value, data = filtered_df)

# Get R-squared value from the model summary
r_squared <- summary(lm_model)$r.squared

p <- ggplot(data = filtered_df, aes(x = Value, y = Conc)) +
  geom_point(size = 2, color = "blue") +    # Customize the points
  scale_y_continuous() +
  scale_x_continuous() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add regression
  labs(title = paste("Sediment Scatter Plot with Best-Fit Regression Line -", Sites), # Add title
       x = "Value (FNU)",                 # X-axis label
       y = "SSC (mg/l)") +               # Y-axis label
  geom_text(aes(x = max(Value), y = max(Conc),
                label = paste("y =", round(lm_model$coefficients[1], 2), "+", 
                              round(lm_model$coefficients[2], 2), "x")),
            color = "black", hjust = 1.75, vjust = 1) +
  geom_text(x = Inf, y = Inf,  # Position the text in the upper right corner
            label = paste("R-squared =", round(r_squared, 3)),
            hjust = 2, vjust = 5, nudge_x = -0.2, nudge_y = -0.2,
            size = 4, color = "black")
  theme_minimal()  
p
ggplotly(p)

# Convert the ggplot object to a plotly object
#interactive_plot <- ggplotly(p)

# View the interactive plot
#interactive_plot

################################################################################

