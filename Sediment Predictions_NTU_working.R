################################################################################
#Script to calculate turbidity and in turn SSC from turbidity
#Created 16/08/2023 by Ashton Eaves

# Load libraries 
library(Hilltop)
library(dplyr)
library(tidyverse)
library(hms) 
library(lubridate) 
library(gt)
library(ggplot2)
library(plotly)

#set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/306 HCE Project/Sites/ISCO_Processing.dsn")

# Get measurement list for respective sites 
#measurementlist <- Hilltop::MeasurementList(dfile,"Tukituki River at Red Bridge")
SiteList(dfile, "")

#ISCO sites 
#Sites <-  c( "Tutaekuri River at Puketapu HBRC Site", "Tukituki River at Red Bridge", "Karamu Stream at Floodgates", "Esk River at Waipunga Bridge", "Mangaone River at Rissington", "Maraetotara River at Waimarama Road")
Sites <- "Karamu Stream at Floodgates" 

################################################################################
#Measurements/data that we want to pull from the Hilltop file 
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Turbidity (FNU) [Turbidity (FNU)]")  

#measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Turbidity (FNU) [Turbidity FNU (lab)]")  

Hilltop::SiteList(dfile)
# Date range. 'date2' = todays date 
date1 <- "01-Jul-2022 00:00:00"
#date2 <- format(Sys.time(), "%Y-%m-%d")
date2 <- "30-Jun-2023 00:00:00"

#____________________________________________________________________________________________________________________________________________________________________________________
# trial_loop. This was developed by Ahmed to pull data from multiple sites out of Hilltop  --------------------------------------------------------------
site_no <- length(Sites)
site_id <- Sites

method <- ""
interval <- ""

for(i in 1:site_no){
  
  Multiple_sites <- GetData(dfile, site_id[i] ,measurement, date1, date2) # You will struggle to use this format in most packages 
  
  #do this to make it more useful
  Multiple_sites_id <- do.call(rbind, lapply(Multiple_sites, function(x) cbind(zoo::fortify.zoo(x),
                                                                               SiteName = attr(x, 'SiteName'), Measurement = attr(x, 'Measurement')))) %>% 
    dplyr::rename(zoodata='x') %>% 
    dplyr::rename(Site=SiteName) 
  
  
  if(i==1){
    melt <- Multiple_sites_id } 
  else
  {melt<- rbind(melt, Multiple_sites_id)
  }
  #Multiple_sites_fin <- dplyr::bind_rows()
  
}
#________________________________________________________________________________________________________________________________________________________________________________________
# Rename column names for the new dataframe called 'melt' 
colnames(melt) <- c("SampleTaken", "Value", "SiteName","Measurement")

melt <- as.data.frame(sapply(melt, gsub, pattern = "<|>", replacement = ""))

# Use dplyr to filter the 'melt' table and extract SSC and flow data 
SSC <- filter(melt, Measurement %in% c("Suspended Sediment Concentration", "Suspended Solids"))
Value <- filter(melt, Measurement == "Turbidity (FNU)")
#Value <- filter(melt, Measurement == "Turbidity FNU (lab)")
#Flow$SampleTaken1 <-  lubridate::floor_date(Flow$SampleTaken, "15 minutes")


###############################################################################
# Create a new table with SSC data and associated turbidity data 
# Covert date to character in order to merge data 
SSC$SampleTaken <- as.POSIXct(SSC$SampleTaken, format = "%Y-%m-%d %H:%M:%S")


SSC$SampleTaken <- lubridate::round_date(SSC$SampleTaken, "15 minutes") 
SSC$SampleTaken <-as.character(SSC$SampleTaken) 
Value$SampleTaken <-as.character(Value$SampleTaken) 


merged <- merge(Value, SSC, by = "SampleTaken" )
merged <- merged[,c(1,2,3,5)]

colnames(merged) <- c('DateTime', 'Value', 'Site', 'Conc')


merged1 <- select(merged, DateTime, Value, Conc)
merged1$DateTime <- as.POSIXct(merged1$DateTime, format = "%Y-%m-%d %H:%M:%S")
merged1$Date <- format(as.POSIXct(merged1$DateTime,format='%m/%d/%Y %H:%M:%S'),format='%Y%m%d')
merged1$Date2 <- format(as.POSIXct(merged1$DateTime,format='%m/%d/%Y %H:%M:%S'),format='%d/%m/%Y')


merged1$Time <- format(merged1$DateTime, format = "%H%M%S")
merged1$Time1 <- format(merged1$DateTime, format = "%I:%M:%S %p")


merged1$Value <- as.numeric(merged1$Value)
#merged1$Value <- formatC(merged1$Value, digits = 2, format = "f")
#merged1$Value <- as.numeric(merged1$Value)
merged1$Conc <- as.numeric(merged1$Conc)
merged1$new <- ""


merged1 <- merged1 %>% filter(!Conc > 6000)

Final <- data.frame(merged1$Value, merged1$Date,merged1$Time)

nrow(Final)


#print(Final)
#print(Final[1:104,],right=T, row.names = FALSE)
#print(Final[201:296,],right=T, row.names = FALSE)


#setwd('I:/306 HCE Project/R_analysis/2023analysis/output')
#xlsx::write.xlsx(merged1, 'merged1NTU.xlsx')
#xlsx::write.xlsx(melt, 'melt.xlsx')

merged1$Value <- as.numeric(merged1$Value)
merged1$Conc <- as.numeric(merged1$Conc)

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

