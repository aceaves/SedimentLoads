################################################################################
#Script to calculate turbidity and in turn SSC from turbidity
#Created 16/08/2024 by Ashton Eaves

# Load libraries 
library(Hilltop)
library(dplyr)
library(scales)
library(ggplot2)
library(plotly)
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
library(purrr)

#set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/Land/EROSION_MONITORING/ISCO_Programme/Hilltop/ISCO_Processing.dsn")

# Get site list or measurement list for respective sites 
sitelist <- SiteList(dfile, "")
Hilltop::SiteList(dfile)

#ISCO sites subset
#sitelist <- sitelist[sitelist == "Karamu Stream at Floodgates" | 
#                       sitelist == "Tukituki River at Red Bridge"]
sitelist <- sitelist[sitelist == "Tukituki River at Red Bridge"]

################################################################################
#Measurements/data that we want to pull from the Hilltop file 
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', 'Turbidity (FNU)', "Flow")  

# Date range. 
date1 <- "01-Jul-2021 00:00:00"
date2 <- "12-Feb-2023 00:00:00"

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
# End loop --------------------------------------------------------------------
  
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
# Remove duplicate timesteps
Flow <- Flow %>% group_by(SampleTaken, SiteName, Measurement) %>%
  summarise(Flow = mean(Flow))
Flow <- Flow[,c(1,2,4)]
# Omit rows with negative values in columns 1, 2, and 4
Flow <- Flow[!( Flow[, 3] < 0), ]

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

# Unlist FNU_Min and ensure it's the same length as the original data frame
unlisted_FNU_Min <- unlist(merged_wide2$FNU_Min)
# Check if the lengths match
if (length(unlisted_FNU_Min) == nrow(merged_wide2)) {
  # Convert to numeric and assign back to the data frame
  merged_wide2$FNU_Min <- as.numeric(unlisted_FNU_Min)
} else {
  # If lengths do not match, investigate further
  warning("Length mismatch: Expected", nrow(merged_wide2), "but got", length(unlisted_FNU_Min))
}
# After assignment, handle NAs if necessary
merged_wide2$FNU_Min[is.na(merged_wide2$FNU_Min)] <- 0  # or other imputation method
# Average FNU_Min for each entry
merged_wide2$FNU_Min <- sapply(merged_wide2$FNU_Min, function(x) {
  if (length(x) > 0) {
    return(mean(as.numeric(x), na.rm = TRUE))  # Averaging and handling NAs
  } else {
    return(NA)  # Or any default value
  }
})

# Unlist SSC and ensure it's the same length as the original data frame
unlisted_SSC <- unlist(merged_wide2$SSC)
# Check if the lengths match
if (length(unlisted_SSC) == nrow(merged_wide2)) {
  # Convert to numeric and assign back to the data frame
  merged_wide2$SSC <- as.numeric(unlisted_SSC)
} else {
  # If lengths do not match, investigate further
  warning("Length mismatch: Expected", nrow(merged_wide2), "but got", length(unlisted_SSC))
}
# After assignment, handle NAs if necessary
merged_wide2$SSC[is.na(merged_wide2$SSC)] <- 0  # or other imputation method
# Average FNU_Min for each entry
merged_wide2$SSC <- sapply(merged_wide2$SSC, function(x) {
  if (length(x) > 0) {
    return(mean(as.numeric(x), na.rm = TRUE))  # Averaging and handling NAs
  } else {
    return(NA)  # Or any default value
  }
})

# Back to numeric
merged_wide2$Flow <- as.numeric(merged_wide2$Flow)
merged_wide2$FNU_Min <- as.numeric(merged_wide2$FNU_Min)
merged_wide2$SSC <- as.numeric(merged_wide2$SSC)

# Remove rows where FNU_Min or SSC are NA
merged_wide2 <- merged_wide2 %>%
  filter(!is.na(FNU_Min) & !is.na(SSC))

#####  Write out merged_wide2 for external use ######################
#write.csv(merged_wide2, file = "I:/306 HCE Project/R_analysis/SedimentLoads/Outputs/merged_wide2.csv", row.names = FALSE)


########### Define regression ##################################################

# Define the fit_models function
fit_models <- function(df) {
  models <- list()
  
  # Define models
  models$linear <- lm(SSC ~ FNU_Min, data = df)
  models$logarithmic <- try(lm(SSC ~ log(FNU_Min), data = df), silent = TRUE)
  models$exponential <- try(nls(SSC ~ a * exp(b * FNU_Min), data = df, 
                                start = list(a = 1, b = 0.1), algorithm = "port"), silent = TRUE)
  models$power <- try(nls(SSC ~ a * FNU_Min^b, data = df, 
                          start = list(a = 1, b = 0.1), algorithm = "port"), silent = TRUE)
  models$polynomial <- lm(SSC ~ poly(FNU_Min, 2), data = df)
  
  # Filter out models that failed
  models <- models[!sapply(models, inherits, "try-error")]
  
  # Compare models based on AIC
  aic_values <- map_dbl(models, AIC)
  best_model <- names(which.min(aic_values))
  
  # Initialize R-squared
  r_squared <- NA
  
  # Get R-squared for applicable models
  if (best_model %in% c("linear", "polynomial")) {
    r_squared <- summary(models[[best_model]])$r.squared
    # Set default R-squared for Tukituki site if NA
    if (is.na(r_squared) && best_model == "linear" && Site == "Tukituki River at Red Bridge") {
      r_squared <- 1
    }
  }
  
  # Extract coefficients and create the equation
  coeffs <- coef(models[[best_model]])
  equation <- switch(best_model,
                     linear = paste("SSC =", round(coeffs[1], 4), "+", round(coeffs[2], 4), "* FNU_Min"),
                     logarithmic = paste("SSC =", round(coeffs[1], 4), "+", round(coeffs[2], 4), "* log(FNU_Min)"),
                     exponential = paste("SSC =", round(coeffs[1], 4), "* exp(", round(coeffs[2], 4), " * FNU_Min)"),
                     power = paste("SSC =", round(coeffs[1], 4), "* FNU_Min^", round(coeffs[2], 4)),
                     polynomial = paste("SSC =", round(coeffs[1], 4), "+", 
                                        round(coeffs[2], 4), "* FNU_Min +", 
                                        round(coeffs[3], 4), "* FNU_Min^2"),
                     NA)
  
  # Return results
  return(list(best_model = best_model, aic = aic_values[best_model], 
              r_squared = r_squared, equation = equation))
}
#------------------------------------------------------------------------------
# Apply the function to each site
results <- merged_wide2 %>%
  group_by(Site) %>%
  summarise(best_model_info = list(fit_models(cur_data()))) %>%
  unnest_wider(best_model_info)

# Define a function to remove outliers based on residuals
remove_outliers <- function(model, data) {
  # Predict the values
  predicted_values <- predict(model, data)
  
  # Calculate residuals
  residuals <- data$SSC - predicted_values
  
  # Calculate the standard deviation of the residuals
  residual_sd <- sd(residuals, na.rm = TRUE)
  
  # Define the outlier threshold (2 standard deviations)
  threshold <- 2 * residual_sd
  
  # Remove rows with residuals greater than the threshold
  data_filtered <- data[abs(residuals) <= threshold, ]
  
  return(data_filtered)
}

# For Tukituki River at Red Bridge
tukituki_data <- merged_wide2 %>% filter(Site == "Tukituki River at Red Bridge")
linear_model_tukituki <- lm(SSC ~ FNU_Min, data = tukituki_data)

# Remove outliers from Tukituki data
tukituki_data_filtered <- remove_outliers(linear_model_tukituki, tukituki_data)

# Refit the linear model after removing outliers
linear_model_tukituki_filtered <- lm(SSC ~ FNU_Min, data = tukituki_data_filtered)

# Calculate AIC, equation, and R-squared for Tukituki
linear_model_aic <- AIC(linear_model_tukituki_filtered)
linear_model_equation <- paste("SSC =", round(coef(linear_model_tukituki_filtered)[1], 4), "+", 
                               round(coef(linear_model_tukituki_filtered)[2], 4), "* FNU_Min")
r_squared_manual <- summary(linear_model_tukituki_filtered)$r.squared

# Print the results for Tukituki
print(paste("Equation Tukituki:", linear_model_equation))
print(paste("R-squared Tukituki:", round(r_squared_manual, 3)))

#----------------------------

# For Karamu Stream at Floodgates
karamu_data <- merged_wide2 %>% filter(Site == "Karamu Stream at Floodgates")
polynomial_model <- lm(SSC ~ FNU_Min + I(FNU_Min^2), data = karamu_data)

# Remove outliers from Karamu data
karamu_data_filtered <- remove_outliers(polynomial_model, karamu_data)

# Refit the polynomial model after removing outliers
polynomial_model_filtered <- lm(SSC ~ FNU_Min + I(FNU_Min^2), data = karamu_data_filtered)

# Extract coefficients and R-squared for Karamu
coeffs <- coef(polynomial_model_filtered)
equation <- paste("SSC =", round(coeffs[1], 4), "+", 
                  round(coeffs[2], 4), "* FNU_Min +", 
                  round(coeffs[3], 4), "* FNU_Min^2")
r_squared_karamu <- summary(polynomial_model_filtered)$r.squared

# Print the results for Karamu
print(paste("Equation Karamu:", equation))
print(paste("R-squared Karamu:", round(r_squared_karamu, 3)))

######## Graph SSC vs turbidity ################################################

# Basic plots
ggplot(tukituki_data_filtered %>% filter(Site == "Tukituki River at Red Bridge"), aes(x = FNU_Min, y = SSC)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("SSC vs. FNU_Min for Tukituki River at Red Bridge")

ggplot(karamu_data_filtered %>% filter(Site == "Karamu Stream at Floodgates"), aes(x = FNU_Min, y = SSC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  # Polynomial regression (degree 2)
  ggtitle("SSC vs. FNU_Min for Karamu Stream at Floodgates (Polynomial Fit)") +
  labs(x = "FNU_Min", y = "SSC")  # Axis labels


####### Tuki

# Filter the dataset for "Tukituki River at Red Bridge"
tuki_data <- tukituki_data_filtered %>% filter(Site == "Tukituki River at Red Bridge")

# Fit the regression model for SSC ~ FNU_Min
lm_model <- lm(SSC ~ FNU_Min, data = tuki_data)
linear_model_tukituki_filtered <- lm(SSC ~ FNU_Min, data = tukituki_data_filtered)

# Get R-squared value from the model summary
r_squared <- summary(linear_model_tukituki_filtered)$r.squared

# Create the plot with further adjustments
p <- ggplot(data = tuki_data, aes(x = FNU_Min, y = SSC)) +
  geom_point(size = 2, color = "blue") +    # Customize the points
  scale_y_continuous() +
  scale_x_continuous() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # Add regression line
  labs(title = "SSC vs. FNU_Min for Tukituki River at Red Bridge",
       x = "FNU (Value)",                 # X-axis label
       y = "SSC (mg/l)") +               # Y-axis label
  
  # Move the equation text further up and to the right
  geom_text(aes(x = median(FNU_Min) + 0.3*diff(range(FNU_Min)), 
                y = min(SSC) + 0.2*diff(range(SSC)),
                label = paste("y =", round(lm_model$coefficients[1], 2), "+", 
                              round(lm_model$coefficients[2], 2), "* FNU_Min")),
            color = "black", hjust = 0.5, vjust = 1) +
  
  # Move the R-squared text further up and to the right
  geom_text(aes(x = median(FNU_Min) + 0.3*diff(range(FNU_Min)), 
                y = min(SSC) + 0.15*diff(range(SSC)),
                label = paste("R-squared =", round(r_squared, 3))),
            hjust = 0.5, vjust = 1, size = 4, color = "black") +
  
  theme_minimal()  # Use a minimal theme
# Display the plot
p
# Convert to an interactive plot using plotly
ggplotly(p)

########## Karamu 

# Filter the dataset for "Karamu Stream at Floodgates"
karamu_data <- karamu_data_filtered %>% filter(Site == "Karamu Stream at Floodgates")

# Fit a polynomial regression model explicitly
polynomial_model_filtered <- lm(SSC ~ FNU_Min + I(FNU_Min^2), data = karamu_data_filtered)

# Get coefficients and R-squared value
coeffs <- coef(polynomial_model_filtered)
r_squared <- summary(polynomial_model_filtered)$r.squared

# Create the plot with further adjustments
p <- ggplot(data = karamu_data, aes(x = FNU_Min, y = SSC)) +
  geom_point(size = 2, color = "blue") +    # Customize the points
  scale_y_continuous() +
  scale_x_continuous() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE, color = "red") +  # Polynomial fit (degree 2)
  labs(title = "SSC vs. FNU_Min for Karamu Stream at Floodgates (Polynomial Fit)",
       x = "FNU (Value)",                 # X-axis label
       y = "SSC (mg/l)") +               # Y-axis label
  
  # Move the equation text further up and to the right
  geom_text(aes(x = median(FNU_Min) + 0.3 * diff(range(FNU_Min)), 
                y = min(SSC) + 0.2 * diff(range(SSC)),
                label = paste("SSC =", round(coeffs[1], 4), "+", 
                              round(coeffs[2], 4), "* FNU_Min +", 
                              round(coeffs[3], 4), "* FNU_Min^2")),
            color = "black", hjust = 0.5, vjust = 1) +
  
  # Move the R-squared text further up and to the right
  geom_text(aes(x = median(FNU_Min) + 0.3 * diff(range(FNU_Min)), 
                y = min(SSC) + 0.15 * diff(range(SSC)),
                label = paste("R-squared =", round(r_squared, 4))),
            hjust = 0.5, vjust = 1, size = 4, color = "black") +
  
  theme_minimal()  # Use a minimal theme
# Display the plot
p
# Convert to an interactive plot using plotly
ggplotly(p)

################################################################################

############ Update regression table with results from above ###################

# Read regression file into a data frame
regression_output <- "I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/SedimentLoads/Outputs/Regressions/regression_output_turb.xlsx"
regression <- read.xlsx(regression_output)
# Print the data
print(regression)

###############################################################################

# Rerun this piece to remove midnight NAs.

# Data pulled from Hilltop has different time frequencies. 
# The aggregate function is used to aggregate data to 15 minute intervals.
melt$SampleTaken <-  lubridate::floor_date(melt$SampleTaken, "15 minutes")
melt$SampleTaken <- as.POSIXct(melt$SampleTaken, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)

Flow <- filter(melt, Measurement == "Flow")
Flow$Flow <- as.numeric(Flow$Flow, na.rm = TRUE)
# Remove duplicate timesteps
Flow <- Flow %>% group_by(SampleTaken, SiteName, Measurement) %>%
  summarise(Flow = mean(Flow))
Flow <- Flow[,c(1,2,4)]
# Omit rows with negative values in columns 1, 2, and 4
Flow <- Flow[!( Flow[, 3] < 0), ]

###############################################################################

# Subset merged_wide
merged_wide_sub <- merged_wide[,c(1,2,4)]
# Rename column names for the new dataframe 
colnames(merged_wide_sub) <- c("SampleTaken","SiteName","FNU_Min")
# Merge datasets
FlowFNU_bind <- merge(Flow, merged_wide_sub, by = c("SampleTaken", "SiteName"))
# Average FNU_Min for each entry
FlowFNU_bind$FNU_Min <- sapply(FlowFNU_bind$FNU_Min, function(x) {
  if (length(x) > 0) {
    return(mean(as.numeric(x), na.rm = TRUE))  # Averaging and handling NAs
  } else {
    return(NA)  # Or any default value
  }
})
# Remove rows where FNU_Min is NaN or 0
FlowFNU_bind <- FlowFNU_bind[!is.na(FlowFNU_bind$FNU_Min) & FlowFNU_bind$FNU_Min != 0, ]


# View the merged dataset
head(FlowFNU_bind)

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
  Flow1 <- dplyr::filter(FlowFNU_bind, SiteName == i)
  
  # Assuming 'SiteName' is the key for the lookup
  lookup_site <- i
  
  # Perform lookup to get the corresponding predicted concentration values for regression type
  lookup_result <- regression[regression$SiteName == lookup_site, c("RegressionType", "Slope", "Linear_Intercept", "Exp_Power", "Exp_X", "X_Squared", "Poly_X", "Poly_Intercept", "Log", "Log_Intercept", "Power_X", "Power_Exp")]
  
  # Check if the lookup was successful
  if (nrow(lookup_result) > 0) {
    regression_type <- lookup_result$RegressionType
    
    # Process according to regression type
    if (regression_type == "Exponential") {
      Flow1$PredConc <-  exp(lookup_result$Exp_X * Flow1$FNU_Min) * lookup_result$Exp_Power 
    } else if (regression_type == "Polynomial") {
      Flow1$PredConc <- lookup_result$X_Squared * Flow1$FNU_Min^2 + lookup_result$Poly_X * Flow1$FNU_Min + lookup_result$Poly_Intercept
    } else if (regression_type == "Log") {
      Flow1$PredConc <- lookup_result$Log * log(Flow1$FNU_Min) + lookup_result$Log_Intercept
    } else if (regression_type == "Power") {
      Flow1$PredConc <- lookup_result$Power_X * Flow1$FNU_Min ^ lookup_result$Power_Exp
    } else if (regression_type == "Linear") {
      Flow1$PredConc <- lookup_result$Slope * Flow1$FNU_Min + lookup_result$Linear_Intercept # slope in l/s
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
      Flow1$AccumLoad <- cumsum(Flow1$Load * Flow1$DiffSecs)  # Cumulative sum over load and time
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
setwd('I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/SedimentLoads/Outputs')

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
write.csv(Statistics_Load, file = "Statistics_Load_July2021_Feb2023_TURB.csv", row.names = FALSE)

######## Figure outputs ########################################################

#Loop 3 through sites-----------------------------------------------------------
for (i in sitelist) { 
  
  measure1 <- filter(measure_df, SiteName == i)
  merged2 <- filter(merged_wide_sub, SiteName == i)
  merged3 <- filter(merged_wide2, Site == i)
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
    geom_point(data = merged3_offset, aes(x = SampleTaken, y = SSC, color = Measurement2),colour = '#eebd1c', size = 2) +
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
    geom_point(data = merged3_offset, aes(x = SampleTaken, y = SSC, color = Measurement2),colour = '#eebd1c', size = 2) +
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
measure_df2 <- measure_df[,c(1,2,3,4,5,9,10)]
# Print the result
head(measure_df2)
#Format date time
format(df2$SampleTaken, "%Y-%m-%d %H:%M:%S")
# Change name accordingly
write.csv(measure_df2, file = "I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/SedimentLoads/Outputs/measure_df_July2021_June2024_TURB.csv", row.names = FALSE)

################################################################################
################################################################################
