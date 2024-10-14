################################################################################
#Script to calculate turbidity and in turn SSC from turbidity
#Created 16/08/2023 by Ashton Eaves

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
#regression_output <- "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/Regressions/regression_output_excel.xlsx"
#regression <- read.xlsx(regression_output)
# Print the data
#print(regression)

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

# Remove outliers: Keep values within the threshold
outlier_threshold <- 3 * sd(merged_wide2$FNU_Min)

# Filter to keep values within 3 standard deviations from the mean
merged_wide2 <- merged_wide2 %>% 
  filter(abs(FNU_Min - mean(FNU_Min)) <= outlier_threshold)


#####  Write out merged_wide2 for external use ######################
#write.csv(merged_wide2, file = "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/merged_wide2.csv", row.names = FALSE)


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

# Apply the function to each site
results <- merged_wide2 %>%
  group_by(Site) %>%
  summarise(best_model_info = list(fit_models(cur_data()))) %>%
  unnest_wider(best_model_info)

# For Tukituki River at Red Bridge, ensure linear model results are used if needed
tukituki_data <- merged_wide2 %>% filter(Site == "Tukituki River at Red Bridge")
linear_model_tukituki <- lm(SSC ~ FNU_Min, data = tukituki_data)

# Prepare linear model results for Tukituki
linear_model_aic <- AIC(linear_model_tukituki)
linear_model_equation <- paste("SSC =", round(coef(linear_model_tukituki)[1], 4), "+", 
                               round(coef(linear_model_tukituki)[2], 4), "* FNU_Min")

# Update results for Tukituki if R-squared is NA
results <- results %>%
  mutate(best_model = ifelse(is.na(r_squared) & Site == "Tukituki River at Red Bridge", 
                             "linear", best_model),
         aic = ifelse(is.na(r_squared) & Site == "Tukituki River at Red Bridge", 
                      linear_model_aic, aic),
         equation = ifelse(is.na(r_squared) & Site == "Tukituki River at Red Bridge", 
                           linear_model_equation, equation),
         r_squared = ifelse(is.na(r_squared) & Site == "Tukituki River at Red Bridge", 
                            1, r_squared))

# View results
print(results)

# Calculate manual override regression equation for Karamu
# Fit a polynomial regression model explicitly
polynomial_model <- lm(SSC ~ FNU_Min + I(FNU_Min^2), data = karamu_data)

# Extract coefficients
coeffs <- coef(polynomial_model)

# Equation formatting
equation <- paste("SSC =", round(coeffs[1], 4), "+", 
                  round(coeffs[2], 4), "* FNU_Min +", 
                  round(coeffs[3], 4), "* FNU_Min^2")

# Get R-squared value
r_squared <- summary(polynomial_model)$r.squared

# Print results
print(paste("Equation:", equation))
print(paste("R-squared:", round(r_squared, 4)))


# Calculate manual override R-squared for Tukituki River at Red Bridge
predicted_values <- predict(linear_model_tukituki)
ss_total <- sum((tukituki_data$SSC - mean(tukituki_data$SSC))^2)
ss_residual <- sum((tukituki_data$SSC - predicted_values)^2)
r_squared_manual <- 1 - (ss_residual / ss_total)

# Print manual R-squared value
print(r_squared_manual)

######## Graph SSC vs turbidity ################################################

# Basic plots
ggplot(merged_wide2 %>% filter(Site == "Tukituki River at Red Bridge"), aes(x = FNU_Min, y = SSC)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("SSC vs. FNU_Min for Tukituki River at Red Bridge")

ggplot(merged_wide2 %>% filter(Site == "Karamu Stream at Floodgates"), aes(x = FNU_Min, y = SSC)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE) +  # Polynomial regression (degree 2)
  ggtitle("SSC vs. FNU_Min for Karamu Stream at Floodgates (Polynomial Fit)") +
  labs(x = "FNU_Min", y = "SSC")  # Axis labels


####### Tuki

# Filter the dataset for "Tukituki River at Red Bridge"
tuki_data <- merged_wide2 %>% filter(Site == "Tukituki River at Red Bridge")

# Fit the regression model for SSC ~ FNU_Min
lm_model <- lm(SSC ~ FNU_Min, data = tuki_data)

# Get R-squared value from the model summary
r_squared <- summary(lm_model)$r.squared

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
karamu_data <- merged_wide2 %>% filter(Site == "Karamu Stream at Floodgates")

# Fit a polynomial regression model explicitly
polynomial_model <- lm(SSC ~ FNU_Min + I(FNU_Min^2), data = karamu_data)

# Get coefficients and R-squared value
coeffs <- coef(polynomial_model)
r_squared <- summary(polynomial_model)$r.squared

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

