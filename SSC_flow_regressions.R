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
library(ggdist)
library(ggtext)

#Get flow data #################################################################

#Set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/Land/EROSION_MONITORING/ISCO_Programme/Hilltop/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")

# Get measurement list for respective sites 
sitelist <- SiteList(dfile, "")
Hilltop::SiteList(dfile)

# Date range. 
date1 <- "30-June-2016 00:00:00"
date2 <- "01-June-2025 00:00:00"

#Measurements/data that we want to pull from the Hilltop file. Add SS if needed here. 
measurement <- c(	'Suspended Sediment Concentration', "Flow")

#Loop 1 through sites to get data ----------------------------------------------
name <- data.frame(sitelist)
name$Sites <- as.character(name$sitelist)

site_no <- length(sitelist)
site_id <- sitelist

melt <- NULL  # Initialize an empty data frame to store the results

for (j in 1:site_no) {
  tryCatch({
    Multiple_sites <- GetData(dfile, site_id[j], measurement, date1, date2) 
    Multiple_sites_id <- do.call(rbind, lapply(Multiple_sites, function(x) cbind(zoo::fortify.zoo(x),
                                                                                 SiteName = attr(x, 'SiteName'), Measurement = attr(x, 'Measurement')))) %>% 
      dplyr::rename(zoodata='x') %>% 
      dplyr::rename(Site=SiteName)
    
    melt <- bind_rows(melt, Multiple_sites_id)
  }, error = function(e) {
    cat(paste("Error fetching data for site:", site_id[j], ". Skipping this site.\n"))
    return(NULL)  # Return NULL inside the tryCatch block
  })
  next  # Skip to the next iteration outside the tryCatch block
}
# End loop 1 ---------------------------------------------------------------------

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

SSC <- filter(melt, Measurement == "Suspended Sediment Concentration")

#SSC <- as.data.frame(sapply(SSC, gsub, pattern = "<|>", replacement = ""))
SSC$SampleTaken <- as.POSIXct(SSC$SampleTaken, format = "%d/%m/%Y %H:%M:%S")
SSC$SampleTaken <- lubridate::round_date(SSC$SampleTaken, "15 minutes") 
#SSC$SampleTaken <-as.character(SSC$SampleTaken) 
#Flow$SampleTaken <-as.character(Flow$SampleTaken) 

# Join the datasets together
merged <- merge(Flow, SSC, by = "SampleTaken" )

# Merge the flow and concentration datasets
merged_clean <- merged[merged$SiteName.x == merged$SiteName.y, ]
merged_clean$SiteName <- merged_clean$SiteName.x
merged_clean$Flow <- merged_clean$Flow.x
merged_clean$SSC <- merged_clean$Flow.y  # Assuming Flow.y actually contains SSC values

# Drop the old columns and clean up
merged_clean <- merged_clean[, !names(merged_clean) %in% c("SiteName.x", "SiteName.y", "Flow.x", "Flow.y")]
merged_clean <- merged_clean[, c("SampleTaken", "SiteName", setdiff(names(merged_clean), c("SampleTaken", "SiteName")))]
merged_clean$SampleTaken <- as.POSIXct(SSC$SampleTaken, format = "%d/%m/%Y %H:%M:%S")

head(merged_clean)

merged_clean <- merged_clean[,c(1,2,5,6)]
head(merged_clean, 20)

write.csv(merged_clean, file = "I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/SedimentLoads/Outputs/Regressions/merged_clean_WL.csv", row.names = FALSE)

####    Export to Manager. Rest of script unchecked. ###########################

####    Output    ##############################################################

#Set working directory for outputs
setwd('I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/Regressions')

# Create an empty data frame to store statistics
statistics_table <- data.frame(
  SiteName = character(),
  Iteration = integer(),
  Slope_lm = numeric(),
  Intercept_lm = numeric(),
  RSquared_lm = numeric(),
  Slope_SE_lm = numeric(),  # Standard error for Slope
  Intercept_SE_lm = numeric(),  # Standard error for Intercept
  RSquared_log = numeric(),
  RSquared_power = numeric(),
  RSquared_poly = numeric(),
  RSquared_exp = numeric(),
  stringsAsFactors = FALSE
)

#Loop 2 through sites-----------------------------------------------------------
for (i in sitelist) { 
  
  ###  Get puddle SSC data  ####################################################
  
  MyData <- getPuddleData(
    query_option = "fullPuddleHilltop",
    fromDate = "01-06-2021",
    toDate = "12-02-2023",
    catchments = "",
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
    
    # Fit logarithmic model
    log_model <- lm(log(result) ~ log(Flow), data = merged_df, na.action = na.exclude)
    # Fit power model
    power_model <- lm(result ~ I(Flow^2), data = merged_df, na.action = na.exclude)
    # You can adjust the exponent as needed
    # Fit polynomial model (e.g., quadratic)
    poly_model <- lm(result ~ poly(Flow, 2), data = merged_df, na.action = na.exclude)
    # Adjust the degree (2 in this case) as needed
    # Fit exponential model
    exp_model <- lm(result ~ exp(Flow), data = merged_df, na.action = na.exclude)
    
    
    # Extract coefficients and R-squared
    coef_slope <- round(coef(lm_model)[2], 2)
    coef_intercept <- round(coef(lm_model)[1], 2)
    r_squared_lm <- round(summary(lm_model)$r.squared, 2)
    # Extract coefficients and standard errors
    coefficients <- coef(lm_model)
    se <- summary(lm_model)$coefficients[, "Std. Error"]
    # Extract specific standard errors
    Slope_SE <- round(se["Flow"], 2)
    Intercept_SE <- round(se["(Intercept)"], 2)
    r_squared_log <- round(summary(log_model)$r.squared, 2)
    r_squared_power <- round(summary(power_model)$r.squared, 2)
    r_squared_poly <- round(summary(poly_model)$r.squared, 2)
    r_squared_exp <- round(summary(exp_model)$r.squared, 2)
    
    # Create a new row with statistics
    new_row <- data.frame(
      SiteName = site_name,
      Iteration = i,  # You may want to specify an iteration number here
      Slope_lm = coef_slope,
      Intercept_lm = coef_intercept,
      RSquared_lm = r_squared_lm,
      Slope_SE_lm = se["Flow"],
      Intercept_SE_lm = se["(Intercept)"],
      RSquared_log = r_squared_log,
      RSquared_power = r_squared_power,
      RSquared_poly = r_squared_poly,
      RSquared_exp = r_squared_exp
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
  # Create standard error text
  std_error_text_slope <- paste("Standard Error Slope =", Slope_SE)
  std_error_text_intercept <- paste("Standard Error Intercept =", Intercept_SE)
  
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
      title = paste(
        "Regression Plot:", 
        site_name, 
        "\n (", start_date, " to ", end_date, ")"
      ),
      x = expression("Flow (m"^"3"/"s)"),
      y = "SSC (mg/l)"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  # Extract y-axis range
  y_range <- ggplot_build(Regression)$layout$panel_scales_y[[1]]$range$range
  
  # Define annotations
  annotations <- list(
    list(label = formula_text, color = "blue", vjust = 0.9),
    list(label = r_squared_text, color = "forestgreen", vjust = 0.8),
    list(label = std_error_text_slope, color = "brown", vjust = 0.7),
    list(label = std_error_text_intercept, color = "deeppink4", vjust = 0.6)
  )
  
  # Add annotations using geom_text()
  for (i in seq_along(annotations)) {
    annotation <- annotations[[i]]
    y_pos <- y_range[2] - (i * (y_range[2] - y_range[1]) / (length(annotations) + 1))
    
    Regression <- Regression +
      annotate(
        "text",
        x = min(merged_df$Flow),
        y = y_pos,
        label = annotation$label,
        hjust = 0,    # Adjusted to align to the left
        vjust = annotation$vjust,     # Adjusted vjust based on annotation
        color = annotation$color,
        size = 4
      )
  }
  print(Regression)
  dev.off()  # Close the PNG device
}

#End loop ----------------------------------------------------------------------

######## Output Tables #########

#Filter out duplicates
statistics_table <- distinct(statistics_table, SiteName, .keep_all = TRUE)
# Print the resulting statistical data frame
print(statistics_table)
# Specify the Excel file path and name
excel_file <- "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/Regressions/statistics_output.xlsx"
#excel_file2 <- "I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/Outputs/Regressions/merged_df.xlsx"
# Write the data frame to an Excel file
write_xlsx(statistics_table, excel_file)
# Write the merged dataframe to an Excel file for use in GenDist:
#write_xlsx(merged_df, excel_file2)

################################################################################
