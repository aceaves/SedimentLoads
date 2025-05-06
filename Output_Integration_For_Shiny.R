###################### Output Integration for Shiny ############################
### 03/04/2025
### Dr Ashton Eaves

#-------------------------------------------------------------------------------
  
library(data.table)
library(lubridate)


df <- fread("I:/306 HCE Project/R_analysis/2024analysis/20210701_to_20230212/measure_df_July2021_Feb2023_WL.csv")
df2 <- fread("I:/306 HCE Project/R_analysis/2024analysis/20230212_to_20230219/measure_df_CycloneGabrielle_Feb2023_WL.csv")
df3 <- fread("I:/306 HCE Project/R_analysis/2024analysis/20230219_to_20240630/measure_df_Feb2023_June2024_WL.csv")
df4 <- fread("I:/306 HCE Project/R_analysis/2024analysis/20230219_to_20240630/measure_df_Feb2023_June2024_TURB.csv")

df2$SampleTaken <- as.POSIXct(df2$SampleTaken, format = "%Y-%m-%d %H:%M:%S", na.rm = TRUE)

df_combined <- rbindlist(list(df, df2, df3, df4), use.names = TRUE, fill = TRUE)

df_combined <- df_combined[, 1:6, with = FALSE]
# Remove NAs
df_combined <- df_combined[!is.na(SampleTaken)]

# Ensure df_combined is sorted by time within each site
setorder(df_combined, SiteName, SampleTaken)

# Fix AccumLoad by accumulating Load for each site
df_combined[, Load := fifelse(is.na(Load), 0, Load)]
df_combined[, AccumLoad := cumsum(Load), by = SiteName]

# Convert SampleTaken to hourly timestamps
df_combined[, SampleHour := as.POSIXct(SampleTaken, tz = "UTC")] # Ensure it's datetime
df_combined[, SampleHour := floor_date(SampleHour, unit = "hour")] # Round to nearest hour

# Aggregate by SiteName and SampleHour
df_hourly <- df_combined[, .(
  Flow = mean(Flow, na.rm = TRUE),
  PredConc = mean(PredConc, na.rm = TRUE),
  Load = mean(Load, na.rm = TRUE),
  AccumLoad = max(AccumLoad, na.rm = TRUE)
), by = .(SiteName, SampleHour)]

#Change datetime column name:
df_hourly <- df_hourly %>%
  rename(SampleTaken = SampleHour)

# Convert SampleTaken to proper datetime format
df_hourly[, SampleTaken := as.POSIXct(SampleTaken, format = "%Y-%m-%d %H:%M:%S", tz = "Pacific/Auckland")]
df_hourly[, SampleTaken := format(SampleTaken, "%Y-%m-%d %H:%M:%S")]  # Ensure correct format

# Check for any NA values
sum(is.na(df$SampleTaken))  # If this is greater than 0, some dates failed to convert

#Output
write.csv(df_hourly, file = "I:/306 HCE Project/R_analysis/SedimentLoads/Outputs/df_ISCO_Hourly.csv", row.names = FALSE)

# For shiny app:
write.csv(df_hourly, "I:/306 HCE Project/R_analysis/SedimentLoads/SedimentLoads/app/measure.csv", row.names = FALSE)
