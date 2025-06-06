###################### Output Integration for Shiny ############################
### 03/04/2025
### Dr Ashton Eaves

#-------------------------------------------------------------------------------
  
library(data.table)
library(lubridate)
library(dplyr)


df <- fread("I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/2024analysis/20210701_to_20230212/measure_df_July2021_Feb2023_WL.csv")
df2 <- fread("I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/2024analysis/20230212_to_20230219/measure_df_CycloneGabrielle_Feb2023_WL.csv")
df3 <- fread("I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/2024analysis/20230219_to_20240630/measure_df_Feb2023_June2024_WL.csv")
df4 <- fread("I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/2024analysis/20210701_to_20240630_TukiTurb/measure_df_July2021_June2024_TURB.csv")

# Fix timestamps. If NAs are produced, fix in excel by converting to dd/mm/yyy hh:mm:ss format.
df$SampleTaken <- as.POSIXct(df$SampleTaken, format = "%d/%m/%Y %H:%M:%S", na.rm = TRUE)
df2$SampleTaken <- as.POSIXct(df2$SampleTaken, format = "%d/%m/%Y %H:%M:%S", na.rm = TRUE)
df3$SampleTaken <- as.POSIXct(df3$SampleTaken, format = "%d/%m/%Y %H:%M:%S", na.rm = TRUE)
df4$SampleTaken <- as.POSIXct(df4$SampleTaken, format = "%d/%m/%Y %H:%M:%S", na.rm = TRUE)
#format(df$SampleTaken, "%Y-%m-%d %H:%M:%S")
#format(df3$SampleTaken, "%Y-%m-%d %H:%M:%S")

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
df_combined[, SampleHour := as.POSIXct(SampleTaken)] # Ensure it's datetime
df_combined[, SampleHour := floor_date(SampleHour, unit = "hour")] # Round to nearest hour

# Step 1: Aggregate by SiteName and SampleHour
df_hourly <- df_combined[, .(
  Flow = mean(Flow, na.rm = TRUE),
  PredConc = mean(PredConc, na.rm = TRUE),
  Load = mean(Load, na.rm = TRUE)
  ), by = .(SiteName, SampleHour)]

# Step 2: Order and accumulate load by site
setorder(df_hourly, SiteName, SampleHour)

# Step 3: Accumulate Load × 3600 (convert hourly rate to total load in tonnes)
df_hourly[, AccumLoad := cumsum(Load * 3600), by = SiteName]

#Change datetime column name:
df_hourly <- df_hourly %>%
  rename(SampleTaken = SampleHour)

#Output
write.csv(df_hourly, file = "I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/SedimentLoads/Outputs/df_ISCO_Hourly.csv", row.names = FALSE)

# For shiny app:
write.csv(df_hourly, "I:/Land/EROSION_MONITORING/ISCO_Programme/R_analysis/SedimentLoads/SedimentLoads/app/measure.csv", row.names = FALSE)
