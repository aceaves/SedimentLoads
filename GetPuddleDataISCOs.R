###  Get puddle SSC data  ####################################################

library(HBRCDataAccess)
library(Hilltop)
library(dplyr)

#Set file path to ISCO Hilltop file 
dfile <- HilltopData("I:/306 HCE Project/Hilltop/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")


# Get site list or measurement list for respective sites 
sitelist <- SiteList(dfile, "")
Hilltop::SiteList(dfile)

# Subset the list for analysis after Cyclone Gabby as many sites have no data after.
#sitelist <- sitelist[sitelist == "Wairoa River at Marumaru" ]
#sitelist <- sitelist[sitelist == "Waimaunu Stream at Duncans" | 
#                       sitelist == "Waikatuku Strm off Harrison Rd"]


#Loop through sites in Puddle---------------------------------------------------
#for (i in sitelist) { 

  MyData <- getPuddleData(
    query_option = "",
    fromDate = "01-07-2021",
    toDate = "01-07-2024",
    catchments = "",
    sites = sitelist,
    projects = "340204",
    measurements = "",
    detids = c("SSMUD", "SSSAND"),
  )
  head(MyData, 10)

#}
  
  # Stats for sand #############################################################
  
  # Create an empty list to store the results
  sandsplit_list <- list()
  
  # Create an empty data frame to store statistics or empty any existing data in the dataframe
  Statistics_splitsand <- data.frame(
    site_name = character(),
    Min = numeric(),
    Q1 = numeric(),
    Median = numeric(),
    Mean = numeric(),
    Q3 = numeric(),
    Max = numeric(),
    Sum = numeric(),
    stringsAsFactors = FALSE
  )
  
  #Loop 1 ----------------------------------------------------------------------
  
  for (i in sitelist) { 
    
    # Subset Flow for the current SiteName (replace 'Site' with the correct column name from MyData)
    SSSAND <- dplyr::filter(MyData, Site == i & DetID == "SSSAND")  # Add DetID filter for 'SSSAND'
    
    if (nrow(SSSAND) > 0) {
      
      # Ensure the 'Value' column is numeric
      SSSAND$Value <- as.numeric(SSSAND$Value)
      
      if (all(is.na(SSSAND$Value))) {
        warning(paste("All 'Value' data is NA for site:", i))
        next  # Skip to the next site if all values are NA
      }
      
      # Calculate statistics
      min_val <- min(SSSAND$Value, na.rm = TRUE)
      q1_val <- quantile(SSSAND$Value, 0.25, na.rm = TRUE)
      median_val <- median(SSSAND$Value, na.rm = TRUE)
      mean_val <- mean(SSSAND$Value, na.rm = TRUE)
      q3_val <- quantile(SSSAND$Value, 0.75, na.rm = TRUE)
      max_val <- max(SSSAND$Value, na.rm = TRUE)
      sum_val <- sum(SSSAND$Value, na.rm = TRUE)
      
      # Create a new row with statistics
      new_row <- data.frame(
        Site = i,
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
      sandsplit_list[[i]] <- SSSAND
      
      # Append the current iteration to the Statistics_Load
      Statistics_splitsand <- rbind(Statistics_splitsand, new_row)
    } else {
      warning(paste("No data for site:", i))
    }
  }
  
  
  print(Statistics_splitsand)
  
  # Repeat for mud  ###########################################################
  
  # Create an empty list to store the results
  mudsplit_list <- list()
  
  # Create an empty data frame to store statistics or empty any existing data in the dataframe
  Statistics_splitmud <- data.frame(
    site_name = character(),
    Min = numeric(),
    Q1 = numeric(),
    Median = numeric(),
    Mean = numeric(),
    Q3 = numeric(),
    Max = numeric(),
    Sum = numeric(),
    stringsAsFactors = FALSE
  )
  
  #Loop 2----------------------------------------------------------------------
  
  for (i in sitelist) { 
    
    # Subset Flow for the current SiteName (replace 'Site' with the correct column name from MyData)
    SSMUD <- dplyr::filter(MyData, Site == i & DetID == "SSMUD")  # Add DetID filter for 'SSMUD'
    
    if (nrow(SSMUD) > 0) {
      
      # Ensure the 'Value' column is numeric
      SSMUD$Value <- as.numeric(SSMUD$Value)
      
      if (all(is.na(SSMUD$Value))) {
        warning(paste("All 'Value' data is NA for site:", i))
        next  # Skip to the next site if all values are NA
      }
      
      # Calculate statistics
      min_val <- min(SSMUD$Value, na.rm = TRUE)
      q1_val <- quantile(SSMUD$Value, 0.25, na.rm = TRUE)
      median_val <- median(SSMUD$Value, na.rm = TRUE)
      mean_val <- mean(SSMUD$Value, na.rm = TRUE)
      q3_val <- quantile(SSMUD$Value, 0.75, na.rm = TRUE)
      max_val <- max(SSMUD$Value, na.rm = TRUE)
      sum_val <- sum(SSMUD$Value, na.rm = TRUE)
      
      # Create a new row with statistics
      new_row <- data.frame(
        Site = i,
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
      mudsplit_list[[i]] <- SSMUD
      
      # Append the current iteration to the Statistics_Load
      Statistics_splitmud <- rbind(Statistics_splitmud, new_row)
    } else {
      warning(paste("No data for site:", i))
    }
  }
  
  
  print(Statistics_splitmud)
