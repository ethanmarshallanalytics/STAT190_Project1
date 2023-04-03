# Opening Fault Codes Time Series

rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)

##------------------ For Loop & Append for Time Series Data
# sensor_files <- list.files("Fault Codes Time Series/Fault Codes Time Series")
sensor_files <- list.files("Project1Data/Wind Turbine Data Batch 2 - Fault Status Codes/Fault Status Codes")
sensor_files
# Initialize an empty data frame to store the combined data from all files
combined_data_fc <- data.frame()

# Loop through the list of files
for (file in sensor_files[-1]) {
  
  data <- read.csv(paste0("Project1Data/Wind Turbine Data Batch 2 - Fault Status Codes/Fault Status Codes/", file), header = FALSE)
  print(nrow(data))
  # Append the data to the combined_data data frame
  combined_data_fc <- rbind(combined_data_fc, data)
}

# Write the combined data to a file
write.csv(combined_data_fc, "Project1Data/fault_codes_p2.csv")

# Changing Column Names
colnames(combined_data)[1] <- "location_id"
