# Opening Fault Codes Time Series

rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)

##------------------ For Loop & Append for Time Series Data
sensor_files <- list.files("Fault Codes Time Series/Fault Codes Time Series")
sensor_files
# Initialize an empty data frame to store the combined data from all files
combined_data <- data.frame()

# Loop through the list of files
for (file in sensor_files[-1]) {
  

  data <- read.csv(paste0("Fault Codes Time Series/Fault Codes Time Series/", file), header = FALSE)
  
# Append the data to the combined_data data frame
  combined_data <- rbind(combined_data, data)
}

# Write the combined data to a file
write.csv(combined_data, "Fault Codes Time Series/combined_data.csv")

# Changing Column Names
colnames(combined_data)[1] <- "location_id"
