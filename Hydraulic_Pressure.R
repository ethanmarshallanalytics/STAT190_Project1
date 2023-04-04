# Hydraullic Pressure data
rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)

hp_files <- list.files("HP/Wind Turbine Data Batch 2 - Hydraulic Pressure/Hydraulic Pressure")
hp_files

combined_data_hp <- data.frame()

# Loop through the list of files
for (file in hp_files[-1]) {
  
  
  data <- read.csv(paste0("HP/Wind Turbine Data Batch 2 - Hydraulic Pressure/Hydraulic Pressure/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_data_hp <- rbind(combined_data_hp, data)
}

# Write the combined data to a file
write.csv(combined_data_hp, "HP/Wind Turbine Data Batch 2 - Hydraulic Pressure/Hydraulic Pressure/hydraulic_pressure_p2.csv")
