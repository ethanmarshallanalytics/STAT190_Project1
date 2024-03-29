## Gearbox Data

rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)


## -----------------------------Gearbox Bearing Part 1
sensor_files_1 <- list.files("Bear3/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 2/Gearbox IMS Bearing 2")
sensor_files_1

combined_data_1 <- data.frame()

# Loop through the list of files
for (file in sensor_files_1[-1]) {
  
  
  data <- read.csv(paste0("Bear3/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 2/Gearbox IMS Bearing 2/", file), header = FALSE)
  print(nrow(data))
  # Append the data to the combined_data data frame
  combined_data_1 <- rbind(combined_data_1, data)
}

# Write the combined data to a file
write.csv(combined_data_1, "Bear3/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 2/Gearbox IMS Bearing 2/gearbox_3_p2.csv")


## ------------------------------------------Gearbox Bearing Part 2
sensor_files_2 <- list.files("Project1Data/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 1/Gearbox IMS Bearing 1")
sensor_files_2

combined_data_2 <- data.frame()

# Loop through the list of files
for (file in sensor_files_2[-1]) {
  
  
  data <- read.csv(paste0("Project1Data/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 1/Gearbox IMS Bearing 1/", file), header = FALSE)
  print(nrow(data))
  # Append the data to the combined_data data frame
  combined_data_2 <- rbind(combined_data_2, data)
}

# Write the combined data to a file
write.csv(combined_data_2, "Project1Data/gearbox_2_p2.csv")


## ------------------------------------------Gearbox Bearing Part 3
sensor_files_3 <- list.files("Project1Data/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 2/Gearbox IMS Bearing 2")
sensor_files_3

combined_data_3 <- data.frame()

# Loop through the list of files
for (file in sensor_files_3[-1]) {
  
  data <- read.csv(paste0("Project1Data/Wind Turbine Data Batch 2 - Gearbox IMS Bearing 2/Gearbox IMS Bearing 2/", file), header = FALSE)
  print(nrow(data))
  # Append the data to the combined_data data frame
  combined_data_3 <- rbind(combined_data_3, data)
}


# Write the combined data to a file
write.csv(combined_data_3, "Project1Data/gearbox_3_p2.csv")

## ------------------------------------------Gearbox Oil Temp
sensor_files_4 <- list.files("F2/Wind Turbine Data Batch 2 - Fault Status Codes/F2")
sensor_files_4

combined_data_4 <- data.frame()

# Loop through the list of files
for (file in sensor_files_4[-1]) {
  
  data <- read.csv(paste0("RPM2/Wind Turbine Data Batch 2 - Generator RPM/Generator RPM/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_data_4 <- rbind(combined_data_4, data)
}


# Write the combined data to a file
write.csv(combined_data_4, "RPM2/Wind Turbine Data Batch 2 - Generator RPM/Generator RPM/generatorRPM_p2.csv")

## ------------------------ Windspeed
wind_files <- list.files("Project1Data/Windspeed")
wind_files

wind_data <- data.frame()

# Loop through the list of files
for (file in wind_files[-1]) {
  
  data <- read.csv(paste0("Project1Data/Windspeed/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  wind_data <- rbind(wind_data, data)
}

# Write the combined data to a file
write.csv(wind_data, "Project1Data/windspeed.csv")


# Fault occurred in the next 12/24 hours
# Lag function min/max
