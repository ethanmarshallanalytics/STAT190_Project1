## Gearbox Data

rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)


## -----------------------------Gearbox Bearing Part 1
sensor_files_1 <- list.files("Gearbox HS Bearing Temp Part 1/Drake Analytics Data Sets 2 Part 1")
sensor_files_1

combined_data_1 <- data.frame()

# Loop through the list of files
for (file in sensor_files_1[-1]) {
  
  
  data <- read.csv(paste0("Gearbox HS Bearing Temp Part 1/Drake Analytics Data Sets 2 Part 1/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_data_1 <- rbind(combined_data_1, data)
}

# Write the combined data to a file
write.csv(combined_data_1, "Gearbox HS Bearing Temp Part 1/gearbox_1.csv")


## ------------------------------------------Gearbox Bearing Part 2
sensor_files_2 <- list.files("Gearbox HS Bearing Temp Part 2/Drake Analytics Data Sets 2 Part 2")
sensor_files_2

combined_data_2 <- data.frame()

# Loop through the list of files
for (file in sensor_files_2[-1]) {
  
  
  data <- read.csv(paste0("Gearbox HS Bearing Temp Part 2/Drake Analytics Data Sets 2 Part 2/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_data_2 <- rbind(combined_data_2, data)
}

# Write the combined data to a file
write.csv(combined_data_2, "Gearbox HS Bearing Temp Part 2/gearbox_2.csv")


## ------------------------------------------Gearbox Bearing Part 3
sensor_files_3 <- list.files("Gearbox HS Bearing Temp Part 3/Gearbox HS Bearing Temp Part 3")
sensor_files_3

combined_data_3 <- data.frame()

# Loop through the list of files
for (file in sensor_files_3[-1]) {
  
  data <- read.csv(paste0("Gearbox HS Bearing Temp Part 3/Gearbox HS Bearing Temp Part 3/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_data_3 <- rbind(combined_data_3, data)
}


# Write the combined data to a file
write.csv(combined_data_3, "Gearbox HS Bearing Temp Part 3/gearbox_3.csv")

## ------------------------------------------Gearbox Oil Temp
sensor_files_4 <- list.files("Project1Data/Gearbox Oil Temperature")
sensor_files_4

combined_data_4 <- data.frame()

# Loop through the list of files
for (file in sensor_files_4[-1]) {
  
  data <- read.csv(paste0("Project1Data/Gearbox Oil Temperature/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_data_4 <- rbind(combined_data_4, data)
}


# Write the combined data to a file
write.csv(combined_data_4, "Project1Data/gearbox_oil_temp.csv")

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
