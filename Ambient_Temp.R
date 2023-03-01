# Ambient temperature data
rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)

at_files <- list.files("ambient_temperature/ambient_temperature")
at_files

combined_data_at <- data.frame()

# Loop through the list of files
for (file in at_files[-1]) {
  
  
  data <- read.csv(paste0("ambient_temperature/ambient_temperature/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_data_at <- rbind(combined_data_at, data)
}

# Write the combined data to a file
write.csv(combined_data_at, "Project1Data/ambient_temp.csv")
