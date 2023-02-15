# Active Power data
rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)

ap_files <- list.files("Project1Data/Active Power")
ap_files

combined_data_ap <- data.frame()

# Loop through the list of files
for (file in ap_files[-1]) {
  
  
  data <- read.csv(paste0("Project1Data/Active Power/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_data_ap <- rbind(combined_data_ap, data)
}

# Write the combined data to a file
write.csv(combined_data_ap, "Project1Data/active_power.csv")
