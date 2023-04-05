# Active Power data
rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)

## folder 1
ap_files <- list.files("Project1Data/Wind Turbine Data Batch 2 - Active Power/Active Power")
ap_files

combined_data_ap <- data.frame()

# Loop through the list of files
for (file in ap_files[-1]) {
  
  
  data <- read.csv(paste0("Project1Data/Wind Turbine Data Batch 2 - Active Power/Active Power/", file), header = FALSE)
  print(nrow(data))
  # Append the data to the combined_data data frame
  combined_data_ap <- rbind(combined_data_ap, data)
}

# Write the combined data to a file
write.csv(combined_data_ap, "Project1Data/active_power_p2_1.csv")

## folder 2
# ap_files2 <- list.files("Project1Data/Wind Turbine Data Batch 2 - Active Power 2/Active Power")
# ap_files2
# 
# combined_data_ap2 <- data.frame()
# 
# # Loop through the list of files
# for (file in ap_files2[-1]) {
#   
#   
#   data2 <- read.csv(paste0("Project1Data/Wind Turbine Data Batch 2 - Active Power 2/Active Power/", file), header = FALSE)
#   print(nrow(data2))
#   # Append the data to the combined_data data frame
#   combined_data_ap2 <- rbind(combined_data_ap2, data2)
# }
# 
# # Write the combined data to a file
# write.csv(combined_data_ap2, "Project1Data/active_power_p2_2.csv")
