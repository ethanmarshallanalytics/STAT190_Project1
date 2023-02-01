# Opening Fault Codes Time Series

rm(list = ls())
library(tidyverse)
library(lubridate)
library(readr)


## Should be an easy for loop to add in all of the sensor_files and append them 
sensor_files = list.files("Fault Codes Time Series/Fault Codes Time Series")
sensor_files

# notice the first row is the column headers - you'll probably want to use header=FALSE in read.csv

for(i in 1:length(sensor_files)) {
  s2 = read.csv(header = FALSE, paste0("Fault Codes Time Series/Fault Codes Time Series/", sensor_files[i]))
}

##------------------ For Loop & Append
sensor_files <- list.files("Fault Codes Time Series/Fault Codes Time Series")

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
