# Windspeed & Genearator RPM

windspeed <- list.files("Windspeed/Windspeed")
windspeed

combined <- data.frame()

# Loop through the list of files
for (file in windspeed[-1]) {
  
  
  data <- read.csv(paste0("Windspeed/Windspeed/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined <- rbind(combined, data)
}

# Write the combined data to a file
write.csv(combined, "Windspeed/windspeed.csv")

#------------
rpm <- list.files("Generator RPM/Generator RPM")
rpm

rpm1 <- data.frame()

# Loop through the list of files
for (file in rpm[-1]) {
  
  
  data <- read.csv(paste0("Generator RPM/Generator RPM/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  rpm1 <- rbind(rpm1, data)
}

# Write the combined data to a file
write.csv(combined, "Generator RPM/generator_rpm.csv")
