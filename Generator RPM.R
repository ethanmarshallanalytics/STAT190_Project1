# Active Power Scrap

rpm_files <- list.files("Project1Data/Generator RPM")
rpm_files

combined_rpm <- data.frame()

# Loop through the list of files
for (file in rpm_files[-1]) {
  
  data <- read.csv(paste0("Project1Data/Generator RPM/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_rpm <- rbind(combined_rpm, data)
}


# Write the combined data to a file
write.csv(combined_rpm, "Project1Data/generator_rpm.csv")
