# Active Power Scrap

ap_files <- list.files("Active Power/Active Power")
ap_files

combined_ap <- data.frame()

# Loop through the list of files
for (file in ap_files[-1]) {
  
  data <- read.csv(paste0("Active Power/Active Power/", file), header = FALSE)
  
  # Append the data to the combined_data data frame
  combined_ap <- rbind(combined_ap, data)
}


# Write the combined data to a file
write.csv(combined_ap, "Active Power/combined_ap.csv")