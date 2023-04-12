## LIBRARIES, PACKAGES, and DATA PREP -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
# install.packages("naniar")
library(naniar)
# install.packages("imputeTS")
library(imputeTS)

# Read in Plotting Data
plot_data = read.csv("Project1Data/plotting_data_1.csv")

# Change data types and remove extra columns
plot_data$Round_Time <- ymd_hms(plot_data$Round_Time) # change from chr to datetime
plot_data <- subset(plot_data, select = -c(Wind_Speed_Group, delta_temp)) 


## TIME SERIES MODEL ----
master_data = plot_data

master_data$Oil_Temp = na_interpolation(master_data$Oil_Temp)
master_data$Generator_RPM = na_interpolation(master_data$Generator_RPM)
master_data$Wind_Speed = na_interpolation(master_data$Wind_Speed)
master_data$Gearbox_Temp = na_interpolation(master_data$Gearbox_Temp)
master_data$Active_Power = na_interpolation(master_data$Active_Power)
master_data$Ambient_Temp = na_interpolation(master_data$Ambient_Temp)
master_data$Hydraulic_Pressure = na_interpolation(master_data$Hydraulic_Pressure)

write.csv(master_data, "Project1Data/master_data.csv", row.names=F)
