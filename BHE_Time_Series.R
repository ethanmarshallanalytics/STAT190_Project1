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

## Lag Stuff ------------
master_data <- master_data %>%
  group_by(Turbine) %>%
  arrange(Round_Time) %>%
  mutate(prev_oil_temp = lag(Oil_Temp, n = 36)) %>%
  mutate(prev_generator_RPM = lag(Generator_RPM, n = 36)) %>%
  mutate(prev_wind_speed = lag(Wind_Speed, n = 36)) %>%
  mutate(prev_gearbox_temp = lag(Gearbox_Temp, n = 36)) %>%
  mutate(prev_active_power = lag(Active_Power, n = 36)) %>%
  mutate(prev_ambient_temp = lag(Ambient_Temp, n = 36)) %>%
  mutate(prev_hydraulic_pressure = lag(Hydraulic_Pressure, n = 36))

# Reorder Columns
master_data <- master_data[c("Turbine", "Datetime", "Date", "Fault_Code", "Status",
                             "Fault_Description", "Fault_Type", "Round_Time", "Oil_Temp", "prev_oil_temp",
                             "Generator_RPM", "prev_generator_RPM", "Wind_Speed", "prev_wind_speed", "Gearbox_Temp",
                             "prev_gearbox_temp", "Active_Power", "prev_active_power", "Ambient_Temp", "prev_ambient_temp",
                             "Hydraulic_Pressure", "prev_hydraulic_pressure")]
