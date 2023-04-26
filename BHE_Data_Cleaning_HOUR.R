## LIBRARIES AND PACKAGES -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
#install.packages("naniar")
library(naniar)
library(imputeTS)

# df1 = read.csv("Project1Data/plot_data_hour.csv")

## ORIGINAL DATA -------
# read in windspeed.csv
wind <- read.csv("Project1Data/windspeed.csv")
# read in gearbox_oil_temp.csv
oil_temp <- read.csv("Project1Data/gearbox_oil_temp.csv")
# read in fault_codes.csv
fc <- read.csv("Project1Data/fault_codes.csv")
# read in generator_rpm.csv
rpm <- read.csv("Project1Data/generator_rpm.csv")
# read in gearbox 1, 2, and 3 data
g1 <- read.csv("Project1Data/gearbox_1.csv")
g2 <- read.csv("Project1Data/gearbox_2.csv")
g3 <- read.csv("Project1Data/gearbox_3.csv")
# read in active power data
ap <- read.csv("Project1Data/active_power.csv")
# read in ambient temp data
at <- read.csv("Project1Data/ambient_temp.csv")
# hydraulic_pressure
hp <- read.csv("Project1Data/hydraulic_pressure.csv")

## AGGREGATE SENSOR DATA -----
# convert datetimes and aggregate fault code times
fc$V2 <- ymd_hms(fc$V2) # change from chr to timestamp
fc$V3 <- ymd(fc$V3) # change from chr to date
fc$Round_Time <- round_date(fc$V2, "1 hour") # round datetime of fault code to 1 hour

# function to change data types, group on 60 minute intervals, and find average value, minimum value, maximum value
grouping <- function(data){
  data$V2 <- ymd_hms(data$V2)
  data$Round_Time <- round_date(data$V2, "1 hour")
  data <- data %>% group_by(V1, Round_Time) %>% summarise(Avg_Value = mean(V4, na.rm=TRUE),
                                                          Min_Value = min(V4, na.rm=TRUE),
                                                          Max_Value = max(V4, na.rm=TRUE))
}

# call functions on each dataset
wind = grouping(wind)
oil_temp = grouping(oil_temp)
rpm = grouping(rpm)
g1 = grouping(g1)
g2 = grouping(g2)
g3 = grouping(g3)
ap = grouping(ap)
at = grouping(at)
hp = grouping(hp)

# combine gearbox data together
gearbox <- rbind(g1, g2, g3)

# join fault code data to other data sets
df1 <- fc %>% 
  full_join(wind, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(oil_temp, by=c("Round_Time"="Round_Time", "V1"="V1")) %>% 
  full_join(rpm, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(gearbox, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(ap, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(at, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(hp, by=c("Round_Time"="Round_Time", "V1"="V1"))

# rename remaining columns to something meaningful
df1 <- df1 %>%
  rename("Turbine" = "V1",
         "Datetime" = "V2",
         "Date" = "V3",
         "Fault_Code" = "V4",
         "Status" = "V5",
         "Fault_Description" = "V6",
         "Fault_Type" = "V7",
         "Avg_Wind_Speed" = "Avg_Value.x",
         "Min_Wind_Speed" = "Min_Value.x",
         "Max_Wind_Speed" = "Max_Value.x",
         "Avg_Oil_Temp" = "Avg_Value.y",
         "Min_Oil_Temp" = "Min_Value.y",
         "Max_Oil_Temp" = "Max_Value.y",
         "Avg_Generator_RPM" = "Avg_Value.x.x",
         "Min_Generator_RPM" = "Min_Value.x.x",
         "Max_Generator_RPM" = "Max_Value.x.x",
         "Avg_Gearbox_Temp" = "Avg_Value.y.y",
         "Min_Gearbox_Temp" = "Min_Value.y.y",
         "Max_Gearbox_Temp" = "Max_Value.y.y",
         "Avg_Active_Power" = "Avg_Value.x.x.x",
         "Min_Active_Power" = "Min_Value.x.x.x",
         "Max_Active_Power" = "Max_Value.x.x.x",
         "Avg_Ambient_Temp" = "Avg_Value.y.y.y",
         "Min_Ambient_Temp" = "Min_Value.y.y.y",
         "Max_Ambient_Temp" = "Max_Value.y.y.y",
         "Avg_Hydraulic_Pressure" = "Avg_Value",
         "Min_Hydraulic_Pressure" = "Min_Value",
         "Max_Hydraulic_Pressure" = "Max_Value")

## DETERMINE Is_Fault and WRITE TO CSV -----
# corrections to Is_Fault, Fault_Type, and Fault_Description to deal with missing values
# assume no fault code occurred
df1$Fault_Type[is.na(df1$Fault_Type)] = "No Fault"
df1$Fault_Description[is.na(df1$Fault_Description)] = "No Fault"

# rename blank strings to "no fault"
df1$Fault_Type <- ifelse(df1$Fault_Type == "", "No Fault", df1$Fault_Type)
df1$Fault_Description <- ifelse(df1$Fault_Description == "", "No Fault", df1$Fault_Description)

# Create Is_Fault column on new data
df1$Is_Fault <- ifelse(df1$Fault_Description %in% 
                         c("Backup Battery Error"
                           ,"Converter Trip, External"
                           ,"Converter Tripped, Auto Start"
                           ,"Converter Tripped, General"
                           ,"Gear Oil Pressure Too High/Low"
                           ,"Gear Oil Pump/Blower Superheated"
                           ,"Gear Oil Temperature High"
                           ,"Gear Oil Temperature Low"
                           ,"Gear Oil Temp Sensor Warning"
                           ,"Gearoil Level Too Low"
                           ,"Geninv: 139 U-Phase Sharing"
                           ,"Geninv: 213 Undiagnosed,Delta2"
                           ,"Grdinv: 1 Interlock"
                           ,"Grease Level Low, Gen Bearings"
                           ,"Grease Level Low, Hub"
                           ,"Grid Filter Current Overload"
                           ,"Grid Filter Res Temp Error"
                           ,"Gridvolt<Lower Limit1"               
                           ,"Gridvolt<Lower Limit2"              
                           ,"Gridvolt<Lower Limit3"
                           ,"Gridvolt<Lower Limit4"
                           ,"High Pressure Warning: Blade X"
                           ,"High Upper Voltage Exceeded"
                           ,"Hub Pressureswitch Error"
                           ,"Hub: Blade A Valve Error"
                           ,"Hub: Blade B Valve Error"
                           ,"Hub: Blade C Valve Error"
                           ,"HubCheck valve fail - Stopped"
                           ,"Hs-Gen Gearbearing Superheated"
                           ,"Hs-Gen Gearbearing Temp Warning"
                           ,"Hs-Rot Gearbearing Temp Warning"
                           ,"Ims-Gen Gearbearing Temp Too High"
                           ,"Inv. Cooling  Water Temp High"
                           ,"Inv. Cooling Water Temp Warning"
                           ,"Inv.(Tow) Cool Water Pres. Low"
                           ,"Inv.(Tow) Cool Water Pres. Warning"
                           ,"Inverter Temperature High"
                           ,"Lmu Alarm Overspeed"
                           ,"Low Lower Voltage Exceeded"
                           ,"Low Oil Pressure, Blade A"
                           ,"Low Oil Pressure, Blade B"
                           ,"Low Oil Pressure, Blade C"
                           ,"Low Oil Pressure, Pump Station"
                           ,"Low Pitch Oil Pressure, Start"
                           ,"Main Bearing Temp Too High"
                           ,"Main Bearing Temp Warning"
                           ,"Mainbreaker Cut Out"
                           ,"No Lubrication, Blade A"
                           ,"No Lubrication, Blade B"
                           ,"No Lubrication, Blade C"
                           ,"No Lubrication, Gen Bearings"
                           ,"No Lubrication, Yaw System"
                           ,"No Valid Wind Data"
                           ,"Offlinefilter Motor Superheatd"
                           ,"Overspeed Hcu"
                           ,"Smoke In The A18 Box"
                           ,"Smoke In The A21 Box"
                           ,"Smoke In The A3 Box"
                           ,"Smoke In The A4 Box"
                           ,"Smoke In The Converter Breaker"
                           ,"Smoke In The Inverter"
                           ,"Slip Ring Error"
                           ,"Srsg Activated"
                           ,"Temperature In A18 Too High"
                           ,"Too Many Slip Ring Errors"
                           ,"Tower Conv. Cooling Water Low"
                           ,"Ups Battery Low"
                           ,"UPS Battery Low, Warning"
                           ,"Ups Bypass Error"
                           ,"Ups-Failure"
                           ,"Windspeed Too High To Operate"), 1, 0)

## Lagging Data (NICK RUN THIS)-----
# df1$Round_Time = ymd_hms(df1$Round_Time)
# df1$Datetime = ymd_hms(df1$Datetime)
# 
# df1 <- df1 %>%
#   group_by(Turbine) %>%
#   arrange(Round_Time) %>%
#   mutate(Is_Fault_Lag = ifelse(lead(Is_Fault, n = 1) == 1 | 
#                                  lead(Is_Fault, n = 2) == 1 | 
#                                  lead(Is_Fault, n = 3) == 1 | 
#                                  lead(Is_Fault, n = 4) == 1 | 
#                                  lead(Is_Fault, n = 5) == 1 | 
#                                  lead(Is_Fault, n = 6) == 1, 1, 0))

## Lag data 6 hours and Select unique Turbine and Round_Time entries ----
# df1 = read.csv("Project1Data/plot_data_hour.csv")

# convert to datetime formats
df1$Round_Time = ymd_hms(df1$Round_Time)
df1$Datetime = ymd_hms(df1$Datetime)

# lag data 6 hours
df1 <- df1 %>%
  group_by(Turbine) %>%
  arrange(Round_Time) %>%
  mutate(Is_Fault_Lag = ifelse(lead(Is_Fault, n = 1) == 1 | 
                                 lead(Is_Fault, n = 2) == 1 | 
                                 lead(Is_Fault, n = 3) == 1 | 
                                 lead(Is_Fault, n = 4) == 1 | 
                                 lead(Is_Fault, n = 5) == 1 | 
                                 lead(Is_Fault, n = 6) == 1, 1, 0))

# select unique entries
df1 <- df1 %>% 
  arrange(Datetime) %>%
  group_by(Turbine, Round_Time) %>%
  slice(1) %>%
  ungroup()

# write aggregated file to CSV
write.csv(df1, "Project1Data/plot_data_hour.csv", row.names=F)

## INTERPOLATE MISSING VALUES FOR ML MODEL ------
master_data = df1

master_data$Avg_Oil_Temp_inter = na_interpolation(master_data$Avg_Oil_Temp)
master_data$Min_Oil_Temp_inter = na_interpolation(master_data$Min_Oil_Temp)
master_data$Max_Oil_Temp_inter = na_interpolation(master_data$Max_Oil_Temp)

master_data$Avg_Generator_RPM_inter = na_interpolation(master_data$Avg_Generator_RPM)
master_data$Min_Generator_RPM_inter = na_interpolation(master_data$Min_Generator_RPM)
master_data$Max_Generator_RPM_inter = na_interpolation(master_data$Max_Generator_RPM)

master_data$Avg_Gearbox_Temp_inter = na_interpolation(master_data$Avg_Gearbox_Temp)
master_data$Min_Gearbox_Temp_inter = na_interpolation(master_data$Min_Gearbox_Temp)
master_data$Max_Gearbox_Temp_inter = na_interpolation(master_data$Max_Gearbox_Temp)

master_data$Avg_Active_Power_inter = na_interpolation(master_data$Avg_Active_Power)
master_data$Min_Active_Power_inter = na_interpolation(master_data$Min_Active_Power)
master_data$Max_Active_Power_inter = na_interpolation(master_data$Max_Active_Power)

master_data$Avg_Ambient_Temp_inter = na_interpolation(master_data$Avg_Ambient_Temp)
master_data$Min_Ambient_Temp_inter = na_interpolation(master_data$Min_Ambient_Temp)
master_data$Max_Ambient_Temp_inter = na_interpolation(master_data$Max_Ambient_Temp)

master_data$Avg_Hydraulic_Pressure_inter = na_interpolation(master_data$Avg_Hydraulic_Pressure)
master_data$Min_Hydraulic_Pressure_inter = na_interpolation(master_data$Min_Hydraulic_Pressure)
master_data$Max_Hydraulic_Pressure_inter = na_interpolation(master_data$Max_Hydraulic_Pressure)

write.csv(master_data, "Project1Data/master_data_hour.csv", row.names=F)
