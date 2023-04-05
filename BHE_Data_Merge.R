## LIBRARIES AND PACKAGES -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
# install.packages("naniar")
library(naniar)

## Read in SENSOR DATA and OLD DATA------
# fault data
fc <- read.csv("Project1Data/fault_codes_p2.csv")

# oil_temp
oil_temp <- read.csv("Project1Data/gearbox_oil_temp_p2.csv")

# generator_rpm
rpm <- read.csv("Project1Data/generator_rpm_p2.csv")

# wind_speed
wind <- read.csv("Project1Data/windspeed_p2.csv")

# gearbox_temp
gearbox1 <- read.csv("Project1Data/gearbox_1_p2.csv")
gearbox2 <- read.csv("Project1Data/gearbox_2_p2.csv")
gearbox3 <- read.csv("Project1Data/gearbox_3_p2.csv")

# active_power
ap <- read.csv("Project1Data/active_power_p2.csv")

# ambient_temp
at <- read.csv("Project1Data/ambient_temp_p2.csv")

# hydraulic_pressure
hp <- read.csv("Project1Data/hydraulic_pressure_p2.csv")

# original data (plotting data)
plot_data <- read.csv("Project1Data/plotting_data.csv")


## AGGREGATE SENSOR DATA -----
# convert datetimes and aggregate fault code times
fc$V2 <- ymd_hms(fc$V2) # change from chr to timestamp
fc$V3 <- ymd(fc$V3) # change from chr to date
fc$Round_Time <- round_date(fc$V2, "10 minute")

# function to change data types, group on 10 minute intervals, and find average value
grouping <- function(data){
  data$V2 <- ymd_hms(data$V2)
  data$Round_Time <- round_date(data$V2, "10 minute")
  data <- data %>% group_by(V1, Round_Time) %>% summarise(Avg_Value = mean(V4, na.rm=TRUE))
}

# call functions on each dataset
wind = grouping(wind)
oil_temp = grouping(oil_temp)
rpm = grouping(rpm)
gearbox1 = grouping(gearbox1)
gearbox2 = grouping(gearbox2)
gearbox3 = grouping(gearbox3)
ap = grouping(ap)
hp = grouping(hp)
at = grouping(at)

# combine gearbox data together
gearbox <- rbind(gearbox1, gearbox2, gearbox3)

# join fault code data to other data sets
df1 <- fc %>% 
  full_join(wind, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(oil_temp, by=c("Round_Time"="Round_Time", "V1"="V1")) %>% 
  full_join(rpm, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(gearbox, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(ap, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(hp, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(at, by=c("Round_Time"="Round_Time", "V1"="V1"))

# remove unnecessary columns
df1 <- subset(df1, select = -c(X))

# rename remaining columns to something meaningful
df1 <- df1 %>%
  rename("Turbine" = "V1",
         "Datetime" = "V2",
         "Date" = "V3",
         "Fault_Code" = "V4",
         "Status" = "V5",
         "Fault_Description" = "V6",
         "Fault_Type" = "V7",
         "Wind_Speed" = "Avg_Value.x",
         "Oil_Temp" = "Avg_Value.y",
         "Generator_RPM" = "Avg_Value.x.x",
         "Gearbox_Temp" = "Avg_Value.y.y",
         "Active_Power" = "Avg_Value.x.x.x",
         "Hydraulic_Pressure" = "Avg_Value.y.y.y",
         "Ambient_Temp" = "Avg_Value")

# select unique round_times
df1 <- distinct(df1, Round_Time, .keep_all = T)


## UPDATE Is_Fault -----
# corrections to Is_Fault, Fault_Type, and Fault_Description to deal with missing values
# assume no fault code occurred
clean_data$Fault_Type[is.na(clean_data$Fault_Type)] = "No Fault"
clean_data$Fault_Description[is.na(clean_data$Fault_Description)] = "No Fault"

# rename blank strings to "no fault"
clean_data$Fault_Type <- ifelse(clean_data$Fault_Type == "", "No Fault", clean_data$Fault_Type)
clean_data$Fault_Description <- ifelse(clean_data$Fault_Description == "", "No Fault", clean_data$Fault_Description)



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
                                  ,"Grease Level Low, Gen Bearings"
                                  ,"Grease Level Low, Hub"
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




## UPSERT DATA ----
# change Round_Time to datetime format in plot_format
plot_data$Round_Time <- ymd_hms(plot_data$Round_Time)

# use full_join function in dplyr package
plot_data <- plot_data %>%
  full_join(df1, by=c("Turbine"="Turbine", "Round_Time"="Round_Time"))
  
  
  
  






