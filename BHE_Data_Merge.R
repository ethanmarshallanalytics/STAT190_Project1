## LIBRARIES AND PACKAGES -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
#install.packages("naniar")
library(naniar)

## Read in SENSOR DATA and OLD DATA------
# fault data
fc <- read.csv("Project1Data/faults_p2.csv")

# oil_temp
oil_temp <- read.csv("Project1Data/oil_temp_p2.csv")

# generator_rpm
rpm <- read.csv("Project1Data/generatorRPM_p2.csv")

# wind_speed
wind <- read.csv("Project1Data/windspeed-p2.csv")

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
fc$Round_Time <- round_date(fc$V2, "10 minute") # round to every 10 minutes
fc <- subset(fc, select = -c(X)) # remove unnecessary columns
fc <- fc %>% distinct(V1, Round_Time, .keep_all = TRUE) # select only distinct fault codes

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
gearbox <- gearbox %>% group_by(V1, Round_Time) %>% summarise(Avg_Value = mean(Avg_Value, na.rm=TRUE))

# join fault code data to other data sets
df1 <- fc %>% 
  full_join(wind, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(oil_temp, by=c("Round_Time"="Round_Time", "V1"="V1")) %>% 
  full_join(rpm, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(gearbox, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(ap, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(hp, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(at, by=c("Round_Time"="Round_Time", "V1"="V1"))

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

## Determine Is_Fault -----
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
# remove engineered columns from plot_data (Wind_Speed_Group, delta_temp)
plot_data <- subset(plot_data, select = -c(Wind_Speed_Group, delta_temp)) 

# change datatypes of each dataset to match
plot_data$Datetime <- ymd_hms(plot_data$Datetime)
plot_data$Date <- ymd(plot_data$Date)
df1$Status <- as.character(df1$Status)
plot_data$Round_Time <- ymd_hms(plot_data$Round_Time)
df1$Is_Fault <- as.integer(df1$Is_Fault)

# merge the datasets
plot_data <- unique(rbind(plot_data, df1), by=c("Turbine", "Round_Time"), fromLast=TRUE)

# write to a new data file
write.csv(plot_data, "Project1Data/plotting_data.csv", row.names=F)

## CORRECTION**: SPLIT NEW DATA INTO SEPERATE DATA SET ----
# Read in clean data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")

# create copy of dataset
plot_data_1 = clean_data

# Create function to find the mode of the data
getmode <- function(data) {
  unique_data <- unique(data)
  unique_data[which.max(tabulate(match(data, unique_data)))]
}

# replace imputed values with null
plot_data_1 = plot_data_1 %>%
  replace_with_na(replace = list(Oil_Temp = getmode(plot_data_1$Oil_Temp),
                                 Generator_RPM = getmode(plot_data_1$Generator_RPM),
                                 Wind_Speed = getmode(plot_data_1$Wind_Speed),
                                 Gearbox_Temp = getmode(plot_data_1$Gearbox_Temp),
                                 Active_Power = getmode(plot_data_1$Active_Power),
                                 Ambient_Temp = getmode(plot_data_1$Ambient_Temp),
                                 delta_temp = getmode(plot_data_1$delta_temp),
                                 Hydraulic_Pressure = getmode(plot_data_1$Hydraulic_Pressure)))

# write new CSV to a file
write.csv(plot_data_1, "Project1Data/plotting_data_1.csv", row.names=F)

# Read in merged plotting data
plot_data = read.csv("Project1Data/plotting_data.csv")

# subset to only turbines in new data
plot_data <- subset(plot_data, plot_data$Turbine %in% c("Turbine 21",
                                                        "Turbine 22",
                                                        "Turbine 23",
                                                        "Turbine 24",
                                                        "Turbine 25",
                                                        "Turbine 26",
                                                        "Turbine 27",
                                                        "Turbine 28",
                                                        "Turbine 29",
                                                        "Turbine 30",
                                                        "Turbine 31",
                                                        "Turbine 32",
                                                        "Turbine 33",
                                                        "Turbine 34",
                                                        "Turbine 35",
                                                        "Turbine 36",
                                                        "Turbine 37"))

# remove Gearbox_Temp column
plot_data <- subset(plot_data, select = -c(Gearbox_Temp))

# read in Gearbox HS data
gearboxHS <- read.csv("Project1Data/gearbox_1_p2.csv")

# read in Gearbox IMS data
gearboxIMS1 <- read.csv("Project1Data/gearbox_2_p2.csv")
gearboxIMS2 <- read.csv("Project1Data/gearbox_3_p2.csv")

# function to change data types, group on 10 minute intervals, and find average value
grouping <- function(data){
  data$V2 <- ymd_hms(data$V2)
  data$Round_Time <- round_date(data$V2, "10 minute")
  data <- data %>% group_by(V1, Round_Time) %>% summarise(Avg_Value = mean(V4, na.rm=TRUE))
}

# call function on gearbox data
gearboxHS = grouping(gearboxHS)
gearboxIMS1 = grouping(gearboxIMS1)
gearboxIMS2 = grouping(gearboxIMS2)

# combine gearbox data together
gearboxIMS <- rbind(gearboxIMS1, gearboxIMS2)
gearboxIMS <- gearboxIMS %>% group_by(V1, Round_Time) %>% summarise(Avg_Value = mean(Avg_Value, na.rm=TRUE))

# join fault code data to other data sets
plot_data$Round_Time <- ymd_hms(plot_data$Round_Time) # change from chr to timestamp

plot_data_a <- plot_data %>%
  full_join(gearboxHS, by=c("Round_Time"="Round_Time", "Turbine"="V1")) %>%
  full_join(gearboxIMS, by=c("Round_Time"="Round_Time", "Turbine"="V1"))

# rename columns to something meaningful
plot_data_a <- plot_data_a %>%
  rename("Gearbox_Temp_HS" = "Avg_Value.x",
         "Gearbox_Temp_IMS" = "Avg_Value.y")

# write new CSV to a file
write.csv(plot_data_a, "Project1Data/plotting_data_2.csv", row.names=F)



