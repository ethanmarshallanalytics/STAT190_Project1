## LIBRARIES AND PACKAGES -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
# install.packages("naniar")
library(naniar)

### ORIGINAL DATA -------
# read in windspeed.csv
wind <- read.csv("Project1Data/windspeed.csv")
# read in gearbox_oil_temp.csv
oil_temp <- read.csv("Project1Data/gearbox_oil_temp.csv")
# read in fault_codes.csv
fc <- read.csv("Project1Data/fault_codes.csv")
# read in generator_rpm.csv
rpm <- read.csv("Project1Data/generator_rpm.csv")
# read in work order scrubbed.csv
wo <- read.csv("Project1Data/work order scrubbed.csv")
# read in gearbox 1 and 2 data
g1 <- read.csv("Project1Data/gearbox_1.csv")
g2 <- read.csv("Project1Data/gearbox_2.csv")
g3 <- read.csv("Project1Data/gearbox_3.csv")
# read in active power data
ap <- read.csv("Project1Data/active_power.csv")

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
g1 = grouping(g1)
g2 = grouping(g2)
g3 = grouping(g3)
ap = grouping(ap)

# combine gearbox data together
gearbox <- rbind(g1, g2, g3)

# join fault code data to other data sets
df1 <- fc %>% 
  full_join(oil_temp, by=c("Round_Time"="Round_Time", "V1"="V1")) %>% 
  full_join(rpm, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(wind, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(gearbox, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(ap, by=c("Round_Time"="Round_Time", "V1"="V1"))

# add column to specify if a fault code occurred
df1$Is_Fault <- ifelse(df1$V6 > "", 1, 0)

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
         "Oil_Temp" = "Avg_Value.x",
         "Generator_RPM" = "Avg_Value.y",
         "Wind_Speed" = "Avg_Value.x.x",
         "Gearbox_Temp" = "Avg_Value.y.y",
         "Active_Power" = "Avg_Value")

# replace missing values with mean
df1$Oil_Temp[is.na(df1$Oil_Temp)] <- mean(df1$Oil_Temp, na.rm=TRUE)
df1$Generator_RPM[is.na(df1$Generator_RPM)] <- mean(df1$Generator_RPM, na.rm=TRUE)
df1$Wind_Speed[is.na(df1$Wind_Speed)] <- mean(df1$Wind_Speed, na.rm=TRUE)
df1$Gearbox_Temp[is.na(df1$Gearbox_Temp)] <- mean(df1$Gearbox_Temp, na.rm=TRUE)
df1$Active_Power[is.na(df1$Active_Power)] <- mean(df1$Active_Power, na.rm=TRUE)

# write aggregated file to CSV
write.csv(df1, "Project1Data/clean_BHE_data.csv")

### NEW DATA ... AMBIENT TEMPERATURE --------
# Read in Clean Data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")
# Read in ambient temperature
at <- read.csv("Project1Data/ambient_temp.csv")
# function to change data types, group on 10 minute intervals, and find average value
grouping <- function(data){
  data$V2 <- ymd_hms(data$V2)
  data$Round_Time <- round_date(data$V2, "10 minute")
  data <- data %>% group_by(V1, Round_Time) %>% summarise(Avg_Value = mean(V4, na.rm=TRUE))
}
# call grouping function on at
at = grouping(at)

# change round time in clean_data to timestamp
clean_data$Round_Time <- ymd_hms(clean_data$Round_Time) # change from chr to timestamp

# join clean data to ambient temperature
clean_data <- clean_data %>% 
  full_join(at, by=c("Round_Time"="Round_Time", "Turbine"="V1"))

# rename new column to something meaningful
clean_data <- clean_data %>% rename("Ambient_Temp" = "Avg_Value")
  
# replace missing values with mean
clean_data$Ambient_Temp[is.na(clean_data$Ambient_Temp)] <- mean(clean_data$Ambient_Temp, na.rm=TRUE)

# slight correction in Is_Fault column to deal with missing values
# assume no fault code occurred
clean_data$Is_Fault[is.na(clean_data$Is_Fault)] = 0

# write aggregated file to CSV
write.csv(clean_data, "Project1Data/clean_BHE_data.csv")

### NEW DATA ... HYDRAULIC PRESSURE ------
# Read in Clean Data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")
# Read in ambient temperature
hp <- read.csv("Project1Data/hydraulic_pressure.csv")
# function to change data types, group on 10 minute intervals, and find average value
grouping <- function(data){
  data$V2 <- ymd_hms(data$V2)
  data$Round_Time <- round_date(data$V2, "10 minute")
  data <- data %>% group_by(V1, Round_Time) %>% summarise(Avg_Value = mean(V4, na.rm=TRUE))
}
# call grouping function on at
hp = grouping(hp)

# change round time in clean_data to timestamp
clean_data$Round_Time <- ymd_hms(clean_data$Round_Time) # change from chr to timestamp

# join clean data to ambient temperature
clean_data <- clean_data %>% 
  full_join(hp, by=c("Round_Time"="Round_Time", "Turbine"="V1"))

# rename new column to something meaningful
clean_data <- clean_data %>% rename("Hydraulic_Pressure" = "Avg_Value")

# replace missing values with mean
clean_data$Hydraulic_Pressure[is.na(clean_data$Hydraulic_Pressure)] <- mean(clean_data$Hydraulic_Pressure, na.rm=TRUE)

# slight correction in Is_Fault column to deal with missing values
# assume no fault code occurred
clean_data$Is_Fault[is.na(clean_data$Is_Fault)] = 0

# remove extra columns from join
clean_data <- subset(clean_data, select = -c(X, X.1, X.2, X.3, X.4))

# write aggregated file to CSV
write.csv(clean_data, "Project1Data/clean_BHE_data.csv")


### NUll/BLANK CORRECTIONS & DATA ENGINEERING ------
# read in clean_data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")

# corrections to Is_Fault, Fault_Type, and Fault_Description to deal with missing values
# assume no fault code occurred
clean_data$Is_Fault[is.na(clean_data$Is_Fault)] = 0
clean_data$Fault_Type[is.na(clean_data$Fault_Type)] = "No Fault"
clean_data$Fault_Description[is.na(clean_data$Fault_Description)] = "No Fault"

# rename blank strings to "no fault"
clean_data$Fault_Type <- ifelse(clean_data$Fault_Type == "", "No Fault", clean_data$Fault_Type)
clean_data$Fault_Description <- ifelse(clean_data$Fault_Description == "", "No Fault", clean_data$Fault_Description)

# Creates the new variable Delta Temp (Ambient Temperature - Gearbox Temperature)
clean_data$delta_temp = abs(clean_data$Ambient_Temp - clean_data$Gearbox_Temp)

# write aggregated file to CSV
write.csv(clean_data, "Project1Data/clean_BHE_data.csv")

### IS_FAULT UPDATES ------
# read in clean_data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")

# replace missing values with mean
clean_data$Oil_Temp[is.na(clean_data$Oil_Temp)] <- mean(clean_data$Oil_Temp, na.rm=TRUE)
clean_data$Generator_RPM[is.na(clean_data$Generator_RPM)] <- mean(clean_data$Generator_RPM, na.rm=TRUE)
clean_data$Wind_Speed[is.na(clean_data$Wind_Speed)] <- mean(clean_data$Wind_Speed, na.rm=TRUE)
clean_data$Gearbox_Temp[is.na(clean_data$Gearbox_Temp)] <- mean(clean_data$Gearbox_Temp, na.rm=TRUE)
clean_data$Active_Power[is.na(clean_data$Active_Power)] <- mean(clean_data$Active_Power, na.rm=TRUE)
clean_data$Ambient_Temp[is.na(clean_data$Ambient_Temp)] <- mean(clean_data$Ambient_Temp, na.rm=TRUE)

# Delta Temp (Ambient Temperature - Gearbox Temperature) to include imputed value
clean_data$delta_temp = abs(clean_data$Ambient_Temp - clean_data$Gearbox_Temp)

# Categorize Wind_Speed into 3 sections
clean_data$Wind_Speed_Group <- cut(clean_data$Wind_Speed,
                                   breaks = c(0, 20, 30, 50),
                                   include.lowest = T,
                                   right=F)

# Convert new Wind_Speed category to factor type and rename groups
clean_data$Wind_Speed_Group <- factor(clean_data$Wind_Speed_Group,
                                      levels = c("[0,20)", "[20,30)", "[30,50]"),
                                      labels = c("Low", "Medium", "High"))


# remove Is_Fault column to start fresh
clean_data <- subset(clean_data, select = -c(Is_Fault))

# change Is_Fault column to only include the correct faults
# Faults listed are NOT important ... Is_Fault = 0
clean_data$Is_Fault <- ifelse(clean_data$Fault_Description %in% 
                                  c("Ice Detection: Low Torque"
                                    ,"No Fault"
                                    ,"Remote Stop - Oem"
                                    ,"Remote Stop - Owner"
                                    ,"Stop For Powerdown"
                                    ,"Stopped Due To Power Up Delay"
                                    ,"Stopped For Sw Update"
                                    ,"Stopped, Untwisting Cables"
                                    ,"Manual Stop"
                                    ,"Manual Idle Stop"
                                    ,"Manual Idle Stop - Yawing"
                                    ,"Mcb Cleaning Ended"
                                    ,"Mcb Cleaning In Progresss"
                                    ,"No Valid Wind Data"
                                    ,"Lmu Sensor Error"
                                    ,"Yaw Converter Error"
                                    ,"Too Many Slip Ring Errors"
                                    ,"Too Many Yaw Conv. Errors"
                                    ,"Rpm Sensor Error"
                                    ,"No Valid Wind Data"
                                    ,"Inline (Bef) Pressure Sensor Error"
                                    ,"Inline (Aft) Pressure Sensor Error"
                                    ,"Hub: Blade A Valve Error"
                                    ,"Hub: Blade B Valve Error"
                                    ,"Hub: Blade C Valve Error"
                                    ,"Hyd Oil Level Error"
                                    ,"Grid Filter Res Temp Error"
                                    ,"Grd. Inv. Communication Error"
                                    ,"Ft1 Sonic Wind Sensor Error"
                                    ,"Ambient Temp Sensor Error"
                                    ,"Backup Battery Error"
                                    ,"Brake (Gen) Temperature Error"
                                    ,"Can 3 Buffer Overrorun Error"
                                    ,"Can:Hub Communication Error"
                                    ,"Can:Gs-1 Communication Error"
                                    ,"Can:Gm  Communication Error"
                                    ,"Can:Hub Module Init. Error"
                                    ,"Can:Io-1 Communication Error"
                                    ,"Can:Io-2 Communication Error"
                                    ,"Can:Io-3 Communication Error"
                                    ,"Can:Io-3 Module Init. Error"
                                    ,"Can:Io-4 Communication Error"
                                    ,"Can:Io-4 Module Init. Error"
                                    ,"Can:Io-7 Communication Error"
                                    ,"Offline Filter Stopped"
                                    ,"Pitch A Tracking During Stop"
                                    ,"Pitch B Tracking During Stop"
                                    ,"Pitch C Tracking During Stop"
                                    ,"Pitch Pump Time Too Long,Stop|"
                                    ,"Pitch Pawl A Feedb. Stop"
                                    ,"Timeout  Dc-Circuit Charging"
                                    ,"Low Torque, No Ice (Temp=Norm)"
                                    ,"Local, Ad-Hoc / Repair Work"
                                    ,"Local, Customer / Guest Visit"
                                    ,"Local, Scheduled Service Work"), 0, 1)


# write to a fresh CSV file
write.csv(clean_data, "Project1Data/clean_BHE_data.csv", row.names=F)

## REMOVE IMPUTED VALUES IN NEW DATA SET -----
clean_data = read.csv("Project1Data/clean_BHE_data.csv")

# create copy of dataset
plotting_data = clean_data

# replace imputed values with null
plotting_data = plotting_data %>%
  replace_with_na(replace = list(Oil_Temp = median(plotting_data$Oil_Temp),
                                 Generator_RPM = median(plotting_data$Generator_RPM),
                                 Wind_Speed = median(plotting_data$Wind_Speed),
                                 Gearbox_Temp = median(plotting_data$Gearbox_Temp),
                                 Active_Power = median(plotting_data$Active_Power),
                                 Ambient_Temp = median(plotting_data$Ambient_Temp),
                                 delta_temp = median(plotting_data$delta_temp)))

# write new CSV to a file
write.csv(plotting_data, "Project1Data/plotting_data.csv", row.names=F)

