## LIBRARIES AND PACKAGES -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
# install.packages("naniar")
library(naniar)
## CREATE MASTER DATA ----
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
# Read in ambient temperature
at <- read.csv("Project1Data/ambient_temp.csv")
# Read in hydraulic pressure
hp <- read.csv("Project1Data/hydraulic_pressure.csv")


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
at = grouping(at)
hp = grouping(hp)

# combine gearbox data together
gearbox <- rbind(g1, g2, g3)

# join fault code data to other data sets
df1 <- fc %>% 
  full_join(oil_temp, by=c("Round_Time"="Round_Time", "V1"="V1")) %>% 
  full_join(rpm, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(wind, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(gearbox, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(ap, by=c("Round_Time"="Round_Time", "V1"="V1")) %>%
  full_join(at, by=c("Round_Time"="Round_Time", "Turbine"="V1")) %>%
  full_join(hp, by=c("Round_Time"="Round_Time", "Turbine"="V1"))

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
         "Active_Power" = "Avg_Value.x.x.x",
         "Ambient_Temp" = "Avg_Value")

# corrections to Fault_Type, and Fault_Description to deal with missing values
clean_data$Fault_Type[is.na(clean_data$Fault_Type)] = "No Fault"
clean_data$Fault_Description[is.na(clean_data$Fault_Description)] = "No Fault"

# rename blank strings to "no fault"
clean_data$Fault_Type <- ifelse(clean_data$Fault_Type == "", "No Fault", clean_data$Fault_Type)
clean_data$Fault_Description <- ifelse(clean_data$Fault_Description == "", "No Fault", clean_data$Fault_Description)

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

# write aggregated file to CSV
write.csv(df1, "Project1Data/master_BHE_data.csv")


## EXAMPLE CODE ----






## TIME SERIES MODEL ----
