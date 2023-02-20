rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)

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
