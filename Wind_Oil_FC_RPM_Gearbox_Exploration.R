rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)

## DATA PREP -------
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

# filter data to only include Turbine 7
wind_7 <- wind %>% filter(V1 == "Turbine 7")
oil_temp_7 <- oil_temp %>% filter(V1 == "Turbine 7")
rpm_7 <- rpm %>% filter(V1 == "Turbine 7")
g1_7 <- g1 %>% filter(V1 == "Turbine 7")
g2_7 <- g2 %>% filter(V1 == "Turbine 7")
ap_7 <- ap %>% filter(V1 == "Turbine 7")
fc_7 <- fc %>% filter(V1 == "Turbine 7")
wo_7 <- wo %>% filter(location_id == "Turbine 7" & component_type != "null")


# combine gearbox data together
gearbox_7 <- rbind(g1_7, g2_7)

# join fault code data to other data sets
df1 <- fc_7 %>% 
    left_join(oil_temp_7, by=c("Round_Time"="Round_Time")) %>% 
    left_join(rpm_7, by=c("Round_Time"="Round_Time")) %>%
    left_join(wind_7, by=c("Round_Time"="Round_Time")) %>%
    left_join(gearbox_7, by=c("Round_Time"="Round_Time")) %>%
    left_join(ap_7, by=c("Round_Time"="Round_Time"))

# remove unnecessary columns
df1 = subset(df1, select = -c(X, V1.y, V1.x.x, V1.y.y, V1.x.x.x, V1.y.y.y))

# rename remaining columns to something meaningful
df1 <- df1 %>%
    rename("Turbine" = "V1.x",
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

# TODO: INTERPOLATE MEDIAN/MEAN FOR NULL VALUES

# only return distinct columns
df1 <- distinct(df1)

# look at data tables
View(df1)
View(wo_7)

## EXPLORATORY ANALYSIS -------
# histogram of windspeed data (smallest dataset)
ggplot(data = wind_7) + 
  geom_histogram(aes(x=V4), bins = 60) + xlab("Windspeed") + ylab("Frequency")

# histogram of oil temperatures
ggplot(data = oil_temp_7) +
  geom_bar(aes(x=V4)) + xlab("Oil Temperature") + ylab("Frequency")

# histogram of generator RPM
ggplot(data = rpm_7) +
  geom_histogram(aes(x=V4), bins= 100) + xlab("Generator RPM") + ylab("Frequency")

# histogram of gearbox data
ggplot(data = gearbox_7) +
  geom_histogram(aes(x=V4), bins= 60) + xlab("Gearbox Temp") + ylab("Frequency")

#----- Analyzing the lead up to 06/05/2020 Work Order: Converter Controller - Reapired
head(wo_7,1)
df1$dates = as.Date(df1$Date) #Change from Character to Date
# New Data frame looking at Fault Codes from 05/22/2020 - 06/05/2020
df2 = subset(df1, Date < as.Date("2020-06-05") & Date > as.Date("2020-05-22"))
table(df2$Fault_Code)

ggplot(data = df2) +
  geom_bar(aes(x=fct_infreq(Fault_Description))) + xlab("Fault Description") + ylab("Frequency") +
  labs(x = "Fault_Description")+
  coord_flip()
