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
# read in active power data
ap <- read.csv("Project1Data/active_power.csv")

# filter data to only include Turbine 7
wind_7 <- wind %>% filter(V1 == "Turbine 7")
oil_temp_7 <- oil_temp %>% filter(V1 == "Turbine 7")
fc_7 <- fc %>% filter(V1 == "Turbine 7")
rpm_7 <- rpm %>% filter(V1 == "Turbine 7")
wo_7 <- wo %>% filter(location_id == "Turbine 7" & component_type != "null")
g1_7 <- g1 %>% filter(V1 == "Turbine 7")
g2_7 <- g2 %>% filter(V1 == "Turbine 7")
ap_7 <- ap %>% filter(V1 == "Turbine 7")

# change V2 to datetime
wind_7$V2 <- ymd_hms(wind_7$V2)

# aggregate timestamp to every 10 minute interval
wind_7$V2 <- round_date(wind_7$V2, "10 minute")







# combine gearbox data together
gearbox_7 <- rbind(g1_7, g2_7)

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

# join fault code data to other data sets
df1 <- fc_7 %>% 
    left_join(oil_temp_7, by=c("V2"="V2")) %>% 
    left_join(rpm_7, by=c("V2"="V2")) %>%
    left_join(wind_7, by=c("V2"="V2")) %>%
    left_join(gearbox_7, by=c("V2"="V2"))

# remove unnecessary columns
df1 = subset(df1, select = -c(X.x, X.y, V1.y, V3.y, X.x.x, V1.x.x, V3.x.x, 
                              X.y.y, V1.y.y, V3.y.y, X, V1, V3))

# rename remaining columns to something meaningful
df1 <- df1 %>%
    rename("Turbine" = "V1.x",
           "Datetime" = "V2",
           "Date" = "V3.x",
           "Fault_Code" = "V4.x",
           "Status" = "V5.x",
           "Fault_Description" = "V6",
           "Fault_Type" = "V7",
           "Oil_Temp" = "V4.y", 
           "Oil_Type" = "V5.y",
           "Generator_RPM" = "V4.x.x", 
           "RPM_Type" = "V5.x.x",
           "Wind_Speed" = "V4.y.y",
           "Wind_Type" = "V5.y.y",
           "Gearbox_Temp" = "V4",
           "Gearbox_Type" = "V5")

# only return distinct columns
df1 <- distinct(df1)

# look at data tables
View(df1)
View(wo_7)

# filter out null rows for oil_temp, rpm, and gearbox
## NOTE: MAY NOT WANT TO DO THIS TO KEEP ALL RAW DATA
# df1 <- df1 %>% filter(!is.na(Oil_Temp) & !is.na(Generator_RPM) & !is.na(Gearbox_Number))

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
