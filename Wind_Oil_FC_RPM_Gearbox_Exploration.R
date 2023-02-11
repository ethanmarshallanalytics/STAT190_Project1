rm(list = ls())
library(dplyr)
library(tidyverse)

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

# filter data to only include Turbine 7
wind_7 <- wind %>% filter(V1 == "Turbine 7")
oil_temp_7 <- oil_temp %>% filter(V1 == "Turbine 7")
fc_7 <- fc %>% filter(V1 == "Turbine 7")
rpm_7 <- rpm %>% filter(V1 == "Turbine 7")
wo_7 <- wo %>% filter(location_id == "Turbine 7")
g1_7 <- g1 %>% filter(V1 == "Turbine 7")
g2_7 <- g2 %>% filter(V1 == "Turbine 7")

# combine gearbox data together  
gearbox_7 <- rbind(g1_7, g2_7)

# histogram of dates for wind data (smallest dataset)
ggplot(data = wind_7) + 
  geom_bar(aes(x=V3)) + coord_flip()
# 2.5 years of data, all through summer and winter. Appear to be well distributed

# join fault code data to other data sets
df1 <- fc_7 %>% 
    left_join(oil_temp_7, by=c("V2"="V2")) %>% 
    left_join(rpm_7, by=c("V2"="V2")) %>%
    left_join(wind_7, by=c("V2"="V2")) %>%
    left_join(gearbox_7, by=c("V2"="V2"))

df1 = subset(df1, select = -c(X.x, X.y, V1.y, V3.y, X.x.x, V1.x.x, V3.x.x, 
                              X.y.y, V1.y.y, V3.y.y, X, V1, V3))

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
           "Gearbox_Number" = "V4", # ASK ABOUT THIS ... WHAT DOES THIS NUMBER MEAN?
           "Gearbox_Type" = "V5")

# filter out null rows for oil_temp, rpm, and gearbox
full_df1 <- df1 %>% filter(!is.na(Oil_Temp) & !is.na(Generator_RPM) & !is.na(Gearbox_Number))
