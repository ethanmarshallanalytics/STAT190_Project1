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
    left_join(oil_temp_7, by=c('V2'='V2')) %>% 
    left_join(rpm_7, by=c("V2"="V2")) %>%
    left_join(wind_7, by=c("V2"="V2")) %>%
    left_join(gearbox_7, by=c("V2"="V2"))

# filter out null rows for oil_temp, rpm, and gearbox
full_df1 <- df1 %>% filter(!is.na(V1.x.x) & !is.na(V1.y) & !is.na(V1))
