rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)

## DATA PREP -------
# Read in Clean Data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")

# filter data to only include Turbine 7
df1_7 <- df1 %>% filter(Turbine == "Turbine 7")

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

### MULTIVARIATE PLOTS
ggplot(data = df1_7) +
  geom_point(aes(x=Wind_Speed, y=Generator_RPM)) +
  geom_text(aes(x=Wind_Speed, y=Generator_RPM, label=ifelse(Wind_Speed>26, paste0(Fault_Description),"")))


