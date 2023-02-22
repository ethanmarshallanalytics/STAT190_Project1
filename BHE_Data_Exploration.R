rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
library(ggplot2)

## DATA PREP -------
# Read in Clean Data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")
# slight correction in Is_Fault column to deal with missing values
clean_data$Is_Fault[is.na(clean_data$Is_Fault)] = 0

# Read in work order data
wo = read.csv("Project1Data/work order scrubbed.csv")

# filter data to only include Turbine 7
data_7 <- clean_data %>% filter(Turbine == "Turbine 7")
data_7_faults <- clean_data %>% filter(Turbine == "Turbine 7" & Is_Fault == 1)
data_7_no_faults <- clean_data %>% filter(Turbine == "Turbine 7" & Is_Fault == 0)

# filter data to only include Turbine 10
data_12 <- clean_data %>% filter(Turbine == "Turbine 12")

# look at data tables
View(clean_data)
View(wo)

#----- Analyzing the lead up to 06/05/2020 Work Order: Converter Controller - Repaired
head(wo_7,1)
df1$dates = as.Date(df1$Date) #Change from Character to Date
# New Data frame looking at Fault Codes from 05/22/2020 - 06/05/2020
df2 = subset(df1, Date < as.Date("2020-06-05") & Date > as.Date("2020-05-22"))
table(df2$Fault_Code)

ggplot(data = df2) +
  geom_bar(aes(x=fct_infreq(Fault_Description))) + xlab("Fault Description") + ylab("Frequency") +
  labs(x = "Fault_Description")+
  coord_flip()

## MULTIVARIATE PLOTS ------------
#### TURBINE 7
## Multivariate plot looking at Wind Speed and Generator RPM
ggplot(data = data_7) +
  geom_point(aes(x=Wind_Speed, y = Generator_RPM, color = Is_Fault)) +
  # geom_text(aes(x=Wind_Speed, y=Generator_RPM, label=ifelse(Wind_Speed>26, paste0(Fault_Code),""))) +
  geom_jitter(aes(x=Wind_Speed, y = Generator_RPM, color=Is_Fault), alpha = I(0.7)) +
  labs(x = "Wind Speed", y = "Generator RPM", color = "Fault Status") +
  scale_y_continuous(limits=c(0,1500)) + scale_x_continuous(limits=c(20,28))

ggplot(data = data_7_faults) +
  geom_point(aes(x=Wind_Speed, y = Generator_RPM)) +
  # geom_text(aes(x=Wind_Speed, y=Generator_RPM, label=ifelse(Wind_Speed>26, paste0(Fault_Code),""))) +
  geom_jitter(aes(x=Wind_Speed, y = Generator_RPM), alpha = I(0.7)) +
  labs(x = "Wind Speed", y = "Generator RPM") +
  scale_y_continuous(limits=c(0,1500)) + scale_x_continuous(limits=c(20,28))

ggplot(data = data_7_no_faults) +
  geom_point(aes(x=Wind_Speed, y = Generator_RPM)) +
  # geom_text(aes(x=Wind_Speed, y=Generator_RPM, label=ifelse(Wind_Speed>26, paste0(Fault_Code),""))) +
  geom_jitter(aes(x=Wind_Speed, y = Generator_RPM), alpha = I(0.7)) +
  labs(x = "Wind Speed", y = "Generator RPM") +
  scale_y_continuous(limits=c(0,1500)) + scale_x_continuous(limits=c(20,28))

## Multivariate plot looking at Wind Speed and Oil Temp
ggplot(data = data_7) +
  geom_point(aes(x=Wind_Speed, y = Oil_Temp, color = Is_Fault)) +
  labs(x = "Wind Speed", y = "Oil Temp", color = "Fault Status") +
  scale_y_continuous(limits=c(20,75)) + scale_x_continuous(limits=c(20,30))

ggplot(data = data_7_faults) +
  geom_point(aes(x=Wind_Speed, y = Oil_Temp)) +
  labs(x = "Wind Speed", y = "Oil Temp") +
  scale_y_continuous(limits=c(20,75)) + scale_x_continuous(limits=c(20,30))

ggplot(data = data_7_no_faults) +
  geom_point(aes(x=Wind_Speed, y = Oil_Temp)) +
  labs(x = "Wind Speed", y = "Oil Temp") +
  scale_y_continuous(limits=c(20,75)) + scale_x_continuous(limits=c(20,30))

## Multivariate plot looking at Gearbox Temp and Active Power
ggplot(data = data_7) +
  geom_point(aes(x=Gearbox_Temp, y = Active_Power, color = Is_Fault)) +
  labs(x = "Gearbox Temp", y = "Active Power", color = "Fault Status") +
  scale_y_continuous(limits=c(0, 3000)) + scale_x_continuous(limits=c(20, 100))

ggplot(data = data_7_faults) +
  geom_point(aes(x=Gearbox_Temp, y = Active_Power)) +
  labs(x = "Gearbox Temp", y = "Active Power") +
  scale_y_continuous(limits=c(0, 3000)) + scale_x_continuous(limits=c(20, 100))

ggplot(data = data_7_no_faults) +
  geom_point(aes(x=Gearbox_Temp, y = Active_Power)) +
  labs(x = "Gearbox Temp", y = "Active Power") +
  scale_y_continuous(limits=c(0, 3000)) + scale_x_continuous(limits=c(20, 100))

## Multivariate plot looking at Wind Speed and Active Power
ggplot(data = data_7) +
  geom_point(aes(x=Wind_Speed, y = Active_Power, color = Is_Fault)) +
  labs(x = "Wind Speed", y = "Active Power", color = "Fault Status") +
  scale_y_continuous(limits=c(0,3000)) + scale_x_continuous(limits=c(20,30))

ggplot(data = data_7_faults) +
  geom_point(aes(x=Wind_Speed, y = Active_Power)) +
  labs(x = "Wind Speed", y = "Active Power") +
  scale_y_continuous(limits=c(0,3000)) + scale_x_continuous(limits=c(20,30))

ggplot(data = data_7_no_faults) +
  geom_point(aes(x=Wind_Speed, y = Active_Power)) +
  labs(x = "Wind Speed", y = "Active Power") +
  scale_y_continuous(limits=c(0,3000)) + scale_x_continuous(limits=c(20,30))



###### TURBINE 12
