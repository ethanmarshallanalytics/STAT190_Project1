rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
library(ggplot2)

## DATA PREP -------
# Read in Clean Data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")

# Read in work order data
wo = read.csv("Project1Data/work order scrubbed.csv")

# filter data to only include Turbine 7
data_7 <- clean_data %>% filter(Turbine == "Turbine 7")
data_7_faults <- clean_data %>% filter(Turbine == "Turbine 7" & Is_Fault == 1)
data_7_no_faults <- clean_data %>% filter(Turbine == "Turbine 7" & Is_Fault == 0)

# filter data to only include Turbine 12
data_12 <- clean_data %>% filter(Turbine == "Turbine 12")

# look at data tables
View(clean_data)
View(wo)

## MULTIVARIATE PLOTS ------------
## Correlation Matrix
Vars = data.frame(data_7$Oil_Temp, data_7$Generator_RPM, data_7$Wind_Speed, data_7$Gearbox_Temp, data_7$Active_Power)
cor(cbind(Vars), use="pairwise.complete.obs")

## HIGHEST CORRELATIONS FOR TURBINE 7
## 1) Active Power & Generator RPM --> 0.656
## 2) Active Power & Gearbox Temp --> 0.516
## 3) Generator RPM & Oil Temp --> 0.449
## 4) Active Power & Oil Temp --> 0.412
## 5) Gearbox Temp & Oil Temp --> 0.409
## 6) Gearbox Temp & Generator RPM --> 0.394

#### TURBINE 7
## 1) Active Power & Generator RPM
ggplot(data=data_7) +
  geom_point(aes(x=Generator_RPM, y = Active_Power, color = Is_Fault)) +
  geom_jitter(aes(x=Generator_RPM, y = Active_Power, color = Is_Fault), alpha=I(0.5)) +
  labs(x = "Generator RPM", y = "Active Power (kW)", color = "Fault Status") +
  ggtitle("Active Power vs. Generator RPM") +
  theme_bw()


## 2) Gearbox Temp and Active Power
ggplot(data = data_7) +
  geom_point(aes(x=Gearbox_Temp, y=Active_Power, color = Is_Fault)) +
  geom_jitter(aes(x=Gearbox_Temp, y=Active_Power, color = Is_Fault), alpha=I(0.5)) +
  labs(x = "Gearbox Temperature (ºC)", y = "Active Power (kw)", color = "Fault Status") +
  ggtitle("Active Power vs. Gearbox Temperature") +
  theme_bw()

## 3) Generator RPM & Oil Temp 
ggplot(data=data_7) +
  geom_point(aes(x=Generator_RPM, y = Oil_Temp, color = Is_Fault)) +
  geom_jitter(aes(x=Generator_RPM, y = Oil_Temp, color = Is_Fault), alpha=I(0.5)) +
  labs(x = "Generator RPM", y = "Oil Temperature (ºC)", color = "Fault Status") +
  ggtitle("Oil Temperature vs. Generator RPM") +
  theme_bw()

## 4) Active Power & Oil Temp
ggplot(data=data_7) +
  geom_point(aes(x=Active_Power, y = Oil_Temp, color = Is_Fault)) +
  geom_jitter(aes(x=Active_Power, y = Oil_Temp, color = Is_Fault), alpha=I(0.5)) +
  labs(x = "Active Power (kW)", y = "Oil Temperature (ºC)", color = "Fault Status") +
  ggtitle("Oil Temperature vs. Active Power") +
  theme_bw()

## 5) Gearbox Temp & Oil Temp
ggplot(data=data_7) +
  geom_point(aes(x=Gearbox_Temp, y = Oil_Temp, color = Is_Fault)) +
  geom_jitter(aes(x=Gearbox_Temp, y = Oil_Temp, color = Is_Fault), alpha=I(0.5)) +
  labs(x = "Gearbox Temperature (ºC)", y = "Oil Temperature (ºC)", color = "Fault Status") +
  ggtitle("Oil Temperature vs. Gearbox Temperature") +
  theme_bw()