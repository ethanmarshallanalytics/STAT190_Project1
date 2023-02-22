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

# filter data to only include Turbine 12
data_12 <- clean_data %>% filter(Turbine == "Turbine 12")

# look at data tables
View(clean_data)
View(wo)

## MULTIVARIATE PLOTS ------------
## HIGHEST CORRELATIONS FOR TURBINE 7
## 1) Active Power & Generator RPM 0.656
## 2) Active Power & Gearbox Temp 0.516
## 3) Generator RPM & Oil Temp 0.449
## 4) Active Power & Oil Temp 0.412


#### TURBINE 7

## 1) Active Power & Generator RPM 0.656
ggplot(data=data_7) +
  geom_point(aes(x=Generator_RPM, y = Active_Power, color = Is_Fault)) +
  geom_jitter(aes(x=Generator_RPM, y = Active_Power, color = Is_Fault), alpha=I(0.5)) +
  labs(x = "Generator RPM", y = "Active Power", color = "Fault Status") +
  ggtitle("Active Power vs. Generator RPM") +
  theme_bw()


## (2) Multivariate plot looking at Gearbox Temp and Active Power
ggplot(data = data_7) +
  geom_point(aes(x=Gearbox_Temp, y = Active_Power, color = Is_Fault)) +
  geom_jitter(aes(x=Gearbox_Temp, y = Active_Power, color = Is_Fault), alpha=I(0.5)) +
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

## 3) Generator RPM & Oil Temp 0.449


## 4) Active Power & Oil Temp 0.412

