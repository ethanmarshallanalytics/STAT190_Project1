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
# MAYBE EXPAND ON THIS FOR FURTHER ANALYSIS
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

## 6) Windspeed and Active Power
ggplot(data=data_7) +
  geom_point(aes(x=Wind_Speed, y = Active_Power, color = Is_Fault)) +
  geom_jitter(aes(x=Wind_Speed, y = Active_Power, color = Is_Fault), alpha=I(0.5)) +
  labs(x = "Wind Speed (m/s)", y = "Active Power (kW)", color = "Fault Status") +
  ggtitle("Wind Speed vs Active Power") +
  theme_bw()

## SCATTER PLOT NOTES
# scatterplots - zoom in on the main trends in the scatterplots. Sometimes the default x/y limits aren’t ideal 
# scatterplots - find ways to deal with over plotting (alpha blending, faceting, heatmaps)
# scatterplots - independent variable (e.g., windspeed) should be on x axis while dependent variable (e.g.,power output) should be on y axis
# plots - clean them up, make them professional: 172 style. Daniel has noticed both the good and the bad.
# scatterplots - color by fault type, not just existence of a faul
# PLOTS PLOTS PLOTS, MULTIDIMENSIONAL ONES (scatterplots, but also incorporate color,facets) and then models based on that
# Do a decent scatterplot matrix, please: https://plotly.com/ggplot2/splom/


## GLM ------
# Build a model to predict the probability of a fault code occurring
# Build a model to predict the probability of a work order


## EDGE CASE ANALYSIS ----
# Determine what causes fault codes for outlier data points
