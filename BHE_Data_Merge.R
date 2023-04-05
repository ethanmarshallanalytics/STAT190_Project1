## LIBRARIES AND PACKAGES -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
# install.packages("naniar")
library(naniar)

## Read in SENSOR DATA and OLD DATA------
# fault data
fc <- read.csv("Project1Data/fault_codes_p2.csv")

# oil_temp
oil_temp <- read.csv("Project1Data/gearbox_oil_temp_p2.csv")

# generator_rpm
rpm <- read.csv("Project1Data/generator_rpm_p2.csv")

# wind_speed
wind <- read.csv("Project1Data/windspeed_p2.csv")

# gearbox_temp
gearbox1 <- read.csv("Project1Data/gearbox_1_p2.csv")
gearbox2 <- read.csv("Project1Data/gearbox_2_p2.csv")
gearbox3 <- read.csv("Project1Data/gearbox_3_p2.csv")

# active_power
ap <- read.csv("Project1Data/active_power_p2.csv")

# ambient_temp
at <- read.csv("Project1Data/ambient_temp_p2.csv")

# hydraulic_pressure
hp <- read.csv("Project1Data/hydraulic_pressure_p2.csv")

# original data (plotting data)
plot_data <- read.csv("Project1Data/plotting_data.csv")


## AGGREGATE SENSOR DATA -----
## UPSERT DATA ----
# use full_join function in dplyr package
## UPDATE Wind_Speed_Group, delta_temp, and Is_Fault -----
