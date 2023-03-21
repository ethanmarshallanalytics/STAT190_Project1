## LIBRARIES AND PACKAGES -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
library(ggplot2)
# install.packages("GGally")
library(GGally)
# install.packages("plotly")
library(plotly)
library(scales)
#install.packages("fuzzyjoin")
library(fuzzyjoin)
# install.packages("hrbrthemes")
library(hrbrthemes)

## DATA PREP -------
# Read in Clean Data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")

# Changing is_fault from Integer to character
clean_data$Is_Fault <- as.character(clean_data$Is_Fault)

# Read in work order 
wo = read.csv("Project1Data/work order scrubbed.csv")

# wo1 <- wo %>%
#   filter(component_type != "null")
# 
# write.csv(wo1, "Project1Data/wo1.csv")

# Read in filtered work order
wo1 = read.csv("Project1Data/wo1.csv")

# filter data to only include Turbine 7
data_7 <- clean_data %>% filter(Turbine == "Turbine 7")
data_7_faults <- clean_data %>% filter(Turbine == "Turbine 7" & Is_Fault == "1")
data_7_no_faults <- clean_data %>% filter(Turbine == "Turbine 7" & Is_Fault == "0")

# filter data to only include Turbine 12
data_12 <- clean_data %>% filter(Turbine == "Turbine 12")
data_12_faults <- clean_data %>% filter(Turbine == "Turbine 12" & Is_Fault == "1")
data_12_no_faults <- clean_data %>% filter(Turbine == "Turbine 12" & Is_Fault == "0")

# filter data to only include Turbine 14
data_14 <- clean_data %>% filter(Turbine == "Turbine 14")

# look at data tables
View(clean_data)
View(wo)

## UNI- AND MULTI-VARIATE PLOTS ------------
# further subset Turbine 7 data to only include data since Jan 1, 2022
data_7_sub <- filter(data_7, Date > "2022-03-01")

# subset to sensor data columns
Vars = data.frame(data_7_sub$Oil_Temp, 
                  data_7_sub$Generator_RPM,
                  data_7_sub$Gearbox_Temp, 
                  data_7_sub$Active_Power, 
                  data_7_sub$Hydraulic_Pressure,
                  data_7_sub$Is_Fault)

Vars <- Vars %>%
  rename("Oil_Temp" = "data_7_sub.Oil_Temp", 
         "Generator_RPM" = "data_7_sub.Generator_RPM",
         "Gearbox_Temp" = "data_7_sub.Gearbox_Temp", 
         "Active_Power" = "data_7_sub.Active_Power", 
         "Hydraulic_Pressure" = "data_7_sub.Hydraulic_Pressure",
         "Is_Fault" = "data_7_sub.Is_Fault")

# rename columns to something cleaner
# Vars = Vars %>% 
#   rename("Oil_Temp" = "data_7_sub.Oil_Temp",
#          "Generator_RPM" = "data_7_sub.Generator_RPM", 
#          "Wind_Speed" = "data_7_sub.Wind_Speed",
#          "Gearbox_Temp" = "data_7_sub.Gearbox_Temp",
#          "Active_Power" = "data_7_sub.Active_Power",
#          "Ambient_Temp" = "data_7_sub.Ambient_Temp")

## Correlation Matrix
cor(cbind(Vars), use="pairwise.complete.obs")

## Scatter plot matrix
p <- ggpairs(Vars, 
             columns = 1:5,
             aes(color=Is_Fault, alpha=0.2))
ggplotly(p)


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
  geom_jitter(aes(x=Generator_RPM, y = Active_Power, color = Is_Fault), alpha=I(0.3)) +
  geom_text(aes(x=Generator_RPM, y = Active_Power, label = ifelse(Fault_Description == "Geninv: 139 U-Phase Sharing", "139 U-Phase Fault", ""))) +
  scale_fill_manual(values = c("Blue", "Orange")) +
  labs(x = "Generator RPM", y = "Active Power (kW)", color = "Fault Status") +
  ggtitle("Active Power vs. Generator RPM") +
  theme_bw()

ggplot(data=clean_data) +
  geom_point(aes(x=Generator_RPM, y = Active_Power, color = Is_Fault)) +
  geom_jitter(aes(x=Generator_RPM, y = Active_Power, color = Is_Fault), alpha=I(0.3)) +
  labs(x = "Generator RPM", y = "Active Power (kW)", color = "Fault Status") +
  ggtitle("Active Power vs. Generator RPM") +
  facet_wrap(~ Turbine, ncol = 2) +  # add facet by turbine
  theme_bw()

# Active Power & Generator RPM heatmap plot
ggplot(data=data_7, aes(x=Generator_RPM, y=Active_Power, fill=Is_Fault)) +
  # geom_bin2d(bins=100) +
  geom_tile(position="jitter", height=40, width=22, alpha=I(0.3)) +
  scale_fill_manual(values = c("Blue", "Orange")) +
  labs(x = "Generator RPM", y = "Active Power (kW)", fill = "Fault Status") +
  ggtitle("Active Power vs. Generator RPM") +
  theme_bw()

## 2) Gearbox Temp and Active Power
ggplot(data = data_7) +
  geom_point(aes(x=Gearbox_Temp, y=Active_Power, color = Is_Fault)) +
  geom_jitter(aes(x=Gearbox_Temp, y=Active_Power, color = Is_Fault), alpha=I(0.3)) +
  labs(x = "Gearbox Temperature (ºC)", y = "Active Power (kw)", color = "Fault Status") +
  ggtitle("Active Power vs. Gearbox Temperature") +
  theme_bw()

## 3) Generator RPM & Oil Temp 
ggplot(data=data_7) +
  geom_point(aes(x=Generator_RPM, y = Oil_Temp, color = Is_Fault)) +
  geom_jitter(aes(x=Generator_RPM, y = Oil_Temp, color = Is_Fault), alpha=I(0.3)) +
  labs(x = "Generator RPM", y = "Oil Temperature (ºC)", color = "Fault Status") +
  ggtitle("Oil Temperature vs. Generator RPM") +
  theme_bw()

# Generator RPM & Oil Temp heatmap
ggplot(data=data_7, aes(x=Generator_RPM, y = Oil_Temp, fill = Is_Fault)) +
  geom_tile(position="jitter", height=1, width=15, alpha=I(0.3)) +
  scale_fill_manual(values = c("Blue", "Orange")) +
  labs(x = "Generator RPM", y = "Oil Temperature (ºC)", fill = "Fault Status") +
  scale_y_continuous(limits = c(20,80), breaks=c(20,30,40,50,60,70,80)) +
  ggtitle("Oil Temperature vs. Generator RPM") +
  theme_bw()

## 4) Active Power & Oil Temp
ggplot(data=data_7) +
  geom_point(aes(x=Active_Power, y = Oil_Temp, color = Is_Fault)) +
  geom_jitter(aes(x=Active_Power, y = Oil_Temp, color = Is_Fault), alpha=I(0.3)) +
  labs(x = "Active Power (kW)", y = "Oil Temperature (ºC)", color = "Fault Status") +
  ggtitle("Oil Temperature vs. Active Power") +
  theme_bw()

## 5) Gearbox Temp & Oil Temp
ggplot(data=data_7) +
  geom_point(aes(x=Gearbox_Temp, y = Oil_Temp, color = Is_Fault)) +
  geom_jitter(aes(x=Gearbox_Temp, y = Oil_Temp, color = Is_Fault), alpha=I(0.3)) +
  labs(x = "Gearbox Temperature (ºC)", y = "Oil Temperature (ºC)", color = "Fault Status") +
  ggtitle("Oil Temperature vs. Gearbox Temperature") +
  theme_bw()

## 6) Windspeed and Active Power
ggplot(data=data_7) +
  geom_point(aes(x=Wind_Speed, y = Active_Power, color = Is_Fault)) +
  geom_jitter(aes(x=Wind_Speed, y = Active_Power, color = Is_Fault), alpha=I(0.3)) +
  labs(x = "Wind Speed (m/s)", y = "Active Power (kW)", color = "Fault Status") +
  ggtitle("Wind Speed vs Active Power") +
  theme_bw()

# Windspeed and Active Power heatmap
ggplot(data=data_7, aes(x=Wind_Speed, y = Active_Power, fill = Is_Fault)) +
  geom_tile(position="jitter", height=40, width=0.2, alpha=I(0.3)) +
  scale_fill_manual(values = c("Blue", "Orange")) +
  labs(x = "Wind Speed (m/s)", y = "Active Power (kW)", fill = "Fault Status") +
  scale_x_continuous(limits = c(20,30), breaks=c(20,22,24,26,28,30)) +
  ggtitle("Active Power vs. Wind Speed") +
  theme_bw()

## 7) Delta Temp and Active Power
# Delta Temp = ABS(Ambient_Temp - Gearbox_Temp)
ggplot(data=data_7) +
  geom_point(aes(x=delta_temp, y = Active_Power, color = Fault_Type)) +
  geom_jitter(aes(x=delta_temp, y = Active_Power, color = Fault_Type), alpha=I(0.3)) +
  labs(x = "Delta Temperature (Ambient & Gearbox) (ºC)", y = "Active Power (kW)", color = "Fault Type") +
  ggtitle("Delta Temp vs Active Power") +
  theme_bw()

## 8) Hydraulic Pressure & Active Power
subset_fault_1 <- subset(clean_data, Is_Fault == "1")

ggplot(data=data_7, aes(x=Hydraulic_Pressure, y = Active_Power, color = Is_Fault)) +
  geom_point() +
  geom_jitter(alpha=I(0.1)) +
  labs(x = "Hydraulic Pressure (bar)", y = "Active Power (kW)", color = "Fault Status") +
  ggtitle("Hydraulic Pressure vs Active Power") +
  theme_bw()

#### AGGREGATED DATA AND SUMMARY ANALYSIS -----
# count of Fault occurrances aggregated by turbine
agg_fault <- clean_data %>%
  filter(Is_Fault == "1") %>%
  group_by(Turbine) %>%
  summarise(sensor_count = n()) %>%
  as.data.frame()
# Turbine 7, 12, 14, 13, 15 have the most fault occurrences

# Function for summarizing fault codes
fault_summary <- function(turbine) {
  faults <- clean_data %>% filter(Turbine == turbine & Is_Fault == "1")
  faults %>%
    group_by(Fault_Code, Fault_Description) %>%
    summarise(total_count = n(), .groups = 'drop') %>%
    as.data.frame()
}
# Run function on five most populous turbines
agg_fault_7 <- fault_summary("Turbine 7")
agg_fault_12 <- fault_summary("Turbine 12")
agg_fault_14 <- fault_summary("Turbine 14")
agg_fault_13 <- fault_summary("Turbine 13")
agg_fault_15 <- fault_summary("Turbine 15")

# Function for plotting top 5 fault descriptions
top_5_faults <- function(data, turbine) {
  data %>%
    arrange(desc(total_count)) %>%
    slice(1:5) %>%
    ggplot(., aes(x=Fault_Description, y=total_count)) +
    geom_bar(stat='identity') + 
    labs(x="Fault Description", y="Frequency") +
    ggtitle(paste0("Top 5 Most Frequent Fault Descriptions for ", turbine)) +
    coord_flip() + 
    theme_bw()
}
# Run function on five most populous turbines
top_5_faults(agg_fault_7, "Turbine 7")
top_5_faults(agg_fault_12, "Turbine 12")
top_5_faults(agg_fault_14, "Turbine 14")
top_5_faults(agg_fault_13, "Turbine 13")
top_5_faults(agg_fault_15, "Turbine 15")

# fault subsets
u_phase <- subset(data_7, Fault_Description == "Geninv: 139 U-Phase Sharing")
no_lube <- subset(data_12, Fault_Description == "No Lubrication, Gen Bearings")
water_temp <- subset(data_14, Fault_Description == "Inv. Cooling Water Temp Warning")


# subset to sensor data columns
Vars1 = data.frame(u_phase$Oil_Temp, u_phase$Generator_RPM, u_phase$Wind_Speed, 
                   u_phase$Gearbox_Temp, u_phase$Active_Power, u_phase$Ambient_Temp,
                   u_phase$Hydraulic_Pressure)
## Correlation Matrix
cor(cbind(Vars1), use="pairwise.complete.obs")

# scatter plot of Hydraulic Pressure and Active Power
ggplot(data=u_phase, aes(x=Hydraulic_Pressure, y=Active_Power)) +
  geom_point(color="Dark Orange") +
  labs(x = "Generator RPM", y = "Active Power (kW)") +
  ggtitle("Active Power vs. Generator RPM U-Phase Faults") +
  theme_bw()

ggplot(data=data_7_no_faults, aes(x=Hydraulic_Pressure, y=Active_Power)) +
  geom_point(color="Dark Blue") +
  labs(x = "Generator RPM", y = "Active Power (kW)") +
  ggtitle("Active Power vs. Generator RPM No Faults") +
  theme_bw()

ggplot(data=u_phase, aes(x=Oil_Temp, y=Gearbox_Temp)) +
  geom_point(color="Dark Orange") +
  labs(x = "Oil Temperature (ºC)", y = "Gearbox Temperature (ºC)") +
  ggtitle("Gearbox vs. Oil Temperature (ºC) U-Phase Faults") +
  theme_bw()

ggplot(data=data_7_no_faults, aes(x=Oil_Temp, y=Gearbox_Temp)) +
  geom_point(color="Dark Blue") +
  labs(x = "Oil Temperature (ºC)", y = "Gearbox Temperature (ºC)") +
  ggtitle("Gearbox vs. Oil Temperature (ºC) No Faults") +
  theme_bw()

# # scatter plot of Generator_RPM and Active_Power for U-Phase Sharing data
# ggplot(data=u_phase, aes(x=Generator_RPM, y=delta_temp)) +
#   geom_point() +
#   labs(x = "Generator RPM", y = "Delta Temperature (Ambient & Gearbox) (ºC)") +
#   ggtitle("Active Power vs. Delta Temperature for U-Phase Sharing Faults") +
#   theme_bw()


##### OTHER NOTES -----
## SCATTER PLOT NOTES
# scatterplots - zoom in on the main trends in the scatterplots. Sometimes the default x/y limits aren’t ideal 
# scatterplots - find ways to deal with over plotting (alpha blending, faceting, heatmaps)
# scatterplots - independent variable (e.g., windspeed) should be on x axis while dependent variable (e.g.,power output) should be on y axis
# plots - clean them up, make them professional: 172 style. Daniel has noticed both the good and the bad.
# scatterplots - color by fault type, not just existence of a fault
# PLOTS PLOTS PLOTS, MULTIDIMENSIONAL ONES (scatterplots, but also incorporate color,facets) and then models based on that
# Do a decent scatterplot matrix, please: https://plotly.com/ggplot2/splom/

## HEATMAP RESOURCE
# https://r-graph-gallery.com/79-levelplot-with-ggplot2.html
