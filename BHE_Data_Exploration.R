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
library(RColorBrewer)

## DATA PREP -------
# Read in Plotting Data
plot_data = read.csv("Project1Data/plotting_data.csv")
a <- read.csv("Project1Data/plot_data_hour.csv")
b <- read.csv("Project1Data/master_data_hour.csv")

# Read in Clean Data
clean_data = read.csv("Project1Data/clean_BHE_data.csv")

# Changing is_fault from Integer to character
clean_data$Is_Fault <- as.character(clean_data$Is_Fault)
plot_data$Is_Fault <- as.character(plot_data$Is_Fault)

# Read in work order 
wo = read.csv("Project1Data/work order scrubbed.csv")

# wo1 <- wo %>%
#   filter(component_type != "null")
# 
# write.csv(wo1, "Project1Data/wo1.csv")

# Read in filtered work order
wo1 = read.csv("Project1Data/wo1.csv")

# filter data to only include Turbine 7
data_7 <- plot_data %>% filter(Turbine == "Turbine 7")
data_7_faults <- plot_data %>% filter(Turbine == "Turbine 7" & Is_Fault == "1")
data_7_no_faults <- plot_data %>% filter(Turbine == "Turbine 7" & Is_Fault == "0")

# # filter data to only include Turbine 12
# data_12 <- plot_data %>% filter(Turbine == "Turbine 12")
# data_12_faults <- plot_data %>% filter(Turbine == "Turbine 12" & Is_Fault == "1")
# data_12_no_faults <- plot_data %>% filter(Turbine == "Turbine 12" & Is_Fault == "0")

# look at data tables
View(clean_data)
View(plot_data)
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

## Correlation Matrix
cor(cbind(Vars), use="pairwise.complete.obs")

## Scatter plot matrix
p <- ggpairs(Vars, 
             columns = 1:5,
             aes(color=Is_Fault, alpha=0.2)) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  scale_fill_manual(values = c("darkgrey", "darkred"))
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

## FINAL HEATMAP PLOTS ----
## Active Power & Generator RPM heatmap plot
# fault data
ggplot(data=subset(data_7_faults, Active_Power<2000 & Generator_RPM<1400 & Generator_RPM >=100), aes(x=Generator_RPM, y=Active_Power)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(10,20)) +
  labs(x = "Generator RPM", y = "Active Power (kW)") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400)) +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Active Power vs. Generator RPM -- FAULTS") + 
  theme_bw()

# no fault data
ggplot(data=subset(data_7_no_faults, Active_Power<2000 & Generator_RPM<1400 & Generator_RPM >=100), aes(x=Generator_RPM, y=Active_Power)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(10,20)) +
  labs(x = "Generator RPM", y = "Active Power (kW)") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400)) +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Active Power vs. Generator RPM -- NO FAULTS") +
  theme_bw()

## Generator RPM and Gearbox Temp
# fault data
ggplot(data=subset(data_7_faults, Generator_RPM<1400 & Generator_RPM>=100), aes(x=Generator_RPM, y=Gearbox_Temp)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(10,1)) +
  labs(x = "Generator RPM", y = "Gearbox Temperature (ºC)") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400)) +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Gearbox Temperature vs. Generator RPM -- FAULTS") +
  theme_bw()

# no fault data
ggplot(data=subset(data_7_no_faults, Generator_RPM<1400 & Generator_RPM>=100), aes(x=Generator_RPM, y=Gearbox_Temp)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(10,1)) +
  labs(x = "Generator RPM", y = "Gearbox Temperature (ºC)") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400)) +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Gearbox Temperature vs. Generator RPM -- NO FAULTS") +
  theme_bw()

## Gearbox Temp and Hydraulic Pressure
# fault data
ggplot(data=subset(data_7_faults, Hydraulic_Pressure<=225), aes(x=Gearbox_Temp, y=Hydraulic_Pressure)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1,4)) +
  labs(x = "Gearbox Temperature (ºC)", y = "Hydraulic Pressure (bar)") +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Hydraulic Pressure vs. Gearbox Temperature -- FAULTS") +
  theme_bw()

# no fault data
ggplot(data=subset(data_7_no_faults, Hydraulic_Pressure<=225), aes(x=Gearbox_Temp, y=Hydraulic_Pressure)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1,4)) +
  labs(x = "Gearbox Temperature (ºC)", y = "Hydraulic Pressure (bar)") +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Hydraulic Pressure vs. Gearbox Temperature -- NO FAULTS") +
  theme_bw()

## Generator RPM and Hydraulic Pressure
# fault data
ggplot(data=subset(data_7_faults, Hydraulic_Pressure<224), aes(x=Generator_RPM, y=Hydraulic_Pressure)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(20,4)) +
  labs(x = "Generator_RPM", y = "Hydraulic Pressure (bar)") +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Hydraulic Pressure vs. Generator RPM -- FAULTS") +
  theme_bw()

# no fault data
ggplot(data=subset(data_7_no_faults, Hydraulic_Pressure<224), aes(x=Generator_RPM, y=Hydraulic_Pressure)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(20,4)) +
  labs(x = "Generator_RPM", y = "Hydraulic Pressure (bar)") +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Hydraulic Pressure vs. Generator RPM -- NO FAULTS") +
  theme_bw()

#### AGGREGATED DATA AND SUMMARY ANALYSIS -----
# count of Fault occurrances aggregated by turbine
agg_fault <- plot_data %>%
  filter(Is_Fault == "1") %>%
  group_by(Turbine) %>%
  summarise(Fault_Count = n()) %>%
  as.data.frame()
# Turbine 7, 12, 14, 13, 15 have the most fault occurrences

# Function for summarizing fault codes
# Turbine is a string of the turbine name
fault_summary <- function(turbine) {
  faults <- plot_data %>% filter(Turbine == turbine & Is_Fault == "1")
  faults %>%
    group_by(Fault_Code, Fault_Description) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    as.data.frame()
}
# Run function on five most populous turbines
agg_fault_7 <- fault_summary("Turbine 7")
# agg_fault_12 <- fault_summary("Turbine 12")
# agg_fault_14 <- fault_summary("Turbine 14")
# agg_fault_13 <- fault_summary("Turbine 13")
# agg_fault_15 <- fault_summary("Turbine 15")

# Function for plotting top 5 fault descriptions
# data is an aggregated dataset for turbines
# turbine is a string of a turbine name
top_5_faults <- function(data, turbine) {
  data %>%
    arrange(desc(Count)) %>%
    slice(1:5) %>%
    ggplot(., aes(x=Fault_Description, y=Count)) +
    geom_bar(stat='identity') + 
    labs(x="Fault Description", y="Frequency") +
    ggtitle(paste0("Top 5 Fault Descriptions for ", turbine)) +
    coord_flip() + 
    theme_bw()
}
# Run function on five most populous turbines
top_5_faults(agg_fault_7, "Turbine 7")
# top_5_faults(agg_fault_12, "Turbine 12")
# top_5_faults(agg_fault_14, "Turbine 14")
# top_5_faults(agg_fault_13, "Turbine 13")
# top_5_faults(agg_fault_15, "Turbine 15")

# fault subsets
u_phase <- subset(data_7, Fault_Description == "Geninv: 139 U-Phase Sharing")
# gen_bear <- subset(data_12, Fault_Description == "No Lubrication, Gen Bearings")
# water_temp <- subset(data_14, Fault_Description == "Inv. Cooling Water Temp Warning")

# Active Power and Generator RPM
ggplot(data=subset(u_phase, Avg_Active_Power<2000 & Avg_Generator_RPM<1400 & Avg_Generator_RPM >=100), aes(x=Avg_Generator_RPM, y=Avg_Active_Power)) +
  geom_point(color="darkred") +
  labs(x = "Generator RPM", y = "Active Power (kW)") +
  ggtitle("Active Power vs. Generator RPM U-Phase Faults") +
  scale_x_continuous(limits = c(125,1400), breaks=c(200,400,600,800,1000,1200,1400))+
  theme_bw()

# Gearbox Temperature and Generator RPM
ggplot(data=subset(u_phase, Avg_Generator_RPM<1400 & Avg_Generator_RPM>=100), aes(x=Avg_Generator_RPM, y=Avg_Gearbox_Temp)) +
  geom_point(color="darkred") +
  labs(x = "Generator RPM", y = "Gearbox Temperature (ºC)") +
  ggtitle("Gearbox Temperature vs. Generator RPM U-Phase Faults") +
  scale_x_continuous(breaks=c(200,400,600,800,1000,1200,1400))+
  theme_bw()

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
# https://tidyverse.github.io/ggplot2-docs/reference/geom_bin2d.html

