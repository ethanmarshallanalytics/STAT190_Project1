## LOAD IN DATA AND PACKAGES ------
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

# load in raw data (original)
plot_data = read.csv("Project1Data/plotting_data.csv")
# filter data to only include Turbine 7
data_7 <- plot_data %>% filter(Turbine == "Turbine 7")

# further subset Turbine 7 data to only include data since Jan 1, 2022
data_7_sub <- filter(data_7, Date > "2022-03-01")

## SCATTER PLOT MATRIX -------
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

## HEATMAP PLOTS --------
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
## SCATTER PLOTS ----
# fault subsets
u_phase <- subset(data_7, Fault_Description == "Geninv: 139 U-Phase Sharing")

# Active Power and Generator RPM
ggplot(data=subset(u_phase, Active_Power<2000 & Generator_RPM<1400 & Generator_RPM >=100), aes(x=Generator_RPM, y=Active_Power)) +
  geom_point(color="darkred") +
  labs(x = "Generator RPM", y = "Active Power (kW)") +
  ggtitle("Active Power vs. Generator RPM U-Phase Faults") +
  scale_x_continuous(limits = c(125,1400), breaks=c(200,400,600,800,1000,1200,1400))+
  theme_bw()

# Gearbox Temperature and Generator RPM
ggplot(data=subset(u_phase, Generator_RPM<1400 & Generator_RPM>=100), aes(x=Generator_RPM, y=Gearbox_Temp)) +
  geom_point(color="darkred") +
  labs(x = "Generator RPM", y = "Gearbox Temperature (ºC)") +
  ggtitle("Gearbox Temperature vs. Generator RPM U-Phase Faults") +
  scale_x_continuous(breaks=c(200,400,600,800,1000,1200,1400))+
  theme_bw()
