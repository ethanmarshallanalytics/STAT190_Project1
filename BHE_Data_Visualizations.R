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
library(imputeTS)

# load in raw data (original)
plot_data = read.csv("Project1Data/plot_data_hour.csv")
plot_data$Is_Fault <- factor(plot_data$Is_Fault)
# filter data to only include Turbine 7
data_7 <- plot_data %>% filter(Turbine == "Turbine 7")
data_7_faults <- plot_data %>% filter(Turbine == "Turbine 7" & Is_Fault == "1")
data_7_no_faults <- plot_data %>% filter(Turbine == "Turbine 7" & Is_Fault == "0")

# further subset Turbine 7 data to only include data since Jan 1, 2022
data_7_sub <- filter(data_7, Date >= "2022-01-01")

## SUMMARY STATISTICS -------
model_variables = plot_data %>% select(Avg_Oil_Temp_inter,
                                       Min_Oil_Temp_inter,
                                       Max_Oil_Temp_inter,
                                       Avg_Generator_RPM_inter,
                                       Min_Generator_RPM_inter,
                                       Max_Generator_RPM_inter,
                                       Avg_Gearbox_Temp_inter,
                                       Min_Gearbox_Temp_inter,
                                       Max_Gearbox_Temp_inter,
                                       Avg_Active_Power_inter,
                                       Min_Active_Power_inter,
                                       Max_Active_Power_inter,
                                       Avg_Ambient_Temp_inter,
                                       Min_Ambient_Temp_inter,
                                       Max_Ambient_Temp_inter,
                                       Avg_Hydraulic_Pressure_inter,
                                       Min_Hydraulic_Pressure_inter,
                                       Max_Hydraulic_Pressure_inter)

summary(model_variables)

# distribution of Is_Fault
ggplot(data=plot_data, aes(x=Is_Fault, fill="")) +
  geom_bar() +
  scale_fill_manual(values = c("darkred")) +
  labs(y="Count", title="Distribution of Is_Fault") + 
  scale_y_continuous(breaks=c(15000,30000,45000,60000,75000), limits=c(0,75000)) +
  theme_bw() +
  theme(legend.position = "none")

## SCATTER PLOT MATRIX -------
# subset to sensor data columns
Vars = data.frame(data_7_sub$Avg_Oil_Temp, 
                  data_7_sub$Avg_Generator_RPM,
                  data_7_sub$Avg_Gearbox_Temp, 
                  data_7_sub$Avg_Active_Power, 
                  data_7_sub$Avg_Hydraulic_Pressure,
                  data_7_sub$Avg_Ambient_Temp,
                  data_7_sub$Is_Fault)

Vars <- Vars %>%
  rename("Oil_Temp" = "data_7_sub.Avg_Oil_Temp", 
         "Generator_RPM" = "data_7_sub.Avg_Generator_RPM",
         "Gearbox_Temp" = "data_7_sub.Avg_Gearbox_Temp", 
         "Active_Power" = "data_7_sub.Avg_Active_Power", 
         "Hydraulic_Pressure" = "data_7_sub.Avg_Hydraulic_Pressure",
         "Ambient_Temp" = "data_7_sub.Avg_Ambient_Temp",
         "Is_Fault" = "data_7_sub.Is_Fault")



## Correlation Matrix
# cor(cbind(Vars), use="pairwise.complete.obs")

## Scatter plot matrix
p <- ggpairs(Vars, 
             columns = 1:6,
             aes(color=Is_Fault, alpha=0.2)) +
  scale_color_manual(values = c("darkgrey", "darkred")) +
  scale_fill_manual(values = c("darkgrey", "darkred")) +
  ggtitle("Scatter Plot Matrix of Turbine 7 (2022)") + theme_bw()
ggplotly(p)

## HEATMAP PLOTS --------
## Active Power & Generator RPM heatmap plot
# fault data
ggplot(data=subset(data_7_faults, Avg_Active_Power<2000 & Avg_Generator_RPM<1400 & Avg_Generator_RPM >=100), aes(x=Avg_Generator_RPM, y=Avg_Active_Power)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(10,20)) +
  labs(x = "Generator RPM", y = "Active Power (kW)") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400)) +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Active Power vs. Generator RPM -- FAULTS") + 
  theme_bw()

# no fault data
ggplot(data=subset(data_7_no_faults, Avg_Active_Power<2000 & Avg_Generator_RPM<1400 & Avg_Generator_RPM >=100), aes(x=Avg_Generator_RPM, y=Avg_Active_Power)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(10,20)) +
  labs(x = "Generator RPM", y = "Active Power (kW)") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400)) +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Active Power vs. Generator RPM -- NO FAULTS") + 
  theme_bw()

## Generator RPM and Gearbox Temp
# fault data
ggplot(data=subset(data_7_faults, Avg_Generator_RPM<1400 & Avg_Generator_RPM>=100), aes(x=Avg_Generator_RPM, y=Avg_Gearbox_Temp)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(10,1)) +
  labs(x = "Generator RPM", y = "Gearbox Temperature (ºC)") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400)) +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Gearbox Temperature vs. Generator RPM -- FAULTS") +
  theme_bw()

# no fault data
ggplot(data=subset(data_7_no_faults, Avg_Generator_RPM<1400 & Avg_Generator_RPM>=100), aes(x=Avg_Generator_RPM, y=Avg_Gearbox_Temp)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(10,1)) +
  labs(x = "Generator RPM", y = "Gearbox Temperature (ºC)") +
  scale_x_continuous(breaks = c(200,400,600,800,1000,1200,1400)) +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Gearbox Temperature vs. Generator RPM -- NO FAULTS") +
  theme_bw()

## Gearbox Temp and Hydraulic Pressure
# fault data
ggplot(data=subset(data_7_faults, Avg_Hydraulic_Pressure<=225), aes(x=Avg_Gearbox_Temp, y=Avg_Hydraulic_Pressure)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1,4)) +
  labs(x = "Gearbox Temperature (ºC)", y = "Hydraulic Pressure (bar)") +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Hydraulic Pressure vs. Gearbox Temperature -- FAULTS") +
  theme_bw()

# no fault data
ggplot(data=subset(data_7_no_faults, Avg_Hydraulic_Pressure<=225), aes(x=Avg_Gearbox_Temp, y=Avg_Hydraulic_Pressure)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(1,4)) +
  labs(x = "Gearbox Temperature (ºC)", y = "Hydraulic Pressure (bar)") +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Hydraulic Pressure vs. Gearbox Temperature -- NO FAULTS") +
  theme_bw()

## Generator RPM and Hydraulic Pressure
# fault data
ggplot(data=subset(data_7_faults, Avg_Hydraulic_Pressure<224), aes(x=Avg_Generator_RPM, y=Avg_Hydraulic_Pressure)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(20,4)) +
  labs(x = "Generator_RPM", y = "Hydraulic Pressure (bar)") +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Hydraulic Pressure vs. Generator RPM -- FAULTS") +
  theme_bw()

# no fault data
ggplot(data=subset(data_7_no_faults, Avg_Hydraulic_Pressure<224), aes(x=Avg_Generator_RPM, y=Avg_Hydraulic_Pressure)) +
  stat_bin2d(aes(fill = after_stat(count)), binwidth = c(20,4)) +
  labs(x = "Generator_RPM", y = "Hydraulic Pressure (bar)") +
  scale_fill_gradient(low="darkgrey", high="darkred", name="Frequency") +
  ggtitle("Hydraulic Pressure vs. Generator RPM -- NO FAULTS") +
  theme_bw()
## SCATTER PLOTS ----
# fault subsets
u_phase <- subset(data_7, Fault_Description == "Geninv: 139 U-Phase Sharing")

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

## TIME SERIES INTERPOLATED PLOTS ----
# load in interpolated data
plot_data_full = read.csv("Project1Data/master_data_hour.csv")

# filter data to only include Turbine 7
# Filter the data to include only dates between March 2020 and May 2020
data_7 <- plot_data_full %>% 
  filter(Turbine == "Turbine 7") %>%
  filter(Round_Time >= as.Date("2020-02-01") & Round_Time <= as.Date("2020-05-31"))

# change Round_Time to datetime format
data_7$Round_Time = ymd_hms(data_7$Round_Time)

# Gearbox Temp time series plot
ggplot(data_7, aes(x = Round_Time)) +
  # Add the first y variable as a line
  geom_line(aes(y = Avg_Gearbox_Temp), color = "black") +
  # Add points where Gearbox_Temp is null
  geom_point(aes(y = ifelse(is.na(Avg_Gearbox_Temp), Avg_Gearbox_Temp_inter, NA)), color = "red") +
  # Set the y-axis label and legend
  labs(x = "Time (March 2020 - May 2020)", y = "Gearbox Temperature (ºC)") +
  ggtitle("Interpolated Gearbox Temperature Sensor Data") +
  theme_bw()

# distribution of interpolated values
gearbox_subset = plot_data_full %>% select(Round_Time, Avg_Gearbox_Temp, Avg_Gearbox_Temp_inter)
# change Round_Time to datetime
gearbox_subset$Round_Time = ymd_hms(gearbox_subset$Round_Time)
# Visualize the distribution of missing values
ggplot_na_distribution2(gearbox_subset$Avg_Gearbox_Temp, interval_size = 2160,
                        title = "Gearbox Temperature -- Missing Values by Quarter",
                        subtitle = "",
                        color_missing = "darkred",
                        color_existing = "black",
                        xlab = "Time (Interval Size: 2160 Rows (1 Quarter))",
                        theme = ggplot2::theme_bw())

ggplot_na_distribution2(plot_data_full$Avg_Oil_Temp, interval_size = 2160,
                        title = "Oil Temperature -- Missing Values by Quarter",
                        subtitle = "",
                        color_missing = "darkred",
                        color_existing = "black",
                        xlab = "Time (Interval Size: 2160 Rows (1 Quarter))",
                        theme = ggplot2::theme_bw())


## SENSITIVITY PLOTS ------
# load in data - Lag1_Pred0
sens = read.csv("Project1Data/Lag1_Pred0.csv")
summary(sens) # N=857

# bar chart grouped by Fault_Type
# ggplot(data = sens, aes(x = Fault_Type)) +
#   geom_bar(fill="grey", color="black", size=0.5) +
#   labs(title = "Sensitivity Analysis -- Fault Type Distribution", x = "Fault Type", y = "Count") +
#   theme_bw()

# scatter plot matrix (Avg)
# testing data
Vars = data.frame(sens$Avg_Oil_Temp_inter,
                  sens$Avg_Generator_RPM_inter,
                  sens$Avg_Gearbox_Temp_inter,
                  sens$Avg_Active_Power_inter,
                  sens$Avg_Ambient_Temp_inter,
                  sens$Avg_Hydraulic_Pressure_inter)

Vars <- Vars %>%
  rename("Oil_Temp" = "sens.Avg_Oil_Temp_inter",
         "Generator_RPM" = "sens.Avg_Generator_RPM_inter",
         "Gearbox_Temp" = "sens.Avg_Gearbox_Temp_inter",
         "Active_Power" = "sens.Avg_Active_Power_inter",
         "Ambient_Temp" = "sens.Avg_Ambient_Temp_inter",
         "Hydraulic_Pressure" = "sens.Avg_Hydraulic_Pressure_inter")

## Scatter plot matrix
p <- ggpairs(Vars,
             columns = 1:6) +
  ggtitle("Sensitivity Scatter Plot -- Average Data")
ggplotly(p)

# histograms of average data
# Ambient_Temp
ggplot(data = sens, aes(x = Avg_Ambient_Temp_inter)) +
  geom_histogram(binwidth = 1, fill="darkgrey", color="black", size=0.5) +
  labs(title = "Sensitivity Analysis -- Average Ambient Temp Distribution", x = "Ambient Temperature (ºC)", y = "Count") +
  scale_x_continuous(breaks = c(21,22,23,24,25,26,27,28,29,30,31,32,33,34)) +
  theme_bw()

# ggplot(data = sens, aes(x = Min_Ambient_Temp_inter)) +
#   geom_histogram(binwidth = 1, fill="darkgrey", color="black", size=0.5) +
#   labs(title = "Sensitivity Analysis -- Minimum Ambient Temp Distribution", x = "Ambient Temperature (ºC)", y = "Count") +
#   scale_x_continuous(breaks = c(21,22,23,24,25,26,27,28,29,30,31)) +
#   theme_bw()
# 
# ggplot(data = sens, aes(x = Max_Ambient_Temp_inter)) +
#   geom_histogram(binwidth = 1, fill="darkgrey", color="black", size=0.5) +
#   labs(title = "Sensitivity Analysis -- Maximum Ambient Temp Distribution", x = "Ambient Temperature (ºC)", y = "Count") +
#   scale_x_continuous(breaks = c(21,22,23,24,25,26,27,28,29,30,31,32,33,34)) +
#   theme_bw()

# Active_Power
ggplot(data = sens, aes(x = Avg_Active_Power_inter)) +
  geom_histogram(binwidth=60, fill="red", color="black", size=0.5) +
  labs(title = "Sensitivity Analysis -- Average Active Power Distribution", x = "Active Power (kW)", y = "Count") +
  scale_x_continuous(breaks = c(0,300,600,900,1200,1500,1800,2100,2400)) +
  theme_bw()

# ggplot(data = sens, aes(x = Min_Active_Power_inter)) +
#   geom_histogram(binwidth=5, fill="red", color="black", size=0.5) +
#   labs(title = "Sensitivity Analysis -- Minimum Oil Temp Distribution", x = "Oil Temperature (ºC)", y = "Count") +
#   scale_x_continuous(breaks = c(25,50,75,100,125,150,175,200)) +
#   theme_bw()
# 
# ggplot(data = sens, aes(x = Max_Active_Power_inter)) +
#   geom_histogram(binwidth=5, fill="red", color="black", size=0.5) +
#   labs(title = "Sensitivity Analysis -- Maximum Oil Temp Distribution", x = "Oil Temperature (ºC)", y = "Count") +
#   scale_x_continuous(breaks = c(25,50,75,100,125,150,175,200)) +
#   theme_bw()

# Hydraulic_Pressure
ggplot(data = sens, aes(x = Avg_Hydraulic_Pressure_inter)) +
  geom_histogram(binwidth=5, fill="darkred", color="black", size=0.5) +
  labs(title = "Sensitivity Analysis -- Average Hydraulic Pressure Distribution", x = "Hydraulic Pressure (bar)", y = "Count") +
  scale_x_continuous(breaks = c(25,50,75,100,125,150,175,200, 225)) +
  theme_bw()

# ggplot(data = sens, aes(x = Min_Hydraulic_Pressure_inter)) +
#   geom_histogram(binwidth=5, fill="darkred", color="black", size=0.5) +
#   labs(title = "Sensitivity Analysis -- Minimum Hydraulic Pressure Distribution", x = "Hydraulic Pressure (bar)", y = "Count") +
#   scale_x_continuous(breaks = c(25,50,75,100,125,150,175,200,225)) +
#   theme_bw()
# 
# ggplot(data = sens, aes(x = Max_Hydraulic_Pressure_inter)) +
#   geom_histogram(binwidth=5, fill="darkred", color="black", size=0.5) +
#   labs(title = "Sensitivity Analysis -- Maximum Hydraulic Pressure Distribution", x = "Hydraulic Pressure (bar)", y = "Count") +
#   scale_x_continuous(breaks = c(50,75,100,125,150,175,200,225,250)) +
#   theme_bw()
