## LIBRARIES, PACKAGES, and DATA PREP -----
rm(list = ls())
library(dplyr)
library(tidyverse)
library(forcats)
library(lubridate)
# install.packages("naniar")
library(naniar)
# install.packages("imputeTS")
library(imputeTS)

# Read in Plotting Data
plot_data = read.csv("Project1Data/plotting_data.csv")

## TIME SERIES MODEL ----
master_data = plot_data

# na_interpolation(master_data$Wind_Speed)
# ggplot_na_distribution(file)