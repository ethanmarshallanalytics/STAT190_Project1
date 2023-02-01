rm(list = ls())
library(tidyverse)
library(lubridate)

## WORK ORDER ------
wo <- read.csv("Project1Data/work order scrubbed.csv")

str(wo)
# start_date and finish_date are being read incorrectly -- read in as character
# is_failure is all 1
# cause_code is all NULL

wo$wo_start_date <- ymd_hms(wo$wo_start_date)
summary(wo)
wo$wo_finish_date <- ymd_hms(wo$wo_finish_date)
summary(wo)

# histogram of work order finish dates
ggplot(data = wo) + 
  geom_bar(aes(x = wo_finish_date))

# histogram of component type
ggplot(data = wo) +
  geom_bar(aes(x = component_type)) +
  coord_flip()

# subset of data based on repair time
# only look at repairs where component type <> null
filter_wo = filter(wo, component_type != 'null') # 74 rows

# find the length of time each repair took
filter_wo$length = difftime(filter_wo$wo_finish_date, filter_wo$wo_start_date, units="days")

# filter to only show repairs that took longer than 3 days to complete
long_repair = filter(filter_wo, length > 0)
# 27 repairs took longer than 1 day to complete (36.4%)
# 17 repairs took longer than 3 days to complete (23.0%)
# 12 repairs took longer than 1 week to complete (16.2%)

View(long_repair)

# most common components needed for repairs taking longer than 1 day in length
ggplot(data = long_repair) +
  geom_bar(aes(x = component_type)) +
  coord_flip()
# Converter No subsystem (Converter) and Converter Cooling System tied for most
# Transmission Cooling System in second


# find the most frequently repaired component for each Turbine
filter_wo %>% 
  filter(., location_id == 'Turbine 1') %>% # filter to a specific Turbine
  ggplot(data = .) + geom_bar(aes(x = component_type)) + coord_flip() #plot histogram of component_types
