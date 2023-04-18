## Random Forrest for STAT 190

## Packages and Reading In---------

rm(list=ls())

library(randomForest)
library(ggplot2) # For professional exploratory graphics
library(pROC) #for ROC curves
library(dplyr)


master_data = read.csv("Project1Data/master_data.csv")

## Turbine 7 Data Cleaning ---------
data_7 <- subset(clean_data, Turbine == "Turbine 7")
data_7 <- subset(data_7, select = -c(Datetime, Date, Status, Fault_Description, Round_Time, Wind_Speed_Group))

data_7 <- data_7 %>%
  mutate_if(sapply(data_7, is.character), as.factor)

# Changing Is_Fault from numerical to factor to run categorical instead of regression
data_7$Is_Fault <- factor(data_7$Is_Fault)

data_7$Fault_Code <- as.character(data_7$Fault_Code)

# Removes columns with missing data
data_7 <- na.omit(data_7)

## 2000 from each Turbine Cleaning ------
all_turbine <- clean_data %>%
  group_by(Turbine) %>%
  slice(1:2000)

all_turbine <- subset(all_turbine, select = -c(Datetime, Date, Status, Fault_Description, Round_Time, Wind_Speed_Group))

#data_7 <- data_7 %>%
#  mutate_if(sapply(data_7, is.character), as.factor)

# Changing Is_Fault from numerical to factor to run categorical instead of regression
all_turbine$Is_Fault <- factor(all_turbine$Is_Fault)

all_turbine$Fault_Code <- as.character(all_turbine$Fault_Code)

# Removes columns with missing data
all_turbine <- na.omit(all_turbine)

RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx = sample(x=1:nrow(all_turbine), size = .7*nrow(all_turbine))
train.data = all_turbine[train.idx, ]
train.data$Is_Fault <- factor(train.data$Is_Fault)
test.data = all_turbine[-train.idx, ]
## TUNING THE FOREST ----
# Moral of the story: fit as many trees as you have time for
# You cannot fit based on too many trees

# tune m actually is important and can affect model performance

# create a sequence of m values we want to try
mtry = c(2,3,6) # This can only be the number of x variables

#note: you can do each possible number if you have time if you are computationally limited,
#   see the notes on page 28 for how to choose a more limited list.

# make room for m and oob error (empty data frame)

master_data <- na.omit(master_data)

master_data <- master_data %>%
  mutate_if(sapply(master_data, is.character), as.factor)

keeps = data.frame(m = rep(NA, length(mtry)),
                   OOB_error_rate = rep(NA, length(mtry)))

#master_data <- head(master_data, 250000)


RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx = sample(x=1:nrow(master_data), size = .7*nrow(master_data))
train.data = master_data[train.idx, ]
test.data = master_data[-train.idx, ]

# create a loop that will fill the keeps data frame
for(idx in 1:length(mtry)){
  print(paste("Fitting m = ", mtry[idx])) # print out what iteration we are on
  tempforest = randomForest(Is_Fault ~ Turbine + Fault_Type + prev_oil_temp + prev_gearbox_temp + prev_active_power +
                              prev_wind_speed + prev_generator_RPM + prev_active_power + prev_ambient_temp + prev_hydraulic_pressure,
                            data = train.data,
                            ntree = 100,
                            mtry = mtry[idx])
  keeps[idx, "m"] = mtry[idx]
  keeps[idx, "OOB_error_rate"] = mean(predict(tempforest) != train.data$Is_Fault)
  
  
}
keeps

#plot the OOB error rate vs m
ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_error_rate))


#Final Forest
final_forest = randomForest(Is_Fault ~ Fault_Code + Fault_Type + Oil_Temp + Generator_RPM +
                              Wind_Speed + Gearbox_Temp + Active_Power + Ambient_Temp + Hydraulic_Pressure + delta_temp,
                            data = train.data,
                            ntree = 500, # of classification trees in forest
                            mtry = 7,  # SQRT of 10
                            importance = TRUE)
final_forest

## Results ------
pi_hat = predict(final_forest, test.data, type = "prob")[, "1"] # extract prob of positive event
rocCurve = roc(response = test.data$Is_Fault,
               predictor = pi_hat,
               levels = c("0", "1"))
plot(rocCurve, print.thres = TRUE, print.auc = TRUE)

pi_star = coords(rocCurve, "best", ret = "threshold")$threshold[1]
pi_star

test.data$forest_pred = as.factor(ifelse(pi_hat > pi_star, "1", "0"))
View(test.data)

table(train_data$Is_Fault)

# AUC = 0.892
# pi* = 0.9015 ... we will only predict a fault occurring when the P(fault) > .9015
# Specificity: .789
# Sensitivity: .980 (VERY VERY GOOD)


