## Random Forrest for STAT 190

## Packages and Reading In---------

rm(list=ls())

library(randomForest)
library(ggplot2) # For professional exploratory graphics
library(pROC) #for ROC curves
library(dplyr)
library(foreach)
library(doParallel)
library(lubridate)

master_data = read.csv("Project1Data/master_data_hour.csv")

master_data$Is_Fault_Lag <- factor(master_data$Is_Fault_Lag)

master_data <- na.omit(master_data)

## Sub-setting Master to variables we need (WHOLE DATA) -------
RF_data <- master_data %>% 
  select(Is_Fault_Lag, Fault_Type, Avg_Oil_Temp_inter, Min_Oil_Temp_inter,
         Max_Oil_Temp_inter, Avg_Generator_RPM_inter, Min_Generator_RPM_inter, Max_Generator_RPM_inter, 
         Avg_Gearbox_Temp_inter, Min_Gearbox_Temp_inter, Max_Gearbox_Temp_inter, Avg_Active_Power_inter,
         Min_Active_Power_inter, Max_Active_Power_inter, Avg_Ambient_Temp_inter, Min_Ambient_Temp_inter,
         Max_Ambient_Temp_inter, Avg_Hydraulic_Pressure_inter, Min_Hydraulic_Pressure_inter, Max_Hydraulic_Pressure_inter)

RF_data$Is_Fault_Lag <- factor(RF_data$Is_Fault_Lag)

RF_data <- na.omit(RF_data)

#RF_data <- RF_data %>%
#  mutate_if(sapply(master_data, is.character), as.factor)

#master_data$Round_Time = ymd_hms(master_data$Round_Time)

RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx = sample(x=1:nrow(RF_data), size = .8*nrow(RF_data))
train.data = RF_data[train.idx, ]
test.data = RF_data[-train.idx, ]

## Baseline Forrest ------------------
myforest = randomForest(Is_Fault_Lag ~ Turbine + Fault_Type + Oil_Temp_inter + Generator_RPM_inter +
                          Gearbox_Temp_inter + Active_Power_inter + Ambient_Temp_inter + Hydraulic_Pressure_inter,
                        data = train.data,
                        ntree = 200, # of classification trees in forest
                        mtry = 3,  # SQRT of 12
                        importance = TRUE)
myforest
## TUNING THE FOREST ----
# Moral of the story: fit as many trees as you have time for
# You cannot fit based on too many trees

# tune m actually is important and can affect model performance

# create a sequence of m values we want to try
mtry = c(1:18) # This can only be the number of x variables

#note: you can do each possible number if you have time if you are computationally limited,
#   see the notes on page 28 for how to choose a more limited list.

# make room for m and oob error (empty data frame)

keeps = data.frame(m = rep(NA, length(mtry)),
                   OOB_error_rate = rep(NA, length(mtry)))

# master_data <- head(master_data, 200000)




# create a loop that will fill the keeps data frame
for(idx in 1:length(mtry)){
  print(paste("Fitting m = ", mtry[idx])) # print out what iteration we are on
  tempforest = randomForest(Is_Fault_Lag ~ Avg_Oil_Temp_inter + Min_Oil_Temp_inter + Max_Oil_Temp_inter + 
                            Avg_Generator_RPM_inter + Min_Generator_RPM_inter + Max_Generator_RPM_inter + 
                            Avg_Gearbox_Temp_inter + Min_Gearbox_Temp_inter + Max_Gearbox_Temp_inter + 
                            Avg_Active_Power_inter + Min_Active_Power_inter + Max_Active_Power_inter + 
                            Avg_Ambient_Temp_inter + Min_Ambient_Temp_inter + Max_Ambient_Temp_inter + 
                              Avg_Hydraulic_Pressure_inter + Min_Hydraulic_Pressure_inter + Max_Hydraulic_Pressure_inter,
                            data = train.data,
                            ntree = 500,
                            mtry = mtry[idx])
  keeps[idx, "m"] = mtry[idx]
  keeps[idx, "OOB_error_rate"] = mean(predict(tempforest) != train.data$Is_Fault_Lag)
  
  
}
keeps


#plot the OOB error rate vs m
ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_error_rate))


#Final Forest
final_forest = randomForest(Is_Fault_Lag ~ Avg_Oil_Temp_inter + Min_Oil_Temp_inter +
                              Max_Oil_Temp_inter + Avg_Gearbox_Temp_inter + Min_Generator_RPM_inter + Max_Generator_RPM_inter + 
                              Avg_Gearbox_Temp_inter + Min_Gearbox_Temp_inter + Max_Gearbox_Temp_inter + Avg_Active_Power_inter +
                              Min_Active_Power_inter + Max_Active_Power_inter + Avg_Ambient_Temp_inter + Min_Ambient_Temp_inter +
                              Max_Ambient_Temp_inter + Avg_Hydraulic_Pressure_inter + Min_Hydraulic_Pressure_inter + Max_Hydraulic_Pressure_inter,
                            data = train.data,
                            ntree = 500,
                            mtry = 8, 
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

test_lag1_pred0 = test.data %>% subset(Is_Fault_Lag == "1" & forest_pred == "0")
test_lag0_pred1 = test.data %>% subset(Is_Fault_Lag == "0" & forest_pred == "1")

write.csv(test_lag1_pred0, "Project1Data/Lag1_Pred0.csv", row.names = FALSE)
write.csv(test_lag0_pred1, "Project1Data/Lag0_Pred1.csv", row.names = FALSE)

