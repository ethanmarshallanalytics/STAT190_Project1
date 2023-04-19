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

master_data = read.csv("Project1Data/master_data.csv")

master_data$Is_Fault <- factor(master_data$Is_Fault)

master_data <- na.omit(master_data)

master_data <- master_data %>%
  mutate_if(sapply(master_data, is.character), as.factor)

master_data$Round_Time = ymd_hms(master_data$Round_Time)

RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx = sample(x=1:nrow(master_data), size = .7*nrow(master_data))
train.data = master_data[train.idx, ]
test.data = master_data[-train.idx, ]

## Baseline Forrest ------------------
myforest = randomForest(Is_Fault ~ Turbine + Fault_Type + prev_oil_temp + prev_gearbox_temp + prev_active_power +
                          prev_wind_speed + prev_generator_RPM + prev_active_power + prev_ambient_temp + prev_hydraulic_pressure,
                        data = train.data,
                        ntree = 500, # of classification trees in forest
                        mtry = 3,  # SQRT of 12
                        importance = TRUE)
myforest
## TUNING THE FOREST ----
# Moral of the story: fit as many trees as you have time for
# You cannot fit based on too many trees

# tune m actually is important and can affect model performance

# create a sequence of m values we want to try
mtry = c(7:9) # This can only be the number of x variables

#note: you can do each possible number if you have time if you are computationally limited,
#   see the notes on page 28 for how to choose a more limited list.

# make room for m and oob error (empty data frame)

keeps = data.frame(m = rep(NA, length(mtry)),
                   OOB_error_rate = rep(NA, length(mtry)))

# master_data <- head(master_data, 200000)




# create a loop that will fill the keeps data frame
for(idx in 1:length(mtry)){
  print(paste("Fitting m = ", mtry[idx])) # print out what iteration we are on
  tempforest = randomForest(Is_Fault ~ Fault_Type + prev_oil_temp + prev_gearbox_temp + prev_active_power +
                              prev_wind_speed + prev_generator_RPM + prev_active_power + prev_ambient_temp + prev_hydraulic_pressure,
                            data = train.data,
                            ntree = 500,
                            mtry = mtry[idx])
  keeps[idx, "m"] = mtry[idx]
  keeps[idx, "OOB_error_rate"] = mean(predict(tempforest) != train.data$Is_Fault)
  
  
}
keeps

#plot the OOB error rate vs m
ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_error_rate))


#Final Forest
final_forest = randomForest(Is_Fault ~ Fault_Type + prev_oil_temp + prev_gearbox_temp + prev_active_power +
                              prev_wind_speed + prev_generator_RPM + prev_active_power + prev_ambient_temp + prev_hydraulic_pressure,
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

test.data_sens = test.data %>% subset(Is_Fault == "1" & forest_pred == "0")

write.csv(test.data_sens, "Project1Data/T10_Sens_Forest.csv", row.names = FALSE)


## foreach package ------
cl = makeCluster(4) # Setting up a cluster with 3 machines
registerDoParallel(cl)

inputs = master_data[,c(1,8,10,12,14,16,18,20,22)]
outputs = master_data[,23]

rf <- foreach(ntree=rep(500, 4), .combine = combine, .packages='randomForest') %dopar% {
  randomForest(x=inputs, y=outputs, ntree=ntree, mtry=3)
}

## Ranger package ----
library(ranger)
RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx = sample(x=1:nrow(master_data), size = .7*nrow(master_data))
train.data = master_data[train.idx, ]
test.data = master_data[-train.idx, ]

inputs = master_data[,c(1,8,10,12,14,16,18,20,22)]
outputs = master_data[,23]

ranger_model <- ranger(formula = as.formula(paste(outputs, "~", paste(inputs, collapse = "+"))), 
                   data = train.data)

## Turbine 10 
master_data = master_data %>% subset(Turbine == "Turbine 10")
RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx = sample(x=1:nrow(master_data), size = .8*nrow(master_data))
train.data = master_data[train.idx, ]
test.data = master_data[-train.idx, ]
