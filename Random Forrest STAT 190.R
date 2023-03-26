## Random Forrest for STAT 190

## Packages and Reading In---------

rm(list=ls())

library(randomForest)
library(ggplot2) # For professional exploratory graphics
library(pROC) #for ROC curves
library(dplyr)


clean_data = read.csv("Project1Data/clean_BHE_data.csv")
data_7 <- subset(clean_data, Turbine == "Turbine 7")
data_7 <- subset(data_7, select = -c(Datetime, Date, Status, Fault_Description, Round_Time, Wind_Speed_Group))

data_7 <- data_7 %>%
  mutate_if(sapply(data_7, is.character), as.factor)

# Changing Is_Fault from numerical to factor to run categorical instead of regression
data_7$Is_Fault <- factor(data_7$Is_Fault)

data_7$Fault_Code <- as.character(data_7$Fault_Code)

# Removes columns with missing data
data_7 <- na.omit(data_7)

RNGkind(sample.kind = "default")
set.seed(2291352)
train.idx = sample(x=1:nrow(data_7), size = .8*nrow(data_7))
train.data = data_7[train.idx, ]
train.data$Is_Fault <- factor(train.data$Is_Fault)
test.data = data_7[-train.idx, ]

## BASELIEN FOREST -----
base_forest = randomForest(Is_Fault ~ Fault_Code + Fault_Type + Oil_Temp + Generator_RPM +
                           Wind_Speed + Gearbox_Temp + Active_Power + Ambient_Temp + Hydraulic_Pressure + delta_temp,
                        data = train.data,
                        ntree = 1000, # of classification trees in forest
                        mtry = 3,  # SQRT of 10
                        importance = TRUE) 

# importance = TRUE will help us identify important predictors (later)
# note: it does make the algorithm slower
base_forest

### TUNING THE FOREST ----
# Answering the question: Can a too-high B overfit?
# R actually calculates the average OOB error for us
# it does it for 10 trees, 11 trees, 12 trees... all the way up to however many trees we fit

plot(base_forest)

# Moral of the story: fit as many trees as you have time for
# You cannot fit based on too many trees

# tune m actually is important and can affect model performance

# create a sequence of m values we want to try
mtry = c(1:12) # This can only be the number of x variables

#note: you can do each possible number if you have time if you are computationally limited,
#   see the notes on page 28 for how to choose a more limited list.

# make room for m and oob error (empty data frame)

keeps = data.frame(m = rep(NA, length(mtry)),
                   OOB_error_rate = rep(NA, length(mtry)))

# create a loop that will fill the keeps data frame
for(idx in 1:length(mtry)){
  print(paste("Fitting m = ", mtry[idx])) # print out what iteration we are on
  tempforest = randomForest(Is_Fault ~ Turbine + Fault_Code + Fault_Type + Oil_Temp + Generator_RPM +
                              Wind_Speed + Gearbox_Temp + Active_Power + Ambient_Temp + Hydraulic_Pressure + delta_temp,
                            data = train.data,
                            ntree = 1000,
                            mtry = mtry[idx])
  keeps[idx, "m"] = mtry[idx]
  keeps[idx, "OOB_error_rate"] = mean(predict(tempforest) != train.data$Result)
  
  
}
keeps

#plot the OOB error rate vs m
ggplot(data = keeps) +
  geom_line(aes(x = m, y = OOB_error_rate))


#Final Forest
final_forest = randomForest(Is_Fault ~ Turbine + Fault_Code + Fault_Type + Oil_Temp + Generator_RPM +
                              Wind_Speed + Gearbox_Temp + Active_Power + Ambient_Temp + Hydraulic_Pressure + delta_temp,
                            data = train.data,
                            ntree = 1000, # of classification trees in forest
                            mtry = 4,  # SQRT of 12
                            importance = TRUE)
final_forest

