# Berkshire Hathaway Fault Data Modeling

Authors: Nick Pittman and Ethan Marshall

Project Description: Using data provided by Berkshire Hathaway Energy (BHE), we built a Random Forest model to help predict a fault code occuring in the next six hours. This can help turbine engineers prepare for maintenance they may need to work on in the future and helps BHE stay on top of there possible turbine maitenance problems.

Programming Language: R-Studio

Packages Used: dyplr tidyr ggplot2 lubridate pROC randomForest forcats tidyverse naniar imputeTS GGally plotly scales hrbrthemes RColorBrewer readr

Steps in the Data:
  1. Import raw data
  2. Clean the raw data and join all the data into one dataframe
  3. Build explanatory plots to have a better visual understanding of the data
  4. Run Forest 1
  5. Tune Forest
  6. Analyze rocCurve
  8. Build visualizations to analyze results

Datafiles in Project: 
  1. Datafiles such as Active Power, Ambient_Temp, Gearbox Data, Geneartor RPM, Hydraulic Pressure, and Windspeed compile all take in the raw data and clean it before appending it into a different file
  2. BHE_Data_Cleaning: This file takes in all of our raw data files from above and cleans it so that we can run analysis. (i.e. interpolations, grouping, etc.)
  3. BHE_Data_Cleaning_HOUR: This file is a subset of the BHE_Data_Cleaning. This adds the fault lagging variable as well as groups by one hour intervals.
  4. BHE_Data_Exploration: This file is where we ran exploratory plot analysis
  5. Random Forrest STAT 190: This is where we ran the actual Random Forest model
  6. Work Order: This is where we initially looked at work order data

Random Forest Outcome: We built two Random Forest models; one untuned and one tuned. After analyzing 90,945 BHE datapoints of sensor data every hour, here are some statistics.

Forest 1 (Untuned): Sensitiity: 80.8% Specificity: 67.2% AUC: 0.798

Forest 2 (Tuned): Sensitiity: 88.8% Specificity: 84.3% AUC: 0.939

Forest 1 was not properly tuned but showed a baseline of how our model would perform. After we tuned our model to analyze 500 trees and found mtry = 8 to have the lowest OOB error we ran the model and got a wonderful model. These numbers provide to Berkshire Hathaway strong predictive modeling that can hopefully help detect fault codes occuring in the future
