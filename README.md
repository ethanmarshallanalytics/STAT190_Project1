# Berkshire Hathaway Fault Data Modeling

Authors: Nick Pittman and Ethan Marshall

Project Description: Using data provided by Berkshire Hathaway Energy (BHE), we built a Random Forest model to help predict a fault code occuring in the next six hours. This can help turbine engineers be proactive in preparing for future maintenance problems.

Programming Language: R-Studio

Packages Used: dyplr tidyr ggplot2 lubridate pROC randomForest forcats tidyverse naniar imputeTS GGally plotly scales hrbrthemes RColorBrewer readr

Steps in the Data:
  1. Import raw data
  2. Clean the raw data and join all the data into one dataframe
  3. Build explanatory plots to have a better visual understanding of the data
  4. Run Baseline Random Forest
  5. Tune Forest
  6. Analyze rocCurve
  8. Build visualizations to analyze results

Datafiles in Project: 
  1. Datafiles such as Active Power, Ambient_Temp, Gearbox Data, Geneartor RPM, Hydraulic Pressure, and Windspeed compile all take in the raw data and clean it before appending it into a different file. Fault Codes is the raw data for faults.
  2. BHE_Data_Cleaning: This file takes in all of our raw data files from above and cleans it so that we can run analysis. (i.e. interpolations, grouping, etc.)
  3. BHE_Data_Cleaning_HOUR: This file is a subset of the BHE_Data_Cleaning. This adds the fault lagging variable as well as groups by one hour intervals.
  4. BHE_Data_Exploration: This file is where we ran exploratory plot analysis.
  5. BHE_Data_Visualizations: This file presents all refined visualizations for the whitepaper and final presentation.
  6. Random Forrest STAT 190: This is where we ran the actual Random Forest model
  7. BHE_Data_Merge: This file was used to import data for new turbines (not used in final model).
  8. Work Order: This is where we initially looked at work order data

Random Forest Outcome: We built two Random Forest models; one untuned and one tuned. After analyzing 90,945 BHE datapoints of sensor data every hour, here are some statistics.

Forest 1 (Untuned): Sensitiity: 80.8% Specificity: 67.2% AUC: 0.798

Forest 2 (Tuned): Sensitiity: 88.8% Specificity: 84.3% AUC: 0.939

Forest 1 was not properly tuned but showed a baseline of how our model would perform. After tuning to minimize OOB error, 500 trees and mtry=8 were used to develop our final model. The results were great, and the numbers indicate that this model can provide Berkshire Hathaway a strong analytical framework for predicting fault codes in the future.
