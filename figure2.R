# Load required libraries
library(caret) # For machine learning
library(pROC) # For AUC calculation

##import performance function and feature importance function
source(".\performance.R")
source(".\fi.R")

######################
###### simulation begin
# simulated data with 29 features and 1 binary target variable.
# And the target variable (the one you want to predict) is named 'target_variable'
data <- read.table(".\simulated_data.txt", head=TRUE)
data$target_variable <- as.factor(data$target_variable)

#calculate feature importance
out <- fi(data)

# print feature importance scores
print(out$fi)

#$fi
# [1]  1.495957e-03 -1.384843e-04  2.060766e-03  2.779235e-04  2.076819e-03 -1.267516e-03
# [7]  1.314307e-03 -2.955485e-04  1.862637e-03  1.890001e-03  1.202576e-04  1.467286e-03
#[13] -2.523005e-04  4.195095e-03 -5.498657e-04  2.191047e-03  8.177225e-04 -5.621989e-04
#[19] -8.363181e-04  5.812795e-04 -2.792114e-05  2.997767e-04 -2.997767e-04 -3.930484e-04
#[25]  5.522719e-04  1.440458e-04  4.184590e-04 -2.739245e-04 -4.358531e-04