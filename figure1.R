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

#########################################
# select all features
k <- 1:29
# threshold for maximum FI
E0 <- 0

# save data for performancecomparison
data0 <- data

# initializing flag with true value
flag <- TRUE
while (flag) {
    FI <- fi(data)
    idx <- which.min(FI$fi)
    idx <- idx[which.max(FI$auc[idx])]
    E <- FI$fi[idx]
    if (E <= E0) {
        flag <- TRUE        
        k <- k[-idx]
        data <- data[, c(k, which(names(data) %in% "target_variable"))]
        k <- 1:(ncol(data) - 1)
    } else {
        flag <- FALSE
    }
}

################ testing the performance after recursive feature elimination

# we can see that number of features reduced from 29 to 22
names(data)
# [1] "X1"              "X3"              "X4"              "X5"              "X7"
# [6] "X8"              "X9"              "X10"             "X11"             "X12"
# [11] "X13"             "X14"             "X15"             "X16"             "X17"
# [16] "X20"             "X21"             "X22"             "X24"             "X26"
# [21] "X27"             "X28"             "target_variable"
dim(data)
# [1] 200  23

# and with performance (average_npv12) increased from 0.995416 to 0.9986244
performance(data0)$performance
# $performance
# [1] 0.995416
performance(data)$performance
# $performance
# [1] 0.9986244
