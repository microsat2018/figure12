performance <- function(data) {
    # Define the number of repeats and folds
    num_repeats <- 10
    num_folds <- 5

    # Shuffle the data (optional, for shuffling)
    set.seed(123) # Set a seed for reproducibility
    data_shuffled <- data[sample(nrow(data)), ]

    # Create stratified folds
    folds <- createMultiFolds(data_shuffled$target_variable, k = num_folds, times = num_repeats)

    # Define the base rate for NPV calculation (12% in this example)
    base_rate <- 0.12

    # Initialize vectors to store the metrics for each fold
    auc_values <- numeric()
    npv_values <- numeric()
    ppv_values <- numeric()
    sn_values <- numeric()
    sp_values <- numeric()
    acc_values <- numeric()
    precision_values <- numeric()
    npv12_values <- numeric()

    predicted_probs <- function(model, newdata) {
        prob <- predict(model, newdata = newdata, probability = TRUE)
        return(attr(prob, "probabilities")[, 1])
    }

    # Perform the cross-validation using SVM with radial basis function (RBF) kernel
    # Make sure you have installed and loaded the necessary package for SVM (e.g., 'e1071').
    for (fold in 1:(num_repeats * num_folds)) {
        # Split the data into training and testing sets for this fold
        test_indices <- setdiff(1:nrow(data_shuffled), folds[[fold]])
        test_data <- data_shuffled[test_indices, ]
        train_data <- data_shuffled[folds[[fold]], ]

        # Train the SVM model with radial basis function (RBF) kernel
        model <- svm(target_variable ~ .,
            data = train_data,
            kernel = "radial",
            probability = TRUE
        ) # Set probability to TRUE to enable class probabilities

        # Make predictions on the test set
        # predicted_probs <- predict(model, newdata = test_data, probability = TRUE)

        probs <- predicted_probs(model, newdata = test_data)

        actual_labels <- test_data$target_variable

        # Calculate AUC for this fold and store it in the auc_values vector
        auc_values <- c(auc_values, roc(actual_labels, probs)[["auc"]])

        # Calculate comfusion matrix
        cm <- confusionMatrix(as.factor(predict(model, newdata = test_data)), as.factor(actual_labels))

        # Calculate Sensitivity (SN) for this fold and store it in the sn_values vector
        sn_values <- c(sn_values, cm$byClass["Sensitivity"])

        # Calculate Specificity (SP) for this fold and store it in the sp_values vector
        sp_values <- c(sp_values, cm$byClass["Specificity"])

        # Calculate Accuracy (ACC) for this fold and store it in the acc_values vector
        acc_values <- c(acc_values, cm$overall["Accuracy"])

        # Calculate Precision for this fold and store it in the precision_values vector
        precision_values <- c(precision_values, cm$byClass["Precision"])

        # Calculate NPV for this fold and store it in the npv_values vector
        npv_values <- c(npv_values, cm$byClass["Neg Pred Value"])

        # Calculate PPV for this fold and store it in the ppv_values vector
        ppv_values <- c(ppv_values, cm$byClass["Pos Pred Value"])

        # Calculate NPV12 for this fold and store it in the npv_values vector
        npv12 <- cm$byClass["Specificity"] * (1 - base_rate) / ((1 - cm$byClass["Sensitivity"]) * base_rate + cm$byClass["Specificity"] * (1 - base_rate))
        npv12_values <- c(npv12_values, npv12)
    }

    # Calculate the average values for all metrics across all folds
    # if some following metric is preferred, please use the corresponding average value.
    average_auc <- mean(auc_values)
    # average_npv <- mean(npv_values)
    # average_ppv <- mean(ppv_values)
    # average_sn <- mean(sn_values)
    # average_sp <- mean(sp_values)
    # average_acc <- mean(acc_values)
    # average_precision <- mean(precision_values)
    average_npv12 <- mean(npv12_values)

    # Print the average values for all metrics
    # print(paste("Average AUC:", average_auc))
    # print(paste("Average NPV:", average_npv))
    # print(paste("Average PPV:", average_ppv))
    # print(paste("Average Sensitivity (SN):", average_sn))
    # print(paste("Average Specificity (SP):", average_sp))
    # print(paste("Average Accuracy (ACC):", average_acc))
    # print(paste("Average Precision:", average_precision))
    print(paste("NPV at Base Rate (12%):", average_npv12))

    # set average NPV12 as performance
    # if some metric is preferred, please use the corresponding average value instead.

    performance <- average_npv12
    return(list(performance = performance, auc = average_auc))
}
