fi <- function(data) {
    performance0 <- performance(data)$performance
    fi_values <- numeric()
    auc_values <- numeric()
    for (i in 1:(ncol(data) - 1)) {
        tmp <- performance(data[, -i])
        fi_values <- c(fi_values, performance0 - tmp$performance)
        auc_values <- c(auc_values, tmp$auc)
    }
    return(list(fi = fi_values, auc = auc_values))
}
