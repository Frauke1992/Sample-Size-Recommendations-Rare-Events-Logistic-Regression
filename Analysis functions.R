library(caret)
library(tibble)
library(caret)

########################################################
############# Analysis errors and warnings #############
########################################################
# Function to extract errors and warnings from datadata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAACAAAAAgCAYAAABzenr0AAADG0lEQVR42u2XW0hTYRzA99jt0XOOmHi+s4f6vrPoJaigiOhG2bCSJHJQL711oXuZ1JQuRhZlaUpGF7M2B5q2tLW5i6YQQRcMIixQAqUIJKFcq4fT9x+dcXa2s52zNZ/6w5+x/7bz/b7/fSaTSvLMeB7HW1ZxReJq1kzWsAJZywp4HYMs6xmeFOfxZCNnxlZOEEtYRDazPC5lBMtWRhDLOES2cQhvzxfExQihGaZMBA6XshBXR5cUejbQQWGWZgQBN88WIBwOS4G+gU62aP6ywsLCmYYAwO3ZAoAAhM8fcjMCWWEIAuL9LwBkCK8/1J2P8MqCgkWzdAHwC5Zv6ep/L51o8kvlVe1SyVFH9BXegz0c+a0bQIbw+AJPwLNpITYdc5RbD7dOwKFauvP0Q6nv9ahuABmi2+P1gnc1IejD7akOVqvD91Y3gAzh7vH0Mry4geMWzk64ufLhHz99jdOh4XHJ5X0j7ahuj4MIvhrRDSBDPOrxBKGPxA4vs7vm0Id9Vj4YZOrnrxjAxOSPqG3sy7c4AFtVh/R9KpIAUHOxLq0qXO/co3YvCBystIEXQCobnsbZHw8OG64URsBnlbH36gEIvPgQtdfcDsbZTzUHDQPQFn5eCTCWDADc3t3/LqrPh0ZjtsSq6DQOwONaJUAkGYBaIARq94OWVrQZBxDES0qA8VQhkG9f7xzU6AvGPcAgckV3DuytdUcrAtyvLkPQk80BwwAcj68aqgLIAxBIRPV3M6kCumPUp+0DSgC4udwLlHmQrA/orILrqjbstCkBoNTUCQehUNsDL0cympgswk05mwX6ypDc0BhITpv1YMtkummYbAYY64TkpuZInkuW7Ie5X9HYm9E+oKsKEL6lCUA3391SjoUm4R1tACTuyzkAT1q0t2JEDkwDwD3tpZTHh3IfAvF+iq2YHJmGHHiQIgfw8VwD0GHkTPHXjFTmGoDmWVuqMiymrdJO41RNG8YZDonnYIOhiXMB5jilv0zruI7jxWs0XA30s0ZordDdoMFAjUOZUb0L2U5X8VZwOf2dA24Oh3MC2WX6L3/lDxSyAwzwmsreAAAAAElFTkSuQmCCsets, requires the data to
# be a list of datasets from a single combination of simulation factors
data.warnings <- function(sample_data){
  # Loop iterating over the different datasets within a condition
  total_results_predictors <- sapply(sample_data, FUN = function(current_sample){
    sample_counter <<- sample_counter + 1
    
    # Set up a matrix to hold the warnings
    warning_matrix <- matrix(nrow = 2, ncol = 4)
    # Set the columnames of the matrix to the different warnings that appeared
    colnames(warning_matrix) <- c("Non-Convergence", "Separation fitted probs",
                                  "Separation adjusted probs", "Rank-Deficient Fit")
    # Set the rownames to the type of evaluation
    rownames(warning_matrix) <- c("LogReg", "UpsamplingLogReg")
    
    # Extract the first element of the LR-results of the current sample
    logreg <- current_sample$LogReg[1]
    
    # If the name of the element is "coefficients", no warning was thrown
    if(names(logreg) == "coefficients"){
      # set all elements of the LR-line to false
      warning_matrix[1, ] <- FALSE
    
    # If a warning happenend: check which name it matches, and set that element of
    # the LR-line to true while setting all other elements to false
    } else if(names(logreg) =="Non-Convergence!"){
      warning_matrix[1, ] <- FALSE
      warning_matrix[1, 1] <- TRUE
    } else if(names(logreg) == "Separation!"){
      if(grepl("fitted probabilities", logreg[[1]])){
        warning_matrix[1, ] <- FALSE
        warning_matrix[1, 2] <- TRUE
      } else if(grepl("adjusted probabilities", logreg[[1]])){
        warning_matrix[1, ] <- FALSE
        warning_matrix[1, 3] <- TRUE
      }
    } else if(names(logreg) == "Rank-Deficient Fit!"){
      warning_matrix[1, ] <- FALSE
      warning_matrix[1, 4] <- TRUE
      
      # If instead an error occured: print out the error
    } else if(names(logreg) == "Error!"){
      print(paste0("Error in LogReg in ", 
                   names(result_data_rearranged)[condition_counter],
                   ", ", names(sample_data)[sample_counter], ": ", logreg[[1]]))
      # if none of this is true: print out that the element had an unexpected name 
    } else{
      print(paste0("Unexpected name in LogReg in", 
                   names(result_data_rearranged)[condition_counter],
                   ", ", names(sample_data)[sample_counter]))
    }
    
    # Same procedure for LogReg with upsampling
    # Extract the first element of LR with upsampling of the current sample
    upsamp_logreg <-current_sample$UpsamplingLogReg[1]
    # If the name of the element is "coefficients", no warning was thrown
    if(names(upsamp_logreg) == "coefficients"){
      # set all elements of the LR-with-upsamplingline to false
      warning_matrix[2, ] <- FALSE
      
     # If a warning happenend: check which name it matches, and set that element of
     # the LR-with-upsampling-line to true while setting all other elements to false
    } else if(names(upsamp_logreg) =="Non-Convergence!"){
      warning_matrix[2, ] <- FALSE
      warning_matrix[2, 1] <- TRUE
    } else if(names(upsamp_logreg) == "Separation!"){
      if(grepl("fitted probabilities", upsamp_logreg[[1]])){
        warning_matrix[2, ] <- FALSE
        warning_matrix[2, 2] <- TRUE
      } else if(grepl("adjusted probabilities", upsamp_logreg[[1]])){
        warning_matrix[2, ] <- FALSE
        warning_matrix[2, 3] <- TRUE
      }
    } else if(names(upsamp_logreg) == "Rank-Deficient Fit!"){
      warning_matrix[2, ] <- FALSE
      warning_matrix[2, 4] <- TRUE
      # If instead an error occured: print out the error
    } else if(names(upsamp_logreg) == "Error!"){
      print(paste0("Error in Upsampling in ", 
                   names(result_data_rearranged)[condition_counter],
                   ", ", names(sample_data)[sample_counter], 
                   ": ", upsamp_logreg[[1]]))
      # if none of this is true: print out that the element had an unexpected name 
    } else{
      print(paste0("Unerwarteter Name in Upsampling in", 
                   names(result_data_rearranged)[condition_counter],
                   ", ", names(sample_data)[sample_counter]))
    }
    # Return the matrix
    return(warning_matrix)
  }, simplify = "array")
}


###############################################
############# Analysis predictors #############
###############################################
# Function to extract the predictor values from the data, requires the data to
# be a list of datasets from a single combination of simulation factors
# Extracts significant coefficients for LR and non-zero coefficients for ElasticNet
data.predictors <- function(sample_data, warnings, noise, condition) {
  total_results_predictors <- sapply(sample_data, FUN = function(current_sample) {
    
    # Checks if warning happened in less than 50% of datasets for current 
    # condition for LR
    if(warnings["LogReg", condition, noise]< .5){
      
      # Checks if no warning happened in the current dataset
      if(names(current_sample$LogReg[1]) == "coefficients"){
        # Check if p-values of coefficients below .05
        logreg <- current_sample$LogReg$coefficients[, 2] < 0.05
        # Set all elements for which the p-value was not below .05 to NA
        logreg[logreg == FALSE] <- NA
        # multiply the matrix with the results of the significance-check with the 
        # coefficients-matrix
        logreg_pred <- matrix(logreg * current_sample$LogReg$coefficients[, 1], 
                              ncol = 1)
      } else{
        # if a warning happened: return NA for all predictors
        logreg_pred <- current_sample$ElasticNetRoc$coefficients
        logreg_pred[] <- NA
      }
    } else{
      # if warnings happened in 50% or more of datasets: return NA for all predictors
      logreg_pred <- current_sample$ElasticNetRoc$coefficients
      logreg_pred[] <- NA
    }
    
    # Check which coefficients have non-zero values
    enet_roc <- current_sample$ElasticNetRoc$coefficients != 0
    # Set all other coefficients to NA
    enet_roc[enet_roc == FALSE] <- NA
    # multiply the matrix with the results of the non-zero-check with the 
    # coefficients-matrix
    enet_roc_pred <- enet_roc * current_sample$ElasticNetRoc$coefficients
    
    # Check which coefficients have non-zero values
    enet_logloss <- current_sample$ElasticNetLogloss$coefficients != 0
    # Set all other coefficients to NA
    enet_logloss[enet_logloss == FALSE] <- NA
    # multiply the matrix with the results of the non-zero-check with the 
    # coefficients-matrix
    enet_logloss_pred <- enet_logloss * current_sample$ElasticNetLogloss$coefficients
    
    # checks if warning happened in less than 50% of datasets for current 
    # condition for LR with upsampling
    if(warnings["UpsamplingLogReg", condition, noise]< .5){
      
      # Checks if no warning happened in the current dataset
      if(names(current_sample$UpsamplingLogReg[1]) == "coefficients"){
        # Check if p-values of coefficients below .05
        upsamp_logreg <- current_sample$UpsamplingLogReg$coefficients[, 2] < 0.05
        # Set all elements for which the p-value was not below .05 to NA
        upsamp_logreg[upsamp_logreg == FALSE] <- NA
        
        # Multiply the matrix with the results of the significance-check with the 
        # coefficients-matrix
        upsamp_logreg_pred <- matrix(upsamp_logreg * 
                                       current_sample$UpsamplingLogReg
                                     $coefficients[, 1], 
                                     ncol = 1)
      } else{
        # if a warning happened: return NA for all predictors
        upsamp_logreg_pred <- current_sample$ElasticNetRoc$coefficients
        upsamp_logreg_pred[] <- NA
      }
    } else{
      # if warnings happened in 50% or more of datasets: return NA for all predictors
      upsamp_logreg_pred <- current_sample$ElasticNetRoc$coefficients
      upsamp_logreg_pred[] <- NA
    }
    
    # Check which coefficients have non-zero values
    upsamp_enet_roc <- current_sample$UpsamplingElasticNetRoc$coefficients != 0
    # Set all other coefficients to NA
    upsamp_enet_roc[upsamp_enet_roc == FALSE] <- NA
    # multiply the matrix with the results of the non-zero-check with the 
    # coefficients-matrix
    upsamp_enet_roc_pred <- enet_roc * 
      current_sample$UpsamplingElasticNetRoc$coefficients
    
    # Check which coefficients have non-zero values
    upsamp_enet_logloss <- current_sample$UpsamplingElasticNetLogloss$coefficients != 0
    # Set all other coefficients to NA
    upsamp_enet_logloss[upsamp_enet_logloss == FALSE] <- NA
    # multiply the matrix with the results of the non-zero-check with the 
    # coefficients-matrix
    upsamp_enet_logloss_pred <- upsamp_enet_logloss * 
      current_sample$UpsamplingElasticNetLogloss$coefficients
    
    # Combine results for all methods with and without upsampling
    predictors_results <- matrix(c(logreg_pred, enet_roc_pred, enet_logloss_pred, 
                                   upsamp_logreg_pred, upsamp_enet_roc_pred, 
                                   upsamp_enet_logloss_pred), ncol = 6)
    
    # Set column names of the resulting matrix to the method-upsampling-combinations
    colnames(predictors_results) <- c("LogReg","EnetRoc", "EnetLogloss",
                                      "UpsamplingLogReg", "UpsamplingEnetRoc", 
                                      "UpsamplingEnetLogloss")
    
    # Set row names of the matrix to coefficient names
    rownames(predictors_results) <- rownames(enet_roc_pred)
    
    # Return the matrix
    return(predictors_results)
  }, simplify = "array")
}



######################################################
############# Analysis balanced accuracy #############
######################################################
# Function to calculate balanced accuracy for all models from the data, 
# requires the data to be a list of datasets from a single combination of 
# simulation factors

data.balanced.accuracy <- function(sample_data, warnings, noise, condition){
  # Loops over all datasets
  total_results_predictors <- sapply(sample_data, FUN = function(current_sample){
    # Checks if warning happened in less than 50% of datasets for current 
    # condition for LR
    if(warnings["LogReg", condition, noise]< .5){
      if(names(current_sample$LogReg[1]) == "coefficients"){
        logreg_train <- caret::confusionMatrix(
          current_sample$LogReg$confusionMatrixTraining, 
          positive = "1")$byClass["Balanced Accuracy"]
        logreg_val <- caret::confusionMatrix(
          current_sample$LogReg$confusionMatrixvalidation, 
          positive = "1")$byClass["Balanced Accuracy"]
      } else{
        logreg_train <- NA
        logreg_val <- NA
      }
    } else{
      logreg_train <- NA
      logreg_val <- NA
    }
    
    enet_roc_train <- caret::confusionMatrix(
      current_sample$ElasticNetRoc$confusionMatrixTraining, 
      positive = "ONE")$byClass["Balanced Accuracy"]
    enet_roc_val <- caret::confusionMatrix(
      current_sample$ElasticNetRoc$confusionMatrixvalidation, 
      positive = "1")$byClass["Balanced Accuracy"]
    
    enet_ll_train <- caret::confusionMatrix(
      current_sample$ElasticNetLogloss$confusionMatrixTraining, 
      positive = "ONE")$byClass["Balanced Accuracy"]
    enet_ll_val <- caret::confusionMatrix(
      current_sample$ElasticNetLogloss$confusionMatrixvalidation, 
      positive = "1")$byClass["Balanced Accuracy"]
    # Checks if warning happened in less than 50% of datasets for current 
    # condition for LR with upsampling
    if(warnings["UpsamplingLogReg", condition, noise]< .5){
      if(names(current_sample$UpsamplingLogReg[1]) == "coefficients"){
        upsamp_logreg_train <- caret::confusionMatrix(
          current_sample$UpsamplingLogReg$confusionMatrixTraining, 
          positive = "1")$byClass["Balanced Accuracy"]
        upsamp_logreg_val <- caret::confusionMatrix(
          current_sample$UpsamplingLogReg$confusionMatrixvalidation, 
          positive = "1")$byClass["Balanced Accuracy"]
      } else{
        upsamp_logreg_train <- NA
        upsamp_logreg_val <- NA
      }
    } else{
      upsamp_logreg_train <- NA
      upsamp_logreg_val <- NA
    }
    
    upsamp_enet_roc_train <- caret::confusionMatrix(
      current_sample$UpsamplingElasticNetRoc$confusionMatrixTraining, 
      positive = "ONE")$byClass["Balanced Accuracy"]
    upsamp_enet_roc_val <- caret::confusionMatrix(
      current_sample$UpsamplingElasticNetRoc$confusionMatrixvalidation, 
      positive = "1")$byClass["Balanced Accuracy"]
    
    upsamp_enet_ll_train <- caret::confusionMatrix(
      current_sample$UpsamplingElasticNetLogloss$confusionMatrixTraining, 
      positive = "ONE")$byClass["Balanced Accuracy"]
    upsamp_enet_ll_val <- caret::confusionMatrix(
      current_sample$UpsamplingElasticNetLogloss$confusionMatrixvalidation, 
      positive = "1")$byClass["Balanced Accuracy"]
    
    train_results <- c(logreg_train, enet_roc_train, enet_ll_train, 
                       upsamp_logreg_train, upsamp_enet_roc_train, 
                       upsamp_enet_ll_train)
    
    validation_results <- c(logreg_val, enet_roc_val, enet_ll_val, 
                            upsamp_logreg_val, upsamp_enet_roc_val, 
                            upsamp_enet_ll_val)
    
    total_results <- rbind(train_results, validation_results)
    
    colnames(total_results) <- c("LogReg","EnetRoc", "EnetLogloss",
                                 "UpsamplingLogReg", "UpsamplingEnetRoc", 
                                 "UpsamplingEnetLogloss")
    rownames(total_results) <- c("Train","Validation")
    
    return(total_results)
  }, simplify = "array")
}



########################################################
############# Analysis performance metrics #############
########################################################

data.performance.metrics <- function(sample_data, warnings, noise, condition){
  total_results_predictors <- sapply(sample_data, FUN = function(current_sample){
    # Checks if warning happened in less than 50% of datasets for current 
    # condition for LR
    if(warnings["LogReg", condition, noise]< .5){
      if(names(current_sample$LogReg[1]) == "coefficients"){
        logreg <- as.data.frame(current_sample$LogReg$performanceMetrics)
        logreg <- add_column(logreg, "logLossModel" = NA, .after = 2)
      } else{
        logreg <- matrix(rep(NA, 8), ncol = 4)
      }
    } else{
      logreg <- matrix(rep(NA, 8), ncol = 4)
    }
    enet_roc <- current_sample$ElasticNetRoc$performanceMetrics
    
    enet_ll <- current_sample$ElasticNetLogloss$performanceMetrics
    # Checks if warning happened in less than 50% of datasets for current 
    # condition for LR with upsampling
    if(warnings["UpsamplingLogReg", condition, noise] < .5){
      if(names(current_sample$UpsamplingLogReg[1]) == "coefficients"){
        upsamp_logreg <- as.data.frame(current_sample$UpsamplingLogReg$performanceMetrics)
        upsamp_logreg <- add_column(upsamp_logreg, "logLossModel" = NA, .after = 2)
      } else{
        upsamp_logreg <- matrix(rep(NA, 8), ncol = 4)
      }
    } else{
      upsamp_logreg <- matrix(rep(NA, 8), ncol = 4)
    }
    
    upsamp_enet_roc <- current_sample$UpsamplingElasticNetRoc$performanceMetrics
    
    upsamp_enet_ll <- current_sample$UpsamplingElasticNetLogloss$performanceMetrics
    
    total_results <- abind(logreg, enet_roc, enet_ll, upsamp_logreg, 
                           upsamp_enet_roc, upsamp_enet_ll, along = 3)
    
    dimnames(total_results)[[3]] <- c("LogReg","EnetRoc", "EnetLogloss",
                                      "UpsamplingLogReg", "UpsamplingEnetRoc", 
                                      "UpsamplingEnetLogloss")
    
    return(total_results)
  }, simplify = "array")
}
