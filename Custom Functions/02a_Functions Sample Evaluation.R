##### Load neccessary packages #####
library(caret) # for logistic regression calculation
library(glmnet) # used for elastic net
library(dplyr)
library(pROC) # to calculate AUC
library(ModelMetrics) # to calculate LogLoss

####################################################
###### Function for LR without regularization ######
####################################################
output.glm <- function(train_data, validation_data, upsampling){
  # change y to factor
  train_data_caret <- train_data
  train_data_caret$Y <- as.factor(train_data_caret$Y)
  validation_data_factor <- as.factor(validation_data$Y)
  # set factor levels
  level_names <- c("ZERO","ONE")
  levels(train_data_caret$Y) <- level_names
  levels(validation_data_factor) <- level_names
  # set specifications for train function
  trainControl_normal <- trainControl(method = "none", # no cross validation
                                      classProbs = TRUE, # class probabilities are computed and
                                      # saved along with prediction values
                                      savePredictions = TRUE, # all prediction values are saved
                                      returnData = TRUE, # saves the data
                                      # Summary function that computes sensitivity, 
                                      # specificity and area under ROC curve
                                      summaryFunction = twoClassSummary, 
                                      # upsampling to resolve class imbalance or none
                                      sampling = upsampling)
  
  
  # train model
  fit_normal <- train(form = Y ~ (. + .)^2, 
                      # any given x and its two-way interactions with other 
                      # potential predictors can go into the model
                      data = train_data_caret,
                      trControl = trainControl_normal, 
                      # trainControl object containing the specifications above
                      method = "glm", 
                      #dichotomous outcomes, family selects logistic regression
                      family = "binomial", 
                      metric = "ROC") # Area under the ROC curve as performance metric
  # no cross validation, therefore not too important, 
  # which measure is chosen
  
  #### Analysis of training data ####
  # extract coefficients of the fitted logistic regression model
  
  coeff_normal <- matrix(data= c(coef(summary(fit_normal))[, 1], 
                                 coef(summary(fit_normal))[,4]),
                         ncol=2)
  # calculate predicted probabilities in training sample
  predicted_prob_train <- predict(fit_normal, train_data[-1], 
                                  type = "prob")
  
  # assign the predicted probabilities to 0 and 1 class according to
  # threshold of 0.5
  predicted_class_train <- as.matrix(round(predicted_prob_train[,2]))
  
  # calculate AUC
  auc_train <- auc(train_data$Y, predicted_prob_train[,2])
  
  # calculate misclassification error
  misclassification_train <- mean(predicted_class_train != train_data$Y)
  
  
  # calculate logloss
  logloss_train <- mlogLoss(train_data_caret$Y, predicted_prob_train)
  
  # create confusion matrix
  confusion_train_normal <- as.matrix(caret::confusionMatrix(
    data = as.factor(predicted_class_train),
    reference = as.factor(train_data$Y),
    positive = "1")$table)
  
  #### Analysis of Validation data ####
  # get predicted probabilities
  predicted_prob_validation <- predict(fit_normal, validation_data[-1],
                                       type = "prob" )
  
  # assign the predicted probabilities to 0 and 1 class according to
  # threshold of 0.5
  predicted_class_validation <- as.matrix(round(predicted_prob_validation)[2])
  
  # calculate AUC
  auc_validation <- auc(validation_data$Y, predicted_prob_validation[,2])
  
  # calculate misclassification error
  misclassification_validation <- mean(predicted_class_validation
                                       != validation_data$Y)
  
  # get deviance
  logloss_validation <- mlogLoss(validation_data_factor, 
                                 predicted_prob_validation)
  
  # create confusion matrix
  confusion_validation_normal <- as.matrix(caret::confusionMatrix(
    data = as.factor(predicted_class_validation),
    reference = as.factor(validation_data$Y),
    positive = "1")$table)
  
  
  performance_metrics <- rbind(c(auc_train, 
                                 misclassification_train, 
                                 logloss_train),
                               c(auc_validation,
                                 misclassification_validation, 
                                 logloss_validation))
  colnames(performance_metrics) <- c("auc", "misclassification", "logloss")
  rownames(performance_metrics) <- c("train", "validation")
  
  
  results_normal <- list(coeff_normal, predicted_prob_validation, 
                         performance_metrics, confusion_train_normal, 
                         confusion_validation_normal)
  names(results_normal) <- c("coefficients", "predictedProbabilities",
                             "performanceMetrics", "confusionMatrixTraining",
                             "confusionMatrixvalidation")
  
  return(results_normal)
}

#####################################
###### Function for ElasticNet ######
#####################################
output.enet <- function(train_data, validation_data, upsampling, summarytype, 
                        metrictype){
  
  train_data_caret <- train_data
  # change y to factor
  train_data_caret$Y <- as.factor(train_data_caret$Y)
  validation_data_factor <- as.factor(validation_data$Y)
  
  # set factor levels
  levels(train_data_caret$Y) <- c("ZERO","ONE")
  levels(validation_data_factor) <- c("ZERO","ONE")
  
  ##### Area under the ROC curve #####
  # set specifications for train function
  trainControl_enet <- trainControl(method = "cv", # cross validation
                                    # number of folds for cross validation
                                    number = 10, 
                                    # class probabilities are computed and saved 
                                    classProbs = TRUE,
                                    # all prediction values are saved
                                    savePredictions = TRUE, 
                                    returnData = TRUE, # saves the data
                                    # specified summarytype given to the function
                                    summaryFunction = summarytype, 
                                    # upsampling to resolve class imbalance or none
                                    sampling = upsampling) 
  # train model for area under ROC curve
  fit_enet <- train(Y ~ (. + .)^2,# any given x and its two-way interactions with other 
                    # potential predictors can go into the model
                    data = train_data_caret,
                    # trainCOntrol object containing the specifications above
                    trControl = trainControl_enet,
                    method = "glmnet", 
                    # dichotomous outcomes, family selects logistic regression
                    family = "binomial", 
                    metric = metrictype,# specified performance metric
                    tuneLength = 20) # set tunelength to 20
  
  #### Analysis of training data ####
  # extract coefficients of the fitted logistic regression model
  coeff_enet <- as.matrix(coef(fit_enet$finalModel,
                               fit_enet$finalModel$tuneValue$lambda))
  
  # extract predicted probabilities in training sample
  predicted_enet <- subset(fit_enet$pred, # choose the predicted values
                           alpha == fit_enet$bestTune$alpha & #for the best alpha
                             lambda == fit_enet$bestTune$lambda) # and the best lambda
  
  predicted_sorted <- arrange(predicted_enet, rowIndex) 
  # sort data according to rowIndex in order to have rows match the rows of the 
  # original data
  
  predicted_prob_train_enet <- predicted_sorted$ONE # select only the probabilities
  
  # assign the predicted probabilities to 0 and 1 class according to a
  # threshold of 0.5
  predicted_class_train_enet <- round(predicted_prob_train_enet)
  if(metrictype == "ROC"){
    auc_train_enet <- subset(fit_enet$results,
                             alpha== fit_enet$bestTune$alpha &
                               lambda == fit_enet$bestTune$lambda)$ROC
    # calculate logloss
    logloss_train_enet_calc <- mlogLoss(fit_enet$pred$obs, 
                                        fit_enet$pred[c( "ZERO", "ONE")])
    logloss_train_enet <- NA
    
    
  } else if(metrictype == "logLoss"){
    # extract logloss
    logloss_train_enet <- subset(fit_enet$results,
                                 alpha== fit_enet$bestTune$alpha &
                                   lambda == fit_enet$bestTune$lambda)$logLoss
    # calculate logloss
    logloss_train_enet_calc <- mlogLoss(fit_enet$pred$obs, 
                                        fit_enet$pred[c( "ZERO", "ONE")])
    # calculate AUC
    auc_train_enet <- auc(train_data$Y, predicted_prob_train_enet)
  }
  
  
  # calculate misclassification error
  misclassification_train_enet <- mean(predicted_class_train_enet != 
                                         train_data$Y)
  
  
  # create confusion matrix
  confusion_train_enet <- as.matrix(confusionMatrix.train(
    data = fit_enet,
    norm = "none",
    positive = 1)$table)
  
  #### Analysis of Validation data ####
  # get predicted probabilities
  predicted_prob_validation_enet <- predict(fit_enet, validation_data[-1],
                                            type = "prob" )
  
  # assign the predicted probabilities to 0 and 1 class according to
  # threshold of 0.5
  predicted_class_validation_enet <- round(predicted_prob_validation_enet[,2])
  
  # calculate AUC
  auc_validation_enet <- auc(validation_data$Y, 
                             predicted_prob_validation_enet[,2])
  
  # calculate misclassification error
  misclassification_validation_enet <- mean(predicted_class_validation_enet
                                            != validation_data$Y)
  
  # calculate logloss
  logloss_validation_enet_calc <- mlogLoss(validation_data_factor, 
                                           predicted_prob_validation_enet)
  logloss_validation_enet <- NA
  
  # create confusion matrix
  confusion_validation_enet <- as.matrix(caret::confusionMatrix(
    data = as.factor(predicted_class_validation_enet),
    reference = as.factor(validation_data$Y),
    positive = "1")$table)
  
  #### Create Output ####
  # bind performance metrics in matrix
  performance_metrics_enet <- rbind(c(auc_train_enet, 
                                      misclassification_train_enet, 
                                      logloss_train_enet,
                                      logloss_train_enet_calc),
                                    c(auc_validation_enet,
                                      misclassification_validation_enet, 
                                      logloss_validation_enet,
                                      logloss_validation_enet_calc))
  # name columns
  colnames(performance_metrics_enet) <- c("auc", "misclassification", 
                                          "logLossModel", "logLossCalculated")
  # name rows
  rownames(performance_metrics_enet) <- c("train", "validation")
  
  # create list with outputs
  results_enet <- list(coeff_enet, 
                       predicted_prob_validation_enet[2], 
                       performance_metrics_enet, 
                       confusion_train_enet, 
                       confusion_validation_enet,
                       fit_enet$bestTune$alpha,
                       fit_enet$bestTune$lambda)
  # name sublists
  names(results_enet) <- c("coefficients", "predictedProbabilities",
                           "performanceMetrics", "confusionMatrixTraining",
                           "confusionMatrixvalidation",
                           "bestAlpha",
                           "bestLambda")
  
  return(results_enet)
}

##########################################################################
##### Function for analysis and error handling for LR and ElasticNet #####
##########################################################################
results.caret <- function(train_data, validation_data, samplingtype){
  ### Logistic regression without regularization ###
  # convert warning messages into error messages
  options(warn = 2) 
  # create an empty object for error messages
  errmessage <- NA
  # use try-function to handle potential errors produced by the logistic 
  # regression output function, e.g. convergence error
  glm_output <- try({ 
    # run the logistic regression output function
    output.glm(train_data, validation_data, upsampling = samplingtype) 
  }, silent = TRUE) # makes sure error messages are suppressed
  # nested if loop to analyze what, if any, errors occurred during the try-function
  # and handle those errors
  if (class(glm_output)[1] == "try-error"){  # if there is an error
    errmessage <- geterrmessage() # get the error message
    # if the error is that the model didn't converge
    if(grepl("did not converge", errmessage)){ 
      options(warn = -1) # turn off warnings
      # run the function again without the warnings
      glm_output <- try({output.glm(train_data, validation_data, 
                                    upsampling = samplingtype)}, silent = TRUE)
      if(class(glm_output)[1] == "try-error"){  # if there is again an error
        new_error_message <- geterrmessage()
        if(grepl("data must contain some levels that overlap the reference", 
                 new_error_message)){
          glm_output <- new_error_message # saves the error message
          names(glm_output) <- "Error!"
          print(paste0("Error after non-convergence in iteration ", loop_counter ,"! ",
                       new_error_message))
        } else {
          stop(paste0("Unerwarteter Fehler GLM after non-convergence in iteration ", 
                      loop_counter, ": ", errmessage))
        }
      } else {
        glm_output <- append(glm_output, paste("Warnung: ", errmessage), 
                             after = 0) # save the warning on top of the output
        names(glm_output)[1] <- "Non-Convergence!"
      }
      
    }  else if(grepl("adjusted probabilities", errmessage)){  # if the error was 
      # originally a  warning 
      # about fitted probabilities 
      # close to 1 or 0
      options(warn = -1) # turn off warnings
      # run the function again without the warnings
      glm_output <- try({output.glm(train_data, validation_data, 
                                    upsampling = samplingtype)}, silent = TRUE)
      if(class(glm_output)[1] == "try-error"){  # if there is again an error
        new_error_message <- geterrmessage()
        if(grepl("data must contain some levels that overlap the reference", 
                 new_error_message)){
          glm_output <- new_error_message # saves the error message
          names(glm_output) <- "Error!"
          print(paste0("Error after adjusted probs in iteration ", loop_counter ,"! ",
                       new_error_message))
        } else {
          stop(paste0("Unerwarteter Fehler GLM after adjusted probs in iteration ", 
                      loop_counter, ": ", errmessage))
        }
      } else {
        glm_output <- append(glm_output, paste("Warnung: ", errmessage), 
                             after = 0) # save the warning on top of the output
        names(glm_output)[1] <- "Separation!"
      }
      
      
    }  else if(grepl("fitted probabilities", errmessage)){ # if the error was 
      # originally a  warning 
      # about fitted probabilities 
      # close to 1 or 0
      options(warn = -1) # turn off warnings
      # run the function again without the warnings
      glm_output <- try({output.glm(train_data, validation_data, 
                                    upsampling = samplingtype)}, silent = TRUE) 
      if(class(glm_output)[1] == "try-error"){  # if there is again an error
        new_error_message <- geterrmessage()
        if(grepl("data must contain some levels that overlap the reference", 
                 new_error_message)){
          glm_output <- new_error_message # saves the error message
          names(glm_output) <- "Error!"
          print(paste0("Error after fitted probs in iteration ", loop_counter ,"! ", 
                       new_error_message))
        } else {
          stop(paste0("Unerwarteter Fehler GLM after fitted probs in iteration ", 
                      loop_counter, ": ", errmessage))
        }
      } else {
        glm_output <- append(glm_output, paste("Warnung: ", errmessage), 
                             after = 0) # save the warning on top of the output
        names(glm_output)[1] <- "Separation!"
      }
      
    } else if(grepl("rank-deficient fit", errmessage)){   
      # if the error was originally a  warning about a prediction from rank-deficient fit
      options(warn = -1) # turn off warnings
      # run the function again without the warnings
      glm_output <- try({output.glm(train_data, validation_data, 
                                    upsampling = samplingtype)}, silent = TRUE)
      
      if(class(glm_output)[1] == "try-error"){  # if there is again an error
        new_error_message <- geterrmessage()
        if(grepl("data must contain some levels that overlap the reference", 
                 new_error_message)){
          glm_output <- new_error_message # saves the error message
          names(glm_output) <- "Error!"
          print(paste0("Error after rank-deficient fit in iteration ", 
                       loop_counter ,"! ", new_error_message))
        } else {
          stop(paste0("Unerwarteter Fehler GLM after rank-deficient fit in iteration ", 
                      loop_counter, ": ", errmessage))
        }
      } else {
        glm_output <- append(glm_output, paste("Warnung: ", errmessage), 
                             after = 0) # save the warning on top of the output
        names(glm_output)[1] <- "Rank-Deficient Fit!"
      }
      
      
    }
    else{
      # stop the loop in case of an unexpected, not yet defined error
      stop(paste0("Unerwarteter Fehler GLM in iteration ", loop_counter, ": ", 
                  errmessage))} 
  }
  # turn off conversion of warning messages into error messages
  options(warn = 0) 
  
  ### Elastic Net ###
  enet_output_roc <- try({
    output.enet(train_data, validation_data, samplingtype,
                summarytype=twoClassSummary, metrictype="ROC")
  }, silent = TRUE)
  
  # if an error occured
  if(class(enet_output_roc)[1] == "try-error"){
    # stop the loop in case of an unexpected, not yet defined error
    stop("Unerwarteter Fehler ENET")
  }
  
  enet_output_logloss <- try({
    output.enet(train_data, validation_data, samplingtype,
                metrictype = "logLoss", summarytype = mnLogLoss)
  }, silent = TRUE)
  # if an error occured
  if(class(enet_output_logloss)[1] == "try-error"){
    # stop the loop in case of an unexpected, not yet defined error
    stop("Unerwarteter Fehler ENET")
  }
  
  results<-list(glm_output , enet_output_roc, enet_output_logloss)
  names(results) <- c("LogReg" , "ElasticNetRoc", "ElasticNetLogloss")
  return(results)
  
}

