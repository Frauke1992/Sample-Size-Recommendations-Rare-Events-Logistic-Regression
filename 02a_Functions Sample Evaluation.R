##### Load neccessary packages #####
library(caret) # for logistic regression calculation
library(glmnet) # used for elastic net
library(dplyr)
library(pROC) # to calculate AUC
library(ModelMetrics) # to calculate LogLoss
library(gbm) # for gradient boosting machine

####################################################
###### Function for LR without regularization ######
####################################################
output.glm <- function(train_data, validation_data, upsampling, event_frac = 0.5, model = NULL){
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
                                      verboseIter = FALSE, # no output during training
                                      # Summary function that computes sensitivity, 
                                      # specificity and area under ROC curve
                                      summaryFunction = twoClassSummary, 
                                      # upsampling to resolve class imbalance or none
                                      sampling = upsampling)
  
  if(is.null(model)){
    model <- Y ~ (. + .)^2
  } 
  
  # train model
  fit_normal <- train(form = model, 
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
  
  coeff_normal <- coef(summary(fit_normal))
  
  # calculate predicted probabilities in training sample
  predicted_prob_train <- predict(fit_normal, train_data[-1], type = "prob")
  auc_train <- auc(train_data$Y, predicted_prob_train[,2])
  logloss_train <- mlogLoss(train_data_caret$Y, predicted_prob_train)
  
  # get predicted probabilities for validation sample
  predicted_prob_validation <- predict(fit_normal, validation_data[-1], type = "prob" )
  auc_validation <- auc(validation_data$Y, predicted_prob_validation[,2])
  logloss_validation <- mlogLoss(validation_data_factor, predicted_prob_validation)
  
  # Prepare output lists for each event_frac value
  misclassification_train <- numeric(length(event_frac))
  misclassification_validation <- numeric(length(event_frac))
  balanced_acc_train <- numeric(length(event_frac))
  balanced_acc_validation <- numeric(length(event_frac))
  confusion_train_normal <- vector("list", length(event_frac))
  confusion_validation_normal <- vector("list", length(event_frac))
  names(confusion_train_normal) <- paste0("event_frac_", event_frac)
  names(confusion_validation_normal) <- paste0("event_frac_", event_frac)
  performance_metrics <- matrix(NA, nrow = 2 * length(event_frac), ncol = 4)
  rownames(performance_metrics) <- as.vector(outer(c("train", "validation"), paste0("_", event_frac), paste0))
  colnames(performance_metrics) <- c("auc", "misclassification", "logloss", "balanced_accuracy")
  
  # Loop over all event_frac values
  for(i in seq_along(event_frac)) {
    frac <- event_frac[i]
    
    # assign the predicted probabilities to 0 and 1 class according to event_frac threshold
    predicted_class_train <- as.integer(predicted_prob_train[,2] >= frac)
    predicted_class_validation <- as.integer(predicted_prob_validation[,2] >= frac)
    
    # calculate misclassification error
    misclassification_train[i] <- mean(predicted_class_train != train_data$Y)
    misclassification_validation[i] <- mean(predicted_class_validation != validation_data$Y)
    
    # calculate balanced accuracy
    balanced_acc_train[i] <- balanced_accuracy(train_data$Y, predicted_class_train)
    balanced_acc_validation[i] <- balanced_accuracy(validation_data$Y, predicted_class_validation)
    
    # create confusion matrices
    confusion_train_normal[[i]] <- as.matrix(caret::confusionMatrix(
      data = as.factor(predicted_class_train),
      reference = as.factor(train_data$Y),
      positive = "1")$table)
    confusion_validation_normal[[i]] <- as.matrix(caret::confusionMatrix(
      data = as.factor(predicted_class_validation),
      reference = as.factor(validation_data$Y),
      positive = "1")$table)
    
    # performance metrics (auc and logloss are threshold-invariant)
    performance_metrics[2*i-1, ] <- c(auc_train, misclassification_train[i], logloss_train, balanced_acc_train[i])
    performance_metrics[2*i,   ] <- c(auc_validation, misclassification_validation[i], logloss_validation, balanced_acc_validation[i])
    rownames(performance_metrics)[2*i-1] <- paste0("train_", frac)
    rownames(performance_metrics)[2*i]   <- paste0("validation_", frac)
  }
  
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
output.enet <- function(train_data, validation_data, upsampling, summarytype, event_frac = 0.5,
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
                                    sampling = upsampling,
                                    selectionFunction = "oneSE") 
  # train model for area under ROC curve
  fit_enet <- train(Y ~ (. + .)^2,# any given x and its two-way interactions with other 
                    # potential predictors can go into the model
                    data = train_data_caret,
                    # trainCOntrol object containing the specifications above
                    trControl = trainControl_enet,
                    method = "glmnet", 
                    # dichotomous outcomes, family selects logistic regression
                    family = "binomial", 
                    verbose = FALSE,
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
  
  # calculate AUC
  if(metrictype == "ROC"){
    auc_train_enet <- subset(fit_enet$results,
                             alpha== fit_enet$bestTune$alpha &
                               lambda == fit_enet$bestTune$lambda)$ROC
    bestModel_lines <- apply(fit_enet$pred[,c("alpha","lambda")], 1, function(x){all(x == fit_enet$bestTune)})
    # calculate logloss
    logloss_train_enet_calc <- mlogLoss(fit_enet$pred$obs[bestModel_lines],
                                        fit_enet$pred[bestModel_lines, c( "ZERO", "ONE")])
    logloss_train_enet <- NA
  } else if(metrictype == "logLoss"){
    # extract logloss
    logloss_train_enet <- subset(fit_enet$results,
                                 alpha== fit_enet$bestTune$alpha &
                                   lambda == fit_enet$bestTune$lambda)$logLoss
    bestModel_lines <- apply(fit_enet$pred[,c("alpha","lambda")], 1, function(x){all(x == fit_enet$bestTune)})
    # calculate logloss
    logloss_train_enet_calc <- mlogLoss(fit_enet$pred$obs[bestModel_lines],
                                        fit_enet$pred[bestModel_lines, c( "ZERO", "ONE")])
    # calculate AUC
    auc_train_enet <- auc(train_data$Y, predicted_prob_train_enet)
  }
  
  # get predicted probabilities for validation sample
  predicted_prob_validation_enet <- predict(fit_enet, validation_data[-1],
                                            type = "prob" )
  auc_validation_enet <- auc(validation_data$Y, predicted_prob_validation_enet[,2])
  logloss_validation_enet_calc <- mlogLoss(validation_data_factor, predicted_prob_validation_enet)
  logloss_validation_enet <- NA
  
  # Prepare output for multiple event_frac thresholds
  misclassification_train_enet <- numeric(length(event_frac))
  misclassification_validation_enet <- numeric(length(event_frac))
  balanced_acc_train_enet <- numeric(length(event_frac))
  balanced_acc_validation_enet <- numeric(length(event_frac))
  confusion_train_enet <- vector("list", length(event_frac))
  confusion_validation_enet <- vector("list", length(event_frac))
  names(confusion_train_enet) <- paste0("event_frac_", event_frac)
  names(confusion_validation_enet) <- paste0("event_frac_", event_frac)
  
  # performance metrics matrix
  performance_metrics_enet <- matrix(NA, nrow = 2 * length(event_frac), ncol = 5)
  rownames(performance_metrics_enet) <- as.vector(outer(c("train", "validation"), paste0("_", event_frac), paste0))
  colnames(performance_metrics_enet) <- c("auc", "misclassification", 
                                          "logLossModel", "logLossCalculated", "balanced_accuracy")
  
  for(i in seq_along(event_frac)) {
    frac <- event_frac[i]
    
    # assign the predicted probabilities to 0 and 1 class according to event_frac threshold
    predicted_class_train_enet <- as.integer(predicted_prob_train_enet >= frac)
    predicted_class_validation_enet <- as.integer(predicted_prob_validation_enet[,2] >= frac)
    
    # calculate misclassification error
    misclassification_train_enet[i] <- mean(predicted_class_train_enet != train_data$Y)
    misclassification_validation_enet[i] <- mean(predicted_class_validation_enet != validation_data$Y)
    
    # calculate balanced accuracy
    balanced_acc_train_enet[i] <- balanced_accuracy(train_data$Y, predicted_class_train_enet)
    balanced_acc_validation_enet[i] <- balanced_accuracy(validation_data$Y, predicted_class_validation_enet)
    
    # create confusion matrices
    confusion_train_enet[[i]] <- as.matrix(caret::confusionMatrix(
      data = as.factor(predicted_class_train_enet),
      reference = as.factor(train_data$Y),
      positive = "1")$table)
    confusion_validation_enet[[i]] <- as.matrix(caret::confusionMatrix(
      data = as.factor(predicted_class_validation_enet),
      reference = as.factor(validation_data$Y),
      positive = "1")$table)
    
    # performance metrics (auc, logloss model and calc are threshold-invariant)
    performance_metrics_enet[2*i-1, ] <- c(auc_train_enet, 
                                           misclassification_train_enet[i], 
                                           logloss_train_enet, 
                                           logloss_train_enet_calc,
                                           balanced_acc_train_enet[i])
    performance_metrics_enet[2*i, ] <- c(auc_validation_enet,
                                         misclassification_validation_enet[i], 
                                         logloss_validation_enet,
                                         logloss_validation_enet_calc,
                                         balanced_acc_validation_enet[i])
    rownames(performance_metrics_enet)[2*i-1] <- paste0("train_", frac)
    rownames(performance_metrics_enet)[2*i] <- paste0("validation_", frac)
  }
  
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


##################################################
###### Function for Gradien Boosting Machine #####
##################################################

output.gbm <- function(train_data, validation_data, upsampling, summarytype, event_frac = 0.5, 
                       metrictype){
  
  train_data_caret <- train_data
  # change y to factor
  train_data_caret$Y <- as.factor(train_data_caret$Y)
  validation_data_factor <- as.factor(validation_data$Y)
  
  # set factor levels
  levels(train_data_caret$Y) <- c("ZERO","ONE")
  levels(validation_data_factor) <- c("ZERO","ONE")
  
  grid_gbm <- expand.grid(
    interaction.depth = c(1,2,3), # tree depth (= max_depth in xgboost)
    n.minobsinnode = c(5, 10),    # end node size (= min_child_weight in xgboost)
    n.trees = c(50,100,150),      # max number of trees (= nTrees in xgboost)
    shrinkage = seq(.051, .201, .05)) # shrinkage/learning rate (= eta in xgboost)
  
  trainControl_gbm <- trainControl(method = "cv", # cross validation
                                   # number of folds for cross validation
                                   number = 10,
                                   # class probabilities are computed and saved
                                   classProbs = TRUE,
                                   # all prediction values are saved
                                   savePredictions = TRUE,
                                   verboseIter = FALSE, # no output during training
                                   returnData = TRUE, # saves the data
                                   # specified summarytype given to the function
                                   summaryFunction = summarytype,
                                   # upsampling to resolve class imbalance or none
                                   sampling = upsampling,
                                   selectionFunction = "oneSE")
  
  # train model for area under ROC curve
  fit_gbm <- train(Y ~ .,# any given x and its two-way interactions with other
                   # potential predictors can go into the model
                   data = train_data_caret,
                   # trainControl object containing the specifications above
                   trControl = trainControl_gbm,
                   method = "gbm",
                   verbose = FALSE,
                   metric = metrictype,# specified performance metric
                   tuneGrid = grid_gbm) # set tunelength to 20
  
  #### Analysis of training data ####
  # extract predicted probabilities in training sample
  predicted_gbm <- subset(fit_gbm$pred, # choose the predicted values
                          n.trees == fit_gbm$bestTune$n.trees & #for the best alpha
                            interaction.depth == fit_gbm$bestTune$interaction.depth &
                            shrinkage == fit_gbm$bestTune$shrinkage &
                            n.minobsinnode == fit_gbm$bestTune$n.minobsinnode) # and the best lambda
  
  
  predicted_sorted <- arrange(predicted_gbm, rowIndex)
  # sort data according to rowIndex in order to have rows match the rows of the
  # original data
  
  predicted_prob_train_gbm <- predicted_sorted$ONE # select only the probabilities
  
  # calculate AUC
  if(metrictype == "ROC"){
    auc_train_gbm <- subset(fit_gbm$results,
                            n.trees == fit_gbm$bestTune$n.trees & #for the best alpha
                              interaction.depth == fit_gbm$bestTune$interaction.depth &
                              shrinkage == fit_gbm$bestTune$shrinkage &
                              n.minobsinnode == fit_gbm$bestTune$n.minobsinnode)$ROC
    # calculate logloss 
    bestModel_lines <- apply(fit_gbm$pred[,c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode")], 1, function(x){all(x == fit_gbm$bestTune)})
    logloss_train_gbm_calc <- mlogLoss(fit_gbm$pred$obs[bestModel_lines],
                                       fit_gbm$pred[bestModel_lines, c( "ZERO", "ONE")])
    logloss_train_gbm <- NA
  } else if(metrictype == "logLoss"){
    # extract logloss
    logloss_train_gbm <- subset(fit_gbm$results,
                                n.trees == fit_gbm$bestTune$n.trees & #for the best alpha
                                  interaction.depth == fit_gbm$bestTune$interaction.depth &
                                  shrinkage == fit_gbm$bestTune$shrinkage &
                                  n.minobsinnode == fit_gbm$bestTune$n.minobsinnode)$logLoss
    # calculate logloss 
    bestModel_lines <- apply(fit_gbm$pred[,c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode")], 1, function(x){all(x == fit_gbm$bestTune)})
    logloss_train_gbm_calc <- mlogLoss(fit_gbm$pred$obs[bestModel_lines],
                                       fit_gbm$pred[bestModel_lines, c( "ZERO", "ONE")])
    # calculate AUC
    auc_train_gbm <- auc(train_data$Y, predicted_prob_train_gbm)
  }
  
  # get predicted probabilities for validation sample
  predicted_prob_validation_gbm <- predict(fit_gbm, validation_data[-1],
                                           type = "prob" )
  auc_validation_gbm <- auc(validation_data$Y,
                            predicted_prob_validation_gbm[,2])
  logloss_validation_gbm_calc <- mlogLoss(validation_data_factor,
                                          predicted_prob_validation_gbm)
  logloss_validation_gbm <- NA
  
  # Prepare output for multiple event_frac thresholds
  misclassification_train_gbm <- numeric(length(event_frac))
  misclassification_validation_gbm <- numeric(length(event_frac))
  balanced_acc_train_gbm <- numeric(length(event_frac))
  balanced_acc_validation_gbm <- numeric(length(event_frac))
  confusion_train_gbm <- vector("list", length(event_frac))
  confusion_validation_gbm <- vector("list", length(event_frac))
  names(confusion_train_gbm) <- paste0("event_frac_", event_frac)
  names(confusion_validation_gbm) <- paste0("event_frac_", event_frac)
  
  # performance metrics matrix
  performance_metrics_gbm <- matrix(NA, nrow = 2 * length(event_frac), ncol = 5)
  rownames(performance_metrics_gbm) <- as.vector(outer(c("train", "validation"), paste0("_", event_frac), paste0))
  colnames(performance_metrics_gbm) <- c("auc", "misclassification",
                                         "logLossModel", "logLossCalculated", "balanced_accuracy")
  
  for(i in seq_along(event_frac)) {
    frac <- event_frac[i]
    
    # assign the predicted probabilities to 0 and 1 class according to event_frac threshold
    predicted_class_train_gbm <- as.integer(predicted_prob_train_gbm >= frac)
    predicted_class_validation_gbm <- as.integer(predicted_prob_validation_gbm[,2] >= frac)
    
    # calculate misclassification error
    misclassification_train_gbm[i] <- mean(predicted_class_train_gbm != train_data$Y)
    misclassification_validation_gbm[i] <- mean(predicted_class_validation_gbm != validation_data$Y)
    
    # calculate balanced accuracy
    balanced_acc_train_gbm[i] <- balanced_accuracy(train_data$Y, predicted_class_train_gbm)
    balanced_acc_validation_gbm[i] <- balanced_accuracy(validation_data$Y, predicted_class_validation_gbm)
    
    # create confusion matrices
    confusion_train_gbm[[i]] <- as.matrix(caret::confusionMatrix(
      data = as.factor(predicted_class_train_gbm),
      reference = as.factor(train_data$Y),
      positive = "1")$table)
    confusion_validation_gbm[[i]] <- as.matrix(caret::confusionMatrix(
      data = as.factor(predicted_class_validation_gbm),
      reference = as.factor(validation_data$Y),
      positive = "1")$table)
    
    # performance metrics (auc, logloss model and calc are threshold-invariant)
    performance_metrics_gbm[2*i-1, ] <- c(auc_train_gbm,
                                          misclassification_train_gbm[i],
                                          logloss_train_gbm,
                                          logloss_train_gbm_calc,
                                          balanced_acc_train_gbm[i])
    performance_metrics_gbm[2*i, ] <- c(auc_validation_gbm,
                                        misclassification_validation_gbm[i],
                                        logloss_validation_gbm,
                                        logloss_validation_gbm_calc,
                                        balanced_acc_validation_gbm[i])
    rownames(performance_metrics_gbm)[2*i-1] <- paste0("train_", frac)
    rownames(performance_metrics_gbm)[2*i]   <- paste0("validation_", frac)
  }
  
  # Extract variable importances from gbm
  varImp_gbm <- varImp(fit_gbm)
  
  # create list with outputs
  results_gbm <- list(varImp_gbm$importance,
                      predicted_prob_validation_gbm[2],
                      performance_metrics_gbm,
                      confusion_train_gbm,
                      confusion_validation_gbm,
                      fit_gbm$bestTune)
  # name sublists
  names(results_gbm) <- c("variableImportances", "predictedProbabilities",
                          "performanceMetrics", "confusionMatrixTraining",
                          "confusionMatrixvalidation",
                          "TuningParameters")
  
  return(results_gbm)
}

# Hilfsfunktion für balanced accuracy
balanced_accuracy <- function(true, predicted) {
  TP <- sum(true == 1 & predicted == 1)
  TN <- sum(true == 0 & predicted == 0)
  FP <- sum(true == 0 & predicted == 1)
  FN <- sum(true == 1 & predicted == 0)
  sens <- if ((TP + FN) > 0) TP / (TP + FN) else NA
  spec <- if ((TN + FP) > 0) TN / (TN + FP) else NA
  mean(c(sens, spec), na.rm = TRUE)
}
