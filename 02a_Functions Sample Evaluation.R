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
output.glm <- function(train_data, validation_data, upsampling,
                       event_frac = 0.5, model = NULL) {
  # 1) Y als Factor (für caret)
  train_data_caret <- train_data
  train_data_caret$Y <- as.factor(train_data_caret$Y)
  validation_data_factor <- as.factor(validation_data$Y)
  levels(train_data_caret$Y)   <- c("ZERO","ONE")
  levels(validation_data_factor) <- c("ZERO","ONE")
  
  # 2) trainControl mit ROC, Sensitivity, Specificity
  library(caret)
  trainControl_normal <- trainControl(
    method           = "none",
    classProbs       = TRUE,
    savePredictions  = TRUE,
    returnData       = TRUE,
    verboseIter      = FALSE,
    summaryFunction  = twoClassSummary,
    sampling         = upsampling
  )
  
  if (is.null(model)) {
    model <- Y ~ (. + .)^2
  }
  
  # 3) Modell fitten
  fit_normal <- train(
    form     = model,
    data     = train_data_caret,
    trControl= trainControl_normal,
    method   = "glm",
    family   = "binomial",
    metric   = "ROC"
  )
  
  # 4) Basis-Metriken (train & validation)
  predicted_prob_train      <- predict(fit_normal, train_data[-1],     type = "prob")
  auc_train                 <- auc(train_data$Y, predicted_prob_train[,2])
  logloss_train             <- mlogLoss(train_data_caret$Y, predicted_prob_train)
  
  predicted_prob_validation <- predict(fit_normal, validation_data[-1], type = "prob")
  auc_validation            <- auc(validation_data$Y, predicted_prob_validation[,2])
  logloss_validation        <- mlogLoss(validation_data_factor, predicted_prob_validation)
  
  # 5) Vorbereitung der Vektoren und Matrix für alle Thresholds
  n_thresh <- length(event_frac)
  misclass_train    <- numeric(n_thresh)
  misclass_val      <- numeric(n_thresh)
  balacc_train      <- numeric(n_thresh)
  balacc_val        <- numeric(n_thresh)
  sens_train        <- numeric(n_thresh)
  spec_train        <- numeric(n_thresh)
  sens_val          <- numeric(n_thresh)
  spec_val          <- numeric(n_thresh)
  
  confusion_train   <- vector("list", n_thresh)
  confusion_val     <- vector("list", n_thresh)
  names(confusion_train) <- paste0("event_frac_", event_frac)
  names(confusion_val)   <- paste0("event_frac_", event_frac)
  
  performance_metrics <- matrix(
    NA,
    nrow = 2 * n_thresh,
    ncol = 6,
    dimnames = list(
      as.vector(outer(c("train","validation"), paste0("_", event_frac), paste0)),
      c("auc","misclassification","logloss","balanced_accuracy","sensitivity","specificity")
    )
  )
  
  # 6) Schleife über alle event_frac-Werte
  for (i in seq_along(event_frac)) {
    frac <- event_frac[i]
    
    # 6.1 Klassenvorhersagen
    pred_class_train <- as.factor(as.integer(predicted_prob_train[,2] >= frac))
    pred_class_val   <- as.factor(as.integer(predicted_prob_validation[,2] >= frac))
    levels(pred_class_train) <- levels(train_data_caret$Y)
    levels(pred_class_val)   <- levels(train_data_caret$Y)
    
    # 6.2 Misclassification
    misclass_train[i] <- mean(pred_class_train != train_data_caret$Y)
    misclass_val[i]   <- mean(pred_class_val   != validation_data_factor)
    
    # 6.3 Confusion Matrices + Sensitivity/Specificity/Balanced Accuracy
    cm_train <- caret::confusionMatrix(
      data     = pred_class_train,
      reference= train_data_caret$Y,
      positive = "ONE"
    )
    cm_val   <- caret::confusionMatrix(
      data     = pred_class_val,
      reference= validation_data_factor,
      positive = "ONE"
    )
    confusion_train[[i]] <- cm_train$table
    confusion_val[[i]]   <- cm_val$table
    
    sens_train[i] <- cm_train$byClass["Sensitivity"]
    spec_train[i] <- cm_train$byClass["Specificity"]
    sens_val[i]   <- cm_val$byClass["Sensitivity"]
    spec_val[i]   <- cm_val$byClass["Specificity"]
    
    balacc_train[i] <- cm_train$byClass["Balanced Accuracy"]
    balacc_val[i]   <- cm_val$byClass["Balanced Accuracy"]
    
    # 6.4 Results in performance_metrics
    performance_metrics[2*i-1, ] <- c(
      auc_train, misclass_train[i], logloss_train, balacc_train[i],
      sens_train[i], spec_train[i]
    )
    performance_metrics[2*i, ]   <- c(
      auc_validation, misclass_val[i], logloss_validation, balacc_val[i],
      sens_val[i], spec_val[i]
    )
  }
  
  # 7) Ausgabe
  results_normal <- list(
    coefficients            = coef(summary(fit_normal)),
    predictedProbabilities  = predicted_prob_validation,
    performanceMetrics      = performance_metrics,
    confusionMatrixTraining = confusion_train,
    confusionMatrixValidation = confusion_val
  )
  return(results_normal)
}

#####################################
###### Function for ElasticNet ######
#####################################
output.enet <- function(train_data, validation_data, upsampling,
                        summarytype, event_frac = 0.5, metrictype) {
  # 1) Y als Factor
  train_data_caret <- train_data
  train_data_caret$Y <- as.factor(train_data_caret$Y)
  validation_data_factor <- as.factor(validation_data$Y)
  levels(train_data_caret$Y) <- c("ZERO", "ONE")
  levels(validation_data_factor) <- c("ZERO", "ONE")
  
  # 2) trainControl mit CV und classProbs
  trainControl_enet <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    savePredictions = TRUE,
    returnData = TRUE,
    summaryFunction = summarytype,
    sampling = upsampling,
    selectionFunction = "oneSE"
  )
  
  # 3) Fit glmnet
  fit_enet <- train(
    Y ~ (. + .)^2,
    data = train_data_caret,
    trControl = trainControl_enet,
    method = "glmnet",
    family = "binomial",
    metric = metrictype,
    tuneLength = 20,
    verbose = FALSE
  )
  
  # 4) Koeffizienten und Basis-Prädiktionen
  coeff_enet <- as.matrix(coef(
    fit_enet$finalModel,
    fit_enet$finalModel$tuneValue$lambda
  ))
  
  pred_df <- subset(fit_enet$pred,
                    alpha == fit_enet$bestTune$alpha &
                      lambda == fit_enet$bestTune$lambda)
  pred_sorted <- arrange(pred_df, rowIndex)
  prob_train <- pred_sorted$ONE
  

  # AUC und LogLoss train
  if (metrictype == "ROC") {
    auc_train_enet <- with(fit_enet$results,
                           ROC[alpha == fit_enet$bestTune$alpha &
                                 lambda == fit_enet$bestTune$lambda])
    logloss_train_calc <- mlogLoss(pred_df$obs[bml <-
                                                 apply(pred_df[,c("alpha","lambda")], 1,
                                                       function(x) all(x == fit_enet$bestTune))],
                                   pred_df[bml, c("ZERO","ONE")])
    logloss_train_enet <- NA
  } else if (metrictype == "logLoss") {
    logloss_train_enet <- with(fit_enet$results,
                               logLoss[alpha == fit_enet$bestTune$alpha &
                                         lambda == fit_enet$bestTune$lambda])
    logloss_train_calc <- mlogLoss(pred_df$obs[bml <-
                                                 apply(pred_df[,c("alpha","lambda")], 1,
                                                       function(x) all(x == fit_enet$bestTune))],
                                   pred_df[bml, c("ZERO","ONE")])
    auc_train_enet <- auc(train_data$Y, prob_train)
  }
  
  # 5) Validierungsdaten
  prob_val_df <- predict(fit_enet, validation_data[-1], type = "prob")
  auc_validation_enet <- auc(validation_data$Y, prob_val_df[,2])
  logloss_validation_calc <- mlogLoss(validation_data_factor, prob_val_df)
  logloss_validation_enet <- NA
  
  # 6) Vorbereitung Threshold-Schleife
  n_thresh <- length(event_frac)
  misclass_train <- numeric(n_thresh)
  misclass_val   <- numeric(n_thresh)
  sens_train     <- numeric(n_thresh)
  spec_train     <- numeric(n_thresh)
  balacc_train   <- numeric(n_thresh)
  sens_val       <- numeric(n_thresh)
  spec_val       <- numeric(n_thresh)
  balacc_val     <- numeric(n_thresh)
  
  confusion_train <- vector("list", n_thresh)
  confusion_val   <- vector("list", n_thresh)
  names(confusion_train) <- paste0("event_frac_", event_frac)
  names(confusion_val)   <- paste0("event_frac_", event_frac)
  
  performance_metrics_enet <- matrix(
    NA,
    nrow = 2 * n_thresh,
    ncol = 7,
    dimnames = list(
      as.vector(outer(c("train","validation"), paste0("_", event_frac), paste0)),
      c("auc", "misclassification", "logLossModel", "logLossCalculated",
        "balanced_accuracy", "sensitivity", "specificity")
    )
  )
  
  
  # 7) Schleife 
  for (i in seq_along(event_frac)) {
    frac <- event_frac[i]
    # Prädiktionen klassifiziert
    class_train <- factor(as.integer(prob_train >= frac), levels = c(0,1), labels = levels(train_data_caret$Y))
    class_val   <- factor(as.integer(prob_val_df[,2] >= frac), levels = c(0,1), labels = levels(train_data_caret$Y))
    
    # Confusion Matrices
    cm_tr <- caret::confusionMatrix(class_train, train_data_caret$Y, positive = "ONE")
    cm_val <- caret::confusionMatrix(class_val, validation_data_factor, positive = "ONE")
    confusion_train[[i]] <- cm_tr$table
    confusion_val[[i]]   <- cm_val$table
    
    # Kennzahlen
    misclass_train[i] <- mean(class_train != train_data_caret$Y)
    misclass_val[i]   <- mean(class_val   != validation_data_factor)
    
    sens_train[i]   <- cm_tr$byClass["Sensitivity"]
    spec_train[i]   <- cm_tr$byClass["Specificity"]
    balacc_train[i] <- cm_tr$byClass["Balanced Accuracy"]
    
    sens_val[i]   <- cm_val$byClass["Sensitivity"]
    spec_val[i]   <- cm_val$byClass["Specificity"]
    balacc_val[i] <- cm_val$byClass["Balanced Accuracy"]
    
    performance_metrics_enet[2*i-1, ] <- c(
      auc_train_enet, misclass_train[i], logloss_train_enet, logloss_train_calc,
      balacc_train[i], sens_train[i], spec_train[i]
    )
    performance_metrics_enet[2*i, ]   <- c(
      auc_validation_enet, misclass_val[i], logloss_validation_enet, logloss_validation_calc,
      balacc_val[i], sens_val[i], spec_val[i]
    )
  }
  
  # 8) Ausgabe
  results_enet <- list(
    coefficients             = coeff_enet,
    predictedProbabilities   = prob_val_df[,2],
    performanceMetrics       = performance_metrics_enet,
    confusionMatrixTraining  = confusion_train,
    confusionMatrixValidation= confusion_val,
    bestAlpha                = fit_enet$bestTune$alpha,
    bestLambda               = fit_enet$bestTune$lambda
  )
  return(results_enet)
}

##################################################
###### Function for Gradien Boosting Machine #####
##################################################

output.gbm <- function(train_data, validation_data, upsampling,
                       summarytype, event_frac = 0.5, metrictype) {
  # 1) Y als Factor
  train_data_caret <- train_data
  train_data_caret$Y <- as.factor(train_data_caret$Y)
  validation_data_factor <- as.factor(validation_data$Y)
  levels(train_data_caret$Y) <- c("ZERO", "ONE")
  levels(validation_data_factor) <- c("ZERO", "ONE")
  
  # 2) Hyperparameter-Grid und trainControl
  grid_gbm <- expand.grid(
    interaction.depth = c(1, 2, 3),
    n.minobsinnode   = c(5, 10),
    n.trees          = c(50, 100, 150),
    shrinkage        = seq(0.051, 0.201, 0.05)
  )
  library(caret)
  trainControl_gbm <- trainControl(
    method           = "cv",
    number           = 10,
    classProbs       = TRUE,
    savePredictions  = TRUE,
    verboseIter      = FALSE,
    returnData       = TRUE,
    summaryFunction  = summarytype,
    sampling         = upsampling,
    selectionFunction= "oneSE"
  )
  
  # 3) Modell fitten
  fit_gbm <- train(
    Y ~ .,
    data     = train_data_caret,
    trControl= trainControl_gbm,
    method   = "gbm",
    metric   = metrictype,
    tuneGrid = grid_gbm,
    verbose  = FALSE
  )
  
  # 4) Trainings-Prädiktionen und Metriken
  pred_df <- subset(fit_gbm$pred,
                    n.trees         == fit_gbm$bestTune$n.trees &
                      interaction.depth == fit_gbm$bestTune$interaction.depth &
                      shrinkage       == fit_gbm$bestTune$shrinkage &
                      n.minobsinnode  == fit_gbm$bestTune$n.minobsinnode)
  pred_sorted <- arrange(pred_df, rowIndex)
  prob_train <- pred_sorted$ONE
  
  if (metrictype == "ROC") {
    auc_train_gbm <- with(fit_gbm$results,
                          ROC[interaction.depth == fit_gbm$bestTune$interaction.depth &
                                n.trees == fit_gbm$bestTune$n.trees &
                                shrinkage == fit_gbm$bestTune$shrinkage &
                                n.minobsinnode == fit_gbm$bestTune$n.minobsinnode])
    best_lines <- apply(pred_df[, c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode")], 1,
                        function(x) all(x == fit_gbm$bestTune))
    logloss_train_calc <- mlogLoss(pred_df$obs[best_lines], pred_df[best_lines, c("ZERO", "ONE")])
    logloss_train_gbm <- NA
  } else {
    logloss_train_gbm <- with(fit_gbm$results,
                              logLoss[interaction.depth == fit_gbm$bestTune$interaction.depth &
                                        n.trees == fit_gbm$bestTune$n.trees &
                                        shrinkage == fit_gbm$bestTune$shrinkage &
                                        n.minobsinnode == fit_gbm$bestTune$n.minobsinnode])
    best_lines <- apply(pred_df[, c("n.trees", "interaction.depth", "shrinkage", "n.minobsinnode")], 1,
                        function(x) all(x == fit_gbm$bestTune))
    logloss_train_calc <- mlogLoss(pred_df$obs[best_lines], pred_df[best_lines, c("ZERO", "ONE")])
    auc_train_gbm <- auc(train_data$Y, prob_train)
  }
  
  # 5) Validierungs-Prädiktionen und Metriken
  prob_val_df <- predict(fit_gbm, validation_data[-1], type = "prob")
  auc_validation_gbm       <- auc(validation_data$Y, prob_val_df[,2])
  logloss_validation_calc  <- mlogLoss(validation_data_factor, prob_val_df)
  logloss_validation_gbm   <- NA
  
  # 6) Vorbereiten der Ergebnisstrukturen
  n_thresh <- length(event_frac)
  misclass_tr <- numeric(n_thresh)
  misclass_val<- numeric(n_thresh)
  sens_tr     <- numeric(n_thresh)
  spec_tr     <- numeric(n_thresh)
  balacc_tr   <- numeric(n_thresh)
  sens_val    <- numeric(n_thresh)
  spec_val    <- numeric(n_thresh)
  balacc_val  <- numeric(n_thresh)
  
  confusion_tr <- vector("list", n_thresh)
  confusion_val <- vector("list", n_thresh)
  names(confusion_tr)  <- paste0("event_frac_", event_frac)
  names(confusion_val) <- paste0("event_frac_", event_frac)
  
  performance_metrics_gbm <- matrix(
    NA, nrow = 2 * n_thresh, ncol = 7,
    dimnames = list(
      as.vector(outer(c("train","validation"), paste0("_", event_frac), paste0)),
      c("auc","misclassification","logLossModel","logLossCalculated",
        "balanced_accuracy","sensitivity","specificity")
    )
  )
  
  # 7) Schleife über thresholds
  for (i in seq_along(event_frac)) {
    frac <- event_frac[i]
    class_tr <- factor(as.integer(prob_train >= frac), levels = c(0,1), labels = levels(train_data_caret$Y))
    class_val<- factor(as.integer(prob_val_df[,2] >= frac), levels = c(0,1), labels = levels(train_data_caret$Y))
    
    cm_tr  <- caret::confusionMatrix(class_tr,  train_data_caret$Y,     positive = "ONE")
    cm_val <- caret::confusionMatrix(class_val, validation_data_factor, positive = "ONE")
    confusion_tr[[i]]  <- cm_tr$table
    confusion_val[[i]] <- cm_val$table
    
    misclass_tr[i] <- mean(class_tr != train_data_caret$Y)
    misclass_val[i]<- mean(class_val != validation_data_factor)
    
    sens_tr[i]   <- cm_tr$byClass["Sensitivity"]
    spec_tr[i]   <- cm_tr$byClass["Specificity"]
    balacc_tr[i] <- cm_tr$byClass["Balanced Accuracy"]
    
    sens_val[i]   <- cm_val$byClass["Sensitivity"]
    spec_val[i]   <- cm_val$byClass["Specificity"]
    balacc_val[i] <- cm_val$byClass["Balanced Accuracy"]
    
    performance_metrics_gbm[2*i-1, ] <- c(
      auc_train_gbm, misclass_tr[i], logloss_train_gbm, logloss_train_calc,
      balacc_tr[i], sens_tr[i], spec_tr[i]
    )
    performance_metrics_gbm[2*i, ]   <- c(
      auc_validation_gbm, misclass_val[i], logloss_validation_gbm, logloss_validation_calc,
      balacc_val[i], sens_val[i], spec_val[i]
    )
  }
  
  # 8) Variable Importance und Ausgabe
  varImp_gbm <- varImp(fit_gbm)
  results_gbm <- list(
    variableImportances    = varImp_gbm$importance,
    predictedProbabilities = prob_val_df[,2],
    performanceMetrics     = performance_metrics_gbm,
    confusionMatrixTraining = confusion_tr,
    confusionMatrixValidation = confusion_val,
    tuningParameters        = fit_gbm$bestTune
  )
  return(results_gbm)
}

