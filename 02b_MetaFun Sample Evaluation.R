
##########################################################################
##### Function for analysis and error handling for LR and ElasticNet #####
##########################################################################
results.caret <- function(train_data, validation_data, samplingtype, oracle_model = NULL){
  ### Logistic regression without regularization ###
  # convert warning messages into error messages
  options(warn = 2) 
  # create an empty object for error messages
  errmessage <- NA
  # use try-function to handle potential errors produced by the logistic 
  # regression output function, e.g. convergence error
  glm_output <- try({ 
    # run the logistic regression output function
    output.glm(train_data, validation_data, upsampling = samplingtype, model = oracle_model) 
  }, silent = TRUE) # makes sure error messages are suppressed
  # nested if loop to analyze what, if any, errors occurred during the try-function
  # and handle those errors
  if (class(glm_output)[1] == "try-error"){  # if there is an error
    errmessage <- geterrmessage() # get the error message
    
    options(warn = -1) # turn off warnings
    # run the function again without the warnings
    glm_output <- try({output.glm(train_data, validation_data, 
                                  upsampling = samplingtype, model = oracle_model)}, silent = TRUE)
    
    # We adapt the SAS criterion to detect separation
    # https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/statug/statug_logistic_details10.htm
    # That is, we declare separation if fitted probabilities = 0/1 occur 
    # AND if any of the standard errors of the coefficients are >= 70
    # this is valid because we simulated standardized predictor variables
    glm_output$fitted_probs <- grepl("glm.fit: fitted probabilities numerically 0 or 1 occurred", errmessage)
    glm_output$large_SEs <- any(glm_output$coefficients[-1, "Std. Error"] >= 70)
    glm_output$separation <- glm_output$fitted_probs & glm_output$large_SEs                        
    glm_output$errmessage <- errmessage
    
  } else {
    
    glm_output$errmessage <- errmessage
    glm_output$fitted_probs <- glm_output$large_SEs <- glm_output$separation <- FALSE
  }
  
  # turn off conversion of warning messages into error messages
  options(warn = 0) 
  # create an empty object for error messages
  errmessage <- NA
  
  ### Elastic Net ###
  enet_output_roc <- try({
    output.enet(train_data, validation_data, samplingtype,
                summarytype=twoClassSummary, metrictype="ROC")
  }, silent = TRUE)
  
  # if an error occured
  if(class(enet_output_roc)[1] == "try-error"){
    errmessage <- geterrmessage() # get the error message
    enet_output_roc$problem = TRUE
    enet_output_roc$errmessage <- errmessage
  }
  # create an empty object for error messages
  errmessage <- NA
  enet_output_logloss <- try({
    output.enet(train_data, validation_data, samplingtype,
                metrictype = "logLoss", summarytype = mnLogLoss)
  }, silent = TRUE)
  # if an error occured
  if(class(enet_output_logloss)[1] == "try-error"){
    errmessage <- geterrmessage() # get the error message
    enet_output_logloss$problem = TRUE
    enet_output_logloss$errmessage <- errmessage
  }
  ### GBM ###
  # create an empty object for error messages
  errmessage <- NA
  gbm_output_roc <- try({
    output.gbm(train_data, validation_data, samplingtype,
               summarytype=twoClassSummary, metrictype="ROC")
  }, silent = TRUE)
  
  # if an error occured
  if(class(gbm_output_roc)[1] == "try-error"){
    # stop the loop in case of an unexpected, not yet defined error
    errmessage <- geterrmessage() # get the error message
    gbm_output_roc$problem = TRUE
    gbm_output_roc$errmessage <- errmessage
  }
  # create an empty object for error messages
  errmessage <- NA
  gbm_output_logloss <- try({
    output.gbm(train_data, validation_data, samplingtype,
               metrictype = "logLoss", summarytype = mnLogLoss)
  }, silent = TRUE)
  # if an error occured
  if(class(gbm_output_logloss)[1] == "try-error"){
    errmessage <- geterrmessage() # get the error message
    gbm_output_logloss$problem = TRUE
    gbm_output_logloss$errmessage <- errmessage
  }
  
  results<-list(glm_output , enet_output_roc, enet_output_logloss, 
                gbm_output_roc, gbm_output_logloss)
  names(results) <- c("LogReg" , "ElasticNetRoc", "ElasticNetLogloss", 
                      "GBMRoc", "GBMLogloss")
  return(results)
  
}
