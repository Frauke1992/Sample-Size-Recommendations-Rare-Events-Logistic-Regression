
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
  ### GBM ###
  gbm_output_roc <- try({
    output.gbm(train_data, validation_data, samplingtype,
               summarytype=twoClassSummary, metrictype="ROC")
  }, silent = TRUE)
  
  # if an error occured
  if(class(gbm_output_roc)[1] == "try-error"){
    # stop the loop in case of an unexpected, not yet defined error
    stop("Unerwarteter Fehler gbm")
  }
  
  gbm_output_logloss <- try({
    output.gbm(train_data, validation_data, samplingtype,
               metrictype = "logLoss", summarytype = mnLogLoss)
  }, silent = TRUE)
  # if an error occured
  if(class(gbm_output_logloss)[1] == "try-error"){
    # stop the loop in case of an unexpected, not yet defined error
    stop("Unerwarteter Fehler gbm")
  }
  
  results<-list(glm_output , enet_output_roc, enet_output_logloss, 
                gbm_output_roc, gbm_output_logloss)
  names(results) <- c("LogReg" , "ElasticNetRoc", "ElasticNetLogloss", 
                      "GBMRoc", "GBMLogloss")
  return(results)
  
}
