#### Requires the script "Analysis Functions" ####
#### Requires the folder "Results Data after Model Development" to be downloaded
##### Load neccessary packages #####
library(stringr)
library(dplyr)
library(abind)
library(str2str)
library(caret)

# Input directory to folder containing necessary scripts
directory_script <- "C:/Users/Frauke/Desktop/Masterarbeit/Code"

# Loads the R-script containing the analysis functions
source(paste0(directory_script, "/Analysis functions.R"))

######################################################
############# Data Preparation #############
######################################################
# Set the levels of NV
n_noise <- c (5, 10, 20, 30)


# Input directory to folder containing folder "Results Data after Model Development"
# with results folders
directory_data <- "C:/Users/Frauke/Desktop/Masterarbeit/Results"
# Loop to import evaluation data, cycles through different folders 
# for each number of NV 
results_total <- sapply(n_noise, FUN = function (current_noise){

  # Get list of files containing evaluation data of current number of NV
  file_names <- list.files(path = paste0(directory_data, "/Results Data",
                                         " after Model Development/Results ", 
                                         current_noise, " NV"), 
                           pattern = ".rda")
  # Split the file names at underscores, keeping only the third part of the split
  # to get the name of the condition (combinations of sample size and events fraction)
  condition_name <- str_split_i(file_names, "_", 3)
  # Remove the file ending
  condition_name <- str_sub(condition_name, end = -5)
  
  # Set working directory to the folder with the evaluation data
  setwd(paste0(directory_data, "/Results Data after Model Development/Results ",
               current_noise, " NV"))
  
  # Loop over the files in the folder
  result_data <- lapply(file_names, FUN = function(file_name){
    # Read in current file
    current_file <- readRDS(file_name)
    # Set list names to match number of sample
    names(current_file) <- c(paste0("Sample ", 1:1000))
    # Return the data
    return(current_file)
  })
  
  # Renaming the conditions matching the excel-file of conditions
  names(result_data) <- condition_name
  
  
  # Reordering the data so that data is sorted by sample sizes within events fractions
  new_order <- c(1, 5, 9, 13, 2, 6, 10, 14, 3, 7, 11, 15, 4, 8, 12, 16)
  result_data_rearranged <- result_data[new_order]
  # Return the reordered data
  return(result_data_rearranged)
  
}, simplify = FALSE)

# Set the names of the outer list to the levels of NV
names(results_total) <- n_noise


######################################################


########################################################
############# Analysis errors and warnings #############
########################################################

# Loop to extract warnings in different datasets
# Loops through numbers of NV
warnings_total <- sapply(results_total, FUN = function(result_data_rearranged){
  # Initializes counter for number of combinations of sample size and events fractions
  condition_counter <<- 0
  # Loops through different combinations of sample size and events fraction to
  # extract warnings in each
  warning_results <- sapply(result_data_rearranged, FUN = function(results){
    condition_counter <<- condition_counter + 1
    # initializes counter of the datasets
    sample_counter <<- 0
    warning_results_condition <- data.warnings(results)
    return(warning_results_condition)
  }, simplify = "array")
  
  # Calculates the proportion of warnings in each combination of sample size and 
  # events fraction
  warnings_counted_total <- apply(warning_results, MARGIN = c(1,4), FUN = function(x){
    sum(x)/1000
    
  })
  return(warnings_counted_total)
}, simplify = "array")


#########################################################
############# Analysis true model frequency #############
#########################################################
# Initialize NV counter for the current analysis
nv_counter <- 0

# Loop to extract for each dataset whether the true parameters, true simple and
# interaction effects as well as noise predictors and noise interactions are 
# included in final models
true_model_frequency <- sapply(results_total, FUN = function (result_data_rearranged){
  # raise NV counter by 1
  nv_counter <<- nv_counter + 1
  # extract number of NV from results_total
  n_noise <- names(results_total[nv_counter])
  # Calculate number of predictors
  n_pred <- as.numeric(n_noise) + 3
  # calculate number of regression weights
  n_weights <- (n_pred * (n_pred + 1)) / 2 + 1
  # Initialize counter for number of combinations of sample size and events fractions
  condition_counter <<- 0
  
  # loop through all combinations of sample size and events fractions
  true_model_current_NV<- sapply(result_data_rearranged, FUN = function(results){
    # raise condition counter by 1
    condition_counter <<- condition_counter + 1
    # Extract the current condition name 
    current_condition <- names(result_data_rearranged[condition_counter])
    
    # Run custom function data.predictors to extract coefficient results
    predictor_results <- data.predictors(results, warnings_total, n_noise, current_condition)
    # Check which values are not NA
    predictors_boolean <- apply(predictor_results, MARGIN = c(1, 2,3),
                                FUN = function(current_sample) !is.na(current_sample))
    
    # Set up vector of indices of true parameters
    simulated_predictors <- c(2, 3, 4, n_pred + 2, n_pred + 3, (n_pred * 2 + 1))
    # Separate the values for the true parameters from the rest
    simulated_predictors_boolean <- predictors_boolean[simulated_predictors, ,]
    # Calculate the number of true predictors for each dataset
    number_simulated_predictors<- apply(simulated_predictors_boolean, 
                                        MARGIN = c(2,3),
                                        FUN = function(x) sum(x))
    # Calculate the number of true simple effects for each dataset
    number_simulated_simple_effects <- apply(simulated_predictors_boolean[1:3,,], 
                                             MARGIN = c(2,3), FUN = function(x) sum(x))
    # Calculate the number of true interactions for each dataset
    number_simulated_interactions <- apply(simulated_predictors_boolean[4:6,,], 
                                           MARGIN = c(2,3), FUN = function(x) sum(x))
    
    # Set up vector of indices of noise parameters
    noise_predictors <- c(5:(n_pred + 1))
    # Separate the values for the noise parameters from the rest
    noise_pred_boolean <- predictors_boolean[noise_predictors, , ]
    # Calculate the number of noise predictors for each dataset
    number_noise_predictors <- apply(noise_pred_boolean, MARGIN = c(2,3),
                                     FUN = function(x) sum(x))
    
    # Set up vector of indices of noise interactions
    noise_interactions <- c(c((n_pred + 4):(2 * n_pred)), c((2 * n_pred + 2): n_weights))
    # Separate the values for the noise interactions from the rest
    noise_interaction_boolean <- predictors_boolean[noise_interactions, ,]
    # Calculate the number of noise interactions for each dataset
    number_noise_interaction <- apply(noise_interaction_boolean, MARGIN = c(2, 3),
                                      FUN = function(x) sum(x))
    
    # Bind all the results in an array 
    total_predictors <- abind(number_simulated_predictors, 
                              number_simulated_simple_effects,
                              number_simulated_interactions,
                              number_noise_predictors, 
                              number_noise_interaction, along = 3)
    
    
    # Set the names of the new dimension
    dimnames(total_predictors)[[3]] <- c("Simulated Predictors", 
                                         "Simulated Simple Effects",
                                         "Simulated Interactions",
                                         "Noise Predictors", 
                                         "Noise Interactions")
    
    # Set results for LR or LR with upsampling to NA if the conditions have 
    # warnings in more than 50% of datasets
    if(warnings_total["LogReg", current_condition, n_noise] > .5){
      total_predictors[1,,] <- NA
    }
    if(warnings_total["UpsamplingLogReg", current_condition, n_noise] > .5){
      total_predictors[4,,] <- NA
    }
    
    # Return array
    return(total_predictors)
    
  }, simplify = "array") 
}, simplify = "array") 

# Loops through results of previous analysis, checking whether the  complete 
# data generating model, all simple effect and all interactions are included in 
# the final model as well as whether no noise is included additionally
true_model <- apply(true_model_frequency, MARGIN = c(1, 2, 4, 5), FUN = function (number){
  # If not all values are NA
  if(is.na(sum(number)) == FALSE){
  # initialize matrix
  model_numbers <- c(rep(FALSE, 6))
  # Check if all true parameters were found
  if(number["Simulated Predictors"] == 6){
    model_numbers[1] <- TRUE
    # Check if noise predictors and/or noise interactions were included
    if(number["Noise Predictors"] == 0 || number["Noise Interactions"]  == 0){
      model_numbers[4] <- TRUE
    }
  } 
  # Check if all simple effects were found
  if (number["Simulated Simple Effects"] == 3){
    model_numbers[2] <- TRUE
    # Check if noise predictors and/or noise interactions were included
    if(number["Noise Predictors"] == 0||number["Noise Interactions"]  == 0){
      model_numbers[5] <- TRUE
    }
  }
  # Check if all true interactions were found
  if (number["Simulated Interactions"] == 3){
    model_numbers[3] <- TRUE
    # Check if noise predictors and/or noise interactions were included
    if(number["Noise Predictors"] == 0||number["Noise Interactions"]  == 0){
      model_numbers[6] <- TRUE
    }
  }
  } else(model_numbers <- c(rep(NA, 6))) # else set complete matrix to NA
  # Return the matrix
  return(model_numbers)
}, simplify = TRUE)

# Separate the first dimension of the last analysis to form the proper separations,
# binding the results along a new dimension of the array and then sorting the dimensions
true_model_array <- aperm(abind(true_model[1:3, , , , ], true_model[4:6, , , , ], 
                                along = 6), 
                          c(1, 6, 2, 4, 3, 5))


# Set the names of the new dimensions in the array (formed both by the separation 
# previously performed and from the analysis) 
dimnames(true_model_array)[[1]] <- c("Complete Model", "Simple Effects", "Interactions")
dimnames(true_model_array)[[2]] <- c("True Model Included", "True Model Only")

# calculate the relative frequencies of the complete data generating model, all 
# simple effect and all interactions being included in the final model as well 
# as being in the final model without additional noise parameters
# Separately for each simulation condition
number_true_model <- apply(true_model_array, MARGIN = c(1, 2, 3, 4, 6), 
                           FUN = function(x) sum(x)/1000)


########################################################
############# Analysis predictor frequency #############
########################################################
# Initialize NV counter for the current analysis
nv_counter <- 0
# Loop to calculate frequencies of the true parameters and average numbers as well
# as 95%-quantiles of the quantities of noise predictors and noise interactions
# in the final model
predictor_frequency<- sapply(results_total, FUN = function (result_data_rearranged){
  # raise NV counter by 1
  nv_counter <<- nv_counter + 1
  # Extract the current number of NV 
  n_noise <- names(results_total[nv_counter])
  # Calculate the current number of predictors 
  n_pred <- as.numeric(n_noise) + 3
  # Calculate the current number of regression weights
  n_weights <- (n_pred * (n_pred + 1)) / 2 + 1
  # Initialize counter for number of combinations of sample size and events fractions
  condition_counter <<- 0
  # Loop through all combinations of sample size and events fractions
  frequency_predictor_results<- sapply(result_data_rearranged, FUN = function(results){
    # raise condition counter by 1
    condition_counter <<- condition_counter + 1
    # Extract the current condition name 
    current_condition <- names(result_data_rearranged[condition_counter])
    
    # Run custom function data.predictors to extract coefficient results
    predictor_results <- data.predictors(results, warnings_total, n_noise, current_condition)
    # Check which values are not NA
    predictor_boolean <- !is.na(predictor_results)
    
    # Set up vector of indices of true parameters
    simulated_predictors <- c(2, 3, 4, n_pred + 2, n_pred + 3, (n_pred * 2 + 1))
    # Separate the values for the true parameters from the rest
    simulated_predictor_boolean <- predictor_boolean[simulated_predictors, , ]
    # Calculate the average over the datasets
    simulated_predictor_average <- apply(simulated_predictor_boolean, 
                                              MARGIN = c(1,2), FUN = function(x) 
                                                mean(x, na.rm = TRUE))
    # Calculate the 95%-quantiles over the datasets    
    simulated_predictor_quantile <- apply(simulated_predictor_boolean, 
                                               MARGIN = c(1, 2), FUN = function (x) 
                                                 quantile(x, probs = c(.025, .975),
                                                          na.rm = TRUE))
    # Set up vector of indices of noise parameters
    noise_predictors <- c(5:(n_pred + 1))
    # Separate the values for the noise parameters from the rest
    noise_pred_boolean <- predictor_boolean[noise_predictors, ,]
    # Calculate the number of noise predictors for each dataset
    noise_predictor_number <- apply(noise_pred_boolean, MARGIN = c(2,3),
                                    FUN = function(x) {
                                      sum(x)})
    # Calculate the average over the datasets
    noise_predictor_average <- apply(noise_predictor_number, 
                                     MARGIN = 1, FUN = function(method){
                              mean(method)  
                            })
    # Calculate the 95%-quantiles over the datasets
    noise_predictor_quantile <- apply(noise_predictor_number, 
                                        MARGIN = 1, 
                                        FUN = function (x) 
                                          quantile(abs(x), probs = c(.025, .975),
                                                   na.rm = TRUE))
    
    # Set up vector of indices of noise interactions
    noise_interactions <- c(c((n_pred + 4):(2 * n_pred)), 
                            c((2 * n_pred + 2):n_weights))
    # Separate the values for the noise interactions from the rest
    noise_interaction_boolean <- predictor_boolean[noise_interactions, ,]
    # Calculate the number of noise interactions for each dataset
    noise_interaction_number <- apply(noise_interaction_boolean, MARGIN = c(2,3),
                                    FUN = function(x) {
                                      sum(x)})
    # Calculate the average over the datasets
    noise_interaction_average <- apply(noise_interaction_number, 
                                     MARGIN = 1, FUN = function(method){
                                       mean(method)  
                                     })
    # Calculate the 95%-quantiles over the datasets
    noise_interaction_quantile <- apply(noise_interaction_number, 
                                      MARGIN = 1, 
                                      FUN = function (x) 
                                        quantile(abs(x), probs = c(.025, .975),
                                                 na.rm = TRUE))

    # Bind the averages for the true parameters, the noise predictors and the
    # noise interactions by row
    total_predictors_average <- rbind(simulated_predictor_average, 
                                        noise_predictor_average, 
                                        noise_interaction_average)
    # Bind the lower bounds for the true parameters, the noise predictors and the
    # noise interactions by row
    total_predictors_lower_bound <- rbind(simulated_predictor_quantile[1,,], 
                                          noise_predictor_quantile[1,], 
                                          noise_interaction_quantile[1,])
    # Bind the upper bounds for the true parameters, the noise predictors and the
    # noise interactions by row
    total_predictors_upper_bound <- rbind(simulated_predictor_quantile[2,,], 
                                          noise_predictor_quantile[2,], 
                                          noise_interaction_quantile[2,])
    
    # Set the row names of the matrices containing the averages, lower bounds
    # and upper bounds
    rownames(total_predictors_average) <- c(rownames(simulated_predictor_boolean),
                                            "Noise Predictors", "Noise Interactions")
    rownames(total_predictors_lower_bound) <- rownames(total_predictors_average)
    rownames(total_predictors_upper_bound) <- rownames(total_predictors_average)
    
    
    # Bind the matrices containing averages, lower bounds and upper bounds into
    # a 3-dimensional array
    total_predictors <- abind(total_predictors_average, 
                              total_predictors_lower_bound,
                              total_predictors_upper_bound,
                              along = 3)
    
    # Set the names of the new dimension
    dimnames(total_predictors)[[3]] <- c("mean", "lower_bound", "upper_bound")  

    
    # Set results for LR or LR with upsampling to NA if the conditions have 
    # warnings in more than 50% of datasets
    if(warnings_total["LogReg", current_condition, n_noise] > .5){
      total_predictors[,1,] <- NA
    }
    if(warnings_total["UpsamplingLogReg", current_condition, n_noise] > .5){
      total_predictors[,4,] <- NA
    }
          
    # Return the array
    return(total_predictors)
    
  }, simplify = "array") 
}, simplify = "array") 



######################################################
############# Analysis balanced accuracy #############
######################################################


# Set up variables matching conditions names to events fractions
five_percent <- c("condition 1", "condition 5", "condition 9", "condition 13")
ten_percent <- c("condition 2", "condition 6", "condition 10", "condition 14")
thirty_percent <- c("condition 3", "condition 7", "condition 11", "condition 15")
fifty_percent <- c("condition 4", "condition 8", "condition 12", "condition 16")

# Initialize NV counter for the current analysis
nv_counter <- 0

# Loop to calculate average relative BACC; as well as 95% percentiles
balanced_accuracy_result <- sapply(results_total, FUN = function (result_data_rearranged){
  # raise NV counter by 1
  nv_counter <<- nv_counter + 1
  # Extract the current number of NV
  n_noise <- names(results_total[nv_counter])
  # Initialize counter for number of combinations of sample size and events fractions
  condition_counter <<- 0
  # Loop through all combinations of sample size and events fractions
  balanced_accuracy_current_NV<- sapply(result_data_rearranged, FUN = function(results){
    # raise condition counter by 1
    condition_counter <<- condition_counter + 1
    # Extract the name of the current condition
    current_condition <- names(result_data_rearranged[condition_counter])
    # Run custom functionto extract BACC values
    balanced_accuracy_data <- data.balanced.accuracy(results,
                                                     warnings_total, 
                                                     n_noise, 
                                                     current_condition)
    
    # Set the benchmark value by checking what group the name of the 
    # current condition is in
    if(current_condition%in% five_percent){
      bacc_benchmark <- 0.8362169
    } else if (current_condition%in% ten_percent){
      bacc_benchmark <- 0.8265238
    } else if (current_condition%in% thirty_percent){
      bacc_benchmark <- 0.8053806
    } else if (current_condition%in% fifty_percent){
      bacc_benchmark <- 0.7876216
    } else{
      print("Unbekannte Condition!")
    }
    # Calculate the relative BACC 
    balanced_accuracy_condition <- balanced_accuracy_data/ bacc_benchmark
    
    # Calculate the average over the datasets
    balanced_accuracy_average <- apply(balanced_accuracy_condition, 
                                       MARGIN = c(1, 2), 
                                       FUN = function(x) 
                                         mean(x, na.rm = TRUE))
    # Calculate the 95%-quantiles over datasets
    balanced_accuracy_quantile <- apply(balanced_accuracy_condition, 
                                          MARGIN = c(1, 2), 
                                        FUN = function (x) 
                                            quantile(x, probs = c(.025, .975),
                                                     na.rm = TRUE))
    
    # Bind the matrices containing averages, lower bounds and upper bounds into
    # an array
    results_balanced_accuracy <-  abind(balanced_accuracy_average, 
                                        balanced_accuracy_quantile[1,,],
                                        balanced_accuracy_quantile[2,,],
                                        along = 3)
    # Set the names of the new dimension
    dimnames(results_balanced_accuracy) [[3]] <- c("mean", "lower_bound", 
                                                   "upper_bound")
    # Return the array
    return(results_balanced_accuracy)  
  }, simplify = "array")
  return(balanced_accuracy_current_NV)
}, simplify = "array")

########################################################
############# Analysis performance metrics #############
########################################################
# Set up variables matching condition names to events fractions
five_percent <- c("condition 1", "condition 5", "condition 9", "condition 13")
ten_percent <- c("condition 2", "condition 6", "condition 10", "condition 14")
thirty_percent <- c("condition 3", "condition 7", "condition 11", "condition 15")
fifty_percent <- c("condition 4", "condition 8", "condition 12", "condition 16")

# Initialize NV counter for the current analysis
nv_counter <- 0 

# Loop to calculate average relative AUC; as well as 95% percentiles
performance_metrics_results <- sapply(results_total, FUN = function(result_data_rearranged){
  # raise NV counter by 1
  nv_counter <<- nv_counter + 1
  n_noise <- names(results_total[nv_counter])
  # Initialize counter for number of combinations of sample size and events fractions
  condition_counter <<- 0
  
  # Loop through all combinations of sample size and events fractions
  performance_metrics_current_NV <- sapply(result_data_rearranged, FUN = function(results){
    # raise NV counter by 1
    condition_counter <<- condition_counter + 1
    # Extract the name of the current condition
    current_condition <- names(result_data_rearranged[condition_counter])
    
    # Run custom function data.performance.metrics to extract performance metric
    # results
    performance_metrics_total <- data.performance.metrics(results, 
                                                              warnings_total, 
                                                              n_noise, 
                                                              current_condition)
    # Remove the calculated log loss values
    performance_metrics_condition <- performance_metrics_total[,-3,,]
    
    # Set the benchmark value by checking what group the name of the 
    # current condition is in
    if(current_condition%in% five_percent){
      performance_benchmark <- c(0.9422347,0.2154537, 0.082626)
    } else if (current_condition%in% ten_percent){
      performance_benchmark <- c(0.9258972, 0.2715161, 0.1076)
    } else if (current_condition%in% thirty_percent){
      performance_benchmark <- c(0.8919801, 0.3829237, 0.169016)
    } else if (current_condition%in% fifty_percent){
      performance_benchmark <- c(0.8720858, 0.4337199, 0.212044)
    } else{
      print("Unbekannte Condition!")
    }
    
    # Caltulate the values relative to the benchmark value
    performance_metrics_ratio <- apply(performance_metrics_condition,
                                       MARGIN = c(1,3,4), 
                                       FUN = function (metrics){
                                         ratio <- metrics/performance_benchmark
                                         return(ratio)
                                       })
    
    # Calculate the average over the datasets
    performance_metrics_average <- apply(performance_metrics_ratio, 
                                         MARGIN = c(1, 2, 3), 
                                         FUN = function(x) 
                                           mean(x, na.rm = TRUE))
    # Calculate the 95%-quantiles of the datastes
    performance_metrics_quantile <- apply(performance_metrics_ratio, 
                                          MARGIN = c(1, 2, 3), 
                                          FUN = function (x) 
                                            quantile(x, probs = c(.025, .975),
                                                     na.rm = TRUE))

    # Bind the matrices containing averages, lower bounds and upper bounds into
    # an array
    results_performance_metrics <-  abind(performance_metrics_average, 
                                          performance_metrics_quantile[1,,,], 
                                          performance_metrics_quantile[2,,,],
                                          along = 4)
    
    # Set the names of the new dimension
    dimnames(results_performance_metrics) [[4]] <- c("mean", "lower_bound", 
                                                     "upper_bound")
    
    # Return the array
    return(results_performance_metrics)
  }, simplify = "array")
  return(performance_metrics_current_NV)
}, simplify = "array")


#########################################
############# Analysis bias #############
#########################################
# Set the values of the coefficients of the true parameters
beta_vector <- c(0.470003629, 1.029619417, 1.609437912, 
                 0.470003629, 1.029619417, 1.609437912)

# Repeat the true coefficient vector six times to form a matrix with a column for
# each method-upsampling-combination
beta_matrix <- matrix(rep(beta_vector,6),ncol=6)

# Initialize NV counter for the current analysis
nv_counter <- 0
# Loop to calculate average biases of the true parameters and average values of noise
# predictors and noise interactions; as well as 95% percentiles
results_bias_total <- sapply(results_total, FUN = function(result_data_rearranged){
  # raise NV counter by 1
  nv_counter <<- nv_counter + 1
  # Extract the current number of NV 
  n_noise <- names(results_total[nv_counter])
  # Calculate the current number of predictors 
  n_pred <- as.numeric(n_noise) + 3
  # Calculate the current number of regression weights
  n_weights <- (n_pred * (n_pred + 1)) / 2 + 1
  # Initialize counter for number of combinations of sample size and events fractions
  condition_counter <<- 0
  
  # Loop through all combinations of sample size and events fractions
  results_bias <- sapply(result_data_rearranged, function(results) {
    # raise condition counter by 1
    condition_counter <<- condition_counter + 1
    # Extract the current condition name 
    current_condition <- names(result_data_rearranged[condition_counter])
    
    # Run custom function data.predictors to extract coefficient results
    predictor_results <- data.predictors(results, warnings_total, n_noise, current_condition)
    
    # Set up vector of indices of true parameters
    simulated_predictors <- c(2, 3, 4, n_pred + 2, n_pred + 3, (n_pred * 2 + 1))
    # Separate the coefficients for the true parameters from the rest
    simulated_predictor_values <- predictor_results[simulated_predictors, , ]
    # Calculate the bias for in each sample for each predictor
    simulated_predictor_list <- apply(simulated_predictor_values, MARGIN = c(3),
                                      FUN = function(method){
                                        (method-beta_matrix) / beta_matrix
                                      }, simplify = "array")
    # Turns the list into an array
    simulated_predictor_diff <- sapply(simulated_predictor_list, FUN = function(x){
      x
    }, simplify = "array")
    # Calculate average bias for each true parameter
    simulated_predictor_bias_average <- apply(simulated_predictor_diff, 
                                              MARGIN = c(1,2), FUN = function(x) 
                                                mean(x, na.rm = TRUE))*100
    # Calculate the 95%-quantiles of coefficients for each of the true parameters
    simulated_predictor_bias_quantile <- apply(simulated_predictor_diff, 
                                               MARGIN = c(1, 2), FUN = function (x) 
                                                 quantile(x, probs = c(.025, .975),
                                                          na.rm = TRUE))*100

    
    # Set up vector of indices of noise predictors
    noise_predictors <- c(5 : (n_pred + 1))
    # Separate the coefficients for the noise predictors from the rest
    noise_pred_values <- predictor_results[noise_predictors, , ]
    # Calculate average regression weight of noise predictors
    noise_predictor_average <- apply(noise_pred_values, 
                                     MARGIN = 2,
                                     FUN = function(x) mean(abs(x), na.rm = TRUE))
    # Calculate the 95%-quantiles from the coefficients for all noise predictors
    noise_predictor_quantile <- apply(noise_pred_values, 
                                      MARGIN = 2, 
                                      FUN = function (x) 
                                        quantile(abs(x), probs = c(.025, .975),
                                                 na.rm = TRUE))
    
    # Set up vector of indices of noise interactions
    noise_interactions <- c(c((n_pred + 4) : (2 * n_pred)), 
                            c((2 * n_pred + 2) : n_weights))
    # Separate the coefficients for the noise interactions from the rest
    noise_interaction_values <- predictor_results[noise_interactions, , ]
    # Calculate average regression weight of noise interactions
    noise_interaction_average <- apply(noise_interaction_values, 
                                       MARGIN = 2,
                                       FUN = function(x) mean(abs(x), na.rm = TRUE))
    
    # Calculate the 95%-quantiles from the coefficients for all noise interactions
    noise_interaction_quantile <- apply(noise_interaction_values, 
                                      MARGIN = 2, 
                                      FUN = function (x) 
                                        quantile(abs(x), probs = c(.025, .975),
                                                 na.rm = TRUE))
    
    # Bind the averages for the true parameters, the noise predictors and the
    # noise interactions by row
    total_predictors_average <- rbind(simulated_predictor_bias_average, 
                                      noise_predictor_average, 
                                      noise_interaction_average)
    
    # Bind the lower bounds for the true parameters, the noise predictors and the
    # noise interactions by row
    total_predictors_lower_bound <- rbind(simulated_predictor_bias_quantile[1,,], 
                                          noise_predictor_quantile[1,], 
                                          noise_interaction_quantile[1,])
    
    
    # Bind the upper bounds for the true parameters, the noise predictors and the
    # noise interactions by row
    total_predictors_upper_bound <- rbind(simulated_predictor_bias_quantile[2,,], 
                                          noise_predictor_quantile[2,], 
                                          noise_interaction_quantile[2,])
    
    # Set the row names of the matrices containing the averages, lower bounds
    # and upper bounds
    rownames(total_predictors_average) <- c(rownames(simulated_predictor_values),
                                            "Noise Predictors", "Noise Interactions")
    rownames(total_predictors_lower_bound) <- rownames(total_predictors_average)
    rownames(total_predictors_upper_bound) <- rownames(total_predictors_average)
    
    
    # Bind the matrices containing averages, lower bounds and upper bounds into
    # a 3-dimensional array
    total_predictors <- abind(total_predictors_average, 
                              total_predictors_lower_bound,
                              total_predictors_upper_bound,
                              along = 3)
    
    # Set the names of the new dimension
    dimnames(total_predictors)[[3]] <- c("mean", "lower_bound", "upper_bound")
    
    # Change the order of the dimensions
    total_predictors <- aperm(total_predictors, c(1, 3, 2))
    
    # Return the array
    return(total_predictors)
    
    
  }, simplify = "array")
}, simplify = "array")

