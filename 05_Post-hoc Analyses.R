#### Requires data to be imported into "results_total" using the script "Data analysis applied" ####

##### Load neccessary packages #####

######################################################################
######################### Post-hoc analysis ######################### 
######################################################################

############################################################
##################### Data Preparation #####################
############################################################

# Initialize NV counter for the current analysis
nv_counter <- 0
# Loop to calculate relative frequencies of noise parameters being smaller 
# than true parameters
noise_true_compare <- sapply(results_total, FUN = function (result_data_rearranged){
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
  # Set up vector of indices of true parameters
  simulated_preds <- c(2, 3, 4, n_pred + 2, n_pred + 3, (n_pred * 2 + 1))
  
  # Loop through all combinations of sample size and events fractions
  noise_true_total <- sapply(result_data_rearranged, FUN = function(results){
    # raise condition counter by 1
    condition_counter <<- condition_counter + 1
    # Extract the current condition name 
    current_condition <- names(result_data_rearranged[condition_counter])
    
    # Run custom function data.predictors to extract coefficient results
    predictor_results <- data.predictors(results, warnings_total, n_noise, current_condition)
    # Keep the coefficients for the ElasticNet analyses only 
    predictor_results <- predictor_results[,c(2,3,5,6),]
    
    # Initialize an array for the results
    noise_true <- array(0, dim = c(6,9,4,1000))
    # Set names for the different dimensions of the array
    dimnames(noise_true)[[1]] <- c("X1","X2","X3","X1:X2","X1:X3","X2:X3")
    dimnames(noise_true)[[2]] <- c("true_found","noise_pred_included", "predictor_smaller", 
                                      "pred_smaller_diff", "pred_bigger_diff", 
                                      "noise_inter_included", "interaction_smaller", 
                                      "inter_smaller_diff", "inter_bigger_diff")
    dimnames(noise_true)[[3]] <- c("EnetRoc", "EnetLogloss",
                                   "UpsamplingEnetRoc", "UpsamplingEnetLogloss")
    dimnames(noise_true)[[4]] <- c(paste0("Sample ", 1:1000))
    
    # Loop through all true parameters
    for(i in 1: length(simulated_preds)){
      # set effectsize to the name of the current true parameter
      effectsize = simulated_preds[i]
      # get the regression weight of the current true parameter
      simulated_parameter <- predictor_results[effectsize, , ]
      # Set up vector of indices of noise predictors
      noise_predictors <- c(5:(n_pred + 1))
      # Separate the coefficients for the noise predictors from the rest
      noise_predictors_values <- predictor_results[noise_predictors, ,]
      
      
      # Set up vector of indices of noise interactions
      noise_interactions <- c(c((n_pred + 4):(2 * n_pred)), c((2 * n_pred + 2):
                                                                n_weights))
      # Separate the coefficients for the noise interactions from the rest
      noise_interaction_values <- predictor_results[noise_interactions, ,]
      
      # Custom function to get max value from all non-NA values
      my.max <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA)
      # Get max absolute value of all noise predictors for each dataset and method
      max_noise_predictors <- apply(noise_predictors_values, MARGIN = c(2,3), 
                                    FUN = function(x) my.max(abs(x)))
      # Get max absolute value of all noise interactions for each dataset and method
      max_noise_interactions <- apply(noise_interaction_values, MARGIN = c(2,3), 
                                      FUN = function(x) my.max(abs(x)))
      
      # Bind the value for the current true parameter and the max values of noise
      # parameters
      min_max_total <- abind(simulated_parameter,
                             max_noise_predictors, 
                             max_noise_interactions,
                             along = 3)
      dimnames(min_max_total)[[3]] <- c("true_value", "noise_pred", 
                                        "noise_inter")
      
      # Loop to check for current true predictor if it is larger than the noise
      # parameters and calculate the differences
      results_general <- apply(min_max_total, MARGIN = c(1,2), FUN = function(min_max){
        noise_compare <- c(rep(NA, 9))
        names(noise_compare) <- c("true_found","noise_pred_included", "predictor_smaller", 
                                  "pred_smaller_diff", "pred_bigger_diff", 
                                  "noise_inter_included", "interaction_smaller", 
                                  "inter_smaller_diff", "inter_bigger_diff")
        # Check if the true parameter was found
        if(!is.na(min_max[1])){
          # Set that value for true parameter foundin the matrix to 1
          noise_compare["true_found"] <- 1
          # Check if no noise predictors were included in the final model
          if(is.na(min_max[2])){
            # Set the value for noise predictor smaller than true parameter
            # in the matrix to 1
            noise_compare["predictor_smaller"] <- 1
            # Set the difference for noise predictor smaller to the value 
            # of the true parameter
            noise_compare["pred_smaller_diff"] <- min_max[1]
          } else{
            # Otherwise check if the value of the true parameter is bigger than 
            # the biggest value of the noise predictors
            if(min_max[1] > min_max[2]){
              # Set the value for noise predictor smaller than true parameter
              # in the matrix to 1
              noise_compare["predictor_smaller"] <- 1
              # Calculate the difference between true and noise value 
              # for noise predictor smaller in matrix
              # of the true parameter
              noise_compare["pred_smaller_diff"] <- min_max[1] - min_max[2]
            } else{
              # Otherwise set the value for noise predictor smaller than true 
              # parameter in the matrix to 0
              noise_compare["predictor_smaller"] <- 0
              # Calculate the difference between true and noise value 
              # for noise predictor bigger in matrix
              noise_compare["pred_bigger_diff"] <- min_max[1] - min_max[2]
            }
          }
          # Check if no noise interaction was included in the final model
          if(is.na(min_max[3])){
            # Set the value for noise interaction smaller than true parameter
            # in the matrix to 1
            noise_compare["interaction_smaller"] <- 1
            # Set the difference for noise interaction smaller to the value 
            # of the true parameter
            noise_compare["inter_smaller_diff"] <- min_max[1]
          } else{
            # Otherwise check if the value of the true parameter is bigger than 
            # the biggest value of the noise interactions
            if(min_max[1] > min_max[3]){
              # Set the value for noise interaction smaller than true parameter
              # in the matrix to 1
              noise_compare["interaction_smaller"] <- 1
              # Calculate the difference between true and noise value 
              # for noise interaction smaller in matrix
              noise_compare["inter_smaller_diff"] <- min_max[1] - min_max[3]
            } else{
              # Otherwise set the value for noise interaction smaller than 
              # true parameter in the matrix to 0
              noise_compare["interaction_smaller"] <- 0
              # Calculate the difference between true and noise value 
              # for noise interaction bigger in matrix
              noise_compare["inter_bigger_diff"] <- min_max[1] - min_max[3]
            }
          }
          
        }
        # Check if noise predictors were in the final model
        if(!is.na(min_max[2])){
          # Set the value for noise predictors included to 1
          noise_compare["noise_pred_included"] <- 1
          # Check if the true parameter was not found
          if(is.na(min_max[1])){
            # Set the value for noise predictor smaller than true 
            # parameter in the matrix to 0
            noise_compare["predictor_smaller"] <- 0
            # Set the difference for noise interaction bigger to the value 
            # of the noise predictor
            noise_compare["pred_bigger_diff"] <- min_max[2]
          }
        }
        # Check if noise interactions were in the final model
        if(!is.na(min_max[3])){
          noise_compare["noise_inter_included"] <- 1
          # Check if the true parameter was not found
          if(is.na(min_max[1])){
            # Otherwise set the value for noise interaction smaller than true 
            # parameter in the matrix to 0
            noise_compare["interaction_smaller"] <- 0
            # Set the difference for noise interaction bigger to the value 
            # of the noise interaction
            noise_compare["inter_bigger_diff"] <- min_max[3]
          }
        }

        # return the matrix
        return(noise_compare)
      }, simplify = TRUE)
    # add results_general to the total results
    noise_true[i,,,] <- results_general
    }
    
    # return the array
    return(noise_true)
  }, simplify = "array")
}, simplify = "array") 


# Loop over data to calculate relative frequencies of the noise parameters being
# smaller than the true parameters
compare_sum <- apply(noise_true_compare, MARGIN = c(1, 3, 5, 6),
                     FUN = function(samples){
                       # Calculate the number of  datasets in which the respective 
                       # true parameter was bigger than the noise predictor
                       total_noise_pred <- sum(samples["predictor_smaller",], 
                                               na.rm = TRUE)
                       # Divide that number by 1000
                       rel_noise_pred <- total_noise_pred / 1000
                       # Calculate the number of datasets in which the respective
                       # true parameter was bigger than the noise interaction
                       total_noise_inter <- sum(samples["interaction_smaller",],
                                                na.rm = TRUE)
                       # Divide that number by 1000
                       rel_noise_inter <- total_noise_inter / 1000
                       
                       # Bind the tesults for the noise predictors and noise interactions
                       results <- c(rel_noise_pred, rel_noise_inter)
                       
                     }, simplify = TRUE)

# Set names of the new dimension
dimnames(compare_sum)[[1]] <- c("noise_predictor", "noise_interaction")


# Loop over data to calculate averages and 95%-quantiles of differences between
# true and noise parameters
compare_diff <- apply(noise_true_compare, MARGIN = c(1, 3, 5, 6),
                      FUN = function (samples){
                        # Take only the differences between noise parameters and
                        # true parameters from the data
                        differences <- samples[c(4,5,8,9),]
                        # Loop over data
                        results <- apply(differences, MARGIN = 1, FUN = function(values) {
                          # Initialize vector for results
                          results_vector <- c(rep(NA, 3))
                          # Check if not all values are NA
                          if(sum(is.na(values))!= 1000){
                          # Calculate average of absolute differences
                          average_value <- mean(abs(values), na.rm = TRUE)
                          # calculate 95%-quantiles of absolute differences
                          value_quantiles <- quantile(abs(values), 
                                                          probs = c(.025, .975), 
                                                          na.rm = TRUE)
                          
                          # Bind averages, lower bounds and upper bounds in vector
                          results_vector <- c(average_value, value_quantiles[1], 
                                              value_quantiles[2])
                          }
                          # Set names for vector
                          names(results_vector) <- c("mean", "lower_bound", 
                                                     "upper_bound")
                          # Return vector with results
                          return(results_vector)
                        }, simplify = T)
                        # return matrix
                        return(results)
                      }, simplify = TRUE)

# Split first dimension of the array into two dimensions and bind them to new array
compare_diff_sorted <- abind(compare_diff[1:3,,,,], compare_diff[4:6,,,,],
                             compare_diff[7:9,,,,],compare_diff[10:12,,,,], 
                             along = 6)

# Set names for the first dimension
dimnames(compare_diff_sorted)[[1]] <- c("mean", "lower_bound", 
                                        "upper_bound")

# Split last dimension of the array into two dimensions and bind them to new array
compare_diff_sorted_new <- abind(compare_diff_sorted[,,,,,1:2], 
                                 compare_diff_sorted[,,,,,3:4],
                                 along = 7)

# Set names for the new dimensions
dimnames(compare_diff_sorted_new)[[6]] <- c("noise_smaller", "noise_bigger")
dimnames(compare_diff_sorted_new)[[7]] <- c("noise_predictor", "noise_interaction")
