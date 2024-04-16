#### Requires the script "Functions Sample Generation" ####

##### Load neccessary packages #####
library(clusterGeneration) # used to create random correlation matrix for noise variables

# set directory to folder containing necessary scripts
directory_script <- "C:/Users/Frauke/Desktop/Masterarbeit/Code"

# Load R-script containing setup for data visualization
source(paste0(directory_script, "/Functions Sample Generation.R"))


##### Data preparation #####

# Read in table containing different combinations of simulation factors
condition_table <- as.data.frame(read.csv2("Conditions_final.csv", header = TRUE))

# create random correlation matrix for all variables
set.seed(123)
correlation_mat <- rcorrmatrix(33)

# set row and column names for the correlation matrix
colnames(correlation_mat) <- rownames(correlation_mat) <-    
  c(paste0("X", 1:33))

 nloop = 1000 # set the number of iterations for the sample draw

# save random correlation matrix
 
###############################

seed_sample <- 123
seed_validation <- 321321
####### Sample Generation #######
##### Loop to go through different conditions in condition_table #####
sample_generation <- apply(condition_table, MARGIN = 1, FUN = function(conditions){
  options(warn = 0)
  ##### Extracting information: Get information for current condition #####
  # sample size
  n_sample <- as.double(conditions[1])
  # number of actual predictors
  n_predictors <- as.double(conditions[2]) 
  # number of interactions between predictors
  n_interactions <- as.double(conditions[3]) 
  # calculate number of correlations that need to be provided
  n_correlations <- n_predictors*(n_predictors - 1)/2 
  # calculate number of betas that need to be provided
  n_beta <- n_predictors + n_interactions+1 
  # number of noise variables
  n_noise_variables <- as.double(conditions[4]) 
  # number of columns in table before correlations are given
  first_col_corr <- as.double (conditions[5]) 
  # get the correlations for the actual predictors
  correlation_x <- as.double(c(conditions[(first_col_corr) :
                                            (n_correlations + 
                                               first_col_corr-1)])) 
  # make the class of the correlations object into a symmetric distance matrix
  class(correlation_x) <- 'dist' 
  # givs the number of predictors as size for the correlations object
  attr(correlation_x, 'Size') <- n_predictors 
  # make matrix with correlations
  correlation_x <- as.matrix(correlation_x) 
  # add the diagonal with ones for the matrix (correlation of a predictor with itself)
  diag(correlation_x) <- 1 
  # get the betas for the predictors and interactions
  beta_vector <- c(as.double(conditions[(n_correlations + first_col_corr) :
                                          (n_correlations + n_beta + 
                                             first_col_corr-1)])) 
  # get the model formula from the table
  model_equation <- as.character(conditions[6]) 
  # change the model formula into the needed format for glmnet and model.matrix
  model_x <- terms(formula(gsub('Y', '', conditions[6]))) 
  
  
  ##### Set current correlation matrix ####
  # number of potential predictors
  n_variables <- n_noise_variables + n_predictors 
  
  current_correlation_mat <- correlation_mat[1:n_variables, 1:n_variables]
  # replace the first part of the matrix with the provided correlations of the actual predictors
  current_correlation_mat[1:n_predictors, 1:n_predictors] <- correlation_x
  # add small values to the diagonal to prevent issues with eigenvalues
  current_correlation_mat <- nearPD(current_correlation_mat, corr = TRUE, keepDiag = TRUE)
  current_correlation_mat <- as.matrix(current_correlation_mat$mat)
  
  
  ##### Sample generation #####
  # lapply-loop to generate the samples for the current condition and save them in a nested list
  condition_samples = lapply(1 : nloop, FUN = function(isample){
    seed_sample <<- seed_sample + 1
    set.seed(seed_sample)
    # generate a general sample that serves as training sample
    train_values <- generate.random.data(n_sample, 
                                         beta_vector, 
                                         current_correlation_mat, 
                                         model_x)
    
    seed_validation <<- seed_validation + 1
    set.seed(seed_validation)
    # generate a validation sample that is half the size of the training sample with 
    # the same conditions for the sample generation
    validation_values <- generate.random.data(n_sample / 2, 
                                              beta_vector, 
                                              current_correlation_mat, 
                                              model_x)
    print(seed_sample)
    # return both samples in a list
    samples <- list(train_values, validation_values)
    names(samples) <- c("train", "validation")
    return(samples)
  })
  return(condition_samples)
})

# save the samples in rda files
saveRDS(sample_generation, file = "samples_total.rdata")

saveRDS(sample_generation[1:16], file = "samples_nnoise_5.rdata")

saveRDS(sample_generation[17:32], file = "samples_nnoise_10.rdata")

saveRDS(sample_generation[33:48], file = "samples_nnoise_20.rdata")

saveRDS(sample_generation[49:64], file = "samples_nnoise_30.rdata")


