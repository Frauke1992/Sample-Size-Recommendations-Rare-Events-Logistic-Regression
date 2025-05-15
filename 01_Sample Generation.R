#### Requires the script "Functions Sample Generation" ####

##### Load neccessary packages #####
library(clusterGeneration) # used to create random correlation matrix for noise variables
library(Matrix)
# set directory to folder containing necessary scripts
directory_script <- getwd()

# Load R-script containing setup for data visualization
source(paste0(directory_script, "/01a_Functions Sample Generation.R"))
source("00a_generate_predictor_corr.R")


##### Data preparation #####

# Read in table containing different combinations of simulation factors
condition_table <- as.data.frame(read.csv("Conditions.csv", header = TRUE))


###############################
nloop <- 1000 # number of samples for each condition
seed_sample <- 123
seed_validation <- 321321
####### Sample Generation #######
##### Loop to go through different conditions in condition_table #####

clust <- makeCluster(24, 
                     type = "FORK", 
                     outfile = paste0("evaluationDataStatus", 
                                      Sys.Date(),".txt"))

all_conditions <- parLapply(clust, 1:nrow(condition_table), fun = function(i_condition){
  conditions <- condition_table[i_condition,]
  options(warn = 0)
  ##### Extracting information: Get information for current condition #####
  # sample size
  n_sample <- conditions$sample_size
    # number of noise variables
  n_noise_variables <- conditions$n_noise_variables 
  # get the betas for the predictors and interactions
  beta_vector <- c(conditions$intercept, unlist(conditions[1,grepl("b_", names(conditions))]))

  
  # get the model formula from the table
  model_equation <- conditions$model 
  # change the model formula into the needed format for glmnet and model.matrix
  model_x <- terms(formula(gsub('Y', '', model_equation))) 
  model_formula <- as.formula(paste0("Y",gsub('Y', '', model_equation)))
  
  ##### Set current correlation matrix ####
  # number of potential predictors
  n_variables <- n_noise_variables + n_predictors 
  
  current_correlation_mat <- full_correlation_mat[1:n_variables, 1:n_variables]

  
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
    validation_values <- generate.random.data(n_sample, 
                                              beta_vector, 
                                              current_correlation_mat, 
                                              model_x)
    # print(seed_sample)
    # return both samples in a list
    samples <- list(train = train_values, validation = validation_values, oracle_model = model_formula)
    return(samples)
  })
  save(condition_samples, file = paste0("./data/samples_condition_", i_condition, ".rdata"), compress = TRUE, compression_level = 6)
  print(paste0("Condition ", i_condition, " done"))
  return(i_condition)
})

stopCluster(clust)
# save the samples in rda files
# save(all_conditions, file = "samples_total.rdata", compress = TRUE, compression_level = 6)



