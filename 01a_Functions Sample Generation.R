##### Load neccessary packages #####
library(mvtnorm) # used to draw random samples from multivariate normal distribution

##########################################
###### Function to generate samples ######
##########################################
generate.random.data <- function(nsample, reliability, betavect, corrmat, modelx){
  # create a vector with zeroes (standardized predictors) as my values for the 
  # number of predictors in the correlation matrix
  muvectx <- rep(0, nrow(corrmat)) 
  # draw the x-values from a multivariate normal distribution, given the correlation
  # matrix and a vector of zeros for standardized predictors
  xmat_true <- rmvnorm(n = nsample, mean = muvectx, sigma = corrmat)

  # Simulate error (independent standard normal noise for each variable)
  error_mat <- matrix(rnorm(nsample * ncol(xmat_true)), nrow = nsample)
  
  # Compute observed (unreliable) data with target reliability
  # Replace 'reliability' with your scalar reliability value (e.g., 0.80)
  xmat <- sqrt(reliability) * xmat_true + sqrt(1 - reliability) * error_mat
  
  # name the columns of the matrix of x-values
  colnames(xmat) <- c(paste0("X", 1 : ncol(xmat))) 
  # turn the x-values of the actual predictors into a model.matrix, according to 
  # the formula provided in modelx
  xmatrix <- model.matrix(modelx, data = as.data.frame(xmat)) 
  # calculate the probabilities based on the model.matrix and the betas
  probs <- exp(xmatrix%*%betavect)/(1 + exp(xmatrix%*%betavect)) 
  # draw the y-values, from a binomial distribution following the calculated probabilities
  yvalues <- as.matrix(rbinom(n = nsample, size = 1, prob = probs)) 
  # name the column
  colnames(yvalues) <- "Y" 
  
  # create a data frame with the drawn x- and y-values
  sample_data <- data.frame(yvalues, xmat) 
  # return that data frame
  return(sample_data) 
}
