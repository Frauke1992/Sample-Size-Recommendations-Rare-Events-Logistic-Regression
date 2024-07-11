library(clusterGeneration)
library(Matrix)
library(parallel)

load("full_correlation_mat.RData")

# define function to obtain marginal probabilities:
get_intercepts <- function(n = 1000, corMatrix = NULL){
  
  X <- mvtnorm::rmvnorm(n, sigma = corMatrix)
  X <- as.data.frame(X)
  X <- model.matrix(~V1*V2*V3-(V1:V2:V3), data = X)
  
  # Step 3: Generate Outcome -----------------------------------------------------
  
  BETA0 <- seq(2,-20, length.out = 1000000) # potential intercepts
  
  

  marginal_probs <- sapply(BETA0, function(iB0){
    
    BETA <- c(iB0, rep(c(log(1.5),	log(2.5),	log(4)),2)) # beta coefficients
    eta <- X %*% BETA
    prob <- 1/(1+exp(-eta))
    
    mean(prob)
    
  }, simplify = "array")
  
  chosen_intercepts <- approx(marginal_probs, BETA0, xout = c(0.5, 0.1, 0.05, 0.01, 0.005))

  out_list <- list(chosen_intercepts$y, beta_weights = rep(c(log(1.5),	log(2.5),	log(4)),2))
  return(out_list)
  
}

# define correlation matrices for different numbers of irrelevant predictors 
# this is a sanity check showing that the computed intercepts do not 
# vary depending on the number of irrelevant predictors

(b0_5noise <- get_intercepts(n = 1000000, corMatrix = full_correlation_mat[1:8,1:8]))
# [1]  -0.2828419  -5.0714385  -7.2546937 -12.5384392 -14.8572436

(b0_30noise <- get_intercepts(n = 1000000, corMatrix = full_correlation_mat))
# [1]  -0.2537262  -5.0727858  -7.1989813 -12.5159021 -14.7479046


save(list = ls(), file = "intercepts_and_weights.RData")