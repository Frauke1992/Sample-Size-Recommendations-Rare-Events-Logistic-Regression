### TO-DO Document this script.
### GOAL: plan data generating model parameters in order to achieve a certain event fraction
### We aim at marginal probs: 0.5, 0.1, 0.05, 0.01, 0.005
### maximal OR for large effects is 4 based on http://dx.doi.org/10.1037/bul0000084 (p. 210, Tab 4)

library(clusterGeneration)



# Step 1: Construct correlation matrix of the predictors -----------------------

nnoise <- 0 # use 0 irrelevant predictors
corMatrix <- rcorrmatrix(3 + nnoise)

corMatrix[1,2] <- corMatrix[2,1] <- corMatrix[1,3] <- corMatrix[3,1] <- 0.1
corMatrix[2,3] <- corMatrix[3,2] <- 0.5
corMatrix

# legacy code to ensure positive definiteness of the correlation matrix
# min(eigen(corMatrix)$values)
# corMatrix <- Matrix::nearPD(corMatrix, corr = TRUE)$mat
# min(eigen(corMatrix)$values)
# corMatrix[1:3, 1:3] # correlations of the relevant predictors - attenuated slightly


# Step 2: Generate Predictor Values --------------------------------------------

n <- 1000 # sample size
X <- mvtnorm::rmvnorm(n, sigma = corMatrix)
X <- as.data.frame(X)
X <- model.matrix(~V1*V2*V3-(V1:V2:V3), data = X)

# Step 3: Generate Outcome -----------------------------------------------------

BETA0 <- seq(20,-20, length.out = 100000) # potential intercepts

marginal_probs <- sapply(BETA0, function(iB0){
  
  BETA <- c(iB0, rep(c(1.5,	log(2.5),	log(4)),2)) # beta coefficients
  eta <- X %*% BETA
  prob <- 1/(1+exp(-eta))
  
  mean(prob)
  
}, simplify = "array")

plot(BETA0, marginal_probs, type = "l", xlab = "Intercept", 
     ylab = "Marginal Probability", col = "blue", lwd = 2)

# Step 4: Find intercepts for marginal probabilities ---------------------------
plot(marginal_probs, BETA0, type = "l", xlab = "Marginal Probability", 
     ylab = "Intercept", col = "blue", lwd = 2)

chosen_intercepts <- approx(marginal_probs, BETA0, xout = c(0.5, 0.1, 0.05, 0.01, 0.005))
chosen_intercepts$y


# Step 5: Final check ----------------------------------------------------------

sapply(chosen_intercepts$y, function(iB0){
  
  BETA <- c(iB0, rep(c(1.5,	log(2.5),	log(4)),2)) # beta coefficients
  eta <- X %*% BETA
  prob <- 1/(1+exp(-eta))
  sim_dat <- rbinom(n = 100000, size = 1, prob = prob)

  c(mean(prob), mean(sim_dat))
  
}, simplify = "array")
