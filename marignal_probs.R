library(clusterGeneration)

# Step 1: Construct correlation matrix of the predictors -----------------------

nnoise <- 10 # use 10 irrelevant predictors
corMatrix <- rcorrmatrix(3+nnoise)

corMatrix[1,2] <- corMatrix[2,1] <- corMatrix[1,3] <- corMatrix[3,1] <- 0.1
corMatrix[2,3] <- corMatrix[3,2] <- 0.5

min(eigen(corMatrix)$values)

corMatrix <- Matrix::nearPD(corMatrix, corr = TRUE)$mat

min(eigen(corMatrix)$values)

corMatrix[1:3, 1:3] # correlations of the relevant predictors - attenuated slightly


# Step 2: Generate Predictor Values --------------------------------------------

n <- 1000 # sample size

X <- mvtnorm::rmvnorm(n, sigma = matrix(corMatrix@x, nrow = 3+nnoise))

X <- as.data.frame(X)

# Step 3: Generate Outcome -----------------------------------------------------

efrac <- 0.005 # event fraction
BETA0 <- log(efrac/(1-efrac))

BETA <- c(BETA0, rep(log(1.6), 6)) # beta coefficients

eta <- BETA[1] + BETA[2]*X$V1 + BETA[3]*X$V2 + BETA[4]*X$V3 + BETA[5]*X$V1*X$V2 + BETA[6]*X$V1*X$V3 + BETA[7]*X$V2*X$V3

# let's look at eta:

summary(eta) # mean is larger than BETA0
# (due to the positive correlation of the predictors, the means of the product terms are > 0)

hist(eta) # distribution is skewed and has a rather long tail
# arises from the combination of positively correlated predictors with positive (interaction) effects

# transform eta to probability:

prob <- 1/(1+exp(-eta))

summary(prob)

hist(prob) # the long tail in eta translates to probabilities ranging from 0-1

# draw observations:

y <- rbinom(n, 1, prob)
mean(y)
