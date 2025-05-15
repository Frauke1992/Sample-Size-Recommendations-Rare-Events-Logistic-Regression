# The predictor correlations need to be hardcoded and can only be changed here
# because the computation of b0 for the manipulation of the marginal probabilities
# depends on this correlation matrix

# create random correlation matrix for all variables
set.seed(123)

# draw a large random correlation matrix including the maximum number of predictors and noise variables
n_predictors <- 3
correlation_mat <- clusterGeneration::rcorrmatrix(33)

# set row and column names for the correlation matrix
colnames(correlation_mat) <- rownames(correlation_mat) <-    
  c(paste0("X", 1:33))

# replace correlations for the actual predictors with the intended values
correlation_mat[1,2] <- correlation_mat[2,1] <- correlation_mat[1,3] <- correlation_mat[3,1] <- 0.1
correlation_mat[2,3] <- correlation_mat[3,2] <- 0.5

# add small values to the diagonal to prevent issues with eigenvalues
full_correlation_mat <- Matrix::nearPD(correlation_mat, corr = TRUE, keepDiag = TRUE)
full_correlation_mat <- as.matrix(full_correlation_mat$mat)

# save random correlation matrix
save(full_correlation_mat, file = "full_correlation_mat.RData")