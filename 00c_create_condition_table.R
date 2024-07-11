# get the computed intercepts and weights from 00a
load("intercepts_and_weights.RData")

sample_sizes <- c(100, 500, 1000, 5000)
model <- "Y~X1*X2*X3-(X1:X2:X3)"
intercepts <- b0_30noise[[1]]
regression_weights <- b0_30noise$beta_weights
n_noise_variables <- c(5, 10, 20, 30)
names(regression_weights) <- c("b_X1", "b_X2", "b_X3", "b_X1X2", "b_X1X3", "b_X2X3")

# create a table with the conditions
condition_table <- expand.grid(sample_sizes, model, intercepts, n_noise_variables)
names(condition_table) <- c("sample_size", "model", "intercept", "n_noise_variables")
condition_table <- data.frame(condition_table,
                              matrix(regression_weights, nrow = nrow(condition_table), 
                                     ncol = length(regression_weights), byrow = TRUE, 
                                     dimnames = list(NULL, names(regression_weights))))

# save the table
write.csv(condition_table, "Conditions.csv", row.names = FALSE) 
 
# adapt the remainder of Fraukes script -> use colnames instead of indices
# only put the things in the table that are allowed to be changed!
