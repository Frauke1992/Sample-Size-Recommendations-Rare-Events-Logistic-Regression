# get the computed intercepts and weights from 00a
load("intercepts_and_weights.RData")

sample_sizes <- c(100, 500, 1000, 5000)
model <- "Y~X1*X2*X3-(X1:X2:X3)"


intercepts <- final_params["M",, "intercept"]

regression_weights <- cbind(final_params["M",, "intercept"], final_params["M",, "weight"] %o% beta_init)

n_noise_variables <- c(5, 15) #c(5, 15, 30) 
colnames(regression_weights) <- c("intercept", "b_X1", "b_X2", "b_X3", "b_X1X2", "b_X1X3", "b_X2X3")


# create a table with the conditions
condition_table <- expand.grid(sample_sizes, model, intercepts, n_noise_variables)
names(condition_table) <- c("sample_size", "model", "intercept", "n_noise_variables")
condition_table <- merge(condition_table, regression_weights, by = "intercept")

# save the table
write.csv(condition_table, "Conditions.csv", row.names = FALSE) 
 

