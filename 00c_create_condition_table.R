# get the computed intercepts and weights from 00a
load("intercepts_and_weights.RData")



sample_sizes <- c(250, 500, 1000, 2000, 4000)
reliability <- c(0.8,1)
model <- "Y~X1*X2*X3-(X1:X2:X3)"


conditions <- final_params[,1:4,"M"]


regression_weights <- cbind(final_params[,c("target_frac", "target_auc", "intercept"), "M"], final_params[,"weight","M"] %o% beta_init)

n_noise_variables <- c(5, 15) #c(5, 15, 30) 


# create a table with the conditions
condition_table <- expand.grid(sample_sizes, reliability, model, n_noise_variables, target_aucs, target_fracs)
names(condition_table) <- c("sample_size", "reliability", "model", "n_noise_variables", "target_auc", "target_frac")



condition_table <- merge(condition_table, regression_weights, by = c("target_auc", "target_frac"))

# save the table
write.csv(condition_table, "Conditions.csv", row.names = FALSE) 
 

