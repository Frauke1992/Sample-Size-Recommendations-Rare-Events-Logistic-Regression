# Load Packages
library(clusterGeneration)
library(MASS)
library(Matrix)
library(akima)
library(future)
library(future.apply)
library(pROC)
options(future.globals.maxSize = 1.0 * 1e9)

# Load simulated correlation matrix
load("full_correlation_mat.RData")
corMatrix <- full_correlation_mat[1:6,1:6]

# Target AUC value
target_aucs  <- c(0.7,0.9)

# Target marginal probabilities
target_fracs <- 0.5^c(1,2,3,4,5,6)



# Initial betas:
# Note, these are not the actual coefficients
# rather they determine the relative sizes of the effects to each other
beta_init <- c(1,2,3,
               1,2,3)
# for easier interpretation:
names(beta_init) <- c("b_X1", "b_X2", "b_X3",
                   "b_X1X2", "b_X1X3", "b_X2X3")

# sample size of the optimization sample
n <- 10e6

#------------- Do optimization for each target marginal probability -------------

# 1) Set up a multisession plan with 10 workers
plan(multisession, workers = 20)

# 2) Generate 20 distinct random seeds
set.seed(42)
nSeeds <- 40
seeds <- sample.int(1e4, nSeeds, replace = FALSE)

# 3) Define the single-run function, explicitly accepting all globals
run_once <- function(seed, n, corMatrix, beta_init,
                     target_aucs, target_fracs) {
  ## —————— 1. Generierung der Trainingsdaten ——————
  X_raw <- mvrnorm(n, mu = rep(0, nrow(corMatrix)),
                   Sigma = corMatrix, empirical = TRUE)
  X_df  <- as.data.frame(X_raw)
  X_mm  <- model.matrix(~0 + X1 * X2 * X3 - (X1:X2:X3), data = X_df)
  rm(X_raw, X_df); gc()
  
  ## —————— 2. Definition der Loss-Funktion ——————
  loss_function <- function(params, target_auc, target_frac, beta_init) {
    set.seed(seed)
    intercept <- params[1]
    betas     <- params[2] * beta_init
    linpred   <- drop(intercept + X_mm %*% betas)
    probs     <- plogis(linpred)
    probs     <- pmin(pmax(probs, 1e-6), 1 - 1e-6)
    y         <- rbinom(n, 1, probs)
    auc_val   <- if (length(unique(y)) < 2) 0.5 else auc(y, linpred, quiet = TRUE)
    frac      <- mean(probs)
    (auc_val - target_auc)^2 + (frac - target_frac)^2
  }
  
  ## —————— 3. Grid erzeugen und Optimieren ——————
  param_grid <- expand.grid(
    target_auc  = target_aucs,
    target_frac = target_fracs,
    stringsAsFactors = FALSE
  )
  
  opt_mat <- t(apply(param_grid, 1, function(p) {
    opt <- optim(
      par    = c(-0.1, 1),
      fn     = loss_function,
      target_auc  = as.numeric(p["target_auc"]),
      target_frac = as.numeric(p["target_frac"]),
      beta_init   = beta_init,
      method = "L-BFGS-B",
      lower  = c(-Inf, 0),
      upper  = c( Inf, Inf)
    )
    c(intercept = opt$par[1],
      weight    = opt$par[2],
      conv      = opt$convergence)
  }))
  
  results <- cbind(
    param_grid,
    as.data.frame(opt_mat, stringsAsFactors = FALSE)
  )
  
  rm(X_mm); gc()
  
  ## —————— 4. Generierung der Validierungsdaten ——————
  set.seed(seed + 1)
  X_chk_raw <- mvrnorm(n, mu = rep(0, nrow(corMatrix)),
                       Sigma = corMatrix, empirical = TRUE)
  X_chk_df  <- as.data.frame(X_chk_raw)
  X_chk_mm  <- model.matrix(~0 + X1 * X2 * X3 - (X1:X2:X3), data = X_chk_df)
  rm(X_chk_raw); gc()
  
  ## —————— 5. Validierung & Rückgabe ——————
  checks <- lapply(seq_len(nrow(results)), function(i) {
    p         <- results[i, ]
    intercept <- p$intercept
    weight    <- p$weight
    betas     <- weight * beta_init
    
    linpred <- drop(intercept + X_chk_mm %*% betas)
    probs   <- plogis(linpred)
    y       <- rbinom(n, 1, probs)
    
    fit <- glm(y ~ X1 + X2 + X3 + X1:X2 + X1:X3 + X2:X3,
               family = binomial, data = X_chk_df)
    auc_emp <- if (length(unique(y)) < 2) NA else
      auc(y, predict(fit, type = "response"), quiet = TRUE)[1]
    
    list(
      target_auc    = p$target_auc,
      target_frac   = p$target_frac,
      betas_target  = betas,
      betas_emp     = fit$coefficients,
      marg_prob_emp = mean(y),
      auc_emp       = auc_emp
    )
  })
  
  rm(X_chk_mm, X_chk_df); gc()
  
  list(
    results = results,
    checks  = checks
  )
}

# 4) Execute in parallel, passing globals and enabling parallel-safe RNG
all_runs <- future_lapply(
  seeds,
  run_once,
  n            = n,
  corMatrix    = corMatrix,
  beta_init    = beta_init,
  target_aucs   = target_aucs,
  target_fracs = target_fracs,
  future.seed  = TRUE
)

# 5) (Optional) Revert to sequential execution
plan(sequential)

# Example access:
# all_runs[[1]]$results
# all_runs[[1]]$checks

all_params <- sapply(all_runs, function(iRun){
  t(iRun$results)
}, simplify = "array")

# Identify indices where conv != 0
nonconv_idx <- which(all_params[, "conv", ] != 0, arr.ind = TRUE)

# Set corresponding intercept and weight to NA
for (i in seq_len(nrow(nonconv_idx))) {
  row <- nonconv_idx[i, 1]
  slice <- nonconv_idx[i, 2]
  all_params[row, "intercept", slice] <- NA
  all_params[row, "weight", slice] <- NA
  all_params[row, "conv", slice] <- NA
}

# Boxplot pro Parameter
par(mfrow = c(2,2))
apply(all_params[,"intercept",],1, boxplot)
dev.off()

# Parameters and their standard deviations across runs
apply(all_params, 1:2, function(iRun){ c(M = mean(iRun, na.rm = TRUE), SD = sd(iRun, na.rm = TRUE)) })



all_checks <- sapply(all_runs, function(iRun){
  t(sapply(iRun$checks, function(iFrac){
    c(target_frac = iFrac$target_frac,
      emp_frac = iFrac$marg_prob_emp,
      diff_frac = iFrac$target_frac - iFrac$marg_prob_emp,
      target_auc = target_auc,
      emp_auc = iFrac$auc_emp,
      diff_auc = target_auc - iFrac$auc_emp)
  }, USE.NAMES = TRUE))
  
}, USE.NAMES = TRUE, simplify = "array")

n <- 5
diffs <- as.vector(all_checks[, "diff_frac.target_frac", ])
order_idx <- order(abs(diffs), decreasing = TRUE)[1:n]
top_diffs <- diffs[order_idx]
arr.ind <- arrayInd(order_idx, dim(all_checks)[c(1,3)])

# Übersicht als Tabelle
top_table <- data.frame(
  row = arr.ind[,1],
  slice = arr.ind[,2],
  diff = top_diffs
)
print(top_table)

# delete these top differences 
all_params[4,c("intercept", "weight"),top_table$slice] <- NA

# Parameters and their standard deviations across runs
final_params <- apply(all_params, 1:2, function(iRun){ c(M = mean(iRun, na.rm = TRUE), SD = sd(iRun, na.rm = TRUE)) })

save(list = ls(), file = "intercepts_and_weights.RData")


