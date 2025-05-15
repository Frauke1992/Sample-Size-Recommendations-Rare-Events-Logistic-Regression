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
corMatrix <- full_correlation_mat[1:8,1:8]

# Target AUC value
target_auc  <- 0.75

# Target marginal probabilities
target_fracs <- c(0.5, 0.1, 0.05, 0.01)



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
plan(multisession, workers = 10)

# 2) Generate 20 distinct random seeds
set.seed(42)
nSeeds <- 10
seeds <- sample.int(1e4, nSeeds, replace = FALSE)

# 3) Define the single-run function, explicitly accepting all globals
run_once <- function(seed, n, corMatrix, beta_init, target_auc, target_fracs) {
  ## —————— 1. Generierung der Trainingsdaten ——————
  X_raw <- mvrnorm(n, mu = rep(0, nrow(corMatrix)),
                   Sigma = corMatrix, empirical = TRUE)
  X_df  <- as.data.frame(X_raw)
  X_mm  <- model.matrix(~0 + X1 * X2 * X3 - (X1:X2:X3), data = X_df)
  
  # Große Zwischenobjekte löschen
  rm(X_raw, X_df)
  gc()  # Speicher sofort zurückholen
 
  ## —————— 2. Definition der Loss-Funktion & Optimierung ——————
  loss_function <- function(params, target_auc, target_frac, beta_init) {
    set.seed(seed)
    intercept <- params[1]
    betas     <- params[2] * beta_init
    linpred   <- drop(intercept + X_mm %*% betas)
    probs     <- plogis(linpred)
    probs <- pmin(pmax(probs, 1e-6), 1 - 1e-6)
    y         <- rbinom(n, 1, probs)
    if (length(unique(y)) < 2) {
      auc_val <- 0.5  # oder NA, je nachdem was du willst
    } else {
      auc_val <- auc(y, linpred, quiet = TRUE)
    }
    frac <- mean(probs)
    # Quadratsumme der Abweichungen
    (auc_val - target_auc)^2 + (frac - target_frac )^2
  }
  

   results <- sapply(target_fracs, function(target_frac) {
    
    opt <- optim(par    = c(-0.1,1),
          fn     = loss_function,
          target_auc  = target_auc,
          target_frac = target_frac,
          beta_init   = beta_init,
          method = "L-BFGS-B",
          lower  = c(-Inf, 0),
          upper  = c(Inf, Inf)
    )

    c(intercept = opt$par[1], weight = opt$par[2], target_frac = target_frac, conv = opt$convergence)
  })
  
  ## —————— 3. Trainings-Matrix nicht mehr nötig ——————
  rm(X_mm)
  gc()
  
  ## —————— 4. Generierung der Validierungsdaten ——————
  set.seed(seed + 1)
  X_chk_raw <- mvrnorm(n, mu = rep(0, nrow(corMatrix)),
                       Sigma = corMatrix, empirical = TRUE)
  X_chk_df  <- as.data.frame(X_chk_raw)
  X_chk_mm  <- model.matrix(~0+X1 * X2 * X3 - (X1:X2:X3), data = X_chk_df)
  
  # Auch hier räumen wir auf
  rm(X_chk_raw)
  gc()
  

  ## —————— 5. Validierung & Rückgabe ——————
  checks <- apply(results, 2, function(params) {
    set.seed(seed)
    intercept <- params["intercept"]
    weight    <- params["weight"]
    betas     <- weight * beta_init
    
    linpred <- drop(intercept + X_chk_mm %*% betas)
    probs   <- plogis(linpred)
    y       <- rbinom(n, 1, probs)
    
    fit     <- glm(y ~ X1 + X2 + X3 + X1:X2 + X1:X3 + X2:X3,
                   family = binomial, data = X_chk_df)  
    if (length(unique(y)) < 2) {
      warning("Only one response level in y; skipping AUC calculation.")
      auc_emp <- NA
    } else {
      auc_emp <- auc(y, predict(fit, type = "response"), quiet = TRUE)[1]
    }
    
    list(params        = params,
         betas_target  = weight * beta_init,
         betas_emp     = fit$coefficients,
         target_frac   = params["target_frac"],
         marg_prob_emp = mean(y),
         auc_emp       = auc_emp)
  })
  
  ## Wichtig: X_chk_mm kann jetzt ebenfalls entfernt werden,
  ## falls Sie es nicht mehr außerhalb brauchen:
  rm(X_chk_mm, X_chk_df)
  gc()
  
  list(results = results, checks = checks)
}

# 4) Execute in parallel, passing globals and enabling parallel-safe RNG
all_runs <- future_lapply(
  seeds,
  run_once,
  n            = n,
  corMatrix    = corMatrix,
  beta_init    = beta_init,
  target_auc   = target_auc,
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


apply(all_params, 1:2, function(iRun){ c(M = mean(iRun), SD = sd(iRun)) })


all_runs[[1]]$results
all_runs[[5]]$checks


# save(list = ls(), file = "intercepts_and_weights.RData")
