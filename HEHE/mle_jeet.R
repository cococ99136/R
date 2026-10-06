# Q1: Hypergeometric MLE for Subpopulation
N <- 94; n <- 10; x <- 6
M_hat <- floor(x * (N + 1) / n)
cat("MLE of white balls:", M_hat, "\n") # Output: 57

# Q2: Hypergeometric MLE for Total Population
M <- 9225; n <- 150; x <- 24
N_hat <- floor(M * n / x)
cat("MLE of population size N:", N_hat, "\n") # Output: 57656

# Q3: Restricted Parameter Space
n <- 25; black_balls <- 14
P_options <- seq(0.30, 0.70, by = 0.05)
# Likelihood function for Binomial
likelihoods <- dbinom(black_balls, n, P_options)
MLE_P_with_rep <- P_options[which.max(likelihoods)]

cat("MLE of P (With Replacement):", MLE_P_with_rep, "\n") # Output: 0.55

# Q4: Uniform Distribution
obs <- c(1.45, 0.68, 1.10, 0.90, 1.40, 0.16, 0.36, 0.54, 0.76, 1.80)
n <- length(obs)
MLE_theta <- max(obs)
MVUE_theta <- ((n + 1) / n) * MLE_theta
SE_est <- sqrt((MLE_theta^2) / (n * (n + 2))) # Estimated SE

cat("MLE:", MLE_theta, "| MVUE:", MVUE_theta, "| SE:", SE_est, "\n")

# Q5: Multinomial Genetics (Root Finding)
log_lik_deriv <- function(theta) {
  (357 / (2 + theta)) - (70 / (1 - theta)) + (94 / theta)
}
# Find root between 0 and 1
MLE_genetics <- uniroot(log_lik_deriv, c(0.001, 0.999))$root
cat("MLE of theta:", MLE_genetics, "\n") # Output: ~0.729

# Q6: Cauchy MLE Iteration
obs_cauchy <- c(5.47, 9.29, 5.02, 5.26, 3.86, 5.22, 4.58, 5.50, 1.11, 1.79, 3.34)
# Negative log-likelihood function
nll_cauchy <- function(theta) {
  -sum(log(1 / (pi * (1 + (obs_cauchy - theta)^2))))
}
# Optimize starting from the median
mle_cauchy <- optim(par = median(obs_cauchy), fn = nll_cauchy, method = "BFGS")$par
cat("Iterative MLE for Cauchy theta:", mle_cauchy, "\n") # Output: ~4.99

# Q7: Zero-Truncated Poisson
eggs <- 1:9
freqs <- c(22, 18, 18, 11, 9, 6, 3, 0, 1)
n_flowers <- sum(freqs)
x_bar <- sum(eggs * freqs) / n_flowers

eq_truncated <- function(lambda) { (lambda / (1 - exp(-lambda))) - x_bar }
MLE_lambda <- uniroot(eq_truncated, c(0.1, 10))$root
cat("MLE of Poisson lambda:", MLE_lambda, "\n") # Output: ~2.74

# Q8: Shifted Exponential
obs_exp <- c(3.71, 9.36, 16.24, 2.67, 6.78, 8.92, 10.22, 4.31)
n <- length(obs_exp)
X_1 <- min(obs_exp)
unbiased_theta <- X_1 - (2 / n)
SE_theta <- sqrt(4 / n^2) # Variance of X_(1) is 4/n^2

cat("Unbiased Estimate:", unbiased_theta, "| SE:", SE_theta, "\n")

# Q9: Exponential Distribution
life_hrs <- c(980, 1020, 995, 1015, 990, 1030, 975, 950, 1050, 870)
n <- length(life_hrs)
theta_hat <- mean(life_hrs)
se_theta_hat <- theta_hat / sqrt(n)
prob_survive <- exp(-100 / theta_hat)

cat("MLE:", theta_hat, "| SE:", se_theta_hat, "| P(X>=100):", prob_survive, "\n")

# Q10: Type I Censoring
failed_times <- c(9.8, 15.8, 17.2, 11.2, 13.8, 18.9, 14.6, 19.6)
censored_times <- c(20, 20)
total_time <- sum(failed_times) + sum(censored_times)
r_failures <- length(failed_times)

MLE_censored_theta <- total_time / r_failures
cat("MLE of mean life:", MLE_censored_theta, "\n") # Output: 20.1125

# Q11: Invariance Property
n <- 154
x1 <- 79; x2 <- 32; x5 <- 17
mle_combo <- (x1 - 2*x2 + 6*x5) / n
cat("MLE of function:", mle_combo, "\n") # Output: ~0.7597

