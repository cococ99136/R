# ====================================================================
# QUESTION 1: Method of Moments for Uniform(a, a+1)
# ====================================================================
cat("--- QUESTION 1 ---\n")

# Load the observations
obs <- c(1.768, 1.500, 1.809, 1.274, 1.977, 
         1.279, 1.806, 1.015, 1.254, 1.675, 
         1.224, 1.820, 1.959, 1.262, 1.853, 
         1.562, 1.122, 1.772, 1.139, 1.882)
n <- length(obs)

# Math: For Uniform(a, a+1), E(X) = (a + a + 1) / 2 = a + 0.5
# By MoM, we equate sample mean to population mean: x_bar = a_hat + 0.5
x_bar <- mean(obs)
a_hat <- x_bar - 0.5

# Math: The variance of the estimator a_hat is Var(X_bar - 0.5) = Var(X_bar)
# Var(X_bar) = Var(X)/n. For Uniform(a, a+1), Var(X) = (a+1 - a)^2 / 12 = 1/12.
var_a_hat <- (1/12) / n

cat("MoM Estimate of 'a' (a_hat):", a_hat, "\n")
cat("Estimated Variance of the estimator:", var_a_hat, "\n\n")


# ====================================================================
# QUESTION 2: Restricted MLE for Bernoulli Distribution
# ====================================================================
cat("--- QUESTION 2 ---\n")

# The sum of 10 independent Bernoulli(p) trials follows a Binomial(10, p) distribution.
n_trials <- 10
sum_x <- 7
p_space <- seq(0.1, 0.9, by = 0.1)

# Calculate the likelihood (probability) of observing exactly 7 successes for each p
likelihoods <- dbinom(sum_x, size = n_trials, prob = p_space)

# Find the parameter 'p' that maximizes this likelihood
mle_p <- p_space[which.max(likelihoods)]

cat("Parameter Space (p):", p_space, "\n")
cat("Likelihoods:        ", round(likelihoods, 5), "\n")
cat("MLE of p:           ", mle_p, "\n\n")


# ====================================================================
# QUESTION 3: Hypothesis Testing & Power Function (Z-Test)
# ====================================================================
cat("--- QUESTION 3 ---\n")

# Input Parameters
n_bp <- 36
x_bar_bp <- 10.5
sigma_bp <- 4
mu_0 <- 12
alpha <- 0.05

# (a) Compute test statistic and decision
se_bp <- sigma_bp / sqrt(n_bp)
z_stat <- (x_bar_bp - mu_0) / se_bp
z_crit <- qnorm(alpha) # Left-tailed test for H1: mu < 12

cat("Part (a):\n")
cat("Test Statistic (Z):", z_stat, "\n")
cat("Critical Z-Value:  ", z_crit, "\n")
cat("Decision:          ", ifelse(z_stat < z_crit, "Reject H0", "Fail to Reject H0"), "\n\n")

# (b) Calculate the Type I error probability
cat("Part (b):\n")
cat("Type I Error Probability (alpha):", alpha, "\n\n")

# (c) Derive power function and plot
# Find the critical boundary (c) in terms of the sample mean
c_val <- mu_0 + z_crit * se_bp

cat("Part (c):\n")
cat("Critical Boundary for X-bar (c):", c_val, "\n")
cat("Generating Power Curve Plot...\n")

# Plotting the power curve K(mu) = P(Reject H0 | mu) = P(X-bar < c | mu)
curve(pnorm((c_val - x) / se_bp), from = 8, to = 14, 
      col = "blue", lwd = 2,
      xlab = expression(mu ~ "(True Mean BP Reduction)"), 
      ylab = "Power / Probability of Rejecting H0", 
      main = "Power Curve for BP Reduction Drug Test")
abline(h = alpha, v = 12, col = c("red", "darkgray"), lty = 2)

# "The power curve demonstrates the test's ability to correctly reject the null hypothesis when the[cite: 6]
# drug underperforms (i.e., true mean mu < 12). As the true mean reduction drops further below[cite: 6]
# 12 mmHg (towards 8 mmHg), the power quickly approaches 1.0, indicating high sensitivity to[cite: 6]
# severe underperformance. However, as the true mean approaches 12 mmHg from below, the[cite: 6]
# power drops significantly, showing that the test is less sensitive to minor underperformance. At[cite: 6]
# exactly mu = 12, the power matches the Type I error rate (alpha = 0.05)."[cite: 6]
