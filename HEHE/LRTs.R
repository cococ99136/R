# ==============================================================================
# SOLUTIONS TO LIKELIHOOD RATIO TEST AND HYPOTHESIS TESTING PROBLEMS
# ==============================================================================

# ==============================================================================
# PROBLEM 1
# ------------------------------------------------------------------------------
# Let X_1, ..., X_n ~ Exponential(mean = theta) with PDF:
#   f(x; theta) = (1/theta) * exp(-x/theta), x > 0
#
# Null Hypothesis (H0): theta = theta_0
# Alternative Hypothesis (H1): theta = theta_1 (where theta_1 > theta_0)
#
# Likelihood Function:
#   L(theta) = (1/theta)^n * exp(-sum(X_i)/theta)
#
# Likelihood Ratio:
#   lambda(x) = L(theta_0) / L(theta_1)
#             = (theta_1 / theta_0)^n * exp(-sum(X_i) * (1/theta_0 - 1/theta_1))
#
# Critical Region: Reject H0 if lambda(x) < k
# Since theta_1 > theta_0, (1/theta_0 - 1/theta_1) > 0.
# Rejection condition simplifies to:
#   sum(X_i) > C
#
# Distribution under H0:
#   2 * sum(X_i) / theta_0 ~ Chi-Square(df = 2n)
#   Therefore, sum(X_i) > (theta_0 / 2) * qchisq(1 - alpha, df = 2*n)
#
# Unbiasedness:
#   The test power function power(theta) = P(sum(X_i) > C | theta)
#   Since 2 * sum(X_i) / theta ~ Chi-Square(2n), 
#   power(theta) = P(ChiSquare(2n) > (2*C)/theta) = 1 - F_chi2((2*C)/theta)
#   Because theta_1 > theta_0, (2*C)/theta_1 < (2*C)/theta_0, 
#   which implies power(theta_1) > power(theta_0) = alpha.
#   Thus, Power >= Size for all theta in H1.
#   CONCLUSION ON UNBIASEDNESS: Yes, the LRT procedure is UNBIASED.
# ==============================================================================

cat("========================================================\n")
cat("PROBLEM 1: Exponential Distribution LRT & Unbiasedness\n")
cat("========================================================\n")

# Sample data
data1 <- c(0.48, 2.29, 0.27, 0.08, 0.34, 0.63, 1.17, 0.58, 0.67, 1.07)
n1 <- length(data1)
sum_x1 <- sum(data1)

cat("Sample size (n):", n1, "\n")
cat("Sample sum (sum_x):", sum_x1, "\n")
cat("Theoretical Rejection Rule: Reject H0 if sum(X_i) > (theta_0 / 2) * qchisq(1 - alpha, df = 2*n)\n")
cat("Is the test procedure unbiased?: YES. The power function power(theta) increases monotonically with theta, ensuring power(theta_1) > alpha for all theta_1 > theta_0.\n\n")


# ==============================================================================
# PROBLEM 2
# ------------------------------------------------------------------------------
# Let X_1, ..., X_n ~ Exponential(location = a, scale = 1) with PDF:
#   f(x; a) = exp(-(x - a)),  for x >= a
#
# Data: 3, 2, 6, 4, 1, 8, 5, 6
# Testing H0: a = 0 vs H1: a != 0 (since a <= min(X_i), under H1 a can be != 0)
#
# Likelihood function:
#   L(a) = exp(-sum(X_i - a)) = exp(-sum(X_i) + n*a), provided a <= X_(1)
#
# Under H0: a = 0 => L(0) = exp(-sum(X_i))
# Under H1: MLE of a is a_hat = min(X_i) = X_(1)
#   L(a_hat) = exp(-sum(X_i) + n * X_(1))
#
# Likelihood Ratio:
#   lambda = L(0) / L(a_hat) = exp(-n * X_(1))
#
# Reject H0 if lambda < k  <=>  exp(-n * X_(1)) < k  <=>  X_(1) > C
# Under H0 (a=0), X_i ~ Exp(1). 
#   Distribution of X_(1) = min(X_i) ~ Exponential(rate = n)
#   P(X_(1) > C) = exp(-n * C) = alpha  =>  C = -log(alpha) / n
# ==============================================================================

cat("========================================================\n")
cat("PROBLEM 2: Shifted Exponential Distribution LRT\n")
cat("========================================================\n")

data2 <- c(3, 2, 6, 4, 1, 8, 5, 6)
n2 <- length(data2)
x_min2 <- min(data2)
alpha2 <- 0.05

critical_value2 <- -log(alpha2) / n2
p_value2 <- exp(-n2 * x_min2)

cat("Sample size (n):", n2, "\n")
cat("Minimum sample value X_(1):", x_min2, "\n")
cat("Critical Value C at alpha = 0.05:", critical_value2, "\n")
cat("Likelihood Ratio Statistic lambda:", exp(-n2 * x_min2), "\n")
cat("P-value:", p_value2, "\n")

if (x_min2 > critical_value2) {
  cat("Decision: Reject H0 (a = 0)\n\n")
} else {
  cat("Decision: Fail to reject H0 (a = 0)\n\n")
}


# ==============================================================================
# PROBLEM 3
# ------------------------------------------------------------------------------
# Biotech Recovery Rates (Binomial Proportion Test)
# H0: p = 0.60
# H1: p > 0.60 (One-tailed test)
# Data: n = 40, x = 30
# ==============================================================================

cat("========================================================\n")
cat("PROBLEM 3: Biotech Drug Recovery Rate Test\n")
cat("========================================================\n")

n3 <- 40
x3 <- 30
p0_3 <- 0.60

# One-tailed Exact Binomial Test
binom_res3 <- binom.test(x = x3, n = n3, p = p0_3, alternative = "greater")

cat("Observed Successes:", x3, "out of", n3, "\n")
cat("Sample Proportion:", x3 / n3, "\n")
cat("Null Proportion (p0):", p0_3, "\n")
cat("P-value:", binom_res3$p.value, "\n")

if (binom_res3$p.value < 0.05) {
  cat("Decision: Reject H0. The new drug is significantly more efficient.\n\n")
} else {
  cat("Decision: Fail to reject H0. Not enough evidence to show the new drug is more efficient.\n\n")
}


# ==============================================================================
# PROBLEM 4
# ------------------------------------------------------------------------------
# Telecom Dropped Calls (Poisson Test)
# H0: lambda = 2.5 dropped calls per hour
# H1: lambda != 2.5 dropped calls per hour (Two-tailed test)
# Data over 10 hours: 3, 2, 4, 3, 1, 2, 3, 2, 5, 3
# Total dropped calls T = sum(X_i) = 28 over total time t = 10 hours
# ==============================================================================

cat("========================================================\n")
cat("PROBLEM 4: Telecom Dropped Calls Rate Test\n")
cat("========================================================\n")

data4 <- c(3, 2, 4, 3, 1, 2, 3, 2, 5, 3)
total_calls4 <- sum(data4)
total_hours4 <- length(data4)
lambda0_4 <- 2.5

poisson_res4 <- poisson.test(x = total_calls4, T = total_hours4, r = lambda0_4, alternative = "two.sided")

cat("Total Dropped Calls observed:", total_calls4, "in", total_hours4, "hours\n")
cat("Observed Rate:", total_calls4 / total_hours4, "per hour\n")
cat("Null Rate (lambda0):", lambda0_4, "per hour\n")
cat("P-value:", poisson_res4$p.value, "\n")

if (poisson_res4$p.value < 0.05) {
  cat("Decision: Reject H0. There is a significant change in the average dropped call rate.\n\n")
} else {
  cat("Decision: Fail to reject H0. No significant change in the average dropped call rate.\n\n")
}


# ==============================================================================
# PROBLEM 5
# ------------------------------------------------------------------------------
# Metal Rod Lengths (One-sample Z-test with known population variance)
# H0: mu = 50 cm
# H1: mu != 50 cm
# Data: 12 rods, population variance sigma^2 = 1 => sigma = 1
# ==============================================================================

cat("========================================================\n")
cat("PROBLEM 5: Metal Rod Mean Length Test (Known Variance)\n")
cat("========================================================\n")

data5 <- c(49.2, 50.5, 51.1, 48.9, 50.3, 49.7, 50.8, 49.5, 50.0, 51.3, 49.8, 50.6)
n5 <- length(data5)
mean_x5 <- mean(data5)
mu0_5 <- 50
sigma5 <- 1

# Z-statistic calculation
z_stat5 <- (mean_x5 - mu0_5) / (sigma5 / sqrt(n5))
p_value5 <- 2 * (1 - pnorm(abs(z_stat5)))

cat("Sample Size (n):", n5, "\n")
cat("Sample Mean:", mean_x5, "\n")
cat("Null Mean (mu0):", mu0_5, "\n")
cat("Population Standard Deviation (sigma):", sigma5, "\n")
cat("Z-statistic:", z_stat5, "\n")
cat("P-value:", p_value5, "\n")

if (p_value5 < 0.05) {
  cat("Decision: Reject H0. There is a real shift in average length.\n\n")
} else {
  cat("Decision: Fail to reject H0. There is no evidence of a real shift in average length.\n\n")
}