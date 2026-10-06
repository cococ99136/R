# ====================================================================
# PROBLEM 1: LRT for Exponential Mean
# ====================================================================
# ASSUMPTION: The missing text asks to test H0: theta = theta_0 against 
# H1: theta != theta_0[cite: 7].
#
# 1. MATHEMATICAL DERIVATION
# Given a sample X_1, ..., X_10 ~ Exponential(mean = theta)[cite: 7], 
# the likelihood function is: L(theta) = theta^(-n) * exp(-sum(x_i)/theta).
#
# The unrestricted Maximum Likelihood Estimator (MLE) is theta_hat = x_bar.
# Under the null hypothesis, the restricted MLE is simply the constant theta_0.
#
# The Likelihood Ratio (Lambda) is:
# Lambda = L(theta_0) / L(theta_hat)
# Lambda = [theta_0^(-n) * exp(-n * x_bar / theta_0)] / [x_bar^(-n) * exp(-n)]
# Lambda = (x_bar / theta_0)^n * exp(-n * (x_bar / theta_0) + n)
#
# We reject H0 if Lambda < c.
#
# Is the test unbiased? Yes[cite: 7]. While standard equal-tail tests for 
# asymmetric distributions are biased, the Likelihood Ratio Test inherently 
# partitions the rejection region asymmetrically to exactly balance the 
# power curve, resulting in a uniformly most powerful unbiased (UMPU) test 
# for the exponential mean.

obs1 <- c(0.48, 2.29, 0.27, 0.08, 0.34, 0.63, 1.17, 0.58, 0.67, 1.07)
n1 <- length(obs1)
x_bar1 <- mean(obs1)

# NOTE: Replace 'NA' with the actual null hypothesis value (theta_0) from your original paper
theta_0 <- NA 

if (!is.na(theta_0)) {
  lambda_stat <- (x_bar1 / theta_0)^n1 * exp(-n1 * (x_bar1 / theta_0) + n1)
  
  # Wilks' Theorem: -2 ln(Lambda) follows a Chi-square distribution with 1 df
  chi_stat <- -2 * log(lambda_stat)
  p_value <- pchisq(chi_stat, df = 1, lower.tail = FALSE)
  
  cat("Lambda:", lambda_stat, "\n-2ln(L):", chi_stat, "\nP-value:", p_value, "\n")
} else {
  cat("Please provide theta_0 to complete the calculation.\n")
}


# ====================================================================
# PROBLEM 2: LRT for Shifted Exponential
# ====================================================================
# ASSUMPTION: The missing probability density function (p.d.f.) is the 
# two-parameter shifted exponential f(x) = (1/theta) * exp(-(x-a)/theta) 
# for x >= a, with an unknown scale parameter theta[cite: 7]. 
# We are testing H0: a = 0 against H1: a != 0[cite: 7].
#
# 1. MATHEMATICAL DERIVATION
# The likelihood function is L(a, theta) = theta^(-n) * exp(-sum(x_i - a)/theta) 
# for a <= min(x_i).
#
# - Unrestricted MLEs: To maximize the likelihood, 'a' must be as large as 
#   possible without exceeding the smallest observation. Thus, a_hat = x_(1) 
#   (the minimum order statistic). Solving for theta yields theta_hat = x_bar - x_(1).
#
# - Restricted MLEs (under H0): Since a = 0[cite: 7], theta_hat_0 = x_bar.
#
# The Likelihood Ratio is:
# Lambda = L(0, theta_hat_0) / L(a_hat, theta_hat)
# Lambda = [x_bar^(-n) * e^(-n)] / [(x_bar - x_(1))^(-n) * e^(-n)]
# Lambda = ((x_bar - x_(1)) / x_bar)^n 
# Lambda = (1 - x_(1)/x_bar)^n
#
# We reject H0 if Lambda < c, which simplifies algebraically to rejecting 
# if the ratio x_(1)/x_bar > k.

obs2 <- c(3, 2, 6, 4, 1, 8, 5, 6)
n2 <- length(obs2)
x_bar2 <- mean(obs2)
x_min <- min(obs2)

# Calculate Lambda
lambda_stat2 <- (1 - (x_min / x_bar2))^n2

# Asymptotic Chi-square test (1 degree of freedom since we restrict 1 parameter 'a')
chi_stat2 <- -2 * log(lambda_stat2)
p_value2 <- pchisq(chi_stat2, df = 1, lower.tail = FALSE)

cat("Lambda:", lambda_stat2, "\n-2ln(L):", chi_stat2, "\nP-value:", p_value2, "\n")
