# ============================================================
# LIKELIHOOD RATIO TEST (LRT)
# ============================================================
# ============================================================
# QUESTION 1
# ============================================================
# Random sample of size 10 from Exponential distribution
# Mean = theta
#
# Data:
# 0.48, 2.29, 0.27, 0.08, 0.34,
# 0.63, 1.17, 0.58, 0.67, 1.07
#
# NOTE:
# The exact hypotheses and restriction on theta are missing
# from the uploaded document.
#
# Data is entered below for calculation.

x1 <- c(0.48, 2.29, 0.27, 0.08, 0.34,
        0.63, 1.17, 0.58, 0.67, 1.07)

n1 <- length(x1)
sum_x1 <- sum(x1)
mean_x1 <- mean(x1)

cat("Q1 Sample size =", n1, "\n")
cat("Q1 Sum =", sum_x1, "\n")
cat("Q1 Mean =", mean_x1, "\n")


# Exponential likelihood:
#
# L(theta) = (1/theta)^n * exp(-sum(x)/theta)
#
# Log likelihood:
#
# log L(theta) = -n log(theta) - sum(x)/theta
#
# MLE:
#
# theta_hat = mean(X)

theta_hat <- mean_x1

cat("MLE of theta =", theta_hat, "\n")


# ============================================================
# QUESTION 2
# ============================================================
# Sample:
# 3, 2, 6, 4, 1, 8, 5, 6
#
# H0: a = 0
# H1: a != 0
#
# Exact PDF is missing from the uploaded document.
#
# Therefore, the exact LRT cannot be calculated without
# the PDF.
#
# Data:

x2 <- c(3, 2, 6, 4, 1, 8, 5, 6)

n2 <- length(x2)
sum_x2 <- sum(x2)
mean_x2 <- mean(x2)

cat("\nQ2 Sample size =", n2, "\n")
cat("Q2 Sum =", sum_x2, "\n")
cat("Q2 Mean =", mean_x2, "\n")


# ============================================================
# QUESTION 3
# ============================================================
# Historical recovery rate = 0.60
# n = 40
# Observed recoveries = 30
#
# H0: p = 0.60
# H1: p > 0.60
#
# This is a one-sided binomial test.


n <- 40
x <- 30
p0 <- 0.60

# Observed proportion
p_hat <- x / n

cat("\nQ3 Observed proportion =", p_hat, "\n")


# ------------------------------------------------------------
# Exact binomial test
# ------------------------------------------------------------

test_Q3 <- binom.test(
  x,
  n,
  p = p0,
  alternative = "greater"
)

test_Q3


# ------------------------------------------------------------
# LRT
# ------------------------------------------------------------
# Likelihood under H0:
# L(p0) = choose(n,x) p0^x (1-p0)^(n-x)
#
# Unrestricted MLE:
# p_hat = x/n

L0_Q3 <- dbinom(x, n, p0)

L1_Q3 <- dbinom(x, n, p_hat)

LR_Q3 <- L0_Q3 / L1_Q3

cat("L(H0) =", L0_Q3, "\n")
cat("L(MLE) =", L1_Q3, "\n")
cat("Likelihood Ratio =", LR_Q3, "\n")

# Conclusion
if (test_Q3$p.value < 0.05) {
  cat("Reject H0: New drug is significantly more efficient.\n")
} else {
  cat("Do not reject H0: Insufficient evidence that the new drug is more efficient.\n")
}


# ============================================================
# QUESTION 4
# ============================================================
# Historical average dropped calls = 2.5 per hour
# 10-hour data:
#
# 3, 2, 4, 3, 1, 2, 3, 2, 5, 3
#
# H0: lambda = 2.5
# H1: lambda != 2.5
#
# Poisson model


x4 <- c(3, 2, 4, 3, 1, 2, 3, 2, 5, 3)

n4 <- length(x4)
sum_x4 <- sum(x4)
mean_x4 <- mean(x4)

lambda0 <- 2.5

cat("\nQ4 Sample size =", n4, "\n")
cat("Q4 Total dropped calls =", sum_x4, "\n")
cat("Q4 Sample mean =", mean_x4, "\n")


# ------------------------------------------------------------
# Exact Poisson test
# ------------------------------------------------------------
#
# Since this is a two-sided test, use poisson.test()

test_Q4 <- poisson.test(
  sum_x4,
  T = n4,
  r = lambda0,
  alternative = "two.sided"
)

test_Q4


# ------------------------------------------------------------
# Likelihood Ratio
# ------------------------------------------------------------
#
# For Poisson:
#
# MLE of lambda = sample mean

lambda_hat_Q4 <- mean_x4

# Log likelihood:
# l(lambda) = sum(x)*log(lambda)
#             - n*lambda
#             - sum(log(x!))

# Likelihood ratio:
# Lambda = L(lambda0) / L(lambda_hat)

logL0_Q4 <- sum_x4 * log(lambda0) -
  n4 * lambda0 -
  sum(lfactorial(x4))

logL1_Q4 <- sum_x4 * log(lambda_hat_Q4) -
  n4 * lambda_hat_Q4 -
  sum(lfactorial(x4))

LR_Q4 <- exp(logL0_Q4 - logL1_Q4)

cat("MLE of lambda =", lambda_hat_Q4, "\n")
cat("Likelihood Ratio =", LR_Q4, "\n")


# Conclusion
if (test_Q4$p.value < 0.05) {
  cat("Reject H0: There is a significant change in the average rate.\n")
} else {
  cat("Do not reject H0: No significant change in the average rate.\n")
}


# ============================================================
# QUESTION 5
# ============================================================
# Rod lengths:
#
# 49.2, 50.5, 51.1, 48.9, 50.3, 49.7,
# 50.8, 49.5, 50.0, 51.3, 49.8, 50.6
#
# H0: mu = 50
# H1: mu != 50
#
# Population variance = 1
# Therefore sigma = 1
#
# Two-sided Z test


x5 <- c(
  49.2, 50.5, 51.1, 48.9,
  50.3, 49.7, 50.8, 49.5,
  50.0, 51.3, 49.8, 50.6
)

n5 <- length(x5)
mu0 <- 50
sigma5 <- 1

xbar5 <- mean(x5)

cat("\nQ5 Sample size =", n5, "\n")
cat("Q5 Sample mean =", xbar5, "\n")


# ------------------------------------------------------------
# Test statistic
# ------------------------------------------------------------

Z5 <- (xbar5 - mu0) /
  (sigma5 / sqrt(n5))

cat("Z statistic =", Z5, "\n")


# ------------------------------------------------------------
# Critical values
# ------------------------------------------------------------

alpha <- 0.05

z_critical <- qnorm(1 - alpha / 2)

cat("Critical values = +/-", z_critical, "\n")


# ------------------------------------------------------------
# P-value
# ------------------------------------------------------------

p_value5 <- 2 * (1 - pnorm(abs(Z5)))

cat("P-value =", p_value5, "\n")


# ------------------------------------------------------------
# Decision
# ------------------------------------------------------------

if (p_value5 < alpha) {
  cat("Reject H0.\n")
  cat("There is evidence of a real shift in the mean length.\n")
} else {
  cat("Do not reject H0.\n")
  cat("There is insufficient evidence of a real shift.\n")
}


# ============================================================
# LRT FORM FOR QUESTION 5
# ============================================================
# For known variance:
#
# H0: mu = mu0
# H1: mu != mu0
#
# MLE under unrestricted model:
# mu_hat = xbar


mu_hat5 <- mean(x5)

# Log likelihood:
#
# l(mu) = -n/2 * log(2*pi*sigma^2)
#         - sum((x-mu)^2)/(2*sigma^2)

logL0_5 <- -n5 / 2 * log(2 * pi * sigma5^2) -
  sum((x5 - mu0)^2) / (2 * sigma5^2)

logL1_5 <- -n5 / 2 * log(2 * pi * sigma5^2) -
  sum((x5 - mu_hat5)^2) / (2 * sigma5^2)

LR5 <- exp(logL0_5 - logL1_5)

cat("\nQ5 MLE of mu =", mu_hat5, "\n")
cat("Q5 Likelihood Ratio =", LR5, "\n")


# ============================================================
# END OF LIKELIHOOD RATIO TEST PROBLEM SET
# ============================================================