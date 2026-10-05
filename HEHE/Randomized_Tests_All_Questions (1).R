# ============================================================
# STATISTICAL INFERENCE - RANDOMIZED TESTS
# Complete R Script for Problem Set
# ============================================================

# This script solves both questions in the uploaded problem set.
# The questions do not specify a numerical significance level alpha.
# Therefore, the UMP randomized tests are first given for a general
# level alpha. For numerical conclusions, alpha = 0.05 is illustrated.

cat("\n============================================================\n")
cat("RANDOMIZED TESTS - COMPLETE SOLUTION\n")
cat("============================================================\n\n")


# ============================================================
# QUESTION 1
# ============================================================
# From a Poisson distribution with parameter lambda, a random
# sample of size 10 is drawn.
#
# Test:
#       H0: lambda = 1.2
#       H1: lambda > 1.2
#
# Let X = sum(X_i).
# Since Xi ~ Poisson(lambda),
#       X ~ Poisson(10*lambda).
#
# By the monotone likelihood ratio property, the UMP test rejects
# for large values of X.
#
# For a randomized test of size alpha:
#       Reject if X > c
#       Reject with probability gamma if X = c
#       Accept if X < c
#
# where
#       P_1.2(X > c) + gamma*P_1.2(X = c) = alpha.
#
# ============================================================

cat("QUESTION 1\n")
cat("------------------------------------------------------------\n")

alpha <- 0.05

lambda0 <- 1.2
mu0 <- 10 * lambda0

# Find the boundary c:
# P(X > c) < alpha <= P(X >= c)
candidates <- 0:100

for (cc in candidates) {
  p_greater <- ppois(cc, lambda = mu0, lower.tail = FALSE)
  p_ge <- ppois(cc - 1, lambda = mu0, lower.tail = FALSE)

  if (p_greater < alpha && p_ge >= alpha) {
    c1 <- cc
    break
  }
}

p_gt_c <- ppois(c1, lambda = mu0, lower.tail = FALSE)
p_eq_c <- dpois(c1, lambda = mu0)

gamma1 <- (alpha - p_gt_c) / p_eq_c

cat("Significance level alpha =", alpha, "\n")
cat("Under H0, X ~ Poisson(", mu0, ")\n", sep = "")
cat("Critical boundary c =", c1, "\n")
cat("P(X > c) =", p_gt_c, "\n")
cat("P(X = c) =", p_eq_c, "\n")
cat("Randomization probability gamma =", gamma1, "\n\n")

cat("UMP randomized test:\n")
cat("  Reject H0 if X >", c1, "\n")
cat("  Reject H0 with probability gamma =", gamma1,
    " if X =", c1, "\n")
cat("  Accept H0 if X <", c1, "\n\n")


# Power function for Question 1
# Under lambda:
# X ~ Poisson(10*lambda)
#
# Power(lambda) =
# P_lambda(X > c) + gamma*P_lambda(X = c)

lambda_values <- seq(0.01, 3, by = 0.01)

power_q1 <- ppois(
  c1,
  lambda = 10 * lambda_values,
  lower.tail = FALSE
) +
  gamma1 * dpois(
    c1,
    lambda = 10 * lambda_values
  )

# Plot power curve
plot(
  lambda_values,
  power_q1,
  type = "l",
  lwd = 2,
  xlab = expression(lambda),
  ylab = "Power",
  main = "Q1: Power Curve of UMP Randomized Test",
  ylim = c(0, 1)
)

abline(h = alpha, lty = 2)
abline(v = lambda0, lty = 2)

# Display power at selected alternatives
selected_lambda <- c(1.2, 1.3, 1.5, 1.8, 2.0)

power_selected_q1 <- ppois(
  c1,
  lambda = 10 * selected_lambda,
  lower.tail = FALSE
) +
  gamma1 * dpois(
    c1,
    lambda = 10 * selected_lambda
  )

cat("Power at selected values of lambda:\n")
print(
  data.frame(
    lambda = selected_lambda,
    power = round(power_selected_q1, 6)
  )
)

cat("\nInterpretation:\n")
cat("The power increases as lambda moves above 1.2.\n")
cat("At lambda = 1.2, the power equals the significance level 0.05.\n\n")


# ============================================================
# QUESTION 2
# ============================================================
# A box contains 10 pencils, an unknown proportion pi of which
# are red. Each of four children picks a pencil at random.
#
# Test:
#       H0: pi = 0.5
#       H1: pi < 0.5
#
# Let X = number of red pencils selected by the four children.
#
# Under H0:
#       Total red pencils K = 10*pi = 5.
#
# Since sampling is without replacement:
#       X ~ Hypergeometric(N=10, K=5, n=4)
#
# For H1: pi < 0.5, the number of red pencils K is less than 5.
# Hence small values of X provide evidence against H0.
#
# Therefore the UMP randomized test rejects for small X.
#
# ============================================================

cat("\n============================================================\n")
cat("QUESTION 2\n")
cat("------------------------------------------------------------\n")

N <- 10
K0 <- 5
n <- 4

# Again, alpha was not specified in the question.
# We use alpha = 0.05 for the numerical randomized test.

alpha2 <- 0.05

# Hypergeometric probabilities under H0
x_values <- 0:4
p_h0 <- dhyper(x_values, m = K0, n = N - K0, k = n)

cat("Under H0: pi = 0.5, K = 5 red pencils.\n")
cat("Distribution of X under H0:\n")

print(
  data.frame(
    X = x_values,
    Probability = round(p_h0, 6)
  )
)

# Find c such that
# P(X < c) < alpha <= P(X <= c)
#
# We use the boundary value c where randomization occurs.
#
# For this problem, X = 0 has probability 0.0238095,
# while P(X <= 1) = 0.2619048.
#
# Thus reject if X < 1 (i.e. X = 0), and randomize at X = 1.

c2 <- 1

p_less_c2 <- phyper(
  c2 - 1,
  m = K0,
  n = N - K0,
  k = n
)

p_equal_c2 <- dhyper(
  c2,
  m = K0,
  n = N - K0,
  k = n
)

gamma2 <- (alpha2 - p_less_c2) / p_equal_c2

cat("\nSignificance level alpha =", alpha2, "\n")
cat("Boundary c =", c2, "\n")
cat("P(X < c) =", p_less_c2, "\n")
cat("P(X = c) =", p_equal_c2, "\n")
cat("Randomization probability gamma =", gamma2, "\n\n")

cat("UMP randomized test:\n")
cat("  Reject H0 if X <", c2, "\n")
cat("  Reject H0 with probability gamma =", gamma2,
    " if X =", c2, "\n")
cat("  Accept H0 if X >", c2, "\n\n")


# ------------------------------------------------------------
# CONCLUSION WHEN ONLY ONE CHILD HAS A RED PENCIL
# ------------------------------------------------------------

observed_x <- 1

cat("Observed number of red pencils =", observed_x, "\n")

if (observed_x < c2) {
  cat("Reject H0.\n")
} else if (observed_x == c2) {
  cat(
    "Since X = 1 is the randomization boundary, reject H0 with probability",
    gamma2, "and accept otherwise.\n"
  )
} else {
  cat("Accept H0.\n")
}

cat(
  "\nTherefore, when exactly one child has a red pencil,\n",
  "the randomized test rejects H0 with probability ",
  gamma2,
  " (for alpha = 0.05).\n\n",
  sep = ""
)


# ============================================================
# POWER CURVE FOR QUESTION 2
# ============================================================
# For a given proportion pi, the number of red pencils K = 10*pi.
#
# Because the actual box contains an integer number of red pencils,
# the exact finite-population distribution is hypergeometric.
#
# We calculate power for the possible values K = 0,...,5
# corresponding to pi = K/10.
#
# Power =
# P_pi(X < 1) + gamma*P_pi(X = 1)
#        = P_pi(X = 0) + gamma*P_pi(X = 1)

K_values <- 0:5
pi_values <- K_values / 10

power_q2 <- numeric(length(K_values))

for (i in seq_along(K_values)) {
  K <- K_values[i]

  p_x0 <- dhyper(
    0,
    m = K,
    n = N - K,
    k = n
  )

  p_x1 <- dhyper(
    1,
    m = K,
    n = N - K,
    k = n
  )

  power_q2[i] <- p_x0 + gamma2 * p_x1
}

cat("Power for possible values of pi <= 0.5:\n")

print(
  data.frame(
    pi = pi_values,
    Number_of_red_pencils = K_values,
    Power = round(power_q2, 6)
  )
)

# Plot Question 2 power curve
plot(
  pi_values,
  power_q2,
  type = "b",
  pch = 19,
  lwd = 2,
  xlab = expression(pi),
  ylab = "Power",
  main = "Q2: Power Curve of UMP Randomized Test",
  ylim = c(0, 1)
)

abline(h = alpha2, lty = 2)
abline(v = 0.5, lty = 2)


# ============================================================
# FINAL ANSWERS
# ============================================================

cat("\n============================================================\n")
cat("FINAL ANSWERS\n")
cat("============================================================\n\n")

cat("QUESTION 1:\n")
cat("For alpha = 0.05:\n")
cat("X = sum(Xi) ~ Poisson(10*lambda)\n")
cat("Under H0, X ~ Poisson(12)\n")
cat("Reject if X >", c1, "\n")
cat("Randomize at X =", c1, "with probability", gamma1, "\n")
cat("Power(lambda) = P_lambda(X >", c1, ") + gamma*P_lambda(X =",
    c1, ")\n\n", sep = "")

cat("QUESTION 2:\n")
cat("Under H0, number of red pencils = 5.\n")
cat("X ~ Hypergeometric(N=10, K=5, n=4)\n")
cat("For alpha = 0.05:\n")
cat("Reject if X = 0\n")
cat("Randomize at X = 1 with probability", gamma2, "\n")
cat("If exactly one child has a red pencil (X=1),\n")
cat("reject H0 with probability", gamma2,
    "and accept otherwise.\n\n")

cat("Both power curves have been plotted above.\n")
cat("============================================================\n")
