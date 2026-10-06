# ============================================================
# STATISTICAL INFERENCE - RANDOMIZED TESTS
# ============================================================


# ============================================================
# QUESTION 1
# ============================================================

# Xi ~ Poisson(lambda)
# n = 10
#
# H0: lambda = 1.2
# H1: lambda > 1.2
#
# X = sum(Xi)
# Therefore X ~ Poisson(10*lambda)
#
# For H1: lambda > 1.2, reject for large X.
#
# alpha = 0.05


alpha = 0.05

lambda0 = 1.2

# Under H0:
# X ~ Poisson(10*1.2) = Poisson(12)

mu0 = 10*lambda0

# Find critical value
# P(X > c) < alpha
# P(X >= c) >= alpha
#
# For lambda = 1.2:
# P(X > 20) < 0.05
# P(X >= 20) > 0.05
#
# Therefore c = 20

c = 20

p_greater = ppois(c, lambda = mu0, lower.tail = FALSE)

p_equal = dpois(c, lambda = mu0)

# Randomization probability
#
# alpha = P(X > c) + gamma*P(X = c)
#
# Therefore
# gamma = [alpha - P(X > c)] / P(X = c)

gamma = (alpha - p_greater)/p_equal

c
p_greater
p_equal
gamma


# UMP randomized test:
#
# Reject H0 if X > 20
# Reject H0 with probability gamma if X = 20
# Accept H0 if X < 20


# ------------------------------------------------------------
# POWER CURVE
# ------------------------------------------------------------

lambda = seq(0.01, 3, by = 0.01)

# Under any lambda:
# X ~ Poisson(10*lambda)
#
# Power =
# P(X > 20) + gamma*P(X = 20)

power = ppois(c, lambda = 10*lambda, lower.tail = FALSE) +
        gamma*dpois(c, lambda = 10*lambda)

plot(lambda, power,
     type = "l",
     lwd = 2,
     ylim = c(0, 1),
     xlab = expression(lambda),
     ylab = "Power",
     main = "Q1: Power Curve")

abline(h = alpha, lty = 2)
abline(v = lambda0, lty = 2)


# ============================================================
# QUESTION 2
# ============================================================

# Total pencils = 10
# Four children select pencils without replacement.
#
# pi = proportion of red pencils
#
# H0: pi = 0.5
# H1: pi < 0.5
#
# Under H0:
# Number of red pencils = 10*0.5 = 5
#
# X = number of red pencils selected
#
# X ~ Hypergeometric(N = 10, K = 5, n = 4)
#
# alpha = 0.05
#
# Since H1: pi < 0.5,
# reject for SMALL values of X.


alpha = 0.05

N = 10
K = 5
n = 4

# Under H0:
# P(X = 0)

p0 = dhyper(0, m = K, n = N-K, k = n)

# P(X = 1)

p1 = dhyper(1, m = K, n = N-K, k = n)

p0
p1


# We have:
#
# P(X = 0) = 0.0238095
#
# P(X <= 1) = 0.2619048
#
# Therefore:
# Reject if X = 0
# Randomize if X = 1


c = 1

# Randomization probability:
#
# alpha = P(X = 0) + gamma*P(X = 1)
#
# gamma = [alpha - P(X = 0)] / P(X = 1)

gamma = (alpha - p0)/p1

gamma


# UMP randomized test:
#
# Reject H0 if X < 1
# Reject H0 with probability gamma if X = 1
# Accept H0 if X > 1


# ------------------------------------------------------------
# WHEN EXACTLY ONE CHILD HAS A RED PENCIL
# ------------------------------------------------------------

# Observed X = 1
#
# Since X = 1 is the randomization point,
# reject H0 with probability gamma.

gamma


# ============================================================
# POWER CURVE FOR QUESTION 2
# ============================================================

# If there are K red pencils:
# K = 0, 1, 2, 3, 4, 5
#
# pi = K/10
#
# Power =
# P(X = 0) + gamma*P(X = 1)


K = 0:5

pi = K/10

power = dhyper(0,
               m = K,
               n = N-K,
               k = n) +
        gamma*dhyper(1,
                     m = K,
                     n = N-K,
                     k = n)

power


# Plot power curve

plot(pi, power,
     type = "b",
     pch = 19,
     lwd = 2,
     ylim = c(0, 1),
     xlab = expression(pi),
     ylab = "Power",
     main = "Q2: Power Curve")

abline(h = alpha, lty = 2)
abline(v = 0.5, lty = 2)