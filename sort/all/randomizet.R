# ============================================================
# RANDOMIZED TESTS
# ============================================================
# ============================================================
# QUESTION 1
# ============================================================
# X ~ Poisson(lambda)
# Sample size = 10
#
# H0: lambda = 1.2
# H1: lambda > 1.2
#
# We need a UMP randomized test.
#
# Let T = sum(Xi)
#
# Since Xi ~ Poisson(lambda),
# T ~ Poisson(10 * lambda)
#
# Under H0:
# T ~ Poisson(12)
#
# For an upper-tailed test, reject H0 for large values of T.
#
# To construct a randomized test of size alpha:
#
# Reject when T > c
# Accept when T < c
# Randomize when T = c
#
# Let gamma be the probability of rejection when T = c.


# Choose significance level
alpha <- 0.05

lambda0 <- 1.2
n <- 10

# Distribution of T under H0
lambda_T <- n * lambda0

# Find critical value c
# We need:
# P(T > c) <= alpha
# and
# P(T >= c) > alpha

for (c in 0:50) {
  
  p_greater <- 1 - ppois(c, lambda_T)
  p_greater_equal <- 1 - ppois(c - 1, lambda_T)
  
  if (p_greater <= alpha &&
      p_greater_equal > alpha) {
    
    cat("Critical value c =", c, "\n")
    cat("P(T > c) =", p_greater, "\n")
    cat("P(T >= c) =", p_greater_equal, "\n")
    
    # Randomization probability
    gamma <- (alpha - p_greater) /
      dpois(c, lambda_T)
    
    cat("Randomization probability gamma =", gamma, "\n")
    
    break
  }
}


# ============================================================
# POWER FUNCTION - QUESTION 1
# ============================================================
# For lambda > 1.2:
#
# T ~ Poisson(10*lambda)
#
# Power =
# P(T > c) + gamma P(T = c)

power_Q1 <- function(lambda) {
  
  p_greater <- 1 - ppois(c, n * lambda)
  
  p_equal <- dpois(c, n * lambda)
  
  power <- p_greater + gamma * p_equal
  
  return(power)
}


# Values of lambda
lambda <- seq(1.2, 3, by = 0.01)

power_values <- sapply(lambda, power_Q1)

# Plot power curve
plot(lambda, power_values,
     type = "l",
     lwd = 2,
     xlab = expression(lambda),
     ylab = "Power",
     main = "Question 1: Power Curve")

abline(h = alpha, lty = 2)


# ============================================================
# QUESTION 2
# ============================================================
# Box contains 10 pencils
# Unknown proportion pi are red.
#
# Four children pick one pencil each.
#
# H0: pi = 0.5
# H1: pi < 0.5
#
# Let X = number of children who get a red pencil.
#
# X ~ Binomial(4, pi)
#
# Since H1 is pi < 0.5, small values of X are evidence
# against H0.
#
# We construct a randomized test.


n <- 4
pi0 <- 0.5
alpha <- 0.05


# Critical values:
# X = 0, 1, 2, 3, 4


# We need a critical value c such that:
#
# P(X < c) <= alpha
# and
# P(X <= c) > alpha


for (c in 0:n) {
  
  p_less <- pbinom(c - 1, n, pi0)
  p_less_equal <- pbinom(c, n, pi0)
  
  if (p_less <= alpha &&
      p_less_equal > alpha) {
    
    cat("Critical value c =", c, "\n")
    cat("P(X < c) =", p_less, "\n")
    cat("P(X <= c) =", p_less_equal, "\n")
    
    # Randomization probability
    gamma2 <- (alpha - p_less) /
      dbinom(c, n, pi0)
    
    cat("Randomization probability gamma =", gamma2, "\n")
    
    break
  }
}


# ============================================================
# POWER FUNCTION - QUESTION 2
# ============================================================

power_Q2 <- function(pi) {
  
  p_less <- pbinom(c - 1, n, pi)
  
  p_equal <- dbinom(c, n, pi)
  
  power <- p_less + gamma2 * p_equal
  
  return(power)
}


# Values of pi
pi <- seq(0, 0.5, by = 0.001)

power_values_Q2 <- sapply(pi, power_Q2)


# Plot power curve
plot(pi, power_values_Q2,
     type = "l",
     lwd = 2,
     xlab = expression(pi),
     ylab = "Power",
     main = "Question 2: Power Curve")

abline(h = alpha, lty = 2)


# ============================================================
# CONCLUSION FOR QUESTION 2
# ============================================================
# If only ONE of the four children has a red pencil:
#
# X = 1
#
# We calculate the randomized test probability.

x_observed <- 1

if (x_observed < c) {
  
  cat("Reject H0\n")
  
} else if (x_observed > c) {
  
  cat("Accept H0\n")
  
} else {
  
  cat("Randomize with probability =", gamma2, "\n")
}


# ============================================================
# END OF RANDOMIZED TESTS
# ============================================================