# ============================================================
# PROBLEM SET 1 : POWER CURVES
# ============================================================
# ============================================================
# QUESTION 1
# ============================================================
# Normal population, variance = 1, n = 25
# H0: mu = 0

n <- 25
sigma <- 1

# Enter the lower and upper critical limits
L <- -0.3
U <- 0.3

# Power function
power_Q1 <- function(mu) {
  if (is.na(L) || is.na(U)) return(NA)
  
  pnorm((L - mu) / (sigma / sqrt(n))) +
    (1 - pnorm((U - mu) / (sigma / sqrt(n))))
}

mu <- seq(-2, 2, length.out = 500)

plot(mu, sapply(mu, power_Q1),
     type = "l",
     lwd = 2,
     xlab = expression(mu),
     ylab = "Power",
     main = "Q1: Power Curve")


# ============================================================
# QUESTION 2
# ============================================================
# Coin tossed 20 times
# Reject H0 if number of heads < 5 or > 15
#
# H0: p = 0.5

n <- 20

# Critical region
critical_region_Q2 <- c(0:4, 16:20)
critical_region_Q2

# Significance level
alpha_Q2 <- pbinom(4, n, 0.5) +
  (1 - pbinom(15, n, 0.5))

alpha_Q2

# Power function
power_Q2 <- function(p) {
  pbinom(4, n, p) +
    (1 - pbinom(15, n, p))
}

p <- seq(0, 1, by = 0.001)

plot(p, sapply(p, power_Q2),
     type = "l",
     lwd = 2,
     xlab = "p",
     ylab = "Power",
     main = "Q2: Power Curve")


# ============================================================
# QUESTION 3
# ============================================================
# Poisson population
# Sample size = 10
# Critical region: X > 25
#
# X = sum of 10 Poisson observations
# Therefore X ~ Poisson(10m)
#
# Exact null hypothesis is missing in the uploaded document.
#
# Suppose H0: m = m0

n <- 10
m0 <- 2.5       # CHANGE if the question gives another value

# Critical region
critical_region_Q3 <- 26:Inf

# Type I error
alpha_Q3 <- 1 - ppois(25, lambda = n * m0)

alpha_Q3

# Power function
power_Q3 <- function(m) {
  1 - ppois(25, lambda = n * m)
}

m <- seq(0.1, 5, length.out = 500)

plot(m, sapply(m, power_Q3),
     type = "l",
     lwd = 2,
     xlab = "m",
     ylab = "Power",
     main = "Q3: Power Curve")


# ============================================================
# QUESTION 4
# ============================================================
# Electronic tube lifetime
# n = 10
# u = minimum lifetime
#
# The PDF and hypotheses/size are missing from the extracted
# document, so the exact solution cannot be filled in safely.
#
# Once the distribution is available, use:
#
# U = min(X1,...,X10)
#
# P(U > u) = P(X1 > u,...,X10 > u)
#          = [P(X > u)]^10
#
# Power curve can then be constructed from the distribution.


# ============================================================
# QUESTION 4(ii)
# SAMPLE MEAN
# ============================================================

# Once the population distribution and critical value are known:
#
# power_mean <- function(mu) {
#     1 - pnorm((critical_value - mu) / (sigma / sqrt(10)))
# }
#
# mu <- seq(...)
# plot(mu, power_mean(mu), type="l")


# ============================================================
# QUESTION 4(iii)
# ============================================================
# y = number of observations for which x > 21949.4
# Reject H0 if y > 2
#
# Therefore:
# Critical region = {y >= 3}
#
# If probability of X > 21949.4 is q:
#
# Y ~ Binomial(10, q)
#
# Power = P(Y >= 3)

power_Q4_iii <- function(q) {
  1 - pbinom(2, size = 10, prob = q)
}

q <- seq(0, 1, by = 0.001)

plot(q, sapply(q, power_Q4_iii),
     type = "l",
     lwd = 2,
     xlab = "q = P(X > 21949.4)",
     ylab = "Power",
     main = "Q4(iii): Power Curve")


# ============================================================
# QUESTION 5
# ============================================================
# Xi ~ N(mu, sigma^2 = 4)
# n = 100
# H0: mu = 0
# H1: mu > 0
# alpha = 0.01

n <- 100
sigma <- 2
alpha <- 0.01

# Critical value
z_alpha <- qnorm(1 - alpha)

# Critical value for sample mean
critical_mean <- z_alpha * sigma / sqrt(n)

z_alpha
critical_mean

# UMP test:
# Reject H0 if Xbar > critical_mean


# Power function
power_Q5 <- function(mu) {
  1 - pnorm(
    (critical_mean - mu) / (sigma / sqrt(n))
  )
}

mu <- seq(-1, 2, length.out = 500)

plot(mu, sapply(mu, power_Q5),
     type = "l",
     lwd = 2,
     xlab = expression(mu),
     ylab = "Power",
     main = "Q5: Power Curve")

# Find mu for power = 0.9
mu_values <- seq(0, 2, by = 0.0001)

powers <- sapply(mu_values, power_Q5)

mu_90 <- mu_values[which.min(abs(powers - 0.90))]

mu_90


# ============================================================
# QUESTION 5 - PART 2
# ============================================================
# Zi = 1 if Xi > 0
# Zi = 0 otherwise
#
# Under H0: mu = 0
# P(Zi = 1) = 0.5
#
# Therefore sum(Zi) ~ Binomial(n, 0.5)

alpha <- 0.01

# Function to find critical value
find_critical <- function(n) {
  
  for (c in 0:n) {
    
    alpha_value <- 1 - pbinom(c - 1, n, 0.5)
    
    if (alpha_value <= alpha) {
      return(c)
    }
  }
}

# Find n giving power >= 0.90 when mu = 1
for (n in 1:500) {
  
  c <- find_critical(n)
  
  # Under mu = 1:
  # P(X > 0) = Phi(1/2)
  
  p1 <- pnorm(1 / 2)
  
  power <- 1 - pbinom(c - 1, n, p1)
  
  if (power >= 0.90) {
    
    cat("Required n =", n, "\n")
    cat("Critical value =", c, "\n")
    cat("Power =", power, "\n")
    
    break
  }
}


# ============================================================
# QUESTION 6
# ============================================================
# Sample size = 5
# Two tests suggested.
#
# Some mathematical expressions are missing from the uploaded
# document, so the exact statistics cannot be entered.
#
# S = number of observations outside
# (-0.6745, 0.6745)
#
# Under a standard normal distribution:
# P(|X| > 0.6745) is approximately 0.5.

p_outside <- 2 * (1 - pnorm(0.6745))

p_outside


# For Test 2:
# S ~ Binomial(5, 0.5)

# Possible critical values:
for (c2 in 0:5) {
  
  alpha <- 1 - pbinom(c2 - 1, 5, 0.5)
  
  cat("c2 =", c2,
      " Alpha =", alpha, "\n")
}


# ============================================================
# QUESTION 7
# ============================================================
# X ~ N(mu, 100)
# sigma = 10
# n = 12
# Level = 0.05
#
# Exact hypotheses and critical regions are missing from
# the extracted document.
#
# General setup:

n <- 12
sigma <- 10
alpha <- 0.05

# Standard error
SE <- sigma / sqrt(n)

SE

# For a one-sided upper-tail test:
z_alpha <- qnorm(0.95)

critical_xbar <- 60 + z_alpha * SE

critical_xbar


# Power function for upper-tail test
power_Q7 <- function(mu, mu0 = 60) {
  
  1 - pnorm(
    (critical_xbar - mu) / SE
  )
}

mu <- seq(40, 80, length.out = 500)

plot(mu, sapply(mu, power_Q7),
     type = "l",
     lwd = 2,
     xlab = expression(mu),
     ylab = "Power",
     main = "Q7: Power Curve")


# ============================================================
# QUESTION 8
# ============================================================
# Normal population
# sigma = 1.2
# H0: mu = 75
#
# The direction of H1 is missing from the uploaded document.
#
# For a TWO-SIDED test:
# Detecting a difference of 1 unit
# Power = 0.95

sigma <- 1.2
difference <- 1
alpha <- 0.05
power <- 0.95

z_alpha_2 <- qnorm(1 - alpha / 2)
z_power <- qnorm(power)

n_two_sided <- (
  (z_alpha_2 + z_power) *
    sigma / difference
)^2

ceiling(n_two_sided)


# For a ONE-SIDED test:

z_alpha_1 <- qnorm(1 - alpha)

n_one_sided <- (
  (z_alpha_1 + z_power) *
    sigma / difference
)^2

ceiling(n_one_sided)


# ============================================================
# ============================================================
