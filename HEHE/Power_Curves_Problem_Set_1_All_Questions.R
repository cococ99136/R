# ============================================================
# STATISTICAL INFERENCE 2 - SEM 5
# PROBLEM SET 1: POWER CURVES
# Complete solution in ONE R SCRIPT
# ============================================================

# This script uses only base R.
# Run the whole script in RStudio. Each question prints the
# numerical answer and draws the required power curve.

options(digits = 6)

# ------------------------------------------------------------
# QUESTION 1
# Xbar ~ N(mu, 1/25), so SE = 1/5 = 0.2
# Reject H0: mu = 0 if Xbar is NOT between -0.3 and 0.3.
#
# Power(mu) = P_mu(Xbar < -0.3) + P_mu(Xbar > 0.3)
# ------------------------------------------------------------

cat("\n================ QUESTION 1 ================\n")

power1 <- function(mu) {
  pnorm((-0.3 - mu) / 0.2) +
    pnorm((0.3 - mu) / 0.2, lower.tail = FALSE)
}

mu1 <- seq(-1.5, 1.5, length.out = 1000)

cat("Power function:\n")
cat("beta(mu) = Phi((-0.3-mu)/0.2) + 1-Phi((0.3-mu)/0.2)\n")
cat("Power at mu = 0 =", power1(0), "\n")

plot(mu1, power1(mu1), type = "l", lwd = 2,
     xlab = expression(mu), ylab = "Power",
     main = "Q1: Power Curve",
     ylim = c(0, 1))
abline(v = 0, lty = 2)
abline(h = 0.05, lty = 3)


# ------------------------------------------------------------
# QUESTION 2
# X = number of heads ~ Binomial(20, p)
# H0: p = 0.5
# Reject if X < 5 or X > 15:
# Critical region = {0,1,2,3,4,16,17,18,19,20}
#
# Power(p) = P_p(X <= 4) + P_p(X >= 16)
# Level = Power(0.5)
# ------------------------------------------------------------

cat("\n================ QUESTION 2 ================\n")

power2 <- function(p) {
  pbinom(4, size = 20, prob = p) +
    pbinom(15, size = 20, prob = p, lower.tail = FALSE)
}

alpha2 <- power2(0.5)

cat("Critical region: X <= 4 OR X >= 16\n")
cat("Level of significance =", alpha2, "\n")

p2 <- seq(0, 1, length.out = 1001)

plot(p2, power2(p2), type = "l", lwd = 2,
     xlab = "p", ylab = "Power",
     main = "Q2: Power Curve",
     ylim = c(0, 1))
abline(v = 0.5, lty = 2)
abline(h = alpha2, lty = 3)


# ------------------------------------------------------------
# QUESTION 3
# Xi ~ Poisson(m), n = 10
# X = sum(Xi) ~ Poisson(10m)
# H0: m = 2 vs H1: m > 2
# Critical region: X > 25, i.e. X >= 26
#
# Power(m) = P_m(X >= 26) = 1 - P_m(X <= 25)
# Under H0: X ~ Poisson(20)
# ------------------------------------------------------------

cat("\n================ QUESTION 3 ================\n")

power3 <- function(m) {
  ppois(25, lambda = 10 * m, lower.tail = FALSE)
}

alpha3 <- power3(2)

cat("Critical region: X > 25 (equivalently X >= 26)\n")
cat("Type I error probability =", alpha3, "\n")

m3 <- seq(0.01, 6, length.out = 1000)

plot(m3, power3(m3), type = "l", lwd = 2,
     xlab = "m", ylab = "Power",
     main = "Q3: Power Curve",
     ylim = c(0, 1))
abline(v = 2, lty = 2)
abline(h = alpha3, lty = 3)


# ------------------------------------------------------------
# QUESTION 4
# Xi has exponential distribution:
# f(x) = (1/m) exp(-x/m), x >= 0
# H0: m = 9000 vs H1: m > 9000
# n = 10
# ------------------------------------------------------------

cat("\n================ QUESTION 4 ================\n")

m0_4 <- 9000
n4 <- 10
alpha4 <- 0.05

# ---------------- Q4(i): Based on U = min(X1,...,X10) ----------------
#
# U ~ Exponential(rate = n/m)
# P_m(U > u) = exp(-n*u/m)
#
# Since H1 is m > 9000, reject for large U.
# Choose c such that P_9000(U > c) = 0.05.
# c = -(m0/n) log(0.05)

c4_u <- -(m0_4 / n4) * log(alpha4)

power4_u <- function(m) {
  exp(-n4 * c4_u / m)
}

cat("\nQ4(i)\n")
cat("Critical region: U >", c4_u, "\n")
cat("Size =", power4_u(m0_4), "\n")

m4 <- seq(3000, 30000, length.out = 1000)

plot(m4, power4_u(m4), type = "l", lwd = 2,
     xlab = "m", ylab = "Power",
     main = "Q4(i): Power Curve Based on U",
     ylim = c(0, 1))
abline(v = m0_4, lty = 2)
abline(h = alpha4, lty = 3)


# ---------------- Q4(ii): Based on sample mean ----------------
#
# For exponential Xi:
# 2n*Xbar/m ~ Chi-square with 2n df.
#
# Under H0:
# 2n*Xbar/9000 ~ Chi-square(20)
#
# Reject for large Xbar. Find c such that P_H0(Xbar > c)=0.05.

df4 <- 2 * n4

c4_mean <- (m0_4 / (2 * n4)) * qchisq(0.95, df = df4)

power4_mean <- function(m) {
  pchisq((2 * n4 * c4_mean) / m,
         df = df4, lower.tail = FALSE)
}

cat("\nQ4(ii)\n")
cat("Critical region: Xbar >", c4_mean, "\n")
cat("Size =", power4_mean(m0_4), "\n")

plot(m4, power4_mean(m4), type = "l", lwd = 2,
     xlab = "m", ylab = "Power",
     main = "Q4(ii): Power Curve Based on Xbar",
     ylim = c(0, 1))
abline(v = m0_4, lty = 2)
abline(h = alpha4, lty = 3)


# ---------------- Q4(iii): Based on count Y ----------------
#
# Y = number of Xi for which Xi > 21949.4
#
# P_m(X > 21949.4) = exp(-21949.4/m) = q(m)
# Therefore Y ~ Binomial(10, q(m))
#
# Reject H0 if Y > 2, i.e. Y >= 3.

threshold4 <- 21949.4

power4_y <- function(m) {
  q <- exp(-threshold4 / m)
  pbinom(2, size = n4, prob = q, lower.tail = FALSE)
}

alpha4_y <- power4_y(m0_4)

cat("\nQ4(iii)\n")
cat("Critical region: Y > 2 (equivalently Y >= 3)\n")
cat("Type I error / size =", alpha4_y, "\n")

plot(m4, power4_y(m4), type = "l", lwd = 2,
     xlab = "m", ylab = "Power",
     main = "Q4(iii): Power Curve Based on Y",
     ylim = c(0, 1))
abline(v = m0_4, lty = 2)
abline(h = alpha4_y, lty = 3)


# ------------------------------------------------------------
# QUESTION 5
# Xi ~ N(mu, sigma^2 = 4), so sigma = 2.
#
# PART A:
# n = 100
# H0: mu = 0 vs H1: mu > 0
# alpha = 0.01
#
# Xbar ~ N(mu, 4/100) = N(mu, 0.2^2)
# UMP test rejects for large Xbar.
# Critical value:
# c = z_(0.99) * 0.2
#
# Find mu for power = 0.9.
#
# PART B:
# Zi = I(Xi > 0)
# Under mu:
# p(mu) = P(Xi > 0) = Phi(mu/2)
# Sum Zi ~ Binomial(n, p(mu))
# UMP test rejects for sufficiently large sum Zi.
# Find the smallest n for which exact binomial power >= 0.9
# at mu = 1, while size <= 0.01.
# ------------------------------------------------------------

cat("\n================ QUESTION 5 ================\n")

# Part A
n5 <- 100
sigma5 <- 2
alpha5 <- 0.01

z_alpha5 <- qnorm(1 - alpha5)
se5 <- sigma5 / sqrt(n5)
c5 <- z_alpha5 * se5

power5_a <- function(mu) {
  pnorm((c5 - mu) / se5, lower.tail = FALSE)
}

# Solve power(mu) = 0.9
mu5_power90 <- c5 - qnorm(0.9) * se5

cat("\nQ5(A)\n")
cat("Reject H0 if Xbar >", c5, "\n")
cat("Power function: 1 - Phi((c-mu)/0.2)\n")
cat("mu giving power 0.9 =", mu5_power90, "\n")

mu5a <- seq(-1, 1.8, length.out = 1000)

plot(mu5a, power5_a(mu5a), type = "l", lwd = 2,
     xlab = "mu", ylab = "Power",
     main = "Q5(A): Power Curve",
     ylim = c(0, 1))
abline(v = mu5_power90, lty = 2)
abline(h = 0.9, lty = 3)


# Part B
p_mu1 <- pnorm(1 / 2)

find_binomial_critical <- function(n, alpha = 0.01) {
  for (c in 0:n) {
    size <- pbinom(c - 1, size = n, prob = 0.5,
                   lower.tail = FALSE)
    if (size <= alpha) {
      return(list(c = c, size = size))
    }
  }
  return(NULL)
}

answer5b <- NULL

for (n in 1:1000) {
  cr <- find_binomial_critical(n, alpha5)

  if (!is.null(cr)) {
    power_at_mu1 <- pbinom(cr$c - 1, size = n,
                           prob = p_mu1, lower.tail = FALSE)

    if (power_at_mu1 >= 0.9) {
      answer5b <- list(
        n = n,
        critical_value = cr$c,
        size = cr$size,
        power = power_at_mu1
      )
      break
    }
  }
}

cat("\nQ5(B)\n")
cat("Under H0, Zi ~ Bernoulli(0.5)\n")
cat("Under mu = 1, p = Phi(0.5) =", p_mu1, "\n")
cat("Minimum n =", answer5b$n, "\n")
cat("Reject if sum(Zi) >=", answer5b$critical_value, "\n")
cat("Actual size =", answer5b$size, "\n")
cat("Power at mu = 1 =", answer5b$power, "\n")

# Power curve for the Bernoulli-based test
n5b <- answer5b$n
c5b <- answer5b$critical_value

power5_b <- function(mu) {
  p <- pnorm(mu / 2)
  pbinom(c5b - 1, size = n5b, prob = p,
         lower.tail = FALSE)
}

mu5b <- seq(-2, 2, length.out = 1000)

plot(mu5b, power5_b(mu5b), type = "l", lwd = 2,
     xlab = "mu", ylab = "Power",
     main = "Q5(B): Power Curve",
     ylim = c(0, 1))
abline(v = 0, lty = 2)
abline(v = 1, lty = 2)
abline(h = 0.9, lty = 3)


# ------------------------------------------------------------
# QUESTION 6
# Xi ~ N(0, sigma^2), n = 5
# H0: sigma^2 = 1 vs H1: sigma^2 > 1
#
# TEST (i):
# Reject if sum Xi^2 > c1.
#
# Under H0, sum Xi^2 ~ Chi-square(5).
# Choose c1 so size = 0.05.
#
# TEST (ii):
# S = number of Xi outside (-0.6745, 0.6745)
# Reject if S > c2.
#
# Under H0, S ~ Binomial(5,p0), where
# p0 = P(|Z| > 0.6745).
# Choose integer c2 giving size closest to 0.05.
# ------------------------------------------------------------

cat("\n================ QUESTION 6 ================\n")

n6 <- 5
c1_6 <- qchisq(0.95, df = n6)

p0_6 <- 2 * pnorm(0.6745, lower.tail = FALSE)

# Possible integer thresholds
c2_candidates <- 0:n6
sizes_c2 <- sapply(c2_candidates, function(c2) {
  pbinom(c2, size = n6, prob = p0_6, lower.tail = FALSE)
})

best_index <- which.min(abs(sizes_c2 - 0.05))
c2_6 <- c2_candidates[best_index]
size2_6 <- sizes_c2[best_index]

cat("\nQ6(i)\n")
cat("Critical region: sum(Xi^2) >", c1_6, "\n")
cat("Size =", pchisq(c1_6, df = 5, lower.tail = FALSE), "\n")

cat("\nQ6(ii)\n")
cat("p0 = P(|Z| > 0.6745) =", p0_6, "\n")
cat("Chosen c2 =", c2_6, "\n")
cat("Critical region: S >", c2_6, "\n")
cat("Size =", size2_6, "\n")

# Power of Test (i)
power6_1 <- function(sigma2) {
  pchisq(c1_6 / sigma2, df = n6, lower.tail = FALSE)
}

# Power of Test (ii)
power6_2 <- function(sigma2) {
  p <- 2 * pnorm(0.6745 / sqrt(sigma2), lower.tail = FALSE)
  pbinom(c2_6, size = n6, prob = p, lower.tail = FALSE)
}

sigma2_6 <- seq(0.2, 5, length.out = 1000)

plot(sigma2_6, power6_1(sigma2_6), type = "l", lwd = 2,
     xlab = expression(sigma^2), ylab = "Power",
     main = "Q6: Power Curves of Both Tests",
     ylim = c(0, 1))
lines(sigma2_6, power6_2(sigma2_6), lwd = 2, lty = 2)
abline(v = 1, lty = 2)
legend("bottomright",
       legend = c("Test 1: sum Xi^2", "Test 2: S"),
       lty = c(1, 2), lwd = 2, bty = "n")

cat("\nComment: Test 1 uses the full squared observations and is much\n")
cat("more informative here. Test 2 reduces each observation to whether\n")
cat("it falls outside a fixed interval, so it generally loses information.\n")


# ------------------------------------------------------------
# QUESTION 7
# Xi ~ N(mu, 100), n = 12
# Therefore Xbar ~ N(mu, 100/12)
# H0: mu = 65, with the intended right-sided alternative mu > 65.
#
# Test A:
# WA = {Xbar > c1}
# Size 0.05 gives c1 = 65 + z_.95 * 10/sqrt(12)
#
# Test B:
# WB = {62.5 < Xbar < c2}
# Choose c2 so P_65(62.5 < Xbar < c2) = 0.05.
#
# Test C:
# WC = {Xbar > 62.5}
# Its level under H0 is much larger than 0.05.
# ------------------------------------------------------------

cat("\n================ QUESTION 7 ================\n")

n7 <- 12
mu0_7 <- 65
sigma7 <- 10
se7 <- sigma7 / sqrt(n7)

# (a) c1
c1_7 <- mu0_7 + qnorm(0.95) * se7

# (a) c2
lower7 <- 62.5
z_lower7 <- (lower7 - mu0_7) / se7
upper_probability7 <- pnorm(z_lower7) + 0.05
z_upper7 <- qnorm(upper_probability7)
c2_7 <- mu0_7 + z_upper7 * se7

# Level of test C
level_C7 <- pnorm(z_lower7, lower.tail = FALSE)

cat("\nQ7(a)\n")
cat("c1 =", c1_7, "\n")
cat("c2 =", c2_7, "\n")
cat("Size of Test A =", 0.05, "\n")
cat("Size of Test B =", pnorm((c2_7 - mu0_7) / se7) -
      pnorm((lower7 - mu0_7) / se7), "\n")
cat("Level of Test C =", level_C7, "\n")

# Power functions
power7_A <- function(mu) {
  pnorm((c1_7 - mu) / se7, lower.tail = FALSE)
}

power7_B <- function(mu) {
  pnorm((c2_7 - mu) / se7) -
    pnorm((lower7 - mu) / se7)
}

power7_C <- function(mu) {
  pnorm((lower7 - mu) / se7, lower.tail = FALSE)
}

mu7 <- seq(55, 80, length.out = 1000)

plot(mu7, power7_A(mu7), type = "l", lwd = 2,
     xlab = expression(mu), ylab = "Power",
     main = "Q7: Power Curves of Tests A, B and C",
     ylim = c(0, 1))
lines(mu7, power7_B(mu7), lwd = 2, lty = 2)
lines(mu7, power7_C(mu7), lwd = 2, lty = 3)
abline(v = mu0_7, lty = 2)
legend("right",
       legend = c("Test A: Xbar > c1",
                  "Test B: 62.5 < Xbar < c2",
                  "Test C: Xbar > 62.5"),
       lty = c(1, 2, 3), lwd = 2, bty = "n")

cat("\nQ7(b) Comment:\n")
cat("Test A is the most powerful right-tailed test for H1: mu > 65.\n")
cat("Test B has size 0.05 but its rejection region is concentrated just\n")
cat("above 62.5, so its power decreases as mu moves substantially above 65.\n")

cat("\nQ7(c) Comment:\n")
cat("Test C has level approximately", level_C7,
    "which is far greater than 0.05, so it is not a level-0.05 test.\n")


# ------------------------------------------------------------
# QUESTION 8
# Xi ~ N(mu, sigma^2), sigma = 1.2 is known
# H0: mu = 75 vs H1: mu > 75
#
# Use the one-sided Z test.
# Assuming a 5% significance level:
# Reject H0 if
# Xbar > 75 + z_.95 * 1.2/sqrt(n)
#
# Want power = 0.95 when mu = 76.
#
# n = [sigma*(z_.95 + z_.95)/delta]^2
#   = [(1.2)*(1.645+1.645)/1]^2
#   = 15.63, so n = 16.
# ------------------------------------------------------------

cat("\n================ QUESTION 8 ================\n")

mu0_8 <- 75
mu1_8 <- 76
sigma8 <- 1.2
alpha8 <- 0.05
power_target8 <- 0.95

z_alpha8 <- qnorm(1 - alpha8)
z_power8 <- qnorm(power_target8)

n8_exact <- (sigma8 *
              (z_alpha8 + z_power8) /
              (mu1_8 - mu0_8))^2

n8 <- ceiling(n8_exact)

cat("Test: one-sided Z test for known sigma = 1.2\n")
cat("Reject H0 for sufficiently large Xbar.\n")
cat("Required theoretical n =", n8_exact, "\n")
cat("Required integer sample size n =", n8, "\n")

# Verify actual power at n = 16
critical8 <- mu0_8 + z_alpha8 * sigma8 / sqrt(n8)

power8_actual <- pnorm(
  (critical8 - mu1_8) / (sigma8 / sqrt(n8)),
  lower.tail = FALSE
)

cat("Critical value for Xbar when n =", n8, ":", critical8, "\n")
cat("Actual power at mu = 76 =", power8_actual, "\n")


# ------------------------------------------------------------
# FINAL SUMMARY
# ------------------------------------------------------------

cat("\n============================================================\n")
cat("FINAL ANSWERS SUMMARY\n")
cat("============================================================\n")

cat("\nQ1: Power(mu) = Phi((-0.3-mu)/0.2) + 1-Phi((0.3-mu)/0.2)\n")

cat("\nQ2: Critical region: X <= 4 or X >= 16\n")
cat("    Level =", alpha2, "\n")

cat("\nQ3: Critical region: X > 25\n")
cat("    Type I error =", alpha3, "\n")

cat("\nQ4(i): U >", c4_u, "\n")
cat("Q4(ii): Xbar >", c4_mean, "\n")
cat("Q4(iii): Y > 2\n")

cat("\nQ5(A): Reject if Xbar >", c5, "\n")
cat("      Power 0.9 occurs at mu =", mu5_power90, "\n")
cat("Q5(B): n =", answer5b$n,
    ", reject if sum(Zi) >=", answer5b$critical_value, "\n")

cat("\nQ6(i): c1 =", c1_6, "\n")
cat("Q6(ii): c2 =", c2_6,
    ", with size =", size2_6, "\n")

cat("\nQ7: c1 =", c1_7, ", c2 =", c2_7, "\n")
cat("    Test C level =", level_C7, "\n")

cat("\nQ8: One-sided Z test; required sample size =", n8, "\n")

cat("\n================ END OF SCRIPT ================\n")
