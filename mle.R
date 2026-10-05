# ============================================================
# METHOD OF MAXIMUM LIKELIHOOD FOR POINT ESTIMATION
# SINGLE R SCRIPT
# ============================================================


# ============================================================
# QUESTION 1
# ============================================================
# Total balls = 94
# Sample = 10
# White balls observed = 6
# Black balls observed = 4
#
# Find MLE of number of white balls.


N <- 94
n <- 10
x <- 6

# Let W = number of white balls.
#
# Likelihood:
# L(W) = choose(W,6)*choose(94-W,4) / choose(94,10)
#
# Try all possible values of W.

W <- 6:(N - 4)

likelihood_Q1 <- choose(W, x) *
  choose(N - W, n - x) /
  choose(N, n)

W_MLE <- W[which.max(likelihood_Q1)]

cat("Q1 MLE of number of white balls =", W_MLE, "\n")


# ============================================================
# QUESTION 2
# ============================================================
# Population size = N (unknown)
# Number having character A = 9225
# Sample size = 150
# Number having A in sample = 24
#
# Find MLE of N.


A <- 9225
n <- 150
x <- 24

# Likelihood is hypergeometric:
#
# P(X=x) =
# choose(A,x) * choose(N-A,n-x) / choose(N,n)
#
# Try possible values of N.

N_values <- (A + 1):100000

logL_Q2 <- sapply(N_values, function(N) {
  
  lchoose(A, x) +
    lchoose(N - A, n - x) -
    lchoose(N, n)
})

N_MLE <- N_values[which.max(logL_Q2)]

cat("Q2 MLE of N =", N_MLE, "\n")


# ============================================================
# QUESTION 3
# ============================================================
# 25 balls drawn with replacement
# 11 white
# Therefore black = 14
#
# P = proportion of black balls
#
# Possible values:
# 0.30, 0.35, ..., 0.70


n <- 25
black <- 14

P_values <- seq(0.30, 0.70, by = 0.05)

# Likelihood
likelihood_Q3 <- dbinom(
  black,
  size = n,
  prob = P_values
)

result_Q3 <- data.frame(
  P = P_values,
  Likelihood = likelihood_Q3
)

print(result_Q3)

P_MLE <- P_values[which.max(likelihood_Q3)]

cat("Q3 MLE of P =", P_MLE, "\n")


# ------------------------------------------------------------
# QUESTION 3 - WITHOUT REPLACEMENT
# ------------------------------------------------------------
# Total balls = 100
# 25 balls drawn without replacement
# 11 white
# Therefore 14 black
#
# P can take the specified values.
#
# For each P, number of black balls = 100*P.
#
# Hypergeometric likelihood.


N <- 100
n <- 25
black_sample <- 14

P_values <- seq(0.30, 0.70, by = 0.05)

black_population <- N * P_values

likelihood_without_replacement <- sapply(
  black_population,
  function(B) {
    
    choose(B, black_sample) *
      choose(N - B, n - black_sample) /
      choose(N, n)
  }
)

result_Q3_without <- data.frame(
  P = P_values,
  Black_Balls = black_population,
  Likelihood = likelihood_without_replacement
)

print(result_Q3_without)

P_MLE_without <- P_values[
  which.max(likelihood_without_replacement)
]

cat(
  "Q3 MLE of P without replacement =",
  P_MLE_without,
  "\n"
)


# ============================================================
# QUESTION 4
# ============================================================
# Uniform / rectangular distribution:
#
# f(x) = 1/theta, 0 <= x <= theta
#
# Sample:
# 1.45, 0.68, 1.10, 0.90, 1.40,
# 0.16, 0.36, 0.54, 0.76, 1.80
#
# MLE of theta = maximum observation


x4 <- c(
  1.45, 0.68, 1.10, 0.90, 1.40,
  0.16, 0.36, 0.54, 0.76, 1.80
)

n4 <- length(x4)

theta_hat_Q4 <- max(x4)

cat("\nQ4 MLE of theta =", theta_hat_Q4, "\n")


# ------------------------------------------------------------
# Standard error of MLE
# ------------------------------------------------------------
# For Uniform(0,theta):
#
# Var(X_(n)) =
# theta^2 * n / ((n+1)^2 * (n+2))
#
# Estimate theta by theta_hat.


SE_Q4 <- theta_hat_Q4 *
  sqrt(
    n4 /
      ((n4 + 1)^2 * (n4 + 2))
  )

cat("Q4 Estimated standard error =", SE_Q4, "\n")


# ------------------------------------------------------------
# MVUE of theta
# ------------------------------------------------------------
# For Uniform(0,theta):
#
# E(X_(n)) = n*theta/(n+1)
#
# Therefore unbiased estimator:
#
# theta_MVUE = (n+1)/n * X_(n)


theta_MVUE_Q4 <- ((n4 + 1) / n4) *
  theta_hat_Q4

cat("Q4 MVUE of theta =", theta_MVUE_Q4, "\n")


# ============================================================
# QUESTION 5
# ============================================================
# Cauchy population
#
# Observations:
# 5.47, 9.29, 5.02, 5.26, 3.86,
# 5.22, 4.58, 5.50, 1.11, 1.79, 3.34
#
# Use iterative method to obtain MLE of theta.
#
# The exact Cauchy density in the uploaded document is:
# not completely visible in the extracted text.
#
# For standard Cauchy location parameter theta:
#
# f(x;theta) = 1/[pi*(1+(x-theta)^2)]
#
# Score equation:
#
# sum[(x_i-theta)/(1+(x_i-theta)^2)] = 0
#
# We solve it iteratively using Newton-Raphson.


x5 <- c(
  5.47, 9.29, 5.02, 5.26, 3.86,
  5.22, 4.58, 5.50, 1.11, 1.79, 3.34
)


# Score function
score_Q5 <- function(theta) {
  
  sum(
    (x5 - theta) /
      (1 + (x5 - theta)^2)
  )
}


# Derivative of score
score_derivative_Q5 <- function(theta) {
  
  sum(
    ((x5 - theta)^2 - 1) /
      (1 + (x5 - theta)^2)^2
  )
}


# Newton-Raphson iteration

theta <- median(x5)

cat("\nQ5 Initial value =", theta, "\n")

for (i in 1:100) {
  
  theta_new <- theta -
    score_Q5(theta) /
    score_derivative_Q5(theta)
  
  cat(
    "Iteration", i,
    "Theta =", theta_new, "\n"
  )
  
  if (abs(theta_new - theta) < 1e-8) {
    break
  }
  
  theta <- theta_new
}

theta_MLE_Q5 <- theta_new

cat("Q5 MLE of theta =", theta_MLE_Q5, "\n")


# ============================================================
# QUESTION 6
# ============================================================
# Number of eggs follows Poisson distribution.
#
# Number of eggs:
# 1 2 3 4 5 6 7 8 9
#
# Frequencies:
# 22 18 18 11 9 6 3 0 1
#
# Number of flower heads with 0 eggs is unavailable.
#
# Need MLE of Poisson mean.


eggs <- 1:9

frequency <- c(
  22, 18, 18, 11, 9,
  6, 3, 0, 1
)

# Total observed flower heads
n_observed <- sum(frequency)

# Total number of eggs
total_eggs <- sum(eggs * frequency)

cat("\nQ6 Observed flower heads =", n_observed, "\n")
cat("Q6 Total eggs =", total_eggs, "\n")


# ------------------------------------------------------------
# Zero-truncated Poisson likelihood
# ------------------------------------------------------------
# Since zero frequency is unavailable:
#
# P(X=x | X>0) =
# [exp(-lambda)*lambda^x/x!] /
# [1-exp(-lambda)]
#
# Log likelihood:
#
# l(lambda) =
# sum(f*x)*log(lambda)
# - n*lambda
# - sum(f*log(x!))
# - n*log(1-exp(-lambda))


logL_Q6 <- function(lambda) {
  
  if (lambda <= 0) {
    return(-Inf)
  }
  
  total_eggs * log(lambda) -
    n_observed * lambda -
    sum(frequency * lfactorial(eggs)) -
    n_observed * log(1 - exp(-lambda))
}


# Optimize

MLE_Q6 <- optimize(
  logL_Q6,
  interval = c(0.01, 20),
  maximum = TRUE
)

lambda_hat_Q6 <- MLE_Q6$maximum

cat("Q6 MLE of Poisson mean =", lambda_hat_Q6, "\n")


# ------------------------------------------------------------
# Standard error
# ------------------------------------------------------------
# Numerical second derivative / observed information.


h <- 0.0001

second_derivative <- (
  logL_Q6(lambda_hat_Q6 + h) -
    2 * logL_Q6(lambda_hat_Q6) +
    logL_Q6(lambda_hat_Q6 - h)
) / h^2

SE_Q6 <- sqrt(-1 / second_derivative)

cat("Q6 Estimated standard error =", SE_Q6, "\n")


# ============================================================
# QUESTION 7
# ============================================================
# Electron tube lifetimes:
#
# 980, 1020, 995, 1015, 990,
# 1030, 975, 950, 1050, 870
#
# The exact distribution/form in the document is not visible
# in the extracted text.
#
# Therefore the exact MLE cannot be determined safely without
# the missing distribution.
#
# If the intended model is Exponential(theta):
#
# theta_hat = mean(X)
#
# and
#
# SE(theta_hat) = theta_hat/sqrt(n)


x7 <- c(
  980, 1020, 995, 1015, 990,
  1030, 975, 950, 1050, 870
)

n7 <- length(x7)

theta_hat_Q7 <- mean(x7)

SE_Q7 <- theta_hat_Q7 / sqrt(n7)

cat("\nQ7 Assuming exponential distribution:\n")
cat("MLE of theta =", theta_hat_Q7, "\n")
cat("Estimated standard error =", SE_Q7, "\n")


# Probability of surviving at least 100 hours
# For exponential distribution:
#
# P(X >= x) = exp(-x/theta)


x_survive <- 100

survival_probability_Q7 <- exp(
  -x_survive / theta_hat_Q7
)

cat(
  "Estimated P(X >= 100) =",
  survival_probability_Q7,
  "\n"
)


# ============================================================
# QUESTION 8
# ============================================================
# 10 lamps
# Test duration = 20 hours
#
# 2 lamps survived 20 hours.
#
# Failure times of remaining 8:
#
# 9.8, 15.8, 17.2, 11.2,
# 13.8, 18.9, 14.6, 19.6
#
# Assume exponential distribution.
#
# Under Type-I censoring:
#
# Total time on test =
# sum(failure times) +
# number survived * censoring time
#
# MLE of theta =
# Total time on test / total number of lamps


failure_times <- c(
  9.8, 15.8, 17.2, 11.2,
  13.8, 18.9, 14.6, 19.6
)

n_lamps <- 10
n_survived <- 2
censor_time <- 20

total_time <- sum(failure_times) +
  n_survived * censor_time

theta_hat_Q8 <- total_time / n_lamps

cat("\nQ8 Total time on test =", total_time, "\n")
cat("Q8 Estimated mean life theta =", theta_hat_Q8, "\n")


# ------------------------------------------------------------
# If all 10 lamps survived 20 hours
# ------------------------------------------------------------
# Total time on test = 10*20
#
# MLE = total time / n


all_survived_time <- n_lamps * censor_time

theta_all_survived <- all_survived_time / n_lamps

cat(
  "Q8 If all lamps survived, estimated theta =",
  theta_all_survived,
  "\n"
)


# ============================================================
# END OF MLE PROBLEM SET
# ============================================================