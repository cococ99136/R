#Randomized Tests
# Problem 1: Poisson Distribution UMP Randomized Test[cite: 2]
#
# 1. Setup[cite: 2]
# Let X1, X2, ..., X10 be a random sample of size n = 10 drawn from a Poisson distribution[cite: 2]
# with parameter lambda.[cite: 2]
#
# o Null Hypothesis (H0): lambda = 1.2[cite: 2]
#
# o Alternative Hypothesis (H1): lambda > 1.2[cite: 2]
#
# By the factorization theorem and the Monotone Likelihood Ratio (MLR) property of the Poisson[cite: 2]
# distribution, the sufficient test statistic is the sample sum: T = sum_{i=1}^{10} X_i.[cite: 2]
# Under H0, T ~ Poisson(n * lambda0) => T ~ Poisson(10 * 1.2) => T ~ Poisson(12).[cite: 2]
#
# Because this is a right-tailed test (since H1 : lambda > 1.2), the Uniformly Most Powerful (UMP)[cite: 2]
# randomized test function phi(t) takes the form:[cite: 2]
#
# o phi(t) = 1, if T > c (Reject H0)[cite: 2]
# o phi(t) = gamma, if T = c (Reject H0 with probability gamma)[cite: 2]
# o phi(t) = 0, if T < c (Do not reject H0)[cite: 2]
#
# Where the critical value c and randomization probability gamma are determined by the size of the test[cite: 2]
# alpha = 0.05:[cite: 2]
# E_H0[phi(T)] = P_H0(T > c) + gamma * P_H0(T = c) = alpha[cite: 2]
# Setup for Problem 1
alpha <- 0.05
n <- 10
lambda_0 <- 1.2
lambda_T <- n * lambda_0 # T follows Poisson(12) under H0

# Find the critical value c
# We want the smallest c such that P(T > c) <= alpha
c <- qpois(1 - alpha, lambda_T)
if (1 - ppois(c, lambda_T) > alpha) {
  c <- c + 1
}

# Calculate probabilities needed for gamma
prob_greater_than_c <- 1 - ppois(c, lambda_T)
prob_equal_to_c <- dpois(c, lambda_T)

# Calculate gamma
gamma <- (alpha - prob_greater_than_c) / prob_equal_to_c

# Output results
cat("Critical Value (c):", c, "\n")
cat("Randomization Probability (gamma):", round(gamma, 4), "\n")

# Setup for Problem 2
alpha <- 0.05
N <- 10
K_0 <- 10 * 0.5 # Number of red pencils under H0
n_draw <- 4

# Cumulative probabilities under H0
# phyper(q, m (white balls), n (black balls), k (draws))
cum_probs <- phyper(0:4, K_0, N - K_0, n_draw)

# Find c for left-tailed test: smallest value where CDF > alpha
# Since R indices start at 1 (for X=0), we subtract 1
c <- min(which(cum_probs > alpha)) - 1

# Probabilities for gamma calculation
prob_less_than_c <- ifelse(c == 0, 0, phyper(c - 1, K_0, N - K_0, n_draw))
prob_equal_to_c <- dhyper(c, K_0, N - K_0, n_draw)

# Calculate gamma
gamma <- (alpha - prob_less_than_c) / prob_equal_to_c

cat("Critical Value (c):", c, "\n")
cat("Randomization Probability (gamma):", round(gamma, 4), "\n")

# 3. Calculations[cite: 3]
# Using the Poisson(12) distribution, we look for c such that P(T > c) <= 0.05, which means[cite: 3]
# P(T <= c) >= 0.95.[cite: 3]
#
# o P(T <= 17) approx 0.9370 (which is < 0.95)[cite: 3]
# o P(T <= 18) approx 0.9626 (which is >= 0.95)[cite: 3]
#
# Therefore, c = 18.[cite: 3]
#
# o P_H0(T > 18) = 1 - P(T <= 18) = 1 - 0.9626 = 0.0374[cite: 3]
# o P_H0(T = 18) = P(T <= 18) - P(T <= 17) = 0.9626 - 0.9370 = 0.0256[cite: 3]
#
# Now, solve for gamma:[cite: 3]
# 0.0374 + gamma(0.0256) = 0.05[cite: 3]
# gamma = (0.05 - 0.0374) / 0.0256 = 0.0126 / 0.0256 approx 0.4922[cite: 3]
#
# 4. Conclusion and Interpretation[cite: 3]
# The UMP randomized test procedure is as follows:[cite: 3]
# Calculate the sum of the 10 observations, T. If T > 18, reject H0. If T < 18, fail to reject H0.[cite: 3]
# If exactly T = 18, we draw a random number U from a Uniform(0, 1) distribution. If U <[cite: 3]
# 0.4922, we reject H0; otherwise, we fail to reject H0.[cite: 3]




# Problem 2: Proportions Randomized Test[cite: 4]
#
# 1. Setup[cite: 4]
# A box contains N = 10 pencils. The total number of red pencils in the box is K = 10 * pi. A[cite: 4]
# sample of n = 4 pencils is drawn at random without replacement.[cite: 4]
# Let X be the number of red pencils drawn by the children.[cite: 4]
#
# o Null Hypothesis (H0): pi = 0.5 (which means K = 5 red pencils in the box)[cite: 4]
#
# o Alternative Hypothesis (H1): pi < 0.5 (which means K < 5)[cite: 4]
#
# Under H0, X follows a Hypergeometric distribution: X ~ Hypergeometric(N = 10, K = 5, n = 4).[cite: 4]
# Since H1 is a left-tailed alternative (pi < 0.5), the randomized test rejects H0 for small values[cite: 4]
# of X. The test function phi(x) is:[cite: 4]
#
# o phi(x) = 1, if X < c (Reject H0)[cite: 4]
# o phi(x) = gamma, if X = c (Reject H0 with probability gamma)[cite: 4]
# o phi(x) = 0, if X > c (Do not reject H0)[cite: 4]
#
# Where P_H0(X < c) + gamma * P_H0(X = c) = alpha = 0.05.[cite: 4]


# Setup for Problem 2
alpha <- 0.05
N <- 10
K_0 <- 10 * 0.5 # Number of red pencils under H0
n_draw <- 4

# Cumulative probabilities under H0
# phyper(q, m (white balls), n (black balls), k (draws))
cum_probs <- phyper(0:4, K_0, N - K_0, n_draw)

# Find c for left-tailed test: smallest value where CDF > alpha
# Since R indices start at 1 (for X=0), we subtract 1
c <- min(which(cum_probs > alpha)) - 1

# Probabilities for gamma calculation
prob_less_than_c <- ifelse(c == 0, 0, phyper(c - 1, K_0, N - K_0, n_draw))
prob_equal_to_c <- dhyper(c, K_0, N - K_0, n_draw)

# Calculate gamma
gamma <- (alpha - prob_less_than_c) / prob_equal_to_c

cat("Critical Value (c):", c, "\n")
cat("Randomization Probability (gamma):", round(gamma, 4), "\n")

# 3. Calculations[cite: 5]
# Let's find the probabilities for X under H0 using the Hypergeometric formula P(X = x) =[cite: 5]
# ((5 choose x)(5 choose 4-x)) / (10 choose 4):[cite: 5]
#
# o P(X = 0) = ((5 choose 0)(5 choose 4)) / 210 = (1 * 5) / 210 approx 0.0238[cite: 5]
#
# o P(X = 1) = ((5 choose 1)(5 choose 3)) / 210 = (5 * 10) / 210 approx 0.2381[cite: 5]
#
# We check the cumulative probabilities for alpha = 0.05:[cite: 5]
#
# o P(X < 1) = P(X = 0) = 0.0238 <= 0.05[cite: 5]
#
# o P(X < 2) = P(X = 0) + P(X = 1) = 0.2619 > 0.05[cite: 5]
#
# Thus, the critical value is c = 1.[cite: 5]
# Now, solve for gamma:[cite: 5]
# P(X < 1) + gamma * P(X = 1) = 0.05[cite: 5]
# 0.0238 + gamma(0.2381) = 0.05[cite: 5]
# gamma = (0.05 - 0.0238) / 0.2381 approx 0.1100[cite: 5]
#
# 4. Conclusions and Interpretations[cite: 5]
# The test procedure requires us to count the number of red pencils drawn (X). If no red pencils[cite: 5]
# are drawn (X = 0), we reject H0. If 2 or more red pencils are drawn (X >= 2), we do not reject[cite: 5]
# H0. If exactly 1 red pencil is drawn (X = 1), we reject H0 with a probability of 0.11.[cite: 5]
#
# What is your conclusion if only one of the children has a red pencil?[cite: 5]
# If exactly one child has a red pencil (X = 1), the data falls exactly on the critical boundary of[cite: 5]
# our randomized test. Therefore, we do not make a deterministic decision. Instead, we must[cite: 5]
# perform a randomization step: we generate a random number U from a Uniform(0, 1)[cite: 5]
# distribution. If U <= 0.11, we reject the null hypothesis and conclude the proportion of red[cite: 5]
# pencils is less than 0.5. If U > 0.11, we fail to reject the null hypothesis.[cite: 5]
