# ============================================================
# LIKELIHOOD RATIO TEST AND HYPOTHESIS TESTING
# SIMPLE R CODE
# ============================================================


# ============================================================
# QUESTION 1
# ============================================================

# Xi ~ Exponential(theta)
#
# H0: theta = theta0
# H1: theta = theta1 > theta0
#
# Reject H0 if:
# sum(Xi) > C
#
# Under H0:
# 2*sum(Xi)/theta0 ~ Chi-square(2n)
#
# Therefore:
# C = (theta0/2)*qchisq(1-alpha,2n)
#
# The test is unbiased because power increases with theta.


x = c(0.48,2.29,0.27,0.08,0.34,
      0.63,1.17,0.58,0.67,1.07)

n = length(x)

sum_x = sum(x)

theta0 = 1
alpha = 0.05

C = (theta0/2)*qchisq(1-alpha,2*n)

sum_x
C

# Decision:
# Reject H0 if sum_x > C


# ============================================================
# QUESTION 2
# ============================================================

# Xi ~ Exponential(location = a, scale = 1)
#
# H0: a = 0
# H1: a != 0
#
# Likelihood ratio:
#
# lambda = exp(-n*X(1))
#
# Reject H0 if X(1) > C
#
# Under H0:
# X(1) ~ Exponential(rate = n)
#
# P(X(1)>C) = alpha
#
# Therefore:
# C = -log(alpha)/n


x = c(3,2,6,4,1,8,5,6)

n = length(x)

xmin = min(x)

alpha = 0.05

C = -log(alpha)/n

lambda = exp(-n*xmin)

p_value = exp(-n*xmin)

xmin
C
lambda
p_value

# Decision:
# Reject H0 if xmin > C


# ============================================================
# QUESTION 3
# ============================================================

# H0: p = 0.60
# H1: p > 0.60
#
# n = 40
# x = 30
#
# Exact binomial test


n = 40

x = 30

p0 = 0.60

result = binom.test(x,n,p=p0,alternative="greater")

result$p.value

# Decision:
# Reject H0 if p-value < 0.05


# ============================================================
# QUESTION 4
# ============================================================

# Poisson rate test
#
# H0: lambda = 2.5
# H1: lambda != 2.5
#
# Data collected over 10 hours.


x = c(3,2,4,3,1,
      2,3,2,5,3)

total = sum(x)

time = length(x)

lambda0 = 2.5

result = poisson.test(total,
                      T=time,
                      r=lambda0,
                      alternative="two.sided")

total
total/time
result$p.value

# Decision:
# Reject H0 if p-value < 0.05


# ============================================================
# QUESTION 5
# ============================================================

# One-sample Z-test
#
# H0: mu = 50
# H1: mu != 50
#
# sigma^2 = 1
# Therefore sigma = 1


x = c(49.2,50.5,51.1,48.9,
      50.3,49.7,50.8,49.5,
      50.0,51.3,49.8,50.6)

n = length(x)

xbar = mean(x)

mu0 = 50

sigma = 1

# Z statistic:
#
# Z = (xbar-mu0)/(sigma/sqrt(n))

z = (xbar-mu0)/(sigma/sqrt(n))

# Two-sided p-value

p_value = 2*(1-pnorm(abs(z)))

xbar
z
p_value

# Decision:
# Reject H0 if p-value < 0.05


# ============================================================
# END
# ============================================================

