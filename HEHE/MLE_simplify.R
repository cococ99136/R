# ============================================================
# METHOD OF MAXIMUM LIKELIHOOD
# SIMPLE R CODE
# ============================================================


# ============================================================
# QUESTION 1
# ============================================================

# Total balls = 94
# Sample size = 10
# White balls observed = 6
# Black balls observed = 4
#
# Find MLE of number of white balls W.
#
# Likelihood:
# L(W) = choose(W,6)*choose(94-W,4)/choose(94,10)

N = 94
n = 10
x = 6

W = 6:(N-4)

likelihood = choose(W,x)*choose(N-W,n-x)/choose(N,n)

W_MLE = W[which.max(likelihood)]

W_MLE


# ============================================================
# QUESTION 2
# ============================================================

# Number having character A = 9225
# Sample size = 150
# Number having A in sample = 24
#
# Find MLE of population size N.
#
# Likelihood:
# L(N) = choose(9225,24)*choose(N-9225,126)/choose(N,150)

A = 9225
n = 150
x = 24

N = (A+1):100000

logL = lchoose(A,x) +
       lchoose(N-A,n-x) -
       lchoose(N,n)

N_MLE = N[which.max(logL)]

N_MLE


# ============================================================
# QUESTION 3
# ============================================================

# 25 balls drawn with replacement
# 11 white and 14 black
#
# P = proportion of black balls
#
# Possible values:
# 0.30, 0.35, ..., 0.70
#
# X ~ Binomial(25,P)

n = 25
black = 14

P = seq(0.30,0.70,by=0.05)

likelihood = dbinom(black,
                    size=n,
                    prob=P)

P_MLE = P[which.max(likelihood)]

P_MLE


# ------------------------------------------------------------
# QUESTION 3 WITHOUT REPLACEMENT
# ------------------------------------------------------------

# Total balls = 100
# Sample = 25
# Black balls observed = 14
#
# P = proportion of black balls
# Number of black balls in population = 100*P
#
# Hypergeometric likelihood

N = 100
n = 25
black = 14

P = seq(0.30,0.70,by=0.05)

B = N*P

likelihood = choose(B,black) *
             choose(N-B,n-black) /
             choose(N,n)

P_MLE = P[which.max(likelihood)]

P_MLE


# ============================================================
# QUESTION 4
# ============================================================

# X ~ Uniform(0,theta)
#
# Observations:
# 1.45, 0.68, 1.10, 0.90, 1.40,
# 0.16, 0.36, 0.54, 0.76, 1.80
#
# MLE:
# theta_hat = maximum observation

x = c(1.45,0.68,1.10,0.90,1.40,
      0.16,0.36,0.54,0.76,1.80)

n = length(x)

theta_hat = max(x)

theta_hat


# Standard error:
#
# Var(X_(n)) =
# theta^2*n / ((n+1)^2*(n+2))
#
# Estimate theta by theta_hat.

SE = theta_hat *
     sqrt(n/((n+1)^2*(n+2)))

SE


# MVUE:
#
# E(X_(n)) = n*theta/(n+1)
#
# Therefore:
#
# theta_MVUE = (n+1)/n * X_(n)

theta_MVUE = ((n+1)/n)*theta_hat

theta_MVUE


# ============================================================
# QUESTION 5
# ============================================================

# Cauchy location parameter theta
#
# Observations:
# 5.47, 9.29, 5.02, 5.26, 3.86,
# 5.22, 4.58, 5.50, 1.11, 1.79, 3.34
#
# Score equation:
#
# sum[(x-theta)/(1+(x-theta)^2)] = 0
#
# Use Newton-Raphson.


x = c(5.47,9.29,5.02,5.26,3.86,
      5.22,4.58,5.50,1.11,1.79,3.34)

score = function(theta)
{
  sum((x-theta)/(1+(x-theta)^2))
}

derivative = function(theta)
{
  sum(((x-theta)^2-1)/(1+(x-theta)^2)^2)
}


# Starting value

theta = median(x)


# Newton-Raphson

for(i in 1:100)
{
  theta_new = theta - score(theta)/derivative(theta)

  if(abs(theta_new-theta)<0.000001)
    break

  theta = theta_new
}

theta_MLE = theta_new

theta_MLE


# ============================================================
# QUESTION 6
# ============================================================

# Eggs follow Poisson distribution.
#
# Eggs:
# 1 2 3 4 5 6 7 8 9
#
# Frequencies:
# 22 18 18 11 9 6 3 0 1
#
# Number of eggs = 0 is unavailable.
#
# Therefore use zero-truncated Poisson.


eggs = 1:9

frequency = c(22,18,18,11,9,6,3,0,1)

n = sum(frequency)

total_eggs = sum(eggs*frequency)

n
total_eggs


# Log likelihood:
#
# l(lambda) =
# total_eggs*log(lambda)
# - n*lambda
# - n*log(1-exp(-lambda))
# + constant

logL = function(lambda)
{
  total_eggs*log(lambda) -
    n*lambda -
    n*log(1-exp(-lambda))
}


# Find MLE

result = optimize(logL,
                  interval=c(0.01,20),
                  maximum=TRUE)

lambda_MLE = result$maximum

lambda_MLE


# Standard error using second derivative

h = 0.0001

second = (logL(lambda_MLE+h) -
          2*logL(lambda_MLE) +
          logL(lambda_MLE-h))/h^2

SE = sqrt(-1/second)

SE


# ============================================================
# QUESTION 7
# ============================================================

# Electron tube lifetimes:
#
# 980, 1020, 995, 1015, 990,
# 1030, 975, 950, 1050, 870
#
# Assuming Exponential(theta):
#
# MLE of theta = mean(X)
#
# SE = theta_hat/sqrt(n)


x = c(980,1020,995,1015,990,
      1030,975,950,1050,870)

n = length(x)

theta_hat = mean(x)

SE = theta_hat/sqrt(n)

theta_hat
SE


# Probability of surviving at least 100 hours:
#
# P(X >= x) = exp(-x/theta)

survival = exp(-100/theta_hat)

survival


# ============================================================
# QUESTION 8
# ============================================================

# 10 lamps
# Test duration = 20 hours
#
# 2 lamps survived 20 hours.
#
# Failure times:
# 9.8, 15.8, 17.2, 11.2,
# 13.8, 18.9, 14.6, 19.6
#
# Assuming Exponential(theta):
#
# Total time on test =
# sum(failure times) +
# number survived*censoring time
#
# MLE:
# theta_hat = total time on test / number of lamps


failure = c(9.8,15.8,17.2,11.2,
            13.8,18.9,14.6,19.6)

n = 10
survived = 2
time = 20

total_time = sum(failure) + survived*time

theta_hat = total_time/n

total_time
theta_hat


# ------------------------------------------------------------
# If all 10 lamps survived 20 hours
# ------------------------------------------------------------

total_time = n*time

theta_hat = total_time/n

theta_hat


# ============================================================
# END
# ============================================================
```
