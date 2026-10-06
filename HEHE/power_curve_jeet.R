# ==========================================
# Problem 4: Exponential Minimum Statistic
# ==========================================

# 1. Define Parameters
n <- 10              # Sample size
theta_null <- 100    # H0 mean parameter
alpha <- 0.05        # Level of significance

# Under H0, the rate of the minimum statistic 'u' is n / theta_null
rate_null <- n / theta_null 

# 2. Find the Critical Region (Lower tail because H1: theta = 50 is less than 100)
critical_u <- qexp(alpha, rate = rate_null)
cat("Reject H0 if u <", round(critical_u, 4), "hours\n")

# 3. Create a sequence of possible true theta values
# We range from 20 to 150 to see the curve around 50 and 100
theta_true <- seq(20, 150, by = 1)

# 4. Calculate Power for each true theta
# Power = P(u < critical_u | theta)
power_values <- pexp(critical_u, rate = n / theta_true)

# 5. Plot the Power Curve
plot(theta_true, power_values, type = "l", col = "purple", lwd = 2,
     main = "Problem 4: Power Curve (Minimum of 10 Exponentials)",
     xlab = expression("True Mean Lifespan (" * theta * ")"),
     ylab = "Power (Probability of Rejecting H0)")

# Add reference lines
abline(v = 100, col = "red", lty = 2)     # Null hypothesis
abline(h = 0.05, col = "red", lty = 2)    # Alpha level
abline(v = 50, col = "blue", lty = 2)     # Alternative hypothesis

# ==========================================
# 6. CONCLUSIONS & INTERPRETATIONS
# ==========================================
# Critical Region: 
# We reject the null hypothesis if the first tube fails before ~0.5129 hours.
#
# Power Curve Analysis:
# The curve shows that as the true mean lifespan (theta) decreases, the probability 
# of rejecting H0 increases. At exactly theta = 100, the power is 0.05 (alpha). 
# If the true mean drops to theta = 50 (the specific alternative H1), the power 
# is approximately 0.0975. This is relatively low power, indicating that testing 
# only 10 tubes and making a decision solely based on the *first* failure is not a 
# very sensitive test for distinguishing between a 100-hour and 50-hour mean lifespan.

# ==========================================
# Problem 6: Binomial Count Threshold[cite: 8]
# ==========================================

# 1. Define Parameters
n_assumed <- 10      # Assumed sample size (missing in text)[cite: 8]
critical_y <- 2      # Reject H0 if y > 2[cite: 8]

# 2. Critical Region
# The problem states explicitly: "If observed y is greater than 2, 
# null hypothesis is rejected." Therefore, Critical Region is y = {3, 4, ..., n}[cite: 8].

# 3. Create a sequence of true probabilities 'p'
# p represents the true probability that an observation x > 21949.4
p_true <- seq(0, 1, by = 0.01)

# 4. Calculate Power for each true p
# Power = P(y > 2 | p) = 1 - P(y <= 2 | p)
power_values <- 1 - pbinom(critical_y, size = n_assumed, prob = p_true)

# 5. Plot the Power Curve
plot(p_true, power_values, type = "l", col = "orange", lwd = 2,
     main = paste("Problem 6: Power Curve for y > 2 (Assumed n =", n_assumed, ")"),
     xlab = "True Probability p = P(x > 21949.4)",
     ylab = "Power (Probability of Rejecting H0)")

# ==========================================
# 6. CONCLUSIONS & INTERPRETATIONS
# ==========================================
# Critical Region: 
# The critical region is predefined by the problem as y > 2 (i.e., 3 or more)[cite: 8].
#
# Power Curve Analysis:
# The power curve is an S-shaped curve monotonically increasing from 0 to 1. 
# Because the exact null hypothesis is not provided in the text, we cannot pinpoint 
# the level of significance (alpha). However, the curve shows that as the underlying 
# probability of an observation exceeding 21949.4 increases, the likelihood of 
# counting more than 2 such observations approaches 100%.

# ==========================================
# Problem 7: UMP Test for Normal Mean[cite: 8]
# ==========================================

# 1. Define Parameters
n <- 100             # Sample size[cite: 8]
sigma_sq <- 4        # Population variance[cite: 8]
mu_null <- 0         # H0 mean[cite: 8]
alpha <- 0.01        # Size of the test[cite: 8]
target_power <- 0.9  # Target power[cite: 8]

# Standard error of the sample mean
se <- sqrt(sigma_sq / n)  # sqrt(4/100) = 0.2

# 2. Find the UMP Critical Region (Right-tailed)[cite: 8]
# Find 'c' such that P(X_bar > c | mu = 0) = 0.01
critical_c <- qnorm(1 - alpha, mean = mu_null, sd = se)
cat("UMP Critical Region: Reject H0 if X_bar >", round(critical_c, 4), "\n")

# 3. Find the value of mu at which Power is 0.9[cite: 8]
# We need mu such that P(X_bar > critical_c | mu) = 0.9
# This means critical_c is the 10th percentile (1 - 0.9) of the N(mu, se) distribution.
# Z = (critical_c - mu) / se => qnorm(0.1) = (critical_c - mu) / se
z_power <- qnorm(1 - target_power)
mu_target <- critical_c - (z_power * se)
cat("Value of mu for Power = 0.9 is:", round(mu_target, 4), "\n")

# 4. Create a sequence of true means and calculate power
mu_true <- seq(-0.5, 1.5, by = 0.01)
power_values <- 1 - pnorm(critical_c, mean = mu_true, sd = se)

# 5. Plot the Power Curve
plot(mu_true, power_values, type = "l", col = "darkred", lwd = 2,
     main = "Problem 7: Power Curve for UMP Test (n=100)",
     xlab = expression("True Population Mean (" * mu * ")"),
     ylab = expression("Power (" * pi * ")"))

# Add reference lines
abline(v = 0, col = "black", lty = 2)                     # H0[cite: 8]
abline(h = 0.01, col = "black", lty = 2)                  # Alpha[cite: 8]
abline(v = mu_target, col = "blue", lty = 3, lwd = 2)     # Target mu
abline(h = 0.9, col = "blue", lty = 3, lwd = 2)           # Target power

# ==========================================
# 6. CONCLUSIONS & INTERPRETATIONS
# ==========================================
# UMP Test Procedure: 
# The Uniformly Most Powerful test of size 0.01 is to calculate the sample mean 
# of the 100 observations and reject the null hypothesis if it exceeds ~0.4653.
#
# Finding mu for Power = 0.9:
# To achieve a 90% probability of correctly rejecting the null hypothesis, the 
# true population mean must be exactly 0.7216. 
#
# Power Curve Analysis:
# The power curve rises sharply from 0.01 at mu = 0 to nearly 1.0 as the true mean 
# approaches 1.0, visually confirming the calculated target mu at the 90% power mark.
