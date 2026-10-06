download.file(
  "https://github.com/cococ99136/R/archive/refs/heads/main.zip",
  "R.zip",
  mode = "wb"
)

unzip("R.zip")
###################################
# ============================================================
# PROBABILITY DISTRIBUTIONS - R CODES
# ============================================================


# ============================================================
# 1. BERNOULLI DISTRIBUTION
# ============================================================

p <- 0.6

# PMF: P(X = x)
dbinom(x, size = 1, prob = p)

# CDF: P(X <= x)
pbinom(x, size = 1, prob = p)

# Quantile
qbinom(0.95, size = 1, prob = p)

# Random generation
rbinom(10, size = 1, prob = p)

# Mean
mean <- p

# Variance
variance <- p * (1 - p)


# ============================================================
# 2. BINOMIAL DISTRIBUTION
# ============================================================

n <- 10
p <- 0.4

# PMF
dbinom(x, size = n, prob = p)

# CDF
pbinom(x, size = n, prob = p)

# P(X > x)
1 - pbinom(x, size = n, prob = p)

# P(X >= x)
1 - pbinom(x - 1, size = n, prob = p)

# P(a <= X <= b)
pbinom(b, n, p) - pbinom(a - 1, n, p)

# Quantile
qbinom(0.95, size = n, prob = p)

# Random generation
rbinom(10, size = n, prob = p)

# Mean
mean <- n * p

# Variance
variance <- n * p * (1 - p)


# ============================================================
# 3. POISSON DISTRIBUTION
# ============================================================

lambda <- 4

# PMF
dpois(x, lambda)

# CDF
ppois(x, lambda)

# P(X > x)
1 - ppois(x, lambda)

# P(X >= x)
1 - ppois(x - 1, lambda)

# P(a <= X <= b)
ppois(b, lambda) - ppois(a - 1, lambda)

# Quantile
qpois(0.95, lambda)

# Random generation
rpois(10, lambda)

# Mean
mean <- lambda

# Variance
variance <- lambda


# ============================================================
# 4. NEGATIVE BINOMIAL DISTRIBUTION
# ============================================================

r <- 5
p <- 0.4

# PMF
dnbinom(x, size = r, prob = p)

# CDF
pnbinom(x, size = r, prob = p)

# P(X > x)
1 - pnbinom(x, size = r, prob = p)

# P(X >= x)
1 - pnbinom(x - 1, size = r, prob = p)

# Quantile
qnbinom(0.95, size = r, prob = p)

# Random generation
rnbinom(10, size = r, prob = p)

# Mean
mean <- r * (1 - p) / p

# Variance
variance <- r * (1 - p) / p^2


# ============================================================
# 5. HYPERGEOMETRIC DISTRIBUTION
# ============================================================

N <- 50
K <- 10
n <- 5

# PMF
dhyper(x, m = K, n = N - K, k = n)

# CDF
phyper(x, m = K, n = N - K, k = n)

# P(X > x)
1 - phyper(x, m = K, n = N - K, k = n)

# P(X >= x)
1 - phyper(x - 1, m = K, n = N - K, k = n)

# P(a <= X <= b)
phyper(b, K, N - K, n) -
  phyper(a - 1, K, N - K, n)

# Quantile
qhyper(0.95, m = K, n = N - K, k = n)

# Random generation
rhyper(10, m = K, n = N - K, k = n)

# Mean
mean <- n * K / N

# Variance
variance <- n * (K/N) * (1 - K/N) * ((N-n)/(N-1))


# ============================================================
# 6. GEOMETRIC DISTRIBUTION
# ============================================================

p <- 0.3

# PMF
dgeom(x, prob = p)

# CDF
pgeom(x, prob = p)

# P(X > x)
1 - pgeom(x, prob = p)

# P(X >= x)
1 - pgeom(x - 1, prob = p)

# Quantile
qgeom(0.95, prob = p)

# Random generation
rgeom(10, prob = p)

# Mean
mean <- (1 - p) / p

# Variance
variance <- (1 - p) / p^2


# ============================================================
# 7. DISCRETE UNIFORM DISTRIBUTION
# ============================================================

a <- 1
b <- 10

# PMF
pmf <- function(x) {
  ifelse(x >= a & x <= b, 1/(b-a+1), 0)
}

pmf(5)

# CDF
cdf <- function(x) {
  ifelse(x < a, 0,
         ifelse(x >= b, 1,
                (floor(x)-a+1)/(b-a+1)))
}

cdf(5)

# Mean
mean <- (a + b) / 2

# Variance
variance <- ((b-a+1)^2 - 1) / 12


# ============================================================
# 8. CONTINUOUS UNIFORM DISTRIBUTION
# ============================================================

a <- 2
b <- 10

# PDF
dunif(x, min = a, max = b)

# CDF
punif(x, min = a, max = b)

# P(X > x)
1 - punif(x, min = a, max = b)

# P(X >= x)
1 - punif(x, min = a, max = b)

# P(a1 < X < b1)
punif(b1, a, b) - punif(a1, a, b)

# Quantile
qunif(0.95, min = a, max = b)

# Random generation
runif(10, min = a, max = b)

# Mean
mean <- (a + b) / 2

# Variance
variance <- (b-a)^2 / 12


# ============================================================
# 9. NORMAL DISTRIBUTION
# ============================================================

mu <- 50
sigma <- 10

# PDF
dnorm(x, mean = mu, sd = sigma)

# CDF
pnorm(x, mean = mu, sd = sigma)

# P(X > x)
1 - pnorm(x, mean = mu, sd = sigma)

# P(X >= x)
1 - pnorm(x, mean = mu, sd = sigma)

# P(a < X < b)
pnorm(b, mu, sigma) - pnorm(a, mu, sigma)

# Quantile
qnorm(0.95, mean = mu, sd = sigma)

# Random generation
rnorm(10, mean = mu, sd = sigma)

# Mean
mean <- mu

# Variance
variance <- sigma^2


# ============================================================
# 10. STANDARD NORMAL DISTRIBUTION
# ============================================================

# PDF
dnorm(z)

# CDF
pnorm(z)

# Quantile
qnorm(0.95)

# Random generation
rnorm(10)


# ============================================================
# 11. EXPONENTIAL DISTRIBUTION
# ============================================================

lambda <- 0.5

# PDF
dexp(x, rate = lambda)

# CDF
pexp(x, rate = lambda)

# P(X > x)
1 - pexp(x, rate = lambda)

# P(X >= x)
1 - pexp(x, rate = lambda)

# P(a < X < b)
pexp(b, rate = lambda) -
  pexp(a, rate = lambda)

# Quantile
qexp(0.95, rate = lambda)

# Random generation
rexp(10, rate = lambda)

# Mean
mean <- 1 / lambda

# Variance
variance <- 1 / lambda^2


# ============================================================
# 12. GAMMA DISTRIBUTION
# ============================================================

alpha <- 3
beta <- 2

# PDF
dgamma(x, shape = alpha, rate = beta)

# CDF
pgamma(x, shape = alpha, rate = beta)

# P(X > x)
1 - pgamma(x, shape = alpha, rate = beta)

# P(X >= x)
1 - pgamma(x, shape = alpha, rate = beta)

# P(a < X < b)
pgamma(b, alpha, rate = beta) -
  pgamma(a, alpha, rate = beta)

# Quantile
qgamma(0.95, shape = alpha, rate = beta)

# Random generation
rgamma(10, shape = alpha, rate = beta)

# Mean
mean <- alpha / beta

# Variance
variance <- alpha / beta^2


# ============================================================
# 13. BETA DISTRIBUTION
# ============================================================

alpha <- 2
beta <- 5

# PDF
dbeta(x, shape1 = alpha, shape2 = beta)

# CDF
pbeta(x, shape1 = alpha, shape2 = beta)

# P(X > x)
1 - pbeta(x, shape1 = alpha, shape2 = beta)

# P(a < X < b)
pbeta(b, alpha, beta) -
  pbeta(a, alpha, beta)

# Quantile
qbeta(0.95, shape1 = alpha, shape2 = beta)

# Random generation
rbeta(10, shape1 = alpha, shape2 = beta)

# Mean
mean <- alpha / (alpha + beta)

# Variance
variance <- (alpha * beta) /
  ((alpha + beta)^2 * (alpha + beta + 1))


# ============================================================
# 14. CAUCHY DISTRIBUTION
# ============================================================

location <- 0
scale <- 1

# PDF
dcauchy(x, location = location, scale = scale)

# CDF
pcauchy(x, location = location, scale = scale)

# P(X > x)
1 - pcauchy(x, location = location, scale = scale)

# P(a < X < b)
pcauchy(b, location, scale) -
  pcauchy(a, location, scale)

# Quantile
qcauchy(0.95, location = location, scale = scale)

# Random generation
rcauchy(10, location = location, scale = scale)


# ============================================================
# 15. LOGNORMAL DISTRIBUTION
# ============================================================

mu <- 2
sigma <- 0.5

# PDF
dlnorm(x, meanlog = mu, sdlog = sigma)

# CDF
plnorm(x, meanlog = mu, sdlog = sigma)

# P(X > x)
1 - plnorm(x, meanlog = mu, sdlog = sigma)

# P(a < X < b)
plnorm(b, mu, sigma) -
  plnorm(a, mu, sigma)

# Quantile
qlnorm(0.95, meanlog = mu, sdlog = sigma)

# Random generation
rlnorm(10, meanlog = mu, sdlog = sigma)

# Mean
mean <- exp(mu + sigma^2 / 2)

# Variance
variance <- (exp(sigma^2) - 1) *
  exp(2*mu + sigma^2)

# Median
median <- exp(mu)

# Mode
mode <- exp(mu - sigma^2)


# ============================================================
# 16. CHI-SQUARE DISTRIBUTION
# ============================================================

df <- 5

# PDF
dchisq(x, df = df)

# CDF
pchisq(x, df = df)

# P(X > x)
1 - pchisq(x, df = df)

# P(X >= x)
1 - pchisq(x, df = df)

# P(a < X < b)
pchisq(b, df) - pchisq(a, df)

# Quantile
qchisq(0.95, df = df)

# Random generation
rchisq(10, df = df)

# Mean
mean <- df

# Variance
variance <- 2 * df