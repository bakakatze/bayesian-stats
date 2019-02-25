#### Data: ANDROMEDA-SHOCK TRIAL ####
#
# Comparing the use of Capillary Refill Time vs. Lactate Level as a guiding measures to treat hypovelemic shock

# Credit: DanLane911
# Calculating MCID

#Here I am using the estimated reductions from the power calculation to get an OR for the MCID (may need to be converted to RR instead of OR)#

a <- 0.3 * 420 #Intervention and Outcome
b <- 0.45 * 420 #Control and Outcome
c <- 420 - a #Intervention No Outcome
d <- 420 - b #Control No Outcome

MCID <- ((a+0.5) * (d+0.5))/((b+0.5) * (c+0.5))

#Hazard Ratio

HR <- 0.75
UC <- 1.02

#Calculate Prior
#Skeptical prior mean estimate
prior.theta <- log(1)
prior.sd <- (log(1.0)-log(MCID-0.05))/1.96 #Calculating skeptical prior SD estimate for 5% probability of exceeding projected estimate
#Enthusiastic Prior
prior.theta <- log(MCID)
prior.sd <- (log(1.05)-log(MCID))/1.96

#Calculate Likelihood
L.theta <- log(HR)
L.sd <- (log(UCI)-log(HR))/1.96

#Calculate Posterior
post.theta <- ((prior.theta/prior.sd^2)+(L.theta/L.sd^2))/((1/prior.sd^2)+(1/L.sd^2))
post.sd <- sqrt(1/((1/prior.sd^2)+(1/L.sd^2)))

#Calculate posterior median effect and 95% certainty interval
cbind(exp(qnorm(0.025, post.theta,post.sd, lower.tail = T)), exp(qnorm(0.5, post.theta,post.sd)), exp(qnorm(0.975, post.theta,post.sd)))

#Calculate probability benefit (HR < 1.0)
pnorm(log(1), post.theta,post.sd, lower.tail=T)

# Plot the prior density
mu.plot <- seq(-2,2,by=0.025)
prior <- dnorm(mu.plot, prior.theta, prior.sd)
likelihood <- dnorm(mu.plot, L.theta, L.sd)
posterior <- dnorm(mu.plot, post.theta, post.sd)

plot(exp(mu.plot), exp(prior),
     type=“l”, col=“black”, lty=3,
     xlim=c(0,1.5),
     ylim=c(1,15),
     lwd=2,
     xlab=“Hazard Ratio”,
     ylab=“Probability Density”)
#likelihood
lines(exp(mu.plot), exp(likelihood), col=“green”,lwd=2,lty = 2)
#posterior
lines(exp(mu.plot), exp(posterior), col=“red”,lwd=2)
abline(v=1, col = “gray”)

legend(“topleft”,
        col=c(“black”,“green”,“red”),
        lty=c(3,2,1),
        lwd=2, #line width = 2
        legend=c(“Prior”, “Likelihood”, “Posterior”))

#### Conjugate Priors for Analytical Approach ####

# Prior -------- Likelihod/Sampling Dist ----- Posterior

# Uniform ------ Binomial -------------------- Beta
# Beta --------- Binomial -------------------- Beta

# Log-Uniform -- Poisson --------------------- Gamma
# Gamma -------- Poisson --------------------- Gamma
# Gamma -------- Exponential ----------------- Gamma

# Dirichlet ---- Multinomial ----------------- Multinomial

# Normal ------- Normal ---------------------- Normal


#### Data: 20 volcanic eruptions in 10.000 years ####

# What is the probability that there is at least one eruption in 50 years?

# set up the prior distribution and the probability table
lambda = seq(1, 100, by = 0.1)
prior = 1/lambda # log uniform prior
prior = prior/sum(prior)

# set up the likelihood
lik = dpois(20, lambda)

# set up the posterior distribution
h = prior * lik
Z = sum(h) # normalising constant

post = h/Z


# Classical Approach
1 - dpois(0, lambda = 0.0025*20)


## Bayesian Approach

# Conditional probability
prob = 1 - dpois(0, 0.0025*lambda)
plot(lambda, prob)

# Marginal probability
sum(post*prob)


## They are both very similar!


#### Data: Hairs in burgers ####

# Sample 3 burgers: found 5, 6, 4 hairs respectively
# Following poisson distribution: lambda^x * e^-lambda / x!

# we got the following sampling distribution/likelihood function:
# p(x1, x2, x3 | lambda) = lambda^(5+6+4) * e^(-3*lambda) / 5!+6!+4!

lambda = seq(0, 20, by = 0.001)

# prior will be 1/lambda (variance of poisson distribution) << forget that this is an improper prior, we can play with the parameters later
# this will knock off the lambda^15 to lambda^14
# So, the posterior distribution
plot(lambda, lambda^14 * exp(-3*lambda), type = "l") # this follows a gamma distribution

# Gamma(15, 1/3)


#### Data: 250 coin flips (140 heads, 110 tails) ####

## Q: Is the coin biased? How biased it is?

# set the possible parameters
theta = seq(0, 1, by = 0.01)

# set prior probs (Spike and Slab prior)
prior = rep(0.5/100, 101)

# where theta is very close to 0.5, set the probability to 0.5
prior[abs(theta - 0.5) < 1e-6] = 0.5

# plot the prior
plot(theta, prior, type = 'h')

# set the likelihood
lik = dbinom(140, 250, theta)

# Compute the posterior probs
h = prior * lik
Z = sum(h)
post = h/Z

plot(theta, post, type = "h") # the posterior probs also got an appearance of Spike and Slab

# the probability that the coin is fair (i.e. probs of head = 0.5)
prob = post[abs(theta - 0.5) < 1e-6] 
prob

# it's 0.68
# You'll get different result using frequentist approach. BUT... the choice of prior is weird
prop.test(140,250,0.5)


## Now, if we want to get the odds ratio of the Null/Alt
post_odds = prob/(1-prob)
post_odds

# post_odds = prior_odds * bayes_factor
bayes_factor = post_odds/1 # cause the prior is 1


## I want to try a different prior
prior2 = dbinom(1000, 2000, theta)
plot(theta, prior2, type = "h")

h2 = prior2*lik
z2 = sum(h2)
post2 = h2/z2

plot(theta, post2, type = 'h')

# what is the probability that the average expected head count of that coin flip is between 0.47 - 0.53
sum(post2[theta >= 0.47 & theta <= 0.53]) # still very likely




#### Example: Election Poll with Metropolis algorithm ####
# Data: 6 Yeses and 4 Nays

# Create a posterior distribution for this, assuming that the prior 

# Steps to take
steps = 10e3

# Thinning
thin = 1 # take every N step, 1 = every step, 10 = every 10 steps

# Load functions that are model-specific
source("model_1param.R")

# Starting position in parameter space (0.5 = equal probability of yes/no in a poll)
params = 0.5 # c(0,0) # for multi-parameters

# Measure how good it is
logh = log_prior(params) + log_likelihood(params)

# set up 2D array for storage of results
keep = array(NA, dim = c(steps/thin, length(params)))

# Set up 1D array
logl_keep = array(NA, dim = steps/thin)

# Count number of accepted proposals
accepted = 0

# Do Metropolis
for(i in 1:steps)
{
  # Propose to move somewhere else
  params2 = proposal(params)
  
  # Measure how good it is
  logh2 = log_prior(params2) + log_likelihood(params2)
  
  # Acceptance probability
  log_alpha = logh2 - logh
  if(log_alpha > 0)
  {
    log_alpha = 0
  }
  
  # Accept the proposal with probability alpha
  if(runif(1) < exp(log_alpha))
  {
    params = params2
    logh = logh2
    accepted = accepted+1
  }
  
  # Store results
  if(i %% thin == 0)
  {
    keep[i/thin, ] = params
    logl_keep[i/thin] = log_likelihood(params)
    cat("Done", i, "iterations.\n")
  }
  
}

# plot them
plot(keep, type = "l", main = "Trace Plot")

# histogram
hist(keep)


## Problems with Metropolis algorithm:
# If your step size is too small.. it won't converge, it jumps around slowly and will never create a reasonable posterior distribution
# If your step size is too large.. it won't move anywhere because any new proposal is just too implausible

#### Example (JAGS): Run a code 100x, 90 successes ####

## What was the success probability of theta?

# This example will run use_jags.R
source("use_jags.R")

# the results will have N parameters, depens on how many you put in
names(results)

# trace plot
plot(results$theta, type = "l", main = "Trace Plot")

# posterior distribution
hist(results$theta, breaks = 100)


mean(results$theta)
#
#### Data: British Coal Mining Accidents ####

# Load the data:

data = list(t = as.integer(c(1851, 1852, 1853, 1854, 1855, 
                                     1856, 1857, 1858, 1859, 1860, 1861, 1862, 1863, 1864, 1865, 1866, 
                                     1867, 1868, 1869, 1870, 1871, 1872, 1873, 1874, 1875, 1876, 1877, 
                                     1878, 1879, 1880, 1881, 1882, 1883, 1884, 1885, 1886, 1887, 1888, 
                                     1889, 1890, 1891, 1892, 1893, 1894, 1895, 1896, 1897, 1898, 1899, 
                                     1900, 1901, 1902, 1903, 1904, 1905, 1906, 1907, 1908, 1909, 1910, 
                                     1911, 1912, 1913, 1914, 1915, 1916, 1917, 1918, 1919, 1920, 1921, 
                                     1922, 1923, 1924, 1925, 1926, 1927, 1928, 1929, 1930, 1931, 1932, 
                                     1933, 1934, 1935, 1936, 1937, 1938, 1939, 1940, 1941, 1942, 1943, 
                                     1944, 1945, 1946, 1947, 1948, 1949, 1950, 1951, 1952, 1953, 1954, 
                                     1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962)), 
                 y = c(4, 5, 4, 1, 0, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5, 4, 5, 3, 
                           1, 4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1, 
                           1, 1, 3, 0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 
                           0, 1, 0, 1, 0, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 
                           1, 1, 1, 1, 2, 4, 2, 0, 0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 
                           1, 0, 0, 1, 0, 1),
            N = 112)


# Run the source
source("use_jags.R")


# results
plot(results$lambda, type = "l")

# histogram
hist(results$lambda, breaks = 100)

#
## Try different model, incorporating change point model

source("use_jags2.R")

# join distribution
plot(results$lambda1, results$lambda2, cex = 0.5)

# cause year is in integer
hist(results$change_year, breaks = 500)

# Posterior distribution
hist(results$lambda, breaks = 500)

# Plot the model through the data
plot(data$t, data$y)
lines(data$t, results$lambda[1, ])
lines(data$t, results$lambda[50, ])



#

#### Data: Road Data - Linear Regression ####

# Y = distance in meter that person can see a road sign
# X = age, N = number of data points
data = structure(list(N = 30, 
            x = c(18, 20, 22, 23, 23, 25, 27, 
                                     28, 29, 32, 37, 41, 46, 90, 53, 55, 63, 65, 66, 67,
                                     68, 70, 71, 72, 73, 74, 75, 77, 79, 82),
            y = c(510, 590, 560, 510, 460, 490, 560, 510, 460, 410, 420, 460, 
                            450, 380, 460, 420, 350, 420, 300, 410, 300, 390, 320, 
                            370, 280, 420, 460, 360, 310, 360)),
            .Names = c("N", "x", "y"))


## We need to choose prior for intercept and the slope and the error

source("use_jags_lin_reg.R")

# Trace plot
names(results)

plot(results$beta0, type = "l")

# join posterior distribution
plot(results$beta0, results$beta1, cex = 0.1)

# prediction for the 90 years old
hist(results$y_next, breaks = 100)

##
# IT is much better to use t-distribution and let the parameter nu varies
# mu approaches infinity will be equal to normal distribution
# JAGS code:
#
# log_nu ~ dunif(0, log(100))
# nu <- exp(log_nu)
#
# y[i] ~ dt(mu[i], 1/sigma^2, nu)
#



#### Data: Widget Spinners (fake data) - T-Test ####

# Manufacturer A: mean lifetime = 42 hours, SD = 7.48 hours, N = 9
# Manufacturer B: mean lifetime = 50 hours, SD = 6.48 hours, N = 4
# Is one manufacturer better than the other, in a way we can expect to generalise?


data = structure(list(y1 = c(41.26297, 35.80629, 36.00830, 43.58714, 37.49637, 52.6908, 42.42694, 32.52, 56.19513),
                      y2 = c(54.96650, 47.07097, 57.12499, 40.83754),
                      N1 = 9,
                      N2 = 4),
                 .Names = c("y1", "y2", "N1", "N2"))

## MODEL 1 (mu1 and mu2 could be anything and independent to each other)
source("use_jags_t_test.R")

# Is mu2 better?
mean(results$mu2 > results$mu1)


# plot two histogram (mu1 could be anything, mu2 might be similar to mu1)
hist(results$mu1, col = "blue", breaks= 100)
hist(results$mu2, col = rgb(1,0,0,0.5), breaks = 100, add=T)
box()

# is mu1 = mu2?
mean(results$mu2 == results$mu1) # this is useless, this model cannot really answer this hypothesis


## MODEL 2
source("use_jags_t_test2.R")

hist(results$difference, breaks = 100)
hist(results$size_of_difference, breaks = 100)

# plot two histogram
hist(results$mu1, col = "red", breaks= 100)
hist(results$mu2, col = "blue", breaks = 100, add=T)

# plot dots
plot(results$mu1, results$mu2, cex = .1)

# test the null hypothesis
mean(results$mu2 == results$mu1)


#
## MODEL 3 (mu1 and mu2 might be close together, or they may be far apart)

source("use_jags_t_test3.R")

# dot plot
plot(results$mu2, results$mu2, cex = 0.1)

# plot two histogram
hist(results$mu1, col = "red", breaks= 100)
hist(results$mu2, col = "blue", breaks = 100, add=T)

# is mu2 > mu1?
mean(results$mu2 > results$mu1)
# much less than model 1, depends on the prior AND the data
# prior won't matter much (usually) if you have good data




#### Data: Starling weights - 4 groups - ANOVA ####

data = list(x = c(78,88,87,88,83,82,81,80,80,89,78,78,83,81,78,81,81,82,76,76,79,73,79,75,77,78,80,78,83,84,77,69,75,70,74,83,80,75,76,75),
            group = c(rep(1,10), rep(2,10), rep(3,10), rep(4,10)),
            N = 40)

# run the codes
source("use_jags_anova.R")

# plot each one
plot(results$mu[,1], type = "l")
plot(results$mu[,2], type = "l")
plot(results$mu[,3], type = "l")
plot(results$mu[,4], type = "l")
# there are some holes in the trace plots... JAGS algorithm is less efficient if the priors are correlated
# May need pre-whitening (to make the priors independent)


# mu2 > mu3
mean(results$mu[,2] > results$mu[,3])



#### Data: Auto-Regressive (1) time series (fake data) ####

# length of simulation
N = 1000

# storage
y = rep(1, N)

# mean value
mu = 0.85

# how much of old value to remember
alpha = 0.99

# how much to 'kick' each time (uncertainty)
beta = 0.1

# simulate the AR(1) model
for(i in 2:N)
{
  y[i] = mu + alpha*(y[i-1] - mu) + beta*rnorm(1) - (i * 1e-5)
}

# plot it
plot(y, type = "l")

# shape the data
data = list(y = y, N = 1000)


## Run the jags code
source("use_jags_AR1.R")


hist(results$beta, breaks = 100)
hist(results$log_L, breaks = 100)
hist(results$mu, breaks = 100)

plot(results$log_L, results$mu, cex = .1)

# So the posterior distribution for future n=1 and n=300 days
# The standard deviation is much greater at day 300
hist(results$y_future[,1], breaks = 100)
hist(results$y_future[,300], breaks = 100)

#
#### Data: Wheel of Fortune (Classic: Chi Squared Test) ####

# Wheel of Fortune: 3 players in 3 diff position
# N = 30, pos 1 wins 8x, pos 2 wins 9x, pos 3 wins 13x

# This is (the likelihood) basically a multinomial distribution

# The prior will be a Dirichlet distribution
# High alpha value means that there is no favor on one theta over the other. This will means a uniform density for multinomial.
# Low alpha value means that high probability density in the corner. Favor one theta over the other.

data = list(x = c(8, 9, 13), N = 30)

source("use_jags_multinom.R")

# We have 3 thetas
plot(results$theta[,1], type = "l")


hist(results$theta[,1], breaks = 100)
hist(results$theta[,2], breaks = 100)
hist(results$theta[,3], breaks = 100)

# the mean probability
mean(results$theta[,1])
mean(results$theta[,2])
mean(results$theta[,3])

# is position 3 > position 1?
mean(results$theta[,3] > results$theta[,1])

## BUT, this is under the assumption of uniform prior which does not really describe reality.
# cause we know that the probability cannot favor one position than the other by much.

# Let's make a hierarchical model
# favoring high value of alpha (which means the probability will be pulled towards the center, favoring equal likelihood of the 3 poisitons)
source("use_jags_multinom_centered_prior.R")


# the mean probability
mean(results$theta[,1])
mean(results$theta[,2])
mean(results$theta[,3])

# is position 3 > position 1?
mean(results$theta[,3] > results$theta[,1])

# correlated posterior/prior
plot(results$theta[,1], results$theta[,3], cex = 0.1)

# what is the likely alpha value
hist(results$a, breaks = 100)








