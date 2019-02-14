
#### Conjugate Priors for Analytical Approach ####

# Prior -------- Likelihod/Sampling Dist ----- Posterior
# Uniform ------ Binomial -------------------- Beta
# Beta --------- Binomial -------------------- Beta

# Log-Uniform -- Poisson --------------------- Gamma
# Gamma -------- Poisson --------------------- Gamma
# Gamma -------- Exponential ----------------- Gamma

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


#