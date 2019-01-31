
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


