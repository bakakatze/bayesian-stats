
require(rms)

#### 1. Frank Harrell's Bayes Analysis in Sequential Testing ####

# Objective:
# Want to detect an efficacy of mu > 0 or futility at mu < 0.05 (assuming minimum clinically significant differencet at 0.05)

#### 1.1 Specification of Prior ====
# Let's decide that the prior is a mixture of two normal distribution with mean 0.
# SD1 is chosen that P(mu > 1) = 0.1
# SD2 is chosen that P(mu > 0.25) = 0.05
sd1 = 1/qnorm(1 - 0.1)
sd2 = 0.25/qnorm(1 - 0.05)
wt = 0.5  # 1:1 mixture

pdensity = function(x) wt * dnorm(x, 0, sd1) + (1-wt) * dnorm(x, 0, sd2)

x = seq(-3, 3, length = 200)
plot(x, pdensity(x), type = 'l', xlab = 'Efficacy', ylab = 'prior degree of belief')

#
#### 1.2 Sequential Testing Simulation ====




#
#### 4.2 Prediction in the Bus Problem [Bayes' Box] ####
# predicting a good bus given the data that 2 out of 5 bus are good:

# a vector of probabilities
theta = seq(0, 1, by = 0.1)

# prior distribution
prior = rep(1/11, 11) # uniform distribution

# likelihood function
lik = dbinom(2, 5, theta)

# posterior distribution = prior * likelihood
h = prior*lik
post = h/sum(h) # normalising constant


## probability of good bus tomorrow:
# This happens to be the same as the posterior expectation of theta
prob_tomorrow = sum(theta*post) # 42.87%


#
#### 5 Parameter Estimation: Analytical Methods ####

# Analytical methods only work on some cases.
# If analytical methods do not work, we will use MCMC and JAGS!







