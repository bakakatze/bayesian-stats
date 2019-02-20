
require(rms)
require(Hmisc)

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
#### 5. Parameter Estimation: Analytical Methods ####

# Analytical methods only work on some cases.
# If analytical methods do not work, we will use MCMC and JAGS!

### Choice of different priors other than Uniform prior

## 2. Emphasising the extremes:
# p(theta) ~ theta^-0.5 * (1-theta)^-0.5
# or.. inverse binomial


## 3. Well informed prior:
# p(theta) ~ theta^100 * (1-theta)^100
# just put more weight to the prior


## General Form = Beta Distribution ##
# p(x|a, b) ~ x^(a-1) * (1-x)^(b-1)

# when data is very large, the choice of prior does not matter much
# however when data is small, the choice of prior can means a lot <<< this means we need more data

#### 6. Summarising the Posterior Distribution ####

# Summarising posterior distribution depends on the shape

# if it is normal, then just do mean +- SD

# if it has multiple peaks, then display mean, median, mode

## To estimate mean median mode, you use loss functions
# quadratic loss function = mean
# absolute loss function = median
# all-or-nothing loss function = mode

### Computing Point Estimates from a Bayes' Box

# posterior mean is simple to get:
post_mean = sum(theta*post)

# posterior mode:
highest_prob = max(post)
post_mode = theta[post == highest_prob]

# posterior median:
csF = cumsum(post)
dist = abs(csF - 0.5)
post_median = theta[dist == min(dist)]

# Credible Intervals:
# similar to median, just use 0.025 and 0.975

#### 7. Hypothesis Testing and Model Selection ####

# Let's assume the mean Systolic Blood Pressure (SBP) in the population is 120mmHg with SD = 15 mmHg
# We do a trial on 100 sample and get a mean of 115.9 mmHg
# Test the hypothesis that the mean is below 120 mmHg

#### 8. Markov Chain Monte Carlo ####

plot(theta, post, xlab = "theta", ylab = "posterior probability")

# markov chain monte carlo is an algorithm that generates random samples of parameter values
# drawn from the posterior distribution.


#### 9. Just Another Gibbs Sampler (JAGS) ####

# A programme that automates MCMC sampling
# You specify: prior, likelihood, and data

#
#### 10. Posterior Predictive Checks ####

# modify the likelihood:

# it looks like this
model = "model
{
  
  ## Likelihood
  for(i in 1:N)
  {
  mu[i] <- beta0 + beta1*x[i]
  y[i] ~ dnorm(mu[i], 1/sigma^2)

  y_new[i] ~ dnorm(mu[i], 1/sigma^2)
  }
  
}
"
# Then look at the y_new distribution
# there is a formal hypothesis test for this






