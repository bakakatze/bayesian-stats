
#### EDIT THIS PART TO SUIT YOUR ASSUMPTIONS ####


# We need to choose a prior for the intercept and the slope
# We can use Bayesian Linear Regresion in JAGS, including making the standard deviation unknown

model = "model
{
  
  # In JAGS, the second object of the dnorm function is 1 over the variance... pretty annoying eh
  # In JAGS, we rarely use uniform distribution because the Gibbs Sampler works more efficient under normal WIDE prior.
  beta0 ~ dnorm(0, 1/1000^2) 
  beta1 ~ dnorm(0, 1/1000^2)
  
  # Log Uniform prior, because the standard deviation needs to be positive
  log_sigma ~ dunif(-10, 10)
  sigma <- exp(log_sigma)

  for(i in 1:N)
  {
    mu[i] <- beta0 + beta1*x[i]
    y[i] ~ dnorm(mu[i], 1/sigma^2)
  }

  # If we have an extra data to predict, then:
  mu_next <- beta0 + beta1*90
  y_next ~ dnorm(mu_next, 1/sigma^2)

}
"

# Variables to monitor
variable_names = c('beta0','beta1','sigma', 'y_next')

# How many burn-in steps?
burn_in = 1e3

# How many proper steps?
steps = 1e4

# Thinning?
thin = 1

# Random number seed
seed = 939

#
#### NO NEED TO EDIT PAST HERE!!! ####
# Just run it all and use the results list.

library('rjags')

# Write model out to file
fileConn=file("model.temp")
writeLines(model, fileConn)
close(fileConn)

if(all(is.na(data)))
{
  m = jags.model(file="model.temp", inits=list(.RNG.seed=seed, .RNG.name="base::Mersenne-Twister"))
} else
{
  m = jags.model(file="model.temp", data=data, inits=list(.RNG.seed=seed, .RNG.name="base::Mersenne-Twister"))
}
update(m, burn_in)
draw = jags.samples(m, steps, thin=thin, variable.names = variable_names)
# Convert to a list
make_list <- function(draw)
{
  results = list()
  for(name in names(draw))
  {
    # Extract "chain 1"
    results[[name]] = as.array(draw[[name]][,,1])
    
    # Transpose 2D arrays
    if(length(dim(results[[name]])) == 2)
      results[[name]] = t(results[[name]])
  }
  return(results)
}
results = make_list(draw)
