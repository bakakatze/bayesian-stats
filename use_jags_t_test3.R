
#
#### EDIT THIS PART TO SUIT YOUR ASSUMPTIONS ####

# This part will be sent to JAGS and is run using JAGS language
model = "model
{

  # Prior for hyperparameters
  grand_mean ~ dnorm(0, 1/1000^2)
  log_diversity ~ dunif(-10,10)
  diversity <- exp(log_diversity)
  

  # Prior: normal distribution but we don't know what kind of normal
  mu1 ~ dnorm(grand_mean, 1/diversity^2)
  mu2 ~ dnorm(grand_mean, 1/diversity^2)

  # Prior for sigma
  log_sigma ~ dunif(-10,10)
  sigma <- exp(log_sigma)


  ## Likelihood
  for(i in 1:N1)
  {
    y1[i] ~ dnorm(mu1, 1/sigma^2)
  }

  for(i in 1:N2)
  {
    y2[i] ~ dnorm(mu2, 1/sigma^2)
  }

}
"

# Variables to monitor
variable_names = c('mu1', 'mu2', 'sigma', 'diversity', 'grand_mean')

# How many burn-in steps?
burn_in = 1e3

# How many proper steps?
steps = 1e5

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
