
#
#### EDIT THIS PART TO SUIT YOUR ASSUMPTIONS ####

# This part will be sent to JAGS and is run using JAGS language
model = "model
{

  # Timescale: make it so that the alpha level decays as it moves further into the future (negative inverse log)
  log_L ~ dunif(-10,10)
  L <- exp(log_L)
  alpha <- exp(-1/L)

  # Mean level
  mu ~ dnorm(0.8, 1/0.5^2)

  # Standard deviation/uncertatinty
  log_beta ~ dunif(-10,10)
  beta <- exp(log_beta)

  # Time 1 (posterior)
  sigma <- beta/sqrt(1-alpha^2)
  y[1] ~ dnorm(mu, 1/sigma^2)

  ## Likelihood
  for(i in 2:N)
  {
    y[i] ~ dnorm(mu + alpha*(y[i-1] - mu), 1/beta^2)
  }


  ## Prediction
  y_future[1] ~ dnorm(mu + alpha*(y[N] - mu), 1/beta^2)
  
  for(i in 2:365)
  {
    y_future[i] ~ dnorm(mu + alpha*(y_future[i-1] - mu), 1/beta^2)
  }

}
"

# Variables to monitor
variable_names = c('log_L', 'alpha', 'mu', 'beta', 'y_future')

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
