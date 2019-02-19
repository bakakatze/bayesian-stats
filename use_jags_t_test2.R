
#
#### EDIT THIS PART TO SUIT YOUR ASSUMPTIONS ####

# This part will be sent to JAGS and is run using JAGS language
model = "model
{

  # Prior for mu1
  mu1 ~ dnorm(0, 1/1000^2)

  # Prior for mu2 (where there is 0.5 probability that it will be the same as mu1)
  u ~ dunif(-1,1)

  # length of exponential prior if difference is not zero
  L <- 5
  size_of_difference <- step(u)*(-L*log(1-u))

  # to make difference positive or negative
  C ~ dbin(0.5, 1) # this is a coin flip parameter between 1 or 0
  difference <- (2*C - 1)*size_of_difference

  mu2 <- mu1 + difference

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
variable_names = c('mu1', 'mu2', 'sigma', 'u', 'size_of_difference', 'C', 'difference')

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
