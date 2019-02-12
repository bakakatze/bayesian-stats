#### EDIT THIS PART TO SUIT YOUR ASSUMPTIONS ####

# This part will be sent to JAGS and is run using JAGS language
model = "model
{
  
  # Uniform Prior (use 2 parameters)
  lambda1 ~ dunif(0, 100)
  lambda2 ~ dunif(0, 100)
  change_year ~ dunif(1851, 1962) # this is the year info from the data
  
  # Likelihood + change point model
  for(i in 1:N)
  {
    lambda[i] <- lambda1 + step(t[i] - change_year) * (lambda2 - lambda1)
    y[i] ~ dpois(lambda[i])
  }
  
}
"


# Variables to monitor
variable_names = c('lambda1', 'lambda2', 'change_year', 'lambda')

# How many burn-in steps?
burn_in = 1e3

# How many proper steps?
steps = 1e5

# Thinning?
thin = 10

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
