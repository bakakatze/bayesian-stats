
#
#### EDIT THIS PART TO SUIT YOUR ASSUMPTIONS ####

# This part will be sent to JAGS and is run using JAGS language
model = "model
{

  ## Let's make alpha pretty high
  # The negative 1 is pretty liberal because we are quite sure that one position cannot have too much advantage over the other.
  # the exp(5) will draw the probability to the center of the triangle of the dirichlet distribution.
  log_a ~ dunif(-1,5)
  a <- exp(log_a)

  ## Dirichlet Prior (conjugate prior for multinomial)
  for(i in 1:3)
  {
    alpha[i] <- a
  }
  
  theta ~ ddirch(alpha)


  ## Likelihood
  x ~ dmulti(theta, N)


}
"

# Variables to monitor
variable_names = c('theta', 'a')

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
