
#
#### EDIT THIS PART TO SUIT YOUR ASSUMPTIONS ####

# This part will be sent to JAGS and is run using JAGS language
model = "model
{

  ## Dirichlet Prior (conjugate prior for multinomial)
  for(i in 1:3)
  {
    alpha[i] <- 1 # this is basically uniform prior in multinomial sense
  }
  
  theta ~ ddirch(alpha)


  ## Likelihood
  x ~ dmulti(theta, N)


}
"

# Variables to monitor
variable_names = c('theta')

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
