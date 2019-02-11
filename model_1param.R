# Prior widths for each parameter (these help set scale for proposal)
widths = c(1000, 1000)

# Functions for prior, likelihood, and proposal
log_prior = function(params)
{
    logp = 0
    if(params <0 || params>1)
      logp = -Inf
    return(logp)
}

# Might need to load data from a file here
# Log likelihood
log_likelihood = function(params)
{
    logl = 6*log(params) + 4*log(1-params)
    return(logl)
}

# Proposal distribution
proposal = function(params)
{
    # Copy the parameters
    params2 = params

    # Which parameter to change?
    i = sample(1:length(params), 1)

    # Step size - Brendon's favourite magic
    step_size = 1 #widths[i]*10^(1.5 - 3*abs(rt(1, df=2)))

    params2[i] = params2[i] + step_size*rnorm(1)
    return(params2)
}

