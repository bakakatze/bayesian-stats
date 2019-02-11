# Prior widths for each parameter (these help set scale for proposal)
widths = c(1000, 1000)

# Functions for prior, likelihood, and proposal
log_prior = function(params)
{
    # Example: a Normal(0, 1000^2) prior
    # applied independently to two parameters
    logp = sum(dnorm(params, 0, 1000, log=TRUE))
    return(logp)
}

# Might need to load data from a file here
# Log likelihood
log_likelihood = function(params)
{
    # Just a made-up likelihood function
    logl = sum(dnorm(params, log=TRUE))

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
    step_size = widths[i]*10^(1.5 - 3*abs(rt(1, df=2)))

    params2[i] = params2[i] + step_size*rnorm(1)
    return(params2)
}

