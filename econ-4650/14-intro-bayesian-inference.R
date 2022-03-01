

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                      AN INTRO TO BAYESIAN INFERENCE                         #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files'. Select your desired folder, and click on 'More', then select 
# the option 'Set as Working Directory'.


#==============================================================================#



#== Installing and/or loading required packages:


library(tidyverse)
library(patchwork)
library(ggridges)
library(rjags)
library(ggmcmc)





#==================================================================================================================#



############=== A single-parameter example:




###=== As a first example, let's estimate a Bayesian "toy" model for the proportion of 
###=== heterodox economists across US departments.




### Prior:

# Let us start with a completely uninformative prior.
# The distribution that best captures this is the
# uniform distribution.

# This means that any proportion (p) between 0 and 1 (0 and 100%) 
# is equally likely.


# In pictures...



curve(dunif(x, min = 0, max = 1),
      main = "Prior distribution")    ## does this make sense?



### Likelihood:


# Now, we have to choose how to model this situation. 
# Basically, we have two possible outcomes: 

# 1. The economist being heterodox;
# 2. The economist being neoclassical.

# If we define being heterodox as a "success" (1), and being
# neoclassical as "failure" (0), this setting describes a
# Bernoulli trial. We conduct independent trials (asking
# people about their preferences) and record either
# a success (X=1) or a failure (X=0). We can then
# express this as a Binomial distribution.

# Suppose we interview 50 individuals, obtaining 14
# "Heterodox" answers, and 36 "Neoclassical" answers.



curve(dbinom(x = 14, size = 50, prob = x))     ## does this make sense?



##############


### A useful procedure, before any further posterior estimation, is to
### draw samples from the prior and likelihood and see what they look like:

set.seed(1234)


prior_unif <- runif(n = 1000, min = 0, max = 1)


prior_unif %>% as_tibble() %>% ggplot(aes(prior_unif)) + geom_histogram(color = "white")
prior_unif %>% as_tibble() %>% ggplot(aes(prior_unif)) + geom_density(fill = "lightblue")


## For the likelihood, we first define the proportion space,
## which can be viewed as a "grid":


p_grid <- seq(from = 0, to = 1, length.out = 1000)  ## 1000 possible values of "p" ranging from 0 to 1.


## And we simulate one poll result for each "p" in 'p_grid':


het_poll <- rbinom(n = 1000, size = 50, p = p_grid)


## And we tabulate the results:


het_poll_sim <- tibble(p_grid, het_poll)


het_poll_sim %>% ggplot(aes(x = p_grid, y = het_poll, group = het_poll,
                            fill = het_poll == 14)) + geom_density_ridges()



## In general, the likelihood is the distribution of "p" from which each possible outcome of (i.e., either
## "Heterodox" or "Neoclassical") was generated.


## This highlighted likelihood is the relative compatibility of different possible values of "p", 
## the underlying proportion of heterodox economists, with the observed poll in
## which X = 14  of n = 50 economists.


het_poll_sim %>% filter(het_poll %in% "14") 






### Posterior estimation:


## We have:

# Prior: p ~ Unif[0,1]
# Likelihood: X ~ Binom(50, p)


## In this setting, we could obtain a closed-form solution, but
## let us use JAGS to obtain samples from the posterior:



# We first define the model:


heterodox_model <- "model{
    # Likelihood model for X (the succes variable; in our case, Heterodox.)
    X ~ dbin(p, n)
    
    # Prior model for p
    p ~ dunif(0, 1)
}"                               


# Then we compile the model:


heterodox_jags <- jags.model(textConnection(heterodox_model), 
                             data = list(X = 14, n = 50),
                             inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 123))


# And, finally, we simulate the posterior distribution:


heterodox_sim <- coda.samples(model = heterodox_jags, variable.names = c("p"), n.iter = 10000)


# Let's have a look at the posterior:


heterodox_sim %>% plot(trace = FALSE, main = "Posterior distribution of p")


# And some useful summary statistics:


heterodox_sim %>% summary()     ## Two important summary statistics here: posterior mean and credibility interval.

                                ## The posterior mean (average) proportion of heterodox economists in the US
                                ## is of 28.81%, with a standard deviation of 0.062 percentage points.

                                ## 95% credibility interval for our parameter:
                                ## In light of our priors and
                                ## observed data, there's a 95%
                                ## (posterior) chance that the proportion of heterodox economists is
                                ## between 17.45% & 41.75%.




## To make visualizations better, let us use the "ggmcmc" package, so we can use ggplot2:


post_draws <- heterodox_sim %>% ggs()    ## these are 100,000 plausible values of "p"
                                         ## that could have generated these data.


post_draws %>% ggs_density()

post_draws %>% ggs_traceplot()




#############################################


###=== Trying a different prior:



## Instead of having a completely uninformative prior,
## we can assume an underlying proportion of Heterodox economists
## according to some prior knowledge. What about 40%?

## A well-known distribution for proportions is the Beta distribution.
## It consists of two shape parameters: 'alpha' and 'beta.' If we believe that
## the underlying proprtion of Heterodox economists is around 40%, the first 
## parameter is set as 40, and the second, 60.

## Here is what it looks like:


curve(dbeta(x, shape1 = 40, shape2 = 60), main = "A Beta prior distribution for p")

curve(dbeta(x, shape1 = 1, shape2 = 1), col = "blue", add = TRUE)   ## just the uniform distribution again.


# We first define the model:


heterodox_model2 <- "model{
    # Likelihood model for X
    X ~ dbin(p, n)
    
    # Prior model for p
    p ~ dbeta(a, b)
}"                               


# Then we compile the model:


heterodox_jags2 <- jags.model(textConnection(heterodox_model2), 
                             data = list(a = 40, b = 60, X = 14, n = 50),
                             inits = list(.RNG.name = "base::Wichmann-Hill", .RNG.seed = 123))


# And, finally, we simulate the posterior distribution:


heterodox_sim2 <- coda.samples(model = heterodox_jags2, variable.names = c("p"), n.iter = 10000)


# And we summarize our posterior results:


heterodox_sim2 %>% summary()                ## what now?






#####
post_draws2 <- heterodox_sim2 %>% ggs()


post_draws2 %>% ggs_density()          ## density plot.

post_draws2 %>% ggs_traceplot()        ## trace plot.




## Comparing the two posteriors:


post_unif <- post_draws %>% ggs_density() + labs(title = "Posterior with uniform prior") +
  geom_vline(xintercept = 0.2881017, color = "blue")
post_beta <- post_draws2 %>% ggs_density() + labs(title = "Posterior with beta prior") +
  geom_vline(xintercept = 0.3595568, color = "blue")


post_unif / post_beta     ## so what?

