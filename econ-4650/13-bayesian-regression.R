

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                   AN INTRO TO BAYESIAN LINEAR REGRESSION                    #
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
library(rjags)
library(ggmcmc)





#==================================================================================================================#



############=== A simple regression example:



## We can assume that weights (Y) are normally distributed around some mean "m," with a standard deviation "s."

## Thus, Y ~ Normal(m, s).



## Now, let us incorporate a potential predictor of weight: an individual's height (X).
## Then, we can state that the mean of individuals' weights depends on their respective heights. Now, we may write:


## Y ~ Normal(m[i], s), where m[i] denotes the dependence of the variable's mean on the predictor.



## Our regression model takes off from the above statement. Thus, m[i] = beta0 + beta1 * X[i].
## Lastly, we incorporate the potential deviations from the regression line between weight and height (s).



##== Priors:


curve(dnorm(x, mean = 0, sd = 100), xlim = c(-300, 300))   # for beta0.

curve(dnorm(x, mean = 1, sd = 0.5), xlim = c(-3.5, 3.5))   # for beta1.

curve(dunif(x, min = 0, max = 20), xlim = c(-2, 22))       # for s.




##== Data:



reg_data <- read_csv("reg_data.csv")                                  # the data come from McElreath (2020).


reg_data %>% ggplot(aes(weight)) + geom_histogram(color = "white")    # a histogram.


reg_data %>% ggplot(aes(x = height, y = weight)) + geom_point()       # a scatter plot.




##== Posterior estimation using RJAGS:



## First, we define the model:  


weight_model <- "model{
  # Likelihood model for Y[i]
  for(i in 1: length(Y)) {
      Y[i] ~ dnorm(m[i], s^(-2)) ## in RJAGS, instead of SD, we use a precision measure, SD^(-2).
      m[i] <- beta0 + beta1 * X[i]
  }  

  # Define priors:

  beta0 ~ dnorm(0, 100^(-2))
  beta1 ~ dnorm(1, 0.5^(-2))
  s ~ dunif(0, 20)  
    
}"




## Second, we compile the model:


weight_jags <- jags.model(
  textConnection(weight_model),
  data = list(Y = reg_data$weight, X = reg_data$height),
  n.chains = 4)                                               ## here, we are using 4 Markov chains instead of 1.



## Lastly, we simulate the posterior (with 10,000 iterations):

    
weight_sim <- coda.samples(model = weight_jags, 
                           variable.names = c("beta0", "beta1", "s"), 
                           n.iter = 10000)



## And we look at the summary:


weight_sim %>% summary()


## And compare to the usual "lm" output (frequentist regression):


weight_reg <- lm(weight ~ height, data = reg_data)

weight_reg %>% summary()      ## so what?



## Some diagnostic plots (density and trace):


weight_chains <- weight_sim %>% ggs()




weight_chains %>% ggs_density()     ## density plot.

weight_chains %>% ggs_traceplot()   ## trace plot.




## Some other plots:




chains <- weight_sim[[1]] %>% as_tibble()      ## transforming the chains into a tibble.




reg_data %>% ggplot(aes(y = weight, x = height)) + geom_point() +
  geom_abline(intercept = mean(chains$beta0), 
              slope = mean(chains$beta1), color = "blue")   ## the mean posterior regression line.



reg_data %>% ggplot(aes(y = weight, x = height)) + geom_point() +
  geom_abline(intercept = head(chains$beta0, 1000), 
              slope = head(chains$beta1, 1000), color = "blue", alpha = 0.1)  ## 1,000 posterior regression lines.


reg_data %>% ggplot(aes(y = weight, x = height)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)                            ## and the usual "lm" fit.





##================================================================================================================##


###=== Adding a binary predictor:


reg_data <- reg_data %>% mutate(male = as_factor(male))    ## making sure that the binary variable is a "factor."



## First, we define the model: 


weight_dummy_model <- "model{
  # Likelihood model for Y[i]
  for(i in 1: length(Y)) {
      Y[i] ~ dnorm(m[i], s^(-2))
      m[i] <- beta0 + d[X[i]]
  }  

  # Define priors:

  beta0 ~ dnorm(77, 25^(-2))
  
  # Notice that d has two levels:
  
  d[1] <- 0
  d[2] ~ dnorm(0, 2^(-2))
  s ~ dunif(0, 20)  
    
}"




## Second, we compile the model: 


weight_dummy_jags <- jags.model(textConnection(weight_dummy_model),
  data = list(Y = reg_data$weight, X = reg_data$male),
  n.chains = 4)




## Finally, we simulate the posterior: 


weight_dummy_sim <- coda.samples(model = weight_dummy_jags, 
                                 variable.names = c("beta0", "d", "s"), 
                                 n.iter = 10000)



## And we look at the summary:


weight_dummy_sim %>% summary()


## And compare to the usual "lm" output (frequentist regression):


weight_dummy_reg <- lm(weight ~ male, data = reg_data)

weight_dummy_reg %>% summary()



## Some diagnostic plots (density and trace):



weight_dummy_chains <- weight_dummy_sim %>% ggs()


weight_dummy_chains %>% ggs_traceplot()

weight_dummy_chains %>% ggs_density()



##================================================================================================================##



###=== Multiple regression:



## First, we define the model: 


weight_multi_model <- "model{
  # Likelihood model for Y[i]
  for(i in 1: length(Y)) {
      Y[i] ~ dnorm(m[i], s^(-2))
      m[i] <- beta0 + d[X[i]] + beta1 * X2[i]
  }  

  # Define priors:

  beta0 ~ dnorm(0, 100^(-2))
  
  # Notice that d has two levels:
  
  d[1] <- 0
  d[2] ~ dnorm(0, 2^(-2))
  
  beta1 ~ dnorm(1, 0.5^(-2))
  s ~ dunif(0, 100)  
    
}"




## Second, we compile the model: 


weight_multi_jags <- jags.model(textConnection(weight_multi_model),
                                data = list(Y = reg_data$weight, X = reg_data$male, X2 = reg_data$height),
                                n.chains = 4)



## Lastly, we simulate the posterior: 


weight_multi_sim <- coda.samples(model = weight_multi_jags, 
                                 variable.names = c("beta0", "d", "beta1" , "s"), 
                                 n.iter = 10000)


## And we look at the summary:


weight_multi_sim %>% summary()



## And compare to the usual "lm" output (frequentist regression):


weight_multi_reg <- lm(weight ~ male + height, data = reg_data)

weight_multi_reg %>% summary()



## Some diagnostic plots (density and trace):


weight_multi_chains <- weight_multi_sim %>% ggs()


weight_multi_chains %>% ggs_traceplot()

weight_multi_chains %>% ggs_density()




## Another plot:


chains_multi <- weight_multi_sim[[1]] %>% as_tibble() %>% rename(d1 = `d[1]`, d2 = `d[2]`)


## The following plot simply describes in a picture what was described on slide 14.

reg_data %>% ggplot(aes(y = weight, x = height, color = male)) + geom_point() +
  geom_abline(intercept = mean(chains_multi$beta0), slope = mean(chains_multi$beta1), 
              color = "blue", alpha = 0.4) + 
  
  geom_abline(intercept = mean(chains_multi$beta0) + mean(chains_multi$d2),
              slope = mean(chains_multi$beta1), color = "red") +
  ylim(c(-15, 70))
