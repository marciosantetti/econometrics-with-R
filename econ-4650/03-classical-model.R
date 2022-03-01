

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                THE CLASSICAL LINEAR REGRESSION MODEL (CLRM)                 #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files'. Select your desired folder, and click on 'More', then select 
# the option 'Set as Working Directory'.




#==============================================================================#

# Installing and/or loading required packages:

library(tidyverse)
library(broom)
library(rstatix)
library(wooldridge)



#==============================================================================#


#========= Assumption 1: the model is linear in parameters, well-specified, 
#========= and contains an additive error term.



set.seed(1234)


x1_true <- 30 + 2*runif(n=100, min=20, max=60)          ## the "true" process defining x1.


x2_true <-  10/x1_true + x1_true*rnorm(n=100)           ## the "true" process defining x2.


y_true <- 45 + 2*x1_true + 4*x2_true + rnorm(n=100)     ## the "true" process defining y.



true_data <- as_tibble(cbind(y_true, x1_true, x2_true))   ## joining the 3 above variables in a single data set.




true_data %>% ggplot(mapping = aes(x=x1_true, y=x2_true)) + geom_point()





model_omit <- lm(y_true ~ x2_true, data = true_data)

model_omit %>% tidy()



#== Estimating the model with all the relevant variables:


model_true <- lm(y_true ~ x1_true + x2_true, data = true_data)

model_true %>% tidy()


#==============================================================================#



#========= Assumption 2: the error term has a zero population mean.



set.seed(200)                 ## setting a seed number, so that our results match. Before creating
                              ## any new random data, make sure to set the seed number.


x <- 50 - 100*runif(n = 50, min = -50, max = 50)      ## creating an 'x' vector, whose function has an 
                                                      ## intercept of 50, and its slope multiplies 
                                                      ## a random variable uniformly
                                                      ## distributed, with 50 observations, ranging from
                                                      ## -50 and +50.


y <- 100 + 2*x + rnorm(50)             ## 'y' is a function of x, and also of a normally 
                                       ## distributed component, with 50 observations. If we
                                       ## do not specify a mean and standard deviation to the
                                       ## 'rnorm()' command, it assumes a standard normal 
                                       ## distribution, that is, a mean of 0 and SD of 1.


data_clrm2 <- as_tibble(cbind(y, x))



data_clrm2 %>% ggplot(mapping = aes(x=x, y=y)) + geom_point()



model_clrm2 <- lm(y ~ x, data = data_clrm2)



data_clrm2 %>% ggplot(mapping = aes(x=x, y=y)) + geom_point() + geom_smooth(method = "lm", se = FALSE)


u_term <- model_clrm2 %>% resid()



u_term %>% plot()


u_term %>% mean()


#=========================================================================================#


#========= Assumption 3: All explanatory variables are uncorrelated with the error term.


## To check this assumption, let us simply calculate correlation coefficients from
## our previous model:


data_clrm2 %>% summarize(cor_x_u = cor(x, u_term))    ## Is there a correlation?




#=========================================================================================#



#========= Assumption 4: Observations of the error term are uncorrelated with each other.


##=== For this example, let us use some real-world data from the 'wooldridge package:

data("fertil3")

fertil3 <- as_tibble(fertil3)


model_clrm4 <- lm(gfr ~ pe + ww2 + pill, data = fertil3)



resid_auto <- model_clrm4 %>% resid()                 ## extracting the residual term.


fertil3 <- fertil3 %>% cbind(resid_auto)              ## adding the residuals series to the original data set.



fertil3 %>% ggplot(mapping = aes(x=year, y=resid_auto)) +      ## now, a plot. What does it look like?
  geom_point() + geom_line()


## Now, compare to a non-autocorrelated residual term:


u_term %>% plot()                                              ## from our CLRM Assumption 2 section.


#=========================================================================================#




#========== Assumption 5: the error term has a constant variance (Homoskedasticity assumption).



u_term %>% var()


u_term %>% plot() %>% abline(h=0, col='red')



z <- seq(from=1, to=50, by=1)                        ## creating a sequence from 1 to 50, 1 by 1.

set.seed(200)

w <- 50 + 0*z + rnorm(n=50, mean=0, sd=0.1*z)          ## 'w' is a vector which is not 
                                                     ## linearly related to 'z', but has a
                                                     ## random component whose standard 
                                                     ## variation is a linear 
                                                     ## function of 'z'.


data_het <- as_tibble(cbind(w, z))

data_het %>% ggplot(mapping = aes(x=z, y=w)) + geom_point()



model_het <- lm(w ~ z)


resid_het <- model_het %>% resid()


resid_het %>% plot() %>% abline(h=mean(resid_het), col='red')



#== Question: what is the main difference between the model run in Assumption 2
#== and the one for Assumption 5?




#======================================================================================#


#========== Assumption 6: No explanatory variable is a perfect linear function
#========== of any other explanatory variable (no multicollinearity assumption).



set.seed(123)

x1 <- runif(25, 0, 100) ## 'x1' is a random variable uniformly distributed. 25 observations,
                        ## ranging from 0 to 100.




x2 <- 0.25*x1        ## we are generating a variable 'x2', which is clearly a linear 
                        ## function of 'x1'.




y_multi <- 0.5*x1 + 0.5*x2 + rnorm(25)  ## 'y' is a function of x1, x2, and also has a random,
                                        ## normally distributed component.

ols_multi <- lm(y_multi ~ x1 + x2)


summary(ols_multi)                      ## What happened?


#=================================================================================================#


#========= Assumption 7: the error term is normally distributed.



data_het <- data_het %>% cbind(resid_het)


data_het %>% ggplot(aes(resid_het)) + 
  geom_histogram(aes(y=..density..), color='black', fill='white', bins=20) + 
  geom_density(alpha=.2, fill="red") + theme_bw()


resid_het %>% shapiro_test()


data_clrm2 <- data_clrm2 %>% cbind(u_term)



data_clrm2 %>% ggplot(aes(u_term)) + 
  geom_histogram(aes(y=..density..), color='black', fill='white', bins=20) + 
  geom_density(alpha=.2, fill="red") + theme_bw()


u_term %>% shapiro_test()




