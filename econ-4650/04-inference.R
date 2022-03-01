

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#           INFERENCE: CONFIDENCE INTERVALS AND HYPOTHESIS TESTING            #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files'. Select your desired folder, and click on 'More', then select 
# the option 'Set as Working Directory'.

#==============================================================================#



#== Installing and/or loading required packages:

library(tidyverse)
library(wooldridge)
library(broom)
library(car)


#==============================================================================#


#============= First of all, let us have a more detailed look at a regression's summary:


data('wage2')

wage2 <- as_tibble(wage2)


wage_model <- lm(lwage ~ educ + exper + tenure, data = wage2)

wage_model %>% summary()


## The first part of a summary provides measures of relative location for the estimated
## residual term. With that, we can draw a box plot:


resid_wage_model <- wage_model %>% resid()

resid_wage_model %>% boxplot()


## Or, using 'ggplot2'...


wage2 <- wage2 %>% cbind(resid_wage_model)

wage2 %>% ggplot(mapping = aes(x=resid_wage_model)) + geom_boxplot()



## Then, we have a table with the estimated 'beta' coefficients ('Estimate' column);
## their standard errors ('Std. Error' column); the test statistic for the coefficient's
## statistical significance ('t value' column); and this test's p-value ('Pr(>|t|') column).


## The punctuation signs indicate at which level of significance (or
## confidence) the estimated coefficients are statistically 
## significant. For example, '***' means significance at alpha =.001;
## '**', at .01, and so on.


##=== Where do these t-statistics come from?


## Let us calculate a t-test for educ's coefficient. 

#== H0: beta_educ = 0; (the coefficient is not statistically significant)
#== Ha: beta_educ =/ 0      (a two-sided/two-tailed test)


wage_model %>% tidy()

t_test_educ <- 0.0749/0.00651    ## is this value equal to the one given by the tibble above?


# Looking for critical values:

qt(p=.05/2, df=935-3-1)          ## for the lower tail value, using alpha.
qt(p=1-.05/2, df=935-3-1)        ## for the upper tail, using 1-alpha.
                                 ## Note: this is a two-tailed test, so we divide alpha by 2.

# Based on the t-statistic and on the critical value, what is our inference from this test?



## Now, for exper's coefficient.

## Let us calculate a t-test for educ's coefficient. 

#== H0: beta_exper = 0; 
#== Ha: beta_exper =/ 0      (a two-sided/two-tailed test)

t_test_exper <- 0.0153/0.00337   ## is this value equal to the one given by the tibble above?


# Based on the t-statistic and on the critical value, what is our inference from this test?



##=== Finally, below the values for the R2 and the adjusted R2, we have the test statistic
##=== and the p-value for an F-test evaluating the joint significance of all slope coefficients.




##===============================================================================================##


#======== Estimating confidence intervals:


wage_model %>% confint()                ## Interpretation: it is 95% likely that the 
                                        ## 'true' (population) coefficient value lies in this 
                                        ## interval.



wage_model %>% confint(level = 0.99)    ## the default confidence level is 95%.





## Try estimating these intervals manually.




##==============================================================================================##


#=========== Testing for linear restrictions (F-test):


data("mlb1")    ## also from the 'wooldridge' package.

mlb1 <- as_tibble(mlb1)



##=== Let us compute an F-test manually. To do that, we need an 'unrestricted' (full) model, as well
##=== a 'restricted' regression model.



#== The 'unrestricted' model:

unr_model <- lm(log(salary) ~ years + gamesyr + bavg + hrunsyr + rbisyr, data = mlb1)   ## full model.

unr_model %>%  summary()     ## what about individual statistical significances?


## We want to test the null hypothesis H0: beta3 = 0, beta4 = 0, beta5 = 0. In
## other words, we want to test whether batting average, home runs per year, and runs 
## batted per year are jointly significant, once we also control for years as an
## active player and games played per year. We'll use alpha = 1%.


#== The 'restricted' model:

res_model <- lm(log(salary) ~ years + gamesyr, data = mlb1)

res_model %>% summary()


r2_unr <- unr_model %>% glance() %>% select(r.squared)
r2_res <- res_model %>% glance() %>% select(r.squared)      # extracting the R-squared from each model.


dof <- 353 - 5 - 1                                          # degrees-of-freedom from the unrestricted model.


f_stat <- (r2_unr - r2_res)/(1-r2_unr)*dof/3                # ...and the F-test statistic!



#== F critical value:


qf(p=1-.01/2, df1=3, df2=dof)  # Statistical inference?
qf(p=.01/2, df1=3, df2=dof)



#=== A better (and easier) way is to use the command 'linearHypothesis', from the 'car' package.



unr_model %>% linearHypothesis(c('bavg=0', 'hrunsyr=0', 'rbisyr=0'))   # Does this command give the 
                                                                       # same test statistic?





## And, as you may already have figured out, the F-statistics given in the regression summary
## is a test for joint significance for all slope coefficients. We can obtain the same value with
## the 'linearHypothesis' function:


unr_model %>% summary()


unr_model %>% linearHypothesis(c('bavg=0', 'hrunsyr=0', 'rbisyr=0', 'years=0', 'gamesyr=0'))




#===================================================================================================#


#======== Practice:


#== Import the 'cps5_small.csv' data set into your R environment. This data set is available
#== at this week's page on Canvas. Estimate a regression model of
#== wages on experience and education. By now, you should be able to interpret every component of
#== this regression's output, given by the 'summary()' function. Try it out and let me know if you  
#== are having any issues with that.


