

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                     PERFECT/IMPERFECT MULTICOLLINEARITY                     #
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
library(pwt9)
library(car)
library(corrr)




#==============================================================================#



#======= First, let's use artificial data to illustrate the multicollinearity problem.


set.seed(123)



x1 <- runif(n = 100, min = 0, max = 100) ## 'x1' is a uniformly distributed random variable. 100 observations,
                                         ## ranging from 0 to 100.



x2 <- 0.25*x1                            ## we are generating a variable 'x2', which is clearly a linear 
                                         ## function of 'x1'.



y <- 0.5*x1 + 0.5*x2 + rnorm(n=100, mean=0, sd=1)         ## 'y' is a function of x1, x2, and also has a random,
                                                          ## normally distributed component.


data_set <- cbind(y, x1, x2) %>% as_tibble()



## A bit of data visualization:

# Histogram:

data_set %>% ggplot(mapping = aes(x=x1)) + geom_histogram(bins=15, fill="grey", color="black")
data_set %>% ggplot(mapping = aes(x=x2)) + geom_histogram(bins=15, fill="grey", color="black")



# Scatter plots:

data_set %>% ggplot(mapping = aes(x=x1, y=x2)) + geom_point()
data_set %>% ggplot(mapping = aes(x=x1, y=y)) + geom_point()


## ...and a regression model:


ols_multi <- lm(y ~ x1 + x2, data = data_set)


ols_multi %>% summary()                           ## Interpretation?


#=============================================================================================#



#========== An example of imperfect multicollinearity:


x2.2 <- 0.25*x1 + rnorm(n=100, mean=0, sd=1)   ## now, x2.2, has a stochastic term. It is not solely defined
                                               ## by x1 anymore.


# Then, what is the correlation coefficient between 'x1' and 'x2.2'?

data_set %>% summarize(cor_x12 = cor(x1, x2.2))




y.1 <- 0.5*x1 + 0.5*x2.2 + rnorm(n=100, mean=0, sd=1)           ## 'true' model for y.1


# Including 'x2.2' and 'y.1' into 'data_set':


data_set <- data_set %>% cbind(x2.2, y.1)



# ...and a regression model:


ols_multi2 <- lm(y.1 ~ x1 + x2.2, data = data_set)

ols_multi2 %>% summary()



## Visualizing data:

data_set %>% ggplot(mapping = aes(x=x1, y=x2.2)) + geom_point()

data_set %>% ggplot(mapping = aes(x=x2.2, y=y.1)) + geom_point()



#============= A second example (with dummy variables):



mroz_data <- read_csv('mroz.csv')

mroz_data <- mroz_data %>% 
       mutate(unemployed = 1 - lfp)              ## creating a variable called 'unemployed', taking on 1 if
                                                 ## she is unemployed, or 0 otherwise.


## What happens if we include 'unemployed' and 'lfp' in the same model?

dummy_model <- lm(log(faminc) ~ lfp + unemployed + wage + hwage, data = mroz_data)

dummy_model %>% summary()   ## so what?



#== Fixing the problem:

dummy_model_correct <- lm(log(faminc) ~ lfp + wage + hwage, data = mroz_data)

dummy_model_correct %>% summary()                ## now, we have fixed the problem.




#== Next, we will work will real data.



#===========================================================================================#


data("pwt9.1")

pwt <- as_tibble(pwt9.1)


##== Filtering only observations for the US economy (1950-2017):


pwt_sample <- pwt %>% filter(country %in% 'United States of America')


##== And selecting variables

pwt_sample <- pwt_sample %>% select(rgdpna, emp, pop, ccon, cn)


##== A quick correlation matrix:

correlate(pwt_sample) 



##== Some visual checks:

pwt_sample %>% ggplot(mapping = aes(x=pop, y=emp)) + geom_point() + theme_minimal()

pwt_sample %>% ggplot(mapping = aes(x=cn, y=ccon)) + geom_point() + theme_minimal()

pwt_sample %>% ggplot(mapping = aes(x=emp, y=cn)) + geom_point() + theme_minimal()


##== Now, a regression model:


multi_ols <- lm(rgdpna ~ pop + emp + ccon + cn, data = pwt_sample)

multi_ols %>% summary()


##== And let's calculate this model's Variance Inflation Factors (VIFs),
##== with the 'vif()' function from the 'car' package:

multi_ols %>% vif()


## Where do these values come from?


# pop:

aux_pop <- lm(pop ~ emp + ccon + cn, data = pwt_sample)

aux_pop %>% summary()

vif_pop <- 1/(1 - 0.9857)


# emp:

aux_emp <- lm(emp ~ pop + ccon + cn, data = pwt_sample)

aux_emp %>% summary()

vif_emp <- 1/(1 - 0.9746)


# ccon:

aux_ccon <- lm(ccon ~ pop + emp + cn, data = pwt_sample)

aux_ccon %>% summary()

vif_ccon <- 1/(1 - 0.9953)


# cn:

aux_cn <- lm(cn ~ pop + emp + ccon, data = pwt_sample)

aux_cn %>% summary()

vif_cn <- 1/(1 - 0.9942)     ## are these values close to the ones given by the 'vif()' function?



##== Since our VIFs are not very exciting, we estimate a new model:


multi_ols2 <- lm(rgdpna ~ emp + ccon + cn, data = pwt_sample)

multi_ols2 %>% summary()


multi_ols2 %>% vif()    ## still not good.


##


multi_ols3 <- lm(rgdpna ~ emp + ccon + log(cn), data = pwt_sample)

multi_ols3 %>% summary()


multi_ols3 %>% vif()  ## and now?



##


multi_ols4 <- lm(rgdpna ~ log(emp) + cn, data = pwt_sample)

multi_ols4 %>% summary()


multi_ols4 %>% vif()  ## and now?
