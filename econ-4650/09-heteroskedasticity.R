

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                              HETEROSKEDASTICITY                             #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files'. Select your desired folder, and click on 'More', then select 
# the option 'Set as Working Directory'.


#=======================================================================================================#



#== Installing and/or loading required packages:

library(tidyverse)
library(wooldridge)
library(patchwork)
library(estimatr)   ## for robust standard errors.
library(skedastic)  ## for heteroskedasticity tests.


#=======================================================================================================#


#======= We start by studying heteroskedasticity using artificial data:


set.seed(123)   

het_error <- rep(0, 100)  ## creating an error vector with 100 observations equal to zero.
                          ## Soon, these will be filled with meaningful elements.


for(i in 1:100){
  het_error[i] <- rnorm(n=1, mean=0, sd = i/10)}  ## creating a loop to fill the 'error' vector following 
                                                  ## a normal distribution, with a mean of 0 and a variance
                                                  ## equal to each observation (i) over 10.
                                                  ## Therefore, the variance will not be constant, since
                                                  ## it depends on each observation. This loop does this 
                                                  ## procedure 100 times.




x <- seq(1, 100, length=100)       ## creating an 'x' object, a sequence from 1 to 100.

y <- 2 + 0.8*x + het_error         ## y's 'true' model.


##== Now, let's create an error term with constant variance.


homosk_error <- rnorm(n = 100, mean = 10, sd = 3)    ## here, the SD is 3, that is, a constant.

y_homosk <- 2 + 0.8*x + homosk_error                 ## a true model for "y_homosk", with a homoskedastic error term.


##== Joining everything in a single data set:

data_set <- data.frame(y, x, het_error, y_homosk, homosk_error) %>% as_tibble()


##== Quick visual checks:


p1 <- data_set %>% ggplot(aes(y=y, x=x)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) + theme_bw() + labs(title="y vs. x")

p2 <- data_set %>% ggplot(aes(y=y_homosk, x=x)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) + theme_bw() + labs(title="y_homosk vs. x")

p3 <- data_set %>% ggplot(aes(y=het_error, x=x)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) + theme_bw() + labs(title="Heteroskedastic residuals")

p4 <- data_set %>% ggplot(aes(y=homosk_error, x=x)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE) + theme_bw() + labs(title="Homoskedastic residuals")


(p1 + p2) / (p3 + p4)    ## using "patchwork."


#=======================================================================================================#


#========= Testing for heteroskedasticity:


## First, let's estimate 2 regression models from our data set:


reg_1 <- lm(y ~ x, data = data_set)   

reg_1 %>% summary()



reg_2 <- lm(y_homosk ~ x, data = data_set)

reg_2 %>% summary()


##== The Breusch-Pagan test:


reg_1 %>% breusch_pagan()   ## using the function from the 'skedastic' package.

reg_2 %>% breusch_pagan()   ## what do we conclude?




##== The White test:


reg_1 %>% white_lm(interactions = TRUE)

reg_2 %>% white_lm(interactions = TRUE)      ## what do we conclude?



#=======================================================================================================#


#========== Correcting for heteroskedasticity: Robust standard errors


#== Eicker-Huber-White robust standard errors:


reg_1_robust_ehw <- lm_robust(y ~ x, data = data_set, se_type = "HC0")  ## from the 'estimatr' package.

reg_1_robust_ehw %>% summary()



#== MacKinnon-White standard errors:


reg_1_robust_mw <- lm_robust(y ~ x, data = data_set, se_type = "HC1")

reg_1_robust_mw %>% summary()


#=======================================================================================================#


##=== Using real data:


##== As a first applied example, let us use a sample from the 2018 American Community Survey (ACS), 
##== downloaded from IPUMS.


ipums_data <- read_csv("het_data.csv")



## Quick visual inspection:


ipums_data %>% ggplot(aes(y=personal_income, x=education, color=as_factor(i_female))) +
  geom_point(show.legend = FALSE)

ipums_data %>% ggplot(aes(y=personal_income, x=education)) +
  geom_point(show.legend = FALSE) + facet_wrap(~i_female, nrow=2)



## Estimating a regression model:


ipums_model <- lm(personal_income ~ education + i_female, data = ipums_data)

ipums_model %>% summary()


## Extracting the residuals


ipums_resid <- ipums_model %>% resid()

ipums_data <- ipums_data %>% add_column(ipums_resid)


ipums_data %>% ggplot(aes(x=education, y=ipums_resid)) + geom_point(shape=21)  ## do the residuals look
                                                                               ## heteroskedastic?




## Testing for heteroskedasticity:


ipums_model %>% breusch_pagan()

ipums_model %>% white_lm(interactions = TRUE)              ## what do we conclude from these tests?



## Manually estimating the Breusch-Pagan test:


# First, we need to estimate an auxiliary regression, with the squared residual term as the
# dependent variable, and the independent variables from the original model maintained on
# this new model's right-hand-side.


ipums_data <- ipums_data %>% mutate(ipums_resid_sq = ipums_resid^2)

bp_reg <- lm(ipums_resid_sq ~ education + i_female, data = ipums_data)

bp_reg %>% summary()


# Now, we have what we need to compute the Breusch-Pagan's LM test statistic:

lm_stat_bp <- nrow(ipums_data)*0.01579    ## is this test statistic the same as the one given by the
                                          ## "breusch_pagan()" function?


# Now, we compare the test statistic with a critical value from the Chi-squared distribution:


qchisq(p=.95, df=2)  ## the test uses 2 degrees-of-freedom, since we have k=2 independent variables in
                     ## the auxiliary regression.



## Manually estimating the White test:


# The only difference in the White test's procedure is that we take into account model misspecification.
# Therefore, similarly to the RESET test, we put the fitted values for the dependent variable
# and potential functional forms that may be contributing to heteroskedasticity.

ipums_data <- ipums_data %>% mutate(y_fitted = fitted(ipums_model))


white_reg <- lm(ipums_resid_sq ~ y_fitted + I(y_fitted^2), data = ipums_data)

white_reg %>% summary()


# And we compute the test statistic (same as before):

lm_stat_white <- nrow(ipums_data)*0.02489    ## so what?





#== Eicker-Huber-White Robust Standard Errors:


ipums_model_ehw <- lm_robust(personal_income ~ education + i_female, data = ipums_data, se_type = "HC0")

ipums_model_ehw %>% summary()


#== Mackinnon-White Robust Standard Errors:


ipums_model_mw <- lm_robust(personal_income ~ education + i_female, data = ipums_data, se_type = "HC1")

ipums_model_mw %>% summary()


#=======================================================================================================#


##=== A second example: Housing prices data



data("hprice2")   ## from the "wooldridge" package.

hprice2 <- hprice2 %>% as_tibble()



## Visual inspection:


p1 <- hprice2 %>% ggplot(aes(y=lprice, x=log(dist))) + geom_point(color="tomato3") +
  labs(title="Median housing price vs. distance to employment centers", 
       y="Median housing price (logs)", x="Distance (logs)") 

p2 <- hprice2 %>% ggplot(aes(y=lprice, x=lnox)) + geom_point(color="tomato3") + 
  labs(title="Median housing price vs. nitrous oxide concentration", 
       y="Median housing price (logs)", x="NO2 concentration (logs)") 

p3 <- hprice2 %>% ggplot(aes(y=lprice, x=rooms)) + geom_point(color="tomato3") + 
  labs(title="Median housing price vs. number of rooms", 
       y="Median housing price (logs)", x="Rooms") 

p4 <- hprice2 %>% ggplot(aes(y=lprice, x=stratio)) + geom_point(color="tomato3") +
  labs(title="Median housing price vs. average student-to-teacher ratio", 
       y="Median housing price (logs)", x="Student-to-teacher ratio") 


(p1 + p2) / (p3 + p4)


## Estimating regression models:


price_model <- lm(lprice ~ lnox + log(dist) + rooms + stratio, data = hprice2)

price_model %>% summary()


## Testing for heteroskedasticity:


price_model %>% breusch_pagan()

price_model %>% white_lm(interactions = TRUE)   ## what do we conclude?



## Correcting for heteroskedasticity: Robust standard errors:


# Eicker-Huber-White:


price_model_ehw <- lm_robust(lprice ~ lnox + log(dist) + rooms + stratio, data = hprice2, se_type="HC0")

price_model_ehw %>% summary()


# Mackinnon-White:


price_model_mw <- lm_robust(lprice ~ lnox + log(dist) + rooms + stratio, data = hprice2, se_type="HC1")

price_model_mw %>% summary()




#=======================================================================================================#

 
#======= Practice:



#== Download the 'vacation.csv' file, available at this week's module main page.
#== A brief description is given in the text file referring to its data.
#== Import this data set into R and estimate a regression model with 'miles' as the
#== dependent variable, controlling for 'income', 'age', and 'kids'. Run a battery
#== of heteroskedasticity tests. In case such problem is detected, re-estimate the model with
#== heteroskedasticity-robust standard errors. Then, interpret the model's coefficients.



