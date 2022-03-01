

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                      SERIAL CORRELATION/AUTOCORRELATION                     #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files'. Select your desired folder, and click on 'More', then select 
# the option 'Set as Working Directory'.


#==============================================================================#



#== Installing and/or loading required packages:

library(tidyverse)
library(tsibble)
library(lmtest) 
library(orcutt)
library(patchwork)
library(forecast)


#======================================================================================#


#========== First, we will demonstrate serial correlation through artificial data.


dates <- yearquarter("1996-01-01") + 0:99


set.seed(1234)

gamma <- rnorm(n=100, mean=0, sd=1)          ## creating a 'gamma' random variable.



error <- rep(0, 100)     ## 'error', for now, is a vector of 100 spots with 0 values.
error

error[1]                ## 'error''s first observation.

gamma[1]                ## 'gamma''s first obervation.



##== We will define error's first observation as gamma's first observation. The 
##== command is the following:

error[1] <- gamma[1]

error[1]                ## did it work?



#== Now, let us take care of the other 99 observations contained in 'error'. We will
#== do it using a 'for' loop function.


for(i in 2:100){
  error[i] <- 0.95 * error[i-1] + gamma[i]}   ## error's other spots will be a function
                                              ## of its own lagged observations 
                                              ## (error[i-1]), as well as a random 
                                              ## component coming from 'gamma'. 
                                              ## Therefore, 'gamma' is a stochastic 
                                              ## residual inside 'error'.


##== Now, checking out our 'error' variable:

error



##== Now, let's create an independent variable 'x', ranging from 0 to 100.

x <- seq(1, 100, length=100)



##== Now, a dependent variable 'y', which is a function of 'x' and has a stochastic part
##== term, 'error'. Therefore, the stochastic component of 'y' also has a stochastic part.


y <- 2 + 0.5*x + error            # true process governing 'y'.


##== Just for fun, let's create an error term which is simply normally distributed, 
##== to see what happens if this term is part of the true process defining y.
#== We call this other dependent variable "y_alternative".


error_normal <- rnorm(n=100, mean=5, sd=2)

y_alternative <- 2 + 0.5*x + error_normal



##=== Preparing the data set (using a "tsibble" -- a tibble for time-series data):


data_set <- data.frame(dates, y, y_alternative, x, error, error_normal) %>% as_tsibble(index = dates)


##=== Quick visual inspection:


data_set %>% ggplot(aes(x=dates, y=error)) + geom_point() + geom_line()

data_set %>% ggplot(aes(x=dates, y=y_alternative)) + geom_point() + geom_line()

data_set %>% ggplot(aes(x=dates, y=error_normal)) + geom_point() + geom_line()



##=== Estimating regression models:

##== First, with 'y', whose true definition has a serially correlated stochastic term, "error".


model_1 <- lm(y ~ x, data = data_set)   

model_1 %>% summary()


##== And let's model this error term's Markov process:


resid_1 <- model_1 %>% resid()

data_set <- data_set %>% add_column(resid_1)  ## adding it to the data set.

resid_model1 <- lm(resid_1 ~ 0 + lag(resid_1, n = 1), data = data_set)

resid_model1 %>% summary()   ## what is the value of "rho", the autocorrelation coefficient?


##== Now, a model for y_alternative, whose stochastic component is normally distributed, with no autocorrelation.


model_2 <- lm(y_alternative ~ x, data = data_set)

model_2 %>% summary()


resid_2 <- model_2 %>% resid()   ## extracting its residual.


data_set <- data_set %>% add_column(resid_2)  ## adding it to the data set.

resid_model2 <- lm(resid_2 ~ 0 + lag(resid_2, n = 1), data = data_set)

resid_model2 %>% summary()   ## what is the value of "rho", the autocorrelation coefficient?



##=== Testing for serial correlation:

##== Let's first use the R commands for the Durbin-Watson (DW) and Breusch-Godfrey (BG) tests,
##== using the functions from the 'lmtest' package.

model_1 %>% dwtest() ## H0: there is no serial correlation (CLRM Assumption IV is satisfied).

model_1 %>% bgtest(order = 1, fill = NA)     ## the "fill = NA" argument will be made clear in a moment.



model_2 %>% dwtest()

model_2 %>% bgtest(order = 1, fill = NA)     ## what do we conclude from these tests?



##===========================================================================================================##


##============ Now, using real data:


##=== Okun's law model:


okun <- read_csv('okun.csv') 

okun <- okun %>% mutate(date = yearquarter(date))    ## setting the 'date' column to an actual date column.

okun <- okun %>% as_tsibble(index = date)                    ## now, setting it as a tsibble.


okun <- okun %>% mutate(du = u - lag(u, n=1))        ## creating the change in unemployment variable.



##== Visual inspection:


plot1 <- okun %>% ggplot(aes(x=date, y=du)) + geom_line(color="red") + geom_point(color="red")

plot2 <- okun %>% ggplot(aes(x=date, y=g)) + geom_line(color="blue") + geom_point(color="blue")


plot1 / plot2    ## using the "patchwork" package...



##== Regression model:


okun_model <- lm(du ~ g, data = okun)

okun_model %>% summary()



okun_resid <- okun_model %>% resid()

okun <- okun %>% add_column(okun_resid = c(NA, okun_resid))  ## we have to add an "NA" value, since we have one 
                                                             ## less observation for "du".


##== And we plot the residuals:

okun %>% ggplot(aes(x=date, y=okun_resid)) + geom_line() + geom_point()


##== Testing for serial correlation:

okun_model %>% dwtest()

okun_model %>% bgtest(order = 1, fill = NA)   ## what do we conclude?


##== Now, where do these results come from?


## First, for the DW test.

## Its test statistic can be approximated by 2*(1 - rho), 
## where "rho" is the error term's autocorrelation coefficient.

## So let's estimate "rho":

okun_resid_lm <- lm(okun_resid ~ 0 + lag(okun_resid, n = 1), data = okun)

okun_resid_lm %>% summary()   ## what is its value?

rho <- 0.3339

## We can also look at the Autocorrelation Function (ACF) plot for this residual:

okun %>% select(du) %>% ggAcf() + 
  labs(title = "ACF plot for Okun's law residual term")   ## this plot gives "rho" at every lag.


## Next, we just plug into the DW test statistic:

dw_stat <- 2*(1 - rho)    ## is this test statistic close the the "dwtest()" command's?

## Given this test statistic, we simply look for lower and upper critical values in the DW table.



##== Now, to the Breusch-Godfrey test.

## It requires a more comprehensive auxiliary regression for the regression's residuals,
## also including the independent variable(s) from the original model.


okun_resid_lm2 <- lm(okun_resid ~ lag(okun_resid, n=1) + g, data = okun)

okun_resid_lm2 %>% summary()

## Its test statistic is given by LM = (n - q)*R2, where R2 is the coefficient of determination
## from this latter auxiliary regression. "n" is the sample size, and "q" is the
## order of autocorrelation we are testing for.


lm_stat <- (152 - 1)*0.1202   ## recall that the sample size is 152 because we lose one observation by using "du".

## Is this test statistic close to the one given by the "bgtest()" function?



##=== Given that we have serial correlation in our Okun's law model,
##=== we cannot trust in its standard errors. Therefore, inference from this model is unreliable.


##=== As a solution for first-degree serial correlation, we can use the Cochrane-Orcutt (CO) estimator:


co_okun_model <- okun_model %>% cochrane.orcutt()

co_okun_model %>% summary()    ## do we still have serial correlation?



##=== Now, doing the CO procedure manually:


okun <- okun %>% mutate(du_lag = lag(du, n = 1),
                        g_lag = lag(g, n = 1))   ## incorporating the first lags of both "g" and "du"



## Recall that we need quasi-differenced values for our independent variables.

## In this case, g_tilde = g_t - rho*g_(t-1), and du_tilde = du_t - rho*du_(t-1).

## This way, our model becomes nonlinear in parameters. Thus, we use the "nls" function:


co_manual <- nls(du ~ b0*(1-rho) + b1*g - b1*rho*g_lag + rho*du_lag,
                 data = okun,
                 start = list(b0 = 0.5, b1=-0.5, rho=0.5))  ## this list requires only initial values
                                                            ## to get the function going.


co_manual %>% summary()   ## are these results the same?


##===========================================================================================================##


##============ A second example: interest rates vs. profit rates in the US (1955-2016)


##=== Data prep:


macro_data <- read_csv('annual_data.csv')

macro_ts <- macro_data %>% as_tsibble(index=year)


macro_ts <- macro_ts %>% mutate(int_rate = ifedfunds - infrate,
                                profit_rate = (cp/k) * 100)      ## creating time series for the real interest
                                                                 ## rate and the profit rate.



##=== Visual inspection:


plot3 <- macro_ts %>% ggplot(mapping = aes(y=int_rate, x=year)) + geom_line(size=0.7, color="blue") + 
  geom_point(color="blue") + theme_minimal()

plot4 <- macro_ts %>% ggplot(mapping = aes(y=profit_rate, x=year)) + geom_line(size=0.7)  + geom_point() +
  theme_minimal()


plot3 / plot4


##=== Regresion model:


macro_model <- lm(int_rate ~ profit_rate, data = macro_ts)

macro_model %>% summary()


macro_resid <- macro_model %>% resid()   ## extracting residuals.

macro_ts <- macro_ts %>% add_column(macro_resid)


##=== Testing for serial correlation:

macro_model %>% dwtest()

macro_model %>% bgtest(order = 1)     ## what do we conclude?




##=== Cochrane-Orcutt estimator:


co_macro_model <- macro_model %>% cochrane.orcutt()

co_macro_model %>% summary()     ## what do we conclude?


##===========================================================================================================##
