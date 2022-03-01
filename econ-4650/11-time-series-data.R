

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                      TIME-SERIES REGRESSION MODELS                          #
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
library(forecast)
library(lmtest)
library(car)
library(patchwork)
library(scales)
library(orcutt)
library(feasts)
library(lubridate)


#==================================================================================================#


##=== Let us start off with some data from the US Environmental Protection Agency (EPA) [https://www.epa.gov/].
##=== It contains monthly data on greenhouse gas emissions, electrical plant retirements, electricity generation, etc.


## Some data prep:


energy_data <- read_csv("energy_data.csv")


energy_data <- energy_data %>% select(-t)                    ## eliminating the 't' column.

energy_ts <- energy_data %>% as_tsibble(index = month)       ## Now, a 'tsibble' with the date column as its index.


## Quick visual inspection:


p1 <- energy_ts %>% ggplot(aes(x=month, y=generation_gwh)) + geom_line(size=0.4) +
  geom_point() + scale_x_yearmonth() + 
  scale_y_continuous(name = "Electricity generation (GWH)", labels = comma) +
  labs(title = "Monthly electricity generation")

p2 <- energy_ts %>% ggplot(aes(x=month, y=emissions_nox)) + geom_line(size=0.4) +
  geom_point() + scale_x_yearmonth() + 
  scale_y_continuous(name = "Nitrous oxide emissions", labels = comma) +
  labs(title = "NOx emissions")

p3 <- energy_ts %>% ggplot(aes(x=month, y=n_retirements)) + geom_line(size=0.4) +
  geom_point() + scale_x_yearmonth() + 
  scale_y_continuous(name = "Retired units", labels = comma) +
  labs(title = "Number of retired units")


p1 / p2 / p3




## A static model:


energy_static <- lm(emissions_nox ~ n_retirements + generation_gwh, data = energy_ts)

energy_static %>% summary()


# Checking the model's residuals:


energy_static %>% checkresiduals()   ## a very nice function from the 'forecast' package.




energy_static_co <- energy_static %>% cochrane.orcutt()

energy_static_co %>% summary()       ## did it solve the serial correlation problem?


## A distributed lag model:


energy_dl <- lm(emissions_nox ~ n_retirements + lag(n_retirements, 1) + lag(n_retirements, 2) +
                  lag(n_retirements, 3) + generation_gwh, data = energy_ts)

energy_dl %>% summary()


# Checking for multicollinearity:


energy_dl %>% vif()                 ## what do we conclude?


# Checking residuals:


energy_dl %>% checkresiduals()


energy_dl %>% bgtest(order = 3, fill=NA)       ## what do we conclude?



## A dynamic model:


energy_dyn <- lm(emissions_nox ~ n_retirements + generation_gwh + lag(emissions_nox, 1) +
                   lag(emissions_nox, 2), data = energy_ts)

energy_dyn %>% summary()


energy_dyn %>% checkresiduals()



## The Lagrange Multiplier Serial Correlation (LMSC) test:


# First, we extract the residuals:


energy_resid <- energy_dyn %>% resid()


energy_ts <- energy_ts %>% add_column(energy_resid = c(NA, NA, energy_resid))


# And the auxiliary regression:


aux_reg <- lm(energy_resid ~ n_retirements + generation_gwh + lag(emissions_nox, 1) +
                lag(emissions_nox, 2) + lag(energy_resid, 1) + lag(energy_resid, 2), data = energy_ts)

aux_reg %>% summary()


# And compute the LM test statistic:


lm_stat <- 172*0.323


qchisq(p = .95, df = 2)                 ## what do we conclude?



#==================================================================================================#



##=== Secondly, we work with US data for aggregate personal consumption, 
##=== household debt, and disposable income.



macro_data <- read_csv("macro_data.csv")


macro_data <- macro_data %>% 
  mutate(date = yearquarter(date))        ## transforming the 'date' column into a 'date' object.



macro_ts <- macro_data %>% as_tsibble(index=date)    ## now, a 'tsibble'.



# Let's plot the three series in the same plot:


macro_ts %>% pivot_longer(c(cons, di, hd), names_to = "Series") %>% 
  autoplot(value, size=0.5) + geom_point(size=0.5) +
  scale_y_continuous(labels = comma) + labs(x="", y="")




# Creating growth rates:


macro_ts <- macro_ts %>% mutate(gcons = ((cons - lag(cons, 4))/lag(cons, 4))*100,
                         gdi = ((di - lag(di, 4))/lag(di, 4))*100,
                         ghd = ((hd - lag(hd, 4))/lag(hd, 4))*100)



# Plotting growth rates:


macro_ts %>% pivot_longer(c(gcons, gdi, ghd), names_to = "Series") %>% 
  autoplot(value, size=0.5) + geom_point(size=0.5) +
  scale_y_continuous(labels = comma) + labs(x="", y="")



## Now, a regression model with the variables in levels:


macro_dl <- lm(cons ~ di + lag(di, 4) + hd + lag(hd, 4), data = macro_ts)

macro_dl %>% summary()


# Checking for multicollinearity:


macro_dl %>% vif()


# Checking residuals:


macro_dl %>% checkresiduals()


## Now, the same model, but with growth rates:


macro_dl_growth <- lm(gcons ~ gdi + lag(gdi, 4) + ghd + lag(ghd, 4), data = macro_ts)

macro_dl_growth %>% summary()


# Checking for multicollinearity:


macro_dl_growth %>% vif()


# Checking residuals:


macro_dl_growth %>% checkresiduals()



## Now, a dynamic model for consumption:


macro_dyn_growth <- lm(gcons ~ gdi + ghd + lag(gcons,4), data = macro_ts)

macro_dyn_growth %>% summary()


macro_dyn_growth %>% checkresiduals()



##=== Granger causality: H0: the independent variable does not Granger-cause the dependent variable.


## Does disposable income Granger-cause consumption?


grangertest(cons ~ di, order = 2, data = macro_ts)


# Manually...


granger_reg1 <- lm(cons ~ lag(cons, 1) + lag(cons, 2) + lag(di, 1) + lag(di, 2), data = macro_ts)

granger_reg1 %>% linearHypothesis(c("lag(di, 1)=0", "lag(di, 2)=0"), test="F")


## Does consumption Granger-cause disposable income?


grangertest(di ~ cons, order = 2, data = macro_ts)


# Manually...


granger_reg2 <- lm(di ~ lag(cons, 1) + lag(cons, 2) + lag(di, 1) + lag(di, 2), data = macro_ts)

granger_reg2 %>% linearHypothesis(c("lag(cons, 1)=0", "lag(cons, 2)=0"), test="F")




###=== Spurious regressions:


## Let us estimate a regression model with two time series: retail sales index in The Netherlands,
## and data on the price of frozen orange juice concentrate in the orange-growing
## region of Florida.


spurious_data <- read_csv("sales_juice.csv")

spurious_data <- spurious_data %>% mutate(date = as.Date(date, format = "%m/%d/%Y"))


spurious_data <- spurious_data %>% as_tsibble(index=date)



## ...and a regression:


spurious_reg <- lm(dutch_sales ~ frozen_juice, data = spurious_data)

spurious_reg %>% summary()   ## so what?


spurious_data %>% pivot_longer(c(dutch_sales, frozen_juice), names_to = "Series") %>% 
  autoplot(value, size=0.5) + geom_point(size=0.5) +
  scale_y_continuous(labels = comma) + labs(x="", y="")




