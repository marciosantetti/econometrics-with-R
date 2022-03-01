

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                      AN INTRO TO PANEL DATA MODELS                          #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files'. Select your desired folder, and click on 'More', then select 
# the option 'Set as Working Directory'.


#==============================================================================#



#== Installing and/or loading required packages:


library(tidyverse)
library(scales)
library(broom)
library(lmtest)
library(pmdplyr)
library(viridis)
library(janitor)
library(plm)



#==================================================================================================================#



###=== Let us start off with some data from the National Longitudinal Survey (NLS), a panel of data 
###=== collected between 1982 and 1988 for 716 working women.



nls_data <- read_csv("nls_panel.csv")

nls_pd <- nls_data %>% as_pibble(.i = id, .t = year, .d = 0)   ## for panel data, a nice way to organize the data
                                                               ## is to use a 'pibble', from the 'pmdplyr' package. 



## Let us estimate a model for the log of wages, controlling for age, education, race, union membership, tenure, 
## and experience, using different fixed effects estimators and the random effects estimator.




##== The "within" estimator:



nls_pd <- nls_pd %>% group_by(id) %>% mutate(tilde_lwage = lwage - mean(lwage),
                                   tilde_exper = exper - mean(exper),
                                   tilde_educ = educ - mean(educ),
                                   tilde_tenure = tenure - mean(tenure),
                                   tilde_age = age - mean(age),
                                   tilde_black = black - mean(black),
                                   tilde_union = union - mean(union)) %>% ungroup()


nls_within <- lm(tilde_lwage ~ 0 + tilde_age + tilde_educ + tilde_black + tilde_union + 
                   tilde_tenure + tilde_exper, data = nls_pd)

nls_within %>% tidy()




##== The "least-squares dummy variable" estimator:



nls_dummy <- lm(lwage ~ 0 + age + educ + black + union + tenure + exper + factor(id), data = nls_pd)

nls_dummy %>% tidy()



##== The Fixed Effects estimator (with "plm"):



nls_fixed <- plm(lwage ~ age + educ + black + union + tenure + exper,
                 data = nls_pd,
                 index = c("id", "year"),
                 model = "within")          ## "within" means fixed effects estimation.


nls_fixed %>% tidy()


## Ceteris paribus, an additional year of experience will increase an individual's wages by [100*0.0306 = ]
## 3.06%.


##== The Random Effects estimator:


## Whenever we believe that there is no correlation between the unobserved heterogeneity
## residual term (u) and the time-varying independent variable(s), we may use the random 
## effects estimator for our panel data models.


nls_random <- plm(lwage ~ age + educ + black + union + tenure + exper,
                  data = nls_pd,
                  index = c("id", "year"),
                  model = "random")


nls_random %>% tidy()

## Ceteris paribus, an additional year of education increases an individual's wages by [100*.0739=] 7.39%.



##== Fixed or Random effects?


## Whenever in doubt, we can conduct a Hausman test to decide whether random effects are
## preferred over fixed effects.

## Its null hypothesis states that there is NO correlation between the time-varying independent
## variable(s) and the unobserved heterogeneity (u) residual term. 

## In case we reject the null hypothesis,
## only the fixed effects estimator is valid. 

## If we do not reject, both methods are valid, but
## given that random effects are more efficient than fixed effects 
## estimators, we prefer the random effects model.



phtest(lwage ~ age + educ + black + union + tenure + exper,
       data = nls_pd,
       method = "chisq")                                         


## Or...


phtest(nls_fixed, nls_random, 
       method = "chisq")               ## what do we conclude?

# H0: there is no correlation between the unobserved heterogeneity and the time-varying independent variables.
# (both fixed and random effects are consistent -- given that RE are more more efficient, we choose the latter.)

# Ha: the null hypothesis is not true (only fixed effects are consistent; random effects are not valid.)

## In this case, fixed effects are the only consistent estimator.

##====================================================================================================================##




##=== A second example: unemployment and poverty rates and minimum wages in the US



us_data <- read_csv("us_data.csv")        ## take a look at the column names.

us_data <- us_data %>% clean_names()      ## with the 'clean_names()' function, we make the column names more readable.



## Some data cleaning:


us_data <- us_data %>% 
  arrange(state_name) %>% 
  select(state_name, year, population, unemployment_rate, federal_minimum_wage, state_minimum_wage,
         gross_state_product, personal_income, poverty_rate) %>% 
  mutate(gsp_percapita = gross_state_product/population) %>% 
  drop_na() 



us_pd <- us_data %>% as_pibble(.i = state_name, .t = year, .d = 1)    ## now, a 'pibble.'



##== Some visual inspection:


# Poverty rate:


us_pd %>% filter(state_name %in% c("UT", "CA", "TX", "NY", "FL")) %>%
  ggplot(aes(x = year, y = poverty_rate, color = state_name)) +
  geom_point(size = 2, alpha = 0.4) +
  geom_line(size = 0.7, alpha = 0.5) +
  labs(title = "Poverty rate", x = "", y = "%") +
  scale_color_viridis_d("State", option = "C", end = 0.7)




# Unemployment rate:


us_pd %>% filter(state_name %in% c("UT", "CA", "TX", "NY", "FL")) %>%
  ggplot(aes(x = year, y = unemployment_rate, color = state_name)) +
  geom_point(size = 2, alpha = 0.4) +
  geom_line(size = 0.7, alpha = 0.5) +
  labs(title = "Unemployment rate", x = "", y = "%") +
  scale_color_viridis_d("State", option = "A", end = 0.7)


# State minimum wage:


us_pd %>% filter(state_name %in% c("UT", "CA", "TX", "NY", "FL")) %>%
  ggplot(aes(x = year, y = state_minimum_wage, color = state_name)) +
  geom_point(size = 2, alpha = 0.4) +
  geom_line(size = 0.7, alpha = 0.5) +
  labs(title = "State mimimum wage", x = "", y = "Wage per hour") +
  scale_y_continuous(labels = dollar) +
  scale_color_viridis_d("State", option = "B", end = 0.7)


# Gross state product per capita:


us_pd %>% filter(state_name %in% c("UT", "CA", "TX", "NY", "FL")) %>%
  ggplot(aes(x = year, y = gsp_percapita, color = state_name)) +
  geom_point(size = 2, alpha = 0.4) +
  geom_line(size = 0.7, alpha = 0.5) +
  labs(title = "Gross state product per capita", x = "", y = "GSP per capita") +
  scale_color_viridis_d("State", option = "B", end = 0.7)




##== Fixed Effects model:


us_fixed <- plm(poverty_rate ~ unemployment_rate + state_minimum_wage,
                data = us_pd,
                index = c("state_name", "year"),
                model = "within")

us_fixed %>% tidy()               ## interpretation?



##== Random Effects model:


us_random <- plm(poverty_rate ~ unemployment_rate + state_minimum_wage,
                data = us_pd,
                index = c("state_name", "year"),
                model = "random")

us_random %>% tidy()             ## interpretation?



##== Hausman test:


phtest(us_fixed, us_random, 
       method = "chisq")         ## what do we conclude?



##== Testing for heteroskedasticity/serial correlation:


## In case we want to test for heteroskedasticity, we must we the "bptest()" function from the
## 'lmtest' package.


us_fixed %>% bptest()

us_fixed %>% bgtest()            ## what do we conclude? The fixed effects model shows evidence
                                 ## of heteroskedasticity and serial correlation.  





library(ggrepel)


us_pd %>% filter(year == max(year)) %>% 
  filter(state_name %in% c("UT", "CA", "TX", "NY", "FL")) %>% 
  ggplot(aes(x=poverty_rate, y=unemployment_rate)) + geom_point() +
  geom_text_repel(aes(label = state_name))

                  