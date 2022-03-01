

#== ECON 4650-001 -- Spring 2021
#== Marcio Santetti

#=============================================================================#
#                          MORE ON FUNCTIONAL FORMS                           #
#=============================================================================#


#== IMPORTANT: Before any operations, make sure to set your working directory.
# In other words, you have to tell R in which folder you will save your work, or
# from which folder external data sets will come from. In the lower-right pane, 
# click on 'Files'. Select your desired folder, and click on 'More', then select 
# the option 'Set as Working Directory'.

#==============================================================================#



#== Installing and/or loading required packages:

library(tidyverse)
library(broom)
library(wooldridge)
library(car)  


#==============================================================================#


#========== Regression through the origin (without the intercept term):



data('saving')

saving <- as_tibble(saving)



model_origin <- lm(cons ~ 0 + inc, data = saving)    ## we just need to put a '0' after the '~' symbol.

model_origin %>% summary()


## A little data visualization...


saving %>% ggplot(mapping = aes(x=inc, y=cons)) + geom_point() +
  geom_smooth(method="lm", se=FALSE)    ## Is this the best functional form?


#== For this model, however, we lose an important component of the model by dropping 
#== the intercept.


model_no_origin <- lm(cons ~ inc, data = saving)

model_no_origin %>% summary()


#== In case we exclude the negative observations for 'cons', we may have a proper 
#== regression model without an intercept.


saving_positive <- saving %>% filter(cons > 0)


saving_positive %>% ggplot(mapping = aes(x=inc, y=cons)) + geom_point()


model_origin2 <- lm(cons ~ 0 + inc, data = saving_positive)   ## better, isn't it?

model_origin2 %>% summary()



## And, including an intercept:


model_no_origin2 <- lm(cons ~ inc, data = saving_positive)

model_no_origin2 %>% summary()   ## These two last models make more sense according to this functional form.
                                 ## However, we've done a little 'data massaging' to achieve our goal.
                                 ## Be careful with such practices!


#======================================================================================================#


#========== Using quadratic terms:


data('hprice2')

hprice2 <- as_tibble(hprice2)



model_quad <- lm(lprice ~ lnox + log(dist) + rooms + 
                   I(rooms^2) + stratio, data = hprice2) ## notice the use of the 'I()' function for
                                                         ## the squared term. It is necessary, since
                                                         ## this function tells R to read the squared variable
                                                         ## "as it is."

model_quad %>% summary()


hprice2 %>% ggplot(mapping=aes(x=nox, y=lprice)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE, formula = "y ~ x + I(x^2)")


#== How does the number of rooms relate to the price of a house?

## Interpretation: At low values (number) of rooms, an additional room has a negative 
## effect on log(price). Then, at some point, the effect becomes positive, due to the 
## positive sign on rooms^2. Quantitatively, 
## 100*[-.5451 + 2*(.0622)*(rooms)]. Since the variable 'room' remains here for the 
## marginal effect computation, a good practice is to put the average number of that
## variable. In this case, rooms.

hprice2 %>% summarize(mean(rooms))

## Rounding it, the average number of rooms in this sample is 6. Plugging in 6 there,
## we get the marginal effect of the number of rooms in the price of a house. Given
## that the dependent variable is in logs, we multiply it by 100 and get the result as a 
## percentage. Therefore,

## 100*[-.5451 + 2*(.0622)*(6)] = 20.13. Ceteris paribus, for a house starting with 6
## rooms, an additional room will increase its price by 20.13%.



#== A log-log model with quadratic terms:


model_quad2 <- lm(lprice ~ lnox + I(lnox^2) + crime + rooms + I(rooms^2) + stratio, data = hprice2)

model_quad2 %>% summary()



## How does the concentration of nitrous oxide (nox) relate to the price of a house?

## The marginal effect will be [2.438 + 2*-.844*(log(nox))]. If we insert the mean
## of log(nox), we obtain our answer. [2.438 + 2*-.844*(1.69)] = -0.41472%. Therefore,
## for a 1% increase in nitrous oxide concentration, the price of a house decreases by
## .4147%.


#==========================================================================================#


#========= Inverse terms:


chicken_data <- read_csv('newbroiler.csv')


inverse_model <- lm(q ~ p + I(1/p), data = chicken_data)

inverse_model %>% summary()

chicken_data %>% ggplot(mapping = aes(x=p, y=q)) + geom_point() + 
  geom_smooth(method="lm", se=FALSE, formula = "y ~ I(1/x)")


## Interpretation:

# Marginal effect =  [-beta_(price)/p^2 = -48.365/mean(p)^2]

chicken_data %>% summarize(mean(p))

# A one-unit increase in the real price index of chicken decreases per capita consumption of chicken by
# [-48.365/(1.45^2) =] 23 pounds.





#==========================================================================================#


#========= Interaction terms:


data("attend")


## whenever we wish to add interaction terms, we use a ":" symbol between the variables.


model_int <- lm(stndfnl ~ atndrte + priGPA + ACT + I(priGPA^2) + I(ACT^2) + priGPA:atndrte, data = attend)

model_int %>% summary()




#== What is the partial effect of percent classes attended (atndrte) on 
#== the standardized exam score (stndfnl)?

## The partial effect is given by (-.0067 + .0056*(priGPA)). Plugging in the mean 
## cumulative GPA prior to the term,

attend %>%  summarize(mean(priGPA))

## (-.0067 + .0056*(2.59)) = approx. .0079. Given that atndrte is measured as a 
## percentage, it means that a 10 percentage point increase in
## atndrte increases 'stndfnl' by .079 standard deviations from the mean final exam score, all else constant.




#== What is the partial effect of prior GPA (priGPA) on 
#== the standardized exam score (stndfnl)?

## The partial effect is given by -1.628540 + 2*0.295905*priGPA + 0.005586*atndrte.


attend %>% summarize(mean(atndrte))


## (-1.628540 + 0.005586*81.70956 + 2*0.295905*2.586775) = 0.3587


## All else constant, if the cumulative GPA prior to the term increases by 1 point,
## increases the standardized final score by 0.35 standard deviations.


#==============================================================================#


#========== Using dummy (binary) variables:


##=== Intercept dummy variables:


data('wage1')


model_dummy <- lm(wage ~ female + educ + exper + tenure, data = wage1)

model_dummy %>% summary()

#== Female is a dummy variable that attributes 1 if the individual is female, and 0 if
#== otherwise.

## To interpret the coefficient on 'female', we state the following: If we take a female 
## and a male worker with the same levels of
## education, experience, and tenure, a female earns, on average, $1.81 less per 
## hour than a male worker, all else constant. 


data('gpa1')


model_dummy2 <- lm(colGPA ~ PC + hsGPA + ACT, data = gpa1)

model_dummy2 %>% summary()



## Interpreting the effect of computer ownership on the GPA score: a student who owns a 
## PC has a predicted GPA about .16 points higher than a comparable 
## student without a PC, all else constant.


## For a graphical representation, we can use 'ggplot2'. The slopes will be same for each
## category (PC=1 and PC=0), while the intercept changes.

gpa1 %>% ggplot(mapping = aes(y=colGPA, x=PC, color=as_factor(PC))) + 
  geom_point() + geom_smooth(method='lm', se=FALSE) + theme_bw()

gpa1 %>% ggplot(mapping = aes(y=colGPA, x=ACT, color=as_factor(PC))) + 
  geom_point() + geom_smooth(method='lm', se=FALSE) + theme_bw()





##=== Slope dummy variables (interactive terms):


model_dummy3 <- lm(wage ~ female + educ + female:educ + exper + I(exper^2) +
                     tenure + I(tenure^2), data = wage1)

model_dummy3 %>% summary()







#== What is the estimated return to education for men?
## [0.5759 + (-0.1273)*0=] $ 0.5759

#== What is the estimated return to education for women?
## [0.5759 + (-0.1273)*1=] $ 0.4486


#== This means that the wage gender DIFFERENTIAL between male and female workers with the SAME
#== educational attainment is given by the
#== difference between the two values above (0.5759 - 0.4486 = ) 0.1273, or simply the 
#== coefficient on the slope dummy variable.

##=================================================================##


##========== What if we have log-level models with dummy variables?


## When we have log-level models with dummy variables on the right-hand side, our
## interpretations must change slightly. Let's see:


#== Intercept dummy variables:


model_dummy4 <- lm(lwage ~ female + educ + exper + I(exper^2) +
                     tenure + I(tenure^2), data = wage1)

model_dummy4 %>% summary()


# How to interpret the effect of gender on wages?

# When female=1 (i.e., for female workers), hourly wages are 100*[exp(beta_female) - 1] = 
# 100*[exp(-0.2965) - 1 =] 25.65% lower for female workers, relative to male workers.

# When female=0 (i.e., for male workers), hourly wages are 100*[exp(-beta_female) - 1] = 
# 100*[exp(+0.2965) - 1 =] 34.51% higher for male workers, relative to female workers.



#== Slope dummy variables:

## The above calculation gives the exact effect of the binary variable on the logged independent variable.
## For slope dummy variables, we can use the usual derivative approach, which will give an approximate
## marginal effect.



model_dummy5 <- lm(lwage ~ female + educ + female:educ + exper + I(exper^2) +
                     tenure + I(tenure^2), data = wage1)

model_dummy5 %>% summary()


#== What is the estimated return to education for men?
## 100*[.082 + (-.0056)*0=] 8.2%.

#== What is the estimated return to education for women?
## 100*[.082 + (-.0056)*1=] 7.6%.

## The gender differential for education between men and women is given by the 
## coefficient on the slope dummy variable female*educ.
## Therefore, the difference is of .56% percentage point less for women. However, this 
## coefficient is not statistically significant for this sample.




