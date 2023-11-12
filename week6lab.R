## load the packages we will be using
library(tidyverse)
library(lattice)
library(moderndive)
library(skimr)
library(lme4)

## work with the example gapminder data 
install.packages("gapminder")
library(gapminder)

## filter for just the 2007 data then ignore the year column
gapminder2007 = gapminder %>%
  filter(year == 2007) %>%
  select(country,continent,lifeExp,pop,gdpPercap)

## observe the structure of this dataset
str(gapminder2007)

## summarise the life expectancy by continent
lifeExp_by_continent = gapminder2007 %>%
  group_by(continent) %>%
  summarize(mean = mean(lifeExp), median = median(lifeExp))
lifeExp_by_continent

## try fitting a model of life expectancy, predicted by continent
lifeExp_continent_model = lm(data = gapminder2007, formula = lifeExp ~ continent)
summary(lifeExp_continent_model)


## now using the entirety of gapminder data, not just 2007
## try fitting a model of life expectancy, predicted by year and GDP per capita
model2 = lm(data = gapminder, 
                            formula = lifeExp ~ year + gdpPercap)
summary(model2)

## a low R squared suggests the fit may not be good, so check some diagnostics
par(mfrow = c(2,2))
plot(model2)
## the residuals vs fitted plot should be completely random
## the Q-Q residuals plot should be a straight line
## the fact they are not implies that the errors are not normally distributed
## the scale-location plot should be completely random

## try a model with a log transform
gapminder = gapminder %>%
  mutate(loggdpPercap = log(gdpPercap))
model3 = lm(data = gapminder, formula = lifeExp ~ year + loggdpPercap)
summary(model3)

par(mfrow = c(2,2))
plot(model3)
## this one is an improvement


## try a polynomial regression model
model4 = lm(data = gapminder, 
            formula = lifeExp ~ year + gdpPercap + I(gdpPercap^2))
summary(model4)

par(mfrow = c(2,2))
plot(model4)


## observe whether being in a given continent rather than another has any effect
## on the correlation between life expectancy and year
gapminder %>%
  ggplot(aes(x = year, y = lifeExp, color = continent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## the slopes are different, but run a linear model with interaction terms to check
model5 = lm(data = gapminder, formula = lifeExp ~ year*continent)
summary(model5)
## being in europe or oceania has less effect on the correlation between life
## expectancy and year, indicated by the lower significances on their interactions




## we move onto looking at multilevel modelling
## load the foreign library to load the file
library(foreign)
school = read.dta("school.dta")
attach(school)
str(school)

## to start, plot a simple linear model between homework and math (their math grade)
model6 = lm(data = school, formula = math ~ homework)
summary(model6)

## plot this
school %>%
  ggplot(aes(x = homework, y = math)) +
  geom_point() + 
  geom_smooth(method = "lm")

## try the same, but with a line of regression for each of the 10 schools
school %>%
  ggplot(aes(x = homework, y = math, color = as.factor(schnum))) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE)
## we can see that every school has different correlations

## produce a random slope and random intercept model
modb = lmer(formula = math ~ homework + (1 + homework|schnum), REML = FALSE)
summary(modb)

## visualise this
predictedmath = fitted(modb)
xyplot(predictedmath ~ homework, data = school, groups = schnum,
       typ = c("p","l"), col = "red")





















