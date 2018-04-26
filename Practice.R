library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)
library(gapminder)
?gapminder
dat = gapminder
glimpse(dat)

ggplot(dat, aes(gdpPercap)) + geom_histogram()

ggplot(dat, aes(lifeExp)) + geom_histogram()

library(fitdistrplus)
?fitdist

plot(fitdist(dat$lifeExp, distr = "norm"))
plot(fitdist(dat$lifeExp, distr = "logis"))

plot(fitdist(dat$lifeExp, distr = "norm"))
hist(dat$lifeExp)
ggplot(dat, aes(x=lifeExp, y=gdpPercap, col = continent)) + geom_point() + stat_smooth()

ggplot(dat, aes(x=gdpPercap, y=lifeExp, col = pop)) + geom_point() + stat_smooth()

### pretty difficult to read, going to try subsetting
glimpse(dat$year)
glimpse(dat$pop)
summary(dat$country)
summary(dat$continent)

dat1 = subset(dat, continent %in% c("Americas"))
summary(dat1$country)
dat1_compare = subset(dat1, country %in% c("Mexico", "United States", "Canada"))

ggplot(dat1_compare, aes(x=lifeExp, y=gdpPercap, col = country)) + geom_point() + 
  stat_smooth() 

### ok this has gotten away from me, going to do different subsetting ###

glimpse(dat)
dat$country
dat2 = subset(dat, country %in% c("Canada", "United States", "Mexico", "Brazil"))

ggplot(dat2, aes(x=lifeExp, y=gdpPercap, col = country)) + geom_point() + 
  stat_smooth() 
## looks like brazil and mexico data have some interaction 



