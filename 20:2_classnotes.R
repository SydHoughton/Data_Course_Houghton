library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)

df = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/mushroom_growth.csv")
glimpse(df)
str(df)

ggplot(df, aes(x=GrowthRate)) + geom_histogram()

df$GrowthRate
library(fitdistrplus)

?fitdist()
plot(fitdist(df$GrowthRate, distr = "norm"))

ggplot(df, aes(x=Nitrogen, y=GrowthRate, col = Species)) + 
  geom_point() + facet_grid(facets = ~Humidity)
# to look for a trend use stat_smooth

ggplot(df, aes(x=Nitrogen, y=GrowthRate, col = Species)) +
  geom_point() + facet_grid(facets = ~Humidity) + stat_smooth()
# at high humidity we see a difference in the species response, looks 
#like we have a reaction between humidity
# and species and nitrogen

# looking at interaction between species and humidity in regaurds to light
ggplot(df, aes(x=Light, y=GrowthRate, col = Species)) + geom_point() + 
  facet_grid(facets = ~Humidity) + stat_smooth()

# look at our first model
mod1 = aov(GrowthRate ~ Light*Species*Humidity*Temperature*Nitrogen, data = df)
summary(mod1)
# what do we see? looking at p-values we see some significant interactions
# light, species and humidity show a significant interaction and that is what our graph showed

# anova by default is a sequential sum of squares, which gives some bias to the model because it runs it 
# in the order you gave it,  Light*Species*Humidity*Nitrogen

P.c = subset(df, Species == "P.cornucopiae")
mod2 = aov(GrowthRate ~ Light*Species*Humidity*Temperature*Nitrogen, data = P.c)
# we get an error, contrasts can be applied only to factors with 2 or more levels
# we have species with 1 level, remove it from the model
mod2 = aov(GrowthRate ~ Light*Humidity*Nitrogen, data = P.c)
summary(mod2)

ggplot(P.c, aes(x=Nitrogen, y=GrowthRate, col= Humidity)) + geom_point() + stat_smooth()
# seems like nitrogen should matter
ggplot(P.c, aes(x=Nitrogen, y=GrowthRate, col= Humidity)) + geom_point() + stat_smooth(method = "lm")
# the anova is looking at straight lines so we do not get a true interpretation. 
# "aov" the anova test cannot do everything, it is just a good way to test our ideas

# next step
mod3 = aov(GrowthRate ~ Light*Humidity, data = P.c)
summary(mod3)
# all significant
# which model is better 2 or 3

anova(mod2,mod3)
# we see that they are not significantly different so we would choose the simpiler model, mod3
# mod3 is good enought for us to accurately describe my data
# but which makes the best predictions?

?predict() # wants a model and new data
predict(obeject = mod3, newdata = ) # we need newdata
df$Light
df$Humidity
new = data.frame(Light = 30, Humidity = "High")

predict(object = mod3, newdata = new) # input the new data we created

ggplot(df, aes(x=Light, y=GrowthRate, col= Species)) + geom_point() + stat_smooth(method = "lm") + facet_grid(facets = ~Humidity)
# ###################

# lets compare our models some more

mod4 = aov(GrowthRate ~ Light*Humidity*Temperature, data = P.c)
summary(mod4)

library(modelr)
add_predictions() # we dont get to set our own values, this adds predictions based on what is in the data set

mod3_pred = add_predictions(P.c, model = mod3)

mod4_pred = add_predictions(P.c, model = mod4)

# how different is the prediction column from the growth rate column?

# we can see that by doing this for each model
mean((mod3_pred$pred - mod3_pred$GrowthRate)^2)
mean((mod4_pred$pred - mod4_pred$GrowthRate)^2)
# subrtratcting, squaring then taking the mean of those
# which value is smaller? mod4 which has temperature included which significantly improved our model

anova(mod3, mod4)

ggplot(P.c, aes(x=Temperature, y=GrowthRate, col = Humidity)) + geom_point() + stat_smooth(method = "lm")
# these lines are not parallel so there is an interaction, its looking good

# after all of this investigating and testing we would end up with this
ggplot(P.c, aes(x=Temperature, y=GrowthRate, col = Humidity)) + geom_point() + stat_smooth(method = "lm")
# then tidy or summary
mod4
# you would show this,  aov(formula = GrowthRate ~ Light * Humidity * Temperature, data = P.c)
summary(mod4)
# and the summary
# use tidy to make it a data frame
tidy(mod4)

sink("/Users/sydneyhoughton/Desktop/anova_table.txt") # will print it to somewhere
summary(mod4)
sink(NULL) # to close the circuit, end the command

# the temperature is kind of a waste on the x- axis with the two variables, lets see if we can make things
# easier to read

ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity, shape = factor(Temperature))) + 
  geom_point() + 
  stat_smooth(method = "lm")
# how do we make it cleaner
ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity, shape = factor(Temperature))) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE)
# alittle better

ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity)) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE, aes(col = factor(Temperature)))
# no that didnt help

?stat_smooth

ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity, linetype = factor(Temperature))) + 
  geom_point() + 
  stat_smooth(method = "lm", se = FALSE)
# looks good but there are only 3 types of light recorded, we can make it better

# use geom_jitter
ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity, linetype = factor(Temperature))) + 
  geom_jitter() + 
  stat_smooth(method = "lm", se = FALSE)
# too much

ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity, linetype = factor(Temperature))) + 
  geom_jitter(width = 0.01) + 
  stat_smooth(method = "lm", se = FALSE)
# not enough jitter

ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity, linetype = factor(Temperature))) + 
  geom_point(alpha = 1, size = 4) + 
  stat_smooth(method = "lm", se = FALSE)

# add labels and a theme
ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity, linetype = factor(Temperature))) + 
  geom_point(alpha = 1, size = 4) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(y="Growth Rate", title = "Model 4") +
  theme_bw()

#we might prefer the geom_jitter
ggplot(P.c, aes(x=Light, y=GrowthRate, col = Humidity, linetype = factor(Temperature))) + 
  geom_jitter(width = 1,alpha = .5, size = 4) + 
  stat_smooth(method = "lm", se = FALSE) +
  labs(y="Growth Rate", title = "P.cornucopiae") +
  ### changing legend labels ###
  scale_linetype_discrete(name = "Temperature") + scale_color_discrete(name = "Humidity \n level") +  
  theme_bw()

# install package gapminder
library(gapminder)
dat = gapminder
glimpse(dat)

# homework assignment work with the mushroom growth data or play with this 
# new built in data set from gapminder