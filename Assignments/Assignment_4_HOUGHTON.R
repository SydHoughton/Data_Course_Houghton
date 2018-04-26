#### Assignment_4_for_credit
# data set mushroom_growth has varied levels of nitrogen (mg/g soil), light (hours per day),
# humidity (ambient vs high), and temperature (deg C).
# The response variable was growth rate (g/day).

mush = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/mushroom_growth.csv")

print(mush)
summary(mush)
plot(mush$GrowthRate, mush$Nitrogen)

for(i in names(mush)){
  plot(mush[,"GrowthRate"] ~ mush[,i], xlab = i, ylab = "GrowthRate", main = i)
}
# Nitrogen levels seem to be the most influential factor

hist(mush$GrowthRate)
plot(density(mush$GrowthRate))

library(fitdistrplus)
?fitdist
plot(fitdist(mush$GrowthRate, "norm")) #good
plot(fitdist(mush$GrowthRate, "gamma")) #better


##### what are the variables that predict growth rate?
# using a model to find trends, we can see in what ways growth rate (our response) depends
# on our other variables. A model will approximate this

##### Which variables are explanatory?
# Light, Nitrogen, Humidity, Temperature

##### Are the explanatory variables continuous, categorical, or both?
# They seem to fit more into the description of categorical because there are
# only a few variations that recycle, not a continuous string
# Light 0,10,20
# Nitrogen 0,5,10,20,25,30,35,40,45
# Humidity Low, High
# Temperature 20,25

#### Is the response variable continuous, a count, a proportion, a category?
# Growth Rate is a continuous variable

library(ggplot2)

ggplot(mush, mapping = aes(x=Humidity, y=GrowthRate, col = Species)) +
  geom_point() +
  geom_smooth(method = "aov", se = FALSE) +
  ggtitle("No interaction between Humidity and Species") +
  labs(subtitle = "...with respect to the effect on growth rate")

ggplot(mush, mapping = aes(x=Nitrogen, y=GrowthRate, col = Species)) +
  geom_point() +
  geom_smooth(method = "aov", se = FALSE) +
  ggtitle("Clear interaction between Nitrogen and Species") +
  labs(subtitle = "...with respect to the effect on growth rate")

ggplot(mush, mapping = aes(x=Light, y=GrowthRate, col = Species)) +
  geom_point() +
  geom_smooth(method = "aov", se = FALSE) +
  ggtitle("Clear interaction between Light and Species") +
  labs(subtitle = "...with respect to the effect on growth rate")

ggplot(mush, mapping = aes(x=Temperature, y=GrowthRate, col = Species)) +
  geom_point() +
  geom_smooth(method = "aov", se = FALSE) +
  ggtitle("Clear interaction between Temperature and Species") +
  labs(subtitle = "...with respect to the effect on growth rate")

mod1 = glm(GrowthRate ~ Light + Nitrogen + Temperature + Humidity, data = mush)

summary(mod1)
library(broom)
tidy(mod1)
# P-values for Light, Temperature, and Humidity show strong evidence to support against the NULL hypothesis
# I am unsure why for humidity it says "HumidityLow" when there are two levels of humidity ?????
# The P-value for Nitrogen is > 0.05 which indicates weak evidence against the null hypothesis, so we fail 
# to reject the null hypothesis.

par(mfrow = c(2, 2)) 
plot(mod1) 
par(mfrow = c(1, 1))

mush.mod = mush %>% add_predictions(model = mod1)
# Why do we calculate the mean squared difference between actual and predicted values of growth rate?
# so we do not end up with a negative.
mean((mush.mod$GrowthRate - mush.mod$pred)^2)
# 1216.57

mod2 = glm(GrowthRate ~ Light*Temperature, data = mush)
summary(mod2)
tidy(mod2)
mush.mod2 = mush %>% add_predictions(model = mod2)
mean((mush.mod2$GrowthRate - mush.mod2$pred)^2)
# 1496.251
# Higher mean value than mush.mod, not as good of a model


plot(mush$GrowthRate ~ mush$Nitrogen)
abline(mod, col = "Red")
abline(mod2, col = "Blue")
abline(lm(GrowthRate ~ Nitrogen, data = mush)) 
points(mush$GrowthRate ~ mush$Nitrogen, col = mush$Species) 

ggplot(data = mush) +
  geom_point(aes(x=Nitrogen, y=GrowthRate, col = factor(Species))) +
  geom_smooth(aes(x=Nitrogen, y=GrowthRate), method = "glm", col = "Black")

# I have been messing around with where I enter what variables into the ggplot function. Still not 
# getting a graph that is telling me much. 

mush$Nitrogen = as.factor(mush$Nitrogen)

mod3 = aov(GrowthRate ~ (Nitrogen)*Species, data = mush)

par(mfrow = c(2, 2)) 
plot(mod3)
par(mfrow = c(1, 1)) 

summary(mod3)
tidy(mod3)
plot(TukeyHSD(mod3))
# Interesting, what does this mean?

# I did not get through a full analysis of this data set. I am still learning some of the meanings
# of the basic statistical terminology along with understanding how to enter and interperat data
# through ggplot and ANOVA. A work in progress, please provide feedback :)










