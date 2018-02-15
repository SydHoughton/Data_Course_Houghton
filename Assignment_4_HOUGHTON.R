## Assignment_4_for_credit
## data set mushroom_growth has varied levels of nitrogen (mg/g soil), light (hours per day),
## humidity (ambient vs high), and temperature (deg C).
## The response variable was growth rate (g/day).

mush = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/mushroom_growth.csv")

print(mush)
summary(mush)
plot(mush$GrowthRate, mush$Nitrogen)

for(i in names(mush)){
  plot(mush[,"GrowthRate"] ~ mush[,i], xlab = i, ylab = "GrowthRate", main = i)
}

hist(mush$GrowthRate)
plot(density(mush$GrowthRate))

library(fitdistrplus)
?fitdist
plot(fitdist(mush$GrowthRate, "norm"))
plot(fitdist(mush$GrowthRate, "gamma"))


##### what are the variables that predict growth rate?
## using a model to find trends, we can see in what ways growth rate (our response) depends
## on our other variables. A model will approximates this

##### Which variables are explanatory?
# Light, Nitrogen, Humidity, Temperature

##### Are the explanatory variables continuous, categorical, or both?
## They seem to fit more into the description of categorical because there are
## only a few variations
## Light 0,10,20
## Nitrogen 0,5,10,20,25,30,35,40,45
## Humidity Low, High
## Temperature 20,25

#### Is the response variable continuous, a count, a proportion, a category?


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

mod1 = glm(GrowthRate ~ Light + Nitrogen + Temperature, data = mush)

summary(mod1)










mush$Species = as.numeric(mush$Species)
mush$Light = as.numeric(mush$Light)
mush$Nitrogen = as.numeric(mush$Nitrogen)
mush$Humidity = as.numeric(mush$Humidity)
mush$Temperature = as.numeric(mush$Temperature)







plot(fitdist(mush$Temperature, "gamma"))

plot(fitdist(mush$Humidity, "gamma"))