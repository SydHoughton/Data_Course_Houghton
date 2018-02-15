mush = read.csv(file = "/Users/sydneyhoughton/Desktop/Data_Course/data/mushroom_growth.csv")
library(tidyr)
library(ggplot2)
library(dplyr)
library(devtools)
library(fitdistrplus)
sample_n(mush, 10)
levels(mush$GrowthRate)

fitdist(mush$GrowthRate, "norm")
plot(fitdist(mush$GrowthRate, "gamma"))
mush.mod = glm(GrowthRate ~ Light + Nitrogen + Humidity + Temperature , data = mush)
summary(mush.mod)
tidy(mush.mod)

boxplot(mush$GrowthRate~mush$Light)
boxplot(mush$GrowthRate~mush$Nitrogen)
boxplot(mush$GrowthRate~mush$Humidity)
boxplot(mush$GrowthRate~mush$Temperature)
boxplot(mush$GrowthRate~mush$Nitrogen + mush$Temperature)

ANOVA1 = aov(GrowthRate~Nitrogen, data = mush)
summary(ANOVA1)
ANOVA2 = aov(GrowthRate~Light:Species, data = mush)
summary(ANOVA2)
ANOVA3 = aov(GrowthRate~Humidity, data = mush)
summary(ANOVA3)
### relationship between growth rate and Light shows the lowest P-value

ggplot(mush, mapping = aes(x=Humidity, y=GrowthRate, fill = Species)) +
  geom_boxplot()

TukeyHSD(ANOVA2)
### error, need to change Light from numeric to factor
mush$Light <- as.factor(mush$Light)
TukeyHSD(ANOVA2)

ggplot(mush, aes(x=GrowthRate, y=Light)) + geom_point()

aov(GrowthRate ~ Light*Nitrogen*Humidity*Temperature*Species, data = mush) %>% summary()

aov(GrowthRate ~ Light*Humidity*Temperature*Species, data = mush) %>% summary()

mush.mod1 = aov(GrowthRate ~ Light*Humidity*Temperature*Species, data = mush) %>% summary()

sub1 = subset(mush, Light %in% c("10", "20"))
ggplot(sub1, aes(x=Light, y=Temperature, col=Species)) + geom_point()

## I am struggling to find a way to work with this data to show a nice graph of relationships
## all my graphs end up looking like this last ggplot above. Dots to each side.

## The Tukey test showed some clear relationships but I am not understanding how to work with it in ggplot


