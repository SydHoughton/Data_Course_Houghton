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







# +++++++++++++  Zahn Comments and Edits

# consider:

ggplot(mush, aes(x=Nitrogen,y=GrowthRate,color=Species)) + 
  geom_point() +
  stat_smooth()
# different response to nitrogen additions in different species? at least a possibility
model1 = aov(GrowthRate ~ Nitrogen*Species, data = mush)
summary(model1) # Hm....nope. That interaction term is not significant!

# But if I was trying to find out what the best way to grow each species is....
# I could subset and look at each species separately, since they appear to have different growth rates across the board...
P.o = subset(mush, Species == "P.ostreotus")
P.c = subset(mush, Species == "P.cornucopiae")

# Try two different models on each subset of data (by species)
model2.Po = aov(GrowthRate ~ Nitrogen*Humidity, data = P.o)
model2.Pc = aov(GrowthRate ~ Nitrogen*Humidity, data = P.c)
model3.Po = aov(GrowthRate ~ Nitrogen*Humidity*Temperature, data = P.o)
model3.Pc = aov(GrowthRate ~ Nitrogen*Humidity*Temperature, data = P.c)

anova(model2.Po, model3.Po)  # No difference between the models (Pvalue of 0.09749)
anova(model2.Pc, model3.Pc)  # yes difference between the models (Pvalue of 0.0002811)
# so.... for P. ostereotus, the two models are equally valid, for P. cornucopiae, there is a difference...not sure which is better.
# I'll deal with that later. For now I want to investigate a bit more 

ggplot(P.o, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth()
      # I see a peak productivity in there... Nitrogen of ~20 and high humidity

# let's try same plot with P. cornucopiae
ggplot(P.c, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth()
      # Hey now...Looks like the humidity difference is a big deal for this species!

# check out the following plot that's separated by temperature as well
ggplot(P.c, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth() +
  facet_grid(~Temperature)
      # Here, it looks like for this species, the effect of humidity is a little bit dependent on the temperature
      # In other words, Humidity doesn't matter at 25 degrees, but has an influence at 20 degrees
      # let's look at our anova table again to confirm:

summary(model3.Pc) # BINGO! See the Humidity:Temperature interaction? Solidly in the realm of "significant"

# We see it visually. It makes sense. Tukey's Test will allow us to confirm which one has greater effect on response variable
TukeyHSD(model3.Pc)

# Except we get this shitty error:
# 2: In replications(paste("~", xx), data = mf) :
#   non-factors ignored: Temperature

# Tukey's test can only deal with categorical data since it's basically running a set of T-Tests on every combination we told it to model
# I'm gonna change Temperature to a factor since there are only two levels of it!
model4.Pc = model3.Pc = aov(GrowthRate ~ Nitrogen*Humidity*factor(Temperature), data = P.c)
TukeyHSD(model4.Pc)

# Tukey shows us comparisons between groups: notice the following line:
# Low:20-High:20  -218.84933 -286.29288 -151.405769 0.0000000
# That just means that the difference between Low and High Humidity at 20 degrees is significant

# Low:25-High:25   -70.28589 -137.72945   -2.842335 0.0376880
# This means that the difference between humidity levels at 25 degrees is NOT!!! significant

# Basically, mathematically confirming what we see in the figure

# If I wanted to summarize the bulk of these findings in a figure, it might look like this:

plotP.c = ggplot(P.c, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth() +
  scale_y_continuous(limits = c(0,700)) + # this will force both graphs to have same y-axis scale
  facet_grid(~Temperature) +
  ggtitle("P. cornucopiae")

plotP.o = ggplot(P.o, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth() +
  scale_y_continuous(limits = c(0,700)) + # this will force both graphs to have same y-axis scale
  facet_grid(~Temperature) +
  ggtitle("P. ostreotus")

library(gridExtra)
grid.arrange(plotP.c,plotP.o)
