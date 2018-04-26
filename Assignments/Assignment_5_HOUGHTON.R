
# analysis of mushroom growth data provided by Dr. Geoff Zahn
# What variables significantly impact the fungi's growth rate and what is the best way to grow each species. 

library(tidyr)
library(ggplot2)
library(dplyr)
library(devtools)
library(fitdistrplus)
library(gridExtra)

mush = read.csv(file = "/Users/sydneyhoughton/Desktop/Data_Course/data/mushroom_growth.csv")
# We have two species of fungi and growth rate was measured for varying settings. Variables include: Light, 
# Nitrogen, Humidity, and Temperature

# investigation into which variables have a significant correlation with growth rate. 
# This plot shows different response to nitrogen levels in each species. 
ggplot(mush, aes(x=Nitrogen,y=GrowthRate,color=Species)) + 
  geom_point() +
  stat_smooth()
# To test significance I created a model.
mod1 = aov(GrowthRate ~ Nitrogen*Species, data = mush)
summary(mod1)
# Nitrogen:Species not significant
# Since I can see that each species responds differently to Nitrogen, looking at them seperately might be best. 
# subset to break up data set
P.ost = subset(mush, Species == "P.ostreotus")
P.cor = subset(mush, Species == "P.cornucopiae")

# investigation code
mod2.Post = aov(GrowthRate ~ Nitrogen*Humidity, data = P.ost)
mod2.Pcor = aov(GrowthRate ~ Nitrogen*Humidity, data = P.cor)
mod3.Post = aov(GrowthRate ~ Nitrogen*Humidity*Temperature, data = P.ost)
mod3.Pcor = aov(GrowthRate ~ Nitrogen*Humidity*Temperature, data = P.cor)
summary(mod2.Post)
summary(mod2.Pcor)
summary(mod3.Post) # humidity and temperature have significant interaction
summary(mod3.Pcor) # again humidity and temperature have significant interaction

anova(mod2.Post, mod3.Post)  # No difference between the models (Pvalue of 0.09749)
anova(mod2.Pcor, mod3.Pcor)

# found some peak in growth at ~20 nitrogen level and high humidity
ggplot(P.ost, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth() + theme_bw()

# For P.cornucopiae a noticeable difference in growth with high humidity and ~20 nitrogen level
ggplot(P.cor, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth() + theme_bw()

# Here we can see that the temperature matters as well at high humidity. 
# At 25 degrees there is some increase in growth but at a slightly lower temperature this species does better and we see much more growth. 
ggplot(P.cor, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth() +
  facet_grid(~Temperature)
# which is confirmed with the anova table and Tukey test
mod4.Pcor = mod3.Pcor = aov(GrowthRate ~ Nitrogen*Humidity*factor(Temperature), data = P.cor)
summary(mod4.Pcor)
TukeyHSD(mod4.Pcor)



plotP.cor = ggplot(P.cor, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth() +
  scale_y_continuous(limits = c(0,700)) + # this will force both graphs to have same y-axis scale
  facet_grid(~Temperature) +
  ggtitle("P. cornucopiae")

plotP.ost = ggplot(P.ost, aes(x=Nitrogen,y=GrowthRate,col=Humidity)) +
  geom_point() +
  stat_smooth() +
  scale_y_continuous(limits = c(0,700)) + # this will force both graphs to have same y-axis scale
  facet_grid(~Temperature) +
  ggtitle("P. ostreotus")

# putting it into a visual print
grid.arrange(plotP.cor,plotP.ost)



