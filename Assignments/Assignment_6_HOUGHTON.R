library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)
library(gapminder)

df = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/BioLog_Plate_Data.csv")
levels(df$Substrate)

amino = subset(df, Substrate %in% c("L-Arginine","L-Asparganine", "Glycyl-L-Glutamic Acid",
                                    "L-Phenylalanine", "L-Serine", "L-Threonine" ))

ggplot(amino, aes(x=Substrate, y=Hr_48, col = Sample.ID)) + 
  geom_point() + stat_smooth()

absorbance = c(amino$Hr_24, amino$Hr_48, amino$Hr_144)

amino[,-c(6,7,8)]
?gather()
amino_long = gather(amino, "Time", "Absorbance", c("Hr_24", "Hr_48", "Hr_144")) 

ggplot(amino_long, aes(x=Time, y=Absorbance, col = Substrate)) + 
  geom_point() +
  stat_smooth()

?mapvalues 
library(plyr)

amino_long$Time = as.numeric(mapvalues(amino_long$Time, from = c("Hr_24", "Hr_48", "Hr_144"), 
                                       to = c(24,48,144)))

ggplot(amino_long, aes(x=Time, y=Absorbance, col = Substrate)) + 
  geom_point() +
  stat_smooth()

levels(amino_long$Sample.ID)
sub1 = subset(amino_long, Sample.ID %in% c("Waste_Water", "Clear_Creek"))
ggplot(sub1, aes(x=Time, y=Absorbance, col = Substrate)) + 
  geom_point() +
  stat_smooth()

library(fitdistrplus)
plot(fitdist(amino_long$Absorbance, "norm"))
plot(fitdist(amino_long$Absorbance, "logis"))

plot(fitdist(sub1$Absorbance, "norm"))
plot(fitdist(sub1$Absorbance, "logis"))

ggplot(sub1, mapping = aes(x=Time, y=Absorbance, col = Substrate, shape = Sample.ID)) +
  geom_point() +
  stat_smooth(method = "aov") +
  ggtitle("Amino Acids")



?ggplot()

sub2 = subset(amino_long, Sample.ID %in% c("Soil_1", "Soil_2"))
ggplot(sub2, mapping = aes(x=Time, y=Absorbance, col = Substrate, shape = Sample.ID)) +
  geom_point() +
  stat_smooth(method = "aov") +
  ggtitle("Amino Acids")


levels(amino_long$Substrate)
amino1 = subset(df, Substrate %in% c("L-Arginine","L-Asparganine", "Glycyl-L-Glutamic Acid",
                                    "L-Phenylalanine", "L-Serine", "L-Threonine", "Water" ))
amino_long1 = gather(amino1, "Time", "Absorbance", c("Hr_24", "Hr_48", "Hr_144")) 
amino1[,-c(6,7,8)]
amino_long1$Time = as.numeric(mapvalues(amino_long1$Time, from = c("Hr_24", "Hr_48", "Hr_144"), 
                                       to = c(24,48,144)))
ggplot(amino_long1, mapping = aes(x=Time, y=Absorbance, col = Substrate, shape = Sample.ID)) +
  geom_point() +
  stat_smooth(method = "aov") +
  ggtitle("Amino Acids")
levels(amino_long1$Sample.ID)
sub3 = subset(amino_long1, Sample.ID %in% c("Soil_1", "Soil_2"))
ggplot(sub3, mapping = aes(x=Time, y=Absorbance, col = Substrate, shape = Sample.ID)) +
  geom_point() +
  stat_smooth(method = "aov") +
  ggtitle("Amino Acids")
