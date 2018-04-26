df = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/thatch_ant.csv")

for (i in names(df)) {
  plot(df[,"Mass"] ~ df[,i], xlab = i, ylab = "Mass", main = i)
}

# colony ~ mass, shows an equal distribution through colonies
# distance ~ mass, measure of distance from the ant hill. This is showing a pretty equal distribution as well
# mass ~ mass, this looks like what we would expect, not useful info
# mass ~ headwidth, has a fairly equal distribution and slope with some outliers. 
# mass ~ headwidth..mm., gives a better look at the relationship
# mass ~ size class, has outliers
ggplot(df, aes(x=Headwidth..mm., y=Mass, color = Colony)) + geom_point() + stat_smooth()

mod1 = aov(Mass ~ Headwidth..mm.*Species, data = df)
summary(mod1)

ggplot(df, aes(x=Headwidth..mm., y=Mass, color = Size.class)) + geom_point() + stat_smooth()

mod2 = aov(Mass ~ Headwidth..mm.*Colony, data = df)
summary(mod2)
# no significant interaction

ggplot(df, aes(x=Colony, y=Headwidth..mm., color = Size.class)) + geom_point() + stat_smooth()
##### try a new data set #####

p_dat = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/wingspan_vs_mass.csv")
str(p_dat)

p_dat1 = subset(p_dat, select = -c(X))

for (i in names(p_dat1)) {
  plot(p_dat1[,"velocity"] ~ p_dat1[,i], xlab = i, ylab = "Velocity", main = i)
}
