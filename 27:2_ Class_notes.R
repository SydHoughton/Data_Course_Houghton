library(tidyr)

x = rnorm(10)
y = rnorm(10)
z = rnorm(10)
obs = 1:10

df = data.frame(obs = obs, x=x, y=y, z=z)
# say these are stock price changes, x y and z represent a different stock (company)

df.l = gather(df, key = "stock", value = "pricechange", c("x", "y", "z"))
# given data frame, give it column name and then give it the value
?spread
spread(df.l, stock, pricechange)

df.l = gather(df, key = "stock", value = "pricechange", c("x", "y", "z"))

aov1 = aov(pricechange ~ stock*obs, data = df.l)
summary(aov1)
# there is not any significant interaction, because we created these from the same random distribution
# change to mean default to get a bettwe distribution
x = rnorm(10, mean = 10)
y = rnorm(10, mean = 100)
z = rnorm(10)
obs = 1:10

df2 = data.frame(obs = obs, x=x, y=y, z=z)

df.l_2 = gather(df2, key = "stock", value = "pricechange", c("x", "y", "z"))

aov1 = aov(pricechange ~ stock*obs, data = df.l_2)
summary(aov1)

TukeyHSD(aov1)
# $stock
# diff        lwr        upr p adj
# y-x   90.522070   89.65879  91.385352     0
# z-x   -9.890214  -10.75350  -9.026932     0
# z-y -100.412285 -101.27557 -99.549002     0
## we see here the differences in y and x, then z and x, then z and y

# observations are not factors so we did not get everything run through te Tukey test
x
y
z



library(dplyr)
dplyr::
# the %>% comes with dplyr

df %>% sum()
# give me all of df and then do the function sum on all of that. 

c("a", "b", "c") %>% sum()
# does not work because they are characters

df %>% mutate(total = x+y+z, min = apply(df[,2:4], 1,min))
apply(df[,2:4], 1,min)
df$min = apply(df[,2:4], 1,min)

# mutate makes a new column
df %>% mutate(total = x+y+z, logx = log10(x))

?filter

df %>% filter(x>10)
df %>% filter(x>9.5&x<10.5)
df %>% filter(x==9.58895|)

# select picks columns
# filter picks rows

?group_by
df$group = c(rep("A",5, "B",5))
df


df %<% group_by(group) %>% summarise(meanx = mean(x))


df %<% group_by(stock) %>% summarise(mean = mean(pricechange), sum = sum(pricechange), min)




###### comparing models ######
dat = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/iris.csv")

plot(dat)

mod1 = aov(Petal.Length ~ Petal.Width*Species, data = dat)

mod2 = aov(Petal.Length ~ Petal.Width+Species, data = dat)

summary(mod1)
summary(mod2)

# are the models different at all

anova(mod1,mod2)
# they are different, predict data differently
# thats all we know, so which is better
library(modelr)
# to add predictions
?add_predictions
dat2 = add_predictions(dat, mod1, var= "mod1")

dat2 = add_predictions(dat, mod2, var = "mod2")

library(ggplot2)
ggplot(dat2) + geom_point(aes(x=Petal.Width, y=Petal.Length, col = Species)) +
  geom_smooth(aes(x=Pedtal.Width, y=mod1, col = Species), method = "lm")) +
  geom_smooth(aes(x=Pedtal.Width, y=mod2, col = Species), method = "lm", linetype = 5))

# to find which is better
# measure predicted value to the actual value
sqrt(mean((dat$mod1 - dat$Petal.Length)^2))
sqrt(mean((dat$mod1 - dat$Petal.Length)^2))
# gives you the average distance of points from the lm lines

###### for Exam 2 #######
# wide to long data set
# model building and comparison 
# make plots
# anova table
