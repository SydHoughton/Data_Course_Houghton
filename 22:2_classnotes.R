###### learning to work with ugly data #####
# nice rectangle, every column is a variable and every row is an observation
library(tidyr)
library(ggplot2)
library(dplyr)
library(broom)
library(gapminder)

df = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/BioLog_Plate_Data.csv")
# diffrent substrates, reading absorbance of flourecense in each over different time increments
# if we wanted to make a graph of this data set how would we do that?

levels(df$Sample.ID)
unique(df$Rep)
unique(df$Dilution)
levels(df$Substrate)
# water is our control

sub1 = subset(df, Substrate == "D-Cellobiose")
# need to make a column called Time
sub1 = subset(df, Substrate == "D-Cellobiose")
absorbance = c(df$Hr_24,df$Hr_48,df$Hr_144)
# tack onto data frame 3 times
df[,-c(6,7,8)] # remove columns no longer needed

# how to add the Time (3) times
?gather()
sub1_long = gather(sub1, key = "Time", value = "Abs", c("Hr_24", "Hr_48", "Hr_144"))
# going from a wide format to stacking some columns. 

ggplot(sub1_long, aes(x=Time, y=Abs, col = Sample.ID)) + 
  geom_point() +
  stat_smooth()
# there is no line. time is a catergory and also not listed in order
# we need to change it to numbers
?plyr::mapvalues()
library(plyr)
?mapvalues()

# manual way to do it
h24 = which(sub1_long$Time == "Hr_24")
h48 = which(sub1_long$Time == "Hr_48")
h144 = which(sub1_long$Time == "Hr_144")

sub1_long$Time[h24] = 24
sub1_long$Time[h48] = 48
sub1_long$Time[h144] = 144

mapvalues(sub1_long$Time, from = c("Hr_24", "Hr_48", "Hr_144"),
          to = c(24, 48, 144))
# take the original vector and spit out new values to match each value that is there. 
# these are still characters though
sub1_long$Time = as.numeric(mapvalues(sub1_long$Time, from = c("Hr_24", "Hr_48", "Hr_144"),
                     to = c(24, 48, 144)))

ggplot(sub1_long, aes(x=Time, y=Abs, col = Sample.ID)) + 
  geom_point() +
  stat_smooth()
# much better



##### do long form for the entire data set, with time change #####

df_long = gather(df, key = "Time", value = "Abs", c("Hr_24", "Hr_48", "Hr_144"))

df_long$Time = as.numeric(mapvalues(df_long$Time, from = c("Hr_24", "Hr_48", "Hr_144"),
          to = c(24,48,144)))
ggplot(df_long, aes(x=Time, y=Abs, col = Sample.ID)) +
  geom_point() +
  stat_smooth() +

for (i in levels(df_long$Substrate)){
    sub = subset(df_long, Substrate == i)
    plot = ggplot(sub, aes(x=Time, y=Abs, col = Sample.ID)) +
    geom_point() +
    stat_smooth() + ggtitle(i)
    print(plot)
}
levels(df$Substrate)
# not working
# we go this to work finally by wrapping it in the print function

##### practice for loops and lapply() #####
# gather is todays take home message
# mapvalues
# read Tidy Data, and then print cheat sheets
###### use stack overflow #####

# we do not want to change or edit the raw data. This is why we make a long form
# and save it to the working computer
df = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/data/BioLog_Plate_Data.csv")
df.long = gather(df, Time, Abs, c("Hr_24", "Hr_48", "Hr_144"))

times = plyr::mapvalues(df.long$Time), from = c("Hr_24", "Hr_48", "Hr_144"), 
to = c(24,48,144)
df.long$Time = as.numeric(times)

write.csv(df.long, "")











