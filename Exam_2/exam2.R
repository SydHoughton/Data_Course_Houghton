rm(list = ls())

# Load the libraries that you use here:
library(tidyr)
library(ggplot2)
library(fitdistrplus)
library(stats)
library(modelr)
library(dpylr)
library(base)
library(tibble)
############# Part 1 - Preparing wide data ################## ---------------- (30 points possible)

# read in salaries.csv
dat = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/exam2/salaries.csv")
# This is faculty salary information from 1995 - Split up by university, state, faculty rank, and university tier

# convert to usable format so we can look at salaries as a dependent variable (10 points)
?gather()
dat_long = gather(dat, "Rank", "Salary", c("AssistProf", "AssocProf", "FullProf"))

# create boxplot of salary by University Tier, colored by Faculty Rank (10 points)
# x-axis = Tier
# y-axis = Salary
# Boxplot fill color = Rank
# Title = "Faculty Salaries - 1995"
?geom_boxplot()
p <- ggplot(dat_long, aes(Tier, Salary))
p + geom_boxplot(fill = "Light blue") + ggtitle("Faculty Salaries - 1995")


# export this boxplot to a file in your personal repository named "LASTNAME_exam2_plot1.jpeg" (10 points)
jpeg("/Users/sydneyhoughton/Desktop/Data_Course_Houghton/HOUGHTON_exam2_plot1.jpeg")
p + geom_boxplot(fill = "Light blue") + ggtitle("Faculty Salaries - 1995")
dev.off()



################# PART 2 ################### ------------ (70 points possible)

# read in atmosphere.csv
# this data frame has microbial diversity values over time found in atmospheric observation station air filters
# sampling date and two environmental variables [CO2] and [Aerosols] are reported for each measurement
# "Diversity" is the dependent variable
dat2 = read.csv("/Users/sydneyhoughton/Desktop/Data_Course/exam2/atmosphere.csv")


# First, check whether your response variable is normally distributed (5 points)
plot(density(dat2$Diversity))
plot(fitdist(dat2$Diversity, "norm"))
### looks normally distributed ###

# Next, convert "Year" to a factor...just because (5 points)
dat2$Year <- (as.factor(dat2$Year))


# Create a simple ANOVA model with "Year" as the only explanatory variable (5 points)
?aov
mod1 = aov(Diversity ~ Year, data = dat2)
summary(mod1)


# Now, create an ANOVA model that incorporates "Year", "Aerosol_Density", and their interaction (5 points)
mod2 = aov(Diversity ~ Year*Aerosol_Density, data = dat2)
summary(mod2)

# Compare the two models mean-squared difference method to see which is better at making predictions 
# (20 points)
?add_predictions
mod1_pred = add_predictions(dat2, mod1, var = "pred")
mod2_pred = add_predictions(dat2, mod2, var = "pred")
sqrt(mean((mod1_pred$Diversity - mod1_pred$pred)^2))
sqrt(mean((mod2_pred$Diversity - mod2_pred$pred)^2))
### model 2 does a better job at making predictions because it includes the Aerosol Density ###
summary(mod2_pred)

# Export the summary ANOVA table of the better model to a text file in your repository named:
# "LASTNAME_exam2_table1.txt" (10 points)
write.table(mod2_pred, file = "/Users/sydneyhoughton/Desktop/Data_Course_Houghton/HOUGHTON_exam2_table1.txt")
dev.off()

# use this model to predict what diversity should be for the following hypothetical conditions:
# note: only include the conditions that are part of your chosen model! (10 points)

# Year = 2007
# Quarter = "Q4"
# Month = August
# Mday = 10
# BarcodeSequence = "CTCTCTATCAGTGAGT"
# Aerosol_Density = 1000,
# CO2_Concentration = 384
sub = subset(mod2_pred[,3:11])
add_row(sub, row, before = 187, after = 186)

row = c(2007,"Q4", "August", 10, "CTCTCTATCAGTGAGT", 1000, 384)
column = c("Year", "Quater", "Month", "Mday", "BarcodeSequence", "Aerosol_Density", "CO2_Concentrations")
hypo_data = data.frame(,column)
### this was not working for me  ###
### RAN OUT OUT OF TIME ###


# Now, make a pretty plot to the following specifications:
# x-axis = Day
# y-axis = Aerosol_Density
# point transparency based on values of "Diversity"
# Title: "Decadal Aerosol Density"
# Subtitle: "More aerosols contribute to greater microbial diversity in the atmosphere"




# Save this plot in your repository as "LASTNAME_exam2_plot2.jpeg" (10 points)


#### When you are all finished, push the files, including this R script, onto your GitHub repo
#### I will look at your script and look for the three properly named files that you generated


