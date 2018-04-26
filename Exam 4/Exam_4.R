### Re-work of Exam 1 ###

setwd("/Users/sydneyhoughton/Desktop/Data_Course/Exams/Exam_1/")
list.files()

df = read.csv("DNA_Conc_by_Extraction_Date.csv.gz")

## Task I
hist(df$DNA_Concentration_Katy, xlab = "Extraction Number", ylab = "Concentration", main = "DNA Concentrations for Katy", col = 'light gray')

hist(df$DNA_Concentration_Ben,xlab = "Extraction Number", ylab = "Concentration", main = "DNA Concentrations for Ben", col = 'light gray')

## Task II
class(df$Year_Collected)
class(df$DNA_Concentration_Katy)
df$Year_Collected <- factor(df$Year_Collected)

plot(df$Year_Collected, df$DNA_Concentration_Katy, xlab = "YEAR", ylab = "DNA Concentration", main = "Katy's Extratctions")

plot(df$Year_Collected, df$DNA_Concentration_Ben, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extratctions")

## Task III
jpeg(filename = "/Users/sydneyhoughton/Desktop/Data_Course_Houghton/HOUGHTON_Plot1.jpeg")
plot(df$Year_Collected, df$DNA_Concentration_Katy, xlab = "YEAR", ylab = "DNA Concentration", main = "Katy's Extratctions")
dev.off()

jpeg(filename = "/Users/sydneyhoughton/Desktop/Data_Course_Houghton/HOUGHTON_Plot2.jpeg")
plot(df$Year_Collected, df$DNA_Concentration_Ben, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extratctions")
dev.off()

## Task IV
sub = subset(df[,c(2,5,6)])  
summary(sub)

library(tidyr)
dat2 = gather(sub, key = "Collector", value = "DNA_Concentration", 
              c("DNA_Concentration_Katy","DNA_Concentration_Ben"))

which(rowSums(sub[,2:3]) == min(rowSums(sub[,2:3])))
sub[196,]

## Task V
# subset data to only include Ben's values
# one column contains years, one column contains averages of values in each each. 12 rows total in the data frame
# which extraction year has the highest average? and what is the concentration?
ben_df = as.data.frame(subset(sub[,c(1,3)]))
Ben = aggregate(DNA_Concentration_Ben ~ Year_Collected, data = ben_df, mean)
which.max(Ben$DNA_Concentration_Ben)
print(Ben[8,])

write.csv(Ben,"/Users/sydneyhoughton/Desktop/Data_Course_Houghton/Ben_average_by_year.csv")


