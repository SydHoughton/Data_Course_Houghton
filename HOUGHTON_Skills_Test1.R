setwd("/Users/sydneyhoughton/Desktop/Data_Course/Exam_1/")
list.files()

df = read.csv("DNA_Conc_by_Extraction_Date.csv.gz")
## Task I
plot(df$Extract.Number, df$DNA_Concentration_Katy, xlab = "Extraction Number", ylab = "Concentration", main = "DNA Concentrations for Katy", col = 'purple')

plot(df$Extract.Number, df$DNA_Concentration_Ben,xlab = "Extraction Number", ylab = "Concentration", main = "DNA Concentrations for Ben", col = 'forestgreen')

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
?summary
summary(df$DNA_Concentration_Ben)
summary(df$DNA_Concentration_Katy)

which()

## Task V

ben = data.frame(df$DNA_Concentration_Ben, df$Year_Collected)
colnames(ben)
colnames(ben) = DNA_Concentration






