library(tidyverse)
library(ggplot2)

df <- read.csv("./DNA_Conc_by_Extraction_Date.csv")
summary(df)


#Question 1
hist(df$DNA_Concentration_Katy, xlab = "DNA concentration", main = "KATY Histogram")
hist(df$DNA_Concentration_Ben, xlab = "DNA concentration", main = "Ben Histogram")

#Question 2 and Question 3
jpeg("./VILELA_Plot1.jpg")
plot (x=(as.factor(df$Year_Collected)), y =  df$DNA_Concentration_Katy, xlab = "YEAR", ylab = "DNA Concentration", main = "Katy's Extraction") 
dev.off()


jpeg("./VILELA_Plot2.jpg")
plot (x=(as.factor(df$Year_Collected)), y =  df$DNA_Concentration_Ben, xlab = "YEAR", ylab = "DNA Concentration", main = "Ben's Extraction") 
dev.off()

#Question 4
sumPlot <- plot (x = df$DNA_Concentration_Ben, y = df$DNA_Concentration_Katy, xlab = "Ben's Concentration", ylab = "Katy's Concentration")
summary(sumPlot)

vectorMin <- df$DNA_Concentration_Ben / df$DNA_Concentration_Katy
vectorMinDiff <- min(vectorMin)
indexDiff <- which (vectorMinDiff == vectorMin)
LowestDiffYear <- df$Year_Collected[indexDiff]
print(LowestDiffYear)

#Question 5
jpeg("Ben_DNA_over_time.jpg")
downstairsSubset <- filter(df,Lab == "Downstairs")
plot(x= (as.POSIXct(downstairsSubset$Date_Collected)),y= downstairsSubset$DNA_Concentration_Ben, xlab ="Date Collected", ylab = "DNA Concentration Ben" )
dev.off()









