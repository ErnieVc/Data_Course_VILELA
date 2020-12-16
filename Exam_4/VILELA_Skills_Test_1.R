library(tidyverse)
library(ggplot2)

df <- read.csv("./DNA_Conc_by_Extraction_Date.csv")
summary(df)


#Task 1
#Once you get the csv file loaded into an R object as a data frame, feel free to do some exploratory 
#visualizations or summaries to get a feel for the data if you like.
#Your first task, though, is to create separate histograms of the DNA concentrations for Katy and Ben. 
#Make sure to add nice labels to these (x-axis and main title).

#   Creating a histogram of DNA concentrations for Katy
hist(df$DNA_Concentration_Katy, xlab = "DNA concentration", 
     main = "KATY Histogram")
#   Creating a histogram of DNA concentrations for Ben
hist(df$DNA_Concentration_Ben, xlab = "DNA concentration", 
     main = "Ben Histogram")

#Task 2 
#Your second task is to look at DNA concentrations from the different extraction years. 
#One way to do this is a separate figure for each student is demonstrated in those 
#two files:	ZAHN_Plot1.jpeg and ZAHN_Plot2.jpeg 
#Open those files in some image viewing program and take a look. I'd like you to re-create these exactly,
#including the labels.
#This is tricky, so I'll give a hint: the plot() function behaves differently depending on the classes of 
#vectors that are given to it.
#Task 3
#Once you have your code for creating the figures correctly, you need to save those two images in 
#YOUR Exam_1 directory. Name them similarly to how I named mine, but with your LASTNAME
#Make sure your code is saving the files. Don't do it manually with the mouse!

#   Recreating the plot for Katy looking at the DNA concentration peer year by using plot()
#   Saving the plot using jpeg()
jpeg("./VILELA_Plot1.jpg")
plot (x=(as.factor(df$Year_Collected)), 
      y =  df$DNA_Concentration_Katy, 
      xlab = "YEAR", 
      ylab = "DNA Concentration", 
      main = "Katy's Extraction") 
dev.off()

#   Recreating the plot for Ben looking at the DNA concentration peer year by using plot()
#   Saving the plot using jpeg()
jpeg("./VILELA_Plot2.jpg")
plot (x=(as.factor(df$Year_Collected)), 
      y =  df$DNA_Concentration_Ben, 
      xlab = "YEAR", 
      ylab = "DNA Concentration", 
      main = "Ben's Extraction") 
dev.off()

#Task 4
#Take a look at Ben's concentrations vs Katy's concentrations. You can do this however you like... 
#with a plot or with summary stats or both.
#It looks like Ben had consistently higher DNA yields than Katy did...but surely it wasn't uniformly better, 
#right? With some samples, he only had a marginal improvement over Katy.
#With other samples, he had a relatively massive improvement over her.
#Your task here is to write some code that tells us: in which extraction YEAR, was Ben's performance the 
#lowest RELATIVE TO Katy's performance?

#taking a look at Ben vs Katy's concentration
sumPlot <- plot (x = df$DNA_Concentration_Ben, 
                 y = df$DNA_Concentration_Katy, 
                 xlab = "Ben's Concentration", 
                 ylab = "Katy's Concentration")
summary(sumPlot)

#   We calculate the minimum of a vector by using the min() function. 
vectorMin <- df$DNA_Concentration_Ben / df$DNA_Concentration_Katy
vectorMinDiff <- min(vectorMin)

#   We find wich indices are true
indexDiff <- which (vectorMinDiff == vectorMin)

#   We find the year in which the lowest difference occurred and then print it
LowestDiffYear <- df$Year_Collected[indexDiff]
print(LowestDiffYear)

#Task 5
#Do another subset of the data for me. Subset the data frame so it's just the "Downstairs" lab.
#Now, make a scatterplot of the downstairs lab data such that "Date_Collected" is on the x-axis 
#and "DNA_Concentration_Ben" is on the y-axis. Save this scatterplot as "Ben_DNA_over_time.jpg" 
#in your Exam_1 directory. See the file "Downstairs.jpg" for an example of how yours should look. 
#If it looks different, you might need to do some class conversions so the plot() function treats 
#things correctly. HintHintHint: POSIXct

#   We filter for the instances in which the content of lab is "Downstairs
#   Then we plot the DNA concentration of Ben's data by the date it was collected using plot()
#   We then save the image by using jpeg()
jpeg("Ben_DNA_over_time.jpg")
downstairsSubset <- filter(df,Lab == "Downstairs")
plot(x= (as.POSIXct(downstairsSubset$Date_Collected)),
     y= downstairsSubset$DNA_Concentration_Ben, 
     xlab ="Date Collected", 
     ylab = "DNA Concentration Ben" )
dev.off()

#Task 6
#For this final (BONUS) problem, let's just look at Ben's DNA concentration values. I think Katy 
#messed up her PCRs, and at any rate, we can't use them for sequencing.
#Besides, our original purpose for this experiment was to see if DNA extractions sitting in a freezer 
#degraded over time.
#To that end, I want you to make a new data frame (just using Ben's values) that has one column containing
#the years that DNA extractions were made, 
#and another column that contains the AVERAGE of the values within that year.  Just to be clear, this data 
#frame should have only 12 rows (one for each year)! You will need to find a way to take the average of 
#Ben's DNA values in each separate year. A for-loop, or repeated subsetting, or some other way...
#Once you have this new data frame of averages by year, write some code that shows which extraction 
#year has the highest average DNA concentration (and what that concentration is) and then save 
#the 12-row dataframe as a new csv file called "Ben_Average_Conc.csv"

#Ben yearly summary

#Assigning the value of 1 to x
x <-1

#creating an empty vector
avg.ben <-c()

#creating a for look, in which the mean is calculated and then the average is calculated. 
#this produces the mean of each of the years.
for(i in unique(df$Year_Collected)){
  SUB <- mean(df[df$Year_Collected == i, "DNA_Concentration_Ben"])
  avg.ben[x]<- SUB
  x<-x+1
}

#creating a data frame with the Year column, and the Ben_Average column. 
df2 <- data.frame(Year=unique(df$Year_Collected),
           Ben_Average=avg.ben)

#finding the maximum for Ben's averaged values (by year)
df2[which (df2$Ben_Average == max(df2$Ben_Average)),]

#writing the dataframe into a .csv document. 
write.csv(df2, "../Exam_4/Ben_Average_Conc.csv")











