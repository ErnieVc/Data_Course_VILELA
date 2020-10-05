library(tidyverse)
library(ggplot2)

#1) loads the mtcars data set
data("mtcars")
str(mtcars)

#2) subsets the mtcars dataframe to include only automatic transmissions
#3) saves this new subset as a new file called “automatic_mtcars.csv” in your Assignment_6 directory
auto_transmissions <- mtcars$am == "1"
auto_transmission_dataframe <- mtcars[auto_transmissions,]
write.csv(auto_transmission_dataframe,"./automatic_mtcars.csv")

#4) plots the effect of horsepower on miles-per-gallon using ggplot2 
#(update plot to have meaningful labels and title)
#NOTE: effects of Miles per gallon in ALL CARS
names(mtcars)
ggplot(mtcars, aes(x=hp, y=mpg))+
  ggtitle("Effect of horspower on miles-per-gallon")+
  labs(x="Horse power(hp)", y="Miles per gallon (mpg)")+
  geom_smooth(method="lm",se=F, color="firebrick")+
  geom_point(aes(color=mpg))

#5)saves this plot as a png image called “mpg_vs_hp_auto.png” in your Assignment_6 directory
#effects of Miles per gallon in automatics cars
png(filename="./mpg_vs_hp_auto.png")
ggplot(auto_transmission_dataframe, aes(x=hp, y=mpg))+
  ggtitle("Miles-per-gallon (mpg) vs Horse power(hp) in Automatic Cars")+
  labs(x="Gross Horse power(hp)", y="Miles-per-gallon (mpg)", subtitle = "Effect of horspower on miles-per-gallon")+
  geom_smooth(method="lm",se=F, color="firebrick")+
  geom_point(aes(color=mpg))+
  theme(title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic")
        )
dev.off()

#6)plots the effect of weight on miles-per-gallon (with improved labels, again)
#7) saves this second plot as a tiff image called “mpg_vs_wt_auto.tiff” in your Assignment_5 directory
tiff(filename="./mpg_vs_wt_auto.tiff")
ggplot(auto_transmission_dataframe, aes(x=wt, y=mpg))+
  ggtitle("Miles-per-gallon(mpg) vs Weight (1000 lbs) in Automatic Cars")+
  labs(x="Weight (1000 lbs)", y="Miles-per-gallon (mpg)", subtitle = "Effect of weight on miles-per-gallon")+
  geom_smooth(method="lm",se=F, color="firebrick")+
  geom_point(aes(color=mpg))+
  theme(title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic")
  )
dev.off()


#8) subsets the original mtcars dataframe to include only cars with displacements less than or equal to 200 cu.in.
#9) saves that new subset as a csv file called mtcars_max200_displ.csv
names(mtcars)
disp_max200 <- mtcars$disp == 200.0 | mtcars$disp < 200.0
disp_max200_dataframe <- mtcars[disp_max200,]
write.csv(disp_max200_dataframe,"./mtcars_max200_displ.csv")

#10) includes code to calculate the maximum horsepower for each of the three dataframes (original, automatic, max200)
#11) prints these calculations (from task 10) in a readable format to a new plaintext file called hp_maximums.txt

mtcars_max <- max(mtcars$hp)
mtcars_max_auto <- max(auto_transmission_dataframe$hp)
mtcars_max_max200 <- max(disp_max200_dataframe$hp)
print(c(mtcars_max, mtcars_max_auto, mtcars_max_max200))

text<- "#This code calculates the maximum for each of the three dataframes (original, automatic, max200) of the mtcars dataset.
#YOU MUST INCLUDE THE MTCARS DATASET FOR THIS CODE TO WORK.\n

#In this part, we made a dataframe the cars of automatic transmission
auto_transmissions <- mtcars$am == 1
auto_transmission_dataframe <- mtcars[auto_transmissions,]\n

#In this part, we made a dataframe of the cars of with displacements less than or equal to 200 cu.in.
disp_max200 <- mtcars$disp == 200.0 | mtcars$disp < 200.0
disp_max200_dataframe <- mtcars[disp_max200,]\n

#calculating the maximum horsepowers for the original dataframe
mtcars_max <- max(mtcars$hp)\n

#calculating the maximum horsepowers for the automatic transmission dataframe
mtcars_max_auto <- max(auto_transmission_dataframe$hp)\n

#calculating the maximum horsepowers for the automatic max200 dataframe
mtcars_max_max200 <- max(disp_max200_dataframe$hp)\n

#Printing the calculations
print(c(mtcars_max, mtcars_max_auto, mtcars_max_max200))"
writeLines(text,"./hp_maximums.txt")

#12) combines the following 3 plots into one image using the patchwork package (all 3 plots use the full un-subsetted mtcars data)
#   Scatterplot + trendline of the effect of weight on mpg (points and linear trendlines colored by the number of cylinders)
#   Violin chart of the distributions of mpg for cars, separated and colored by the number of cylinders
#   Scatterplot + trendline of the effect of horsepower on mpg (points and linear trendlines colored by the number of cylinders)
library(patchwork)

names(mtcars)

#   Scatterplot + trendline of the effect of weight on mpg (points and linear trendlines colored by the number of cylinders)
p1 <- ggplot(mtcars, aes(x=wt, y=mpg))+
  ggtitle("Miles-per-gallon(mpg) vs Weight (1000 lbs) in Automatic Cars")+
  labs(x="Weight (1000 lbs)", y="Miles-per-gallon (mpg)", subtitle = "Effect of weight on miles-per-gallon")+
  geom_smooth(method="lm",se=F, color="firebrick")+
  geom_point(aes(color=mpg))+
  theme(title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic")
  )
#   Violin chart of the distributions of mpg for cars, separated and colored by the number of cylinders
p2 <-ggplot(mtcars, aes(x=cyl, y=mpg))+
  ggtitle("Miles-per-gallon(mpg) vs Cylinders")+
  labs(x="Cylinders", y="Miles-per-gallon (mpg)", subtitle = "The distributions of mpg for cars, separated and colored by the number of cylinders", color = "Cylinder") +
  geom_violin(aes(color=as.factor(cyl)))+
  theme(title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic")
  )

#   Scatterplot + trendline of the effect of horsepower on mpg (points and linear trendlines colored by the number of cylinders)
p3 <-ggplot(mtcars, aes(x=hp, y=mpg))+
  ggtitle("Horsepower(hp) vs Miles-per-gallon(mpg)")+
  labs(x="Horse power(hp)", y="Miles per gallon (mpg)", 
       subtitle = "Effect of horspower on miles-per-gallon", 
       color="Cylinder")+
  geom_smooth(method="lm",se=F, color="firebrick")+
  geom_point(aes(color=as.factor(cyl)))+
  theme(title = element_text(face="bold"),
        plot.subtitle = element_text(face="italic")
  )

#13)saves that combined figure as a single png image file called combined_mtcars_plot.png in your Assignment_6 directory
png(filename="./combined_mtcars_plot.png", 1500, 500)
p1+p2+p3
dev.off()


