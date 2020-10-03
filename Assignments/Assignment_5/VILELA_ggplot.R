library(tidyverse)
library(ggplot2)
data("iris")

#1)Load the “iris” data set
png(file="iris_fig1.png")  
ggplot(iris,aes(x = Sepal.Length, y= Petal.Length)) +
  geom_point(aes(color=Species)) +
  geom_smooth(method = "lm",aes(color=Species))+
  labs(title = "Sepal length vs petal length", subtitle = "for three iris species")+
  theme_minimal()
dev.off()

#2) Duplicate the first 3 figures below and save them in your Assignment_5 
#directory as “iris_fig1.png”, “iris_fig2.png”, “iris_fig3.png”, respectively.
png(file="iris_fig2.png") 
ggplot(iris,aes(x=Petal.Width,fill=Species))+
  geom_density(alpha=0.5)+
  labs(title = "Distribution of Petal Widths", subtitle = "for three iris species", x="Petal Width")+
  theme_minimal()
dev.off()

#3) Keep in mind that by default, I make most of my figures with theme_minimal()
png(file="iris_fig3.png") 
ggplot(iris, aes(x=Species, y=c(Petal.Width/Sepal.Width), fill=Species)) +
  geom_boxplot()+
  labs(title = "Sepal- to Petal-Width Ratio", subtitle = "for three iris species", y="Ratio of Sepal Width to Petal Width")
dev.off()

#4)Read through the different plot types on this website and use the info to reproduce 
#the fourth figure below. Save it as “iris_fig4.png” (this is a great website to bookmark
png(file="iris_fig4_4.png") 
iris$`inames` <- rownames(iris) # create new column for species names
iris$length_norm <- round((iris$Sepal.Length - mean(iris$Sepal.Length))/sd(iris$Sepal.Length), 3) # compute normalized Lengths
iris <- iris[order(iris$length_norm), ] #sort
iris$`inames` <- factor(iris$`inames`, levels = iris$`inames`)#convert to factor to retain sorted order in plot

ggplot(iris, aes(x=`inames`, y=length_norm, label=length_norm)) + 
  geom_bar(stat='identity', aes(fill=Species), width = 1) +
  labs(title="Sepal length deviance from the mean of all observations", y="Deviance from the Mean", caption = "Note: Deviance = Sepal.Length - mean(Sepal.Length)")+
  coord_flip() + 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y=element_blank()
        )
dev.off()

