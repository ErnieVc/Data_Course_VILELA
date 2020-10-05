library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggimage)

library(ggpubr)
library(jpeg)

library(png)
library(grid)

library(ggimage)

data("starwars")
names(starwars)

imgfile1<- "./george.jpg"
img1<-jpeg::readJPEG(imgfile1)

imgfile<- "./jk.jpeg"
img<-jpeg::readJPEG(imgfile)

starwars$image <- "./zahn.jpeg"

png(filename="./uglyplot7.png",2000,1400)
ggplot(starwars, aes(x=height, y=name, image=image))+
  background_image(img1)+
  geom_image()+
  ggtitle("Height Comparison of Starwars Characters by George Lucas")+
  theme(panel.background = element_rect(fill="green", color="black"),
        plot.background = element_rect(fill="red", color="black"),
        title = element_text(face="bold", color="white"),
        axis.title.y = element_text(color = "green"),
        axis.title.x = element_text(color = "blue"),
        axis.text = element_text(color = "yellow")
        )

dev.off()



  #labs(x="Height", y="Name", 
   #    subtitle = "Effect of horspower on miles-per-gallon", 
     #  color="Cylinder")+
#geom_smooth(method="lm",se=F, color="firebrick")+
  
  
  #geom_point(aes(color=height))
  #theme(title = element_text(face="bold"),
   #     plot.subtitle = element_text(face="italic")
  