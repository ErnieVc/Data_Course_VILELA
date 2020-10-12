# Assignment 7 messy code
# Change this to "tidy" format using dplyr verbs

# There's an intuitive dplyr version for almost everything you see here.

# Note: Do not erase the original code, just comment it out and put your own equivalent code below each section
# i.e., change each line of indicated code to a tidy version that does the same thing.

library(tidyr)
library(dplyr)
library(tidyverse)
library(skimr)
library(ggplot2)


##########################
#        Part 1          #
##########################

# 1) load data (wide format)
utah = read.csv("./Utah_Religions_by_County.csv")
#   making it into a long format. 
utah_long = gather(utah,key = Religion, value = Proportion, -c(1:3) )


# 2) subset to only counties with buddhists observed
#buddhist = utah[utah$Buddhism.Mahayana > 0,]

#   subsetting to only countries with buddhist observed.
utah_long$Proportion <- as.numeric(utah_long$Proportion) #making all the Proportion columns into numeric variables
buddhist_long = utah_long %>% 
  filter(Religion %in% "Buddhism.Mahayana" & Proportion > 0.0) #filtering for only Buddhist counties

# 3) order rows by population (descending)
#buddhist = buddhist[order(buddhist$Pop_2010, decreasing = TRUE),]

#   making long descending
buddhist_long_descending <- buddhist_long %>% arrange(desc(Pop_2010)) #arranging the Buddhist counties in descending order


# 4)write this new dataframe to a file
#write.csv(buddhist, file = "./buddhist_counties.csv", row.names = FALSE, quote = FALSE)

write.csv(buddhist_long_descending, file = "./buddhist_long_counties.csv", row.names = FALSE, quote = FALSE)


## get group summaries of religiousity based on population ##

# divide each county into one of six groups based on populations
# note: keep these two lines the same in your updated code!
groups = kmeans(utah$Pop_2010,6) # clusters data into 6 groups based on proximity to mean of potential groups
utah$Pop.Group = groups$cluster # assigns a new variable to utah giving group for each county

# subset to each group and find summary stats on Religiosity for each
#group1 = mean(utah[utah$Pop.Group == 1,]$Religious)
#group2 = mean(utah[utah$Pop.Group == 2,]$Religious)
#group3 = mean(utah[utah$Pop.Group == 3,]$Religious)
#group4 = mean(utah[utah$Pop.Group == 4,]$Religious)
#group5 = mean(utah[utah$Pop.Group == 5,]$Religious)
#group6 = mean(utah[utah$Pop.Group == 6,]$Religious)

#   Subsetting each group and finding summary stats on REligiosity for each
Group_Religion = utah %>%
  group_by(Pop.Group)%>%
  summarise(Mean_Religion = mean(Religious))


# same, but mean population
#group1.pop = mean(utah[utah$Pop.Group == 1,]$Pop_2010)
#group2.pop = mean(utah[utah$Pop.Group == 2,]$Pop_2010)
#group3.pop = mean(utah[utah$Pop.Group == 3,]$Pop_2010)
#group4.pop = mean(utah[utah$Pop.Group == 4,]$Pop_2010)
#group5.pop = mean(utah[utah$Pop.Group == 5,]$Pop_2010)
#group6.pop = mean(utah[utah$Pop.Group == 6,]$Pop_2010)

#   Subsetting each group and finding summary stats on mean population for each
Group_Population = utah %>%
  group_by(Pop.Group) %>%
  summarise(Mean_Population = mean(Pop_2010))

glimpse(utah)# take quick look at utah

# make data frame of each group and mean religiosity
#religiosity = data.frame(Pop.Group = c("group1","group2","group3","group4","group5","group6"),
#           Mean.Religiosity = c(group1,group2,group3,group4,group5,group6),
#           Mean.Pop = c(group1.pop,group2.pop,group3.pop,group4.pop,group5.pop,group6.pop))

#   making a data frame of each group and mean religiosity
Group_Religiosity <- merge(Group_Religion, Group_Population, by = "Pop.Group")%>% 
  arrange(desc(Mean_Population))# order by decreasing population


#religiosity # take quick look at resulting table

# order by decreasing population
#religiosity = religiosity[order(religiosity$Mean.Pop, decreasing = TRUE),]

#religiosity # take quick look at resulting table

# plot that table (redo this using ggplot)
#plot(x=religiosity$Mean.Pop,y=religiosity$Mean.Religiosity)

#   plot that table using ggplot
ggplot(Group_Religiosity, aes(x=Mean_Population, y=Mean_Religion))+
  labs(title="Comparing the Mean Population and Religion") +
  geom_point(shape = 1)+
  theme_minimal()+
  theme(panel.grid=element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=1))
 



#####################################
#              Part 2               #
# Beginning to look at correlations #
# run this code without changing it #
# it's already in very tidy form    #
#####################################

# Look for correlations between certain religious groups and non-religious people
religions = names(utah)[-c(1:4)]

utah %>%
  pivot_longer(names_to = "Religion", values_to = "Proportion",religions) %>%
  ggplot(aes(x=Proportion,y=Religious)) + geom_point() + geom_smooth(method="lm") + lims(y=c(0,1)) +
  facet_wrap(~Religion,scales = "free") + theme_bw() + theme(panel.grid = element_blank(), strip.background = element_rect(fill="Gray"))+
  coord_cartesian(xlim=c(0,0.1), ylim=c(0, 1000000))

# Look through those plots and answer the following questions:
# 1.  Which religious group correlates most strongly in a given area with the proportion of non-religious people?
#LDS

# 2.  What is the direction of that correlation?


# 3.  What can you say about the relationships shown here?


# 4.  Examine the axis scales. How could you modify the code above to more accurately portray values on an "equal footing?"


# UPLOAD YOUR ANSWERS TO CANVAS
# DON'T FORGET TO PUSH YOUR TIDY CODE TO GITHUB AS WELL!