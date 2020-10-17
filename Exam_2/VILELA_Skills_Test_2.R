library(tidyverse)
install.packages("zoo")
library(zoo)
library(ggplot2)
install.packages("scales")
library(scales)

landdata_df <- read.csv("./landdata-states.csv")#this loads the land and home data
names(landdata_df)#this displays the column title of the landdata dataframe


#I.      Load the landdata-states.csv file into R
#Re-create the graph shown in "fig1.png"
#Export it to your Exam_2 folder as LASTNAME_Fig_1.jpg (note, that's a jpg, not a png)
#       To change the y-axis values to plain numeric, add options(scipen = 999) to your script
options(scipen = 999)#this changes the y-axis values to plain numeric

ggplot(landdata_df, aes(x=Year, y=Land.Value, color=region))+ #This plots the landdata_df dataframe
  theme_minimal() + 
  geom_smooth(size=1.8)+ 
   labs(y="Land Value (USD)", color="Region") 
ggsave("./VILELA_Fig_1.jpg") #saving the ggplot 

# II.     What is "NA Region???"
#Write some code to show which state(s) are found in the "NA" region   
landdata_df_NA <- landdata_df[is.na(landdata_df$region),]
glimpse(landdata_df_NA)#This takes a quick look at the dataframe. 
print(c("The state(s) found in the NA region is:",landdata_df_NA$State[1]))#This displays a message answering question 2

#III.    The rest of the test uses another data set. The unicef-u5mr.csv data. Get it loaded and take a look.
#It's not exactly tidy. You had better tidy it!
unicef_df <- read.csv("./unicef-u5mr.csv")  #this reads the UNICEF data
names(unicef_df) #This displays the column title of the unicef_df
glimpse(unicef_df)#This takes a quick look at the the unicef_df dataframe. 

unicef_df_long <- pivot_longer(unicef_df, starts_with("U5MR."), #this makes the cleans up the data, making it longer.
                               names_to = "Year", 
                               values_to = "Mortality_Rate", 
                               names_prefix = "U5MR.")
glimpse(unicef_df_long)#taking a look at unicef_df_long
names(unicef_df_long) #this displays the column title of the unicef_df_long
unicef_df_long$Year <- as.numeric(unicef_df_long$Year)#changing the year from character to numeric


#IV.     Re-create the graph shown in fig2.png
#        Export it to your Exam_2 folder as LASTNAME_Fig_2.jpg (note, that's a jpg, not a png)
ggplot(unicef_df_long, aes(x=Year, y= Mortality_Rate, color=Continent))+ #This plots the unicef_df_long dataframe
  theme_minimal() + 
  geom_point(size=3)+
  labs(y="Mortality Rate")
ggsave("./VILELA_Fig_2.jpg")#saving the ggplot 


#IV.     Re-create the graph shown in fig3.png
#Note: This is a line graph of average mortality rate over time for each continent 
#(i.e., all countries in each continent, yearly average), this is NOT a geom_smooth() 
#Export it to your Exam_2 folder as LASTNAME_Fig_3.jpg (note, that's a jpg, not a png)
unicef_df_long_MinusNA = unicef_df_long %>% #This code gets rid of those instances where there is NA as mortality rate. 
  filter(Mortality_Rate !="NA")

unicef_df_long_MinusNA_Mean <- unicef_df_long_MinusNA %>% #this assigns the calculated mena to the new dataframe unicef_df_long_MinusNA_Mean
  group_by(Continent, Year) %>% #this specifies the groupping of Continent by year 
  summarise(Mean_Mortality_Rate = mean(Mortality_Rate)) #this calculates the Mean Mortality Rate of each year by continent.

ggplot(unicef_df_long_MinusNA_Mean, aes(x=Year, y= Mean_Mortality_Rate, color=Continent))+ #This plots the Mean Mortality Rate by year: unicef_df_long_MinusNA_Mean dataframe
  geom_line(aes(group=Continent, color=Continent), size=3)+
  theme_minimal() +
  labs(y="Mean Mortality Rate (deaths per 1000 live births)")
ggsave("./VILELA_Fig_3.jpg")#saving the ggplot 

#V.      Re-create the graph shown in fig4.png
#        Note: The y-axis shows proportions, not raw numbers
#        This is a scatterplot, faceted by region
#        Export it to your Exam_2 folder as LASTNAME_Fig_4.jpg (note, that's a jpg, not a png)
unicef_df_long["Mortality_Rate_Proportions"] <- unicef_df_long$Mortality_Rate/1000 #calculating the proportion of Mortality rate

  ggplot(unicef_df_long,aes(x=Year, y=Mortality_Rate_Proportions, color=Mortality_Rate_Proportions))+ #This plots the proportion of Mortality rate
    geom_point(aes(group=Continent), shape=1, color="blue", fill="white", size=0.5, stroke=0.8)+
  facet_wrap(~Region)+ 
  theme_minimal()+
  labs(y="Mortality Rate")+
  theme(strip.background = element_rect(size = 0.5))
ggsave("VILELA_Fig_4.jpg")#Saving the ggplot

#VI.		Commit and push all your code and files to GitHub. I'll pull your repository at 9:30pm sharp and grade what I find.