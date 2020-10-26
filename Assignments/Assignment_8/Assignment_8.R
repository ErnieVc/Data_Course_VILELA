library(modelr)
library(broom)
library(tidyverse)
library(fitdistrplus) 

#PART 1: 
#1) loads the “/Data/mushroom_growth.csv” data set
shroomData <- read_csv("../../Data/mushroom_growth.csv")

#2)creates several plots exploring relationships between the response and predictors
glimpse(shroomData)


#   This plot looks at the Density and Growth rate of each species
ggplot(shroomData,aes(x=GrowthRate, fill=Species))+
  geom_density(alpha=.5) +
  facet_wrap(~Light)

#   This plot deals with the humidity and growth rate of each species. 
ggplot(shroomData, aes(x=Humidity, y=GrowthRate, color=Species))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~Species)

#   This plot deals with the Nitrogen and growth rate of each species. 
ggplot(shroomData, aes(x=Nitrogen, y=GrowthRate, color=Species))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~Species)


#3)defines at least 2 models that explain the dependent variable “GrowthRate"
#One must be a lm() and
#one must be an aov()


#   This is a model that explains the dependent variable "GrowthRate" as a function of "Humidity" using lm()
mod1 <- lm(data=shroomData,
            GrowthRate ~ Humidity)
plot(mod1$residuals)
summary(mod1)

#   This is a model that explains the dependent variable "GrowthRate" as a function 
# of "Humidity" AND "Light" AND the interaction between "Humidity" and "Light"
mod2 <-aov(data=shroomData,
           GrowthRate ~ Humidity * Light)
plot(mod2$residuals)
summary(mod2)

#   This is a model that explains the dependent variable "GrowthRate" as a function 
# of "Humidity" AND "Light" AND "Temperature"
mod3 <- glm(data=shroomData,
            GrowthRate ~ Humidity + Light + Temperature)
plot(mod3$residuals)
summary(mod3)

#   This is a model that explains the dependent variable "GrowthRate" as a function 
# of "Humidity" AND "Nitrogen" AND "Species" AND the interaction between "Humidity" AND "Nitrogen" AND "Species"
mod4 <- glm(data=shroomData,
            GrowthRate ~ Humidity * Nitrogen * Species)
plot(mod4$residuals)
summary(mod4)

#4) calculates the mean sq. error of each model
mean(mod1$residuals^2) #7854.92
mean(mod2$residuals^2) #5525.948
mean(mod3$residuals^2) #5736.752
mean(mod4$residuals^2) #6412.843

#5) selects the best model you tried
#6) adds predictions based on new hypothetical values for the independent variables used in your model

#     According to the mean sq. error, the mean fof mod2 is the lowest number 

# making predictions for my real data
shroomDataPred<- add_predictions(shroomData,mod2)
summary(shroomDataPred)

#making a hypothetical data frame
hypo_dataFrame = data.frame(Light= rep(c(5,15,25), 120) , 
                     Nitrogen= rep(c(0,5,15,25,35), 72), 
                     Temperature= rep(c(5,10,15,20,25,30),60), 
                     Species= rep(c("P.ostreotus","P.cornucopiae"),180),
                     Humidity= rep(c("High","Low"), 180))

#   making predictions from the hypothetical dataframe
hypo_dataFrame_pred <- add_predictions(hypothetical_df, mod2)

#   adding new columns showing wheter the data point is real or hypothetical 
shroomDataPred$PredictionType <- "Real"
hypo_dataFrame_pred$PredictionType <- "Hypothetical"

#   joining our real data and hypothetical data (with model predictions)
fullpreds <- full_join(shroomDataPred,hypo_dataFrame_pred)


#7) plots these predictions alongside the real data
ggplot(fullpreds, aes(x=Nitrogen, y = pred, color=PredictionType))+
 geom_point() +
  theme_minimal()+
  geom_point(aes(y=GrowthRate),color="Black") 
  

#PART 2 -> NOTE:THIS IS THE CODE FOR THE PART 2 QUESTIONS, 
#Feel free to uncomment and run it. 
#1)
#shroomDataPred_pred <- shroomData %>%
#  add_predictions(mod2)

#mod5 <- aov(data=shroomDataPred_pred,
#            formula = GrowthRate ~ Species * Light * Nitrogen * Humidity * Temperature)
#summary(mod5)
#summary (shroomDataPred_pred)

#3) 
#nonlinearData <- read_csv("../../Data/non_linear_relationship.csv")
#names(nonlinearData)
#model <- lm(response ~ poly(predictor), data= nonlinearData)
#summary(model)

#ggplot(nonlinearData, aes(x=predictor, y=response))+
#  geom_point()+
#  theme_minimal()+
#  stat_smooth(method=lm, formula = y ~ poly(x,5, raw=TRUE))





