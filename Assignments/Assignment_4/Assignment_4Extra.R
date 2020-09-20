?read.csv()

df <- read.csv("../../Data/landdata-states.csv", colClasses = c("factor","character","numeric","integer","integer","integer","numeric","numeric","numeric","integer","integer")) # why did I change to read.csv ???
class(df) # what type of object is df?
head(df) # shows the first 6 elements of an object (first 6 rows if you give it a data frame)
class(df$State)


State_updated <- as.factor(df$State)
class(State_updated)


?read.csv()
