library(ggplot2)

#Questions 1, 2, 3
# 4) turning into a factor instead of character
?read.table() #This brings up the help file
df <- read.csv("../../Data/landdata-states.csv", colClasses = c("factor")) # why did I change to read.csv ???
class(df$State)
class(df$region)
class(df$Date)

#or

df <- read.csv("../../Data/landdata-states.csv", stringsAsFactors = TRUE)
class(df$State)
class(df$region)
class(df$Date)


#5. What command would give the summary stats for ONLY the Home.Value column?
summary (df$Home.Value)
#6) What happens when you run names (df)[4]
names (df)[4]

#7. What is happening when you add (…col=df$region) to the above plotting code?
#In other words, what happens when you run: 



df <-  read.table("../../Data/ITS_mapping.csv", header = TRUE, sep = "\t")


df <-  read.csv("../../Data/landdata-states.csv") # why did I change to read.csv ???
summary (df$Home.Value) # what type of object is df?
var <- names(df)[4] # shows the first 6 elements of an object (first 6 rows if you give it a data frame)
class(var)

plot (x=df$Year, y=df$Land.Value, (as.factor(col=df$region)))
ggplot(df, aes(y=Land.Value, x=Year,col=region))+geom_point()
ggplot(df, aes(y=Land.Value, x=Year,col=region, shape=region))+geom_point()



