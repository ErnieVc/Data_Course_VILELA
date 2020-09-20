?read.table() #This brings up the help file
df <-  read.csv("../../Data/landdata-states.csv") # why did I change to read.csv ???
class(df) # what type of object is df?
head(df) # shows the first 6 elements of an object (first 6 rows if you give it a data frame)
class(df$State)
State_updated <- as.factor(df$State)
class(State_updated)


df[1]





dfi <- read.csv("../../Data/landdata-states.csv", stringsAsFactors = FALSE)
class(dfi) # what type of object is df?
head(dfi) # shows the first 6 elements of an object (first 6 rows if you give it a data frame)
class(dfi$Home.Value)








library(ggplot2)


df <-  read.csv("../../Data/landdata-states.csv") # why did I change to read.csv ???
summary (df$Home.Value) # what type of object is df?
var <- names(df)[4] # shows the first 6 elements of an object (first 6 rows if you give it a data frame)
class(var)

plot (x=df$Year, y=df$Land.Value, (as.factor(col=df$region)))


ggplot(df, aes(y=Land.Value, x=Year,col=region))+geom_point()
ggplot(df, aes(y=Land.Value, x=Year,col=region, shape=region))+geom_point()
