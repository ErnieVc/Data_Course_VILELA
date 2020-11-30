library(tidyverse)
library(modelr)
library(GGally)
library(lindia)
library(skimr)
library(patchwork)
library(caret)
library(janitor)
library(broom)

# I.      Load and clean FacultySalaries_1995.csv file
#Re-create the graph shown in "fig1.png"
#Export it to your Exam_3 folder as LASTNAME_Fig_1.jpg (note, that's a jpg, not a png)

#this loads the faculty salaries data
df <- read.csv("./FacultySalaries_1995.csv")



names(df)#this displays the column titles of the faculty salaries dataframe


#Before cleaning up the data, the names will be renamed of the columns, so that we can exactly 
#recreate the Fig1.png
renamed_df <- df %>% rename(
                      Full = AvgFullProfSalary,
                      Assoc = AvgAssocProfSalary,
                      Assist = AvgAssistProfSalary)
names(renamed_df)

#cleaning up data

#this creates a Rank column, and a Salary column, allowing us to separate the Salaries by ranking.
#this also creates a new dataframe(ranked_df), which contains the updated information.
ranked_df<- pivot_longer(renamed_df, 
                      -c(1:4,8:17), 
                      names_to ="Rank", 
                      values_to = "Salary", 
                      names_prefix ="Rank")

filtered_ranked_df <- filter(ranked_df, Tier == "I" | Tier == "IIA"| Tier == "IIB")

# make salary category
salary_graph <- ggplot(filtered_ranked_df, aes(x=Rank, y=Salary, fill=Rank))+ #This plots the landdata_df dataframe
  facet_wrap(~Tier)+
  geom_boxplot(outlier.color="black")+
  labs(x="Rank", y="Salary")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle = 45, hjust = 1))
salary_graph
ggsave("./VILELA_Fig_1.jpg", salary_graph) #saving the image


# II.     Export an ANOVA table to a file called "Salary_ANOVA_Summary.txt"
#The ANOVA model should test the influence of "State", "Tier", and "Rank" on 
#"Salary" but should NOT include any interactions between those predictors.

#creating the anova table, with only the interactions between State, Tier, and Rank on Salary
a_table <- aov(data = ranked_df, Salary ~ State + Tier + Rank)
a_table_summary <- summary(a_table) #this outputs a summary.
a_table_summary

#exporing the summarized anova table
capture.output(a_table_summary, file = "./Salary_ANOVA_Summary.txt")


#III.    The rest of the test uses another data set. The "Juniper_Oils.csv" data. 
#Get it loaded and take a look.
# It's not exactly tidy either. Get used to that. It's real data collected as part of a collaboration between 
#Young Living Inc. and UVU Microbiology. A number of dead cedar trees were collected 
#and the chemical composition of their essential oil content was measured. 
#The hypothesis was that certain chemicals would degrade over time since they died in fires. 
#So there are a bunch of columns for chemical compounds, and a column for "YearsSinceBurn." 
#The values under each chemical are Mass-Spec concentrations.

#Loading the dataset
df1 <- read.csv("./Juniper_Oils.csv")
names(df1) #taking a look at the column of df1

#cleaning up the data set
#using janitor to replaces the capitaliztion for low caps, and the spaces and "-" for "_"
names(df1) <- janitor::make_clean_names(names(df1))
names(df1)

#using the janitor package, we also clean up the list given by Dr. Zahn 
list_chemicals <- c("alpha-pinene","para-cymene","alpha-terpineol","cedr-9-ene","alpha-cedrene",
                    "beta-cedrene","cis-thujopsene","alpha-himachalene","beta-chamigrene","cuparene",
                    "compound 1","alpha-chamigrene","widdrol","cedrol","beta-acorenol","alpha-acorenol",
                    "gamma-eudesmol","beta-eudesmol","alpha-eudesmol","cedr-8-en-13-ol","cedr-8-en-15-ol",
                    "compound 2","thujopsenal")
print(list_chemicals)

list_chemicals <- janitor::make_clean_names(print(list_chemicals))
print(list_chemicals)

#Now we can select the data we want: We are going to get the list of chemicals that we care about out of df1.
df1_select_chemicals <- select(df1, "years_since_burn", all_of(list_chemicals))
head(df1_select_chemicals) #looking at the data frame structure
names(df1_select_chemicals) #looking at the column names

#Now we turn it into a long format
df1_select_chemicals_longer <- pivot_longer(df1_select_chemicals, 
                                            cols = list_chemicals, 
                                            names_to= "compound", 
                                            values_to= "concentration")
head(df1_select_chemicals_longer) #looking at the data frame structure
names(df1_select_chemicals_longer) #looking at the column names


#IV.     Make me a graph of the following:
#x = YearsSinceBurn
#y = Concentration
#facet = ChemicalID (use free y-axis scales)
#See Fig2.png for an idea of what I'm looking for
chemical_graph <- ggplot(df1_select_chemicals_longer, aes(x=years_since_burn, y=concentration))+
  facet_wrap(~compound, scales= "free_y")+
  theme_minimal()+
  geom_smooth()+
  labs(x="Years Since Burn", y = "Concentration")
chemical_graph

ggsave("./VILELA_Fig_2.jpg", chemical_graph) #saving the graph.


#V.      Use a generalized linear model to find which chemicals show concentrations that are significantly (significant,
#as in P < 0.05) affected by "Years Since Burn". Use the tidy() function from the broom R package 
#in order to produce a data frame showing JUST the significant chemicals and their model output (coefficient 
#estimates, p-values, etc)     

options(scipen = 999)#this changes the y-axis values to plain numeric
mod1 <- glm(data=df1_select_chemicals_longer, concentration ~ years_since_burn * compound)
mod1

tidymod <- tidy(mod1)
tidymod #glimpsing at our tidy data

significant_tidymod <- filter(tidymod, p.value < 0.05)
significant_tidymod #glimpsing at our filtered data for significant values

#removing the prefix 'compound' in our final data output
significant_tidymod$term <- gsub("compound", "", significant_tidymod$term)
significant_tidymod





