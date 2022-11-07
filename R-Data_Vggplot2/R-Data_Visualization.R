library(tidyverse)
library(ggplot2)
library(readr)
library(splitstackshape)
library(scales)
library(dplyr)
library(carData)
library(plotly)

# Import the data set
data <- read_csv("database.csv")

# Transform the variables into factors
data <- as.data.frame(unclass(data),stringsAsFactors=TRUE)

# Missing values
sapply(data, function(x) sum(is.na(x)))

# Fill the missing value on the Perpetrator.age variable
data <- data %>% 
  mutate(Perpetrator.Age = replace_na(Perpetrator.Age, 0))

sapply(data, function(x) sum(is.na(x)))


# Bar chart ordered (descending) - Which states have the most incidents? -------

by_state <- data %>%
  group_by(State) %>%
  summarise(n_incidents = n())

ggplot(by_state,aes(x=reorder(State,desc(n_incidents)), y = n_incidents))+
  geom_bar(stat="identity", fill="red") +
  labs(x="State",title="Distribution of insidents per state") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


# Bar chart ordered (descending) - Which weapon is used the most?  -------------

by_weapon <- data %>%
  group_by(Weapon) %>%
  summarise(n_incidents = n())

ggplot(by_weapon,aes(x=reorder(Weapon,desc(n_incidents)), y = n_incidents))+
  geom_bar(stat="identity", fill="blue") +
  labs(x="Weapon",title="The most used weapon") +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))


# Box plot - Compare the age of Victims and the Perpetrators. ------------------

df_age <- data %>%
  filter(Perpetrator.Age != 0,
         Victim.Age != 0,
         Perpetrator.Age < 100,
         Victim.Age < 100)

ggplot(df_age,
       aes(y = Victim.Age)) +
  geom_boxplot() +
  labs(title = "")

ggplot(df_age,
       aes(y = Perpetrator.Age)) +
  geom_boxplot(color="purple") +
  labs(title = "")

par(mfrow= c(1,1))
boxplot(df_age$Victim.Age, 
        df_age$Perpetrator.Age, col=c("turquoise","tomato"),
        names=c("Age of victims","Age of Perpetrators"))

# Line plot number of incidents through time. ----------------------------------
by_year <- data %>%
  group_by(Year) %>%
  summarise(n_incidents = sum(Incident))


ggplot(by_year, aes(x = Year, y = n_incidents)) +
  geom_line(size = 1.5, color = "black") +
  geom_point(size = 3, color = "steelblue") +
  labs(y = "Incidents", x = "Year",
       title = "Number of incidents through time")


# Pie chart with gender of victim. ---------------------------------------------

gender <- data %>%
  mutate(total = n()) %>%
  group_by(Victim.Sex, total) %>%
  summarise(n_incidents = n(),
            pct = n_incidents / total) %>%
  distinct(Victim.Sex, .keep_all = T)

ggplot(gender, aes(x="", y=pct, fill=Victim.Sex)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)

# histogram with distribution of victims per crime -----------------------------

ggplot(data, aes(x=Incident, color = 'red')) + geom_histogram(binwidth = 1) + xlim(0,20)
ggplot(df_age, aes(x=Victim.Age, color = 'red')) + geom_histogram(binwidth = 1)

# Heat map ---------------------------------------------------------------------

foo <- data %>%
  group_by(Perpetrator.Race, Weapon) %>%
  summarise(n_incidents = n())

ggplot(foo, aes(Perpetrator.Race, Weapon, fill= n_incidents)) + 
  geom_tile()



foo <- data %>%
  group_by(Perpetrator.Race) %>%
  mutate(total_per_race = n()) %>%
  ungroup() %>%
  group_by(Perpetrator.Race, Weapon, total_per_race) %>%
  summarise(n_incidents = n()) %>%
  mutate(pct = n_incidents / total_per_race)


ggplot(foo, aes(Perpetrator.Race, Weapon, fill= pct)) + 
  geom_tile()

# scatter plot with age of victim vs age of perpetrator ------------------------

ggplot(df_age, aes(x=Victim.Age, y=Perpetrator.Age, color = 'red')) + geom_point(alpha = 1/100)

# Choropleth United states -----------------------------------------------------

data_choro <- data %>%
  select(Year, State, Incident) %>%
  filter(State != "Hawaii",
         State != "Rhodes Island",
         State != "Alaska")

graph <- plot_geo(data_choro,
                  locationmode = "USA -states",
                  frame = ~Year) %>%
  add_trace(locations = ~State, 
            z= ~Incident,
            color = ~Incident)
graph




