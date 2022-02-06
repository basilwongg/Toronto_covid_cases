#data gathered from OpenTorontoData
library(opendatatoronto)
library(dplyr)
library(tidyverse) # A collection of data-related packages
library(janitor) # Helps clean datasets
library(tidyr) # Helps make tidy datasets
library(ggplot2)
library(lubridate)
library(gridExtra)
library(scales)

# get package
package <- show_package("64b54586-6180-4485-83eb-81e8fae3b8fe")
package

# get all resources for this package
resources <- list_package_resources("64b54586-6180-4485-83eb-81e8fae3b8fe")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
#data

# make sure we are working on the correct directory
getwd()
setwd("/Users/basilwong/Documents/UofT/Year5/STA304/Toronto_covid_cases/Scripts")

#write csv to a csv file
write_csv(
  x=data,
  file="../toronto_covid_cases.csv"
)

#read csv and store it in raw_data variable
raw_data <- 
  read.csv(
    "../toronto_covid_cases.csv")

#change the char to date
raw_data <- 
  mutate(raw_data, Episode.Date = as.Date(Episode.Date, format="%Y-%m-%d"))
raw_data <-
  mutate(raw_data, Reported.Date = as.Date(Reported.Date, format="%Y-%m-%d"))

#data cleaning
clean_raw_data <- subset(raw_data, select = -c(X_id, Assigned_ID))
#clean name
clean_raw_data <- clean_names(clean_raw_data)

#recode the name of the age group
clean_raw_data <- 
  clean_raw_data %>%
  mutate(
    age_group = 
      recode(
        age_group,
        '19 and younger' = '≤19',
        '20 to 29 Years' = '20-29',
        '30 to 39 Years' = '30-39',
        '40 to 49 Years' = '40-49',
        '50 to 59 Years' = '50-59',
        '60 to 69 Years' = '60-69',
        '70 to 79 Years' = '70-79',
        '80 to 89 Years' = '80-89',
        '90 and older' = '90+'
      )
  )

#create a graph for source of infection against age group
graph1 <-
  clean_raw_data %>%
  drop_na(age_group) %>%
  ggplot(mapping = aes(x=source_of_infection, fill=age_group)) +
  #ggplot(mapping = aes(x=source_of_infection, y = Freq,fill=age_group)) +
  #geom_col() +
  geom_bar(width=0.7) +
  theme(axis.text.x = element_text(size = 10)) +
  scale_fill_brewer(palette="Paired") +
  coord_flip() +
  labs(x="Source of Infection")

#get the graph that only contains data from community infection
community_data <- subset(clean_raw_data, source_of_infection == "Community") 

graph2 <- 
  community_data %>%
  drop_na(age_group) %>%
  ggplot(mapping = aes(x=age_group)) +
  geom_histogram(stat= 'count', color="darkblue", fill="lightblue") +
  theme(axis.text.x = element_text(size = 10)) +
  labs(x="Age Group", y="Number of \nCommunity Infection")

#arrange the grid with gridExtra package
grid.arrange(graph1, graph2, nrow=2)

#Fatal data
# fatal_data <- 
#   clean_raw_data %>%
#   drop_na(age_group) %>%
#   group_by(age_group, outcome) %>%
#   summarise(n = n())
# fatal_data


# temp_fatal_data <- 
#   clean_raw_data[,c(2,10)] %>%
#   drop_na(age_group) %>%
#   subset(age_group == '≤19')

fatal_rate <-
  clean_raw_data [,c(2,10)] %>%
  drop_na(age_group) %>%
  group_by(age_group, outcome) %>%
  summarise(n = n()) %>%
  group_by(age_group) %>%
  mutate(total = sum(n), Ratio = round(n/total, 5)) %>%
  subset(outcome == "FATAL")

#must re run the above code otherwise the percent function will not work 
#because we cannot convert % to % again
fatal_rate <-
  mutate(fatal_rate, Ratio = percent(Ratio, accuracy = 0.001)) 

fatal_rate[,c(-2)] %>%
  kable(
    caption = "Toronto Covid-19 Fatal Rate (by Age Group)",
    col.names = c("Age Range", "Num of Fatal(s)", "Num of cases", "Fatal Rate"),
    booktabs = TRUE, 
    linesep = "",
    align = c('l', 'r', 'r', 'r'),
    format.args = list(big.mark = ",")
  )


# neighbour_age <- 
#   clean_raw_data[,c(2,14)] %>%
#   ggplot(mapping = aes(x=age_group, fill=ever_hospitalized)) +
#   geom_bar(position="dodge") +
#   theme(axis.text.x = element_text(angle = 60,hjust = 1))
# neighbour_age

# want a graph with hospitalized people and their reported time!! to determine the burden of medic system
hospitalized <- 
  clean_raw_data[,c(8,14)]
my_stamp<-stamp( "2019-02", 
                 orders = "ym") 
hospitalized$reported_date <-
  my_stamp(hospitalized$reported_date)

hospitalized <-
  subset(hospitalized, ever_hospitalized=="Yes")

hospitalized_graph <-
  hospitalized %>%
  #drop_na(reported_date) %>%
  #drop_na(hospitalized) %>%
  ggplot(mapping = aes(x=reported_date)) +
  geom_histogram(stat="count", color="darkgoldenrod", fill="darkgoldenrod1") +
  coord_flip() +
  theme(axis.text.x = element_text(size = 8)) +
  labs(x="Reported Date", y="Number of Hospitalized People")
hospitalized_graph  

timeline <- 
  clean_raw_data[,c(7,8)]
my_stamp<-stamp( "2019-02", 
                 orders = "ym") 
timeline$episode_date <-
  my_stamp(timeline$episode_date)
timeline_graph <-
  timeline %>%
  ggplot(mapping = aes(x=episode_date)) +
  geom_histogram(stat="count") + 
  coord_flip()
timeline_graph













