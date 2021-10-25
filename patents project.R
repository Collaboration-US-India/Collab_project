#Load data
setwd("C:/Users/Seizal Pathania/Downloads/Rfiles")
patents <- read.csv("patents_10.csv", header = TRUE)
head(patents)
View(patents)

#EDA
sum(table(patents$patnum))
sum(table(patents$patnum_kpss))
patents$patnum_kpss = gsub("D","", as.character(patents$patnum))
View(patents)
sum(table(patents$ptype))
sum(table(patents$applnum))
sum(table(patents$ee_number))
sum(table(patents$ee_name))
sum(table(patents$ee_role))
sum(table(patents$ee_role_desc))
sum(table(patents$ee_state))
cor(patents$grantyear,patents$X)
anov=aov(patents$grantyear~patents$ee_ind_fname,data=patents)
summary(anov)
anov=aov(patents$grantyear~patents$ee_ind_lname,data=patents)
summary(anov)
patent=patents[-c(1,3,10,11)]
View(patents)

#count of patents granted by each country
library(dplyr)
patents_by_country = patents %>% group_by(ee_country) %>% summarize(count = n())
head(patents_by_country)
View(patents_by_country)

#Visualising the Patents by countries data
pie(patents_by_country$count,labels = patents_by_country$ee_country,main = "Patents By Country",col= rainbow(length(patents_by_country$count)))
legend("topright",patents_by_country$ee_country,cex=0.5,fill = rainbow(length(patents_by_country$count)))
d1<- density(patents$backward_cites)
plot(d1,main = "Desity of Backward Cities")
d2<- density(patents$forward_cites)
plot(d2,main = "Desity of Forward Cities")

#patents in the US count of type of patents granted by US cities
patents_us = subset(patents, ee_country == "US")
nrow(patents_us)
View(patents_us)
Us_p= patents_us%>% group_by(ee_city,ptype) %>% summarize(count = n())
View(Us_p)
Us_p_type= Us_p%>% group_by(ptype) %>% summarize(count = n())
head(Us_p_type)

#Visualising the data
hist(Us_p_type$count,main = "Types of Patents in US",xlab = "Patent Types Count",col="darkmagenta")

#patents in the India and count of type of patents granted by IN cities
patents_in =subset(patents, ee_country == "IN")
nrow(patents_in)
View(patents_in)
In_p= patents_in%>% group_by(ee_city,ptype) %>% summarize(count = n())
View(In_p)
In_p_type= In_p%>% group_by(ptype) %>% summarize(count = n())
head(In_p_type)

#Visualising the data
hist(In_p_type$count,main = "Types of Patents in In",xlab = "Patent Types Count",col="darkgreen")

#how long does it take to get granted in the US
patents_us$time_to_approval=patents_us$grantyear - patents_us$applyear
sort(table(patents_us$time_to_approval))
aggregate(time_to_approval ~ ee_state,data = patents_us, FUN = mean)

#Visualising the time taken to approve
hist(a$time_to_approval,main = "Time taken to get granted in Us",xlab = "Time",col = "red")
pie(a$time_to_approval,main = "Time taken to get granted in Us")

#how long does it take to get granted in IN
patents_in$time_to_approval=patents_in$grantyear - patents_in$applyear
table(patents_in$time_to_approval)
aggregate(time_to_approval ~ ee_city,data = patents_in, FUN = mean)

#Visualising the time taken to approve
hist(b$time_to_approval,main = "Time taken to get granted in In",xlab = "Time",col = "blue")
pie(b$time_to_approval,main = "Time taken to get granted in In")

#Counting how many patents are granted per type of patents in countries
table(patents$ptype)
patents_types = patents %>% group_by(ee_country,ptype) %>% summarize(count = n())
head(patents_types)
View(patents_types)

## Additional coding for variables used in upcoming code

patents_by_country <- patents %>%
  group_by(ee_country) %>%
  summarize(count = n())

head(patents_by_country)

# US Subset
patents_us <- subset(patents, ee_country == "US")

nrow(patents_us)

# US Approval Time
patents_us$time_to_approval <- patents_us$grantyear - 
  patents_us$applyear

aggregate(time_to_approval ~ ee_state,
          data = patents_us, 
          FUN = mean)

# Count of patents by type per city in US
patents_us_count <- patents_us %>% group_by(ee_city,ptype) %>% summarize(count = n())
head(patents_us_count)

# Top 10 cities with patents in US
patents_us_10 <- patents_us_count[with(patents_us_count, order(-count)),]
top10_patents <- patents_us_10[1:10,]

# Of the top 10 cities in the US with patents, 5 of them are in the Bay Area

# Patent types across countries
table(patents$ptype)
patents_types = patents %>% group_by(ee_country,ptype) %>% summarize(count = n())
head(patents_types)

patent_types_5 <- patents_types[with(patents_types, order(-count)),]
top5_types <- patent_types_5[1:5,]

# Top 5 countries are utility patents of US, Japan, Korea, Germany, and the design patents of US

# Graphing patents by US cities
ggplot(top10_patents, aes(x = reorder(ee_city, -count), y = count)) + 
  geom_bar(stat = "Identity") +
  labs(x = 'City', y = '# of Utility Patents', color = NULL) +
  theme_classic() +
  guides(linetype = FALSE)

# Graphing patents across countries
ggplot(top5_types, aes(x = reorder(ee_country, -count), y = count)) + 
  geom_bar(stat = "Identity") +
  labs(x = 'Country', y = '# of Patents', color = NULL) +
  theme_classic() +
  guides(linetype = FALSE)

##Herman Singh: Project Problem 1

patents <- read.csv("patents_10.csv", header = TRUE)

head(patents)

library(dplyr)

patents_by_country <- patents %>%
  group_by(ee_country) %>%
  summarize(count = n())

head(patents_by_country)

#Will be looking at data from the 3 countries where group members are located

#US Subset
patents_us <- subset(patents, ee_country == "US")

nrow(patents_us)

#Approval Time in the US by State
patents_us$time_to_approval <- patents_us$grantyear - 
  patents_us$applyear

aggregate(time_to_approval ~ ee_state,
          data = patents_us, 
          FUN = mean)

#Count of Patents by City in the US
patents_us_count <- patents_us %>% group_by(ee_city) %>% summarize(count = n())

head(patents_us_count)

#20 Cities With Most Patents in the US
patents_us_20 <- patents_us_count[with(patents_us_count, order(-count)),]

top_20_citypatents_us <- patents_us_20[1:20,]

top_20_citypatents_us

#it is interesting to note that 8 of the top 20 are located in California
#Another fun fact is that half of the top 20 consists of cities from the
#2 states I have lived in

#india subset
patents_in <- subset(patents, ee_country == "IN")

nrow(patents_in)

#Count of Patents by City in India
patents_in_count <- patents_in %>% group_by(ee_city) %>% summarize(count = n())

head(patents_in_count)

#20 Cities With Most Patents in India
patents_in_20 <- patents_in_count[with(patents_in_count, order(-count)),]

top_20_citypatents_in <- patents_in_20[1:20,]

top_20_citypatents_in

#Unsurprisingly Mumbai, Bangalore, and New Delhi are the highest ranking cities 
#for patents in india. Additionally, they also show up with other cities further
#down the list.

#germany subset
patents_de <- subset(patents, ee_country == "DE")

nrow(patents_de)

#Count of Patents by City in Germany
patents_de_count <- patents_de %>% group_by(ee_city) %>% summarize(count = n())

head(patents_de_count)

#20 Cities with most patents in Germany
patents_de_20 <- patents_de_count[with(patents_de_count, order(-count)),]

top_20_citypatents_de <- patents_de_20[1:20,]

top_20_citypatents_de

#Munich and Stuttgart top the list in Germany, which is no surprise given that
#they are considered innovation centers in the country

#Looking at these countries together, it is apparent that the numbers in the US
#far exceeds those of Germany and India. Something of note is that the city in 
#20th place for the US, Seattle, would find itself in 3rd place if it were in 
#Germany, and in 1st place by a very large margin if it were in India