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

#patents in the US count of type of patents granted by US cities
patents_us = subset(patents, ee_country == "US")
nrow(patents_us)
View(patents_us)
Us_p= patents_us%>% group_by(ee_city,ptype) %>% summarize(count = n())
View(Us_p)

#patents in the India and count of type of patents granted by IN cities
patents_in =subset(patents, ee_country == "IN")
nrow(patents_in)
View(patents_in)
In_p= patents_in%>% group_by(ee_city,ptype) %>% summarize(count = n())
View(In_p)

#how long does it take to get granted in the US
patents_us$time_to_approval=patents_us$grantyear - patents_us$applyear
sort(table(patents_us$time_to_approval))
aggregate(time_to_approval ~ ee_state,data = patents_us, FUN = mean)

#how long does it take to get granted in IN
patents_in$time_to_approval=patents_in$grantyear - patents_in$applyear
table(patents_in$time_to_approval)
aggregate(time_to_approval ~ ee_city,data = patents_in, FUN = mean)

#Counting how many patents are granted per type of patents in countries
table(patents$ptype)
patents_types = patents %>% group_by(ee_country,ptype) %>% summarize(count = n())
head(patents_types)
View(patents_types)

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

