library(tidyverse, vtable, dplyr)
library(ggplot2)

patents <- read.csv("patents_10.csv", header = TRUE)

head(patents)

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

