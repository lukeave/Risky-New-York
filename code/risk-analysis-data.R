## RISK FACTOR ANALYSIS

library(tidyverse)
library(tidyr)

#load data from 1965 to 1989

gayguides1965to1989 <- read_rds("~/Risky-New-York/gay guides data/data.rds")


#load gay guides data from 1990 to 1996
gayguides1990 <- read.csv("~/Risky-New-York/gay guides data/gayguides1990.csv")
gayguides1991 <- read.csv("~/Risky-New-York/gay guides data/gayguides1991.csv")
gayguides1992 <- read.csv("~/Risky-New-York/gay guides data/gayguides1992.csv")
gayguides1993 <- read.csv("~/Risky-New-York/gay guides data/gayguides1993.csv")
gayguides1994 <- read.csv("~/Risky-New-York/gay guides data/gayguides1994.csv")
gayguides1995 <- read.csv("~/Risky-New-York/gay guides data/gayguides1995.csv")
gayguides1996 <- read.csv("~/Risky-New-York/gay guides data/gayguides1996.csv")

#combine all data frames from 1990 to 1996 into a single data frame
gayguides1990to1996 <- rbind(gayguides1990, gayguides1991, gayguides1992, gayguides1993, gayguides1994, gayguides1995, gayguides1996)
rm(gayguides1990)
rm(gayguides1991)
rm(gayguides1992)
rm(gayguides1993)
rm(gayguides1994)
rm(gayguides1995)
rm(gayguides1996)

#select  columns that matter in 1965-1989
gayguides1965to1989 <- gayguides1965to1989 %>% 
  select(title, description, type, amenityfeatures, city, state, Year)

#select  columns that matter in 1965-1989
gayguides1990to1996 <- gayguides1990to1996 %>% 
  select(title, description, type, amenityfeatures, city, state, Year)

#merge data sets
prerisk.data <- rbind(gayguides1965to1989, gayguides1990to1996)

#subset data to New York
total.nylocations <- prerisk.data %>% 
  filter(state == "NY") %>% 
  group_by(Year) %>% 
  summarize(count = n())

#filter data for risk factor
total.nyrisky <- prerisk.data %>% 
  filter(state == "NY") %>% 
  filter(grepl('AYOR', amenityfeatures) | grepl('HOT', amenityfeatures) | grepl('locally', description) | grepl('HOT!', description) | grepl('hot)', description)) %>% 
  group_by(Year) %>% 
  summarize(count = n())

#join data sets
risky.newyork.count <- full_join(total.nylocations, total.nyrisky, by = "Year")

colnames(risky.newyork.count)[2] <- "total.count"
colnames(risky.newyork.count)[3] <- "risky.count"

#get the relative risk per total number of locations in each year
risky.newyork.count <- risky.newyork.count %>% 
  mutate(relative.risk = risky.count / total.count * 100)

write.csv(risky.newyork.count, file = "~/Risky-New-York/gay guides data/risky.newyork.count.csv")

#create a subset of the data "risky.newyork" that is geocoded
risky.newyork <- prerisk.data %>%
  filter(state == "NY") %>% 
  filter(grepl('AYOR', amenityfeatures) | grepl('HOT', amenityfeatures) | grepl('locally', description) | grepl('HOT!', description) | grepl('hot)', description))

#make sure the "cruisy" column added to gayguides.complete also exist in risky.newyork
risky.newyork <- risky.newyork %>%
  mutate(cruisy = ifelse(grepl('cruis', description), "TRUE",
                         ifelse(grepl('Cruis', description), "TRUE", 
                                ifelse(grepl('Cruis', amenityfeatures), "TRUE",
                                       ifelse(grepl('Cruis', type), "TRUE", "FALSE")))))

#make sure to collapse types into new analytic categories as done in gayguides.complete
risky.newyork$categories <- ifelse(grepl('Religious', risky.newyork$type), "Religious Institutions",
                                        ifelse(grepl('Hotel', risky.newyork$type), "Accommodations",
                                               ifelse(grepl('Accommodation', risky.newyork$type), "Accommodations",
                                                      ifelse(grepl('Bar', risky.newyork$type), "Bars",
                                                             ifelse(grepl('Restaurant', risky.newyork$type), "Restaurants",
                                                                    ifelse(grepl('Cafe', risky.newyork$type), "Restaurants",
                                                                           ifelse(grepl('Gym', risky.newyork$type), "Gyms",
                                                                                  ifelse(grepl('Health', risky.newyork$type), "Gyms",
                                                                                         ifelse(grepl('Bath', risky.newyork$type), "Bath Houses",
                                                                                                ifelse(grepl('Business', risky.newyork$type), "Businesses",
                                                                                                       ifelse(grepl('Travel', risky.newyork$type), "Businesses",
                                                                                                              ifelse(grepl('Tour', risky.newyork$type), "Businesses",
                                                                                                                     ifelse(grepl('Shop', risky.newyork$type), "Businesses",
                                                                                                                            ifelse(grepl('Erotica', risky.newyork$type), "Businesses",
                                                                                                                                   ifelse(grepl('Book', risky.newyork$type), "Businesses",
                                                                                                                                          ifelse(grepl('Theat', risky.newyork$type), "Businesses",
                                                                                                                                                 ifelse(grepl('Service', risky.newyork$type), "Businesses",
                                                                                                                                                        ifelse(grepl('Community', risky.newyork$type), "Organizations",
                                                                                                                                                               ifelse(grepl('Publication', risky.newyork$type), "Organizations",
                                                                                                                                                                      ifelse(grepl('Hotline', risky.newyork$type), "Organizations",
                                                                                                                                                                             ifelse(grepl('Men', risky.newyork$type), "Organizations",
                                                                                                                                                                                    ifelse(grepl('Organization', risky.newyork$type), "Organizations",
                                                                                                                                                                                           ifelse(grepl('Cruis', risky.newyork$type), "Cruising Areas", "NA")))))))))))))))))))))))

write.csv(risky.newyork, file = "~/Risky-New-York/gay guides data/risky.newyork.csv")
