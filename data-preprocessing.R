#load data from 1965 to 1985
data(gayguides)
gayguides1965to1985 <- gayguides
rm(gayguides)

#load data from 1986 to 1996
gayguides1986 <- read.csv("~/Risky-New-York/gay guides data/gayguides1986.csv")
gayguides1987 <- read.csv("~/Risky-New-York/gay guides data/gayguides1987.csv")
gayguides1988 <- read.csv("~/Risky-New-York/gay guides data/gayguides1988.csv")
gayguides1989 <- read.csv("~/Risky-New-York/gay guides data/gayguides1989.csv")
gayguides1990 <- read.csv("~/Risky-New-York/gay guides data/gayguides1990.csv")
gayguides1991 <- read.csv("~/Risky-New-York/gay guides data/gayguides1991.csv")
gayguides1992 <- read.csv("~/Risky-New-York/gay guides data/gayguides1992.csv")
gayguides1993 <- read.csv("~/Risky-New-York/gay guides data/gayguides1993.csv")
gayguides1994 <- read.csv("~/Risky-New-York/gay guides data/gayguides1994.csv")
gayguides1995 <- read.csv("~/Risky-New-York/gay guides data/gayguides1995.csv")
gayguides1996 <- read.csv("~/Risky-New-York/gay guides data/gayguides1996.csv")

#delete column "X" in 1965-1985 data that does not exist in other data frames
gayguides1965to1985 <- gayguides1965to1985[,-1]

#change name of column 13 in 1965-1985 data to match column name in other data frames
colnames(gayguides1965to1985)[13] <- "unclear_address"

#delete all other columns in 1965-1985 data that are irrelevant for analysis to ensure that data frames have the same variables throughout
gayguides1965to1985 <- gayguides1965to1985[,-11]
gayguides1965to1985 <- gayguides1965to1985[,-10]
gayguides1965to1985 <- gayguides1965to1985[,-10]

#combine all data frames from 1986 to 1996 into a single data frame
gayguides1986to1996 <- rbind(gayguides1986, gayguides1987, gayguides1988, gayguides1989, gayguides1988, gayguides1989, gayguides1990, gayguides1991, gayguides1992, gayguides1993, gayguides1994, gayguides1995, gayguides1996)

#delete all columns in 1986-1996 data that are irrelevant for analysis to ensure that data frames have the same variables throughout
gayguides1986to1996 <- gayguides1986to1996[,-13]
gayguides1986to1996 <- gayguides1986to1996[,-11]
gayguides1986to1996 <- gayguides1986to1996[,-10]

#again, change name of column 1 in 1986-1996 data to ensure consistency across data frames
colnames(gayguides1986to1996)[1] <- "ID"

#combine 1965-1985 with 1986-1996
gayguides.complete <- rbind(gayguides1986to1996, gayguides1965to1985) %>% 
  na.exclude()

#count types
type.count <- gayguides.complete %>% 
  group_by(type) %>% 
  summarize(count = n())

#subset data to NY locations only
gaynewyork <- gayguides.complete %>% 
  filter(state == "NY")

#replace empty strings with NAs
gaynewyork[gaynewyork == ''] <- NA

##prepare new york types for analysis
#check existing NA types. result: 17 rows
gaynewyork %>% 
  filter(is.na(type)) %>% 
  head()

#account for typos in NY types
gaynewyork <- gaynewyork %>% mutate(type = fct_collapse(type, "Accommodations" = c("Accommodations", "Accomodations")))

gaynewyork <- gaynewyork %>% mutate(type = fct_collapse(type, "Organizations" = c("Organization", "Organizations")))

gaynewyork <- gaynewyork %>% mutate(type = fct_collapse(type, "Religious Institutions" = c("Religious Institution", "Religious Organizations", "Religious Organization")))

#count unique NY types
newyork.types <- gaynewyork %>% 
  group_by(type) %>% 
  summarize(count = n())

#exclude NA types from count
newyork.types <- newyork.types %>% na.exclude()

#create a new column to denote cruisy for each location in NY
gaynewyork <- gaynewyork %>%
  mutate(cruisy = ifelse(grepl('cruis', description), "TRUE",
                         ifelse(grepl('Cruis', description), "TRUE", 
                                ifelse(grepl('Cruis', amenityfeatures), "TRUE",
                                       ifelse(grepl('Cruis', type), "TRUE", "FALSE")))))

#subset the NY data to those locations that were assigned a risk factor
risky.newyork <- gaynewyork %>% 
  filter(grepl('AYOR', amenityfeatures) | grepl('HOT', amenityfeatures) | grepl('locally', description) | grepl('HOT!', description) | grepl('hot)', description))

##Hierarchy of Analytic Categories
## 1. Religious Institutions
## 2. Accommodations
### 2.1. Hotels
## 3. Bars
## 4. Restaurants
### 4.1. Cafes
## 5. Gyms
### 5.1. Health Clubs
## 6. Bath Houses
## 7. Businesses
### 7.1. Travel; Tour
### 7.2. Shops; Erotica; Bookstores
### 7.3. Theaters
### 7.4. Services (including Info Lines)
## 8. Organizations
#### 8.1. Community Centers
#### 8.2. Publications
### 8.3. Hotlines
### 8.4. Men's Club
## 9. Cruising Areas
### 9.1. Cruisy + Cruising

#based on the new hierarchy of types, collapse types into analytic categories
newyork.types$categories <- ifelse(grepl('Religious', newyork.types$type), "Religious Institutions",
                                   ifelse(grepl('Hotel', newyork.types$type), "Accommodations",
                                          ifelse(grepl('Accommodation', newyork.types$type), "Accommodations",
                                                 ifelse(grepl('Bar', newyork.types$type), "Bars",
                                                        ifelse(grepl('Restaurant', newyork.types$type), "Restaurants",
                                                               ifelse(grepl('Cafe', newyork.types$type), "Restaurants",
                                                                      ifelse(grepl('Gym', newyork.types$type), "Gyms",
                                                                             ifelse(grepl('Health', newyork.types$type), "Gyms",
                                                                                    ifelse(grepl('Bath', newyork.types$type), "Bath Houses",
                                                                                           ifelse(grepl('Business', newyork.types$type), "Businesses",
                                                                                                  ifelse(grepl('Travel', newyork.types$type), "Businesses",
                                                                                                         ifelse(grepl('Tour', newyork.types$type), "Businesses",
                                                                                                                ifelse(grepl('Shop', newyork.types$type), "Businesses",
                                                                                                                       ifelse(grepl('Erotica', newyork.types$type), "Businesses",
                                                                                                                              ifelse(grepl('Book', newyork.types$type), "Businesses",
                                                                                                                                     ifelse(grepl('Theat', newyork.types$type), "Businesses",
                                                                                                                                            ifelse(grepl('Service', newyork.types$type), "Businesses",
                                                                                                                                                   ifelse(grepl('Community', newyork.types$type), "Organizations",
                                                                                                                                                          ifelse(grepl('Publication', newyork.types$type), "Organizations",
                                                                                                                                                                 ifelse(grepl('Hotline', newyork.types$type), "Organizations",
                                                                                                                                                                        ifelse(grepl('Men', newyork.types$type), "Organizations",
                                                                                                                                                                               ifelse(grepl('Cruis', newyork.types$type), "Cruising Areas", "NA"))))))))))))))))))))))
