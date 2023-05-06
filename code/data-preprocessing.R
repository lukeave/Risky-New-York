###DATA PREPROCESSING

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


#delete all columns in 1990-1996 data that are irrelevant for analysis to ensure that data frames have the same variables throughout
gayguides1990to1996 <- gayguides1990to1996[,-13]
gayguides1990to1996 <- gayguides1990to1996[,-10]

#again, change name of column 1 in 1990-1996 data to ensure consistency across data frames
colnames(gayguides1990to1996)[1] <- "ID"

#use the unclear_address and streetaddress columns in 1990-1996 to filter out unclear entries
gayguides1990to1996 <- gayguides1990to1996 %>% 
  filter(unclear_address != "checked") %>% 
  filter(streetaddress != "")
  
#now that unclear addresses are filtered out, remove unclear_address column in 1990-1996
gayguides1990to1996 <- gayguides1990to1996[,-9]

#before merging data sets, geocode 1990-1996
library(tidyverse)
library(ggmap)

# paste together the street address, city and state in order to ensure we use full addresses for geocoding. Will minimize mistakes caused by common streetnames. 
gayguides1990to1996$full.address <- paste(gayguides1990to1996$streetaddress, ", ", gayguides1990to1996$city, ", ", gayguides1990to1996$state, sep="") 

# Register the google api code for the georeferencing service.
register_google(key = Sys.getenv("MGG_GOOGLE_KEY"))

# Loop through the addresses to get the latitude and longitude of each address and add it to the origAddress data frame in new columns lat and lon
for(i in 1:nrow(gayguides1990to1996)) {
  # Print("Working...")
  result <- tryCatch(geocode(gayguides1990to1996$full.address[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
  gayguides1990to1996$lon[i] <- as.numeric(result[1])
  gayguides1990to1996$lat[i] <- as.numeric(result[2])
  gayguides1990to1996$geoAddress[i] <- as.character(result[3])
}

rm(result)
rm(i)

#save geocoded 1990-1996 data as a csv and look for off coordinates (just a check)
write.csv(gayguides1990to1996, file = "gayguides1990to1996.csv")

gayguides1990to1996 <- read.csv("~/Risky-New-York/gay guides data/gayguides1990to1996.csv")

#load other libraries after geocoding
library(ggplot2)
library(DigitalMethodsData)
library(tidygeocoder)
library(tidyr)
library(dplyr)
library(igraph)
library(networkD3)
library(ggraph)
library(leaflet)
library(leaftime)
library(geojsonR)

#remove NAs from 1990-1996
gayguides1990to1996 <- gayguides1990to1996 %>% 
  filter(geoAddress != "NA")

#change class of lat column in 1965-1989 data to numeric - for some reason it is character, and it has an entry with a character value
gayguides1965to1989$lat <- as.numeric(gayguides1965to1989$lat)

#remove NAs from 1965-1989
gayguides1965to1989 <- gayguides1965to1989 %>% 
  na.exclude()

#remove column full.address and column geoAddress in 1990-1996
gayguides1990to1996 <- gayguides1990to1996[,-14]
gayguides1990to1996 <- gayguides1990to1996[,-11]

#combine 1965-1989 with 1990-1996
gayguides.complete <- rbind(gayguides1965to1989, gayguides1990to1996)
#remove notes column - will not use for analysis
gayguides.complete <- gayguides.complete[, -10]

write.csv(gayguides.complete, file = "~/Risky-New-York/gay guides data/gayguides.complete.csv")

rm(gayguides1965to1989)
rm(gayguides1990to1996)

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

#based on the new hierarchy of types, collapse types into analytic categories in gayguides.complete. test it out before running it

##account for typos in cat
gayguides.complete <- gayguides.complete %>% mutate(type = fct_collapse(type, "Accommodations" = c("Accommodations", "Accomodations")))

gayguides.complete <- gayguides.complete %>% mutate(type = fct_collapse(type, "Organizations" = c("Organization", "Organizations")))

gayguides.complete <- gayguides.complete %>% mutate(type = fct_collapse(type, "Religious Institutions" = c("Religious Institution", "Religious Organizations", "Religious Organization")))


##collapse types in gayguides.complete
gayguides.complete$categories <- ifelse(grepl('Religious', gayguides.complete$type), "Religious Institutions",
                                 ifelse(grepl('Hotel', gayguides.complete$type), "Accommodations",
                                        ifelse(grepl('Accommodation', gayguides.complete$type), "Accommodations",
                                               ifelse(grepl('Bar', gayguides.complete$type), "Bars",
                                                      ifelse(grepl('Restaurant', gayguides.complete$type), "Restaurants",
                                                             ifelse(grepl('Cafe', gayguides.complete$type), "Restaurants",
                                                                    ifelse(grepl('Gym', gayguides.complete$type), "Gyms",
                                                                           ifelse(grepl('Health', gayguides.complete$type), "Gyms",
                                                                                  ifelse(grepl('Bath', gayguides.complete$type), "Bath Houses",
                                                                                         ifelse(grepl('Business', gayguides.complete$type), "Businesses",
                                                                                                ifelse(grepl('Travel', gayguides.complete$type), "Businesses",
                                                                                                       ifelse(grepl('Tour', gayguides.complete$type), "Businesses",
                                                                                                              ifelse(grepl('Shop', gayguides.complete$type), "Businesses",
                                                                                                                     ifelse(grepl('Erotica', gayguides.complete$type), "Businesses",
                                                                                                                            ifelse(grepl('Book', gayguides.complete$type), "Businesses",
                                                                                                                                   ifelse(grepl('Theat', gayguides.complete$type), "Businesses",
                                                                                                                                          ifelse(grepl('Service', gayguides.complete$type), "Businesses",
                                                                                                                                                 ifelse(grepl('Community', gayguides.complete$type), "Organizations",
                                                                                                                                                        ifelse(grepl('Publication', gayguides.complete$type), "Organizations",
                                                                                                                                                               ifelse(grepl('Hotline', gayguides.complete$type), "Organizations",
                                                                                                                                                                      ifelse(grepl('Men', gayguides.complete$type), "Organizations",
                                                                                                                                                                             ifelse(grepl('Organization', gayguides.complete$type), "Organizations",
                                                                                                                                                                                    ifelse(grepl('Cruis', gayguides.complete$type), "Cruising Areas", "NA")))))))))))))))))))))))
#count new categories per year
categories.count <- gayguides.complete %>% 
  group_by(categories, Year) %>% 
  summarize(count = n())

write.csv(categories.count, file = "~/Risky-New-York/gay guides data/categories.count.csv")

##remove NAs from gayguides.complete$categories
gayguides.complete <- gayguides.complete %>% 
  filter(categories != "NA")

#create a new column to denote "cruisy" factor for each location in gayguides.complete. 
#this can enable analysis of correlation between risk and cruising areas.

##cruisy gayguides.complete
gayguides.complete <- gayguides.complete %>%
  mutate(cruisy = ifelse(grepl('cruis', description), "TRUE",
                         ifelse(grepl('Cruis', description), "TRUE", 
                                ifelse(grepl('Cruis', amenityfeatures), "TRUE",
                                       ifelse(grepl('Cruis', type), "TRUE", "FALSE")))))


###NEW YORK SUBSET DATA

#subset data to NY locations only
gaynewyork <- gayguides.complete %>% 
  filter(state == "NY")

write.csv(gaynewyork, file = "~/Risky-New-York/gay guides data/gaynewyork.csv")

#get NY counties
state.newyork.map <- map_data('county', region = "new york")

nyc.counties.map <- state.newyork.map %>% 
  filter(subregion == "queens" | subregion == "bronx" | subregion == "richmond" | subregion == "kings" | subregion == "new york")

#THIS WON'T STAY HERE - IT GOES IN THE RMD FILE
ggplot() +
  geom_map(data = state.newyork.map, map = state.newyork.map, aes(x = long, y = lat, map_id= region), fill = "pink", linewidth = 0.5, color="black") +
  geom_point(data = gaynewyork, aes(x=lon, y=lat), size = 3, shape = 23, fill = "orange")

#count unique NY analytic categories (new collapsed types)
ny.categories.count <- gaynewyork %>% 
  group_by(categories, Year) %>% 
  summarize(count = n())

write.csv(ny.categories.count, file = "~/Risky-New-York/gay guides data/ny.categories.count.csv")

#subset gaynewyork to visualize the different categorizations of the Ramrod Bar over time
ramrod.vis <- gaynewyork %>% 
  filter(grepl('394 West', streetaddress)) %>% 
  select(description, Year, amenityfeatures) %>% 
  na.exclude()

ramrod.vis <- ramrod.vis[, c(2, 3, 1)]
