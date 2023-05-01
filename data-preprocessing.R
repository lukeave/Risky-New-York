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

#keep a copy of the 1965-1985 gay guides data that was previously geocoded for potential exploratory analysis.
data("gayguides")
geocoded.gg.1965to1985 <- gayguides
rm(gayguides)

#subset data to NY locations only
gaynewyork <- gayguides.complete %>% 
  filter(state == "NY")

#create a geocoded version of the NY data
gaynewyork$full.address <- paste(gaynewyork$streetaddress,  gaynewyork$city, gaynewyork$state, sep=", ")

geo.gaynewyork <- gaynewyork %>% 
  mutate_if(is.character, trimws)

# paste together the street address, city and state in order to ensure we use full addresses for geocoding. Will minimize mistakes caused by common streetnames. 
geo.gaynewyork$full.address <- paste(gaynewyork$streetaddress, ", ", gaynewyork$city, ", ", gaynewyork$state, sep="") 

geo.gaynewyork <- geo.gaynewyork %>% 
  filter(unclear_address != "checked" | unclear_address != "Location could not be verified. General coordinates used")

# Register the google api code for the georeferencing service.
register_google(key = Sys.getenv("MGG_GOOGLE_KEY"))

# Loop through the addresses to get the latitude and longitude of each address and add it to the origAddress data frame in new columns lat and lon
for(i in 1:nrow(geo.gaynewyork)) {
  # Print("Working...")
  result <- tryCatch(geocode(gaynewyork$full.address[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
  geo.gaynewyork$lon[i] <- as.numeric(result[1])
  geo.gaynewyork$lat[i] <- as.numeric(result[2])
  geo.gaynewyork$geoAddress[i] <- as.character(result[3])
}

write.csv(geo.gaynewyork, file = "geo.gaynewyork.csv")

#get NY counties
state.newyork.map <- map_data('county', region = "new york")

nyc.counties <- state.newyork.map %>% 
  filter(subregion == "queens" | subregion == "bronx" | subregion == "richmond" | subregion == "kings" | subregion == "new york")

ggplot() +
  geom_map(data = state.newyork.map, map = state.newyork.map, aes(x = long, y = lat, map_id= region), fill = "pink", linewidth = 0.5, color="black") #+
  geom_point(data = geo.gaynewyork, aes(x=longitude, y=latitude), size = 3, shape = 23, fill = "orange")


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
#add full address column to gayguides.complete
gayguides.complete$full.address <- 
  paste(gayguides.complete$streetaddress,  gayguides.complete$city, gayguides.complete$state, sep=", ")
#geocode subset of gay guides data to 1965 to 1970 (first visualization)
##output is stored as gayguides.1965to1970.csv
gayguides.1965to1970 <- gayguides.complete %>% 
filter(state != 'VI' & state != 'PR' & state != 'GU' & state != 'HI') %>%
filter(Year >= 1965 & Year <=1970) %>% 
filter(unclear_address != "checked") %>% 
geocode(address = full.address, method='osm', lat = latitude, long = longitude)

#subset gaynewyork to visualize the different categorizations of the Ramrod Bar over time
ramrod.vis <- gaynewyork %>% 
  filter(grepl('394 West', streetaddress)) %>% 
  select(description, Year, amenityfeatures) %>% 
  na.exclude()

ramrod.vis <- ramrod.vis[, c(2, 3, 1)]
