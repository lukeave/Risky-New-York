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
