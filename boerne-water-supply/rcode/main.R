###################################################################################################################################################
#
# CODE TO READ IN SENSOR OF THINGS DATA FOR NC WATER SUPPLY DASHBOARD
# CREATED BY LAUREN PATTERSON & KYLE ONDA @ THE INTERNET OF WATER
# FEBRUARY 2021
# Modified November 2021 by Vianey Rueda for Boerne
#
###################################################################################################################################################
#######################################################################################################################################################
#
# DOWNLOADS AND CREATES GEOJSON FILES FOR MAP LAYERS IN THE WATER SUPPLY DASHBOARD
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER. 
# FEBRUARY 2021
# MODIFIED MAY 2021 BY SOPHIA BRYSON FOR TEXAS
#
########################################################################################################################################################

######################################################################################################################################################################
#
#   LOAD LIBRARIES
#
######################################################################################################################################################################
######################################################################################################################################################################
#
#   SET GLOBAL VARIABLES
#
library(mapview)
library(sf)
library(dplyr)
library(lubridate)
library(googlesheets4)
library(readxl)
library(spData)
library(rgdal)
library(leaflet)
library(rmapshaper)
library(geojsonio)
library(jsonlite)
library(rvest)
library(purrr)
library(httr)
library(tidyverse)
library(lubridate)
library(stringr)
library(rnoaa)
library(nhdplusTools)
library(magrittr)
library(ckanr)
library(dataRetrieval)
library(EGRET)
######################################################################################################################################################################
options(scipen=999) #changes scientific notation to numeric
rm(list=ls()) #removes anything stored in memory

#set working directory

# if working on a Windows use this to set working directory...
#source_path = rstudioapi::getActiveDocumentContext()$path 
#setwd(dirname(source_path))
#swd_data <- paste0("..\\data\\")
# if working on a Mac use this to set working directory...
source_path = getwd()
print(source_path)
swd_data <-  paste0("boerne-water-supply/data/")


#state info
stateAbb <- "TX"
stateFips <- 48


#variables used to update data
today = substr(Sys.time(),1,10); today;
current.year <- year(today);

start.date = "1990-01-01"; #set for the start of the period that we want to assess
end.date = paste0(year(today), "-12-31")
end.year = year(Sys.time())

mymonths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"); #used below to convert numbers to abbrev

#save out update date for dashboard
update.date <- paste0(mymonths[month(today)]," ", day(today), ", ", end.year) %>% as.data.frame()
colnames(update.date) <- "today_date"
write.csv(update.date, paste0(swd_data, "update_date.csv"), row.names=FALSE)

#calculate moving average function
ma <- function(x,n=7){stats::filter(x,rep(1/n,n), sides=1)}

#useful functions
`%notin%` = function(x,y) !(x %in% y); #function to get what is not in the list

#standardized reference for setting julian values (drops leap days)
jan1 <- as.Date("2021-01-01")
dec31 <- as.Date("2021-12-31")
julian_index <- c(seq(jan1:dec31), "NA")
all.dates <- seq(as.Date(jan1), as.Date(dec31), by = "days")
day_month <- c(substr(all.dates,6,10), "NA")
day_month_leap <- c(day_month[1:59], "02-29", day_month[60:365])
julian_index_leap <- (1:366)

julian <- as.data.frame(matrix(nrow=366))
julian$day_month <- day_month; julian$julian_index <- julian_index
julian$day_month_leap <- day_month_leap; julian$julian_index_leap <- julian_index_leap
julian.ref <- julian %>% select(!V1)
rm(jan1, dec31, julian_index, all.dates, day_month,day_month_leap, julian_index_leap, julian)







#REFERENCE INFO
#https://gost1.docs.apiary.io/#reference/odata-$filter
#observed properties: https://twsd.internetofwater.dev/api/v1.0/ObservedProperties

######################################################################################################################################################################
#
#   Load Old Data
#
######################################################################################################################################################################
#load in geojson for utilities
utilities <- read_sf(paste0(swd_data, "utility.geojson")); 
pwsid.list <- unique(utilities$pwsid) #Boerne is the utility of interest
mymonths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"); #used below to convert numbers to abbrev
#mapview::mapview(utilities)

#read in old data
old_total_demand <- read.csv(paste0(swd_data, "demand/historic_total_demand.csv"))
old_demand_by_source <- read.csv(paste0(swd_data, "demand/historic_demand_by_source.csv"))
old_reclaimed <- read.csv(paste0(swd_data, "demand/historic_reclaimed_water.csv"))
old_pop <- read.csv(paste0(swd_data, "demand/historic_pop.csv"))

#calculate moving average function
ma <- function(x,n=7){stats::filter(x,rep(1/n,n), sides=1)}


######################################################################################################################################################################
#
# Read in new water demand data
#
#####################################################################################################################################################################
service_account_info <- Sys.getenv("GSHEET_SERVICE_ACCOUNT", "")

# Write the credentials to a temporary JSON file
temp_file <- tempfile(fileext = ".json")
writeLines(service_account_info, temp_file)

# Authenticate using the temporary JSON file
gs4_auth(path = temp_file)
demand_data <- read_sheet("https://docs.google.com/spreadsheets/d/1BKb9Q6UFEBNsGrLZhjdq2kKX5t1GqPFCWF553afUKUg/edit#gid=2030520898", sheet = 1, range = "A229:H", col_names = FALSE,col_types = "Dnnnnnnn")
demand_by_source <- demand_data[, c("...1", "...2", "...3", "...6", "...7", "...8")]

#rename columns
demand_by_mgd <- rename(demand_by_source, date = "...1", groundwater = "...2", boerne_lake = "...3", GBRA = "...6", reclaimed = "...7", total = "...8")

#replace na's with 0s
demand_by_mgd <- as.data.frame(demand_by_mgd)
demand_by_mgd[is.na(demand_by_mgd)] <- 0

demand_by_mgd <- as.data.frame(demand_by_mgd)

#change units to MGD
demand_by_mgd$groundwater <- demand_by_mgd$groundwater/1000; demand_by_mgd$boerne_lake <- demand_by_mgd$boerne_lake/1000; demand_by_mgd$GBRA <- demand_by_mgd$GBRA/1000; demand_by_mgd$reclaimed <- demand_by_mgd$reclaimed/1000; demand_by_mgd$total <- demand_by_mgd$total/1000; 

#include PWSId
demand_by_mgd$pwsid <- utilities$pwsid

#add julian indexing
#nx <- demand_by_mgd %>% mutate(year = year(date), month = month(date), day = day(date))
nx <- demand_by_mgd %>% mutate(year = year(date), day_month = substr(date, 6, 10))

for(i in 1:nrow(nx)) { #computationally slow. There's almost certainly a faster way. But it works. 
  
  if(leap_year(nx$year[i]) == TRUE) {nx$julian[i] <- julian.ref$julian_index_leap[julian.ref$day_month_leap == nx$day_month[i]]}
  if(leap_year(nx$year[i]) == FALSE) {nx$julian[i] <- julian.ref$julian_index[julian.ref$day_month == nx$day_month[i]]}
  
  print(paste(round(i/nrow(nx)*100,2),"% complete"))
}

demand_by_mgd <- nx
#split date by month and day
demand_by_mgd = demand_by_mgd %>% 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date))

demand_by_mgd$day <- as.numeric(demand_by_mgd$day)

str(demand_by_mgd)



new_demand_by_mgd <- demand_by_mgd %>% filter(year >= 2022 & date < today)
new_demand_by_mgd$date <- format(as.Date(new_demand_by_mgd$date), "%Y-%m-%d") # make sure the date format is the same for old and new before binding

#remove days that don't have data (utilities director includes 0's for days he hasn't input data yet)
new_demand_by_mgd <- filter(new_demand_by_mgd, groundwater > 0, boerne_lake > 0, GBRA > 0, reclaimed > 0)

#merge old and new data
all_demand_by_mgd <- rbind(old_demand_by_source, new_demand_by_mgd) 

check.last.date <- all_demand_by_mgd %>% filter(date == max(date)) %>% dplyr::select(date, month)
table(check.last.date$date)

#write.csv
write.csv(demand_by_mgd, paste0(swd_data, "demand/all_demand_by_source.csv"), row.names=FALSE)


#include month abbreviations
demand2 <- all_demand_by_mgd %>% group_by(pwsid) %>% mutate(julian = as.numeric(strftime(date, format = "%j")), month = month(date), monthAbb = mymonths[month], year = year(date))

#calculate mean demand
demand2 <- all_demand_by_mgd %>% mutate(date = as.Date(substr(date,1,10),format='%Y-%m-%d')) 
demand3 <- demand2 %>% group_by(pwsid) %>% arrange(date) %>% mutate(timeDays = as.numeric(date - lag(date)))
demand4 <- demand3 %>% group_by(pwsid) %>% mutate(mean_demand = ifelse(timeDays <= 3, round(as.numeric(ma(total)),2), total), 
                                                  julian = as.numeric(strftime(date, format = "%j")), month = month(date), monthAbb = mymonths[month], year = year(date))
demand5 <- demand4 %>% mutate(total = round(total,2), mean_demand = ifelse(is.na(mean_demand)==TRUE, total, mean_demand))

#calculate monthly peak
demand6 <- demand5 %>% group_by(pwsid, month, year) %>% mutate(peak_demand = round(quantile(total, 0.98),1)); #took the 98% to omit outliers

#provide julian date
demand7 <- demand6 %>% mutate(date2 = date, date = paste0(monthAbb,"-",day(date2))) %>% select(-timeDays)

#clean up 
demand7 <- rename(demand7, demand_mgd = "total")
demand7 <- demand7[, c("pwsid", "date","demand_mgd", "mean_demand", "julian", "month", "monthAbb", "year", "peak_demand", "date2")]

#write.csv
write.csv(demand7, paste0(swd_data, "demand/all_total_demand.csv"), row.names=FALSE)


#create comulative demand
demand.data <- demand7 %>% filter(date2>start.date)
foo.count <- demand.data %>% group_by(pwsid, year) %>% count() %>% filter(year < current.year & n>340 | year == current.year) %>% mutate(idyr = paste0(pwsid,"-",year)) 
foo.cum <- demand.data %>% mutate(idyr = paste0(pwsid,"-",year)) %>% filter(idyr %in% foo.count$idyr) %>% arrange(pwsid, year, month, date2)
foo.cum <- foo.cum %>% distinct() %>% filter(year>=2000); #shorten for this file

foo.cum2 <- foo.cum %>% arrange(pwsid, year, julian) %>% dplyr::select(pwsid, year, date, julian, demand_mgd) %>% distinct() %>% 
  group_by(pwsid, year) %>% mutate(demand_mgd2 = ifelse(is.na(demand_mgd), 0, demand_mgd)) %>%  mutate(cum_demand = cumsum(demand_mgd2)) %>% dplyr::select(-demand_mgd, -demand_mgd2) %>% rename(demand_mgd = cum_demand) %>% distinct()

table(foo.cum$pwsid, foo.cum$year)
#in case duplicate days - take average
foo.cum3 <- foo.cum2 %>% group_by(pwsid, year, julian, date) %>% summarize(demand_mgd = round(mean(demand_mgd, na.rm=TRUE),2), .groups="drop") %>% distinct()

write.csv(foo.cum3, paste0(swd_data, "demand/all_demand_cum.csv"), row.names=FALSE)


######################################################################################################################################################################
#
# Reclaimed water data
#
#####################################################################################################################################################################
new_reclaimed <- subset(new_demand_by_mgd, select = -c(total,groundwater,boerne_lake,GBRA))

all_reclaimed <- rbind(old_reclaimed, new_reclaimed)

#include month abbreviations
all_reclaimed2 <- all_reclaimed %>% group_by(pwsid) %>% mutate(julian = as.numeric(strftime(date, format = "%j")), month = month(date), monthAbb = mymonths[month], year = year(date))

#calculate mean demand
all_reclaimed2 <- all_reclaimed2 %>% mutate(date = as.Date(substr(date,1,10),format='%Y-%m-%d')) 
all_reclaimed3 <- all_reclaimed2 %>% group_by(pwsid) %>% arrange(date) %>% mutate(timeDays = as.numeric(date - lag(date)))
all_reclaimed4 <- all_reclaimed3 %>% group_by(pwsid) %>% mutate(mean_reclaimed = ifelse(timeDays <= 3, round(as.numeric(ma(reclaimed)),2), reclaimed), 
                                                  julian = as.numeric(strftime(date, format = "%j")), month = month(date), monthAbb = mymonths[month], year = year(date))
all_reclaimed5 <- all_reclaimed4 %>% mutate(reclaimed = round(reclaimed,2), mean_reclaimed = ifelse(is.na(mean_reclaimed)==TRUE, reclaimed, mean_reclaimed))

#calculate monthly peak
all_reclaimed6 <- all_reclaimed5 %>% group_by(pwsid, month, year) %>% mutate(peak_reclaimed = round(quantile(reclaimed, 0.98),1)); #took the 98% to omit outliers

#provide julian date
all_reclaimed7 <- all_reclaimed6 %>% mutate(date2 = date, date = paste0(monthAbb,"-",day(date2))) %>% select(-timeDays)

#write.csv
all_reclaimed8 <- subset(all_reclaimed7, select = c(pwsid, date, reclaimed, mean_reclaimed, julian, month, monthAbb, year, peak_reclaimed, date2))
write.csv(all_reclaimed8, paste0(swd_data, "demand/all_reclaimed_water.csv"), row.names=FALSE)

#calculate percent of total
all_reclaimed9 <- all_reclaimed8
all_reclaimed9$total <- all_demand_by_mgd$total
all_reclaimed9$percent_of_total <- (all_reclaimed9$reclaimed/all_reclaimed9$total)*100

#write.csv
write.csv(all_reclaimed9, paste0(swd_data, "demand/all_reclaimed_percent_of_total.csv"), row.names=FALSE)


######################################################################################################################################################################
#
# Read in new pop data
#
#####################################################################################################################################################################
all_city_data <- read_sheet("https://docs.google.com/spreadsheets/d/1BKb9Q6UFEBNsGrLZhjdq2kKX5t1GqPFCWF553afUKUg/edit#gid=2030520898", sheet = 1, range = "A4245:K", col_names = FALSE)

#filter for pop data only
all_pop_data <- all_city_data[,c("...1", "...10", "...11")]

#rename columns
pop_data <- rename(all_pop_data, date = "...1", clb_pop = "...10", wsb_pop = "...11")
pop_data <- as.data.frame(pop_data)

#remove na's
pop_data <- na.omit(pop_data)

#add julian indexing
nxx <- pop_data %>% mutate(year = year(date), day_month = substr(date, 6, 10))

for(i in 1:nrow(nxx)) { #computationally slow. There's almost certainly a faster way. But it works. 
  
  if(leap_year(nxx$year[i]) == TRUE) {nxx$julian[i] <- julian.ref$julian_index_leap[julian.ref$day_month_leap == nxx$day_month[i]]}
  if(leap_year(nxx$year[i]) == FALSE) {nxx$julian[i] <- julian.ref$julian_index[julian.ref$day_month == nxx$day_month[i]]}
  
  print(paste(round(i/nrow(nxx)*100,2),"% complete"))
}

pop_data <- nxx


#split date by month and day
pop_data = pop_data %>% 
  mutate(date = ymd(date),
         year = year(date),
         month = month(date),
         day = day(date))


pop_data$day <- as.numeric(pop_data$day)




#include pwsid
pop_data$pwsid <- "TX300001"

new_pop_data <- pop_data %>% filter(year >= 2022)

# merge old and new pop data
all_pop_data <- rbind(old_pop, new_pop_data)

#write.csv
write.csv(pop_data, paste0(swd_data, "demand/all_pop.csv"), row.names=FALSE)

################################################################################################################################################################
# remove all except for global environment 
rm(list= ls()[!(ls() %in% c('julian.ref','update.date', 'current.month', 'current.year', 'end.date', 'end.year', 
                            'mymonths', 'source_path', 'start.date', 'state_fips', 'stateAbb', 'stateFips', 'swd_data', 'today', 
                            '%notin%', 'ma'))])

###################################################################################################################################################
#
# Creates initial map layers and downloads historic data for the dashboard
# CREATED BY LAUREN PATTERSON & KYLE ONDA @ THE INTERNET OF WATER
# FEBRUARY 2021
# Run anytime... change county.list if desire. 
# Updated July 2021 by Sophia Bryson for TX. 
# Modified November 2021 by Vianey Rueda for Boerne
#
###################################################################################################################################################


######################################################################################################################################################################
#
#   READ IN GROUNDWATER SITES AND GROUNDWATER DATA
#
######################################################################################################################################################################
boerne.sites <- read.csv(paste0(swd_data, "gw/well_metadata.csv"))
old.data <- read.csv(paste0(swd_data, "gw/historic_gw_depth.csv")) %>% mutate(date = as.Date(date, format="%Y-%m-%d"))

#double-check that each column is the desired type (numeric, character, etc.) and make necessary changes
str(old.data) # site = chr; date = Date, format; julian = int; depth_ft = num
old.data$site <- as.character(old.data$site)
str(old.data)

######################################################################################################################################################################
#
# PULL IN GW LEVEL DATA DYNAMICALLY
#
#####################################################################################################################################################################
startDate <- max(old.data$date) + 1
endDate <- as.Date(today)

#authenticate account
service_account_info <- Sys.getenv("GSHEET_SERVICE_ACCOUNT", "")

# Write the credentials to a temporary JSON file
temp_file <- tempfile(fileext = ".json")
writeLines(service_account_info, temp_file)

# Authenticate using the temporary JSON file
gs4_auth(path = temp_file)
# number of sheets to be imported
sheet.number <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                  11, 12, 13, 14, 15, 16, 17, 18, 19, 20,
                  21, 22, 23, 24, 25, 26, 27, 28, 29, 30,
                  31, 32, 33, 34, 35, 36, 37, 38, 39, 40,
                  41, 42) #this is the part that changes

# create empty storage dfs
all.well.metadata <- matrix(nrow = 0, ncol = 26) %>% as.data.frame()
colnames(all.well.metadata) <- c("...1", "...2", "...3", "...4", "...5", "...6", "...7", "...8", "...9", "...10",                  
                                 "...11", "...12", "...13", "...14", "...15", "...16", "...17", "...18", "...19", "...20",                 
                                 "...21", "...22", "...23", "...24", "Long_Va", "Lat_Va")                


all.well.data <- matrix(nrow = 0, ncol = 4) %>% as.data.frame()
colnames(all.well.data) <- c("State_Numer", "...1", "...2", "...3")

# loop through sites and pull data
for(i in 1:length(sheet.number)) {
  gw.i.metadata <- read_sheet("https://docs.google.com/spreadsheets/d/1QoaOhrpz6vrSMBc0yc5-i7nhwj2lmsBHZFYOBJc0KVU/edit#gid=1522547605", sheet = sheet.number[i], range = "A2:X3", col_names = FALSE)
  gw.i.data <- read_sheet("https://docs.google.com/spreadsheets/d/1QoaOhrpz6vrSMBc0yc5-i7nhwj2lmsBHZFYOBJc0KVU/edit#gid=1522547605", sheet = sheet.number[i], range = "A6:C", col_names = FALSE)
  gw.i.metadata$Long_Va <- gw.i.metadata [2,2]
  gw.i.metadata$Lat_Va <- gw.i.metadata [2,3]
  gw.i.metadata <- gw.i.metadata[-c(2), ]
  gw.i.metadata$Long_Va <- unlist(gw.i.metadata$Long_Va)
  gw.i.metadata$Lat_Va <- unlist(gw.i.metadata$Lat_Va)
  gw.i.data$State_Number <- gw.i.metadata [1,15]
  
  # Now bind it up to save out
  all.well.metadata <- rbind(all.well.metadata, gw.i.metadata)
  all.well.data <- rbind(all.well.data, gw.i.data)
  
  # Keep an eye on the progress:
  print(paste0("Completed pull for ", sheet.number[i], ". ", round(i*100/length(sheet.number), 2), "% complete."))
}


#clean up data
  #rename columns
  boerne_all_gw_levels <- rename(all.well.data, site = State_Number, date = "...1", depth_ft = "...2", elevation_at_waterlevel = "...3")
  colnames(boerne_all_gw_levels)
  
  #double-check that each column is the desired type (numeric, character, etc.) and make necessary changes
  str(boerne_all_gw_levels)
  boerne_all_gw_levels$site <- unlist(boerne_all_gw_levels$site)
  boerne_all_gw_levels$site <- as.character(boerne_all_gw_levels$site)
  boerne_all_gw_levels$date <- format(as.Date(boerne_all_gw_levels$date), "%Y-%m-%d")
  boerne_all_gw_levels <- as.data.frame(boerne_all_gw_levels)
  
  #add julian indexing
  nx <- boerne_all_gw_levels %>% mutate(year = year(date), day_month = substr(date, 6, 10))
  
  for(i in 1:nrow(nx)) { #computationally slow. There's almost certainly a faster way. But it works. 
  
    if(leap_year(nx$year[i]) == TRUE) {nx$julian[i] <- julian.ref$julian_index_leap[julian.ref$day_month_leap == nx$day_month[i]]}
    if(leap_year(nx$year[i]) == FALSE) {nx$julian[i] <- julian.ref$julian_index[julian.ref$day_month == nx$day_month[i]]}
  
    print(paste(round(i/nrow(nx)*100,2),"% complete"))
  }

  boerne_all_gw_levels <- nx

  boerne_all_gw_levels$agency <- "CCGCD" #include agency that collects data
  
  #remove missing data
  boerne_all_gw_levels <- na.omit(boerne_all_gw_levels)

  # filter data starting in 2022
  new_boerne_gw_levels <- boerne_all_gw_levels %>% filter(year >= 2022)
  new_boerne_gw_depth <- select(new_boerne_gw_levels, c(1, 2, 4, 7))

  check.last.date <- new_boerne_gw_depth %>% filter(date == max(date)) %>% dplyr::select(date)
  table(check.last.date$date)

#combine old and new
all_boerne_gw_depth <- rbind(old.data, new_boerne_gw_depth) %>% arrange(site, date)

#double-check that each column is the desired type (numeric, character, etc.) and make necessary changes
str(all_boerne_gw_depth) # site = chr; date = Date, format; julian = int; depth_ft = num
all_boerne_gw_depth$julian <- as.integer(all_boerne_gw_depth$julian)

write.csv(all_boerne_gw_depth, paste0(swd_data, "gw/all_gw_depth.csv"), row.names=FALSE)

#####################################################################################################################################################################

#given sparse daily data, aggregate to monthly and get average
year.flow <- all_boerne_gw_depth
year.flow$date2 <- floor_date(year.flow$date, "month")
year.flow2 <- year.flow %>% group_by(site, date2) %>% summarize(mean_depth_ft = mean(depth_ft))
year.flow2 <- year.flow2 %>% mutate(month = substr(date2, 6, 7))
year.flow <- year.flow2
year.flow$date2 <- as.character(year.flow$date2)
year.flow <- rename(year.flow, date = date2)

  #re-add julian indexing
    nx <- year.flow %>% mutate(year = year(date), day_month = substr(date, 6, 10))

    for(i in 1:nrow(nx)) { #computationally slow. There's almost certainly a faster way. But it works. 
  
     if(leap_year(nx$year[i]) == TRUE) {nx$julian[i] <- julian.ref$julian_index_leap[julian.ref$day_month_leap == nx$day_month[i]]}
      if(leap_year(nx$year[i]) == FALSE) {nx$julian[i] <- julian.ref$julian_index[julian.ref$day_month == nx$day_month[i]]}
  
     print(paste(round(i/nrow(nx)*100,2),"% complete"))
    }

    year.flow <- nx

#fix variable type to save out monthly averages
year.flow$month <- as.numeric(year.flow$month)
all_boerne_monthly_avg <- select(year.flow, c(1, 2, 3, 4, 5, 7))   
write.csv(all_boerne_monthly_avg, paste0(swd_data, "gw/all_monthly_avg.csv"), row.names=FALSE)


#stats calculations of daily data:
year.flow <- all_boerne_gw_depth  

#account for any duplicates
#  year.flow <- year.flow %>% group_by(site, date, julian) %>% summarize(depth_ft = median(depth_ft, na.rm=TRUE), .groups="drop")

stats <- as.data.frame(matrix(nrow=0,ncol=13));        colnames(stats) <- c("site", "julian", "min", "flow10", "flow25", "flow50", "flow75", "flow90", "max", "Nobs","startYr","endYr","date"); 

#Calculate stats for all data... this takes a long while to do in tidyverse... like to see it making progress in loop
unique.sites <- unique(year.flow$site)
for (i in 1:length(unique.sites)){
  zt <- year.flow %>% filter(site==unique.sites[i]) %>% mutate(year = year(date)) %>% filter(is.na(depth_ft)==FALSE)
  zt.stats <- zt %>% group_by(site, julian) %>% summarize(Nobs = n(), min=round(min(depth_ft, na.rm=TRUE),4), flow10 = round(quantile(depth_ft, 0.10, na.rm=TRUE),4), flow25 = round(quantile(depth_ft, 0.25, na.rm=TRUE),4),
                                                          flow50 = round(quantile(depth_ft, 0.5, na.rm=TRUE),4), flow75 = round(quantile(depth_ft, 0.75, na.rm=TRUE),4), flow90 = round(quantile(depth_ft, 0.90, na.rm=TRUE),4), 
                                                          max = round(max(depth_ft, na.rm=TRUE),4), .groups="drop")
  
  zt.stats <- zt.stats %>%  mutate(startYr = min(zt$year), endYr = max(zt$year)) %>% dplyr::select(site, julian, min, flow10, flow25, flow50, flow75, flow90, max, Nobs, startYr, endYr)
  zt.stats$date2 <- as.Date(zt.stats$julian, origin=paste0(current.year,"-01-01"))
  zt.stats$date <- format(zt.stats$date2, format="%b-%d")
  #if(dim(zt.stats)[1] == 366) {zt.stats$date = julian$month.day366}
  #if(dim(zt.stats)[1] < 366) { 
  #    zt.stats <- merge(zt.stats, julian[,c("julian", "month.day365")], by.x="julian", by.y="julian", all.x=TRUE)   
  #    zt.stats <- zt.stats %>% rename(date = month.day365)
  #} #assumes 365 days... could be wrong
  stats <- rbind(stats, zt.stats)
  print(paste(i, "is ", round(i/length(unique.sites)*100,2), "percent done"))
}
bk.up <- stats;

is.na(stats) <- sapply(stats, is.infinite)
summary(stats)
#stats <- stats %>% mutate(date2 = as.Date(paste0(current.year,"-",date), format="%Y-%b-%d")) %>% as.data.frame()

head(stats) %>% as.data.frame()

#remove sites that have not had new data in last year
if(month(today)>1){
  remove.site <- stats %>% filter(endYr < (current.year-1))  %>% select(site) %>% distinct()
}
######################################################################################################################################################################
#
# CREATE FILES FOR WEBSITE
#
#####################################################################################################################################################################
#Now attach most recent value to stream stats
recent.flow <- year.flow %>% group_by(site) %>% filter(is.na(depth_ft) == FALSE) %>% filter(date == max(date)) 
#skip the following line if in first month
if(month(today)>1){
  recent.flow <- recent.flow %>% filter(julian <= as.POSIXlt(today(), format = "%Y-%m-%d")$yday) %>% filter(site %notin% remove.site$site) #%>% rename(flow = depth_below_surface_ft)
}
current.stat <- merge(recent.flow[,c("site", "julian", "depth_ft")], stats, by.x=c("site","julian"), by.y=c("site","julian"), all.x=TRUE) 

#if else for this year and last years flow... I think flip this for gw
current.stat <- current.stat %>% mutate(status = ifelse(depth_ft <= flow10, "Extremely Wet", ifelse(depth_ft > flow10 & depth_ft <= flow25, "Very Wet", ifelse(depth_ft >= flow25 & depth_ft < flow50, "Moderately Wet", 
                                                                                                                                                               ifelse(depth_ft >= flow50 & depth_ft < flow75, "Moderately Dry", ifelse(depth_ft >= flow75 & depth_ft < flow90, "Very Dry", ifelse(depth_ft >= flow90, "Extremely Dry", "Unknown")))))))
current.stat$status <- ifelse(is.na(current.stat$status), "unknown", current.stat$status)
table(current.stat$status)

#set those that are not collecting data to unknown
max.julian <- current.stat %>% filter(endYr == current.year) %>% summarize(maxJ = max(julian, na.rm=TRUE))
current.stat <- current.stat %>% mutate(status = ifelse(endYr < current.year & julian < 300, "unknown", ifelse(endYr < (current.year-1), "unknown", 
                                                                                                               ifelse(endYr==current.year & julian < (max.julian$maxJ-60), "unknown", status))))
table(current.stat$status, useNA="ifany")

#merge to sites geojson
boerne.sites  <- boerne.sites %>% rename(site = state_id)
boerne.sites2 <- merge(boerne.sites, current.stat[,c("site","status","depth_ft","julian","date","flow50")], by.x="site", by.y="site") %>% distinct()
#convert to sf
boerne.sites2 <- st_as_sf(boerne.sites2, coords = c("dec_long_va", "dec_lat_va"), crs = 4326);
boerne.sites2 <- merge(boerne.sites2 %>% dplyr::select(-date), recent.flow[,c("site","date")], by.x="site", by.y="site", all.x=TRUE)
#Save out
boerne.sites2 <- boerne.sites2 %>% dplyr::select(agency, site, location, elevation, total_depth, aquifer, status, depth_ft, julian, flow50, date, geometry)
boerne.sites2 <- rename(boerne.sites2, AgencyCd = agency, SiteName = location, WellDepth = total_depth, LocalAquiferName = aquifer)
geojson_write(boerne.sites2, file=paste0(swd_data, "gw/all_gw_sites.geojson"))
#mapview::mapview(boerne.sites2)

#plot for fun
#boerne.sites2 <- boerne.sites2 %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", 
#                                                               ifelse(status=="Very Dry", "red", 
#                                                                      ifelse(status=="Moderately Dry", "orange", 
#                                                                             ifelse(status=="Moderately Wet", "cornflowerblue",
#                                                                                    ifelse(status=="Very Wet", "blue", 
#                                                                                           ifelse(status=="Extremely Wet", "navy", "gray")))))))
#leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% 
#  addCircleMarkers(data = boerne.sites2, radius=4, fillOpacity= 0.8, fillColor = boerne.sites2$colorStatus, color="black", weight=0) 


#Now clip time series data to past two years and assign a depth based on stats
year.flow2 <- year.flow %>% filter(date >= as.Date(paste0((current.year-2),"-01-01"), "%Y-%m-%d"))#limits data to be only for past two years
stats2 <- merge(year.flow2[,c("site", "julian", "date", "depth_ft")], stats %>% dplyr::select(-date), by.x=c("site","julian"), by.y=c("site", "julian"), all.x=TRUE) %>% arrange(site, date)

stats2 <- stats2 %>% mutate(status = ifelse(depth_ft <= flow10, "Extremely Wet", ifelse(depth_ft > flow10 & depth_ft <= flow25, "Very Wet", ifelse(depth_ft >= flow25 & depth_ft < flow50, "Moderately Wet", 
                                                                                                                                                   ifelse(depth_ft >= flow50 & depth_ft < flow75, "Moderately Dry", ifelse(depth_ft >= flow75 & depth_ft < flow90, "Very Dry", ifelse(depth_ft >= flow90, "Extremely Dry", "Unknown")))))))
stats2$status <- ifelse(is.na(stats2$status), "unknown", stats2$status)
table(stats2$status, useNA="ifany")
stats2 <- stats2 %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                                                                                                                                                                         ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray")))))))
stats2 <- stats2 %>% dplyr::select(site, julian, date, depth_ft, status, colorStatus) %>% filter(site %in% boerne.sites$site)
write.csv(stats2, paste0(swd_data, "gw/all_gw_status.csv"), row.names=FALSE)


#set up month names and save out stats file
my.month.name <- Vectorize(function(n) c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec")[n])
recent.flow <- year.flow %>% group_by(site) %>% filter(date >= max(as.Date(paste0(current.year, "-01-01"), '%Y-%m-%d'))) 
stats.merge <- stats %>% mutate(date3 = date2, date2 = date) %>% dplyr::select(-date) %>% filter(site %in% boerne.sites$site)
current.stat2 <- merge(recent.flow, stats.merge, by.x=c("site","julian"), by.y=c("site","julian"), all.y=TRUE)%>% filter(site %in% boerne.sites2$site)

current.stat2 <- current.stat2 %>% mutate(month = my.month.name(as.numeric(substr(date,6,7)))) %>% mutate(date = date2, date2 = date3) %>% dplyr::select(-date3);  #okay to have NA for date because want chart to end there
write.csv(current.stat2, paste0(swd_data, "gw/all_gw_stats.csv"), row.names=FALSE)


#let's do annual trends
gw.annual <- year.flow %>% mutate(year = year(date)) %>% group_by(site, year) %>% summarize(medianDepth = median(depth_ft, na.rm=TRUE), nobsv = n(), .groups="drop") %>% 
  filter(site %in% boerne.sites2$site)
write.csv(gw.annual, paste0(swd_data, "gw/all_gw_annual.csv"), row.names=FALSE)

  
################################################################################################################################################################
# remove all except for global environment 
rm(list= ls()[!(ls() %in% c('julian.ref','update.date', 'current.month', 'current.year', 'end.date', 'end.year', 
                            'mymonths', 'source_path', 'start.date', 'state_fips', 'stateAbb', 'stateFips', 'swd_data', 'today', 
                            '%notin%', 'ma'))])




#######################################################################################################################################################
#
# Updates precipitation data based on historic data collected
# also updates forecast maps
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# Can be updated daily
# FEBRUARY 2021
#
# Sample code for accessing TexMesonet API data for Boerne, TX (Kendall County)
# Created by Vianey Rueda
# Updated by Sophia Bryson @ THE INTERNET OF WATER.
# February 2022
#
########################################################################################################################################################


################################################################################################################################################
#
#                      UPDATE THE DROUGHT MAPS AND CREATE TABLE FOR PERCENT IN DROUGHT BY BASIN
#
################################################################################################################################################
# current drought

download.file("https://droughtmonitor.unl.edu/data/shapefiles_m/USDM_current_M.zip", destfile="temp.zip")

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
unzip("temp.zip", files=NULL, exdir="temp")

#get day
d <- today; 
d <- as.Date(d)
prev.days <- seq(d-7,d,by='day');  
d <- prev.days[weekdays(prev.days)=='Tuesday'][1] 
d <- str_remove_all(d, "[-]");

drought <- readOGR(paste0("temp"), paste0("USDM_",d)) %>% st_as_sf() %>% st_transform(crs = 4326) %>% rename(Name = DM) %>% select(Name, geometry) %>% mutate(Name = as.character(Name))
geojson_write(drought, file = paste0(swd_data,"drought/current_drought.geojson"))

mapview::mapview(drought, zcol = "Name", col.regions = c("lightyellow", "yellow", "orange", "red", "darkred"))

#delete temp files
fold = ("temp")
# get all files in the directories, recursively
f <- list.files(fold, include.dirs = F, full.names = T, recursive = T)
# remove the files
file.remove(f)

#download tables for HUCS of interest 
huc8 <- read_sf(paste0(swd_data, "huc8.geojson"))
#huc8 <- huc8 %>% rename(huc8 = HUC8, state_code = STATE, state_name = STATE, name = NAME.1, census_area = CENSUSAREA, geoid = GEO_ID, lsad = LSAD) #clarify & get names into proper format
huc.list <- huc8$huc8

#loop through the lists to create a data frame... it takes a long time to read through. Pull in original and add to it
old.drought <- read.csv(paste0(swd_data, "drought/percentAreaHUC.csv"), colClasses=c("huc8" = "character")) %>% mutate(date = as.Date(date, "%Y-%m-%d")) 
last.date <- max(old.drought$date)

#reformat for the url 
last.day <- day(last.date)+1; last.month = month(last.date); last.year = year(last.date)
last.date <- paste0(last.month,"/",last.day,"/",last.year)
end.date <- paste0("12/31/", year(today))

#create dataframe and pull new data
drought.time <- as.data.frame(matrix(nrow=0, ncol=9)); colnames(drought.time) <- c("huc8","name","date","none","d0","d1","d2","d3","d4")
for (m in 1:length(huc.list)){
  full_url <-paste0("https://usdmdataservices.unl.edu/api/HUCStatistics/GetDroughtSeverityStatisticsByAreaPercent?aoi=",huc.list[m],"&startdate=",last.date,"&enddate=", end.date, "&statisticsType=1&hucLevel=8")
  api.data <- GET(full_url, timeout(15000)) #use httr libirary to avoid timeout
  df <- content(api.data, "parse")
  
  for (i in 1:length(df)){
    zt.name <- as.character(subset(huc8, huc8==huc.list[m])$name) 
    zt = tibble(
      huc8 = as.character(huc.list[m]),
      name = zt.name,
      date = df[[i]]$ValidStart,
      none = df[[i]]$None,
      d0 = df[[i]]$D0,
      d1 = df[[i]]$D1,
      d2 = df[[i]]$D2,
      d3 = df[[i]]$D3,
      d4 = df[[i]]$D4
    )
    drought.time <- rbind(drought.time, zt)
  }
  #print(zt.name)
  print(paste0(zt.name, ", ", round(m*100/length(huc.list), 2), "% complete"))
}
table(drought.time$huc8)

#TAKES 25 SECONDS TO RUN FOR FULL YEAR... IN FUTURE WILL NEED TO SHORTEN
drought2 <- drought.time %>% mutate(date = as.Date(date, "%Y-%m-%d"), none = as.numeric(none), d0 = as.numeric(d0), d1 = as.numeric(d1), d2 = as.numeric(d2), d3=as.numeric(d3), d4=as.numeric(d4)) %>% arrange(huc8, date)
#it seems that drought is cumulative
drought2 <- drought2 %>% mutate(d4x = d4, d3x = d3 - d4, d2x = d2-d3, d1x = d1-d2, d0x = d0-d1)

#slim and save file
drought2 <- drought2 %>% select(huc8, name, date, none, d0x, d1x, d2x, d3x, d4x)

#combine wiht old drought and remove duplicates
drought2 <- rbind(old.drought, drought2)
drought2 <- drought2 %>% arrange(huc8, date) %>% distinct()

#save file
write.csv(drought2, paste0(swd_data, "drought/all_percentAreaHUC.csv"))
rm(drought, drought2, zt.name, zt, last.month, last.year, last.day, last.date, full_url, api.data, drought.time, df, old.drought, m, huc.list, i)

################################################################################################################################################
#
#                      UPDATE THE FORECAST DATA PROVIDED BY NOAA
#
################################################################################################################################################
#HUC8s for TX
huc8 <- read_sf(paste0(swd_data, "huc8.geojson"))

# CRS definition for HRAP projection, details and reference below
crs.hrap <- CRS('+proj=stere +lat_0=90 +lat_ts=60 +lon_0=-105 +x_0=0 +y_0=0 +R=6371200 +units=m +no_defs'); #have to set projection to read in; https://gist.github.com/tcmoran/a3bb702f14a1b45c1bd3

#create the url to obtain the last 7 days of observed precipitation and percent of normal precipitation
#Sometimes the day needs to be one or two earlier for the file to exist
year.url <- year(Sys.Date()); month.url <- month(Sys.Date()); day.url <- day(Sys.Date())
if(nchar(month.url)==1) { month.url = paste0("0", month.url) }
if(nchar(day.url)==1) { day.url = paste0("0", day.url) }

url.used <- paste0("https://water.weather.gov/precip/downloads/",year.url,"/",month.url,"/",day.url,"/nws_precip_last7days_",year.url,month.url,day.url,"_conus.tif")
#call data in as a raster
zt <- raster(url.used)
#the data are provided as 4 bands in one raster. We are interested in Band 1 and Band 4
#Band 1 - Observation - Last 24 hours of QPE spanning 12Z to 12Z in inches
#Band 2 - PRISM normals - PRISM normals in inches (see "Normal Precipitation" section on the About page)
#Band 3 - Departure from normal - The departure from normal in inches
#Band 4 - Percent of normal - The percent of normal
zt1 <- raster(url.used, band=1);
zt4 <- raster(url.used, band=4)

#clip zt2 to huc
zt1.proj <- projectRaster(zt1, crs="+proj=longlat +datum=WGS84")
zt1.proj <- crop(zt1.proj, extent(huc8));    #zt1.proj <- mask(zt1.proj, huc8); #extent makes a box, while mask clips to huc
#mapview::mapview(zt1.proj)
#NA value is -10000
pol <- rasterToPolygons(zt1.proj); colnames(pol@data) <- c("obsv_in")
pol <- pol %>% st_as_sf() %>% mutate(obsv_in = round(obsv_in,2)) %>% ms_simplify(keep = 0.5, keep_shapes=TRUE); #convert to a geojson and simplify to plot faster

#summarize and dissolve based on ranges
pol2 <- pol %>% mutate(bands = ifelse(obsv_in == 0, 0, ifelse(obsv_in <=0.1 & obsv_in > 0, 0.1, ifelse(obsv_in <=0.25 & obsv_in > 0.1, 0.25, ifelse(obsv_in <=0.5 & obsv_in > 0.25, 0.50, ifelse(obsv_in <=1 & obsv_in > 0.5, 1,
                                                                                                                                                                                                 ifelse(obsv_in <=2 & obsv_in > 1,2, ifelse(obsv_in <=3 & obsv_in > 2, 3, ifelse(obsv_in <=4 & obsv_in > 3, 4, ifelse(obsv_in <=5 & obsv_in > 4, 5, ifelse(obsv_in <=6 & obsv_in > 5, 6, 
                                                                                                                                                                                                                                                                                                                                                           ifelse(obsv_in <=8 & obsv_in > 6,8, ifelse(obsv_in <=10 & obsv_in > 8, 10, ifelse(obsv_in <=15 & obsv_in > 10, 15, ifelse(obsv_in <20 & obsv_in > 15, 20, ifelse(obsv_in > 20,30, NA))))))))))))))))
table(pol2$bands, useNA="ifany")
pol2 <- pol2 %>% group_by(bands) %>% summarize(nbands = n(), .groups="drop")

pol2 <- pol2 %>% mutate(colorVal = ifelse(bands==0, "white", ifelse(bands==0.1, "#3fc1bf", ifelse(bands==0.25, "#87b2c0", ifelse(bands==0.5, "#000080", ifelse(bands==1, "#00fc02", ifelse(bands==1.5, "#56b000", 
                                                                                                                                                                                           ifelse(bands==2, "#316400", ifelse(bands==3, "yellow", ifelse(bands==4, "#f7e08b", ifelse(bands==5, "orange", ifelse(bands==6, "red", ifelse(bands==8, "#9a0000",
                                                                                                                                                                                                                                                                                                                                        ifelse(bands==10, "#4e0000", ifelse(bands==15, "#e00079", ifelse(bands>=20,"#8e2eff", "black"))))))))))))))))
#convert pol to geojson and simplify
leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = pol2, fillOpacity= 0.8, fillColor = pol2$colorVal, color="black", weight=0)
#write 7 day observations to file
geojson_write(pol2, file =  paste0(swd_data, "pcp/pcp_7day_obsv.geojson"))


#Redo for percent of normal
zt4.proj <- projectRaster(zt4, crs="+proj=longlat +datum=WGS84")
zt4.proj <- crop(zt4.proj, extent(huc8));     #zt4 proj <- mask(zt4.proj, huc)

pol <- rasterToPolygons(zt4.proj); colnames(pol@data) <- c("percent_norm");
pol <- pol %>% st_as_sf() %>% mutate(percent_norm = round(percent_norm,2)) %>% ms_simplify(keep = 0.5, keep_shapes=TRUE)

#summarize and dissolve based on ranges
pol2 <- pol %>% mutate(bands = ifelse(percent_norm == 0, 0, ifelse(percent_norm <=5 & percent_norm > 0, 5, ifelse(percent_norm <=10 & percent_norm > 5, 10, ifelse(percent_norm <=25 & percent_norm > 10, 25, 
                                                                                                                                                                   ifelse(percent_norm <=50 & percent_norm > 25, 50, ifelse(percent_norm <=75 & percent_norm > 50, 75, ifelse(percent_norm <=90 & percent_norm > 75, 90, ifelse(percent_norm <=100 & percent_norm > 90, 100,
                                                                                                                                                                                                                                                                                                                                ifelse(percent_norm <=110 & percent_norm > 100, 110, ifelse(percent_norm <=125 & percent_norm > 110, 125, ifelse(percent_norm <=150 & percent_norm > 125, 150, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                 ifelse(percent_norm <=200 & percent_norm > 150, 200, ifelse(percent_norm <=300 & percent_norm > 200, 300, ifelse(percent_norm <= 400 & percent_norm > 300, 400, 
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  ifelse(percent_norm > 400 & percent_norm <=600, 600, ifelse(percent_norm > 600, 800, NA)))))))))))))))))
table(pol2$bands, useNA="ifany")
pol2 <- pol2 %>% group_by(bands) %>% summarize(nbands = n(), .groups="drop")
pol2 <- pol2 %>% mutate(colorVal = ifelse(bands==0, "white", ifelse(bands==5, "#4e0000", ifelse(bands==10, "#9a0000", ifelse(bands==25, "red", ifelse(bands==50, "orange", ifelse(bands==75, "#f7e08b", 
                                                                                                                                                                                  ifelse(bands==90, "yellow", ifelse(bands==100, "#316400", ifelse(bands==110, "#00fc02", ifelse(bands==125, "#56b000", ifelse(bands==150, "#316400", ifelse(bands==200, "#3fc1bf",
                                                                                                                                                                                                                                                                                                                                             ifelse(bands==300, "#000080", ifelse(bands==400, "#8e2eff", ifelse(bands>400,"#e00079", "black"))))))))))))))))

leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>%   addPolygons(data = pol2, fillOpacity= 0.8, fillColor = pol2$colorVal, color="black", weight=0)
geojson_write(pol2, file =  paste0(swd_data, "pcp/pcp_7day_percent_normal.geojson"))
#clear out files
rm(zt, zt1, zt4, pol, pol2, zt1.proj, zt4.proj)



###################################################################################################################################
#     
#          6-10 day precipitation and temperature outlooks
#
####################################################################################################################################
end_date_prcp <- as.POSIXct(today)-1 #there is a one day lag time
end_date_prcp <- as.character(end_date_prcp)
end_date_prcp <- gsub('\\s+', '', end_date_prcp)
end_date_prcp <- gsub('-', '', end_date_prcp)
end_date_prcp <- gsub(':', '', end_date_prcp)
end_date_prcp <- substr(end_date_prcp, 1,8)

download.file(paste0("https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/610prcp_",end_date_prcp,".zip"), destfile="temp.zip")
# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
unzip("temp.zip", files=NULL, exdir="temp")
#get data
pcp <- readOGR(paste0("temp"), paste0("610prcp_",end_date_prcp)) %>% st_as_sf() %>% st_transform(crs = 4326) %>% select(Prob, Cat, geometry) %>% rename(percentage = Prob, direction = Cat)
pcp <- pcp %>% mutate(colorVal = ifelse(percentage < 33, "white", "black")) %>% mutate(colorVal = ifelse(direction == "Above" & percentage >= 33 & percentage < 40, "#d4f8d4", colorVal)) %>% 
  mutate(colorVal = ifelse(direction == "Above" & percentage >= 40 & percentage < 50, "#90ee90", ifelse(direction == "Above" & percentage >= 50 & percentage < 60, "#4ce44c", 
                                                                                                        ifelse(direction == "Above" & percentage >= 60 & percentage < 70, "#1ec31e", ifelse(direction == "Above" & percentage >= 70 & percentage < 80, "#169016", 
                                                                                                                                                                                            ifelse(direction == "Above" & percentage >= 80 & percentage <= 100, "#0c4c0c", colorVal))))))

pcp <- pcp %>% mutate(colorVal = ifelse(direction == "Below" & percentage >= 33 & percentage < 40, "#e1d9d2", ifelse(direction == "Below" & percentage >= 40 & percentage < 50, "#b9a797", 
                                                                                                                     ifelse(direction == "Below" & percentage >= 50 & percentage < 60, "#b19d8c", ifelse(direction == "Below" & percentage >= 60 & percentage < 70, "#776250", 
                                                                                                                                                                                                         ifelse(direction == "Below" & percentage >= 70 & percentage < 80, "#5f4f40", ifelse(direction == "Below" & percentage >= 80 & percentage <= 100, "#312821", colorVal)))))))
pcp <- pcp %>% mutate(colorVal = ifelse(direction == "Normal", "white", colorVal))
pcp <- st_zm(pcp)
table(pcp$colorVal)
pcp <- pcp %>% mutate(Name = ifelse(direction != "Normal", paste0(percentage, "% chance of precipitation being ", direction, " Normal"), paste0(percentage, "% chance of precipitation being ", direction)))

leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = pcp, fillOpacity= 0.75, fillColor = pcp$colorVal, color="black", weight=0)
geojson_write(pcp, file =  paste0(swd_data, "pcp/pcp610forecast.geojson"))

#delete temp files
fold = ("temp")
# get all files in the directories, recursively
f <- list.files(fold, include.dirs = F, full.names = T, recursive = T)
# remove the files
file.remove(f)

######################### REPEAT FOR TEMPERATURE FORECAST #########################
end_date_temp <- as.POSIXct(today)-1 #there is a one day lag time
end_date_temp <- as.character(end_date_temp)
end_date_temp <- gsub('\\s+', '', end_date_temp)
end_date_temp <- gsub('-', '', end_date_temp)
end_date_temp <- gsub(':', '', end_date_temp)
end_date_temp <- substr(end_date_temp, 1,8)

download.file(paste0("https://ftp.cpc.ncep.noaa.gov/GIS/us_tempprcpfcst/610temp_",end_date_temp,".zip"), destfile="temp.zip")

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
unzip("temp.zip", files=NULL, exdir="temp")

#get data
pcp <- readOGR(paste0("temp"), paste0("610temp_", end_date_temp)) %>% st_as_sf() %>% st_transform(crs = 4326) %>% select(Prob, Cat, geometry) %>% rename(percentage = Prob, direction = Cat)
pcp <- pcp %>% mutate(colorVal = ifelse(percentage < 33, "white", "black")) %>% mutate(colorVal = ifelse(direction == "Above" & percentage >= 33 & percentage < 40, "#ffc4c4", colorVal)) %>% 
  mutate(colorVal = ifelse(direction == "Above" & percentage >= 40 & percentage < 50, "#ff7676", ifelse(direction == "Above" & percentage >= 50 & percentage < 60, "#ff2727", 
                                                                                                        ifelse(direction == "Above" & percentage >= 60 & percentage < 70, "#eb0000", ifelse(direction == "Above" & percentage >= 70 & percentage < 80, "#b10000", 
                                                                                                                                                                                            ifelse(direction == "Above" & percentage >= 80 & percentage <= 100, "#760000", colorVal))))))

pcp <- pcp %>% mutate(colorVal = ifelse(direction == "Below" & percentage >= 33 & percentage < 40, "#d8d8ff", ifelse(direction == "Below" & percentage >= 40 & percentage < 50, "#9d9dff", 
                                                                                                                     ifelse(direction == "Below" & percentage >= 50 & percentage < 60, "#4e4eff", ifelse(direction == "Below" & percentage >= 60 & percentage < 70, "#1414ff", 
                                                                                                                                                                                                         ifelse(direction == "Below" & percentage >= 70 & percentage < 80, "#0000d8", ifelse(direction == "Below" & percentage >= 80 & percentage <= 100, "#00009d", colorVal)))))))
pcp <- pcp %>% mutate(colorVal = ifelse(direction == "Normal", "white", colorVal))
pcp <- st_zm(pcp)
table(pcp$colorVal)
pcp <- pcp %>% mutate(Name = ifelse(direction != "Normal", paste0(percentage, "% chance of temperature being ", direction, " Normal"), paste0(percentage, "% chance of temperature being ", direction)))

leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = pcp, fillOpacity= 0.6, fillColor = pcp$colorVal, color="black", weight=0)
geojson_write(pcp, file =  paste0(swd_data, "pcp/temp610forecast.geojson"))

#delete temp files
fold = ("temp")
# get all files in the directories, recursively
f <- list.files(fold, include.dirs = F, full.names = T, recursive = T)
# remove the files
file.remove(f)

rm(pcp)


###################################################################################################################################
#
#          1-7 day total precipitation forecast amount
#
###################################################################################################################################
file_to_geojson(input="https://www.wpc.ncep.noaa.gov/kml/qpf/QPF168hr_Day1-7_latest.kml", method='web', output= paste0(swd_data, 'pcp/qpf1-7dayforecast'))
pcp <- read_sf(paste0(swd_data, 'pcp/qpf1-7dayforecast.geojson')) %>% select(Name, geometry) %>% sf::st_transform(crs = 4326)

#pcp2 <- st_crop(pcp, extent(huc8)) 
#add colors
pcp2 <- pcp %>% rename(bands = Name) %>% dplyr::select(bands, geometry) %>% 
  mutate(colorVal = ifelse(bands==0, "white", ifelse(bands==0.01, "lightgray",ifelse(bands==0.1, "#228b22", ifelse(bands==0.25, "#2cb42c", ifelse(bands==0.5, "#000080", #greens
                                                                                                                                                  ifelse(bands==0.75, "#000072",  ifelse(bands==1, "#005fbf",  ifelse(bands==1.25, "#007cfa", ifelse(bands==1.5, "#00bfbf", #blues
                                                                                                                                                                                                                                                     ifelse(bands==1.75, "#9370db", ifelse(bands==2, "#663399", ifelse(bands==2.5, "#800080", #purples
                                                                                                                                                                                                                                                                                                                       ifelse(bands==3, "darkred", ifelse(bands==4, "red", ifelse(bands==5, "#ff4500", ifelse(bands==7, "orange", #red/orange
                                                                                                                                                                                                                                                                                                                                                                                                              ifelse(bands==10, "#8b6313",ifelse(bands==15, "#daa520",ifelse(bands<=20,"yellow", "black"))))))))))))))))))))
#mapview::mapviewOptions(fgb = FALSE)
#mapview::mapview(pcp2)
pcp2 <- st_zm(pcp2); #Not sure what this does but it makes it work
leaflet() %>%  addProviderTiles("Stamen.TonerLite") %>% addPolygons(data = pcp2, fillOpacity= 0.6, fillColor = pcp2$colorVal, color="black", weight=0)
#pcp2 <- pcp2 %>% ms_simplify(0.5, keep_shapes=TRUE)
geojson_write(pcp2, file =  paste0(swd_data, "pcp/qpf1-7dayforecast.geojson"))

rm(pcp, pcp2)
#####################################################################################################################################################

###################################################################################################################################################################################################################################
# 
# Read in old pcp data
#
########################################################################################################################################################
boerne.sites <- read.csv(paste0(swd_data, "pcp/pcp_locations_metadata.csv"))
old.pcp <- read.csv(paste0(swd_data, "pcp/historic_pcp_data.csv")) %>% mutate(date = as.POSIXct(date, format = "%Y-%m-%d")) #historic data
current.year <-year(today);
########################################################################################################################################################

########################################################################################################################################################
# 
# Import New Synoptic Data
#
########################################################################################################################################################
# Synoptic TexMesonet API definitions can be found here: 
# https://developers.synopticdata.com/mesonet/v2/stations/precipitation/

# Base URL & station ID list for API calls: 
base.pcp.url <- "https://api.synopticdata.com/v2/stations/timeseries?stid=" #this is same for all sites
site.ids <- c("cict2", "twb03", "gubt2", "gbft2", "gbkt2", "gbjt2",
              "gbrt2", "gbtt2", "gbvt2", "gbmt2", "gbst2", "gbdt2", "gbqt2",
              "gupt2", "smct2", "ea004", "ea006", "ea035") #this is the part that changes

start_date = "202201010000" # this is the format needed for the listed website above
end_date <- today()-1 #allow a one day lag time
end_date <- as.character(end_date)
#end_date <- gsub('\\s+', '', end_date)
end_date <- gsub('-', '', end_date)
#end_date <- gsub(':', '', end_date)
#end_date <- substr(end_date, 1,12)
end_date <- gsub("^(.{8})(.*)$", "\\12345\\2", end_date)

url_time_start = paste0("&start=",start_date)
url_time_end = paste0("&end=",end_date)

addl.pars.url <- "&vars=precip_accum,precip_accum_since_local_midnight,precip_accum_one_hour,precip_accum_one_minute,precip_accum_five_minute,precip_accum_fifteen_minute&precip=1&units=english"
texmesonet.token <- "c251136014704e98acbbb7df76c639d4"
url_token = paste0("&token=", texmesonet.token)

# Pull the data
# create empty storage dfs
synoptic.all.station.metadata <- matrix(nrow = 0, ncol = 15) %>% as.data.frame()
colnames(synoptic.all.station.metadata) <- c("STATUS", "MNET_ID", "ELEVATION", "NAME",
                                             "STID", "ELEV_DEM", "LONGITUDE", "STATE",
                                             "RESTRICTED", "QC_FLAGGED", "LATITUDE", "TIMEZONE", "ID",
                                             "PERIOD_OF_RECORD.start", "PERIOD_OF_RECORD.end")

synoptic.all.station.data <- matrix(nrow = 0, ncol = 5) %>% as.data.frame()
colnames(synoptic.all.station.data) <- c("OBSERVATIONS.date_time", "OBSERVATIONS.precip_accumulated_set_1d",
                                         "OBSERVATIONS.precip_intervals_set_1d", "station", "agency")

# create a list of assigned stations to their agencies
HADS <- c("CICT2", "GUBT2", "SMCT2")
TWDB <- c("TWB03")
EAA <- c("EA004", "EA006", "EA035")
GBRA <- c("GBFT2", "GBKT2", "GBJT2", "GBRT2", "GBTT2", "GBVT2", "GBMT2", "GBST2", "GBDT2", "GBQT2")
RAWS <- c("GUPT2")


# loop through sites and pull data
for(i in 1:length(site.ids)) {
  api.url <- paste0(base.pcp.url, site.ids[i], url_time_start, url_time_end, addl.pars.url, url_token)
  api.return <- fromJSON(api.url, flatten = TRUE)
  api.station <- api.return$STATION
  api.station.metadata <- subset(api.station, select=-c(16:24))
  api.station.data <- subset(api.station, select=c(22:24))
  api.station.data <- unnest(api.station.data, cols = c(OBSERVATIONS.date_time, OBSERVATIONS.precip_accumulated_set_1d, 
                                                        OBSERVATIONS.precip_intervals_set_1d))
  api.station.data$OBSERVATIONS.date_time <- as.Date(api.station.data$OBSERVATIONS.date_time)
  #api.station.data <- do.call(rbind.data.frame, api.station.data)
  api.station.data <- aggregate(.~OBSERVATIONS.date_time,data=api.station.data,FUN=sum)
  api.station.data$station <- api.station.metadata[1,5]
  api.station.data$agency <- case_when(
    api.station.data$station %in% HADS ~ "HADS",
    api.station.data$station %in% TWDB ~ "TWDB",
    api.station.data$station %in% EAA ~ "EAA",
    api.station.data$station %in% GBRA ~ "GBRA",
    api.station.data$station %in% RAWS ~ "RAWS"
  )
  api.station.metadata$agency <- case_when(
    api.station.metadata$STID %in% HADS ~ "HADS",
    api.station.metadata$STID %in% TWDB ~ "TWDB",
    api.station.metadata$STID %in% EAA ~ "EAA",
    api.station.metadata$STID %in% GBRA ~ "GBRA",
    api.station.metadata$STID %in% RAWS ~ "RAWS"
  )
  # Now bind it up to save out
  synoptic.all.station.metadata <- rbind(synoptic.all.station.metadata, api.station.metadata)
  synoptic.all.station.data <- rbind(synoptic.all.station.data, api.station.data)
  
  # Keep an eye on the progress:
  print(paste0("Completed pull for ", site.ids[i], ". ", round(i*100/length(site.ids), 2), "% complete."))
}


# clean new data
# eliminate cummulative column 
synoptic.all.station.data2 <- select(synoptic.all.station.data, c(1, 3, 4, 5))

# rename columns
synoptic.all.station.data2 <- rename(synoptic.all.station.data2, id = "station", date = "OBSERVATIONS.date_time", pcp_in = "OBSERVATIONS.precip_intervals_set_1d")

# make sure there are no duplicates
synoptic.all.station.data2 <- unique(synoptic.all.station.data2[c("id", "date", "pcp_in")])

#format dates
synoptic.all.station.data2$year <- year(synoptic.all.station.data2$date)
synoptic.all.station.data2$month <- month(synoptic.all.station.data2$date)
synoptic.all.station.data2$day <- day(synoptic.all.station.data2$date)
#synoptic.all.station.data2 <- synoptic.all.station.data2 %>% mutate(date = as.POSIXct(date, format = "%Y-%m-%d"))

#check the last date
check.last.date <- synoptic.all.station.data2 %>% filter(date == max(date)) %>% dplyr::select(date)
table(check.last.date$date)

##################################################################################################################################################################
#
#   Import New NOAA Data  
#
#################################################################################################################################################################
# token for National Climatic Data Center (NCDC) API (now NCEI - National Center for Environmental Information). 
# Obtain unique token from: https://www.ncdc.noaa.gov/cdo-web/token
ncdc_token <- 'xazWKRdECnwWdDDQVelRomkFPJctIhRy'
state_fips = paste0("FIPS:", stateFips)

#lets find stations in TX
noaa.stations <- ghcnd_stations(refresh=TRUE)
#takes a long time to run... save out file for later
#write.csv(noaa.stations, paste0(swd_data,"pcp/noaa_stations.csv"), row.names = FALSE)
#noaa.stations <- read.csv(paste0(swd_data,"pcp/noaa_stations.csv"))
bkup <- noaa.stations;

# filter sites of relevance
noaa.boerne.stations <- noaa.stations %>% filter(id=="USC00410902" | id=="USC00411429" | id=="USC00411433" | id=="USC00411434" | id=="USC00411920") #filter the specific site(s) of interest
noaa.boerne.sites <- noaa.boerne.stations %>% filter(element == "PRCP")
noaa.boerne.sites <- noaa.boerne.sites %>% filter(first_year <= 2015)
noaa.boerne.sites <- noaa.boerne.sites %>% filter(last_year == current.year) 

#Or if we want daily summaries
# Fetch more information about location id FIPS:48
#daily.stations <- ncdc_locs(datasetid = 'GHCND', locationcategoryid = "ST", token = ncdc_token) #Global Historical Climatological Network Daily
# Fetch available locations for the GHCND (Daily Summaries) dataset
## change ST in locationcategoryid

# #Other datasets:
# ncdc_locs(datasetid='GHCND', token = ncdc_token)
# ncdc_locs(datasetid=c('GHCND', 'ANNUAL'), token = ncdc_token)
# ncdc_locs(datasetid=c('GSOY', 'ANNUAL'), token = ncdc_token)
# ncdc_locs(datasetid=c('GHCND', 'GSOM'), token = ncdc_token)

sites <- noaa.boerne.sites

#Pull and save out historic precip data. 
#metadata: https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt
#mflag is the measurement flag. 
#qflag is the quality flag. 
#sflag is the source flag. 

#parameters
unique.stations <- unique(sites$id)

#create data frame
noaa.all.station.data <- as.data.frame(matrix(nrow=0, ncol=9)); colnames(noaa.all.station.data) <- c("id","year","month","day","precip","mflag","qflag","sflag","date")
values <- paste0("VALUE", seq(1,31,1));  mflag <- paste0("MFLAG", seq(1,31,1));  qflag <- paste0("QFLAG", seq(1,31,1));  sflag <- paste0("SFLAG", seq(1,31,1))


# Modify this to fit the number of unique sites you need to loop through 
length(unique.stations) # Split up calls according to the number of unique stations
dat.a <- ghcnd(stationid = unique.stations[1:5], refresh = TRUE, token = ncdc_token)
#dat.b <- ghcnd(stationid = unique.stations[100:199], token = ncdc_token)
#dat.c <- ghcnd(stationid = unique.stations[200:299], token = ncdc_token)
#dat.d <- ghcnd(stationid = unique.stations[300:399], token = ncdc_token)
#dat.e <- ghcnd(stationid = unique.stations[400:length(unique.stations)], token = ncdc_token)
#dat.f <- ghcnd(stationid = unique.stations[500:599], token = ncdc_token)
#dat.g <- ghcnd(stationid = unique.stations[600:699], token = ncdc_token)
#dat.h <- ghcnd(stationid = unique.stations[700:799], token = ncdc_token)
#dat.i <- ghcnd(stationid = unique.stations[800:899], token = ncdc_token) # literally running for 8 hours...
#dat.j <- ghcnd(stationid = unique.stations[900:999], token = ncdc_token)
#dat.k <- ghcnd(stationid = unique.stations[1000:length(unique.stations)], token = ncdc_token) #catch all. Will be slower

#check and bind
dat.all <- dat.a
#dat.all <- rbind(dat.a, dat.b, dat.c, dat.d, dat.e, dat.f, dat.g, dat.h, dat.i, dat.j, dat.k); write.csv(dat.all, paste0(swd_data, "ghcnd_backup_full.csv"), row.names = FALSE)
#rm(dat.a, dat.b, dat.c, dat.d, dat.e, dat.f, dat.g, dat.h, dat.i, dat.j, dat.k) #clear out


#filter and format data 
dat1 <- dat.all
dat <- dat1 %>% filter(year >= 1990) %>%  filter(element == "PRCP")

dat2 <- dat %>% select(id, year, month, values) %>% gather(key = "day", value = "precip", -id, -year, -month)
dat3 <- dat %>% select(id, year, month, all_of(mflag)) %>% gather(key = "day", value = "mflag", -id, -year, -month)
dat4 <- dat %>% select(id, year, month, all_of(qflag)) %>% gather(key = "day", value = "qflag", -id, -year, -month)
dat5 <- dat %>% select(id, year, month, all_of(sflag)) %>% gather(key = "day", value = "sflag", -id, -year, -month)

dat <- cbind(dat2,dat3$mflag,dat4$qflag, dat5$sflag);  colnames(dat) <- c("id", "year", "month","day", "precip", "mflag", "qflag", "sflag")
table(dat$mflag); #t means trace precip
table(dat$qflag);
table(dat$sflag); #N means COCORAHS; G means Offical systems, 0 or 7 means US COOP...

dat <- dat %>% mutate(day = substr(day,6,7), date = as.Date(paste0(year,"-",month,"-",day), "%Y-%m-%d"))
#remove those dates that do not exist
dat <- dat %>% filter(is.na(date)==FALSE)

noaa.all.station.data <- dat

summary(noaa.all.station.data)

#slim down data
zt <- noaa.all.station.data %>% select(id, sflag) %>% distinct() 
zt <- zt %>% filter(sflag != " "); length(unique(zt$id))
zt <- zt %>% filter(sflag != "D") %>% filter(sflag != "Z"); length(unique(zt$id))
zt <- zt %>% filter(sflag != "N"); length(unique(zt$id)) #206 sites that are not cocorhas

#filter data to those that are not cocorahs (CoCoRHaS - N - community collaboraitve rain, hail, snow. Z - Datzilla.)
noaa.boerne.sites <- noaa.boerne.sites %>% filter(id %in% zt$id)
noaa.all.station.data <- noaa.all.station.data %>% filter(id %in% noaa.boerne.sites$id)
table(noaa.all.station.data$qflag);
table(noaa.all.station.data$mflag)

#Assume NA is zero
noaa.all.station.data[is.na(noaa.all.station.data)] <- 0

boerne.data <- noaa.all.station.data 

# clean metadata
noaa.all.station.metadata <- noaa.boerne.sites
noaa.all.station.metadata$agency <- "NOAA"
noaa.all.station.metadata2 <- subset(noaa.all.station.metadata, select=-c(7, 8, 9))

#rename columns and minimize
noaa.all.station.data <- boerne.data %>% select(id, date, precip) %>% mutate(precip = as.numeric(precip))
colnames(noaa.all.station.data) <- c("id", "date", "pcp_in")

#convert date into year, month, day
noaa.all.station.data <- noaa.all.station.data %>% mutate(date = as.Date(date, format="%Y-%m-%d"), year = year(date), month = month(date), day = day(date))
noaa.all.station.data <- noaa.all.station.data %>% arrange(id, date) %>% distinct()
table(noaa.all.station.data$id, noaa.all.station.data$year)

# make sure there are no duplicates
noaa.all.station.data <- unique(noaa.all.station.data[c("date", "pcp_in", "id", "year", "month", "day")])

# filter for new data (beyond 2021)
noaa.all.station.data2 <- noaa.all.station.data %>% filter(year >= 2022)
check.last.date <- noaa.all.station.data2 %>% group_by(id) %>% filter(is.na(pcp_in) == FALSE) %>% filter(date == max(date)) %>% dplyr::select(id, date, month)
table(substr(check.last.date$date,0,10))

#from tenths of a mm, to mm, to inches
noaa.all.station.data2$pcp_mm <- noaa.all.station.data2$pcp_in/10
noaa.all.station.data2$pcp_in <- noaa.all.station.data2$pcp_mm*0.0393701
#truncate the number of decimals
noaa.all.station.data2$pcp_in <- trunc(noaa.all.station.data2$pcp_in*100)/100
#remove pcp_mm
noaa.all.station.data2 <- subset(noaa.all.station.data2, select=-c(pcp_mm))

##################################################################################################################################################################
#
#  Combine New Synoptic and NOAA Data 
#
#################################################################################################################################################################
#data
new.all.station.data <- rbind(synoptic.all.station.data2, noaa.all.station.data2)
new.all.station.data <- new.all.station.data %>% mutate(date = as.Date(date)) # precaution to make sure all are in the same date format
check.last.date <- new.all.station.data %>% group_by(id) %>% filter(date == max(date)) %>% dplyr::select(date)
table(check.last.date$date)

##################################################################################################################################################################
#
#  Combine New and Old Data 
#
#################################################################################################################################################################
#make sure each column is of the same type 
new.all.station.data <- new.all.station.data %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% filter(date > as.Date("2021-12-31", "%Y-%m-%d")) #%>% select(!X)
old.pcp <- old.pcp %>% mutate(date = as.Date(date, format = "%Y-%m-%d")) %>% filter(date < as.Date("2022-01-01", "%Y-%m-%d")) #%>% select(!X)
str(new.all.station.data) 
str(old.pcp)

pcp.data <- rbind(old.pcp, new.all.station.data)
pcp.data <- pcp.data %>% arrange(id, date) %>% distinct() #GBVT2 was showing up twice for some reason
table(pcp.data$id, pcp.data$year)

check.last.date <- pcp.data %>% group_by(id) %>% filter(is.na(pcp_in) == FALSE) %>% filter(date == max(date)) %>% select(id, date, month)
summary(check.last.date)
table(substr(check.last.date$date,0,10))

###################################################################################################################################
#          LOOP THROUGH AND COMBINE OLD AND NEW DATA, REMOVING ANY DUPLICATE DAYS
#          ALL DATA WITH A BAD DATA SCORE REPORT 0 RAIN...so NOTHING TO DO THERE
###################################################################################################################################
#rename columns and minimize
pcp.data <- pcp.data %>% dplyr::select(id, date, pcp_in) 

# #convert date time to just date and add year column
pcp.data <- pcp.data %>% mutate(date = as.Date(substr(date,0,10), "%Y-%m-%d"), year = year(date), month = month(date), day= day(date))
pcp.data <- pcp.data %>% arrange(id, date) #%>% distinct()

table(pcp.data$id, pcp.data$year)

check.last.date <- pcp.data %>% group_by(id) %>% filter(is.na(pcp_in) == FALSE) %>% filter(date == max(date)) %>% dplyr::select(id, date, month)
table(check.last.date$date)

# ...........................................................................
#WRITE OUT UPDATED
write.csv(pcp.data, paste0(swd_data, "pcp/all_pcp_data.csv"), row.names = FALSE)
write.csv(boerne.sites, paste0(swd_data, "pcp/all_pcp_locations_metadata.csv"), row.names = FALSE)

#LOAD IN UPDATED 
pcp.data <- read.csv(paste0(swd_data,"pcp/all_pcp_data.csv"), header = TRUE)
pcp.loc <- read.csv(paste0(swd_data, "pcp/all_pcp_locations_metadata.csv"), header = TRUE)
# ............................................................................

###################################################################################################################################
#          CREATE TABLE FOR MONTHLY PRECIPITATION TOTALS
###################################################################################################################################
pcp.data <- read.csv(paste0(swd_data, "pcp/all_pcp_data.csv"), header = TRUE)

#Can plot like demand - monthly summary
foo.month <- pcp.data %>% group_by(id, year, month) %>% summarize(pcp_in = sum(pcp_in, na.rm=TRUE), ndays = n(), .groups="drop")  %>% 
  pivot_wider(id_cols = c("id", "month"), names_from = year, names_prefix = "yr_", values_from = pcp_in) %>% arrange(id, month)
#we need to do the pivots to get NA fields in there
foo.month <- foo.month %>% pivot_longer(cols = starts_with("yr"), names_to = "year", names_prefix = "yr_", values_to = "pcp_in", values_drop_na = FALSE)
foo.month <- foo.month %>% arrange(id, year, month)
#add ndays back in
foo.m <- pcp.data %>% group_by(id, year, month) %>% summarize(ndays = n(), .groups="drop")
foo.month <- merge(foo.month, foo.m, by.x=c("id", "month", "year"), by.y=c("id", "month", "year"), all=TRUE)

#lets say you have to have ~90% of data... so 27 days... but we want to keep the current months data
yt <- foo.month %>% filter(ndays < 27); table(yt$year, yt$month)
current.month <- month(Sys.time()); current.year <- year(Sys.time())

foo.month <- foo.month %>% mutate(pcp_in = ifelse((month == current.month & year == current.year) | ndays >=27, pcp_in, NA))
yt <- foo.month %>% filter(is.na(pcp_in)); table(yt$year, yt$month)
foo.month <- foo.month %>% mutate(year = as.numeric(as.character(year))) %>% filter(year >= year(start.date))

#save file --- since only plotting recent years will only save out 2000 onward
#foo.month <- foo.month %>% filter(year>=1997)
write.csv(foo.month, paste0(swd_data, "pcp/all_pcp_months_total.csv"), row.names=FALSE)

###################################################################################################################################
#
#          CREATE TABLE FOR CUMULATIVE PRECIPITATION TOTALS
#
###################################################################################################################################
pcp.data <- pcp.data %>% filter(date>start.date)
foo.count <- pcp.data %>% group_by(id, year) %>% count() %>% filter(year < current.year & n>340 | year == current.year) %>% mutate(idyr = paste0(id,"-",year)) 
foo.cum <- pcp.data %>% mutate(idyr = paste0(id,"-",year)) %>% filter(idyr %in% foo.count$idyr) %>% arrange(id, year, month, day) %>% mutate(date= as.Date(date, format="%Y-%m-%d"))
foo.cum <- foo.cum %>% distinct() %>% filter(year>=2000); #shorten for this file

foo.cum$julian <- yday(foo.cum$date)

foo.cum <- foo.cum %>% arrange(id, year, julian) %>% dplyr::select(id, year, date, julian, pcp_in) %>% distinct() %>% 
  group_by(id, year) %>% mutate(pcp_in2 = ifelse(is.na(pcp_in), 0, pcp_in)) %>%  mutate(cum_pcp = cumsum(pcp_in2)) %>% dplyr::select(-pcp_in, -pcp_in2) %>% rename(pcp_in = cum_pcp) %>% distinct()

table(foo.cum$id, foo.cum$year)
#in case duplicate days - take average
foo.cum <- foo.cum %>% group_by(id, year, date, julian) %>% summarize(pcp_in = round(mean(pcp_in, na.rm=TRUE),2), .groups="drop") %>% distinct()
foo.cum <- foo.cum %>% pivot_wider(id_cols = c("id", "julian"), names_from = year, names_prefix = "yr_", values_from = pcp_in, values_fn = mean) %>% arrange(id, julian) %>% distinct()

foo.cum <- foo.cum %>% pivot_longer(cols = starts_with("yr"), names_to = "year", names_prefix = "yr_", values_to = "pcp_in", values_drop_na = FALSE) %>% arrange(id, year, julian) %>% 
  filter(julian == 365 & is.na(pcp_in)==FALSE | julian < 365) %>% group_by(id, year) %>% mutate(ndays = n()) %>% ungroup()

#remove years with more than 30 days missing (with the exception of the current year)
foo.cum2 <- foo.cum %>% group_by(id, year) %>% mutate(nMissing = sum(is.na(pcp_in)))
foo.cum2 <- foo.cum2 %>% filter(year == current.year | year < current.year & nMissing <= 31) %>% dplyr::select(-nMissing) #removes those missing more than a month of data

#add this to include in plot_ly by setting tick format to %b-%d
#foo.cum2 <- merge(foo.cum2, julian[,c("julian","month.day365","month.day366")], by.x="julian", by.y="julian", all.x=TRUE) %>% arrange(id, year, julian)
#foo.cum2$date = ifelse(foo.cum2$ndays==366, foo.cum2$month.day366, foo.cum2$month.day365) 
foo.cum2$date2 <- as.Date(foo.cum2$julian, origin=paste0(foo.cum2$year,"-01-01"))
foo.cum2$date <- format(foo.cum2$date2, format="%b-%d")
foo.cum2 <- foo.cum2 %>% dplyr::select(id, year, julian, pcp_in, date)

write.csv(foo.cum2, paste0(swd_data, "pcp/all_pcp_cum_total.csv"), row.names=FALSE)

###################################################################################################################################
#          Add current status to map
###################################################################################################################################
#convert station sites into an sf file
boerne.loc <- pcp.loc
sites <- st_as_sf(boerne.loc, coords = c("longitude", "latitude"), crs = 4326); 
mapview::mapview(sites)

#sites <- sites %>% mutate(startYr = year(start_date), endYr = year(end_date)) %>% dplyr::select(locID, network, name, elev_ft, agency, startYr, endYr, geometry) %>% rename(id = locID)
sites <- sites %>% rename(startYr = first_year, endYr = last_year) %>% dplyr::select(id, name, elevation, startYr, endYr, geometry, agency) #lacking network and agency variables. Omitted gsn_flag and wmo_id.

#get statistics by julian day to see cumulative pcp
ytd2 <- foo.cum %>% group_by(id, julian) %>%  summarize(min = round(min(pcp_in, na.rm=TRUE),2), flow10 =  round(quantile(pcp_in, 0.10, na.rm=TRUE),2), flow25 = round(quantile(pcp_in, 0.25, na.rm=TRUE),2),
                                                        flow50 = round(quantile(pcp_in, 0.5, na.rm=TRUE),2), flow75 = round(quantile(pcp_in, 0.75, na.rm=TRUE),2), flow90 = round(quantile(pcp_in, 0.90, na.rm=TRUE),2), max = round(max(pcp_in, na.rm=TRUE),2),.groups="drop")

ytd.now <- pcp.data %>% group_by(id) %>% filter(date == max(date))  %>% mutate(julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday) %>% dplyr::select(id, date, julian)
ytd.now.cum <- foo.cum %>% group_by(id) %>% filter(is.na(pcp_in) == FALSE) %>% filter(year == max(year))

ytd.now <- merge(ytd.now, ytd.now.cum, by.x=c("id", "julian"), by.y=c("id","julian"), all.x=TRUE) %>% mutate(year = year(date))
ytd.now <- merge(ytd.now, ytd2, by.x=c("id", "julian"), by.y=c("id","julian"), all.x=TRUE)

ytd.now <- ytd.now %>% mutate(status = ifelse(pcp_in <= flow10, "Extremely Dry", ifelse(pcp_in > flow10 & pcp_in <= flow25, "Very Dry", ifelse(pcp_in >= flow25 & pcp_in < flow50, "Moderately Dry", 
                                                                                                                                               ifelse(pcp_in >= flow50 & pcp_in < flow75, "Moderately Wet", ifelse(pcp_in >= flow75 & pcp_in < flow90, "Very Wet", ifelse(pcp_in >= flow90, "Extremely Wet", "Unknown")))))))
# ytd.now <- ytd.now %>% mutate(date = as.Date(date)) %>% mutate(status = ifelse(is.na(status)==TRUE, "Unknown", status)) %>%
#    mutate(status = ifelse(date <= (max(date)-10), "Unknown", status)) 
ytd.now <- ytd.now%>% distinct() # site GBVT2 was duplicated for some reason 
table(ytd.now$status, useNA="ifany")

boerne.sites <- merge(sites, ytd.now[,c("id", "julian", "date", "year", "pcp_in", "status")], by.x="id", by.y="id", all=TRUE)
geojson_write(boerne.sites, file = paste0(swd_data, "pcp/all_pcp_sites.geojson"))
mapview::mapview(boerne.sites)

################################################################################################################################################################
# remove all except for global environment 
rm(list= ls()[!(ls() %in% c('julian.ref','update.date', 'current.month', 'current.year', 'end.date', 'end.year', 
                            'mymonths', 'source_path', 'start.date', 'state_fips', 'stateAbb', 'stateFips', 'swd_data', 'today', 
                            '%notin%', 'ma'))])   










#######################################################################################################################################################
#
# Updates reservoir  data based on historic data collected
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
# Can update anytime
# Modified June 2021 by SOphia Bryson for TX, including USACE and USBR dam sites
# Modified November 2021 by Vianey Rueda for Boerne
#
########################################################################################################################################################



# Process: 
# load old usace, usbr, combined data
# pull new usace data. combine with old usace data.
# pull new usbr data. combine with old usbr data.
# combine usace and usbr updates and update combined data. 


######################################################################################################################################################################
#
#   LOAD Old Data - make sure parameters to update don't miss any data
#
######################################################################################################################################################################
#Now read in the website data and add the most recent files to the end of it
old.data.usace <- read.csv(paste0(swd_data, "reservoirs/usace_dams.csv"))


last.update <- max(old.data.usace$date); today <- as.Date(substr(Sys.time(),1,10), "%Y-%m-%d")
difftime(today, last.update, units = "weeks") #time diff check since last update
timeAmt = 2
timeUnit = "weeks"

mymonths <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec");
########################################################################################################################################
####### UPDATE ARMY CORPS ##############################################################################################################
########################################################################################################################################

######################################################################################################################################################################
#
#   UPDATE USACE WITH RECENT DATA: CANYON LAKE IS IN THIS DATASET 
#   
######################################################################################################################################################################
# #set up url
# baseURL = 'https://water.usace.army.mil/a2w/'
# report_url = "CWMS_CRREL.cwms_data_api.get_report_json?p_location_id="
# parameter_url <- paste0("&p_parameter_type=Stor%3AElev&p_last=", timeAmt, "&p_last_unit=", timeUnit, "&p_unit_system=EN&p_format=JSON");   

#read in shapefile
project.df <- read_sf(paste0(swd_data, "reservoirs/usace_sites.geojson"))
#mapview::mapview(project.df)
ace.df <- project.df
#add url to data
res.url <- "http://water.usace.army.mil/a2w/f?p=100:1:0::::P1_LINK:"
project.df <- project.df %>% mutate(url_link = paste0(res.url,Loc_ID,"-CWMS"))

# Loop prep

#District list
tx.dist <- c("SWF", "SWT", "SWG", "ABQ")
data.dist <- unique(project.df$District)
tx.dist <- tx.dist[tx.dist %in% data.dist == TRUE] #No dams in ABQ

# API URL building blocks
baseURL = 'https://water.usace.army.mil/a2w/'
last_number = timeAmt; #number of units to collect  
last_unit = timeUnit; #other options are months, weeks, and days
report_url = "CWMS_CRREL.cwms_data_api.get_report_json?p_location_id="
parameter_url <- paste0("&p_parameter_type=Stor%3AElev&p_last=", last_number, "&p_last_unit=", last_unit, "&p_unit_system=EN&p_format=JSON")

# Create final data frame
all_district_data <- as.data.frame(matrix(nrow=0, ncol=8)); colnames(all_district_data) <- c("date", "elev_Ft", "storage_AF", "fstorage_AF", "locid", "district", "NIDID", "name")

# Pull new data for all sites in all districts

for(j in 1:length(tx.dist)){ #loop through districts
  
  district.id = tx.dist[j];
  
  district_data <- as.data.frame(matrix(nrow=0, ncol=8)); colnames(district_data) <- c("date", "elev_Ft", "storage_AF", "fstorage_AF", "locid", "district", "NIDID", "name")
  zt <- subset(project.df, District==district.id) %>% filter(str_detect(string = NIDID, pattern = "TX")) %>% filter(Loc_ID != 2165051) #Only TX, drop Truscott Brine Lake - no outward flow, no active mgmt - & different data structure breaks the loop
  
  for (i in 1:length(zt$Loc_ID)){ #loop through sites within districts
    
    location.id <- zt$Loc_ID[i]; location.id
    
    full_url <- paste0(baseURL, report_url, location.id, parameter_url)
    api.data <- GET(full_url, timeout(15000)) #use httr library to avoid timeout #CAN INCREASE IF TIMING OUT
    
    dam.data <- jsonlite::fromJSON(content(api.data, 'text'), simplifyVector = TRUE, flatten=TRUE) ##
    
    #lake level
    lake_level <- dam.data$Elev[[1]] 
    lake_level <- lake_level %>% mutate(date = as.Date(substr(lake_level$time,1,11), "%d-%b-%Y")) %>% group_by(date) %>% summarize(elev_Ft = round(median(value, na.rm=TRUE),2), .groups = "drop") ##
    plot(lake_level$date, lake_level$elev_Ft, type="l");
    
    #lake storage
    #conservation storage
    cons.stor <- purrr::map(dam.data$`Conservation Storage`, ~ purrr::compact(.)) %>% purrr::keep(~length(.) != 0) #conservation storage = storage_AF
    cons.stor <- cons.stor[[1]] 
    cons.stor <- cons.stor %>% mutate(date = as.Date(substr(cons.stor$time, 1, 11), "%d-%b-%Y")) %>% group_by(date) %>% summarize(storage_AF = round(median(value, na.rm=TRUE), 0), .groups ="drop")
    
    #flood storage
    flood.stor <- purrr::map(dam.data$`Flood Storage`, ~ purrr::compact(.)) %>% purrr::keep(~length(.) != 0)
    flood.stor <- flood.stor[[1]]
    flood.stor <- flood.stor %>% mutate(date = as.Date(substr(flood.stor$time, 1, 11), "%d-%b-%Y")) %>% group_by(date) %>% summarize(fstorage_AF = round(median(value, na.rm=TRUE), 0), .groups ="drop")
    
    #combine storage
    lake_stor <- merge(cons.stor, flood.stor, by.x = "date", by.y = "date", sort = TRUE) 
    plot(lake_stor$date, lake_stor$storage_AF, type="l"); lines(lake_stor$date, lake_stor$fstorage_AF, col="red") #igual
    
    #combine lake data
    lake_data <- merge(lake_level, lake_stor, by.x="date", by.y="date", all=TRUE)
    lake_data$locid <- as.character(location.id);     lake_data$district <- district.id;
    lake_data$NIDID <- as.character(zt$NIDID[i]);     lake_data$name <- as.character(zt$Name[i])
    
    #bind to larger dataframe
    
    district_data <- rbind(district_data, lake_data)
    print(paste0(location.id,": ", as.character(zt$Name[i])))
    
  } #end of site
  
  all_district_data <- rbind(all_district_data, district_data) #save data from each district loop to master df
  rm(api.data, dam.data, lake_level, cons.stor, flood.stor, lake_stor, lake_data, district_data)
  
} #end of district

summary(all_district_data)  #a few NA's in storage_AF and Elev_Ft - Addicks and Barker, as expected. 

new.data.usace <- all_district_data

########################################################################################################################################################################################################################
#
#     ADD OLD AND NEW DATA TOGETHER
#
########################################################################################################################################################################################################################
#pull out unique reservoirs
unique.nid <- unique(new.data.usace$NIDID); unique.nid

#new data
nx <- new.data.usace 
dateFormat = nx$date[1]
if(substr(dateFormat,5,5) == "-") {dateFormatFinal = "%Y-%m-%d"}
if(substr(dateFormat,5,5) != "-") {dateFormatFinal = "%m/%d/%Y"}
dateFormatFinal
nx$date <- as.Date(as.character(nx$date), dateFormatFinal) 
nx$Year <- year(nx$date)
nx$day_month <- substr(nx$date, 6, 10)
#set julian values
for(i in 1:nrow(nx)) { #computationally slow. There's almost certainly a faster way. But it works. 
  
  if(leap_year(nx$Year[i]) == TRUE) {nx$julian[i] <- julian.ref$julian_index_leap[julian.ref$day_month_leap == nx$day_month[i]]}
  if(leap_year(nx$Year[i]) == FALSE) {nx$julian[i] <- julian.ref$julian_index[julian.ref$day_month == nx$day_month[i]]}
  
  print(paste(round(i/nrow(nx)*100,2),"% complete"))
}

#clean data
nx <- nx %>% mutate(elev_Ft = ifelse(elev_Ft <= 0, NA, elev_Ft), storage_AF = ifelse(storage_AF <=0, NA, storage_AF), fstorage_AF = ifelse(fstorage_AF <=0, NA, fstorage_AF))
maxCap <- nx %>% group_by(NIDID) %>% summarize(maxCap = 1.2*quantile(elev_Ft, 0.90, na.rm=TRUE), maxStor = 1.2*quantile(storage_AF, 0.90, na.rm=TRUE), maxfStor = 1.2*quantile(fstorage_AF, 0.90, na.rm=TRUE), .groups="drop");
nx <- nx %>% left_join(maxCap, by="NIDID") %>% mutate(elev_Ft = ifelse(elev_Ft > maxCap, NA, elev_Ft), storage_AF = ifelse(storage_AF > maxStor, NA, storage_AF), fstorage_AF = ifelse(fstorage_AF > maxfStor, NA, fstorage_AF)) %>% 
  select(-maxCap, -maxStor, -maxfStor)

#include month abbreviations  
nx2 <- nx %>% mutate(julian = as.numeric(strftime(date, format = "%j")), month = month(date), monthAbb = mymonths[month])

#old data
fx <- old.data.usace
dateFormat = fx$date[1]
if(substr(dateFormat,5,5) == "-") {dateFormatFinal = "%Y-%m-%d"}
if(substr(dateFormat,5,5) != "-") {dateFormatFinal = "%m/%d/%Y"}
fx$date <- as.Date(as.character(fx$date), dateFormatFinal) 
fx$Year <- year(fx$date)
fx$day_month <- substr(fx$date, 6, 10)
# #set julian values - SHOULDN't BE NEEDED GOING FORWARD.
# for(i in 1:nrow(fx)) { #computationally slow. There's almost certainly a faster way. But it works. 
#   
#   if(leap_year(fx$Year[i]) == TRUE) {fx$julian[i] <- julian.ref$julian_index_leap[julian.ref$day_month_leap == fx$day_month[i]]}
#   if(leap_year(fx$Year[i]) == FALSE) {fx$julian[i] <- julian.ref$julian_index[julian.ref$day_month == fx$day_month[i]]}
#   
#   print(paste(round(i/nrow(fx)*100,2),"% complete"))
# }

#include month abbreviations  
fx2 <- fx %>% mutate(julian = as.numeric(strftime(date, format = "%j")), month = month(date), monthAbb = mymonths[month])

#what is the most recent date?
old.last.date <- fx2 %>% group_by(NIDID) %>% filter(date == max(date, na.rm=TRUE)) %>% select(NIDID, date) %>% distinct() %>% rename(lastDate = date)

#remove anything new after that date
nx2 <- nx2 %>% left_join(old.last.date, by="NIDID") %>% filter(date > lastDate)
fx.2020 <- fx2 %>% filter(Year>=2020) %>% select(NIDID, day_month, OT_Ft, OT_AF) %>% distinct(); #2020 has complete data
nx2 <- merge(nx2, fx.2020, by.x=c("NIDID","day_month"), by.y=c("NIDID","day_month"), all.x=TRUE)  
nx2 <- nx2 %>% mutate(percentStorage = round(storage_AF/OT_AF*100,2))
nx2 <- nx2 %>% select(NIDID, name, date, Year, day_month, julian, elev_Ft, storage_AF, OT_Ft, OT_AF, percentStorage, monthAbb, month); #colnames(nx2) <- colnames(fx2)

#combine
fx <- rbind(fx2, nx2)

#make sure no duplicates
fx <- fx %>% distinct()
#arrange by NIDID and date
fx <- fx %>% arrange(NIDID, date)

#SCOTT KERR HAS HAD A NEW SEDIMENT SURVEY
scott.ot = 36639
fx <- fx %>% mutate(OT_AF = ifelse(NIDID == "NC00300" & Year >=2017, 36639, OT_AF))

fx <- fx %>% mutate(percentStorage = round(storage_AF/OT_AF*100,2)) %>% mutate(storage_AF = ifelse(percentStorage > 300, NA, storage_AF), percentStorage = ifelse(percentStorage > 300, NA, percentStorage))
summary(fx)

tx.dams <- fx 
tx.dams <- tx.dams %>% mutate(jurisdiction = "USACE") 

write.csv(tx.dams, paste0(swd_data, "reservoirs/all_usace_dams.csv"), row.names=FALSE) 


# filter out reservoir(s) of interest
canyon.lake <- tx.dams %>% filter(name == "Canyon Lake")

#usace changed reporting units so multiply by 1000 for those dates that are not in the same units as previous observations
canyon.lake <- canyon.lake %>% mutate(storage_AF = ifelse(storage_AF < 1000, storage_AF*1000, storage_AF))
#recalculate storage
canyon.lake <- canyon.lake %>% mutate(percentStorage = round(storage_AF/OT_AF*100,2))
write.csv(canyon.lake, paste0(swd_data, "reservoirs/all_reservoir_data.csv"), row.names=FALSE)

########################################################################################################################################################################################################################
#
#          UPDATE RESERVOIR STATUS AND STATS
#
########################################################################################################################################################################################################################

#fx <- read.csv(paste0(swd_data, "reservoirs/all_canyon_lake"), header = TRUE) #for picking up part-way
fx <- canyon.lake %>% filter(is.na(OT_Ft) == FALSE) #drop sites without operational target
unique.sites <- unique(fx$NIDID) 

#set up data frame for stats and include year
stats <- as.data.frame(matrix(nrow=0,ncol=13));        colnames(stats) <- c("nidid", "julian", "min", "flow10", "flow25", "flow50", "flow75", "flow90", "max", "Nobs","startYr","endYr","date"); 
year.flow  <- as.data.frame(matrix(nrow=0, ncol=10));   colnames(year.flow) <- c("nidid", "name", "date", "year", "julian", "elev_ft","storage_af", "target_ft", "target_af", "percent_storage")


#for (i in 1:length(unique.sites)){  ##### loop not needed because it is only one site
#for (i in 17:length(unique.sites)){  #test
zt <- fx %>% filter(NIDID == unique.sites) %>% filter(Year >= year(start.date))
#summarize annual
zt.stats <- fx %>% group_by(NIDID, julian) %>% summarize(Nobs = n(), min=round(min(percentStorage, na.rm=TRUE),4), flow10 = round(quantile(percentStorage, 0.10, na.rm=TRUE),4), flow25 = round(quantile(percentStorage, 0.25, na.rm=TRUE),4),
                                                         flow50 = round(quantile(percentStorage, 0.5, na.rm=TRUE),4), flow75 = round(quantile(percentStorage, 0.75, na.rm=TRUE),4), flow90 = round(quantile(percentStorage, 0.90, na.rm=TRUE),4), 
                                                         max = round(max(percentStorage, na.rm=TRUE),4), .groups="drop")
zt.stats <- zt.stats %>% mutate(NIDID = as.character(NIDID), startYr = min(fx$Year), endYr = max(fx$Year)) 
if(dim(zt.stats)[1] == 366) {zt.stats$date = julian.ref$day_month_leap}
if(dim(zt.stats)[1] == 365) {zt.stats$date = julian.ref$day_month[c(1:365)]} 

#fill dataframe
stats <- rbind(stats, zt.stats)

zt <- fx %>% filter(Year>=2017) #%>% select(NIDID, name, date, year, julian, elev_Ft, storage_AF, OT_Ft, OT_AF, percentStorage, jurisdiction);    
colnames(zt) <- c("NIDID", "name", "date", "year", "day_month", "julian", "elev_ft","storage_af", "target_ft", "target_af", "percent_storage", "month", "monthAbb", "jurisdiction")
year.flow <- rbind(year.flow, zt)

#    print(i)
#  }
bk.up <- stats
summary(stats)
summary(year.flow)

#fix date stuff  
stats2 <- stats
stats2$endYr <- as.character(stats$endYr)
stats2$startYr <- as.character(stats$startYr)
stats2 <- stats2 %>% mutate(date2 = as.Date(paste0(end.year, "-",date)))
stats2 <- stats2 %>% mutate(month = substr(date,0,2))
stats2 <- stats2 %>% mutate(month = ifelse(month == "01", "Jan", month)); stats2 <- stats2 %>% mutate(month = ifelse(month == "07", "Jul", month))
stats2 <- stats2 %>% mutate(month = ifelse(month == "02", "Feb", month)); stats2 <- stats2 %>% mutate(month = ifelse(month == "08", "Aug", month))
stats2 <- stats2 %>% mutate(month = ifelse(month == "03", "Mar", month)); stats2 <- stats2 %>% mutate(month = ifelse(month == "09", "Sep", month))
stats2 <- stats2 %>% mutate(month = ifelse(month == "04", "Apr", month)); stats2 <- stats2 %>% mutate(month = ifelse(month == "10", "Oct", month))
stats2 <- stats2 %>% mutate(month = ifelse(month == "05", "May", month)); stats2 <- stats2 %>% mutate(month = ifelse(month == "11", "Nov", month))
stats2 <- stats2 %>% mutate(month = ifelse(month == "06", "Jun", month)); stats2 <- stats2 %>% mutate(month = ifelse(month == "12", "Dec", month))

#Now attach most recent value to stream stats for the map
recent.flow <- year.flow %>% group_by(NIDID) %>% filter(is.na(storage_af) == FALSE) %>% filter(date == max(date)); #do we want to do most recent date or most recent date with data?
current.stat <- merge(recent.flow, stats2, by.x=c("NIDID","julian"), by.y=c("NIDID", "julian"), all.x=TRUE) #%>% rename(date = date.x)
#clean
current.stat <- current.stat %>% select(-date.x, -year, -date.y, -elev_ft, -storage_af, -target_af, -target_ft, -month.x, -jurisdiction, -month.y)
current.stat <- current.stat %>% rename(date = day_month, month = monthAbb)

#if else for this year and last years flow
current.stat <- current.stat %>% mutate(status = ifelse(percent_storage <= flow10, "Extremely Dry", ifelse(percent_storage > flow10 & percent_storage <= flow25, "Very Dry", ifelse(percent_storage >= flow25 & percent_storage < flow50, "Moderately Dry", 
                                                                                                                                                                                    ifelse(percent_storage >= flow50 & percent_storage < flow75, "Moderately Wet", ifelse(percent_storage >= flow75 & percent_storage < flow90, "Very Wet", ifelse(percent_storage >= flow90, "Extremely Wet", "Unknown")))))))
current.stat$status <- ifelse(is.na(current.stat$status), "unknown", current.stat$status)
table(current.stat$status)

#merge to sites geojson 
project.df <- read_sf(paste0(swd_data, "reservoirs/all_canyon_lake_site.geojson"))
res.loc <- project.df %>% select(NIDID, Name, Jurisdiction, geometry) #why are there 4 of everything?
res.loc <- res.loc %>% slice(1)
canyon.lake.site.stats <- merge(res.loc, current.stat[,c("NIDID","status","percent_storage","julian","flow50")], by.x="NIDID", by.y="NIDID") #why are there 4 of everything?
#mapview::mapview(canyon.lake.site.stats)
geojson_write(canyon.lake.site.stats, file=paste0(swd_data, "reservoirs/all_canyon_lake_site.geojson"))

#rename nidid to site so can use same code as streamflow - used to make charts
current.year <- year.flow %>% filter(year == year(max(date)));     last.year <- year.flow %>% filter(year == (year(max(date))-1));     
stats.flow <- merge(stats, current.year, by.x=c("NIDID","julian"), by.y=c("NIDID","julian"), all.x=TRUE) %>% rename(site = NIDID, flow = percent_storage)
stats.past <- merge(stats, last.year, by.x=c("NIDID", "julian"), by.y=c("NIDID", "julian"), all.x=TRUE) %>% rename(site = NIDID, flow = percent_storage) %>% as.data.frame()
#clean and bind
stats.flow <- stats.flow %>% select(-date.x, -name, -year, -elev_ft, -storage_af, -target_af, -target_ft, -month, -jurisdiction)
stats.flow <- stats.flow %>% rename(date = day_month, date2 = date.y, month = monthAbb)
stats.past <- stats.past %>% select(-date.x, -name, -year, -elev_ft, -storage_af, -target_af, -target_ft, -month, -jurisdiction)
stats.past <- stats.past %>% rename(date = day_month, date2 = date.y, month = monthAbb)
stats.flow <- rbind(stats.past, stats.flow)

#get status
stats.flow <- stats.flow %>% mutate(status = ifelse(flow <= flow10, "Extremely Dry", ifelse(flow > flow10 & flow <= flow25, "Very Dry", ifelse(flow >= flow25 & flow < flow50, "Moderately Dry", 
                                                                                                                                               ifelse(flow >= flow50 & flow < flow75, "Moderately Wet", ifelse(flow >= flow75 & flow < flow90, "Very Wet", ifelse(flow >= flow90, "Extremely Wet", "Unknown")))))))
stats.flow <- stats.flow %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                                                                                                                                                                                 ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray")))))))

#save out
write.csv(stats.flow, paste0(swd_data, "reservoirs/all_reservoir_stats.csv"), row.names=FALSE)

################################################################################################################################################################
# remove all except for global environment 
rm(list= ls()[!(ls() %in% c('julian.ref','update.date', 'current.month', 'current.year', 'end.date', 'end.year', 
                            'mymonths', 'source_path', 'start.date', 'state_fips', 'stateAbb', 'stateFips', 'swd_data', 'today', 
                            '%notin%', 'ma'))])



#######################################################################################################################################################
#
# Updates streamflow data based on historic data collected
# CREATED BY LAUREN PATTERSON @ THE INTERNET OF WATER
# FEBRUARY 2021
# Update daily if desire
# Updated by Sophia Bryson 6/1/2021 for TX
# Modified November 2021 by Vianey Rueda for Boerne
#
########################################################################################################################################################


#CALCULATE ROLLING AVERAGES MOVING FORWARD
#https://waterdata.usgs.gov/blog/moving-averages/
#owasa <- read_sf("https://geoconnex.us/ref/pws/NC0368010")

#################################################################################################################################
#       SET UP INFORMATION TO CALL API
#################################################################################################################################
#Identify parameter of interest: https://help.waterdata.usgs.gov/code/parameter_cd_query?fmt=rdb&inline=true&group_cd=%
pcode = '00060' #discharge (cfs)
#Identify statistic code for daily values: https://help.waterdata.usgs.gov/code/stat_cd_nm_query?stat_nm_cd=%25&fmt=html
scode = "00003"  #mean
#pick service
serv <- "dv"


#################################################################################################################################
#
##############                               LOAD DATA
#
#################################################################################################################################
#Old Data
boerne.sites <- read_sf(paste0(swd_data, "streamflow/stream_gauge_sites.geojson")) %>% select(site, name, huc8, startYr, endYr, nYears, geometry,ws_watershed)
boerne.site.metadata <- read.csv(paste0(swd_data, "streamflow/stream_gauge_metadata.csv"))
boerne.data <- read.csv(paste0(swd_data, "streamflow/historic_stream_data.csv"), colClasses=c("site" = "character")) %>% mutate(date = as.Date(date, format="%Y-%m-%d"))
boerne.data <- boerne.data %>% group_by(site) %>% filter(date < max(date))

#ws.bounds <- read_sf(paste0(swd_data, "streamflow/boerne_ws_watersheds.geojson")) %>% select(geometry)
# this is NC ws watersheds 
#ws.bounds <- read_sf("/Users/VianeyRueda/Desktop/Boerne IoW/TriangleWaterSupplyDashboard-master/deploy/nc-water-supply/data/water_supply_watersheds.geojson") 
#################################################################################################################################
#
#                 IDENTIFY GAUGES WITH PWSID WATER SUPPLY WATERSHEDS 
#                 XX Not currently using supply watersheds for TX XX
#
#################################################################################################################################
##intersect together gauge location with watershed bounds
#gauges_huc <- st_intersection(boerne.sites, ws.bounds);

# #merge names together
# zt <- gauges_huc %>% as.data.frame() %>% select(site, STREAM_NAM) %>% distinct()
# nc.sites <- merge(nc.sites, zt, by.x="site", by.y="site", all.x=TRUE)
# table(tx.sites$STREAM_NAM, useNA="ifany")

#go through stream sites and calculate statistics
unique.sites <- unique(boerne.sites$site)

#set up data frame for stats and include year
year.flow  <- as.data.frame(matrix(nrow=0, ncol=4));    colnames(year.flow) <- c("site", "date", "julian", "flow")

#Loop through each site reads in new data
for (i in 1:length(unique.sites)){
  old.data <- boerne.data %>% filter(site==unique.sites[i]) %>% filter(date==max(date))
  
  zt <- readNWISuv(siteNumbers = unique.sites[i], parameterCd = pcode, startDate=(old.data$date[1]+1), endDate = today); #only read in new data
  zt <- renameNWISColumns(zt)
  
  if (dim(zt)[1] > 0)  {
    zt <- zt %>% mutate(julian = as.POSIXlt(dateTime, format = "%Y-%m-%d")$yday) %>% mutate(date = as.Date(dateTime, format="%Y-%m-%d")); #calculates julian date
    #calculate mean value
    zt <- zt %>% group_by(site_no, julian, date) %>% summarize(Flow = median(Flow_Inst, na.rm=TRUE), .groups="drop")
    #In the streamflow data: The code summarizes the median 'Flow_Inst' at one point, but the data I've been pulling for TX has instantaneous flow for each total spillway releases, total reservoir discharge, and total tail race.
    
    zt <- zt %>% dplyr::select(site_no, date, julian, Flow);    colnames(zt) <- c("site", "date", "julian", "flow")
    zt <- zt %>% group_by(site, date, julian) %>% summarize(flow = median(flow, na.rm=TRUE), .groups="drop")
    
    year.flow <- rbind(year.flow, zt)
  }
  
  print(paste0(i, " is ", round(i/length(unique.sites)*100,2), "% done"))
}
summary(year.flow)

#bind old and new data
boerne.data <- rbind(boerne.data, year.flow) %>% arrange(site, date)
write.csv(boerne.data, paste0(swd_data, "streamflow/all_stream_data.csv"), row.names=FALSE)
#do rolling average etc next

#calculate 7 day rolling average (function is in global api)
#Check for missing days, if so, add NA rows: #https://waterdata.usgs.gov/blog/moving-averages/
current.year <-year(today);
year.flow  <- as.data.frame(matrix(nrow=0, ncol=4));    colnames(year.flow) <- c("site", "date", "julian", "flow")
stats <- as.data.frame(matrix(nrow=0,ncol=13));        colnames(stats) <- c("Site", "julian", "min", "flow10", "flow25", "flow50", "flow75", "flow90", "max", "Nobs","startYr","endYr","date"); 
for (i in 1:length(unique.sites)){
  zt <- boerne.data %>% filter(site==unique.sites[i])
  
  if(as.numeric(diff(range(zt$date))) != (nrow(zt)+1)){
    fullDates <- seq(from=min(zt$date), to = max(zt$date), by="1 day")
    fullDates <- data.frame(date = fullDates, stringsAsFactors = FALSE)
    zt <- full_join(zt, fullDates,by=c("date")) %>% arrange(date) %>% mutate(site=unique.sites[i], julian = as.POSIXlt(date, format = "%Y-%m-%d")$yday)
  }
  zt <- zt %>% mutate(rollMean=round(as.numeric(ma(flow)),4)) %>% mutate(year = year(date))
  
  #summarize annual
  zt.stats <- zt %>% group_by(site, julian) %>% summarize(Nobs = n(), min=round(min(rollMean, na.rm=TRUE),2), flow10 = round(quantile(rollMean, 0.10, na.rm=TRUE),2), flow25 = round(quantile(rollMean, 0.25, na.rm=TRUE),2),
                                                          flow50 = round(quantile(rollMean, 0.5, na.rm=TRUE),2), flow75 = round(quantile(rollMean, 0.75, na.rm=TRUE),2), flow90 = round(quantile(rollMean, 0.90, na.rm=TRUE),2), 
                                                          max = round(max(rollMean, na.rm=TRUE),2),  .groups="drop")
  
  zt.stats <- zt.stats %>% mutate(startYr = min(zt$year), endYr = max(zt$year)) %>% select(site, julian, min, flow10, flow25, flow50, flow75, flow90, max, Nobs, startYr, endYr)
  zt.stats$date2 <- as.Date(zt.stats$julian, origin=paste0(current.year,"-01-01"))
  zt.stats$date <- format(zt.stats$date2, format="%b-%d")
  
  # if(dim(zt.stats)[1] == 366) {zt.stats$date = julian$month.day366; }
  #  if(dim(zt.stats)[1] == 365) {zt.stats$date = subset(julian, julian <= 364)$month.day365; }
  
  zt <- zt %>% filter(year>=(current.year-2)) %>% dplyr::select(site, date, julian, rollMean);    colnames(zt) <- c("site", "date", "julian", "flow")
  
  #fill dataframe
  stats <- rbind(stats, zt.stats)
  year.flow <- rbind(year.flow, zt)
  print(paste0(i, " with ", round(i/length(unique.sites)*100,2), "% done"))
}
summary(stats)
summary(year.flow)

#for current stats - find out status of streamflow for sites and for flow points
#set up date
stats <- stats %>% mutate(date2 = as.Date(paste0(current.year,"-",date), "%Y-%b-%d")) %>% as.data.frame()

#Now attach most recent value to stream stats
recent.flow <- year.flow %>% group_by(site) %>% filter(is.na(flow)==FALSE) %>% filter(date == max(date))
current.stat <- merge(recent.flow[,c("site", "julian", "flow")], stats, by.x=c("site","julian"), by.y=c("site", "julian"))
current.stat <- current.stat %>% mutate(flow = round(flow, 2))

#if else
current.stat <- current.stat %>% mutate(status = ifelse(flow <= flow10, "Extremely Dry", ifelse(flow > flow10 & flow <= flow25, "Very Dry", ifelse(flow >= flow25 & flow < flow50, "Moderately Dry", 
                                                                                                                                                   ifelse(flow >= flow50 & flow < flow75, "Moderately Wet", ifelse(flow >= flow75 & flow < flow90, "Very Wet", ifelse(flow >= flow90, "Extremely Wet", "Unknown")))))))
current.stat$status <- ifelse(is.na(current.stat$status), "unknown", current.stat$status)
table(current.stat$status, useNA="ifany")
#set those that are not collecting data to unknown
current.stat <- current.stat %>% mutate(status = ifelse(endYr < current.year & julian > 30, "unknown", ifelse(endYr < (current.year-1), "unknown", status)))
table(current.stat$status)

#merge to geojson file with current status for map display
boerne.sites2 <- merge(boerne.sites, current.stat[,c("site","status","flow","julian","date","flow50")], by.x="site", by.y="site")
geojson_write(boerne.sites2, file=paste0(swd_data, "streamflow/all_stream_gauge_sites.geojson"))


#to create stats diagram with past and current year - need to make separate for dates and then rbind
year.flow2 <- year.flow %>% mutate(flow = round(flow, 2)) %>% filter(date >= as.Date(paste0(current.year, "-01-01"), "%Y-%m-%d"))
year.past <- year.flow %>% mutate(flow = round(flow, 2)) %>% filter(date >= as.Date(paste0((current.year-1),"-01-01"), "%Y-%m-%d") & date <= as.Date(paste0((current.year-1),"-12-31"), "%Y-%m-%d"))

stats2 <- merge(stats, year.flow2[,c("site", "julian", "flow")], by.x=c("site", "julian"), by.y=c("site", "julian"), all.x=TRUE)
stats.past <- merge(stats, year.past[,c("site", "julian", "flow")], by.x=c("site", "julian"), by.y=c("site", "julian"), all.x=TRUE) %>% mutate(date2 = as.Date(paste0((current.year-1),"-",date), "%Y-%b-%d")) %>% as.data.frame()
stats2 <- rbind(stats.past, stats2)

#stats2 <- stats2 %>% mutate(perFlow = round(flow/flow50*100,2)); summary(stats2)
stats2 <- stats2 %>% mutate(status = ifelse(flow <= flow10, "Extremely Dry", ifelse(flow > flow10 & flow <= flow25, "Very Dry", ifelse(flow >= flow25 & flow < flow50, "Moderately Dry", 
                                                                                                                                       ifelse(flow >= flow50 & flow < flow75, "Moderately Wet", ifelse(flow >= flow75 & flow < flow90, "Very Wet", ifelse(flow >= flow90, "Extremely Wet", "Unknown")))))))
#stats2 <- stats2 %>% mutate(month = substr(date,0,3))
stats2 <- stats2 %>% mutate(colorStatus = ifelse(status=="Extremely Dry", "darkred", ifelse(status=="Very Dry", "red", ifelse(status=="Moderately Dry", "orange", ifelse(status=="Moderately Wet", "cornflowerblue",
                                                                                                                                                                         ifelse(status=="Very Wet", "blue", ifelse(status=="Extremely Wet", "navy", "gray"))))))) %>% mutate(colorStatus = ifelse(is.na(colorStatus), "gray", colorStatus))
stats2 <- stats2 %>% arrange(site, date2)
table(stats2$status, useNA="ifany")
table(stats2$colorStatus, useNA="ifany")

#write out the csv to draw daily flow based on stream status
write.csv(stats2, paste0(swd_data, "streamflow/all_stream_stats.csv"), row.names=FALSE)


# #write out the current status for water supply watershed 
current.stat2 <- current.stat %>% select(site, julian, date, status) %>% mutate(month = substr(date,0,3))
site.huc <- cbind(boerne.sites2$site, boerne.sites2$huc8, boerne.sites2$ws_watershed) %>% as.data.frame(); colnames(site.huc) <- c("site", "huc8", "ws_watershed")
current.stat2 <- merge(current.stat2, site.huc, by.x="site", by.y="site", all.x=TRUE)
write.csv(current.stat2, paste0(swd_data, "streamflow/current_sites_status.csv"), row.names=FALSE)


# #copy over to triangle - NOT DONE FOR TX AS NO SPECIFIED AREA OF INTEREST BUT RETAINED IN CASE ADDED LATER
# t.sites <- read.csv("..//data//streamflow//sites_status.csv", colClasses = c("site" = "character"))
# t.sites2 <- nc.sites2 %>% filter(site %in% t.sites$site)
# geojson_write(t.sites2, file="../data/streamflow/stream_gauge_sites.geojson")
# 
# t.all <- all.data %>% filter(site %in% t.sites$site)
# write.csv(t.all, "..//data//streamflow//all_stream_data.csv", row.names=FALSE)
# 
# t.stats2 <- stats2 %>% filter(site %in% t.sites$site)
# write.csv(t.stats2, "..//data//streamflow//stream_stats.csv", row.names=FALSE)
# 
# t.current <- current.stat2 %>% filter(site %in% t.sites$site)
# write.csv(t.current, "..//data//streamflow//sites_status.csv", row.names=FALSE)


################################################################################################################################################################
# remove all except for global environment 
rm(list= ls()[!(ls() %in% c('julian.ref','update.date', 'current.month', 'current.year', 'end.date', 'end.year', 
                            'mymonths', 'source_path', 'start.date', 'state_fips', 'stateAbb', 'stateFips', 'swd_data', 'today', 
                            '%notin%', 'ma'))])







###################################################################################################################################################
#
# Creates initial map layers and downloads historic data for the dashboard
# CREATED BY VIANEY RUEDA 
# MARCH 2022
#
###################################################################################################################################################

######################################################################################################################################################################
#
#   Load old data
#
######################################################################################################################################################################
old.data <- read.csv(paste0(swd_data, "quality/historic_water_quality.csv"))
boerne_quality_sites <- read_sf(paste0(swd_data, "quality/water_quality_sites.geojson"))

######################################################################################################################################################################
#
#   READ IN QUALITY SITES AND DATA: Texas Stream Team Water Quality Data
#
######################################################################################################################################################################

#verify a google account to use the read_sheet function
#authenticate account
service_account_info <- Sys.getenv("GSHEET_SERVICE_ACCOUNT", "")

# Write the credentials to a temporary JSON file
temp_file <- tempfile(fileext = ".json")
writeLines(service_account_info, temp_file)

# Authenticate using the temporary JSON file
gs4_auth(path = temp_file)
all_quality_data <- read_sheet("https://docs.google.com/spreadsheets/d/1JAQLzSpbU2nMVb4Pe1XUA2lxU3a1XcY4oUYS8UhIaiA/edit#gid=826381059", col_types = "ccccnnnDTnnncnnnnnnnnn")

#filter for relevant sites
boerne_data <- all_quality_data %>% filter(Name %in% c("12600", "15126", "20823", "80186", "80230", "80904", "80966", "81596", "81641", "81671", "81672"))

#rename columns
boerne_data <- rename(boerne_data, site_id = Name, name = Description, basin = Basin, county = County, latitude = Latitude, longitude = Longitude, 
                      stream_segment = `TCEQ Stream Segment`, date = `Sample Date`, sample_depth = `Sample Depth (m)`, flow_severity = `Flow Severity`, 
                      conductivity = `Conductivity (µs/cm)`, dissolved_oxygen = `Dissolved Oxygent (mg/L)`, air_temp = `Air Temperature (°C)`, 
                      water_temp = `Water Temperature (°C)`, ecoli_avg = `E. Coli Average`, secchi_disk_transparency = `Secchi Disk Transparency (m)`,
                      nitrate_nitrogen = `Nitrate-Nitrogen (ppm or mg/L)`)

#filter for relevant data
boerne_data <- boerne_data[-c(9,11:12,21)]

#clean up
boerne_data <- as.data.frame(boerne_data)
boerne_data <- boerne_data %>% arrange(site_id, date)

# limit data to 2022
boerne_data <- boerne_data %>% mutate(year = year(date))
new_boerne_data <- boerne_data %>% filter(year >= 2022)

#combine new and old data
all_boerne_data <- rbind(old.data, new_boerne_data)

#save out
write.csv(all_boerne_data, paste0(swd_data, "quality/all_water_quality.csv"), row.names=FALSE)

################################################################################################################################################################
# remove all except for global environment 
rm(list= ls()[!(ls() %in% c('julian.ref','update.date', 'current.month', 'current.year', 'end.date', 'end.year', 
                            'mymonths', 'source_path', 'start.date', 'state_fips', 'stateAbb', 'stateFips', 'swd_data', 'today', 
                            '%notin%', 'ma'))])
