# Crawl spring migration example

# packages
library(move)
library(lubridate)
library(tidyverse)
library(crawl)
library(sf)
library(rworldmap)
library(dplyr)
library(sp)
library(rgdal)
library(ggspatial)
library(future)
library(furrr)
library(leaflet)
library(viridis)
library(geodist)

# 01 ****************************************************************************************************************
# subset data

# get data from movebank
# Same time period as Erik's SGS analysis
login <- movebankLogin(username = "sjclements9", password="!bobcatsdonthavecloacas0") 

amwo_2022 <- getMovebankData(study = "American Woodcock Migration Ecology in Eastern North America", 
                             login = login, timestamp_start = "20220101000000000", timestamp_end = "20220615000000000",
                             removeDuplicatedTimestamps=TRUE)
amwo_2022<- as.data.frame(amwo_2022)


amwo_2021 <- getMovebankData(study = "American Woodcock Migration Ecology in Eastern North America", 
                             login = login, timestamp_start = "20210101000000000", timestamp_end = "20211231000000000",
                             removeDuplicatedTimestamps=TRUE)
amwo_2021<- as.data.frame(amwo_2021)

amwo_2020 <- getMovebankData(study = "American Woodcock Migration Ecology in Eastern North America", 
                             login = login, timestamp_start = "20200101000000000", timestamp_end = "20201231000000000",
                             removeDuplicatedTimestamps=TRUE)
amwo_2020<- as.data.frame(amwo_2020)

amwo_2019 <- getMovebankData(study = "American Woodcock Migration Ecology in Eastern North America", 
                             login = login, timestamp_start = "20190101000000000", timestamp_end = "20191231000000000",
                             removeDuplicatedTimestamps=TRUE)
amwo_2019<- as.data.frame(amwo_2019)

amwo_all <- as.data.frame(rbind(amwo_2019, amwo_2020, amwo_2021, amwo_2022)) 


#make data frame of relevant columns
amwo <- data.frame(
  ID=amwo_all$local_identifier,
  time=amwo_all$timestamp,
  lon=amwo_all$location_long,
  lat=amwo_all$location_lat,
  sex=amwo_all$sex,
  tagtype=amwo_all$comments,
  age=amwo_all$taxon_detail,
  altitude=amwo_all$height_above_msl,
  migstage=amwo_all$migration_stage,
  bhstate=amwo_all$behavioural_classification,
  yday = yday(amwo_all$timestamp),
  year=year(amwo_all$timestamp)
)
head(amwo)
#amwo_save <- amwo
#amwo <- amwo_save

# Prepare times to match Erik's SGS/tracking paper code - 
# time is imported in "GMT" time and will need to be converted to "EST"
## first step is to assign "GMT" timestamp to current time  #%#tzone
##second step is to convert to "EST" time  #%#tzone_out
# while some timestamps occur in "CST", we will work with time in "EST", as all transmitters were programmed in "EST" 
amwo$time <- force_tzs(amwo$time, tzone = "GMT", tzone_out = "EST")
# round timesamp to the nearest hour, makes data easier to read/interpret
amwo$time<- as.POSIXct(round(amwo$time, "hour"))

#ensure time is in POSIXct format and that all times are correctly displayed in "EST"
#class(amwo$time)
#amwo$time[3]

# extracting date and time into seperate columns
amwo$t<- (strftime(amwo$time, format="%H:%M"))
amwo$date<- strftime(amwo$time, format="%Y-%m-%d")

## remove multiple locations on same day, mainly retaining only 1 of 3 locations 1st day after marking
amwo <- distinct(amwo, date, ID, .keep_all = TRUE)

# replace ?g with Xg because ? will cause issues later
amwo$tagtype[str_detect(amwo$tagtype, '\\?')] <- "Xg - lazarus"

# pull out VA
va <- amwo[str_detect(amwo$ID, 'VA'),]

#subset amwo data frame to Jan 5 to June 15
amwo <- amwo %>% filter(yday>5, yday<166)

# Eliminate birds that stop collecting data before May 1st
amwo <- amwo %>% 
  group_by(ID) %>%
  mutate(max_date_id = max(yday), npoints=length(ID), min_date_id=min(as_datetime(time))) %>%
  filter(max_date_id>122, npoints>10) %>%
  ungroup %>%
  as.data.frame()
#

va <- va %>% 
  group_by(ID) %>%
  mutate(max_date_id = max(yday), npoints=length(ID), min_date_id=min(as_datetime(time))) %>%
  filter(month(min_date_id) %in% c(12,1,2,3)) %>%
  filter(max_date_id>122, npoints>10) %>%
  ungroup %>%
  as.data.frame()
va <- va %>% filter(yday>5, yday<166) # then same date cutoff

# Get rid of non-winter capture states (and unfiltered va)

amwo <- amwo[!str_detect(amwo$ID, 'ME'),] 
amwo <- amwo[!str_detect(amwo$ID, 'PA'),] 
amwo <- amwo[!str_detect(amwo$ID, 'QUE'),] 
amwo <- amwo[!str_detect(amwo$ID, 'RI'),] 
amwo <- amwo[!str_detect(amwo$ID, 'VA'),] 
amwo <- amwo[!str_detect(amwo$ID, 'NY'),] 
amwo <- amwo[!str_detect(amwo$ID, 'WV'),] 
amwo <- amwo[!str_detect(amwo$ID, 'NS'),] 
amwo <- amwo[!str_detect(amwo$ID, 'VT'),]

# combine amwo (which no longer contains va birds) and va
amwo <- rbind(amwo, va) #212 birds

# write csv for use in interoplation and  stopover code
#setwd() # working directory if you want
#write.csv(amwo, file='G:\\My Drive\\Woodcock\\migration_strategies\\SPR_amwo_sub_full_121622.csv')

# 02 *********************************************************************************************************************
# Interpolating AMWO Data - Crawl
# Migration Strategies - SPRING
# SJC
# November 2022

# This script interpolates missing locations using the crawl pacakge
# Continuous time random walk model
# then creates maps 
# the output is full interpolated and original tracks (not combined yet)

# get data, this is from movebank, fall migration 2017-21 subset of 229
birds <- amwo
#birds <- read.csv('G:\\My Drive\\Woodcock\\migration_strategies\\SPR_amwo_sub_full_121622.csv')
birds$time <- as_datetime(birds$time, tz="EST") # this csv was converted to EST in the previous code

# Calcluate time gaps
amwo_list <- list() # separate 
ids <- unique(birds$ID)
for (i in 1:length(ids)){
  amwo_list[[i]] <- birds[which(birds$ID==ids[i]),]
  amwo_list[[i]]$timegap <- rep(0, nrow(amwo_list[[i]]))
  for (j in 2:nrow(amwo_list[[i]])){
    amwo_list[[i]]$timegap[j] <- as.numeric(abs(difftime(amwo_list[[i]]$time[j], amwo_list[[i]]$time[j-1], units='days')))
  }
  #  print(i)
}

birds <- bind_rows(amwo_list)
length(unique(birds$ID)) # 147
hist(birds$timegap)


# Group tracks by id (for crawl model because it likes tidyverse structure)
birds <- group_by(birds, ID) %>% nest()
birds

# Movement model (no covariates = ~1)
# The constraints are based on the godwit example
# Fit model to each bird. Constrain velocity autoregression parameter to c(-7,2)
# full effective range of the beta parameter (according to D Johnson)
birds <- mutate(birds[1:10,],
                fit = purrr::map(data, ~{crawl::crwMLE(mov.model=~1, data=.x, coord=c("lon", "lat"), # movement model = ~1 bc no covariates
                                                       Time.name="time",
                                                       #time.scale='hours', # sometimes need to turn this on, sometimes don't
                                                       method="L-BFGS-B", # there are different optimization methods, default didn't work but this did
                                                       constr=list(lower=c(-Inf,-7), upper=c(Inf,2)),
                                                       attempts=4)}
                )
)
birds
birds$fit[[1]]

# Predict hourly
birds <- mutate(birds,
                pred = purrr::map(fit, ~{
                  crawl::crwPredict(.x, predTime="24 hours", return.type='flat') %>% # use 24 hours, NOT 1 day (because it rounds to the unit)
                    crawl::crw_as_tibble()
                }
                )
)


birds
birds$pred[[1]]

birds[[4]][[1]] %>% 
  st_as_sf(coords = c("mu.x", "mu.y"), crs = 4326) %>% 
  mapview::mapview()

# in these predictions mu.x and y are locations, nu.x and y are location error
# loctype is o or p, o is the data and p is predicted locations
# it also keeps the rest of the data associated with the points

# Recombine predictions
birds_pred <- dplyr::select(birds,ID, pred) %>% unnest(cols=pred)
birdsp <- birds_pred
birdsp

birdsp <- as.data.frame(birdsp) # this is all locations, both observed and predicted
str(birdsp)
head(birdsp)

#write.csv(birdsp, file= 'G:\\My Drive\\Woodcock\\migration_strategies\\SPR_crawl_gap_sub_121622.csv')

#birdsp <- read.csv('G:\\My Drive\\Woodcock\\migration_strategies\\SPR_crawl_gap_sub_121622.csv')

# make plots - this was more just to see if it would work than anything else.
sf_use_s2(FALSE)

usa <- st_read(dsn='G:\\My Drive\\ebird_lab_paper\\spatial\\cb_2018_us_state_5m', layer='cb_2018_us_state_5m')
usa <- st_transform(usa, crs=crs("+proj=longlat +datum=NAD83 +no_defs")) # this is sf
usa_sf <- usa
usa <- st_simplify(usa)
usa <- as(usa, 'Spatial')
#usa <- fortify(usa)
usa_border_2 <- borders(usa, colour='gray40', fill='white')

# canada
can <- st_read('G:\\My Drive\\ebird_lab_paper\\spatial\\gpr_000b11a_e', layer='gpr_000b11a_e')
can <- st_transform(can, crs=crs("+proj=longlat +datum=NAD83 +no_defs")) # this is sf
can <- can %>% filter(PREABBR %in% c("Que.", "P.E.I.", "Man."  , "Ont."  , "N.B.", "N.L."  , "N.S." ))
can_sf <- can
can <- st_simplify(can)
can <- as(can, 'Spatial')
class(can)
#can <- fortify(can)
can_border_2 <- borders(can, colour='gray40', fill='white')

setwd('G:\\My Drive\\Woodcock\\migration_strategies\\current code\\Spring migration\\spring_crw_maps_121622')
track_map <- function(x){
  yam <- ggplot(x) +
    usa_border_2+
    can_border_2+
    geom_point(data=x, aes(x=mu.x, y=mu.y, group=locType, color=locType), size=2, alpha=0.5) +
    geom_path(data=x, aes(x=mu.x, y=mu.y, group=locType, color=locType), size=1, alpha=0.5) +
    coord_cartesian(xlim=c(-92,-60), ylim=c(28,50)) +
    theme_classic() + theme(axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
                            axis.title.y=element_blank(), panel.border=element_rect(color="black", fill=NA, size=0.5),
                            legend.justification=c(1,0), legend.position=c(1,0), legend.text=element_text(size=14),
                            legend.title=element_text(size=16, face="bold"),
                            legend.background=element_rect(fill="white", colour="black"),
                            panel.background = element_rect(fill="gray90")) +
    ggtitle(paste(x$ID[1],' | ', x$tagtype[1]))
  ggsave(plot=yam, filename=paste(x$ID[1], x$tagtype[1], 'crawl2.png'), width=4, height=4, units='in', device='png')
  #yam
}

# separate birds into a list and map in parallel
ids <- unique(birdsp$ID)
all_pr <- list()
for (i in 1:length(ids)){
  all_pr[[i]] <- birdsp[which(birdsp$ID==ids[i]),]
}

track_map(all_pr[[1]])
#lapply(all_pr, FUN=track_map)

# 03 ******************************************************************************************************************************
# combining interpolated and real data - SPRING

# 1. If there is an interpolated and real point on the same day, remove the interpolated point and keep the real point
# 2. If there is a data gap of more than 3 days get rid of all interpolated data within those three days

#amwo <- read.csv('G:\\My Drive\\Woodcock\\migration_strategies\\SPR_crawl_gap_sub_121622.csv')
amwo <- birdsp
head(amwo)


# call all location data lat/lon
amwo$lon <- amwo$mu.x
amwo$lat <- amwo$mu.y

# fill in date for all points
amwo$date <- as_date(amwo$time)
amwo$yday <- yday(amwo$time)

# then need to combine the real and fake data
# if 2 points on the same date, keep the original only
# if more than a 3 day time gap, get rid of interpolated points within that gap

# fill timegap up so that any with loctype p and timegap >3 can be eliminated later
# Note that timegap for the interpolated points is the gap between the two points they are filling
# NOT the gap between that point and the previous point
amwo <- amwo %>%
  dplyr::group_by(ID) %>%
  fill(timegap, .direction = "up") %>%
  dplyr::ungroup() %>%
  as.data.frame()

# add keep column for additional filtering
amwo$keep='Y'

# Then eliminate duplicate real and fake points on the same day and remove interoplated data with 
# more than 3 days between the real data points it's based on
# separate data
ids <- unique(amwo$ID)
brd <- list()
for (i in 1:length(ids)){
  brd[[i]] <- amwo[which(amwo$ID==ids[i]),]
} 

# cut out fake points (p) on days when there is already a real point (o)
frankendata <- list() # this will be the combination of fake and real data
for (i in 1:length(brd)){
  tmp <- brd[[i]]
  length(unique(tmp$date)) 
  nrow(tmp) #143 points
  dates <- unique(tmp$date)
  dates_list <- list()
  for (q in 1:length(dates)){
    dates_list[[q]] <- tmp[which(tmp$date==dates[q]),]
    if (nrow(dates_list[[q]])>1){
      dates_list[[q]]$keep[which(dates_list[[q]]$locType=='p')] <- 'N'
    }
  }
  yam <- bind_rows(dates_list)
  frankendata[[i]] <- yam[which(yam$keep=='Y'),]
}

# then eliminate predicted points between gaps of more than 3 days 
for (i in 1:length(frankendata)){
  for (j in 1:nrow(frankendata[[i]])){
    if (frankendata[[i]]$locType[j]=='p'&frankendata[[i]]$timegap[j]>3){
      frankendata[[i]]$keep[j] <- 'N'
    }
  }
  frankendata[[i]] <- frankendata[[i]][which(frankendata[[i]]$keep=="Y"),]
}


track_map2 <- function(x){
  yam <- ggplot(x) +
    #mp+
    usa_border_2+
    can_border_2+
    geom_point(data=x, aes(x=mu.x, y=mu.y, group=locType, color=locType), size=2, alpha=0.5) +
    geom_path(data=x, aes(x=mu.x, y=mu.y), size=1, alpha=0.5) +
    coord_cartesian(xlim=c(-92,-60), ylim=c(28,50)) +
    #scale_color_manual(values=c("#2166ac", "black", "darkorange3")) +
    theme_classic() + theme(axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
                            axis.title.y=element_blank(), panel.border=element_rect(color="black", fill=NA, size=0.5),
                            legend.justification=c(1,0), legend.position=c(1,0), legend.text=element_text(size=14),
                            legend.title=element_text(size=16, face="bold"),
                            legend.background=element_rect(fill="white", colour="black"),
                            panel.background = element_rect(fill="gray90")) +
    ggtitle(paste(x$ID[1],' | ', x$tagtype[1], "| combine"))
  ggsave(plot=yam, filename=paste(x$ID[1], x$tagtype[1], 'combine.png'), width=4, height=4, units='in', device='png')
}


track_map <- function(x){
  yam <- ggplot(x) +
    #mp+
    usa_border_2+
    can_border_2+
    geom_point(data=x, aes(x=mu.x, y=mu.y, group=locType, color=locType), size=2, alpha=0.5) +
    geom_path(data=x, aes(x=mu.x, y=mu.y), size=1, alpha=0.5) +
    coord_cartesian(xlim=c(-92,-60), ylim=c(28,50)) +
    #scale_color_manual(values=c("#2166ac", "black", "darkorange3")) +
    theme_classic() + theme(axis.title.x=element_blank(), axis.text=element_blank(), axis.ticks=element_blank(),
                            axis.title.y=element_blank(), panel.border=element_rect(color="black", fill=NA, size=0.5),
                            legend.justification=c(1,0), legend.position=c(1,0), legend.text=element_text(size=14),
                            legend.title=element_text(size=16, face="bold"),
                            legend.background=element_rect(fill="white", colour="black"),
                            panel.background = element_rect(fill="gray90")) +
    ggtitle(paste(x$ID[1],' | ', x$tagtype[1], "| combine"))
  ggsave(plot=yam, filename=paste(x$ID[1], x$tagtype[1], 'combine.png'), width=4, height=4, units='in', device='png')
}

# a map out for each bird with an observed and predicted track
plan(multisession) 
future_map(frankendata, track_map) 
plan(sequential)
#lapply(frankendata, FUN=track_map)

amwo <- bind_rows(frankendata)

write.csv(amwo, file='G:\\My Drive\\Woodcock\\migration_strategies\\SPR_amwo_combine_121622.csv')