
#rm(list=ls())
#setwd("C:/Users/Erik/Desktop/R")

#install.packages("momentuHMM")

library(momentuHMM)
library(move)
library(tidyverse)
library(lubridate)
############################################
# import amwo locations data from Movebank
  ## use the 'timestamp_start' and 'timestamp_end' to select specific data periods
login <- movebankLogin(username = "acfish", password="EPvzvNSUJ8")
amwo_2020 <- getMovebankData(study = "American Woodcock Migration Ecology in Eastern North America", 
                               login = login, timestamp_start = "20200105000000000", timestamp_end = "20200630000000000",
                               removeDuplicatedTimestamps=TRUE)

amwo_2020<- as.data.frame(amwo_2020)

amwo_2019 <- getMovebankData(study = "American Woodcock Migration Ecology in Eastern North America", 
                             login = login, timestamp_start = "20190105000000000", timestamp_end = "20190630000000000",
                             removeDuplicatedTimestamps=TRUE)

amwo_2019<- as.data.frame(amwo_2019)

amwo_all <- as.data.frame(rbind(amwo_2019, amwo_2020))

# use to remove and look at individual birds

#VT_bird<- subset(amwo_data, amwo_data$tag_local_identifier=="176559")
#View(VT_bird)



# import data - movebank avenue
# extract ID, timestamp, and lat/long data; then rename variables
amwo_data <- data.frame(ID=amwo_all$local_identifier,
                        time=amwo_all$timestamp,
                        lon=amwo_all$location_long,
                        lat=amwo_all$location_lat,
                        tagtype=amwo_all$comments,
                        sex=amwo_all$sex,
                        age=amwo_all$taxon_detail,
                        altitude=amwo_all$height_above_msl)



# time is imported in "GMT" time and will need to be converted to "EST"
  ## first step is to assign "GMT" timestamp to current time  #%#tzone
  ##second step is to convert to "EST" time  #%#tzone_out
       # while some timestamps occur in "CST", we will work with time in "EST", as all transmitters were programmed in "EST" 
amwo_data$time <- force_tzs(amwo_data$time, tzone = "GMT", tzone_out = "EST")

# round timesamp to the nearest hour, makes data easier to read/interpret
amwo_data$time<- as.POSIXct(round(amwo_data$time, "hour"))

#ensure time is in POSIXct format and that all times are correctly displayed in "EST"
class(amwo_data$time)
amwo_data$time[3]

# extracting date and time into seperate columns
amwo_data$t<- (strftime(amwo_data$time, format="%H:%M"))
amwo_data$date<- strftime(amwo_data$time, format="%Y-%m-%d")

## remove multiple locations on same day, mainly retaining only 1 of 3 locations 1st day after marking
amwo_data <- distinct(amwo_data, date, ID, .keep_all = TRUE)

#summary(amwo_data$ID)
#amwo_data$ID

## prepData function will only work on individuals with >3 locations
## creat summary table of individual ID and identify which AMWO need to be removed
  ## prior to conducting prepData
rm.ID <- as.data.frame(tally(group_by(amwo_data, ID)))

## remove birds with <3 locations
rm.ID<- subset(rm.ID, rm.ID$n<3)


   ##output is numerical indicator of bird, need to manually enter to create remove.list obj
remove.list <- as.list(c(as.character(rm.ID$ID)))
remove.list

# remove all individuals from amwo_data
amwo_data <- amwo_data %>% filter(!(ID %in% remove.list))

# converting all age classes to juvenile and adult
amwo_data$age <- as.character(amwo_data$age)
amwo_data$age[amwo_data$age == "Second Year"] = "juv"
amwo_data$age[amwo_data$age == "Hatch Year"] = "juv"
amwo_data$age[amwo_data$age == "After Hatch Year"] = "ad"
amwo_data$age[amwo_data$age == "After Second Year"] = "ad"
amwo_data$age[amwo_data$age == "Third Year"] = "ad"
amwo_data$age[amwo_data$age == "After Third Year"] = "ad"
amwo_data$age <- as.factor(amwo_data$age)

#extracting tag type (size) and schedules from data
amwo_data$tagtype <- as.character(amwo_data$tagtype)
amwo_data <- amwo_data %>%
  separate(tagtype, c("tagtype", "second", "schedule"), sep = " ") %>% dplyr::select(-second)
amwo_data$tagtype <- as.factor(amwo_data$tagtype)
amwo_data$schedule <- as.factor(amwo_data$schedule)

# extracting state marked and year from data
amwo_data$ID <- as.character(amwo_data$ID)
amwo_data$ID2 <- amwo_data$ID
amwo_data <- amwo_data %>%
  separate(ID2, c("m.state", "m.year", "third"), sep = "-") %>% select (-third)
amwo_data$m.state <- as.factor(amwo_data$m.state)
amwo_data$ID <- as.factor(amwo_data$ID)
amwo_data$m.year <- as.factor(amwo_data$m.year)

## filtering data by sex, age, tagtype, schedule, etc.
  ## Note: to parse code, remove '#' from begining of function and before '%>%' if running multiple filtering functions

## extract just male woodcock

amwo_male <- amwo_data %>%
  filter(sex == "m") #%>%  #"m" for male, "f" for female
  #filter(age == "juv") #%>%  # 'juv' for subadults, 'ad' for adults
  #filter(tagtype == "4g") #%>%  # options include '4g', '5g', or '6.3g'
  #filter(schedule == "Night") #%>%  # options include 'Frequent', Infrequent', 'Hybrid', or 'Night'
  #filter(schedule != "Night") #%>%  # used to remove data from dataframe; will also remove NA data (from what I can gather)
  #filter(month(time) <= 5 & month(time) >= 3) #%>%  # filting dates to subset specific time periods

amwo_male$ID<- droplevels(amwo_male$ID)

summary(amwo_male$ID)

## Is it possible we monitor the same bird over 2 years?  

amwo_male$ID2<- amwo_male$ID

amwo_male$year<- strftime(amwo_male$time, format="%Y")

amwo_male$ID<- paste(amwo_male$ID2, amwo_male$year, sep="-")
  
##############################################################  
# create monentuHMMData object; would specify covariates if included in the dataset
      ##  basically the step length and turn angles are now included
amwo_male$x <-amwo_male$lon
amwo_male$y <-amwo_male$lat
amwoData <- prepData(amwo_male, type = "LL")
amwoData$step <- round(amwoData$step, digits = 2)

#plot step length.  Log transformation is insightful 

hist(log(amwoData$step), breaks=40)


## remove birds with no individual step lengths >16.1km (10 miles)
## in practice this pulls birds that never initiate a substantial migratory movement
## but doesn't penalize birds that DO migrate, as they always make 
## at least one substantial movement.

amwoData$min.step <- ifelse(amwoData$step>16.1, 1,0)

amwoData <- amwoData[(amwoData$ID %in% amwoData$ID [amwoData$min.step == 1]) , ]

## assign a starting state for each individual 

## rank observations (requires correct sorting)

amwoData2<- amwoData %>%
  group_by(ID) %>%
  mutate(ranks = row_number())

## ifelse to covert first observations to state 1 and all others NA

knownStates<- ifelse(amwoData2$ranks == "1", 1, NA)


##### trying 3-state model with fixed state transitions

# label states and create distributions for movements
stateNames <- c("pre","migration", "post")
nsim <- 25 # number of simulations
dist = list(step="gamma", angle="wrpcauchy")

## fix step length parameters and state transitions


fixPar<- list(beta=c(NA, -100, # pre-migration to transitions, can't go pre- to post
                     -100, NA,  #once entered migration can't go back to pre
                     -100, -100)) #once entred post-migration, can't leave

#Liam note: ask erik where these came from
#These serve as the starting parameters for the HMM to optimize
Par0_m1 <- list(step=c(0.485, 211.927, 0.485, #mean in km
                       0.7327, 289.599, 0.7327, # SD in km
                       0.026, 0.036, 0.026), #zeromass ie how many zeroes expected in the distribution
                angle=c(0, 0, 0, #mean
                        0.5, 0.5, 0.5)) #concentration
## for some unknown reason, these angle means and concetrations work, but not output for 'initial_parameter' code
#stateNames <- c("stationary","migratory","stopover")  # three state attmept but did not compute
#Par0_m1 <- list(step=c(300,500000,10000,
#                       100,100000,5000), 
#                angle=c(0.3,0.7, 0))

m1 <- fitHMM(data=amwoData, nbStates=3, dist=dist, Par0 = Par0_m1,
             estAngleMean = list(angle=TRUE),
             fixPar=fixPar, 
             #knownStates = knownStates,
             stateNames = stateNames, nsim = nsim)
m1

plot(m1, plotCI = TRUE)

plotPR(m1)



#predict states for each point(predicted and observed) and input in the dataset
## can then view amwoData to see state assigned to each location.
states <- viterbi(m1)
amwoData$states <- states

# try leaflet viewer

#install.packages("leaflet")
library(leaflet)

# write function to extract leaflet map

leaflet.male<- function(x){
  
  # color pallate for markers
  
  
  
  getColor <- function(state) {
    sapply(amwoDataID$states, function(states) {
      if(states == 1) {
        "green"
      } else if(states == 2) {
        "orange"
      } else {
        "red"
      } })
  }
  
  icons <- awesomeIcons(
    icon = 'ios-close',
    iconColor = 'black',
    library = 'ion',
    markerColor = getColor(amwoDataID)
  )
  
  
  m<- leaflet(options = leafletOptions(zoomControl = TRUE,
                                       minZoom = 1, maxZoom = 22,
                                       dragging = TRUE)) %>%
    addTiles() %>% # Default base mape
    addProviderTiles("Esri.WorldImagery") %>%  # ortho image
    addProviderTiles(providers$Stamen.TonerLines) %>% # state lines and roads.
    #addProviderTiles(providers$Stamen.TonerLabels) %>% # add location and road labels
    addAwesomeMarkers(lng=amwoDataID$lon, 
                      lat=amwoDataID$lat, 
                      icon=icons,
                      popup = amwoDataID$study.local.timestamp) %>%
    addPolylines(lng =amwoDataID$lon, 
                 lat = amwoDataID$lat, 
                 weight=3, color="red")
  
  
  m 
  
}

# clunky, but basically extract individuals 1 at a time and look at them

amwoID<- unique(amwoData$ID)

amwoDataID<- subset(amwoData, amwoData$ID=="MD-2020-10-2020")
leaflet.male(amwoDataID)


#check individual ID and end date of path

tail(amwoDataID)

# view all data if needed

utils::View(amwoDataID)

leaflet.male(amwoDataID)

#Liam note: anything below this point seems to be either tangental or non-functional

## lets play around with moveVis


# library(moveVis)
# library(move)
# 
# amwoMove<- df2move(df=amwoData, x="lon", y="lat",
#                    proj="+proj=longlat +datum=WGS84", 
#                    time="time", track_id="ID")
# 
# #data(amwoMove, package = "moveVis") # move class object
# # if your tracks are present as data.frames, see df2move() for conversion
# 
# # align move_data to a uniform time scale
# m <- align_move(amwoMove, res = 24, unit = "hours")
# 
# amwoData$colour<- ifelse(amwoData$states==1, "blue", ifelse(amwoData$states==2, "red", "purple"))
# 
# amwoMove$colour<- amwoData$colour
# 
#   
# frames <- frames_spatial(m, path_legend=FALSE, tail_length = 3, map_res = 0.35,
#                            map_service = "osm", map_type = "watercolor", alpha = 0.5) %>% 
#   add_labels(x = "Longitude", y = "Latitude", title = "American Woodcock Migration - Fall 2019") %>% # add some customizations, such as axis labels
#   add_northarrow() %>% 
#   add_scalebar() %>% 
#   add_timestamps(m, type = "label") %>% 
#   add_progress()
# 
# frames[[10]] # preview one of the frames, e.g. the 100th frame
# 
# # animate frames
# ## 'fps' is frames per second, and changing this will control how quickely frames transitions in .gif
# ## 'end_pause' determines is there if the /gif pauses post animation
# animate_frames(frames,end_pause = 0, fps = 20, out_file = "G:/My Drive/PhD_AMWO/Animmations/fall2019_all2.gif")
# 
# 
#   
#   
# # create spatial frames with a OpenStreetMap watercolour map
# frames <- frames_spatial(m, 
#                          path_legend=FALSE, 
#                          tail_length = 3, 
#                          map_res = 0.35,
#                          map_service = "osm", 
#                          map_type = "watercolor", 
#                          alpha = 0.5) %>% 
#   add_labels(x = "Longitude", y = "Latitude",
#              title = "American Woodcock Male Spring Migration") %>% # add some customizations, such as axis labels
#   add_northarrow() %>% 
#   add_scalebar() %>% 
#   add_timestamps(m, type = "label") %>% 
#   add_progress()
# 
# frames[[100]] # preview one of the frames, e.g. the 100th frame
# 
# # animate frames
# animate_frames(frames, out_file = "moveVis.gif", 
#                overwrite=TRUE,
#                fps=10,
#                end_pause=5)


### Post-Hmm state assignment data management
## assign ordinal date and predict mid-point for each 
## point-point location 

amwoData$Ordinal<- yday(amwoData$date)

## loop through ordinal date and delineate the midpoint
## for each date gap

## first the midpoint between i and i+1, which gives the theoretical departure date

amwoData$Mid.leave<- NA

for (i in 1:nrow(amwoData)){
  
  amwoData$Mid.leave[i]<- ifelse(amwoData$ID[i]==amwoData$ID[i+1], 
                               (amwoData$Ordinal[i+1]+amwoData$Ordinal[i])/2,
                               NA)
  
}

## next the midpoint between i and i-1, which gives the midpoint for arrival 

amwoData$Mid.arrive<- NA

for (i in 2:nrow(amwoData)){
  
  amwoData$Mid.arrive[i]<- ifelse(amwoData$ID[i]==amwoData$ID[i-1], 
                                 (amwoData$Ordinal[i-1]+amwoData$Ordinal[i])/2,
                                 NA)
  
}

## Define the state transition points for initiation and termination

amwoData$Initiate<- NA

amwoData$Initiate[1]<- 0

for (i in 2:nrow(amwoData)){
  
  amwoData$Initiate[i]<- ifelse(amwoData$states[i]==2 & amwoData$states[i-1]==1, 
                                 1,0)      
}

amwoData$Terminate<- NA

amwoData$Terminate[1]<- 0

for (i in 2:nrow(amwoData)){
  
  amwoData$Terminate[i]<- ifelse(amwoData$states[i]==3 & amwoData$states[i-1]==2, 
                                1,0)      
}

## also calculate the mean precision of locations as the distance between the mid-point
## and the actual location date.

amwoData$precision<- amwoData$Ordinal-amwoData$Mid.arrive

mean(amwoData$precision, na.rm=TRUE)
sd(amwoData$precision, na.rm=TRUE)

## extract all state 2 locations that are not initiation points

amwo.Migrate<- subset(amwoData, amwoData$states==2 & amwoData$Initiate==0)
nrow(amwo.Migrate) #sample size (locations)

amwo.Migrate$ID<-droplevels(amwo.Migrate$ID)

length(levels(amwo.Migrate$ID)) #sample size (individuals)

## extract initiation points

amwo.Initiate<- subset(amwoData, amwoData$states==2 & amwoData$Initiate==1)
nrow(amwo.Initiate) #sample size (locations)

amwo.Initiate$ID<-droplevels(amwo.Initiate$ID)

length(levels(amwo.Initiate$ID)) #sample size (individuals)


## extract termination points 
## Note to self - need to do the crosswalk to see if the early 
## state 3 terminations need to be dropped for birds not followed
## into June

amwo.Terminate<- subset(amwoData, amwoData$states==3 & amwoData$Terminate==1)
nrow(amwo.Terminate) #sample size (locations)

amwo.Terminate$ID<-droplevels(amwo.Terminate$ID)

length(levels(amwo.Terminate$ID)) #sample size (individuals)

#### Migration (stopover) timing.  These are based on the actual 
#### measured days

## convert back to day of year

amwo.Migrate$Day<- day(amwo.Migrate$date)
amwo.Migrate$Month<- month(amwo.Migrate$date)
head(amwo.Migrate$Month)

amwo.Migrate$MonthDay<- as.Date(paste(amwo.Migrate$Month, 
                                      amwo.Migrate$Day, sep="/"),
                                "%m/%d")


# sample size

nrow(amwo.Migrate)

## density plot of migratory locations

mig.plot<- ggplot(amwo.Migrate, aes(x=MonthDay, y=lat, color=year)) + geom_point()+
  #geom_vline(aes(xintercept="5-Apr"), linetype='dashed', color='red')+
  geom_hline(aes(yintercept=44), linetype='dashed', color='blue')+
  scale_x_date(date_labels = "%d-%b", date_breaks="4 weeks")+
  theme_classic()

mig.plot


mig.dens<- ggplot(amwo.Migrate, aes(x=MonthDay, fill=year)) +
  geom_density(alpha=.5)+
  scale_x_date(date_labels = "%d-%b", date_breaks="4 weeks")+
  xlab("")+
  ylab("Relative frequency\nof migration")+
  theme_classic()
mig.dens  

amwo.mig.late<- subset(amwo.Migrate, amwo.Migrate$Ordinal>150)


## Initiation timing - these are based on the Mid.leave date,
## or the halfway point between the last date known at the 
## stopover and the first date known on migration

## Note this incorrectly creates all 2019 dates, but for visulaization
## that's no big deal.

amwo.Initiate$Date.leave<- as.Date(amwo.Initiate$Mid.leave, origin = "2019-01-01")

## create MonthDay term

amwo.Initiate$Day<- day(amwo.Initiate$Date.leave)
amwo.Initiate$Month<- month(amwo.Initiate$Date.leave)
head(amwo.Initiate$Month)

amwo.Initiate$MonthDay<- as.Date(paste(amwo.Initiate$Month, 
                                      amwo.Initiate$Day, sep="/"),
                                "%m/%d")

## density plot of migratory locations

Init.plot<- ggplot(amwo.Initiate, aes(x=MonthDay, y=lat, color=year)) + geom_point()+
  #geom_vline(aes(xintercept="5-Apr"), linetype='dashed', color='red')+
  geom_hline(aes(yintercept=44), linetype='dashed', color='blue')+
  scale_x_date(date_labels = "%d-%b", date_breaks="4 weeks")+
  theme_classic()

Init.plot


Init.dens<- ggplot(amwo.Initiate, aes(x=MonthDay, fill=year)) +
  geom_density(alpha=.5)+
  scale_x_date(date_labels = "%d-%b", date_breaks="4 weeks")+
  xlab("")+
  ylab("Relative frequency\nof initiation")+
  theme_classic()
Init.dens  


## termination timing - these are based on the Mid.arrive date,
## or the halfway point between the last date known at the 
## stopover and the first date known on migration

## Need to cross-check for premature termination 

## Note this incorrectly creates all 2019 dates, but for visualization
## that's no big deal.

amwo.Terminate$Date.arrive<- as.Date(amwo.Terminate$Mid.arrive, origin = "2019-01-01")

## create MonthDay term

amwo.Terminate$Day<- day(amwo.Terminate$Date.arrive)
amwo.Terminate$Month<- month(amwo.Terminate$Date.arrive)
head(amwo.Terminate$Month)

amwo.Terminate$MonthDay<- as.Date(paste(amwo.Terminate$Month, 
                                       amwo.Terminate$Day, sep="/"),
                                 "%m/%d")

## density plot of migratory locations

Term.plot<- ggplot(amwo.Terminate, aes(x=MonthDay, y=lat, color=year)) + geom_point()+
  #geom_vline(aes(xintercept="5-Apr"), linetype='dashed', color='red')+
  geom_hline(aes(yintercept=44), linetype='dashed', color='blue')+
  scale_x_date(date_labels = "%d-%b", date_breaks="4 weeks")+
  theme_classic()
Term.plot


Term.dens<- ggplot(amwo.Terminate, aes(x=MonthDay, fill=year)) +
  geom_density(alpha=.5)+
  scale_x_date(date_labels = "%d-%b", date_breaks="4 weeks")+
  xlab("")+
  ylab("Relative frequency\nof initiation")+
  theme_classic()
Term.dens 

hist(amwo.Terminate$Mid.arrive)

## Check to see if there is an issue with termination dates
## for birds who blink out early.  Create column to signify whether 
## bird was monitored in June or not







