##########################################################################
### WORKSHOP - ANALYSIS OF TRACKING DATA - TOUR DU VALAT              ####
### 26 SEPTEMBER 2024                                                 ####
###                                                                   ####
### by Wouter M.G. Vansteelant (wouter@birdeyes.org)                  ####
### last update: 25 Sept 2024                                         ####
##########################################################################

### CASE STUDY Flamingos from Tour du Valat
# PART 1: Reading and (pre)processing of tracking data
#         --> starting an R project, loading packages and functions, 
#             loading (meta)data, formatting datetimes, extracting 
#             useful (grouping) variables for tracking data analyses
#
#         
# PART 2: Cleaning and standardising tracking data
#         --> calculate movement statistics, flag/remove unrealistic locations,
#             resample data to fixed temporal intervals, calculate aggregate 
#             movement statistics
#
#
# PART 3: Daynight classification --> solar times and exploring diel patterns
#         in movement
#
#
# PART 4: Classifying and segmenting tracks according to behaviour
#         ---> searching classification criteria, segmenting tracks, 
#              explore spatial and temporal patterns in your data
#
#
# PART 5: Environmental annotation (land use)


### PART 1: Reading and (pre)processing of tracking data  ###
#############################################################

## STEP 1.1: LOAD ALL REQUIRED PACKAGES
#######################################
# Create function to install and load a list of packages: this function will install all packages which
# you have not installed before and then load all the packages we need for our script
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Make list of packages needed to run our code
packages <- c("lubridate","tidyverse","fossil","ggplot2","sf","move","suntools","tidyterra","maptiles","rnaturalearth")

# lubridate: package with useful functions to extract time variables from datetime column
# fossil: some useful functions for distance calculations
# tidyverse: highly recommended to easily compute summary statistics, aggregate variables, ...
# sf: geospatial operations 
# ggplot2: highly recommended for plotting (graphs and maps). Steep learning curve, but extremely 
#          versatile paakcage that allows you to plot pretty much anything you can imagine
# move: package to query data directly from Movebank projects (NOT USED IN WORKSHOP)
# suntools: functions for calculating local sunrise/sunset times (https://cran.r-project.org/web/packages/suntools/suntools.pdf)
# tidyterra: wrangle spatial data in tidyverse syntax
# maptiles: obtain satellite imagery
# rnaturalearth: obtain open access spatial data from www.naturalearthdata.com

# apply ipak() function to list of packages
ipak(packages)


## STEP 1.2: LOAD (META)DATA
####################################################
### OPTION 1: load data directly from Movebank study 
## Specify your Movebank login
username = "USER"
password = "PASSWORD"
login<-movebankLogin(username,password)

## Load metadata table
meta <- as.data.frame(getMovebankReferenceTable(study=652989041,login=login))

## Load tracking data table
tracks <- as.data.frame(getMovebankData(study=652989041,login=login,removeDuplicatedTimestamps=TRUE))
###


### OPTION 2: load and combine seperate csv files for multiple individuals
# Create function to read data from multiple csv files stored in the same folder
#read_csv_files <- function(flnm) {
#  read_delim(flnm,delim=',') %>% 
#    mutate(filename = flnm)
#}

# Apply the created function to read all csv files in the folder 'data - tracking'
file_names <- dir("./data - tracking/individual tracks/")
tracks <- do.call(rbind,lapply(paste("./data - tracking/individual tracks/",file_names,sep=""),read.csv))
###


### OPTION 3: load data from csv files stored on desktop
# Load combined tracking data table
tracks <- read.csv("./data - tracking/Phoenicopterus roseus.csv",row.names = "event.id")

# Load metadata table
meta <- read.csv("./data - tracking/Phoenicopterus roseus-reference-data.csv")

# check structure of both object: pay attention to format of each column (character, numeric, ...)
str(meta)
str(tracks)
###

## STEP 1.3: COMBINE METADATA AND TRACKING DATA + FORMAT TIMESTAMPS
######################################################################
# I like to have consistent column names. So before merging tables, I make sure columns with 
# shared values have consistent formatting, column names, ... 
# This way, the merge() function we will use later will know which columns can be matched between
# the 'tracks' and 'meta' dataframes. 
# Alternatively, you can choose to keep column names, and specify which columns need to be matched
# within the merge() function or comparable functions. 
# See here for examples: https://sparkbyexamples.com/r-programming/r-join-multiple-columns/
colnames(meta)[2] <- "individual.local.identifier" #change name to match with tracking data table
colnames(meta)[1] <- "tag.local.identifier" #change name to match with tracking data table

# Use merge() function to combine metadata and tracking data into one table.
# Note that I specify what columns I want to keep from each dataframe.
data <-  merge(tracks[,c("tag.local.identifier","individual.local.identifier","timestamp","location.lat","location.long","ground.speed","height.above.msl","heading","external.temperature")],
               meta[,c("tag.local.identifier","individual.local.identifier","deploy.on.date","deploy.off.date","animal.life.stage","animal.nickname","animal.ring.id","deployment.id")],
               by=c("individual.local.identifier","tag.local.identifier"),
               all.x=TRUE)

# check result: note data has same number of rows as tracks, and retains only 15 columns, as expected
str(data)

# looks good, so we can remove the "tracks" and "meta" dataframes from memory
rm(tracks,meta)

# Simplify column names: OPTIONAL/PERSONAL PREFERENCE
# If you prefer to keep Movebank column names, then you'll need to adjust code that follows accordingly.
colnames(data) <- c("ID","dev","dt","lat","long","spd_gps","alt_msl","heading","temp","session_start","session_end","age","cring","ring","session_ID")

str(data)

# Specify dt and start/end columns as a POSIXct objects.
# This step is crucial to ensure R understands these are "time" variables, and to 
# apply simple functions on this kind of data. 
data$dt <- as.POSIXct(strptime(data$dt, format="%Y-%m-%d %H:%M:%OS"), tz='UTC')
data$session_start <- as.POSIXct(strptime(data$session_start, format="%Y-%m-%d %H:%M:%OS"), tz='UTC')
data$session_end <- as.POSIXct(strptime(data$session_end, format="%Y-%m-%d %H:%M:%OS"), tz='UTC')

# FILTER: remove data outside bounds of deployment
data <- subset(data,data$dt >= data$session_start & ((data$dt < data$session_end + 1)  | is.na(data$session_end) == TRUE))

# Once dt is POSIXct object we can easily extract all kinds of specific time variables 
# using the as.Date() function and a multitide of datetime-wrangling functions from the
# lubridate package (https://lubridate.tidyverse.org)
data$date <- as.Date(data$dt,tz='UTC') # extract date 
data$doy <- yday(data$dt) # extracts day of year (1-365)
data$yr <- year(data$dt)
data$mth <- month(data$dt)

# Explore unique values for categorical/integer variables
unique(data$dev)
length(unique(data$dev))

unique(data$ID)
length(unique(data$ID))

## NOTE: one of the tags is named incorrectly! c04eea81e2ba should be 8444
# It's unclear how this error emerged, and ideally should be fixed within
# the Movebank dataset. But we can just correct this here at the start
# of our work flow!
data$dev <- ifelse(data$dev == "c04eea81e2ba","8444",data$dev)

# Tag numbers could accidentally end up being interpreted as a numeric variable 
# in some operations. To ensure tag numbers are treated categorically, it helps 
# to add a letter or string to the beginning of the number.
data$dev <- paste("Tag_",data$dev,sep="")

# check output
unique(data$dev)

## NOTE: the bird "IDs" are a bit verbose. In fact, each bird ID includes the species name, 
# which is inconsistent with the Movebank data model, since there is an "animal.taxon" column 
# where species names can be stored. Furthermore, by including species names the ID-values
# are very long, which can be quite a nuisance in a later stage, for example when we start 
# making figures and maps, as an ID-legend would require a lot of space. 
# Therefore, lets simplify the ID column values by extracting only the ring number from
# the current text string. This can be done using various approaches. 
# Manipulating text strings in R can be a bit complicated, but I found a nice solution
# on this page: https://www.spsanderson.com/steveondata/posts/2024-06-25/
# using this google search: "extract part of string between brackets R"
data$ID <- sub(".*\\[(.*?)\\].*", "\\1", data$ID)

# check result
unique(data$ID)

# Explore summary statistics for specific variables
max(data$alt_msl,na.rm=TRUE)
mean(data$spd_gps,na.rm=TRUE)
median(data$spd_gps,na.rm=TRUE)
quantile(data$spd_gps,.5)
quantile(data$spd_gps,.9)

# Explore histograms/frequency distributions for specific variables
hist(data$spd_gps)
hist(data$spd_gps,ylim=c(0,10000),breaks=100)

# Same thing, but using the more versatile ggplot2 package
ggplot(data=data,aes(x=spd_gps))+
  geom_histogram(binwidth=.5)+
  scale_y_log10()+ #use a log scale to account for extreme nr of obs at low speed
  theme_classic()



## STEP 1.3B: REDUCE TO MAX 1 POINT PER MINUTE
####################################################
## NOTE: during the workshop at TdV we discovered some peculiarities in the data, in particular 
# bursts of high-resolution data. This is because the researchers programmed the tags to collect
# a burst of 10-15 points at 1-second intervals at every GPS-interval. 
# These high-res bursts complicated the process of behavioral classification and track segmentation, 
# Therefore, in included the step below to "resample" the data to maximum one GPS-position every 5 minutes. 
# Note you can adapt the code below to resample your data to any desired resolution. 
full <- data # store back-up of full dataset

# "Round" datetime to intervals of 5 minutes
full$dt <- lubridate::floor_date(full$dt,"5 minutes")

# To reduce to 1 value per 5 minutes, we will take the median
# latitude, longitude, spd_gps, alt_msl, heading, and temperature values in
# every 5-minute window. 
data <- full %>%
  group_by(ID,age,cring,ring,session_ID,session_start,session_end,dev,date,doy,yr,mth,dt) %>%
  dplyr::summarize(lat=median(lat),
                   long=median(long),
                   spd_gps=median(spd_gps),
                   heading = median(heading,na.rm=TRUE),
                   alt_msl = median(alt_msl,na.rm=TRUE),
                   temp = median(temp,na.rm=TRUE)) %>%
  ungroup()

# Compare "full" and "data": the operation above removed >50K of the >437K positions!
length(full$dt)-length(data$dt)

## STEP 1.4: EXPLORE/SUMMARIZE DATA FOR EACH INDIVIDUAL/TRACK SESSION
#####################################################################
# Calculate some summary statistics to explore data availability 
# for each device/bird
summary.per.bird <- data %>%
  group_by(ID) %>%
  summarise(age = unique(age),
            n.fixes = length(dt),
            first.dt = min(dt),
            last.dt = max(dt),
            track.duration = as.numeric(difftime(last.dt,first.dt,units="days")),
            n.days = length(unique(date)),
            missing.days = round(track.duration) - n.days)

#NOTE: one individual has ca 44 days without any fixes

# Make a barplot to compare duration of tracking sessions between individuals
barplot(summary.per.bird$missing.days~summary.per.bird$ID)

# Let's do something similar, but then for each year of each bird
summary.per.birdyear <- data %>%
  group_by(ID,yr) %>%
  summarise(n.fixes = length(dt),
            first.dt = min(dt),
            last.dt = max(dt),
            track.duration = difftime(last.dt,first.dt,units="days"),
            n.days = length(unique(date)),
            missing.days = round(track.duration) - n.days) %>%
  ungroup()

# NOTE: individual with missing days has missing days in 3 different years

#############################################################################################
## EXCERCISE 1: TRY TO EXPLORE DATA BY CALCULATING SOME SUMMARY STATISTICS FOR YOURSELVES. ##
## example questions:                                                                      ##
## i.   Calculate the highest highest and lowest altitudes and gps_speeds recorded         ##
##.     for each individual.                                                               ##
## ii.  Calculate number of GPS fixes for each bird on each date                           ##
## iii. Check the frequency distribution for altitudes above mean sea level                ##
#############################################################################################



## CODE REQUEST: EXPLORE DATA AVAILABILITY OVER COURSE OF STUDY PERIOD
#########################################################################

### Example 1: produce an overview of the nr. of GPS fixes per hour-block for 
### every individual. 

# Ex.1 Step 1: create "hour" column that will assist in calculating data
# availabilty per hour.
data$dt_hr <- lubridate::floor_date(data$dt,"1 hour")

# Summarise number of fixes per hour for each individual.
summary.per.birdhour <- data %>%
  group_by(ID,dt_hr) %>%
  summarise(n.fixes = length(dt)) %>%
  ungroup()

# Visualize with geom_tile(), which will draw a 'block' for every unit
# along the time axis (i.e. hourly blocks) and which we'll fill according
# to number of fixes. 
ggplot()+
  geom_tile(data=summary.per.birdhour,aes(x=dt_hr,y=ID,fill=n.fixes))+
  scale_fill_viridis_c(option="viridis",na.value="white")+
  xlab("datetime")

# NOTE how number of fixes per hour varies from <3 to 12, and individual
# FRP-CK13538 has much greater variability in data availability than any
# other individual. 


### Example 2: produce an overview of the nr. of GPS fixes per date for 
### every individual. 
summary.per.birdday <- data %>%
  group_by(ID,date) %>%
  summarise(n.fixes = length(dt)) %>%
  ungroup()

ggplot()+
  geom_tile(data=summary.per.birdday,aes(x=date,y=ID,fill=n.fixes))+
  scale_x_date()+
  scale_fill_viridis_c(option="viridis",na.value="white")+
  xlab("date")

### Example 3: for every date in the study period, calculate the nr. of
### individuals with at least one GPS-fix per day. 
summary.per.date <- data %>%
  group_by(date) %>%
  summarise(n.ids = length(unique(ID))) %>%
  ungroup()

# Visualize using geom_line() 
ggplot()+
  geom_line(data=summary.per.date,aes(x=date,y=n.ids,col=n.ids))+
  scale_x_date()+
  scale_fill_viridis_c(option="viridis",na.value="white")

# Say you are interested in extracting those days for which you have >4 individuals. 
# You can easily make a list of all relevant dates like this:
days.wt.many.birds <- unique(summary.per.date[which(summary.per.date$n.ids == 4),]$date)

# Then use that list for subsetting/extracting desired data
ss <- subset(data,data$date %in% days.wt.many.birds)

### Example 4: same as above, but we are interested in a specific study area.
### For example, overview of number of birds per day, within the region of the
### Camargue.

# specify lat/long limits for Camargue area
aoi.minlat <- 43.312674
aoi.maxlat <- 43.688002
aoi.minlong <- 4.037701
aoi.maxlong <- 4.999643

# check positioning of aoi using simple map

#### MAP Camargue
ggplot()+
  borders('world',col='grey5',fill='black',alpha=.8,linewidth=.02)+	
  # geom_polygon(data=nl2,aes(x=x,y=y,group=paste(sfg_id,polygon_id)),col='transparent',fill='white',size=.1)+	 # Map Spain with a light grey background (the tone of which will change with 'alpha' = elevation)
  theme_classic()+
  coord_quickmap(ylim=c(aoi.minlat-0.2,aoi.maxlat+0.2),xlim=c(aoi.minlong-0.2,aoi.maxlong+0.2))+
  geom_rect(aes(xmin=aoi.minlong,xmax=aoi.maxlong,ymin=aoi.minlat,ymax=aoi.maxlat),col='red',fill='transparent',linetype='dashed',linewidth=.4)

# Include lat/long filter in summary calculation.
summary.per.date.camargue <- data %>%
  filter(lat > aoi.minlat & lat < aoi.maxlat & long > aoi.minlong & long < aoi.maxlong) %>% # we filter based on min/max lat/long values
  group_by(date) %>%
  summarise(n.ids = length(unique(ID))) %>%
  ungroup()

# Visualize using geom_line() 
ggplot()+
  geom_line(data=summary.per.date.camargue,aes(x=date,y=n.ids,col=n.ids))+
  scale_x_date()+
  scale_fill_viridis_c(option="viridis",na.value="white")

