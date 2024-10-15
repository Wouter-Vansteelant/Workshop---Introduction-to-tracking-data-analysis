##########################################################################
### WORKSHOP - ANALYSIS OF TRACKING DATA - TOUR DU VALAT              ####
### 26 SEPTEMBER 2024                                                 ####
###                                                                   ####
### by Wouter M.G. Vansteelant (wouter@birdeyes.org)                  ####
### last update: 25 Sept 2024                                         ####
##########################################################################

### PART 2: Explore dataset for outliers and patterns     ###
#############################################################
# Some standard operations for removing incorrect fixes.
# This dataset happens to be very clean. In other cases, 
# quite a bit of filtering is needed to remove bad locations.
data <- subset(data,data$lat != 0 & data$long != 0)
data <- subset(data,is.na(data$lat) == FALSE & is.na(data$long)== FALSE)

##  STEP 2.1: Explore tracks using simple maps
################################################
# Redefine lat/long range for mapping
latlimits <- c(min(data$lat)-0.5,max(data$lat)+0.5)
longlimits <- c(min(data$long)-0.5,max(data$long)+0.5)

ggplot()+
  borders('world',fill='grey60',size=.2)+
  geom_point(data=data,aes(x=long,y=lat,col=ID),size=.5,shape=21,stroke=.2,fill='transparent')+
  scale_colour_viridis_d(option = "turbo")+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'))

# Now THATS COOL DATA :-D. No obvious outliers apparent from this map. 
# Individuals clearly vary a lot in movements/destinations. We got some very neat sea-crossing tracks.
# Lets make those sea-crossings the subject of our workshop! 

# Now, let's explore how the birds' latitude changes over time:
ggplot()+
  geom_point(data=data,aes(x=dt,y=lat,col=ID,group=ID),size=.4)+
  scale_colour_viridis_d(option = "turbo")+
  scale_x_datetime()+
  theme_classic()

# This reveals how birds frequently make big latitudinal movements over the course of their
# early lives. However, movements are dispersed in east/west directions too, and it would
# be nicer to have another metric than latitude/longitude to look at displacements over time.


##  STEP 2.2: Explore tracks using satellite imagery
####################################################
# USING MAPTILES and RNATURALEARTH 

# NOTE: can take a while to download satellite imagery depending on 
# size of area and resolution (i.e. zoom) called for. 
# Example below is only for Camargue, to limit download time,
# but you can adjust this code to get imagery across larger areas.

# download world map from natural earth data using rnaturalearth package
world.map <- ne_countries(scale = "medium", returnclass = "sf")

# prepare the box for the map by giving 4 numbers: 
# i.e. long and lat for bottom left and top right of the box
box <- c(3,43,6,44) # camargue

# tell R that 'box' is a spatial 'bbox' object 
class(box) <- "bbox"
box <- st_as_sfc(box)

# set coordinate system in WGS84 by using coordinate system of natural earth
# world map
st_crs(box) <- crs(world.map)

# download satellite imagery for Camargue using the specified box
tile.Camargue <- get_tiles(box,provider = "Esri.WorldImagery", crop = TRUE, zoom = 13)

# NOTE: be patient, this download will takes a few minutes

# plot data points on satellite imagery of the Camargue
ggplot()+
  geom_spatraster_rgb(data = tile.Camargue,alpha=.5)+
  #borders('world',fill='grey60',size=.1)+
  geom_point(data=data,aes(x=long,y=lat,fill=ID),size=.5,shape=21,stroke=.08,col='black')+
  scale_fill_viridis_d(option = "turbo")+
  scale_y_continuous(limits=c(43.2,43.8))+
  scale_x_continuous(limits=c(3.8,5.4))+
  #coord_quickmap(xlim=c(3,6),ylim=c(43,44))+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'),
        legend.position="bottom",
        legend.direction="horizontal")

# NOTE: to do the same for the whole Mediterranean you will need to define a larger
# 'box' and use a lower amount of 'zoom' to get imagery at a sensible resolution. 

## STEP 2.3: CALCULATE TRAJECTORY MOVEMENT STATISTICS 
#####################################################
# A very useful variable in this study system might be the distance of each point to 
# the tagging location (=colony).

# For that, we first need to define a reference point, in this case the Flamingo colony
# in Camargue.
colony.lat <- 43.51646
colony.long <- 4.199843

# We can now calculate distance of each point to Camargue reference point using the
# deg.dist() function from the 'fossil' package (https://www.rdocumentation.org/packages/fossil/versions/0.4.0/topics/deg.dist).
# Note that the output of this function is a distance in 'kilometers'. 
data$dist_to_colony <- deg.dist(lat1=colony.lat,long1=colony.long,long2=data$long,lat2=data$lat)

## Check whether this calculation is sensical by mapping locations coloured according to distance to colony
ggplot()+
  borders('world',fill='grey60',size=.1)+
  geom_point(data=data,aes(x=long,y=lat,col=dist_to_colony),size=.2,shape=21)+
  scale_colour_viridis_c("Distance to colony [km]",option = "magma")+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'),
        legend.position="bottom",
        legend.direction="horizontal")

# now use distance to colony for timing plot
ggplot()+
  geom_point(data=data,aes(x=dt,y=dist_to_colony,col=ID,group=ID))+
  scale_colour_viridis_d(option = "magma")+
  scale_x_datetime()+
  theme_classic()

## Note that we can easily split ggplot figures into seperate panels for each individual
## by adding a facet_wrap() function in our ggplot code.
ggplot()+
  borders('world',fill='grey60',size=.1)+
  geom_point(data=data,aes(x=long,y=lat,col=dist_to_colony),size=.2,shape=21)+
  scale_colour_viridis_c("Distance to colony [km]",option = "magma")+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'),
        legend.position="bottom",
        legend.direction="horizontal")+
  facet_wrap(~ID,ncol=2)

#############################################################################################
## EXCERCISE 2: TRY TO EXPLORE DATA BY MAKING SOME OTHER MAPS/GRAPHS.                      ##
## example questions:                                                                      ##
## i. Can you make a map showing GPS fixes coloured according                              ##
##     to flight altitude or GPS speed?                                                    ##
## ii. Make a map showing points coloured per individual, and zooming in on the            ##
##     Camargue.                                                                           ##
## iii. I would like to see a graph showing the speed of one individual over time          ##
##      during one month of tracking.                                                      ##
#############################################################################################

# Next, let's calculate trajectory statistics. I.e. --> the distance, time, speed, direction between points
# To do this, let's load some supporting functions from our folder 'supporting scripts'
source('./sidescript_pt2pt_fxns.R')

## Specify in which columns of data-object "x", the functions can find the long, lat and dt values
calcdist <- function(x) pt2pt.distance(longitude=x$long,latitude=x$lat) 
calcdur <- function(x) pt2pt.duration(datetime=x$dt)
calcdir <- function(x) pt2pt.direction(longitude=x$long,latitude=x$lat)

# We must order the dataframe chronologically per bird in order to ensure 
# the correct application of our coming functions.
data <- data[order(data$ID,data$dt),]

# We use 'lapply()' (list-apply) to apply the distance and duration calculations separately for each
# individual track, and store output in temporary lists.
v1 <- lapply(split(data,data$ID),"calcdist")
v2 <- lapply(split(data,data$ID),"calcdur")
v3 <- lapply(split(data,data$ID),"calcdir")

# Append output to our dataframe by using the 'unlist()' function. 
data$dist <- as.numeric(unlist(v1))
data$dur <- as.numeric(unlist(v2))
data$dir <- as.numeric(unlist(v3))

# Now that we have the distance and duration between each fix, we can calculate speeds. Note that
# point-to-point distances and durations are in meters and seconds, and so speeds in m/s
data$spd <- data$dist/data$dur
data$spd.kmh <- data$spd * 3.6

# Check histogram and range of trajectory speeds
ggplot(data=data,aes(x=spd))+
  geom_histogram(binwidth=.5)+
  theme_classic()#+
  #ylim(0,10) # HINT: play with limit of y and x-axes to zoom in on extreme values

# We can see a few speeds above 30m/s, and two points with speeds of several 100 m/s.
# Speeds above 30m/s (108km/h) are unlikely, and above 40m/s probably
# impossible. 

# Lets see what points these are and see if we need to filter them out
ggplot()+
  borders('world',fill='grey60',size=.2)+
  # plot points < 30ms
  geom_point(data=data[which(data$spd <30),],aes(x=long,y=lat),col='black',size=.5,shape=21,stroke=.2,fill='transparent')+
  # plot points > 30ms
  geom_point(data=data[which(data$spd >=30),],aes(x=long,y=lat),col='red',size=.5,shape=21,stroke=.2,fill='transparent')+
  scale_colour_viridis_d(option = "magma")+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'))+
  facet_wrap(~ID,ncol=2)

### Iteration 2: Lets remove spds > 30m/s, recalcualte movement stats and check again
data <- subset(data,data$spd < 30)
data <- data[order(data$ID,data$dt),]

# Re-apply movestat functions
v1 <- lapply(split(data,data$ID),"calcdist")
v2 <- lapply(split(data,data$ID),"calcdur")
v3 <- lapply(split(data,data$dev),"calcdir")

# Append results
data$dist <- as.numeric(unlist(v1))
data$dur <- as.numeric(unlist(v2))
data$dir <- as.numeric(unlist(v3))

# Now that we have the distance and duration between each fix, we can calculate speeds. Note that
# point-to-point distances and durations are in meters and seconds, and so speeds in m/s
data$spd <- data$dist/data$dur

# Check histogram and range of trajectory speeds
ggplot(data=data,aes(x=spd))+
  geom_histogram(binwidth=.5)+
  scale_y_continuous(limits=c(0,10))+ 
  theme_classic()

## SUCCES! all unrealistic speeds are now gone

# Explore variation in sampling intervals (i.e. temporal resolution) of our data.
# To aid interpretation we can add some reference lines: in this case dashed red lines at 5, 10 and 30minute intervals
ggplot()+
  geom_histogram(data=data,aes(x=dur/60),bins=100)+ # divide dur by 60 to get values in minutes
  scale_y_log10()+
  theme_classic()+
  xlab("Interval [mins]")+
  #xlim(0,120*60)+
  geom_vline(xintercept=5,linetype='dashed',col='red',size=.2)+ # line at 5 minute intervals
  geom_vline(xintercept=10,linetype='dashed',col='red',size=.2)+ # line at 10 minute intervals
  geom_vline(xintercept=30,linetype='dashed',col='red',size=.2)+ # line at 30 minute intervals
  facet_wrap(~ID,ncol=2,scales="free")

# We can see that vast majority of data are at 5 to 10-minute intervals for all individuals, 
# but there are gaps of up to hundreds/thousands of minutes (hours to days)

rm(v1,v2,v3)

##  STEP 2.3: Standardise time intervals (OPTIONAL)
####################################################
# Resample data to a fixed interval: we wont do this during this workshop because this procedure
# take a long time to run, but I am giving you the code so you are able to play with it yourself

### OPTION 1: use resampling function provided as supporting script, originally developed by Duarte Viana
# Load resampling function from 'supporting scripts' folder
source('./supporting scripts/sidescript_resampling_function.R')

# Apply resampling function to obtain tracks at resolution of 30 minutes with deviation of 5 minutes
data <- data[order(data$dev,data$dt),]
data_resampled <- moveTimeSample(data,data$dt,data$ID,30,5,subset=TRUE)

# Recalculate movement statistics for resampled data
data_resampled <- data_resampled[order(data_resampled$dev,data_resampled$dt),]
v1 <- lapply(split(data_resampled,data_resampled$dev),"calcdist")
v2 <- lapply(split(data_resampled,data_resampled$dev),"calcdur")
v3 <- lapply(split(data_resampled,data_resampled$dev),"calcdir")

data_resampled$dist <- as.numeric(unlist(v1))
data_resampled$dur <- as.numeric(unlist(v2))
data_resampled$dir <- as.numeric(unlist(v3))
data_resampled$spd <- data_resampled$dist/data_resampled$dur

rm(v1,v2,v3)

### OPTION 2: adapt code in lines 212-239 from P1_Read-Process_TrackingData.R to resample to desired interval

### OTHER OPTIONS: a variety of packages have been developed specifically for movement analysis. 
### These allow to not only resample, but also interpolate tracks to desired resolution. 

##  STEP 2.4: Calculate aggregate movement statistics
####################################################
# Example: daily movement statistics between first and last location on each day
# based on resampled data
daily.stats <- data %>%
  group_by(ID,date) %>%
  arrange(dt,by.group = TRUE) %>%
  summarise(n.obs = length(dt),
            s.lat = head(lat,1),
            s.long = head(long,1),
            e.lat = tail(lat,1),
            e.long = tail(long,1),
            s.dt = head(dt,1),
            e.dt = tail(dt,1),
            d.dist.beeline = deg.dist(long1=s.long,long2=e.long,lat1=s.lat,lat2=e.lat),
            d.dist.cumulative = (sum(dist)-tail(dist,1))/1000,
            d.straightness = d.dist.beeline/d.dist.cumulative,
            d.dur = as.numeric(difftime(e.dt,s.dt,units='hours')),
            d.coverage = d.dur/24,
            d.spd = d.dist.cumulative/d.dur) %>%
  ungroup()

# Daily statistics can be useful to classify seasonal movements/annual phases. 
# To explore potential usefulness of these variables for classifying behaviours of interest,
# we can explore frequency distributions of daily statistics, but also append daily stats
# back to full dataframe in order to explore spatiotemporal patterns in these metrics.

# Example: frequency distributions of daily (cumulative) distances and straightness
# Hint: look for natural breaks in frequency distributions that may hint towards suitable
# criteria/treshold values for behavioural classificaitons.
ggplot()+
  geom_histogram(data=daily.stats,aes(x=d.dist.cumulative),bins=200)+
  theme_classic()+
  scale_y_continuous(limits=c(0,100))

ggplot()+
  geom_histogram(data=daily.stats,aes(x=d.dist.beeline),bins=200)+
  theme_classic()+
  scale_y_continuous(limits=c(0,100))

ggplot()+
  geom_histogram(data=daily.stats,aes(x=d.straightness),bins=100)+
  theme_classic()#+
  #ylim(0,500)

# Example: frequency distribution of daily coverage
ggplot()+
  geom_histogram(data=daily.stats,aes(x=d.coverage),bins=100)+
  theme_classic()#+
#ylim(0,500)

# Merge daily straightness and some other stats of interest to (resampled) data to
# explore how these variables vary across space/time
data <- merge(data,daily.stats[,c("ID","date","d.straightness","d.dist.beeline","d.dist.cumulative")],all.x=TRUE)
data <- data[order(data$ID,data$dt),]

# timing graph showing distance to colony vs datetime, with points coloured according to daily straightness
ggplot()+
  geom_point(data=data,aes(x=dt,y=dist_to_colony,col=d.straightness),size=.3)+
  scale_colour_viridis_c(option = "magma")+
  scale_x_datetime()+
  theme_classic()+
  facet_wrap(~ID,ncol=2,scales="free")
  
# Map with tracks coloured to daily straightness
ggplot()+
  borders('world',fill='grey60',size=.1)+
  geom_point(data=data,aes(x=long,y=lat,col=d.straightness),size=.2,shape=21)+
  scale_colour_viridis_c(option = "magma")+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'),
        legend.position="bottom",
        legend.direction="horizontal")+
  facet_wrap(~ID,ncol=2)

#############################################################################################
## EXCERCISE 2: EXPLORE USEFULNESS OF DAILY STATISTICS TO CLASSIFY SEASONAL MOVEMENTS      ##
## example questions:                                                                      ##
## i. Produce temporal graphs and or maps of daily (cumulative) distances and other        ##
##    metrics to explore potential criteria for classifying seasonal movements.            ##                                     ##
## ii. Explore how daily statistics are inter-related, for example, how does daily coverage##
##     vary with number of daily observations, daily distances, ...?                       ##
#############################################################################################
