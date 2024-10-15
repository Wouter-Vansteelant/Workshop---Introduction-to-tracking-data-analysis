##########################################################################
### WORKSHOP - ANALYSIS OF TRACKING DATA - TOUR DU VALAT              ####
### 26 SEPTEMBER 2024                                                 ####
###                                                                   ####
### by Wouter M.G. Vansteelant (wouter@birdeyes.org)                  ####
### last update: 25 Sept 2024                                         ####
##########################################################################

### PART 3: day vs night classification                   ###
#############################################################

# create vector of coordinates (spatialpoints) and dates 
crds <- cbind(data$long,data$lat)
crds <- SpatialPoints(crds,proj4string=CRS("+proj=longlat +datum=WGS84"))

dates <- as.POSIXct(strptime(data$dt,format="%Y-%m-%d"),tz="UTC")

# calculate sunrise times
srise <- sunriset(crds, dates, direction=c("sunrise"),POSIXct.out=TRUE)

# calculate sunset times
sset <- sunriset(crds, dates, direction=c("sunset"),POSIXct.out=TRUE)

# append sunrise and sunset in exact times back to dataframe
data$srise_R <- srise[,2]
data$sset_R <- sset[,2]

# calculate solar noon
snoon <- solarnoon(crds,dates,POSIXct.out=TRUE)

# append solar noon and calculate dt relative to noon
data$snoon_R <- snoon[,2]
data$dt_to_noon <- as.numeric(difftime(data$dt,data$snoon_R,units='hours'))

# calculate srise and sset relative to solar noon
data$srise_to_noon <- as.numeric(difftime(data$srise_R,data$snoon_R,units='hours'))
data$sset_to_noon <- as.numeric(difftime(data$sset_R,data$snoon_R,units='hours'))

# classify day and night locations
data$daynight <- ifelse(data$dt >= data$srise_R & data$dt < data$sset_R,'day','night')

# Remove obsolete
rm(crds,dates,srise,sset,crep1,crep2,snoon)

# Calculate range of civil sunrise and sunset across all data
# useful for plotting diel graphs with background shading for 
# solar periods
suntimes.mean <- data %>% 
  summarize(srise_earliest = min(srise_to_noon),
            srise_latest = max(srise_to_noon),
            sset_earliest = min(sset_to_noon),
            sset_latest = max(sset_to_noon))%>% 
  ungroup()

# after classifying remove columns we won't need anymore
drops <- c("srise_to_noon","sset_to_noon","srise_R","sset_R","snoon_R")
data <- data[,!(colnames(data) %in% drops)]
rm(drops)

# Map flights according to daynight
ggplot()+
  borders('world',fill='grey60',size=.1)+
  geom_point(data=data,aes(x=long,y=lat,col=daynight),size=.2,shape=21)+
  scale_colour_manual(name="Solar time",values=c("day"='orangered',"night"="navyblue")) +
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'),
        legend.position="bottom",
        legend.direction="horizontal")+
  facet_wrap(~ID,ncol=2)

# COOL!!! Flamingos are very clearly nocturnal migrants. :-D 


# Plot speeds over course of day, using "solar time" on the x-axis, and
# enrichting plots with background shading to indicate which solar times
# correspond to night vs day.
ggplot()+
  geom_rect(data=suntimes.mean,aes(xmin=-Inf,xmax=srise_earliest,ymin=-Inf,ymax=Inf),fill='grey80',alpha=.5)+
  geom_rect(data=suntimes.mean,aes(xmin=srise_earliest,xmax=srise_latest,ymin=-Inf,ymax=Inf),fill='orange',alpha=.5)+
  geom_rect(data=suntimes.mean,aes(xmin=srise_latest,xmax=sset_earliest,ymin=-Inf,ymax=Inf),fill='lightgoldenrod2',alpha=.5)+
  geom_rect(data=suntimes.mean,aes(xmin=sset_earliest,xmax=sset_latest,ymin=-Inf,ymax=Inf),fill='orange',alpha=.5)+
  geom_rect(data=suntimes.mean,aes(xmin=sset_latest,xmax=Inf,ymin=-Inf,ymax=Inf),fill='grey80',alpha=.5)+
  geom_path(data=data,aes(x=dt_to_noon,y=spd,group=paste(ID,date)),linetype='solid',linewidth=.2)+
  theme_classic()+
  facet_wrap(~ID,ncol=2)


#############################################################################################
## EXCERCISE 3: EXPLORE USEFULNESS OF DAILY STATISTICS TO CLASSIFY SEASONAL MOVEMENTS      ##
## example questions:                                                                      ##
## i. Explore diel patterns in altitudes and other movement statistics.                    ##   
#############################################################################################