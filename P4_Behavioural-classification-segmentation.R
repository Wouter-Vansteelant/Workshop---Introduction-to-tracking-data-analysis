##########################################################################
### WORKSHOP - ANALYSIS OF TRACKING DATA - TOUR DU VALAT              ####
### 26 SEPTEMBER 2024                                                 ####
###                                                                   ####
### by Wouter M.G. Vansteelant (wouter@birdeyes.org)                  ####
### last update: 25 Sept 2024                                         ####
##########################################################################

### PART 4: Behavioural classification                    ###
#############################################################

# CLASSIFICATION OF MIGRATORY FLIGHTS 
# Let's start 'easy' and focus on long-range displacements. 
# Because Flamingos are nocturnal migrants it will be quite tricky 
# to do this based on daily stats (a single flight might span multiple
# dates in one night). Therefore, lets try to segment the track in
# "flights" and "localized" segments based on point-to-point speeds. 

# Considering most data has temporal resolution of 5-10 minutes, it seems safe to assume
# a relatively low speed threshold to identify flight behaviour. 
# Lets start classify points with speeds > 1.5m/s or ca 5.4 km/h as clear flight segments

ggplot(data=data,aes(x=spd))+
  geom_histogram(binwidth=.5)+
  scale_y_log10()+ #use a log scale to account for extreme nr of obs at low speed
  theme_classic()+
  scale_y_continuous(limits=c(0,1000))+
 # scale_x_continuous(limits=c(0,30))+
  geom_vline(xintercept=5,col='red',linetype="dashed")


data$travel <- ifelse(data$spd > 1.5,'flight','local')

# lets see what that looks like on a map
ggplot()+
  borders('world',fill='grey60',size=.1)+
  geom_point(data=data,aes(x=long,y=lat,col=travel),size=.3,shape=21)+
  scale_colour_manual(values=c("flight"="orangered","local"="navyblue"))+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'))+
  facet_wrap(~ID,ncol=2)

# and on a timing plot
ggplot()+
  geom_path(data=data,aes(x=dt,y=dist_to_colony,col=travel,group=ID))+
  scale_colour_manual(values=c("flight"="orangered","local"="navyblue"))+
  scale_x_datetime()+
  theme_classic()+
  facet_wrap(~ID,ncol=2,scales="free")+
  geom_hline(yintercept=50,col='black',linetype='dashed',size=.2) # line for 50km away from colony

# it looks like we fully capture the long-haul flights at this cut-off. A lot of local/commuting flights are of course
# classified as 'flight' as well. To extract specifically migratory flights, one approach could be to segment the track
# in periods of continuous flight vs non-flight behaviour, calculate the duration of flight segments, and focus on flights
# lasting at least 2 hours (assuming Flamingos rarely fly continuously for 2 hours in staging areas)

# To segment track in bouts of continuous flight vs non-flight, we can create a column "segment"
# by cleverly combining the cumsum(), as.logical() and diff() functions
# The diff() function detects whether there is a change in the 'ID' and/or 'travel' value compared to the preceding row
# the as.logical() function translates this to a 1 (yes) or 0 (no)
# and the cumsum() function then adds 0 or 1 to the segment number. 
# In other words: we create a column with segment numbers, starting from 0, and a new segment number is created
# each time the value in the column 'ID' or 'travel' changes compared to preceding row. 
data <- data[order(data$ID,data$dt),]
data$segment <- as.numeric(c(0,cumsum(as.logical(diff(as.numeric(as.factor(paste(data$ID,data$travel))))))))

# We now add leading zeros to each segment number so each number consists of 5 numbers 
data$segment <- sprintf("%05d",data$segment)

# Finally, we paste segment number to ID value to obtain a categorical identifier for each unique segment
data$segment <- paste(data$ID,data$segment,sep="_")

# Now we have segmented the track into bouts of continuous filght vs non-flight, we can 
# filter out the flight segments, and calculate summary statistics like the 
# duration and distance covered during each flight segment
flight.segments <- data %>%
  filter(travel == "flight")%>%
  group_by(ID,segment) %>%
  summarise(s.lat = head(lat,1),
            s.long = head(long,1),
            e.lat = tail(lat,1),
            e.long = tail(long,1),
            takeoff = head(dt,1),
            landing = tail(dt,1),
            flight.dist.beeline = deg.dist(long1=s.long,long2=e.long,lat1=s.lat,lat2=e.lat),
            flight.dur = as.numeric(difftime(landing,takeoff,units='hours')),
            flight.dist.cumulative = (sum(dist)-tail(dist,1))/1000,
            flight.straightness = flight.dist.beeline/flight.dist.cumulative) %>%
  ungroup()

# Subset flight data, append flight summary stats to flight data, and explore suitable cut-off
# in flight duration to identify migratory flights
flight.data <- subset(data,data$travel=="flight")
flight.data <- merge(flight.data,flight.segments[,c("ID","segment","takeoff","landing","flight.dur","flight.straightness")])

# lets see what flight data looks like on map
ggplot()+
  borders('world',fill='grey60',size=.1)+
  geom_point(data=flight.data,aes(x=long,y=lat,col=flight.dur),size=.3,shape=21)+
  scale_colour_viridis_c(option="magma")+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'))+
  facet_wrap(~ID,ncol=2)

## CLASSIFY LONG VS SHORT FLIGHT
flight.data$long.flights <- ifelse(flight.data$flight.dur >2,"long","short")

# explore distribution of long vs short flights on map
ggplot()+
  borders('world',fill='grey60',size=.1)+
  geom_point(data=flight.data,aes(x=long,y=lat,col=long.flights),size=.3,shape=21)+
  scale_colour_viridis_d(option="magma")+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'))+
  facet_wrap(~ID,ncol=2)

# and on a timing plot
ggplot()+
  geom_point(data=flight.data,aes(x=dt,y=dist_to_colony,col=long.flights))+
  scale_colour_viridis_d(option="magma")+
  scale_x_datetime()+
  theme_classic()+
  facet_wrap(~ID,ncol=2,scales="free")+
  geom_hline(yintercept=50,col='black',linetype='dashed',size=.2) # line for 50km away from colony

## succes again :p -- all long flights, including all sea-crossings, are adequately classified as such

# we can now dive into more detail, for example exploring how flight range/distance relates to flight duraiton
ggplot()+
  geom_point(data=flight.segments,aes(x=flight.dur,y=flight.dist.cumulative))+
  scale_colour_viridis_d(option="magma")+
  theme_classic()+
  facet_wrap(~ID,ncol=2,scales="free")+
  geom_hline(yintercept=50,col='black',linetype='dashed',size=.2) # line for 50km away from colony

