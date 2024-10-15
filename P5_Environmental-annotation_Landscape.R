##########################################################################
### WORKSHOP - ANALYSIS OF TRACKING DATA - TOUR DU VALAT              ####
### 26 SEPTEMBER 2024                                                 ####
###                                                                   ####
### by Wouter M.G. Vansteelant (wouter@birdeyes.org)                  ####
### last update: 25 Sept 2024                                         ####
##########################################################################

### PART 5: Annotation terrain elevation, country and land/sea mask ###
########################################################################

##  STEP 5.1: Annotate track with vectorized data
################################################
## In this exmple we'll work with country borders

# Load world map from natural earth data using rnaturalearth package
world.map <- ne_countries(scale = "medium", returnclass = "sf")

# Create spatial object of all bird-locations
np <- data[,c("ID","dt","long","lat")]
np <- st_as_sf(np,coords=c("long","lat"),crs=crs(world.map))
st_crs(np) <- st_crs(world.map)

# use st_join() to extract country names at each location 
# points over sea will be NA
npc <- st_join(np,world.map[,c("admin")])

data$country <- npc$admin

# remove temporary objects
rm(np,npc)

### Check annotation by mapping
# Redefine lat/long range for mapping
latlimits <- c(min(data$lat)-0.5,max(data$lat)+0.5)
longlimits <- c(min(data$long)-0.5,max(data$long)+0.5)

ggplot()+
  borders('world',fill='grey60',size=.2)+
  geom_point(data=data,aes(x=long,y=lat,col=country),size=.8,shape=21,stroke=.2,fill='transparent')+
  scale_colour_viridis_d(option = "turbo",na.value="white")+
  coord_quickmap(xlim=longlimits,ylim=latlimits)+
  theme_classic()+
  theme(panel.background = element_rect(fill='grey80'))

### Annotation was conducted correctly. Similarly, one could annotate tracks with other polygon values
## such as protected areas, n2000-sites, or whichever other zonation you may like to use. 

### Example analyses: use barplot to visualize proportioin of locations in each country per bird
ggplot() +
  geom_bar(data=data,aes(x=ID,fill=country),position="fill",col="black")+
  scale_fill_viridis_d(option = "turbo",na.value="white")+
  theme_classic()

### Example analyses: use barplot to visualize proportion of locations in each country per month, 
## seperately for each bird
ggplot() +
  geom_bar(data=data,aes(x=floor_date(date,"month"),fill=country),position="fill",col="black")+
  scale_fill_viridis_d(option = "turbo",na.value="white")+
  theme_classic()+xlab("Date [month]")+
  facet_wrap(~ID,ncol=1,scales="free")
  

##  STEP 5.2: Annotate track with raster data
################################################
## In this exmple we'll work with CORINE land cover
# and for simplicity we'll focus on Camargue

# specify lat/long limits for Camargue area
aoi.minlat <- 43.312674
aoi.maxlat <- 43.688002
aoi.minlong <- 4.037701
aoi.maxlong <- 4.999643

# load CORINE landcover 
clc <- terra::rast("~/Documents/1. Environmental data/CLC_raster100m/DATA/U2018_CLC2018_V2020_20u1.tif")

# check projection of this data
crs(clc)

# Note this data is not in standard WGS84 projection. Therefore: transformation needed! 
# In this case, we'll reproject tracking data to match that of CLC

# First, make data into spatial object
data.camargue <- subset(data,data$lat > aoi.minlat & data$lat < aoi.maxlat & data$long > aoi.minlong & data$long < aoi.maxlong)
data.camargue <- st_as_sf(data.camargue,coords=c("long","lat"),crs=crs(world.map))
data.camargue <- st_transform(data.camargue,crs=crs(clc)) 

# Extract CORINE land cover values for each point in Camargue
clcs.camargue <- terra::extract(clc, data.camargue) 

data.camargue$clc <- clcs.camargue$LABEL3

### Example analyses: use barplot to visualize proportion of locations in differernt land use classes
ggplot() +
  geom_bar(data=data.camargue,aes(x=ID,fill=clc),position="fill",col="black")+
  scale_fill_viridis_d(option = "turbo",na.value="white",drop=FALSE)+
  theme_classic()+xlab("Date [month]")+
  theme(legend.position="bottom",
        legend.direction="horizontal")+
  guides(fill=guide_legend("Land Use",ncol=4))

# Get values for map extent from reprojected tracking data
extent <- st_bbox(data.camargue)

clc.camargue <- terra::crop(clc,extent)

#### MAP Camargue land use + flamingo locations in black
ggplot()+
  geom_raster(data=clc.camargue,aes(x=x,y=y,fill=LABEL3))+
  scale_fill_viridis_d(option = "turbo",na.value="white")+
  geom_sf(data=data.camargue,col="black",shape=21)+
  theme_classic() +
  theme(legend.position="bottom",
                         legend.direction="horizontal")+
  guides(fill=guide_legend("Land Use",ncol=4))


### NOTE: in this case there is a mismatch in legend due to lower number of land use categories at flamingo locations
## compared to full range of categories available. --> In practice, one is likely to select a number of land use categories
## of interest, and then always force ggplot to use all the categories of interest in every legend. It is also possible to 
## simplify the categories, by grouping them into coarser habitat types. 
