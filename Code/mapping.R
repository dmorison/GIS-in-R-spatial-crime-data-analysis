setwd("C:/Users/davem/Projects/201703_crime-data/")
library(ggplot2)
library(rgeos)
library(rgdal)
libs <- c("maps", "mapdata", "mapproj", "maptools", "sp", "ggmap")
lapply(libs, library, character.only = TRUE)

### Variables to keep
retain <- c("Month", "Longitude", "Latitude", "LSOA.code", "LSOA.name", "Crime.type")
### Import the City of London dataset
city.init <- read.csv("Data/city-of-london-police-coord-data/2016-12/2016-12-city-of-london-street.csv")
city <- city.init[which(complete.cases(city.init$Longitude)), ]
city <- city[retain]
### Import the Met dataset
met.init <- read.csv("Data/met-police-coord-data/2016-12/2016-12-metropolitan-street.csv")
met <- met.init[which(complete.cases(met.init$Longitude)), ]
met <- met[retain]

all <- rbind(city, met)
all <- all[which(all$Latitude > 51.275), ]
all <- all[which(all$Latitude < 51.7), ]
all <- all[which(all$Longitude > -0.53), ]
all <- all[which(all$Longitude < 0.35), ]

df <- all

bike <- all[all$Crime.type == "Bicycle theft", ]

###
# map("world2Hires", "UK")
# points(mapproject(x = city.init$Longitude, y = city.init$Latitude))

###
dir_1 <- "References/Creating-maps-in-R-master/Creating-maps-in-R-master/data/"
dir_2 <- "Data/statistical-gis-boundaries-london/ESRI/"

ldn1 <- readOGR(file.path(dir_1), layer = "london_sport")

proj4string(ldn1) <- CRS("+init=epsg:27700")
ldn1.wgs84 <- spTransform(ldn1, CRS("+init=epsg:4326"))
ggplot(ldn1.wgs84) + geom_polygon(aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  geom_point(data = bike, aes(x = Longitude, y = Latitude), colour = "red") +
  theme(axis.title = element_blank(), text = element_text(size = 14, face = "bold")) +
  labs(title = "Bicycle theft in Greater London - December 2016")
# plot the area codes
ggplot(ldn1.wgs84) + geom_polygon(aes(x = long, y = lat, group = group)) +
  geom_point(data = all, aes(x = Longitude, y = Latitude, colour = LSOA.name)) +
  theme(axis.title = element_blank(), text = element_text(size = 14, face = "bold"), legend.position = "none") +
  scale_colour_manual(values = rainbow(4991)) +
  labs(title = "Boroughs of London distinguished by crime incidences",
       subtitle = "Shading of colours within each borough represent the localised areas")

### wrong projections
map1 <- ggplot(ldn1)
map1 <- map1 + geom_polygon(aes(x = long, y = lat, group = group))
map1 + geom_point(data = df, aes(x = Longitude, y = Latitude), colour = "red")

### transforming coordinates ###
class(df)
coordinates(df) <- ~Longitude+Latitude
class(df)
proj4string(df) <- CRS("+init=epsg:4326")

df <- spTransform(df, CRS(proj4string(ldn1)))
identical(proj4string(ldn1), proj4string(df))
df.t <- data.frame(df)
###
plot(ldn1)
points(df.t$Longitude, df.t$Latitude)
###
map2 <- ggplot()
map2 + geom_polygon(data = ldn1, aes(x = long, y = lat, group = group)) +
  geom_point(data = df.t, aes(x = Longitude, y = Latitude), colour = "red")
##########################################################

# proj4string(ldn1) <- CRS("+init=epsg:27700")
# ldn1.wgs84 <- spTransform(ldn1, CRS("+init=epsg:4326"))
ldn1.f <- fortify(ldn1, region = "ons_label")
ldn1.f <- merge(ldn1.f, ldn1@data, by.x = "id", by.y = "ons_label")

map_ldn <- ggplot(ldn1.f, aes(x = long, y = lat, group = group, fill = Partic_Per))
map_ldn + geom_polygon() +
  coord_equal()
########################################################

ldn2 <- readOGR(file.path(dir_2), layer = "OA_2011_London_gen_MHW")
plot(ldn2)
