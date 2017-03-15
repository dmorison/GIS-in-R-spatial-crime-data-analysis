setwd("C:/Users/davem/Projects/201703_crime-data/")
library(ggplot2)
library(rgeos)
library(rgdal)
libs <- c("maps", "mapdata", "mapproj", "maptools", "sp", "ggmap")
lapply(libs, library, character.only = TRUE)

testset <- read.csv("Data/2016-01/2016-01-city-of-london-street.csv")
dataset <- testset[which(complete.cases(testset$Longitude)), ]

###
# map("world2Hires", "UK")
# points(mapproject(x = testset$Longitude, y = testset$Latitude))

###
dir_1 <- "References/Creating-maps-in-R-master/Creating-maps-in-R-master/data/"
dir_2 <- "Data/statistical-gis-boundaries-london/ESRI/"

ldn1 <- readOGR(file.path(dir_1), layer = "london_sport")
plot(ldn1)
points(dataset.df$Longitude, dataset.df$Latitude) # first have to transform the coords as below

### wrong projections
map1 <- ggplot(ldn1)
map1 <- map1 + geom_polygon(aes(x = long, y = lat, group = group))
map1 + geom_point(data = testset, aes(x = Longitude, y = Latitude), colour = "red")
####################

proj4string(ldn1) <- CRS("+init=epsg:27700")
ldn1.wgs84 <- spTransform(ldn1, CRS("+init=epsg:4326"))
ggplot(ldn1.wgs84) + geom_polygon(aes(x = long, y = lat, group = group)) +
  geom_point(data = dataset, aes(x = Longitude, y = Latitude), colour = "red")

### transforming coordinates ###
class(dataset)
coordinates(dataset) <- ~Longitude+Latitude
class(dataset)
proj4string(dataset) <- CRS("+init=epsg:4326")

dataset <- spTransform(dataset, CRS(proj4string(ldn1)))
identical(proj4string(ldn1), proj4string(dataset))
dataset.df <- data.frame(dataset)

map2 <- ggplot()
map2 + geom_polygon(data = ldn1, aes(x = long, y = lat, group = group)) +
  geom_point(data = dataset.df, aes(x = Longitude, y = Latitude), colour = "red")
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
