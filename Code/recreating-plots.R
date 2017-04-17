libs <- c("ggplot2", "rgeos", "rgdal", "maps", "mapdata", "mapproj", "maptools", "sp", "ggmap")
lapply(libs, library, character.only = TRUE)

df <- read.csv("Data/crimes-2016-types.csv")
dir_1 <- "References/Creating-maps-in-R-master/Creating-maps-in-R-master/data/"
dir_2 <- "Data/statistical-gis-boundaries-london/ESRI/"
ldn1 <- readOGR(file.path(dir_1), layer = "london_sport")

# wrong projections if plotting just ldn1 before transforming below and using ldn1.wgs84
map1 <- ggplot(ldn1.wgs84) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", colour = "black") +
  labs(x = "Longitude", y = "Latitude", title = "Map of Greater London with the borough boundaries")
map1 + geom_point(data = df, aes(x = Longitude, y = Latitude, colour = Crime.type)) +
  scale_colour_manual(values = rainbow(14))

# transforming coordinates
proj4string(ldn1) <- CRS("+init=epsg:27700")
ldn1.wgs84 <- spTransform(ldn1, CRS("+init=epsg:4326"))

class(df)
coordinates(df) <- ~Longitude+Latitude
class(df)
proj4string(df) <- CRS("+init=epsg:4326")

df <- spTransform(df, CRS(proj4string(ldn1)))
identical(proj4string(ldn1), proj4string(df))
df.t <- data.frame(df)

plot(ldn1)
points(df.t$Longitude, df.t$Latitude)
