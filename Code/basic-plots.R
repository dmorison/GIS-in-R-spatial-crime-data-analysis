library(ggplot2)
library(ggmap)

setwd("C:/Users/davem/Projects/201703_crime-data")

city <- read.csv("Data/city-of-london-police-coord-data/2016-01/2016-01-city-of-london-street.csv")

# dataset <- data.frame(cbind(city$Longitude, city$Latitude, factor(city$Crime.type)))
# colnames(dataset) <- c("Longitude", "Latitude", "Crime")
city <- city[which(complete.cases(city$Longitude)), ]

p1 <- ggplot(city, aes(x = Longitude, y = Latitude))
p1 + geom_point(aes(colour = Crime.type)) +
  scale_colour_manual(values = rainbow(14))

### basic map
mapDims <- c(left = -0.12, bottom = 51.508, right = -0.07, top = 51.525)
map1 <- get_map(mapDims)
ggmap(map1) + geom_point(data = city, aes(x = Longitude, y = Latitude, colour = Crime.type)) +
  scale_colour_manual(values = rainbow(14)) +
  labs(title = "Crime in the City of London for January 2016")
ggsave("Plots/ggmap-city-of-london-crime-types-jan2016.png")
