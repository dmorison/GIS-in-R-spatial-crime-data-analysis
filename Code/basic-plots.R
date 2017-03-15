library(ggplot2)
library(ggmap)

setwd("C:/Users/davem/Projects/201703_crime-data")

testset <- read.csv("Data/2016-01/2016-01-city-of-london-street.csv")

# dataset <- data.frame(cbind(testset$Longitude, testset$Latitude, factor(testset$Crime.type)))
# colnames(dataset) <- c("Longitude", "Latitude", "Crime")
# dataset <- dataset[which(complete.cases(dataset)), c(1, 2)]

p1 <- ggplot(testset, aes(x = Longitude, y = Latitude))
p1 + geom_point(aes(colour = Crime.type)) +
  scale_colour_manual(values = rainbow(14))

### basic map
mapDims <- c(left = -0.12, bottom = 51.508, right = -0.07, top = 51.525)
map1 <- get_map(mapDims)
ggmap(map1) + geom_point(data = testset, aes(x = Longitude, y = Latitude, colour = Crime.type)) +
  scale_colour_manual(values = rainbow(14))

