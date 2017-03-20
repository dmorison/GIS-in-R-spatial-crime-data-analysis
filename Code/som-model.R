setwd("C:/Users/davem/Projects/201703_crime-data/")
libs <- c("kohonen")
lapply(libs, library, character.only = TRUE)

crimeTypes <- read.csv("Data/crime-types.csv")

dataTrain <- crimeTypes[, 2:ncol(crimeTypes)]
dataTrainMatrix <- as.matrix(scale(dataTrain))
names(dataTrainMatrix) <- names(dataTrain)

somGrid <- somgrid(xdim = 10, ydim = 10, topo = "hexagonal")

system.time(somModel <- som(dataTrainMatrix,
                grid = somGrid,
                rlen = 200,
                alpha = c(0.05,0.01),
                keep.data = TRUE))

plot(somModel, type = "changes")
plot(somModel, type = "counts")
plot(somModel, type = "dist.neighbours")
var <- 4
plot(somModel, type = "property", property = getCodes(somModel, 1)[, 1], main = colnames(getCodes(somModel, 1))[1])
