setwd("C:/Users/davem/Projects/201703_crime-data/")
libs <- c("kohonen")
lapply(libs, library, character.only = TRUE)

crimeTypes <- read.csv("Data/crime-types.csv")

# variable selection
allvars <- ncol(crimeTypes)
varFrom <- 2
varTo <- allvars
vars <- c(3, 5, 14)
dataTrain <- crimeTypes[, vars]
dataTrainMatrix <- as.matrix(scale(dataTrain))
names(dataTrainMatrix) <- names(dataTrain)

somGrid <- somgrid(xdim = 20, ydim = 20, topo = "hexagonal")

system.time(somModel <- som(dataTrainMatrix,
                grid = somGrid,
                rlen = 200,
                alpha = c(0.05,0.01),
                keep.data = TRUE))

print(somModel)
summary(somModel)

# colour palette
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

plot(somModel, type = "changes")
plot(somModel, type = "counts", palette.name = coolBlueHotRed)
plot(somModel, type = "quality", palette.name = coolBlueHotRed)
plot(somModel, type = "dist.neighbours", palette.name=grey.colors)

for(i in 1:length(vars)) {
  print(i)
  pTitle <- colnames(getCodes(somModel, 1))[i]
  png(file.path(paste0('Plots/SOM-output/20x20_r400_vars-2-3-6-14/20x20_r200_', pTitle, '.png')),
      width = 796, height = 562)
  plot(somModel, type = "property", property = getCodes(somModel, 1)[, i],
       main = pTitle, palette.name = coolBlueHotRed)
  dev.off()
}

var <- 3
plot(somModel, type = "property", property = getCodes(somModel, 1)[, var],
     main = colnames(getCodes(somModel, 1))[var], palette.name = coolBlueHotRed)
plot(somModel, type = "codes")

# get values of first 6 and last 6 nodes
getCodes(somModel, 1)[c(1:6, 395:400), ]
# get the nodes the first 6 data points are mapped to
somModel$unit.classif[1:6]
# get the values of these nodes
getCodes(somModel, 1)[c(399, 358, 255, 151, 295, 400), ]
# compare the data points values with the node values it was mapped to
# should be the same as previous step
dataTrainMatrix[1:6, ]
# match the nodes back to the original data points before scaling
dataTrain[1:6, ]
result <- data.frame(cbind(crimeTypes[, 1], dataTrain, somModel$unit.classif))
result[1:6, ]

# inspection
summary(result)
result[result$criminal_damage_and_arson == 168, ]
getCodes(somModel, 1)[380, ]
result[result$bicycle_theft == 216, ]
getCodes(somModel, 1)[400, ]
result[result$vehicle_crime == 179, ]
getCodes(somModel, 1)[360, ]

######
attr(dataTrainMatrix, 'scaled:scale')
######
# install.packages("stats")
# library(stats)
# original <- unscale(dataTrainMatrix, dataTrainMatrix)

# varUnscaled <- aggregate(as.numeric(dataTrain[, var]), by = list(somModel$unit.classif),
#                           FUN = mean, simplify = TRUE)
plot(somModel, type = "property", property = varUnscaled[, 2], main = colnames(getCodes(somModel, 1))[var],
     palette.name = coolBlueHotRed)










