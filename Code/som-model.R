setwd("C:/Users/davem/Projects/201703_crime-data/")
libs <- c("kohonen")
lapply(libs, library, character.only = TRUE)

crimeTypes <- read.csv("Data/crime-types.csv")

# variable selection
allvars <- ncol(crimeTypes)
varFrom <- 2
varTo <- allvars
vars <- c(2, 3, 6, 14)
dataTrain <- crimeTypes[, vars]
dataTrainMatrix <- as.matrix(scale(dataTrain))
names(dataTrainMatrix) <- names(dataTrain)

somGrid <- somgrid(xdim = 20, ydim = 20, topo = "hexagonal")

system.time(somModel <- som(dataTrainMatrix,
                grid = somGrid,
                rlen = 400,
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







