setwd("C:/Users/davem/Projects/201703_crime-data/")
libs <- c("kohonen")
lapply(libs, library, character.only = TRUE)
# Import data
crimeTypes <- read.csv("Data/crime-types.csv")

# SOM training function
trainSOM <- function(dims, lrn) {
  dataTrainMatrix <- as.matrix(scale(dataTrain))
  names(dataTrainMatrix) <- names(dataTrain)
  print(dataTrainMatrix[1:6, ])
  
  somGrid <- somgrid(xdim = dims[1], ydim = dims[2], topo = "hexagonal")
  
  model <- som(dataTrainMatrix,
                  grid = somGrid,
                  rlen = lrn,
                  alpha = c(0.05,0.01),
                  keep.data = TRUE)
  
  print(model)
  summary(model)
  
  return(model)
}

# colour palette
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

# SOM output function
outputSOM <- function(file, folder) {
  print(somModel$unit.classif[1:6])
  print(getCodes(somModel, 1)[somModel$unit.classif[1:6], ])
  
  resultTable <- data.frame(cbind(crimeTypes[, 1], dataTrain, somModel$unit.classif))
  print(summary(resultTable))
  write.csv(resultTable, file = file.path(paste0('Plots/SOM-output/', file, folder, '/', file, folder, '.csv')),
            row.names = FALSE)
  
  nodeFreq <- aggregate(somModel$unit.classif, by = list(somModel$unit.classif), FUN = length)
  nodeFreq <- nodeFreq[order(-nodeFreq$x), ]
  print(nodeFreq[1:20, ])
  write.csv(nodeFreq, file = file.path(paste0('Plots/SOM-output/', file, folder, '/', file, folder, '_node-frequencies',
                                              '.csv')), row.names = FALSE)
  
  png(file.path(paste0('Plots/SOM-output/', file, folder, '/', 'changes.png')), width = 796, height = 562)
  plot(somModel, type = "changes")
  dev.off()
  png(file.path(paste0('Plots/SOM-output/', file, folder, '/', 'counts.png')), width = 796, height = 562)
  plot(somModel, type = "counts", palette.name = coolBlueHotRed)
  dev.off()
  png(file.path(paste0('Plots/SOM-output/', file, folder, '/', 'quality.png')), width = 796, height = 562)
  plot(somModel, type = "quality", palette.name = coolBlueHotRed)
  dev.off()
  png(file.path(paste0('Plots/SOM-output/', file, folder, '/', 'distance.png')), width = 796, height = 562)
  plot(somModel, type = "dist.neighbours", palette.name=grey.colors)
  dev.off()
  png(file.path(paste0('Plots/SOM-output/', file, folder, '/', 'codes.png')), width = 796, height = 562)
  plot(somModel, type = "codes", palette.name = coolBlueHotRed)
  dev.off()
  
  for(i in 1:length(vars)) {
    print(i)
    pTitle <- colnames(getCodes(somModel, 1))[i]
    png(file.path(paste0('Plots/SOM-output/', file, folder, '/', file, pTitle, '.png')),
        width = 796, height = 562)
    plot(somModel, type = "property", property = getCodes(somModel, 1)[, i],
         main = pTitle, palette.name = coolBlueHotRed)
    dev.off()
  }
  
  return(nodeFreq)
}

# Variable to select up to the last variable
allvars <- ncol(crimeTypes)
# Initiate variable to model. First one can only start from 2
vars <- c(2:allvars)
dataTrain <- crimeTypes[, vars]
# CHOOSE INPUT VALUES FOR SOM TRAINING. First run the trainSOM function
somModel <- trainSOM(c(20,20), 200)
# CHOOSE FILE AND FOLDER INPUTS. First run the outputSOM function
result <- outputSOM("20x20_r200_", "vars-all")



###################################
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










