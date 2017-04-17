library(kohonen)

# Import data
crimeTypes <- read.csv("Data/crime-types.csv")

# SOM training function
trainSOM <- function(dims, lrn, file, folder) {
  dataTrainMatrix <- as.matrix(scale(dataTrain))
  
  somGrid <- somgrid(xdim = dims[1], ydim = dims[2], topo = "hexagonal")
  
  model <- som(dataTrainMatrix,
                  grid = somGrid,
                  rlen = lrn,
                  alpha = c(0.05,0.01),
                  keep.data = TRUE)
  
  summary(model)
  dataTrainDf <- data.frame(dataTrainMatrix)
  dataTrainTable <- data.frame(cbind(crimeTypes[, 1], dataTrainDf, model$unit.classif))
  print(dataTrainTable[1:6, ])
  write.csv(dataTrainTable, file = file.path(paste0('Plots/SOM-output/', file, folder, '/', file, folder,
                                                     '_scaled.csv')), row.names = FALSE)
  
  return(model)
}

# colour palette
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

# SOM output function
outputSOM <- function(file, folder) {
  #print(somModel$unit.classif[1:6])
  #print(getCodes(somModel, 1)[somModel$unit.classif[1:6], ])
  write.csv(getCodes(somModel, 1), file = file.path(paste0('Plots/SOM-output/', file, folder, '/', file, folder,
                                                           '_node-values.csv')), row.names = TRUE)
  
  resultTable <- data.frame(cbind(crimeTypes[, 1], dataTrain, somModel$unit.classif))
  print(summary(resultTable))
  write.csv(resultTable, file = file.path(paste0('Plots/SOM-output/', file, folder, '/', file, folder,
                                                 '_unscaled.csv')), row.names = FALSE)
  
  nodeFreq <- aggregate(somModel$unit.classif, by = list(somModel$unit.classif), FUN = length)
  nodeFreq <- nodeFreq[order(-nodeFreq$x), ]
  print(nodeFreq[1:20, ])
  write.csv(nodeFreq, file = file.path(paste0('Plots/SOM-output/', file, folder, '/', file, folder,
                                              '_node-frequencies.csv')), row.names = FALSE)
  
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
    print(paste('Plot: ', i))
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
# Set the file and folder to write output to
dir_1 <- "10x10_r400_"
dir_2 <- "vars-all"
# CHOOSE INPUT VALUES FOR SOM TRAINING. First run the trainSOM function
somModel <- trainSOM(c(10,10), 400, dir_1, dir_2)
# CHOOSE FILE AND FOLDER INPUTS. First run the outputSOM function
result <- outputSOM(dir_1, dir_2)



